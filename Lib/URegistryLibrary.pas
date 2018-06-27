unit URegistryLibrary;

interface

uses
  { Delphi }
  Winapi.Windows,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Win.Registry;

type

  TRegistryItemProc<T> = reference to procedure(var Data: T);

  TGenericRegistryResult = (reError, reOK, reAlreadyExists, reNotFound);

  TGenericRegistry = class
  private

    class function CreateRegistry(AHKEY: HKEY = HKEY_LOCAL_MACHINE): TRegistry;
    class function SaveFields(const RegistryKey: string; const Fields: TArray<TRttiField>; Data: Pointer): TGenericRegistryResult;
    class function LoadFields(const RegistryKey: string; const Fields: TArray<TRttiField>; Data: Pointer): TGenericRegistryResult;

  public

    // Routines to load and save records
    class function SaveRecord<T>(const RegistryKey, RecordName, OldRecordName: string; const Data: T; ClearOldData: Boolean = FALSE): TGenericRegistryResult;
    class function LoadRecord<T>(const RegistryKey, RecordName: string; var Data: T): TGenericRegistryResult;
    class function DeleteRecord(const RegistryKey, RecordName: string): TGenericRegistryResult;
    class function LoadListRecord<T>(const RegistryKey: string; List: TList<T>; SetDefaultProc: TRegistryItemProc<T> = nil; AfterLoadProc: TRegistryItemProc<T> = nil): TGenericRegistryResult;

    // Routines to load and save values
    class function SaveValue<T>(const RegistryKey, ValueName: string; Value: T): TGenericRegistryResult;
    class function DeleteValue(const RegistryKey, ValueName: string): TGenericRegistryResult;
    class function LoadListValue<T>(const RegistryKey: string; var List: TArray<TPair<string,T>>): TGenericRegistryResult;
    class function LoadValue<T>(const RegistryKey, ValueName: string; var Data: T; AHKEY: HKEY = HKEY_LOCAL_MACHINE): TGenericRegistryResult;

    // Auxiliary routines
    class procedure GetKeyNames(const RegistryKey: string; var KeyNames: TArray<string>);
    class procedure DeleteKey(const RegistryKey, KeyName: string);

  end;

implementation

uses
  UMiddlewareLibrary;

////////////////////////////////////////////////////////////////////////////////
// Name: CreateRegistry
//
// Description:
//   Create registry object
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   None
//
// Returns:
//   [TRegistry] - Registry object
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.CreateRegistry(AHKEY: HKEY = HKEY_LOCAL_MACHINE): TRegistry;
begin

  Result         := TRegistry.Create;
  Result.Access  := Result.Access or KEY_WOW64_64KEY;
  Result.RootKey := AHKEY;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: SaveFields
//
// Description:
//   Save RTTI fields into registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] Fields      - Fields to be saved
//   [IN] Data        - Data to be saved
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.SaveFields(const RegistryKey: string; const Fields: TArray<TRttiField>; Data: Pointer): TGenericRegistryResult;

  function MakeGUID: string;
  var
    GUID: TGUID;
  begin

    CreateGUID(GUID);
    Result := GUIDToString(GUID);

  end;

var
  i, j: Integer;
  RecordFields: TArray<TRttiField>;
  Reg: TRegistry;
  Value: TValue;
  aAux: TArray<string>;
  GUID: string;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKey(RegistryKey, TRUE) then
      Exit;

    for i := 0 to High(Fields) do
    begin

      // Store the field value
      Value := Fields[i].GetValue(Data);

      case Fields[i].FieldType.TypeKind of
        tkInteger:     Reg.WriteInteger(Fields[i].Name, Value.AsInteger);
        tkUString:     Reg.WriteString(Fields[i].Name, Value.AsString);
        tkEnumeration: Reg.WriteInteger(Fields[i].Name, Value.AsOrdinal);
        tkSet:         Reg.WriteString(Fields[i].Name, SetToString(Value.TypeInfo, @TBytes(Data)[Fields[i].Offset], TRUE));
        tkDynArray:
        begin

          // Array initialization
          SetLength(aAux, 0);

          case TRttiDynamicArrayType(Fields[i].FieldType).ElementType.TypeKind of
            tkInteger,
            tkEnumeration:
            begin

              // Add items to array
              for j := 0 to Value.GetArrayLength -1 do
                TGenericArray.Add<string>(aAux, Value.GetArrayElement(j).AsOrdinal.ToString);

              // Write to registry
              Reg.WriteString(Fields[i].Name, TGenericArray.Join(aAux));

            end;

            tkUString:
            begin

              // Add items to array
              for j := 0 to Value.GetArrayLength -1 do
                TGenericArray.Add<string>(aAux, Value.GetArrayElement(j).AsString);

              // Write to registry
              Reg.WriteString(Fields[i].Name, TGenericArray.Join(aAux));

            end;

            tkRecord:
            begin

              // Remove current values
              DeleteKey(RegistryKey, Fields[i].Name);

              for j := 0 to Value.GetArrayLength -1 do
              begin

                // Get the record fields
                RecordFields := TRttiDynamicArrayType(Fields[i].FieldType).ElementType.GetFields;

                // Make random GUID
                GUID := MakeGUID;

                // Save fields
                Result := SaveFields(RegistryKey + '\' + Fields[i].Name + '\' + GUID, RecordFields, Value.GetReferenceToRawArrayElement(j));

                // Exit if errors
                if Result <> reOK then
                  Exit;

              end;

            end;

          end;

        end;

        tkRecord:
        begin

          // Get the record fields
          RecordFields := Fields[i].FieldType.AsRecord.GetFields;

          // Save fields
          Result := SaveFields(RegistryKey + '\' + Fields[i].Name, RecordFields, @TBytes(Data)[Fields[i].Offset]);

          // Exit if errors
          if Result <> reOK then
            Exit;

        end;

      end;

    end;

    // Set result
    Result := reOK;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: LoadFields
//
// Description:
//   Load fields
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] Fields      - Fields
//   [IN] Data        - Data to be loaded
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.LoadFields(const RegistryKey: string;
  const Fields: TArray<TRttiField>; Data: Pointer): TGenericRegistryResult;
var
  Reg: TRegistry;
  i, j: Integer;
  RecordFields: TArray<TRttiField>;
  aAux: TArray<string>;
  Value: TArray<TValue>;
  ValidArray: Boolean;
  KeyNames: TArray<string>;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    for i := 0 to High(Fields) do
    begin

      // Records
      if Fields[i].FieldType.TypeKind = tkRecord then
      begin

        // Get the record fields
        RecordFields := Fields[i].FieldType.AsRecord.GetFields;

        // Load record
        Result := LoadFields(RegistryKey + '\' + Fields[i].Name, RecordFields, @TBytes(Data)[Fields[i].Offset]);

        // Exit if errors
        if Result <> reOk then
          Exit;

        // Next field
        Continue;

      end;

      // Check if the value exists
      if not (Reg.ValueExists(Fields[i].Name) or Reg.KeyExists(Fields[i].Name)) then
        Continue;

      // Load
      case Fields[i].FieldType.TypeKind of
        tkInteger:     Fields[i].SetValue(Data, Reg.ReadInteger(Fields[i].Name));
        tkUString:     Fields[i].SetValue(Data, Reg.ReadString(Fields[i].Name));
        tkEnumeration: Fields[i].SetValue(Data, TValue.FromOrdinal(Fields[i].FieldType.Handle, Reg.ReadInteger(Fields[i].Name)));
        tkSet:         StringToSet(Fields[i].GetValue(Data).TypeInfo, Reg.ReadString(Fields[i].Name), @TBytes(Data)[Fields[i].Offset]);
        tkDynArray:
        begin

          // Load from value
          if Reg.ValueExists(Fields[i].Name) then
          begin
            TGenericArray.Split(aAux, Reg.ReadString(Fields[i].Name));
            SetLength(Value, Length(aAux));
          end

          // Load from keys
          else if Reg.KeyExists(Fields[i].Name) then
          begin
            // Get the key names
            GetKeyNames(RegistryKey + '\' + Fields[i].Name, KeyNames);
            SetLength(Value, Length(KeyNames));
          end;

          // Initialization
          ValidArray := TRUE;

          case TRttiDynamicArrayType(Fields[i].FieldType).ElementType.TypeKind of

            // Arrays of enum
            tkEnumeration:
            begin
              for j := 0 to High(Value) do
                Value[j] := TValue.FromOrdinal(TRttiDynamicArrayType(Fields[i].FieldType).ElementType.Handle, aAux[j].ToInteger);
            end;

            // Arrays of unicode string
            tkUString:
            begin
              for j := 0 to High(Value) do
                Value[j] := aAux[j];
            end;

            // Arrays of record
            tkRecord:
            begin

              // Get the record fields
              RecordFields := TRttiDynamicArrayType(Fields[i].FieldType).ElementType.GetFields;

              for j := 0 to High(KeyNames) do
              begin

                // Determine the the value type
                TValue.Make(nil, TRttiDynamicArrayType(Fields[i].FieldType).ElementType.Handle, Value[j]);

                // Load the fields
                if LoadFields(RegistryKey + '\' + Fields[i].Name + '\' + KeyNames[j], RecordFields, Value[j].GetReferenceToRawData) <> reOK then
                  Exit;

              end;

            end;

            else
              ValidArray := FALSE;
          end;

          // Store the value
          if ValidArray then
            Fields[i].SetValue(Data, TValue.FromArray(Fields[i].FieldType.Handle, Value));

        end;

      end;

    end;

  finally
    Reg.Free;
  end;

  // Set result
  Result := reOk;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: SaveRecord
//
// Description:
//   Save data to registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey   - Registry key
//   [IN] RecordName    - Current record name
//   [IN] OldRecordName - Old record name
//   [IN] Data          - Record data
//   [IN] ClearOldData  - Clear old data in case storing other record with same name
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.SaveRecord<T>(const RegistryKey, RecordName, OldRecordName: string; const Data: T; ClearOldData: Boolean = FALSE): TGenericRegistryResult;
var
  Reg: TRegistry;
  NewItem: Boolean;
  Context: TRTTIContext;
  RType: TRttiType;
  Fields: TArray<TRttiField>;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKey(RegistryKey, TRUE) then
      Exit;

    // Check if new item
    NewItem := OldRecordName.IsEmpty;

    // New items
    if NewItem then
    begin

      // If new item, check if there is other item with same name
      if Reg.KeyExists(RecordName) then
        Exit(reAlreadyExists);

    end

    // Modifying - Item name has changed
    else
    begin

      if not AnsiSameText(RecordName, OldRecordName) then
      begin

        // There is other item with the same name
        if Reg.KeyExists(RecordName) then
          Exit(reAlreadyExists);

      end;

      // Clear old data
      if ClearOldData then
      begin

        // Create another key
        Reg.DeleteKey(OldRecordName);
        Reg.CreateKey(RecordName);

      end

      // Rename the key
      else
        Reg.MoveKey(OldRecordName, RecordName, TRUE);

    end;

    // Get type info
    Context := TRTTIContext.Create;
    try

      // Get type and fields
      RType  := Context.GetType(TypeInfo(T));
      Fields := RType.GetFields;

      Result := SaveFields(RegistryKey + '\' + RecordName, Fields, @Data);

    finally
      Context.Free;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: LoadRecord
//
// Description:
//   Load data from registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN]  RegistryKey    - Registry key
//   [IN]  RecordName     - Record name
//   [OUT] Data           - Record data
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.LoadRecord<T>(const RegistryKey, RecordName: string;
  var Data: T): TGenericRegistryResult;
var
  Reg: TRegistry;
  Context: TRTTIContext;
  RType: TRttiType;
  Fields: TArray<TRttiField>;
begin

  // Initialization
  Result := reNotFound;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    // Get type info
    Context := TRTTIContext.Create;
    try

      // Get type and fields
      RType  := Context.GetType(TypeInfo(T));
      Fields := RType.GetFields;

      // Load
      Result := LoadFields(RegistryKey + '\' + RecordName, Fields, @Data);

    finally
      Context.Free;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: DeleteRecord
//
// Description:
//   Delete a key from registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] RecordName  - Record name
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.DeleteRecord(const RegistryKey, RecordName: string): TGenericRegistryResult;
var
  Reg: TRegistry;
begin

  // Initialization
  Result := reNotFound;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    // Delete
    if Reg.KeyExists(RecordName) then
    begin
      Reg.DeleteKey(RecordName);
      Result := reOk;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: LoadListRecord
//
// Description:
//   Load list
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN]  RegistryKey    - Registry key
//   [OUT] List           - Loaded list
//   [IN]  SetDefaultProc - Routine to set default values
//   [IN]  AfterLoadProc  - Routine called after load a record
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.LoadListRecord<T>(const RegistryKey: string;
  List: TList<T>; SetDefaultProc: TRegistryItemProc<T> = nil; AfterLoadProc: TRegistryItemProc<T> = nil): TGenericRegistryResult;
var
  Reg: TRegistry;
  KeyNames: TStringList;
  i: Integer;
  Item: T;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    KeyNames := TStringList.Create;
    try

      // Get key names
      Reg.GetKeyNames(KeyNames);

      for i := 0 to KeyNames.Count -1 do
      begin

        // Set defaults
        SetDefaultProc(Item);

        // Load data
        if TGenericRegistry.LoadRecord<T>(RegistryKey, KeyNames[i], Item) = reOK then
        begin

          // After load
          if Assigned(AfterLoadProc) then
            AfterLoadProc(Item);

          // Add to list
          List.Add(Item);

        end;

      end;

      // Success
      Result := reOk;

    finally
      KeyNames.Free;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: SavePair
//
// Description:
//   Save pair into registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] ValueName   - Value name
//   [IN] Value       - Value
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.SaveValue<T>(const RegistryKey, ValueName: string; Value: T): TGenericRegistryResult;
var
  Reg: TRegistry;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKey(RegistryKey, TRUE) then
      Exit;

    // Save
    case GetTypeKind(T) of
      tkUString: Reg.WriteString(ValueName, TValue.From(Value).AsString);
      else
        Exit;
    end;

    Result := reOK;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: DeleteValue
//
// Description:
//   Delete a value from registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] RecordName  - Record name
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.DeleteValue(const RegistryKey, ValueName: string): TGenericRegistryResult;
var
  Reg: TRegistry;
begin

  // Initialization
  Result := reNotFound;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKey(RegistryKey, FALSE) then
      Exit;

    // Delete
    if Reg.ValueExists(ValueName) then
    begin
      Reg.DeleteValue(ValueName);
      Result := reOk;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: LoadListValue
//
// Description:
//   Load list of pair values
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN]  RegistryKey - Registry key
//   [OUT] List        - Loaded list
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.LoadListValue<T>(const RegistryKey: string;
  var List: TArray<TPair<string,T>>): TGenericRegistryResult;
var
  Reg: TRegistry;
  ValueNames: TStringList;
  i: Integer;
  Item: TPair<string,T>;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry;
  try

    // Open the software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    ValueNames := TStringList.Create;
    try

      // Get key names
      Reg.GetValueNames(ValueNames);

      for i := 0 to ValueNames.Count -1 do
      begin

        // Set the item data
        Item.Key := ValueNames[i];

        case GetTypeKind(T) of
          tkUString: Item.Value := TValue.FromVariant(Reg.ReadString(ValueNames[i])).AsType<T>;
          tkInteger: Item.Value := TValue.FromVariant(Reg.ReadInteger(ValueNames[i])).AsType<T>;
          else
            Continue;
        end;

        // Add to list
        TGenericArray.Add<TPair<string,T>>(List, Item);

      end;

      // Success
      Result := reOk;

    finally
      ValueNames.Free;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: LoadValue
//
// Description:
//   Load value from registry
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN]  RegistryKey - Registry key
//   [IN]  ValueName   - Value name
//   [OUT] Data        - Loaded data
//   [IN]  AHKEY       - Registry root key
//
// Returns:
//   [TGenericRegistryResult] - Result
////////////////////////////////////////////////////////////////////////////////
class function TGenericRegistry.LoadValue<T>(const RegistryKey, ValueName: string; var Data: T; AHKEY: HKEY = HKEY_LOCAL_MACHINE): TGenericRegistryResult;
var
  Reg: TRegistry;
begin

  // Initialization
  Result := reError;

  Reg := CreateRegistry(AHKEY);
  try

    // Open the software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    if not Reg.ValueExists(ValueName) then
      Exit(reNotFound);

    // Load value
    case GetTypeKind(T) of

      // Dyn array
      tkUString: Data := TValue.FromVariant(Reg.ReadString(ValueName)).AsType<T>;
      tkInteger: Data := TValue.FromVariant(Reg.ReadInteger(ValueName)).AsType<T>;

    end;

    // Success
    Result := reOk;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: GetKeyNames
//
// Description:
//   Return key names
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN]  RegistryKey - Regisry key
//   [OUT] KeyNames    - Key names
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
class procedure TGenericRegistry.GetKeyNames(const RegistryKey: string; var KeyNames: TArray<string>);
var
  Reg: TRegistry;
  List: TStringList;
  i: Integer;
begin

  // Initialization
  SetLength(KeyNames, 0);

  Reg := CreateRegistry;
  try

    // Open software key
    if not Reg.OpenKeyReadOnly(RegistryKey) then
      Exit;

    List := TStringList.Create;
    try

      // Get key names
      Reg.GetKeyNames(List);

      // Set output length
      SetLength(KeyNames, List.Count);

      // Copy items
      for i := 0 to List.Count -1 do
        KeyNames[i] := List[i];

    finally
      List.Free;
    end;

  finally
    Reg.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: DeleteKey
//
// Description:
//   Delete a key
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] RegistryKey - Registry key
//   [IN] KeyName     - Key name
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
class procedure TGenericRegistry.DeleteKey(const RegistryKey, KeyName: string);
var
  Reg: TRegistry;
begin

  Reg := CreateRegistry;
  try

    // Open software key
    if not Reg.OpenKey(RegistryKey, FALSE) then
      Exit;

    // Delele
    if Reg.KeyExists(KeyName) then
      Reg.DeleteKey(KeyName);

  finally
    Reg.Free;
  end;

end;

end.
