unit ULinkedRecord;

interface

type

  TLinkedRecordNameChangedProc = procedure(const NewName, OldName: string) of object;
  TLinkedRecordNameChangedProcArray = TArray<TLinkedRecordNameChangedProc>;

  TLinkedUpdateRecord = record
    class var Update: TLinkedRecordNameChangedProcArray;
    class procedure AddUpdateProc(Proc: TLinkedRecordNameChangedProc); static;
    class procedure NotifyUpdate(const NewName, OldName: string); static;
  end;

implementation

uses
  { Middlewares }
  UMiddlewareLibrary;

////////////////////////////////////////////////////////////////////////////////
// Name: AddUpdateProc
//
// Description:
//   Add update procedure
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] Proc - Procedure
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
class procedure TLinkedUpdateRecord.AddUpdateProc(Proc: TLinkedRecordNameChangedProc);
begin

  TGenericArray.Add<TLinkedRecordNameChangedProc>(Update, Proc);

end;

////////////////////////////////////////////////////////////////////////////////
// Name: NotifyUpdate
//
// Description:
//   Notify update
//
// Author: Francisco Luiz Zanini
//
// Parameters List:
//   [IN] NewName - New name
//   [IN] OldName - Old name
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
class procedure TLinkedUpdateRecord.NotifyUpdate(const NewName, OldName: string);
var
  i: Integer;
begin

  for i := 0 to High(Update) do
    Update[i](NewName, OldName);

end;

end.
