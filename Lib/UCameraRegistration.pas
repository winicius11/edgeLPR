unit UCameraRegistration;

interface

uses
  { Delphi }
  System.Generics.Collections,

  URegistryLibrary;

type

  TCamera = record
    FName: string;
    FDirection: string;
    FFirmware: string;
    FIP: string;
    FPort: Integer;
    FLatitude: string;
    FLongitude: string;
  end;

  TCamerasList = TList<TCamera>;

  TCameraRegister = class
  private

    FCameras: TCamerasList;

    procedure Save;
    procedure Load;

  public

    constructor Create; reintroduce;
    destructor Destroy; override;

    function Include(Data: TCamera): Boolean;

    // Properties
    property Cameras: TCamerasList read FCameras write FCameras;

  end;

implementation

{$REGION 'TCameraRegister'}
constructor TCameraRegister.Create;
begin

  inherited Create;

  // Create the list of cameras
  FCameras := TCamerasList.Create;

end;

destructor TCameraRegister.Destroy;
begin

  // Destroy the cameras list
  FCameras.Clear;
  FCameras.Free;

  inherited Destroy;

end;

function TCameraRegister.Include(Data: TCamera): Boolean;
begin

  Result := FCameras.Add(Data) > 1;

end;

procedure TCameraRegister.Load;
begin

  TGenericRegistry.LoadListRecord<TCamera>('Cameras', FCameras);

end;

procedure TCameraRegister.Save;
var
  i: Integer;
begin

  // Save
  for i := 0 to FCameras.Count - 1 do
    TGenericRegistry.SaveRecord<TCamera>('Cameras', Cameras[i].FName, '', Cameras[i]);

end;
{$ENDREGION}

end.
