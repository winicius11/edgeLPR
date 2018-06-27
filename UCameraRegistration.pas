unit UCameraRegistration;

interface

uses
  { Delphi }
  System.Generics.Collections;

type

  TCamera = class
  private

    // Class properties
    FName: string;
    FDirection: string;
    FPort: Integer;
    FFirmware: string;
    FIP: string;

  public

    // Properties
    property Name: string read FName write FName;
    property IP:   string read FIP   write FIP;
    property Port: Integer read FPort write FPort;
    property Direction: string read FDirection write FDirection;
    property Firmware: string read FFirmware write FFirmware;

  end;

  TCamerasList = TList<TCamera>;

  TCameraRegister = class
  private
    FCameras: TCamerasList;


  public

    constructor Create; reintroduce;
    destructor Destroy; override;

    // Properties
    property Cameras: TCamerasList read FCameras write FCameras;

  end;

implementation

{ TCameraRegister }

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

end.
