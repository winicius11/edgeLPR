unit ULPREventsManager;

interface

uses
  UCameraLPRDriver,
  UCameraRegistration;

type

  TLPREventsManager = class
  private

    FCameras: TCamerasList;
    FDrivers: TLPRDrivers;

    FOnPlate: TLPRDriverEvent;

    procedure TriggerOnPlate(const Data: string; const Camera: TCamera);

    procedure HandleOnLPREvent(Sender: TObject; const LicensePlate: string; const Camera: TCamera);

  public

    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure StartManager;

    property Cameras: TCamerasList read FCameras write FCameras;

    // Events
    property OnPlate: TLPRDriverEvent read FOnPlate write FOnPlate;

  end;

implementation

{$REGION 'TLPREventsManager'}
constructor TLPREventsManager.Create;
begin

  inherited Create;

  FCameras := TCamerasList.Create;
  FDrivers := TLPRDrivers.Create;

end;

destructor TLPREventsManager.Destroy;
begin

  FDrivers.Free;
  FCameras.Free;

  inherited Destroy;

end;

procedure TLPREventsManager.StartManager;
var
  i: Integer;
  Driver: TLPRDriver;
begin

  for i := 0 to FCameras.Count - 1 do
  begin

    Driver         := TLPRDriver.Create(FCameras[i]);
    Driver.OnPlate := HandleOnLPREvent;

    Driver.StartMonitor;

  end;

end;

procedure TLPREventsManager.TriggerOnPlate(const Data: string; const Camera: TCamera);
begin

  if Assigned(FOnPlate) then
    FOnPlate(Self, Data, Camera);

end;

procedure TLPREventsManager.HandleOnLPREvent(Sender: TObject; const LicensePlate: string; const Camera: TCamera);
begin

  TriggerOnPlate(LicensePlate, Camera);

end;
{$ENDREGION}

end.
