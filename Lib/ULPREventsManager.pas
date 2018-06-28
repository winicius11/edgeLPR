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

    procedure HandleOnLPREvent(Sender: TObject; const LicensePlate: string);

  public

    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure StartManager;

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

    Driver            := TLPRDriver.Create(FCameras[i]);
    Driver.OnLPREvent := HandleOnLPREvent;

  end;

end;

procedure TLPREventsManager.HandleOnLPREvent(Sender: TObject; const LicensePlate: string);
begin

end;
{$ENDREGION}

end.
