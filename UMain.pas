unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Excel2010,

  UHeliosDriver,
  ULPREventsManager,
  UCameraRegistration, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Layouts, FMX.ListView;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FManager: TLPREventsManager;
    FRegister: TCameraRegister;

    procedure HandleOnLPREvent(Sender: TObject; const LicensePlate: string; const Camera: TCamera);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  Camera: TCamera;
begin

  FRegister := TCameraRegister.Create;

  Camera.Name      := 'Leitura OCR';
  Camera.Direction := 'cima';
  Camera.Firmware  := '5.5';
  Camera.IP        := '179.189.84.189';
  Camera.Port      := 80;
  Camera.Username  := 'admin';
  Camera.Password  := '5695nettel';
  FRegister.Include(Camera);

  Camera.Name      := 'Local';
  Camera.Direction := 'cima';
  Camera.Firmware  := '5.5';
  Camera.IP        := '192.168.1.64';
  Camera.Port      := 80;
  Camera.Username  := 'admin';
  Camera.Password  := 'Abc12345';
  FRegister.Include(Camera);

  FManager := TLPREventsManager.Create;
  FManager.OnPlate := HandleOnLPREvent;
  FManager.Cameras := FRegister.Cameras;

  FManager.StartManager;

end;

procedure TForm1.HandleOnLPREvent(Sender: TObject; const LicensePlate: string; const Camera: TCamera);
var
  Item: TListViewItem;
  Helios: THeliosWebService;
  Data: THeliosWebServiceData;
begin

  Item        := ListView1.Items.Add;
  Item.Text   := LicensePlate;
  Item.Detail := Camera.Name;

  Data.LicensePlate := LicensePlate;
  Data.DateTime     := Now;

  try
    Helios := THeliosWebService.Create('', 80, Data);
  finally
    Helios.Free;
  end;

end;

end.
