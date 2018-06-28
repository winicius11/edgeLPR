unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Excel2010,

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

  FManager := TLPREventsManager.Create;
  FManager.OnPlate := HandleOnLPREvent;
  FManager.Cameras := FRegister.Cameras;

  FManager.StartManager;

end;

procedure TForm1.HandleOnLPREvent(Sender: TObject; const LicensePlate: string; const Camera: TCamera);
var
  Item: TListViewItem;
begin

  Item        := ListView1.Items.Add;
  Item.Text   := LicensePlate;
  Item.Detail := Camera.Name;

end;

end.
