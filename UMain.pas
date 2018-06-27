unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Excel2010,


  UCameraRegistration;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    FRegister: TCameraRegister;
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

  Camera.Name := '1';
  Camera.Direction := 'cima';
  Camera.Firmware  := '5.5';
  Camera.IP        := '192.168.1.64';
  Camera.Port      := 80;
  Camera.Username  := 'admin';
  Camera.Port      := '';

  FRegister := TCameraRegister.Create;
  FRegister.Include(Camera);

end;

end.
