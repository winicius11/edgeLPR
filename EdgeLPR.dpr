program EdgeLPR;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {Form1},
  UCameraRegistration in 'UCameraRegistration.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
