program EdgeLPR;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {Form1},
  UCameraRegistration in 'Lib\UCameraRegistration.pas',
  ULinkedRecord in 'Lib\ULinkedRecord.pas',
  URegistryLibrary in 'Lib\URegistryLibrary.pas',
  UCameraLPRDriver in 'Lib\UCameraLPRDriver.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
