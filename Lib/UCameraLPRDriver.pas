unit UCameraLPRDriver;

interface

uses
  { Delphi }
  Winapi.Windows,
  System.Classes,
  System.Net.HTTPClient,
  System.Net.HTTPClientComponent,
  System.Net.URLClient,
  System.Generics.Collections,
  Xml.XMLDoc,
  Xml.XMLDom,
  Xml.XMLIntf,

  { FMX }
  FMX.Types,

  UCameraRegistration;

type

  TLPRDriverConnectionStage = (cs1None,
                               cs1QueryingLastEvent,
                               cs1ReceivingData);

  TLPRDriver = class;
  TLPRDrivers = TList<TLPRDriver>;

  TLPRDriverEvent = procedure(Sender: TObject; const Plate: string; const Camera: TCamera) of object;
  TLPRDriver = class
  private

    FHTTP: TNetHTTPClient;
    FTimer: TTimer;

    FOnPlate: TLPRDriverEvent;

    FConnectionStage: TLPRDriverConnectionStage;
    FLastPicName: string;
    FLastPlate: string;
    FCamera: TCamera;
    FContentData: TMemoryStream;
    FQuerying: Boolean;

    procedure SendQueryLastEvent;

    procedure ExtractIDs(const Root: IXMLNode);
    procedure ExtractLastID(const Root: IXMLNode);
    procedure ProcessCameraResponse(const Data: string);

    procedure HandleOnTimer(Sender: TObject);
    procedure HandleOnHTTPAuthEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean; var Persistence: TAuthPersistenceType);
    procedure HandleOnHTTPRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);

    procedure TriggerOnPlate(const Data: string);

  public

    constructor Create(const Camera: TCamera); reintroduce;
    destructor Destroy; override;

    procedure StartMonitor;
    procedure StopMonitor;

    // Events
    property OnPlate: TLPRDriverEvent read FOnPlate write FOnPlate;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'TLPRDriver'}
constructor TLPRDriver.Create(const Camera: TCamera);
begin

  inherited Create;

  FCamera := Camera;
  FContentData := TMemoryStream.Create;

  // Create the HTTP
  FHTTP                    := TNetHTTPClient.Create(nil);
  FHTTP.Asynchronous       := TRUE;
  FHTTP.CredentialsStorage.AddCredential(TCredentialsStorage.TCredential.Create(TAuthTargetType.Server, '', '', Camera.Username, Camera.Password));
  FHTTP.OnRequestCompleted := HandleOnHTTPRequestCompleted;
  FHTTP.OnAuthEvent        := HandleOnHTTPAuthEvent;

  // Create pooling timer
  FTimer          := TTimer.Create(nil);
  FTimer.Enabled  := FALSE;
  FTimer.Interval := 1000;
  FTimer.OnTimer  := HandleOnTimer;

  FQuerying := FALSE;

end;

destructor TLPRDriver.Destroy;
begin

  FTimer.Free;
  FHTTP.Free;
  FContentData.Free;

  inherited Destroy;

end;

procedure TLPRDriver.ExtractIDs(const Root: IXMLNode);
var
  i, NodesCount: Integer;
  Plate: string;
begin

  // Check if is a valid node
  if not Assigned(Root) then
    Exit;

  // Initialization
  NodesCount := Root.ChildNodes.Count - 1;

  // Copy values
  for i := 0 to NodesCount do
  begin

    // Read the Plate node value
    if SameStr(Root.ChildNodes[i].NodeName, 'Plate') then
    begin

      Plate := Root.ChildNodes[i].ChildNodes['plateNumber'].Text;
      if SameStr(FLastPlate, Plate) and SameStr(FLastPicName, Root.ChildNodes[i].ChildNodes['picName'].Text) then
        Exit;

      FLastPlate := Plate;

      // Store the Last PicName
      FLastPicName := Root.ChildNodes[i].ChildNodes['picName'].Text;

      //Include the event to Processor
      TriggerOnPlate(Plate);

    end;

  end;

end;

procedure TLPRDriver.ExtractLastID(const Root: IXMLNode);
var
  NodesCount: Integer;
begin

  // Check if is valid node
  if not Assigned(Root) then
    Exit;

  // Store the last node index
  NodesCount := Root.ChildNodes.Count - 1;

  // Change the connection stage
  FConnectionStage := cs1ReceivingData;

  // Check if the count is null
  if Root.ChildNodes.Count = 0 then
    FLastPicName := '0'

  // Store the last ID
  else if SameStr(Root.ChildNodes[NodesCount].NodeName, 'Plate') then
    FLastPicName := Root.ChildNodes[NodesCount].ChildNodes['picName'].Text;

  // Start monitor
  FTimer.Enabled := TRUE;

end;

procedure TLPRDriver.HandleOnTimer(Sender: TObject);
begin

  if FQuerying then
    Exit;

  SendQueryLastEvent;

end;

procedure TLPRDriver.ProcessCameraResponse(const Data: string);
var
  XML: IXMLDocument;
  Root: IXMLNode;
begin

  // Load XML
  XML := TXMLDocument.Create(nil);
  XML.LoadFromXML(Data);
  if not Assigned(XML) then
    Exit;

  // Result Node
  Root := XML.DocumentElement;

  // Check the connection stage
  case FConnectionStage of
    cs1QueryingLastEvent: ExtractLastID(Root);
    cs1ReceivingData:     ExtractIDs(Root);
  end;

end;

procedure TLPRDriver.HandleOnHTTPAuthEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;  var Persistence: TAuthPersistenceType);
begin

end;

procedure TLPRDriver.HandleOnHTTPRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin

  FQuerying := FALSE;

  if (AResponse.StatusCode = 200) and (AResponse.StatusText = 'OK') then
    ProcessCameraResponse(AResponse.ContentAsString);

end;

procedure TLPRDriver.TriggerOnPlate(const Data: string);
begin

  if Assigned(FOnPlate) then
    FOnPlate(Self, Data, FCamera);

end;

procedure TLPRDriver.SendQueryLastEvent;
var
  Command: string;
  Header: TNetHeaders;
  Body: TStrings;
begin

  SetLength(Header, 1);
  Header[0].Create('Content-Type', 'application/x-www-form-urlencoded');

  // Generate the command
  case FConnectionStage of
    cs1QueryingLastEvent: Command := '<AfterTime><picTime>0</picTime></AfterTime>';
    cs1ReceivingData:     Command := Format('<AfterTime><picTime>%s</picTime></AfterTime>', [FLastPicName]);
  end;

  FContentData.Clear;
  FContentData.Write(TEncoding.ANSI.GetBytes(Command), Length(Command));
  FContentData.Position := 0;

  // Send the command
  FQuerying := TRUE;
  FHTTP.Post(Format('http://%s:%d/ISAPI/Traffic/channels/1/vehicleDetect/plates', [FCamera.IP, FCamera.Port]), FContentData);

end;

procedure TLPRDriver.StartMonitor;
begin

  FConnectionStage := cs1QueryingLastEvent;

  FTimer.Enabled := TRUE;

  SendQueryLastEvent;

end;

procedure TLPRDriver.StopMonitor;
begin

  FTimer.Enabled := FALSE;

end;
{$ENDREGION}

end.
