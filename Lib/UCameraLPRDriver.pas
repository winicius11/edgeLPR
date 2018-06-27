unit UCameraLPRDriver;

interface

uses
  { Delphi }
  Winapi.Windows,
  System.Net.HTTPClient,
  System.Net.HTTPClientComponent,
  System.Net.URLClient,
  Xml.XMLIntf,

  { FMX }
  FMX.Types;

type

  TLPRDriverConnectionStage = (cs1None,
                               cs1QueryingLastEvent,
                               cs1ReceivingData);

  TLPRDriver = class
  private

    FHTTP: TNetHTTPClient;

    FTimer: TTimer;

    FConnectionStage: TLPRDriverConnectionStage;
    FLastPicName: string;

    procedure SendQueryLastEvent;

    procedure ExtractIDs(const Root: IXMLNode);
    procedure ExtractLastID(const Root: IXMLNode);
    procedure ProcessCameraResponse(const Data: string);

    procedure HandleOnTimer(Sender: TObject);

    procedure HandleOnHTTPRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);

  public

    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure StartMonitor;
    procedure StopMonitor;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'TLPRDriver'}
constructor TLPRDriver.Create;
begin

  inherited Create;

  // Create the HTTP
  FHTTP                    := TNetHTTPClient.Create(nil);
  FHTTP.Asynchronous       := TRUE;
  FHTTP.OnRequestCompleted := HandleOnHTTPRequestCompleted;

  // Create pooling timer
  FTimer          := TTimer.Create(nil);
  FTimer.Enabled  := FALSE;
  FTimer.Interval := 500;
  FTimer.OnTimer  := HandleOnTimer;

end;

destructor TLPRDriver.Destroy;
begin

  FTimer.Free;

  inherited Destroy;

end;

procedure TLPRDriver.ExtractIDs(const Root: IXMLNode);
var
  i, NodesCount: Integer;
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

      // { TODO : Triggar o evento com placa para outra classe }
      //Include the event to Processor

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

  SendQueryLastEvent;

end;

procedure TLPRDriver.ProcessCameraResponse(const Data: string);
var
  XML: IXMLDocument;
  Root: IXMLNode;
begin

  // Load XML
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

procedure TLPRDriver.HandleOnHTTPRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin

  if (AResponse.StatusCode = 200) and (AResponse.StatusText = 'OK') then
    ProcessCameraResponse(AResponse.ContentAsString);

end;

procedure TLPRDriver.SendQueryLastEvent;
var
  Command: string;
  Header: TNetHeaders;
begin

  SetLength(Header, 1);
  Header[0].Create('Content-Type', 'application/x-www-form-urlencoded');

  // Generate the command
  case FConnectionStage of
    cs1QueryingLastEvent:  Command := '<AfterTime><picTime>0</picTime></AfterTime>';
    cs1ReceivingData:      Command := Format('<AfterTime><picTime>%s</picTime></AfterTime>', [FLastPicName]);
  end;

  // Send the command
  FHTTP.Post('/ISAPI/Traffic/channels/1/vehicleDetect/plates', Command, nil, Header);

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
