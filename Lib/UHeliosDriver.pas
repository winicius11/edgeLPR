unit UHeliosDriver;

interface

uses
  { Delphi }
  System.Classes,
  System.MaskUtils,
  System.SysUtils,
  System.StrUtils,
  System.Net.HTTPClient,
  System.Net.HTTPClientComponent,
  System.Net.URLClient;

type

  TLocation = class
  private

    // Class properties
    FLtd: string;
    FLgd: string;
    FEdc: string;
    Fid_: string;
    FSnt: string;

  public

    // Properties
    property Latitude: string read FLtd write FLtd;
    property Longitude: string read FLgd write FLgd;
    property Endereco: string read FEdc write FEdc;
    property Id: string read Fid_ write Fid_;
    property Sentido: string read FSnt write FSnt;

  end;

  TCommandData = class
  private

    // Class properties
    FCam: string;
    FPlc: string;
    FLoc: TLocation;
    FDat: string;
    FImg: string;
    FUsr: string;

  public

    // Class functions
    function ConvertToJSONString: string;

    // Class constructor and destructor
    constructor Create; reintroduce;
    destructor Destroy; override;

    // Properties
    property Camera: string read FCam write FCam;
    property Placa: string read FPlc write FPlc;
    property Localizacao: TLocation read FLoc write FLoc;
    property Data: string read FDat write FDat;
    property Imagem: string read FImg write FImg;
    property User: string read FUsr write FUsr;

  end;

  TOcorrencia = class
  private

    // Class properties
    FMandado: Boolean;
    FRoubado: Boolean;

  public

    // Properties
    property roubado: Boolean read FRoubado write FRoubado;
    property mandado: Boolean read FMandado write FMandado;

  end;

  TCommandResponse = class
  private

    // Class properties
    FCor: string;
    FPlc: string;
    FModelo: string;
    FAno: string;
    FOcorrencia: TOcorrencia;

  public

    // Class functions
    function ConvertJSONToCommandResponse(const Data: string): TCommandResponse;

    // Class constructor and destructor
    constructor Create; reintroduce;
    destructor Destroy; override;

    // Properties
    property Placa: string read FPlc write FPlc;
    property Modelo: string read FModelo write FModelo;
    property Ano: string read FAno write FAno;
    property Cor: string read FCor write FCor;
    property Ocorrencias: TOcorrencia read FOcorrencia write FOcorrencia;

  end;

  THeliosWebServiceData = record
    EventID:      Int64;
    LicensePlate: string;
    DateTime:     TDateTime;
    Latitude:     Double;
    Longitude:    Double;
    CameraID:     Int64;
    JPEGImage:    TBytes;
  end;

  THeliosWebService = class
  private

    // Request data
    FHost: string;
    FPort: Integer;
    FData: THeliosWebServiceData;
    FCommandData: TCommandData;

    // HTTP client
    FHTTPClient: TNetHTTPClient;

    procedure HandleOnHTTPRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);

  public

    // Constructor
    constructor Create(const Host: string; Port: Integer; const Data: THeliosWebServiceData); reintroduce;
    destructor Destroy; override;

    procedure SendNotification;

  end;

implementation

uses
  { Delphi }
  REST.JSON,

  { Middleware }
  UCrypt;

const
  REFRESH_TOKEN = '{k: %appKey, j: %token, r: %refreshToken}';
  DIGIFORT_TOKEN = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCIsImp0aSI6IjZkZjRjN2ZkLWNiODAtNGVkYi04NDRjLWYwZDAzMjRiMTM1ZiIsImlhdCI6MTQ5MTkxMzY3Njc5NiwiZXhwIjoxNDkxOTEzNzYzMTk2fQ.eyJnIjoi'+'YzkyNzA3NiIsInAiOiJDVFMiLCJyIjoiRFRTIiwidSI6NzEsImYiOlsiMTAwMDMuMTAwMDAzIl0sImkiOiIiLCJrIjoxM30.krnYx_ZuGGrdxceopmd71no0xGoxiK2Z2w-4_rHFvco';

{$REGION 'TCommandData'}
////////////////////////////////////////////////////////////////////////////////
// Name: Create
//
// Description:
//   Class constructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
constructor TCommandData.Create;
begin

  inherited Create;

  // Create Location object
  FLoc := TLocation.Create;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Destroy
//
// Description:
//   Class destructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
destructor TCommandData.Destroy;
begin

  // Release Location object
  FLoc.Free;

  inherited Destroy;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: ConvertToJSONString
//
// Description:
//   Convert the CommandData object to JSON string
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   [String] - TCommandData in JSON
////////////////////////////////////////////////////////////////////////////////
function TCommandData.ConvertToJSONString: string;
begin

  Result := TJSON.ObjectToJsonString(Self);

end;
{$ENDREGION}

{$REGION 'TCommandResponse'}
////////////////////////////////////////////////////////////////////////////////
// Name: Create
//
// Description:
//   Class constructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
constructor TCommandResponse.Create;
begin

  // Create the TOcorrencia object
  FOcorrencia := TOcorrencia.Create;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Destroy
//
// Description:
//   Class destructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
destructor TCommandResponse.Destroy;
begin

  // Release Ocorrencia object
  FOcorrencia.Free;

  inherited Destroy;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: ConvertJSONToCommandResponse
//
// Description:
//   Create a TCommandResponse object from JSON data
//
// Author: Winicius Moreira
//
// Parameters List:
//   [IN] Data - string
//
// Returns:
//   [TCommandResponse] - TCommandResponse object
////////////////////////////////////////////////////////////////////////////////
function TCommandResponse.ConvertJSONToCommandResponse(const Data: string): TCommandResponse;
begin

  Result := TJson.JsonToObject<TCommandResponse>(Data);

end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////
// Name: Create
//
// Description:
//   Constructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   [IN] Host - Host
//   [IN] Port - Port
//   [IN] Data - Request data
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
constructor THeliosWebService.Create(const Host: string; Port: Integer; const Data: THeliosWebServiceData);
begin

  inherited Create;

  // Store parameters
  FHost := Host;
  FPort := Port;
  FData := Data;

  // Create HTTP client
  FHTTPClient                    := TNetHTTPClient.Create(nil);
  FHTTPClient.Asynchronous       := TRUE;
  FHTTPClient.OnRequestCompleted := HandleOnHTTPRequestCompleted;
  FHTTPClient.ContentType        := 'application/json';
  FHTTPClient.CustomHeaders['Authorization'] := DIGIFORT_TOKEN;

  // Create command data
  FCommandData := TCommandData.Create;

  SendNotification;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Destroy
//
// Description:
//   Class destructor
//
// Author: Winicius Moreira
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
destructor THeliosWebService.Destroy;
begin

  FHTTPClient.Free;

  // Release CommandData object
  FCommandData.Free;

  inherited Destroy;

end;

procedure THeliosWebService.HandleOnHTTPRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Execute
//
// Description:
//   Thread routine
//
// Author: Guilherme Silva
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure THeliosWebService.SendNotification;
var
  URL: string;
  BData: TBytes;
  PostData: TMemoryStream;
  Header: TNetHeaders;
  Response: IHTTPResponse;
begin

//  SetLength(Header, 1);
//  Header[0].Create('Authorization', DIGIFORT_TOKEN);


  // Set the URL
  FHost := 'helios.policiamilitar.mg.gov.br';
  FPort := 80;
  URL := Format('http://%s:%d/v1/api/veiculo/enviar', [FHost, FPort]);

  // Create command data to Post
  FCommandData.Placa := FormatMaskText('AAA\-9999;0;', FData.LicensePlate);
  FCommandData.Data  := FormatDateTime('YYYY-MM-DD HH:mm:ss.zzz', FData.DateTime);
  FCommandData.Imagem                := 'data:image/jpeg;base64,'+ TBase64.Base64EncodeBytes(FData.JPEGImage);
  FCommandData.Localizacao.Endereco  := 'Rua Teffé, 334';
  FCommandData.Localizacao.Sentido   := 'Subindo';
  FCommandData.Localizacao.Longitude := '-46.554874';
  FCommandData.Localizacao.Latitude  := '-23.630371';

  // Store post data
  PostData := TMemoryStream.Create;
  try

    // Convert to bytes
    BData := TEncoding.ANSI.GetBytes(FCommandData.ConvertToJSONString);

    // Store post data
    PostData.Write(BData, Length(BData));
    PostData.Position := 0;

    // Send request
    Response := FHTTPClient.Post(URL, PostData);


  finally
    PostData.Free;
  end;

end;

end.
