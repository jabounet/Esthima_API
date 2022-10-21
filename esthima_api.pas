unit ESTHIMA_API;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, synautil,
  FpJson, JSonParser, DCPsha1, DCPsha256, httpsend, lclintf, ExtCtrls, Grids,
  Menus, BCButton, strutils, inifiles, DateUtils, syncobjs, Types, Windows,
  blcksock, base64, ssl_openssl, GenericHTTP, memds, DB;

const
  APIURLStaging = 'https://arion-staging.esthima.fr';
  APIURLProduction = 'https://arion.esthima.fr';

type
  TEsthimaMarque = (incineris_veterinary, incineris_regular, incineris_esthima);

// Record de service Esthima
type
  TEsthimaServices = packed record
    //nom de la clinique
    Name: string;
    //id fonctionnel
    functional_id: string;
    // services, cf infra
    services: array of string;
    { incineris_regular: ancienne convention Incineris
      incineris_veterinary: nouvelle convention marque vétérinaire
      incineris_esthima: nouvelle convention Esthima }
    //réponse http
    answer: string;
  end;

//Product Esthima

type
  TEsthimaProduct = packed record
    functionalId: string;
    {Identifiant du produit•ObligatoireListe récupérée avec le GET /api/fr/products/FR}
    productReturnMode: string;
    {•veterinary•crematorium•scattering(on ne restitue pas d'urne)}
  end;

type
  TEsthimaProducts = array of TEsthimaProduct;

type
  { TEsthima }
  TEsthima = class(TObject)

  const
    GenericHeader = 'Authorization: Bearer {token}' + #13 +
      'Accept: application/json; charset=utf-8' + #13 + 'Content-Type: application/json';

  type Token = packed record
      Value: string;
      expiration: TDateTime;
    end;

  type GenericError = packed record
      code: string;
      message: string;
      informations: string;
      answer: string;
    end;

  var
    CurToken: Token;
  const
    TokenTimeOut = 3600;
  var
    E_URL: string;
  var
    E_pw, E_login, E_Clinic_Id: string;
  var
    E_Error: GenericError;
  var
    E_Marque: string;

  var
    E_Convention_DS, E_Animal_DS, E_Products_DS: TMemDataset;
  public
    // Propriétés
    property LastErrorCode: string read E_Error.code;
    property LastErrorMessage: string read E_Error.message;
    property LastError: GenericError read E_Error;
    property Login: string read E_Login write E_Login;
    property Password: string read E_pw write E_pw;
    property Clinic_Id: string read E_Clinic_Id write E_Clinic_Id;
    property ApiURL: string read E_URL write E_URL;
    property JWT: string read CurToken.Value;

    constructor Create(Sender: TObject);
    destructor Destroy; override;

    // Fonctions/procédures
    procedure PopulateDataSets;
    function GeneratePayload: string;
    function GetError(Data: string): GenericError;
    procedure ClearError;
    function isValidToken: boolean;
    function GetToken: boolean;
    function GetServices: TEsthimaServices;
    function CreateConvention: string;
    function GeneratePDF(id_clinique, id_convention: string): string;
    function Connect(): boolean;
  end;


function GetSubPayload(var m: TmemDataset; i: integer): string;

implementation

uses unit1;

//Fonction formattage date JSON
function FormatJSONDate(a: TdateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss"Z"', a);
end;


//Récupération des infos des MemDatasets
function GetSubPayload(var m: TmemDataset; i: integer): string;
var
  s: string;
  fs: TFormatSettings;
begin
  Result := '';
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := '';
  if i > m.FieldDefs.Count - 1 then
    exit;
  s := format('"%s":', [m.FieldDefs[i].Name]);
  with m do
    case FieldDefs[i].DataType of
      ftString: s := s + format(BoolTostr(FieldDefs[i].Name = 'products',
          '[%s]', '"%s"'), [FieldByName(FieldDefs[i].Name).AsString]);
      ftBoolean: s := s + format('%s',
          [lowercase(BoolTostr(FieldByName(FieldDefs[i].Name).AsBoolean, True))]);
      ftInteger: s := s + format('%s', [FieldByName(FieldDefs[i].Name).AsString]);
      ftDateTime: s := s + format('"%s"',
          [FormatJSONDate(FieldByName(FieldDefs[i].Name).AsDateTime)]);
      ftFloat: s := s + format('%s',
          [FormatFloat('0.00', FieldByName(FieldDefs[i].Name).AsFloat, fs)]);
    end;
  s := s + BoolTostr(i = m.FieldDefs.Count - 1, '', ',' + #13);
  Result := s;
end;

{ TEsthima }

function TEsthima.GetError(Data: string): GenericError;
var
  a: TJSonData;
begin
  if not isValidJson(Data) then
    exit;
  a := GetJSon(Data);
  with Result do
  begin
    code := a.getValue('code');
    message := a.getValue('error');
    answer := Data;
  end;
end;

constructor TEsthima.Create(Sender: TObject);
begin
  curToken.Value := '';
  E_Convention_DS := TMemDataSet.Create(nil);
  E_Animal_DS := TMemDataSet.Create(nil);
end;

destructor TEsthima.Destroy;
begin
  if Assigned(E_Convention_DS) then
    FreeAndNil(E_Convention_DS);
  if Assigned(E_Animal_DS) then
    FreeAndNil(E_Animal_DS);
  inherited;
end;


procedure CreateField(AMemData: TDataSet; AFieldName: string;
  AFieldType: TFieldType; DataSize: int64 = -1);
begin
  if (AMemData = nil) or (AFieldName = '') then
    Exit;
  with AmemData do
    if AfieldType = ftString then
      FieldDefs.Add(AfieldName, AfieldType, DataSize)
    else
      FieldDefs.Add(AfieldName, AfieldType);
end;

procedure TEsthima.PopulateDataSets;
begin

  //DSconvention
  if (E_marque = 'incineris_esthima') or (E_Marque = 'incineris_regular') then
  begin
    CreateField(E_Convention_DS, 'ownerCivility', ftString, 3);
    CreateField(E_Convention_DS, 'ownerLastname', ftString, 50);
    CreateField(E_Convention_DS, 'ownerFirstname', ftString, 50);
    CreateField(E_Convention_DS, 'ownerAddressFirstLine', ftString, 50);
    CreateField(E_Convention_DS, 'ownerAddressSecondLine', ftString, 50);
    CreateField(E_Convention_DS, 'ownerAddressThirdLine', ftString, 50);
    CreateField(E_Convention_DS, 'ownerPostalCode', ftString, 20);
    CreateField(E_Convention_DS, 'ownerCity', ftString, 50);
    CreateField(E_Convention_DS, 'ownerPhone', ftString, 20);
    CreateField(E_Convention_DS, 'ownerEmail', ftString, 100);
    CreateField(E_Convention_DS, 'ownerContactMethod', ftString, 10);
    CreateField(E_Convention_DS, 'offerPresenceType', ftBoolean);
    CreateField(E_Convention_DS, 'offerPresentAt', ftDateTime);
    CreateField(E_Convention_DS, 'ownerStatus', ftString, 10);
    CreateField(E_Convention_DS, 'offerRepatriationChoice', ftString, 20);
    CreateField(E_Convention_DS, 'offerRepatriationOther', ftString, 50);
  end;
  CreateField(E_Convention_DS, 'country', ftString, 2);
  CreateField(E_Convention_DS, 'offer', ftString, 20);
  CreateField(E_Convention_DS, 'products', ftString, 500);
  CreateField(E_Convention_DS, 'clinicReference', ftString, 50);
  CreateField(E_Convention_DS, 'clinicName', ftString, 100);
  CreateField(E_Convention_DS, 'clinicAddress', ftString, 255);
  CreateField(E_Convention_DS, 'clinicPostalCode', ftString, 20);
  CreateField(E_Convention_DS, 'clinicCity', ftString, 50);
  CreateField(E_Convention_DS, 'clinicEmail', ftString, 100);
  CreateField(E_Convention_DS, 'clinicPhone', ftString, 20);
  CreateField(E_Convention_DS, 'veterinaryLastname', ftString, 50);
  CreateField(E_Convention_DS, 'veterinaryFirstname', ftString, 50);
  CreateField(E_Convention_DS, 'incCode', ftString, 50);
  CreateField(E_Convention_DS, 'target', ftString, 10);
  CreateField(E_Convention_DS, 'termsOfUse', ftBoolean);
  E_Convention_DS.CreateTable;

  //DSAnimal
  CreateField(E_Animal_DS, 'name', ftString, 50);
  CreateField(E_Animal_DS, 'weight', ftFloat);
  CreateField(E_Animal_DS, 'age', ftInteger);
  CreateField(E_Animal_DS, 'gender', ftString, 20);
  CreateField(E_Animal_DS, 'deathCause', ftString, 20);
  CreateField(E_Animal_DS, 'deathAt', ftDateTime);
  CreateField(E_Animal_DS, 'functionalId', ftString, 50);
  CreateField(E_Animal_DS, 'exoticBreed', ftString, 50);
  CreateField(E_Animal_DS, 'breed', ftInteger, 50);
  CreateField(E_Animal_DS, 'country', ftString, 2);
  E_Animal_DS.CreateTable;

  with E_Convention_DS do
  begin
    Open;
    Insert;
    Post;
    Close;
  end;

  with E_Animal_DS do
  begin
    Open;
    Insert;
    Post;
    Close;
  end;

end;

function TEsthima.GeneratePayload: string;
var
  s, t: string;
var
  i: integer;
begin
  Result := '';
  try
    s := '';
    with E_Convention_DS do
    begin
      Open;
      First;
      if EOF and BOF then
        exit;
      for i := 0 to FieldDefs.Count - 1 do
        s := s + GetSubPayload(E_Convention_DS, i);
      Close;
    end;
    t := '';
    with E_Animal_DS do
    begin
      Open;
      First;
      if EOF and BOF then
        exit;
      for i := 0 to FieldDefs.Count - 1 do
        t := t + GetSubPayload(E_Animal_DS, i);
      Close;
    end;
    s := format('{%s,' + #13 + '"animal":{%s}' + #13 + '}', [s, t]);

  finally
    Result := s;
  end;
end;


procedure TEsthima.ClearError;
begin
  with E_Error do
  begin
    code := '';
    message := '';
    informations := '';
  end;
end;

function TEsthima.isValidToken: boolean;
begin
  Result := (SecondsBetween(now, CurToken.expiration) > 0) and (CurToken.Value <> '');
end;

function TEsthima.GetToken: boolean;
var
  a: GenericQuery;
var
  b: GenericResponse;
var
  Js: TJSonData;
begin
  Result := False;
  ClearError;
  // Vérification si un token n'est pas déjà présent et valide
  if IsValidToken then
  begin
    Result := True;
    exit;
  end;
  // Aucun token, ou token expiré
  a := DefGenericQuery;
  with a do
  begin
    queryURL := ApiURL + '/api/fr/login';
    timeout := 1000;
    headers := 'X-Hacker: Jabouley Florent' + #13 +
      'X-Infos: A celui qui me lit, bien le bonjour :) Je savais pas quoi mettre dans ce header...';
    method := 'POST';
    body := format('{"username": "%s","password": "%s"}', [E_login, E_pw]);
  end;
  b := GenericHTTPRequest(a);
  with b do
    if Code = 201 then
    begin

      if IsValidJSON(body) then
      begin
        JS := GetJson(Body);
        Curtoken.Value := (JS.GetValue('token'));

      end;
      CurToken.expiration := IncSecond(now, TokenTimeOut);
      Result := True;
    end
    else
      E_Error := GetError(b.body);
end;

function TEsthima.GetServices: TEsthimaServices;
{
GET /api/fr/clinics/{clinic_id}/rests
Headers Authorization: Bearer token(celui récupéré via le POST login)
Accept: application/json;
charset=utf-8
Content-Type: application/json
}

var
  Js: TJSonData;
  JsArr: TJsonArray;
var
  i: integer;
var
  a: GenericQuery;
var
  b: GenericResponse;
begin
  ClearError;
  if not GetToken then
    exit;
  a := DefGenericQuery;
  with a do
  begin
    timeout := 1000;
    queryURL := ApiURL + format('/api/fr/clinics/%s/rests', [E_Clinic_Id]);
    method := 'GET';
    headers := StringReplace(GenericHeader, '{token}', CurToken.Value, [rfReplaceAll]);
    body := '';
  end;
  b := GenericHTTPRequest(a);
  if b.code = 200 then
  begin
    if IsValidJSON(b.body) then
    begin
      JS := GetJson(b.Body);
      with Result do
      begin
        Name := JS.GetValue('name');
        functional_id := JS.GetValue('functional_id');
        if E_Clinic_Id <> functional_id then
          E_Clinic_Id := functional_id;
        JSArr := TJSonArray(JS.FindPath('services'));
        setLength(Services, JSArr.Count);
        for i := 0 to JSArr.Count - 1 do
        begin
          Services[i] := JSarr[i].AsString;
        end;
      end;
    end;
  end
  else
    E_Error := GetError(b.body);
end;

function TEsthima.CreateConvention: string;
{
POST /api/fr/versions/v2/clinics/{clinic_id}/conventions/rests
Headers Authorization: Bearer token (celui récupéré via le POST login)
Accept:application/json;
charset=utf-8
Content-Type: application/json
}
var
  Js: TJSonData;
  JsArr: TJsonArray;
var
  i: integer;
var
  a: GenericQuery;
var
  b: GenericResponse;
begin
  Result := '';
  ClearError;
  if not GetToken then
    exit;
  a := DefGenericQuery;
  with a do
  begin
    timeout := 5000;
    queryURL := ApiURL + format('/api/fr/versions/v2/clinics/%s/conventions/rests',
      [E_Clinic_Id]);
    method := 'POST';
    headers := StringReplace(GenericHeader, '{token}', CurToken.Value, [rfReplaceAll]);
    body := GeneratePayload;
    log(headers);
    log(body);
  end;

  b := GenericHTTPRequest(a);

  if b.code = 201 then
  begin
    if IsValidJSON(b.body) then
    begin
      JS := GetJson(b.Body);
      Result := JS.GetValue('functional_id');
    end;
  end
  else
    E_Error := GetError(b.body);
end;



function TEsthima.GeneratePDF(id_clinique, id_convention: string): string;
var
  Js: TJSonData;
  SLsave: TStringList;
  JsArr: TJsonArray;
  PDFName, PDFData: string;
var
  i: integer;
  time1, time2: TdateTime;
var
  a: GenericQuery;
var
  b: GenericResponse;
begin
  Result := '';
  ClearError;
  if not GetToken then
    exit;
  a := DefGenericQuery;
  with a do
  begin
    timeout := 5000;
    queryURL := ApiURL + format('/api/fr/clinics/%s/conventions/%s/prints',
      [id_clinique, id_convention]);
    method := 'GET';
    headers := StringReplace(GenericHeader, '{token}', CurToken.Value, [rfReplaceAll]);
    body := '';
  end;
  time1 := now;
  b := GenericHTTPRequest(a);
  time2 := now;
  log(MilliSecondsBetween(time1, time2).toString);
  SLSave := TStringList.Create;
  try
    SLSave.Text := b.body;
    SLsave.savetofile(ExtractFilePath(ParamStr(0)) + 'PDFQuerySave.txt');
  finally
    SLsave.Free;
  end;

  if b.code = 200 then
  begin
    if IsValidJSON(b.body) then
    begin
      JS := GetJson(b.Body);
      PDFName := ExtractFilePath(ParamStr(0)) + JS.GetValue('functional_id') + '.pdf';
      PDFData := JS.GetValue('pdf_content');
      try
        ConvertB64StrToFile(PDFData, PDFName);
        Result := PDFName;
      finally
      end;
    end;
  end
  else
    E_Error := GetError(b.body);
end;

function TEsthima.Connect(): boolean;
var
  SV: TEsthimaservices;
begin
  Result := False;
  try
    SV := GetServices;
    if length(sv.services) > 0 then E_marque := sv.services[0];
    PopulateDataSets;
    Result := True;
  finally
  end;
end;




end.
