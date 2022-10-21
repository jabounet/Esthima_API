unit GenericHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, synautil,
  FpJson, JSonParser, DCPsha1, DCPsha256, httpsend, lclintf, ExtCtrls, Grids,
  Menus, BCButton, strutils, inifiles, DateUtils, syncobjs, Types, Windows,
  blcksock, base64, ssl_openssl;

type
GenericQuery = record
  method: string;
  mimeType: string;
  queryURL: string;
  body: string;
  timestamp: string;
  signature: string;
  headers: string;
  keepalive: boolean;
  timeout: integer;
end;

type
GenericResponse = record
  Body: string;
  Headers: string;
  Code: integer;
end;


var
DefGenericQuery: GenericQuery = (method: '';
mimeType: '';
queryURL: '';
body: '';
timestamp: '';
signature: '';
headers: '';
keepalive: False;
timeout: 1500; );


function CreateGenericQuery(m, q, b, h: string): GenericQuery;
function GenericHTTPRequest(Query: GenericQuery): GenericResponse;
function getsha1hash(S: string): string;
function IsValidJSON(JsonStr: string): boolean;
// Conversion base64string -> fichier
function ConvertB64StrToFile(Base64,AFile: String): boolean;

{ TJsonDataHelper }
type
TJsonDataHelper = class helper for TJsonData
  function GetValue(path: string): string;
end;

implementation

{ TJsonDataHelper }

function TJsonDataHelper.GetValue(path: string): string;
begin
  Result := '';
  if Self.FindPath(path) <> nil then
    Result := Self.FindPath(path).AsString;
end;

function IsValidJSON(JsonStr: string): boolean;
var
  Data: TJSONData;
begin
  Result := True;
  if length(JsonStr) = 0 then
    begin
    Result := False;
    exit;
    end;
    try
    Data := GetJSON(JSONstr);
    except
    On E: ejsonparser do
      begin
      Result := False;
      end;
    end;
  Data.Free;
end;


function ConvertB64StrToFile(Base64,AFile: String): boolean;
var
  MS: TMemoryStream;
  Str: String;
begin
  Result := False;
  MS := TMemoryStream.Create;
  try
    Str := DecodeStringBase64(Base64);
    MS.Write(Str[1], Length(Str) div SizeOf(Char));
    MS.Position := 0;
    MS.SaveToFile(AFile);
    sleep(500);
    Result := FileExists(AFile);
  finally
    MS.Free;
  end;
end;


// Création d'une requête générique
function CreateGenericQuery(m, q, b, h: string): GenericQuery;
begin
  Result := DefGenericQuery;
  with Result do
    begin
    method := m;
    queryURL := q;
    body := b;
    headers := h;
    end;
end;

// Encodage SHA1 pour la signature
function getsha1hash(S: string): string;
var
  Hash: TDCP_SHA1;
  Digest: array[0..31] of byte;
  i: integer;
  hashstr: string;
begin
  Result := '';
  //TMBCSEncoding.Create;
  if S <> '' then
    begin
    Hash := TDCP_SHA1.Create(nil);
    try
    Hash.Init;
    Hash.UpdateStr(S);
    Hash.Final(Digest);
    hashstr := '';
    for i := 0 to 19 do
      hashstr := hashstr + lowercase(IntToHex(Digest[i], 2));
    Result := (hashstr);
   finally
    Hash.Free;
    end;
    end;

end;

// Fonction globale HTTP (GET/POST)
function GenericHTTPRequest(Query: GenericQuery): GenericResponse;
var
  Response: TStringList;
  URL: string;
  HTTP: THTTPSend;
  ParseResponse: TJSONData;
  Body: TStringList;
begin
  Result.Body := '';
  Result.Headers := '';
  Result.Code := -1;

  Response := TStringList.Create;
  HTTP := THTTPSend.Create;
    try
    URL := Query.queryURL;
    HTTP.Document.Clear;
    HTTP.Headers.Clear;
    HTTP.Headers.Text := Query.headers;

    if length(Query.body) > 0 then
      begin
      WriteStrToStream(HTTP.Document, Query.body);
      end;
    HTTP.mimeType := (Query.mimeType);
    HTTP.Timeout := Query.timeout;
    HTTP.Sock.ConnectionTimeout := Query.TimeOut;
    HTTP.KeepAlive := Query.keepalive;
    HTTP.KeepAliveTimeOut := Query.Timeout div 1000;
    HTTP.HTTPMethod(query.method, URL);
    Response.LoadFromStream(HTTP.Document);
    Result.Code := HTTP.ResultCode;
    Result.Headers := HTTP.Headers.Text;
    Result.Body := StringReplace(Response.Text, CRLF, '', [RfReplaceAll]);

    finally
    Response.Free;
    HTTP.Free;
    end;
end;

end.

