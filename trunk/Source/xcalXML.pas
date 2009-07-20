unit xcalXML;

{$I xcalDefs.inc}

interface

uses
  Windows, SysUtils, Classes
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TxcalInvalidXMLException = class(Exception);

  TxcalXMLItem = class(TObject)
  private
    FItems: TList;               
    FName: String;               
    FParent: TxcalXMLItem;       
    FText: String;               
    FValue: String;
    function GetCount: Integer;
    function GetItems(Index: Integer): TxcalXMLItem;
    function GetProp(Index: String): String;
    procedure SetProp(Index: String; const Value: String);
  public
    destructor Destroy; override;
    procedure AddItem(Item: TxcalXMLItem);
    procedure Clear;
    procedure InsertItem(Index: Integer; Item: TxcalXMLItem);

    function Add: TxcalXMLItem;
    function Find(const Name: String): Integer;
    function FindItem(const Name: String): TxcalXMLItem;
    function IndexOf(Item: TxcalXMLItem): Integer;
    function PropExists(const Index: String): Boolean;
    function Root: TxcalXMLItem;
    procedure DeleteProp(const Index: String);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TxcalXMLItem read GetItems; default;
    property Name: String read FName write FName;
    property Parent: TxcalXMLItem read FParent;
    property Prop[Index: String]: String read GetProp write SetProp;
    property Text: String read FText write FText;
    property Value: String read FValue write FValue;
  end;

  TxcalXMLDocument = class(TObject)
  private
    FAutoIndent: Boolean;
    FRoot: TxcalXMLItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);

    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
    property Root: TxcalXMLItem read FRoot;
  end;

  TxcalXMLReader = class(TObject)
  private
    FBuffer: PAnsiChar;
    FBufPos: Integer;
    FBufEnd: Integer;
    FPosition: Int64;
    FSize: Int64;
    FStream: TStream;
    procedure SetPosition(const Value: Int64);
    procedure ReadBuffer;
    procedure ReadItem(var {$IFDEF Delphi12}NameS{$ELSE}Name{$ENDIF}, Text: String);
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure RaiseException;
    procedure ReadHeader;
    procedure ReadRootItem(Item: TxcalXMLItem);
    property Position: Int64 read FPosition write SetPosition;
    property Size: Int64 read FSize;
  end;

  TxcalXMLWriter = class(TObject)
  private
    FAutoIndent: Boolean;
    FBuffer: AnsiString;
    FStream: TStream;
    procedure FlushBuffer;
    procedure WriteLn(const s: AnsiString);
    procedure WriteItem(Item: TxcalXMLItem; Level: Integer = 0);
  public
    constructor Create(Stream: TStream);
    procedure WriteHeader;
    procedure WriteRootItem(RootItem: TxcalXMLItem);
  end;

function xcalStrToXML(const s: String): String;
function xcalValueToXML(const Value: Variant): String;
function xcalXMLToStr(const s: String): String;


implementation

{$IFNDEF Delphi6}
uses
  xcalD5Utils;
{$ENDIF}

function xcalStrToXML(const s: String): String;
const
  SpecChars = ['<', '>', '"', #10, #13, '&'];
var
  i, lenRes, resI, ch: Integer;
  pRes: PChar;

  procedure ReplaceChars(var s: String; i: Integer);
  begin
    Insert('#' + IntToStr(Ord(s[i])) + ';', s, i + 1);
    s[i] := '&';
  end;

begin
  lenRes := Length(s);

  if lenRes < 32 then
  begin
    Result := s;
    for i := lenRes downto 1 do
{$IFDEF Delphi12}
      if CharInSet(s[i], SpecChars) then
{$ELSE}
      if s[i] in SpecChars then
{$ENDIF}
        if s[i] <> '&' then
          ReplaceChars(Result, i)
        else
        begin
          if Copy(s, i + 1, 5) = 'quot;' then
          begin
            Delete(Result, i, 6);
            Insert('&#34;', Result, i);
          end;
        end;
    Exit;
  end;

  SetLength(Result, lenRes);
  pRes := PChar(Result) - 1;
  resI := 1;
  i := 1;

  while i <= Length(s) do
  begin
    if resI + 5 > lenRes then
    begin
      Inc(lenRes, 256);
      SetLength(Result, lenRes);
      pRes := PChar(Result) - 1;
    end;

{$IFDEF Delphi12}
    if CharInSet(s[i], SpecChars) then
{$ELSE}
    if s[i] in SpecChars then
{$ENDIF}
    begin
      if (s[i] = '&') and (i <= Length(s) - 5) and (s[i + 1] = 'q') and
        (s[i + 2] = 'u') and (s[i + 3] = 'o') and (s[i + 4] = 't') and (s[i + 5] = ';') then
      begin
        pRes[resI] := '&';
        pRes[resI + 1] := '#';
        pRes[resI + 2] := '3';
        pRes[resI + 3] := '4';
        pRes[resI + 4] := ';';
        Inc(resI, 4);
        Inc(i, 5);
      end
      else
      begin
        pRes[resI] := '&';
        pRes[resI + 1] := '#';

        ch := Ord(s[i]);
        if ch < 10 then
        begin
          pRes[resI + 2] := Chr(ch + $30);
          Inc(resI, 3);
        end
        else if ch < 100 then
        begin
          pRes[resI + 2] := Chr(ch div 10 + $30);
          pRes[resI + 3] := Chr(ch mod 10 + $30);
          Inc(resI, 4);
        end
        else
        begin
          pRes[resI + 2] := Chr(ch div 100 + $30);
          pRes[resI + 3] := Chr(ch mod 100 div 10 + $30);
          pRes[resI + 4] := Chr(ch mod 10 + $30);
          Inc(resI, 5);
        end;
        pRes[resI] := ';';
      end;
    end
    else
      pRes[resI] := s[i];
    Inc(resI);
    Inc(i);
  end;

  SetLength(Result, resI - 1);
end;

function xcalXMLToStr(const s: String): String;
var
  i, j, h, n: Integer;
begin
  Result := s;

  i := 1;
  n := Length(s);
  while i < n do
  begin
    if Result[i] = '&' then
      if (i + 3 <= n) and (Result[i + 1] = '#') then
      begin
        j := i + 3;
        while Result[j] <> ';' do
          Inc(j);
        h := StrToInt(String(Copy(Result, i + 2, j - i - 2)));
        Delete(Result, i, j - i);
        Result[i] := Chr(h);
        Dec(n, j - i);
      end
      else if Copy(Result, i + 1, 5) = 'quot;' then
      begin
        Delete(Result, i, 5);
        Result[i] := '"';
        Dec(n, 5);
      end;
    Inc(i);
  end;
end;

function xcalValueToXML(const Value: Variant): String;
begin
  case TVarData(Value).VType of
    varSmallint, varInteger, varByte:
      Result := IntToStr(Value);

    varSingle, varDouble, varCurrency:
      Result := FloatToStr(Value);

    varDate:
      Result := DateToStr(Value);

    varOleStr, varString, varVariant{$IFDEF Delphi12}, varUString{$ENDIF}:
      Result := xcalStrToXML(Value);

    varBoolean:
      if Value = True then Result := '1' else Result := '0';

    else
      Result := '';
  end;
end;


{ TxcalXMLItem }

destructor TxcalXMLItem.Destroy;
begin
  Clear;
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  inherited;
end;

procedure TxcalXMLItem.Clear;
begin
  if FItems <> nil then
  begin
    while FItems.Count > 0 do
      TxcalXMLItem(FItems[0]).Free;
    FItems.Free;
    FItems := nil;
  end;
end;

function TxcalXMLItem.GetItems(Index: Integer): TxcalXMLItem;
begin
  Result := TxcalXMLItem(FItems[Index]);
end;

function TxcalXMLItem.GetCount: Integer;
begin
  if FItems = nil then
    Result := 0 else
    Result := FItems.Count;
end;

function TxcalXMLItem.Add: TxcalXMLItem;
begin
  Result := TxcalXMLItem.Create;
  AddItem(Result);
end;

procedure TxcalXMLItem.AddItem(Item: TxcalXMLItem);
begin
  if FItems = nil then
    FItems := TList.Create;

  FItems.Add(Item);
  if Item.FParent <> nil then
    Item.FParent.FItems.Remove(Item);
  Item.FParent := Self;
end;

procedure TxcalXMLItem.InsertItem(Index: Integer; Item: TxcalXMLItem);
begin
  AddItem(Item);
  FItems.Delete(FItems.Count - 1);
  FItems.Insert(Index, Item);
end;

function TxcalXMLItem.Find(const Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].Name, Name) = 0 then
    begin
      Result := i;
      break;
    end;
end;

function TxcalXMLItem.FindItem(const Name: String): TxcalXMLItem;
var
  i: Integer;
begin
  i := Find(Name);
  if i = -1 then
  begin
    Result := Add;
    Result.Name := Name;
  end
  else
    Result := Items[i];
end;

function TxcalXMLItem.Root: TxcalXMLItem;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TxcalXMLItem.GetProp(Index: String): String;
var
  i: Integer;
begin
  i := Pos(' ' + AnsiUppercase(Index) + '="', AnsiUppercase(' ' + FText));
  if i <> 0 then
  begin
    Result := Copy(FText, i + Length(Index + '="'), MaxInt);
    Result := xcalXMLToStr(Copy(Result, 1, Pos('"', Result) - 1));
  end
  else
    Result := '';
end;

procedure TxcalXMLItem.SetProp(Index: String; const Value: String);
var
  i, j: Integer;
  s: String;
begin
  i := Pos(' ' + AnsiUppercase(Index) + '="', AnsiUppercase(' ' + FText));
  if i <> 0 then
  begin
    j := i + Length(Index + '="');
    while (j <= Length(FText)) and (FText[j] <> '"') do
      Inc(j);
    Delete(FText, i, j - i + 1);
  end
  else
    i := Length(FText) + 1;

  s := Index + '="' + xcalStrToXML(Value) + '"';
  if (i > 1) and (FText[i - 1] <> ' ') then
    s := ' ' + s;
  Insert(s, FText, i);
end;

function TxcalXMLItem.PropExists(const Index: String): Boolean;
begin
  Result := Pos(' ' + AnsiUppercase(Index) + '="', ' ' + AnsiUppercase(FText)) > 0;
end;

procedure TxcalXMLItem.DeleteProp(const Index: String);
var
  i: Integer;
begin
  i := Pos(' ' + AnsiUppercase(Index) + '="', ' ' + AnsiUppercase(FText));
  if i > 0 then
  begin
    SetProp(Index, '');
    Delete(FText, i, Length(Index) + 4);
  end;
end;

function TxcalXMLItem.IndexOf(Item: TxcalXMLItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;


{ TxcalXMLDocument }

constructor TxcalXMLDocument.Create;
begin
  FRoot := TxcalXMLItem.Create;
end;

destructor TxcalXMLDocument.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TxcalXMLDocument.Clear;
begin
  FRoot.Clear;
end;

procedure TxcalXMLDocument.LoadFromStream(Stream: TStream);
var
  rd: TxcalXMLReader;
begin
  rd := TxcalXMLReader.Create(Stream);
  try
    FRoot.Clear;
    rd.ReadHeader;
    rd.ReadRootItem(FRoot);
  finally
    rd.Free;
  end;
end;

procedure TxcalXMLDocument.SaveToStream(Stream: TStream);
var
  wr: TxcalXMLWriter;
begin
  wr := TxcalXMLWriter.Create(Stream);
  wr.FAutoIndent := FAutoIndent;

  try
    wr.WriteHeader;
    wr.WriteRootItem(FRoot);
  finally
    wr.Free;
  end;
end;

procedure TxcalXMLDocument.LoadFromFile(const FileName: String);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(s);
end;

procedure TxcalXMLDocument.SaveToFile(const FileName: String);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName + '.tmp', fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;

  DeleteFile(FileName);
  RenameFile(FileName + '.tmp', FileName);
  LoadFromFile(FileName);
end;


{ TxcalXMLReader }

constructor TxcalXMLReader.Create(Stream: TStream);
begin
  FStream := Stream;
  FSize := Stream.Size;
  FPosition := Stream.Position;
  GetMem(FBuffer, 4096);
end;

destructor TxcalXMLReader.Destroy;
begin
  FreeMem(FBuffer, 4096);
  FStream.Position := FPosition;
  inherited;
end;

procedure TxcalXMLReader.ReadBuffer;
begin
  FBufEnd := FStream.Read(FBuffer^, 4096);
  FBufPos := 0;
end;

procedure TxcalXMLReader.SetPosition(const Value: Int64);
begin
  FPosition := Value;
  FStream.Position := Value;
  FBufPos := 0;
  FBufEnd := 0;
end;

procedure TxcalXMLReader.RaiseException;
begin
  raise TxcalInvalidXMLException.Create('Invalid file format');
end;

procedure TxcalXMLReader.ReadHeader;
var
  s1, s2: String;
begin
  ReadItem(s1, s2);
  if Pos('?xml', s1) <> 1 then
    RaiseException;
end;

procedure TxcalXMLReader.ReadItem(var {$IFDEF Delphi12}NameS{$ELSE}Name{$ENDIF}, Text: String);
var
  c: Integer;
  curpos, len: Integer;
  state: (FindLeft, FindRight, FindComment, Done);
  i, comment: Integer;
  ps: PAnsiChar;
{$IFDEF Delphi12}
  Name: AnsiString;
{$ENDIF}
begin
  Text := '';
  comment := 0;
  state := FindLeft;
  curpos := 0;
  len := 4096;
  SetLength(Name, len);
  ps := @Name[1];

  while FPosition < FSize do
  begin
    if FBufPos = FBufEnd then
      ReadBuffer;
    c := Ord(FBuffer[FBufPos]);
    Inc(FBufPos);
    Inc(FPosition);

    if state = FindLeft then
    begin
      if c = Ord('<') then
        state := FindRight
    end
    else if state = FindRight then
    begin
      if c = Ord('>') then
      begin
        state := Done;
        break;
      end
      else if c = Ord('<') then
        RaiseException
      else
      begin
        ps[curpos] := AnsiChar(Chr(c));
        Inc(curpos);
        if (curpos = 3) and (Pos(AnsiString('!--'), Name) = 1) then
        begin
          state := FindComment;
          comment := 0;
          curpos := 0;
        end;
        if curpos >= len - 1 then
        begin
          Inc(len, 4096);
          SetLength(Name, len);
          ps := @Name[1];
        end;
      end;
    end
    else if State = FindComment then
    begin
      if comment = 2 then
      begin
        if c = Ord('>') then
          state := FindLeft
        else
          comment := 0;
      end
      else begin
        if c = Ord('-') then
          Inc(comment)
        else
          comment := 0;
      end;
    end;
  end;

  len := curpos;
  SetLength(Name, len);

  if state = FindRight then
    RaiseException;
  if (Name <> '') and (Name[len] = ' ') then
    SetLength(Name, len - 1);

  i := Pos(AnsiString(' '), Name);
  if i <> 0 then
  begin
    Text := UTF8Decode(Copy(Name, i + 1, len - i));
    Delete(Name, i, len - i + 1);
  end;
{$IFDEF Delphi12}
    NameS := String(Name);
{$ENDIF}
end;

procedure TxcalXMLReader.ReadRootItem(Item: TxcalXMLItem);
var
  LastName: String;

  function DoRead(RootItem: TxcalXMLItem): Boolean;
  var
    n: Integer;
    ChildItem: TxcalXMLItem;
    Done: Boolean;
  begin
    Result := False;
    ReadItem(RootItem.FName, RootItem.FText);
    LastName := RootItem.FName;

    if (RootItem.Name = '') or (RootItem.Name[1] = '/') then
    begin
      Result := True;
      Exit;
    end;

    n := Length(RootItem.Name);
    if RootItem.Name[n] = '/' then
    begin
      SetLength(RootItem.FName, n - 1);
      Exit;
    end;

    n := Length(RootItem.Text);
    if (n > 0) and (RootItem.Text[n] = '/') then
    begin
      SetLength(RootItem.FText, n - 1);
      Exit;
    end;

    repeat
      ChildItem := TxcalXMLItem.Create;
      Done := DoRead(ChildItem);
      if not Done then
        RootItem.AddItem(ChildItem) else
        ChildItem.Free;
    until Done;

    if (LastName <> '') and (AnsiCompareText(LastName, '/' + RootItem.Name) <> 0) then
      RaiseException;
  end;

begin
  DoRead(Item);
end;


{ TxcalXMLWriter }

constructor TxcalXMLWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TxcalXMLWriter.FlushBuffer;
begin
  if FBuffer <> '' then
    FStream.Write(FBuffer[1], Length(FBuffer));
  FBuffer := '';
end;

procedure TxcalXMLWriter.WriteLn(const s: AnsiString);
begin
  if not FAutoIndent then
    Insert(s, FBuffer, MaxInt) else
    Insert(s + #13#10, FBuffer, MaxInt);
  if Length(FBuffer) > 4096 then
    FlushBuffer;
end;

procedure TxcalXMLWriter.WriteHeader;
begin
  WriteLn('<?xml version="1.0" encoding="utf-8"?>');
end;

function Dup(n: Integer): AnsiString;
begin
  SetLength(Result, n);
  FillChar(Result[1], n, ' ');
end;

procedure TxcalXMLWriter.WriteItem(Item: TxcalXMLItem; Level: Integer = 0);
var
  s: AnsiString;
begin
  if (Item.FText <> '') then
  begin
    s := UTF8Encode(Item.FText);
    if (s = '') or (s[1] <> ' ') then
      s := ' ' + s;
  end
  else
    s := '';

  if Item.Count = 0 then
  begin
    if Item.Value = '' then
      s := s + '/>'
    else
      s := s + '>' + UTF8Encode(Item.Value) + '</' + AnsiString(Item.Name) + '>'
  end
  else
    s := s + '>';
  if not FAutoIndent then
    s := '<' + AnsiString(Item.Name) + s else
    s := Dup(Level) + '<' + AnsiString(Item.Name) + s;
  WriteLn(s);
end;

procedure TxcalXMLWriter.WriteRootItem(RootItem: TxcalXMLItem);

  procedure DoWrite(RootItem: TxcalXMLItem; Level: Integer = 0);
  var
    i: Integer;
  begin
    if not FAutoIndent then
      Level := 0;

    WriteItem(RootItem, Level);
    for i := 0 to RootItem.Count - 1 do
      DoWrite(RootItem[i], Level + 2);
    if RootItem.Count > 0 then
      if not FAutoIndent then
        WriteLn('</' + AnsiString(RootItem.Name) + '>') else
        WriteLn(Dup(Level) + '</' + AnsiString(RootItem.Name) + '>');
  end;

begin
  DoWrite(RootItem);
  FlushBuffer;
end;

end.
