unit xcalD5Utils;

{$I xcalDefs.inc}

interface

{$IFNDEF Delphi6}
function Utf8Encode(const WS: WideString): String;
function UTF8Decode(const S: String): WideString;
function AnsiToUtf8(const S: String): String;
function Utf8ToAnsi(const S: String): String;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
{$ENDIF}

implementation

uses
  SysUtils;
  
{$IFNDEF Delphi6}
function Utf8Encode(const WS: WideString): String;
var
  L: Integer;
  Temp: String;

  function ToUtf8(Dest: PChar; MaxDestBytes: Cardinal; 
           Source: PWideChar; SourceChars: Cardinal): Cardinal;
  var
    i, count: Cardinal;
    c: Cardinal;
  begin
    Result := 0;
    if Source = nil then Exit;
    count := 0;
    i := 0;
    if Dest <> nil then
    begin
      while (i < SourceChars) and (count < MaxDestBytes) do
      begin
        c := Cardinal(Source[i]);
        Inc(i);
        if c <= $7F then
        begin
          Dest[count] := Char(c);
          Inc(count);
        end
        else if c > $7FF then
        begin
          if count + 3 > MaxDestBytes then
            break;
          Dest[count] := Char($E0 or (c shr 12));
          Dest[count+1] := Char($80 or ((c shr 6) and $3F));
          Dest[count+2] := Char($80 or (c and $3F));
          Inc(count,3);
        end
        else //  $7F < Source[i] <= $7FF
        begin
          if count + 2 > MaxDestBytes then
            break;
          Dest[count] := Char($C0 or (c shr 6));
          Dest[count+1] := Char($80 or (c and $3F));
          Inc(count,2);
        end;
      end;
      if count >= MaxDestBytes then count := MaxDestBytes-1;
      Dest[count] := #0;
    end
    else
    begin
      while i < SourceChars do
      begin
        c := Integer(Source[i]);
        Inc(i);
        if c > $7F then
        begin
          if c > $7FF then
            Inc(count);
          Inc(count);
        end;
        Inc(count);
      end;
    end;
    Result := count+1; 
  end;

begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3);
  L := ToUtf8(PChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: String): WideString;
var
  L: Integer;
  Temp: WideString;

  function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
  var
    i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
    if Source = nil then
    begin
      Result := 0;
      Exit;
    end;
    Result := Cardinal(-1);
    count := 0;
    i := 0;
    if Dest <> nil then
    begin
      while (i < SourceBytes) and (count < MaxDestChars) do
      begin
        wc := Cardinal(Source[i]);
        Inc(i);
        if (wc and $80) <> 0 then
        begin
          wc := wc and $3F;
          if i > SourceBytes then Exit;           // incomplete multibyte char
          if (wc and $20) <> 0 then
          begin
            c := Byte(Source[i]);
            Inc(i);
            if (c and $C0) <> $80 then  Exit;     // malformed trail byte or out of range char
            if i > SourceBytes then Exit;         // incomplete multibyte char
            wc := (wc shl 6) or (c and $3F);
          end;
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;       // malformed trail byte

          Dest[count] := WideChar((wc shl 6) or (c and $3F));
        end
        else
          Dest[count] := WideChar(wc);
        Inc(count);
      end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
    end
    else
    begin
    while (i <= SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
      if (c and $F0) = $F0 then Exit;  // too many bytes for UCS2
      if (c and $40) = 0 then Exit;    // malformed lead byte
      if i > SourceBytes then Exit;         // incomplete multibyte char

      if (Byte(Source[i]) and $C0) <> $80 then Exit;  // malformed trail byte
      Inc(i);
      if i > SourceBytes then Exit;         // incomplete multibyte char
      if ((c and $20) <> 0) and ((Byte(Source[i]) and $C0) <> $80) then Exit; // malformed trail byte
      Inc(i);
      end;
      Inc(count);
    end;
    end;
    Result := count+1;
  end;

begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: String): String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: String): String;
begin
  Result := Utf8Decode(S);
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
begin
  Value := StrToIntDef(S, MaxInt);
  Result := (Value <> MaxInt);
end;
{$ENDIF}

end.
