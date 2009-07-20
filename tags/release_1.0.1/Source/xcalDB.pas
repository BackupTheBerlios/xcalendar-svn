{

    XCalendar - General Calendar Utilities for Delphi
    by adgteq

    Project website: http://xcalendar.sourceforge.net
    Author's e-mail: adgteq@yahoo.co.uk

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

}

unit xcalDB;

{$I xcalDefs.inc}

interface

uses
  DB, SysUtils, Classes,
{$IFDEF Delphi6}
  SqlTimSt, Variants,
{$ENDIF}
  xcalClass;

type

{ TXcalDateTimeField }

  TXcalDateTimeField = class(TDateTimeField)
  private
    FXCalendar: TXCalendar;
    procedure XCalendarSettingsChanged(Sender: TObject);
    procedure SetXCalendar(const Value: TXCalendar);
  protected
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  public
    destructor Destroy; override;
  published
    property XCalendar: TXCalendar read FXCalendar write SetXCalendar;
  end;

{ TXcalDateField }

  TXcalDateField = class(TXcalDateTimeField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TXCalSQLTimeStampField }

{$IFDEF Delphi6}
  TXCalSQLTimeStampField = class(TSQLTimeStampField)
  private
    FXCalendar: TXCalendar;
    procedure XCalendarSettingsChanged(Sender: TObject);
    procedure SetXCalendar(const Value: TXCalendar);

    function InternalSQLTimeStampToStr(const Format: string; DateTime: TSQLTimeStamp): string;
    function InternalTryStrToSQLTimeStamp(const S: string; var TimeStamp: TSQLTimeStamp) : Boolean;
    function InternalStrToSQLTimeStamp(const S: string): TSQLTimeStamp;
  protected
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    destructor Destroy; override;
  published
    property XCalendar: TXCalendar read FXCalendar write SetXCalendar;
  end;
{$ENDIF}


procedure RegisterXCalFields;


implementation

uses
  DBConsts;

{ TxcalDateTimeField }

procedure TXcalDateTimeField.XCalendarSettingsChanged(Sender: TObject);
begin
  PropertyChanged(False);
end;

procedure TXcalDateTimeField.SetXCalendar(const Value: TXCalendar);
begin
  if Assigned(FXCalendar) then
    FXCalendar.RemoveNotify(XCalendarSettingsChanged);

  FXCalendar := Value;
  FXCalendar.AddNotify(XCalendarSettingsChanged);
  PropertyChanged(False);
end;

procedure TXcalDateTimeField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TDateTime;
begin
  if not Assigned(FXCalendar) then
  begin
    inherited;
    Exit;
  end;

  if GetData(@D, False) then
  begin
    if DisplayText and (DisplayFormat <> '') then
      F := DisplayFormat
    else
      case DataType of
        ftDate: F := FXCalendar.FormatSettings.ShortDateFormat;
        ftTime: F := FXCalendar.FormatSettings.LongTimeFormat;
      end;
    FXCalendar.DateTimeToString(Text, F, D);
  end else
    Text := '';
end;

procedure TXcalDateTimeField.SetAsString(const Value: string);
var
  DateTime: TDateTime;
begin
  if not Assigned(FXCalendar) then
  begin
    inherited;
    Exit;
  end;
  
  if Value = '' then Clear else
  begin
    case DataType of
      ftDate: DateTime := FXCalendar.StrToDate(Value);
      ftTime: DateTime := FXCalendar.StrToTime(Value);
    else
      DateTime := FXCalendar.StrToDateTime(Value);
    end;
    SetAsDateTime(DateTime);
  end;
end;


destructor TXcalDateTimeField.Destroy;
begin
  if Assigned(FXCalendar) then
    FXCalendar.RemoveNotify(XCalendarSettingsChanged);

  inherited;
end;

{ TXcalDateField }

constructor TXcalDateField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDate);
end;

function TXcalDateField.GetDataSize: Integer;
begin
  Result := SizeOf(Integer);
end;


{ TXCalSQLTimeStampField }

{$IFDEF Delphi6}
function TXCalSQLTimeStampField.InternalSQLTimeStampToStr(
  const Format: string; DateTime: TSQLTimeStamp): string;
var
  FTimeStamp: TDateTime;
begin
  if not Assigned(FXCalendar) then
  begin
    Result := SQLTimeStampToStr(Format, DateTime);
    Exit;
  end;

  FTimeStamp := SqlTimeStampToDateTime(DateTime);
  FXCalendar.DateTimeToString(Result, Format, FTimeStamp);
end;

function TXCalSQLTimeStampField.InternalTryStrToSQLTimeStamp(
  const S: string; var TimeStamp: TSQLTimeStamp): Boolean;
var
  DT: TDateTime;

      function DaysInAMonth(const AYear, AMonth: Word): Word;
      begin
        Result := SysUtils.MonthDays[(AMonth = 2) and SysUtils.IsLeapYear(AYear), AMonth];
      end;

      function IsSqlTimeStampValid(const ts: TSQLTimeStamp): Boolean;
      begin
        if (ts.Month > 12) or (ts.Day > DaysInAMonth(ts.Year, ts.Month)) or
             (ts.Hour > 23) or (ts.Minute > 59) or (ts.Second > 59) then
          Result := False
        else
          Result := True;
      end;

begin
  if not Assigned(FXCalendar) then
  begin
    Result := TryStrToSQLTimeStamp(S, TimeStamp);
    Exit;
  end;

  Result := FXCalendar.TryStrToDateTime(S, DT);
  if Result then
  begin
    TimeStamp := DateTimeToSQLTimeStamp(DT);
    Result := IsSqlTimeStampValid(TimeStamp);
  end;
  if not Result then
    TimeStamp := NullSQLTimeStamp;
end;

function TXCalSQLTimeStampField.InternalStrToSQLTimeStamp(
  const S: string): TSQLTimeStamp;
begin
  if not InternalTryStrToSqlTimeStamp(S, Result) then
    raise EConvertError.Create(SCouldNotParseTimeStamp);
end;

procedure TXCalSQLTimeStampField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TSQLTimeStamp;
begin
  if not Assigned(FXCalendar) then
  begin
    inherited;
    Exit;
  end;

  if GetData(@D, False) then
  begin
    if DisplayText and (DisplayFormat <> '') then
      F := DisplayFormat
    else
      F := '';
    Text := InternalSQLTimeStampToStr(F, D);
  end else
    Text := '';
end;

procedure TXCalSQLTimeStampField.SetAsString(const Value: string);
begin
  if not Assigned(FXCalendar) then
  begin
    inherited;
    Exit;
  end;

  if Value = '' then Clear else
  SetAsSQLTimeStamp(InternalStrToSQLTimeStamp(Value));
end;

procedure TXCalSQLTimeStampField.SetXCalendar(const Value: TXCalendar);
begin
  if Assigned(FXCalendar) then
    FXCalendar.RemoveNotify(XCalendarSettingsChanged);

  FXCalendar := Value;
  FXCalendar.AddNotify(XCalendarSettingsChanged);
  PropertyChanged(False);
end;

procedure TXCalSQLTimeStampField.XCalendarSettingsChanged(Sender: TObject);
begin
  PropertyChanged(False);
end;

destructor TXCalSQLTimeStampField.Destroy;
begin
  if Assigned(FXCalendar) then
    FXCalendar.RemoveNotify(XCalendarSettingsChanged);

  inherited;
end;

procedure TXCalSQLTimeStampField.SetVarValue(const Value: Variant);
begin
  if (not VarIsClear(Value)) and ((TVarData(Value).VType = varString) or
                                  (TVarData(Value).VType = varOleStr)) then
    SetAsString(String(Value))
  else
    inherited;
end;
{$ENDIF}

procedure RegisterXCalFields;
begin
  RegisterFields([ TXCalDateTimeField,
                   TXcalDateField
{$IFDEF Delphi6}
                  ,TXCalSQLTimeStampField
{$ENDIF}
                 ]);
end;

end.
