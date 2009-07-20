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

unit xcalHijri;

{$I xcalDefs.inc}

interface

uses
  xcalClass, SysUtils, Classes;

const
  { The XCalHijriMonthDays array can be used to quickly find the number of }
  { days in a month:  XCalHijriMonthDays[IsLeapYear(Y), M]                 }
  XCalHijriMonthDays: array [Boolean] of TDayTable =
    ((30, 29, 30, 29, 30, 29, 30, 29, 30, 29, 30, 29),
     (30, 29, 30, 29, 30, 29, 30, 29, 30, 29, 30, 30));

  { Number of days in a year: DaysPerYear[IsLeapYear(Y)] }
  XCalHijriDaysPerYear: array [Boolean] of Word = (354, 355);

type

  THijriLeapYearsKind = (lyk16, lyk15);
  TGetHijriDaysInAMonthEvent = procedure(const Year, Month: Word; const Leap: Boolean; var NDays: Word) of object;

  THijriCalendar = class(TXCalendar)
  private
    FLeapYearsKind: THijriLeapYearsKind;
    FHijriAdjustment: ShortInt;
    FOnGetDaysInAMonth: TGetHijriDaysInAMonthEvent;
    FGettingDaysInAMonth: Boolean;
    procedure SetHijriAdjustment(const Value: ShortInt);
  protected
    procedure SetOnGetDaysInAMonth(const Value: TGetHijriDaysInAMonthEvent); virtual;
    procedure SetLeapYearsKind(const Value: THijriLeapYearsKind); virtual;

    function GetDaysInAMonth(const AYear, AMonth: Word; const ALeap: Boolean): Word;
  public
    constructor Create(AOwner: TComponent); override;

    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; override;
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; override;
    function IsLeapYear(Year: Word): Boolean; override;

    function DaysInAYear(const AYear: Word): Word; override;
    function DaysInAMonth(const AYear, AMonth: Word): Word; override;
  published
    property WeekStartDay default xcalSaturday;

    { Leap year calculation kind:
        lyk16: Years 2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29 in each 30-year cycle are leap years
        lyk15: Years 2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29 in each 30-year cycle are leap years
    }
    property LeapYearsKind: THijriLeapYearsKind read FLeapYearsKind write SetLeapYearsKind default lyk16;

    { The value of HijriAdjustment (positive or negative) is added to the
      calculated dates to accommodate the variances in the start and the end of
      Ramadan and to accommodate the date difference between countries/regions. }
    property HijriAdjustment: ShortInt read FHijriAdjustment write SetHijriAdjustment default 0;

    { You can use this event to provide individual month lengths for the calendar
      according to the year. Please note that for each year, the length of the
      months should sum to either 354 (for common years) or 355 (for leap years).
      Otherwise the calendar might malfunction. }
    property OnGetDaysInAMonth: TGetHijriDaysInAMonthEvent read FOnGetDaysInAMonth write SetOnGetDaysInAMonth;
  end;

implementation

{$R xcalHijri.RES}

{ THijriCalendar }

const
  { Number of days in a common year }
  D1 = 354;

  { Number of days in a 30-year cycle }
  D30 = D1 * 30 + 11;

  { Number of days between the epoch of Hijri Calendar and TDateTime basis (12/31/1899 A.D.) }
  TDateTimeDateDelta = 466580;

  { Number of days between TTimeStamp basis (1/1/0001 A.D.) and the epoch of Hijri Calendar }
  TTimeStampDateDelta = 227014;

  { Approximate values used to calculate year and month spans }
  cnstApproxDaysPerYear  = 354.36666667; { Average over a 30-year cycle }
  cnstApproxDaysPerMonth = cnstApproxDaysPerYear / 12;


constructor THijriCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WeekStartDay := xcalSaturday;

  ApproxDaysPerYear  := cnstApproxDaysPerYear;
  ApproxDaysPerMonth := cnstApproxDaysPerMonth;

  try
    LoadSettingsFromResourceName(HInstance, 'HIJRICALENDARSETTINGS');
  except
  end;
end;

function THijriCalendar.TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Word;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @XCalHijriMonthDays[IsLeapYear(Year)];
  if (Year >= MinYear) and (Year <= MaxYear) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, DayTable^[I]);
    DivMod(Year - 1, 30, Year, I);
    while (I > 0) do
    begin
      Inc(Day, D1);
      if IsLeapYear(I) then
        Inc(Day);
      Dec(I);
    end; { maximum value for Day would be 354 + D30 = 10985 which fits in a Word }
    Date := Year * D30 + Day - TDateTimeDateDelta - FHijriAdjustment;
    Result := True;
  end;
end;

function THijriCalendar.DecodeDateFully(const DateTime: TDateTime;
  var Year, Month, Day, DOW: Word): Boolean;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(DateTime).Date;
  DOW := T mod 7 + 1;
  T := T - TTimeStampDateDelta + FHijriAdjustment;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
    Result := False;
  end else
  begin
    Dec(T);
    Y := 1;
    DivMod(T, D30, I, D);
    Inc(Y, I * 30);
    for I := 1 to 30 do
    begin
      Result := IsLeapYear(I);
      if (Result and (D < D1 + 1)) or (D < D1) then Break;
      Dec(D, D1);
      if Result then
        Dec(D);
      Inc(Y);
    end;
    Result := IsLeapYear(Y);
    DayTable := @XCalHijriMonthDays[Result];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

function THijriCalendar.IsLeapYear(Year: Word): Boolean;
begin
  case FLeapYearsKind of
  lyk15:
    Result := (Year mod 30) in [2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29];
  else
    Result := (Year mod 30) in [2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29];
  end;
end;

function THijriCalendar.DaysInAYear(const AYear: Word): Word;
begin
  Result := XCalHijriDaysPerYear[IsLeapYear(AYear)];
end;

function THijriCalendar.GetDaysInAMonth(const AYear, AMonth: Word; const ALeap: Boolean): Word;
begin
  Result := XCalHijriMonthDays[ALeap][AMonth];

  if Assigned(FOnGetDaysInAMonth) and (not FGettingDaysInAMonth) then
  begin
    FGettingDaysInAMonth := True;
    try
      FOnGetDaysInAMonth(AYear, AMonth, ALeap, Result);
    finally
      FGettingDaysInAMonth := False;
    end;
  end;
end;

function THijriCalendar.DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := GetDaysInAMonth(AYear, AMonth, IsLeapYear(AYear));
end;

procedure THijriCalendar.SetLeapYearsKind(const Value: THijriLeapYearsKind);
begin
  FLeapYearsKind := Value;
  if not (csLoading in ComponentState) then
    Changed;
end;

procedure THijriCalendar.SetHijriAdjustment(const Value: ShortInt);
begin
  FHijriAdjustment := Value;
  if not (csLoading in ComponentState) then
    Changed;
end;

procedure THijriCalendar.SetOnGetDaysInAMonth(const Value: TGetHijriDaysInAMonthEvent);
begin
  FOnGetDaysInAMonth := Value;
  if not (csLoading in ComponentState) then
    Changed;
end;

end.

