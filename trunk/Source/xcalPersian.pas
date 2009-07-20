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

unit xcalPersian;

{$I xcalDefs.inc}

interface

uses
  xcalClass, SysUtils, Classes;

const
  { The XCalPersianMonthDays array can be used to quickly find the number of }
  { days in a month:  XCalPersianMonthDays[IsLeapYear(Y), M]                 }
  XCalPersianMonthDays: array [Boolean] of TDayTable =
    ((31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29),
     (31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 30));

  { Number of days in a year: XCalPersianDaysPerYear[IsLeapYear(Y)] }
  XCalPersianDaysPerYear: array [Boolean] of Word = (365, 366);

type
  TPersianCalendar = class(TXCalendar)
  public
    constructor Create(AOwner: TComponent); override;

    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; override;
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; override;
    function IsLeapYear(Year: Word): Boolean; override;

    function DaysInAYear(const AYear: Word): Word; override;
    function DaysInAMonth(const AYear, AMonth: Word): Word; override;
  published
    property WeekStartDay default xcalSaturday;
  end;

implementation

{$R xcalPersian.RES}

{ TPersianCalendar }

const
  { Number of days in a common year }
  D1 = 365;

  { Number of days in a 33 year cycle }
  D33 = D1 * 33 + 8;

  { Number of days between the epoch of Persian calendar and TDateTime basis (12/31/1899 A.D.) }
  TDateTimeDateDelta = 466700;

  { Number of days between TTimeStamp basis (1/1/0001 A.D.) and the epoch of Persian calendar }
  TTimeStampDateDelta = 226894;

  { Approximate values used to calculate year and month spans }
  cnstApproxDaysPerYear  = 365.24242424; { Average over a 33 year cycle }
  cnstApproxDaysPerMonth = cnstApproxDaysPerYear / 12;


constructor TPersianCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WeekStartDay := xcalSaturday;

  ApproxDaysPerYear  := cnstApproxDaysPerYear;
  ApproxDaysPerMonth := cnstApproxDaysPerMonth;

  try
    LoadSettingsFromResourceName(HInstance, 'PERSIANCALENDARSETTINGS');
  except
  end;
end;

function TPersianCalendar.TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Word;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @XCalPersianMonthDays[IsLeapYear(Year)];
  if (Year >= MinYear) and (Year <= MaxYear) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, DayTable^[I]);
    DivMod(Year - 1, 33, Year, I);
    while (I > 0) do
    begin
      Inc(Day, D1);
      if IsLeapYear(I) then
        Inc(Day);
      Dec(I);
    end; { maximum value for Day would be 365 + D33 = 12418 which fits in a Word }
    Date := Year * D33 + Day - TDateTimeDateDelta;
    Result := True;
  end;
end;

function TPersianCalendar.DecodeDateFully(const DateTime: TDateTime;
  var Year, Month, Day, DOW: Word): Boolean;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(DateTime).Date;
  DOW := T mod 7 + 1;
  T := T - TTimeStampDateDelta;
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
    DivMod(T, D33, I, D);
    Inc(Y, I * 33);
    for I := 1 to 33 do
    begin
      Result := IsLeapYear(I);
      if (Result and (D < D1 + 1)) or (D < D1) then Break;
      Dec(D, D1);
      if Result then
        Dec(D);
      Inc(Y);
    end;
    Result := IsLeapYear(Y);
    DayTable := @XCalPersianMonthDays[Result];
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

function TPersianCalendar.IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 33) in [1, 5, 9, 13, 17, 22, 26, 30];
end;

function TPersianCalendar.DaysInAYear(const AYear: Word): Word;
begin
  Result := XCalPersianDaysPerYear[IsLeapYear(AYear)];
end;

function TPersianCalendar.DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := XCalPersianMonthDays[IsLeapYear(AYear)][AMonth];
end;

end.
