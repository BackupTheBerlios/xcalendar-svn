{
    Port of the AA+ project v1.30 to Delphi
    by adgteq

    http://xcalendar.sourceforge.net
    adgteq@yahoo.co.uk

    AA+ is a collection of freeware C++ classes which provide an implementation
    of the algorithms as presented in the book "Astronomical Algorithms" (2nd
    Edition) by Jean Meeus.

    Original Author:
    PJ Naughter
    http://www.naughter.com
}

unit AADate;
// This unit provides the algorithms which convert between the Gregorian and Julian calendars and the Julian Day. This refers to Chapter 7 and parts of Chapter 9 in the book.

interface

type

  TAACalendarDate = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
  end;

  TDAY_OF_WEEK =
  (
    aaSUNDAY,
    aaMONDAY,
    aaTUESDAY,
    aaWEDNESDAY,
    aaTHURSDAY,
    aaFRIDAY,
    aaSATURDAY
  );

  TAADate = class(TObject)
  protected
    m_dblJulian: Extended;  //Julian Day number for this date
    m_bGregorianCalendar: Boolean; //Is this date in the Gregorian calendar

  public
    constructor Create; overload;
    constructor Create(Year, Month: Integer; Day: Extended; bGregorianCalendar: Boolean); overload;
    constructor Create(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean); overload;
    constructor Create(JD: Extended; bGregorianCalendar: Boolean); overload;

    class function DateToJD(Year, Month: Integer; Day: Extended; bGregorianCalendar: Boolean): Extended;
    class function IsLeap(Year: Integer; bGregorianCalendar: Boolean): Boolean;
    class procedure DayOfYearToDayAndMonth(DayOfYear: Integer; bLeap: Boolean; var DayOfMonth, Month: Integer);
    class function JulianToGregorian(Year, Month, Day: Integer): TAACalendarDate;
    class function GregorianToJulian(Year, Month, Day: Integer): TAACalendarDate;
    class function INT(value: Extended): Integer;
    class function AfterPapalReform(Year, Month: Integer; Day: Extended): Boolean; overload;
    class function AfterPapalReform(JD: Extended): Boolean; overload;
    class function DayOfYear(JD: Extended; Year: Integer; bGregorianCalendar: Boolean): Extended; overload;
    class function DaysInMonth(Month: Integer; bLeap: Boolean): Integer; overload;

    function Julian: Extended;
    function Day: Integer;
    function Month: Integer;
    function Year: Integer;
    function Hour: Integer;
    function Minute: Integer;
    function Second: Extended;
    procedure _Set(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean); overload;
    procedure _Set(JD: Extended; bGregorianCalendar: Boolean); overload;
    procedure SetInGregorianCalendar(bGregorianCalendar: Boolean);
    procedure Get(var Year, Month, Day, Hour, Minute: Integer; var Second: Extended);
    function DayOfWeek: TDAY_OF_WEEK;
    function DayOfYear: Extended; overload;
    function DaysInMonth: Integer; overload;
    function DaysInYear: Integer;
    function Leap: Boolean;
    function InGregorianCalendar: Boolean;
    function FractionalYear: Extended;

  end;


implementation

{
constructor Create; overload;
constructor Create(Year, Month: Integer; Day: Extended; bGregorianCalendar: Boolean); overload;
constructor Create(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean); overload;
constructor Create(JD: Extended; bGregorianCalendar: Boolean); overload;

Parameters
Year The year. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year (1 for January to 12 for December).
Day The day of the month (Can include decimals).
Hour The hour (Can include decimals).
Minute The minute (Can include decimals).
Second The seconds (Can include decimals).
JD The Julian day including decimals.
bGregorianCalendar True to imply a date in the Gregorian Calendar, False means use the Julian Calendar.

Remarks
Constructs a date given a variety of parameters.
}
constructor TAADate.Create;
begin
  inherited Create;
  m_dblJulian := 0;
  m_bGregorianCalendar := False;
end;

constructor TAADate.Create(Year, Month: Integer; Day: Extended; bGregorianCalendar: Boolean);
begin
  inherited Create;
  _Set(Year, Month, Day, 0, 0, 0, bGregorianCalendar);
end;

constructor TAADate.Create(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean);
begin
  inherited Create;
  _Set(Year, Month, Day, Hour, Minute, Second, bGregorianCalendar);
end;

constructor TAADate.Create(JD: Extended; bGregorianCalendar: Boolean);
begin
  inherited Create;
  _Set(JD, bGregorianCalendar);
end;

class function TAADate.AfterPapalReform(Year, Month: Integer; Day: Extended): Boolean;
begin
  Result := ((Year > 1582) or ((Year = 1582) and (Month > 10)) or ((Year = 1582) and (Month = 10) and (Day >= 15)));
end;

class function TAADate.AfterPapalReform(JD: Extended): Boolean;
begin
  Result := (JD >= 2299160.5);
end;

class function TAADate.DateToJD(Year, Month: Integer; Day: Extended; bGregorianCalendar: Boolean): Extended;
var
  Y, M, A, B: Integer;
begin
  Y := Year;
  M := Month;
  if (M < 3) then
  begin
    Y := Y - 1;
    M := M + 12;
  end;

  B := 0;
  if (bGregorianCalendar) then
  begin
    A := INT(Y / 100.0);
    B := 2 - A + INT(A / 4.0);
  end;

  Result := INT(365.25 * (Y + 4716)) + INT(30.6001 * (M + 1)) + Day + B - 1524.5;
end;

{
class function TAADate.IsLeap(Year: Integer; bGregorianCalendar: Boolean): Boolean;

Return Value
True if the specified year is leap otherwise False.

Parameters
Year The year. (Years are counted astronomically i.e. 1 BC = Year 0)
bGregorianCalendar True to imply a date in the Gregorian Calendar, False means use the Julian Calendar.
}
class function TAADate.IsLeap(Year: Integer; bGregorianCalendar: Boolean): Boolean;
begin
  if (bGregorianCalendar) then
  begin                                       
    if ((Year mod 100) = 0) then
      Result := ((Year mod 400) = 0)
    else
      Result := ((Year mod 4) = 0);
  end
  else
    Result := ((Year mod 4) = 0);
end;

{
procedure TAADate._Set(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean);
procedure TAADate._Set(JD: Extended; bGregorianCalendar: Boolean);

Parameters
Year The year. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year (1 for January to 12 for December).
Day The day of the month (Can include decimals).
Hour The hour (Can include decimals).
Minute The minute (Can include decimals).
Second The seconds (Can include decimals).
JD The Julian day including decimals
bGregorianCalendar True to imply a date in the Gregorian Calendar, False means use the Julian Calendar.

Remarks
Allows the date to be modified after construction.
}
procedure TAADate._Set(Year, Month: Integer; Day, Hour, Minute, Second: Extended; bGregorianCalendar: Boolean);
var
  dblDay: Extended;
begin
  dblDay := Day + (Hour/24) + (Minute/1440) + (Second / 86400);
  _Set(DateToJD(Year, Month, dblDay, bGregorianCalendar), bGregorianCalendar);
end;

procedure TAADate._Set(JD: Extended; bGregorianCalendar: Boolean);
begin
  m_dblJulian := JD;
  SetInGregorianCalendar(bGregorianCalendar);
end;

{
procedure TAADate.Get(var Year, Month, Day, Hour, Minute: Integer; var Second: Extended);

Parameters
Year Upon return will contain the year. (Years are counted astronomically i.e. 1 BC = Year 0)
Month Upon return will contain the month of the year (1 for January to 12 for December).
Day Upon return will contain the day of the month.
Hour Upon return will contain the hour.
Minute Upon return will contain the minute.
Second Upon return will contain the seconds (Can include decimals).

Remarks
Allows the date parts to be retrieved.
}
procedure TAADate.Get(var Year, Month, Day, Hour, Minute: Integer; var Second: Extended);
var
  JD, F, dblDay: Extended;
  alpha, Z, A, B, C, D, E: Integer;
begin
  JD := m_dblJulian + 0.5;
  F := Frac(JD);
  Z := Trunc(JD);

  if (m_bGregorianCalendar) then //There is a difference here between the Meeus implementation and this one
  //if (Z >= 2299161)            //The Meeus implementation automatically assumes the Gregorian Calendar
                                 //came into effect on 15 October 1582 (JD: 2299161), while the CAADate
                                 //implementation has a "m_bGregorianCalendar" value to decide if the date
                                 //was specified in the Gregorian or Julian Calendars. This difference
                                 //means in effect that CAADate fully supports a propalactive version of the
                                 //Julian calendar. This allows you to construct Julian dates after the Papal
                                 //reform in 1582. This is useful if you want to construct dates in countries
                                 //which did not immediately adapt the Gregorian calendar
  begin
    alpha := INT((Z - 1867216.25) / 36524.25);
    A := Z + 1 + alpha - INT(INT(alpha)/4.0);
  end
  else
    A := Z;

  B := A + 1524;
  C := INT((B - 122.1) / 365.25);
  D := INT(365.25 * C);
  E := INT((B - D) / 30.6001);

  dblDay := B - D - INT(30.6001 * E) + F;
  Day := Trunc(dblDay);

  if (E < 14) then
    Month := E - 1
  else
    Month := E - 13;

  if (Month > 2) then
    Year := C - 4716
  else
    Year := C - 4715;

  F := Frac(dblDay);
  Hour := INT(F*24);
  Minute := INT((F - (Hour)/24.0)*1440.0);
  Second := (F - (Hour / 24.0) - (Minute / 1440.0)) * 86400.0;
end;

{
procedure TAADate.SetInGregorianCalendar(bGregorianCalendar: Boolean);

Parameters
bGregorianCalendar True to imply a date in the Gregorian Calendar, False means use the Julian Calendar.

Remarks
Allows the date's calendar type to be changed after construction.
}
procedure TAADate.SetInGregorianCalendar(bGregorianCalendar: Boolean);
var
  bAfterPapalReform: Boolean;
begin
  bAfterPapalReform := (m_dblJulian >= 2299160.5);

  m_bGregorianCalendar := (bGregorianCalendar and bAfterPapalReform);
end;

{
function TAADate.Day: Integer;

Return Value
Returns the day of the month this date represents.
}
function TAADate.Day: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Day;
end;

{
function TAADate.Month: Integer;

Return Value
Returns the month (1 - 12) this date represents.
}
function TAADate.Month: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Month;
end;

{
function TAADate.Year: Integer;

Return Value
Returns the year this date represents.
}
function TAADate.Year: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Year;
end;

{
function TAADate.Hour: Integer;

Return Value
Returns the hour this date represents.
}
function TAADate.Hour: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Hour;
end;

{
function TAADate.Minute: Integer;

Return Value
Returns the minute this date represents.
}
function TAADate.Minute: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Minute;
end;

{
function TAADate.Second: Extended;

Return Value
Returns the seconds this date represents.
}
function TAADate.Second: Extended;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  Result := Second;
end;

{
function TAADate.DayOfWeek: TDAY_OF_WEEK;

Return Value
Returns an enum which identifies which day of the week this date represents.
}
function TAADate.DayOfWeek: TDAY_OF_WEEK;
begin
  Result := TDAY_OF_WEEK(Trunc(m_dblJulian + 1.5) mod 7);
end;

{
class function TAADate.DaysInMonth(Month: Integer; bLeap: Boolean): Integer;
function TAADate.DaysInMonth: Integer;

Return Value
Returns the total number of days in the month (28 - 31) which this date represents. The class version of the function can be used if you do not want to construct a TAADate instance to do this test.
}
var
  MonthLength: array[0..12] of Integer =
  (
    31, 28, 31, 30, 31, 30,
    31, 31, 30, 31, 30, 31, 0
  );

class function TAADate.DaysInMonth(Month: Integer; bLeap: Boolean): Integer;
begin
  //Validate our parameters
  Assert((Month >= 1) and (Month <= 12));


  if (bLeap) then
    Inc(MonthLength[1]);

  Result := MonthLength[Month-1];
end;

function TAADate.DaysInMonth: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);
  
  Result := DaysInMonth(Month, IsLeap(Year, m_bGregorianCalendar));
end;

{
function TAADate.DaysInYear: Integer;

Return Value
Returns the total number of days in the year (365 or 366) which this date represents.
}
function TAADate.DaysInYear: Integer;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);

  if (IsLeap(Year, m_bGregorianCalendar)) then
    Result := 366
  else
    Result := 365;
end;

{
function TAADate.DayOfYear: Extended;

Return Value
Returns the day of year (including decimals) this date represents.
}
function TAADate.DayOfYear: Extended;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
begin
  Get(Year, Month, Day, Hour, Minute, Second);

  Result := DayOfYear(m_dblJulian, Year, AfterPapalReform(Year, 1, 1));
end;

class function TAADate.DayOfYear(JD: Extended; Year: Integer; bGregorianCalendar: Boolean): Extended;
begin
  Result := JD - DateToJD(Year, 1, 1, bGregorianCalendar) + 1;
end;

{
function TAADate.FractionalYear: Extended;

Return Value
Returns the years with decimals this date represents e.g. the middle of the year 2000 would be returned as 2000.5.
}
function TAADate.FractionalYear: Extended;
var
  Year, Month, Day, Hour, Minute: Integer;
  Second: Extended;
  DaysInYear: Integer;
begin
  Get(Year, Month, Day, Hour, Minute, Second);

  if (IsLeap(Year, m_bGregorianCalendar)) then
    DaysInYear := 366
  else
    DaysInYear := 365;

  Result := Year + ((m_dblJulian - DateToJD(Year, 1, 1, AfterPapalReform(Year, 1, 1))) / DaysInYear);
end;

{
function TAADate.Leap: Boolean;

Return Value
True if the year which this date represents is leap otherwise False.
}
function TAADate.Leap: Boolean;
begin
  Result := IsLeap(Year(), m_bGregorianCalendar);
end;

{
class procedure TAADate.DayOfYearToDayAndMonth(DayOfYear: Integer; bLeap: Boolean; var DayOfMonth, Month: Integer);

Parameters
DayOfYear The day of the year where 1st of January is 1 going up to 365 or 366 for 31st of December
bLeap True if the year being considered is a leap year, otherwise False.
DayOfMonth Upon return will contain the day of the month.
Month Upon return will contain the month.
}
class procedure TAADate.DayOfYearToDayAndMonth(DayOfYear: Integer; bLeap: Boolean; var DayOfMonth, Month: Integer);
var
  K: Integer;
begin
  if bLeap then
    K := 1
  else
    K := 2;

  Month := INT(9*(K + DayOfYear)/275.0 + 0.98);
  if (DayOfYear < 32) then
    Month := 1;

  DayOfMonth := DayOfYear - INT((275*Month)/9.0) + (K*INT((Month + 9)/12.0)) + 30;
end;

class function TAADate.INT(value: Extended): Integer;
begin
  if (value >= 0) then
    Result := Trunc(value)
  else
    Result := Trunc(value - 1);
end;

{
class function TAADate.JulianToGregorian(Year, Month, Day: Integer): TAACalendarDate;

Return Value
A record containing
Year The year in the Gregorian Calendar. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year in the Gregorian Calendar (1 for January to 12 for December).
Day The day of the month in the Gregorian Calendar.

Parameters
Year The year in the Julian Calendar to convert. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year in the Julian Calendar (1 for January to 12 for December).
Day The day of the month in the Julian Calendar.

Remarks
Converts a calendrical date expressed in the Julian Calendar to the equivalent date in the Gregorian Calendar. It is assumed that the adoption of the Gregorian Calendar occurred in 1582.
}
class function TAADate.JulianToGregorian(Year, Month, Day: Integer): TAACalendarDate;
var
  date: TAADate;
  GregorianDate: TAACalendarDate;
  Hour, Minute: Integer;
  Second: Extended;
begin
  date := TAADate.Create(Year, Month, Day, False);
  try
    date.SetInGregorianCalendar(True);

    Hour := 0;
    Minute := 0;
    Second := 0;
    date.Get(GregorianDate.Year, GregorianDate.Month, GregorianDate.Day, Hour, Minute, Second);
  finally
    date.Free;
  end;

  Result := GregorianDate;
end;

{
class function TAADate.GregorianToJulian(Year, Month, Day: Integer): TAACalendarDate;

Return Value
A record containing
Year The year in the Julian Calendar. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year in the Julian Calendar (1 for January to 12 for December).
Day The day of the month in the Julian Calendar.

Parameters
Year The year in the Gregorian Calendar to convert. (Years are counted astronomically i.e. 1 BC = Year 0)
Month The month of the year in the Gregorian Calendar (1 for January to 12 for December).
Day The day of the month in the Gregorian Calendar.

Remarks
Converts a calendrical date expressed in the Gregorian Calendar to the equivalent date in the Julian Calendar.
}
class function TAADate.GregorianToJulian(Year, Month, Day: Integer): TAACalendarDate;
var
  date: TAADate;
  JulianDate: TAACalendarDate;
  Hour, Minute: Integer;
  Second: Extended;
begin
  date := TAADate.Create(Year, Month, Day, True);
  try
    date.SetInGregorianCalendar(False);

    Hour := 0;
    Minute := 0;
    Second := 0;
    date.Get(JulianDate.Year, JulianDate.Month, JulianDate.Day, Hour, Minute, Second);
  finally
    date.Free;
  end;

  Result := JulianDate;
end;

{
function TAADate.Julian: Extended;

Return Value
Returns the underlying Julian Day including decimals.
}
function TAADate.Julian: Extended;
begin
  Result := m_dblJulian;
end;

{
function TAADate.InGregorianCalendar: Boolean;

Return Value
Returns True if this date is in the Gregorian calendar, False means the Julian Calendar.
}
function TAADate.InGregorianCalendar: Boolean;
begin
  Result := m_bGregorianCalendar;
end;

end.
