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

unit xcalGregorian;

{$I xcalDefs.inc}

interface

uses
  xcalClass, SysUtils, Classes;

type

  { TGregorianCalendar is just a wrapper for the already-available functions }
  TGregorianCalendar = class(TXCalendar)
  public
    constructor Create(AOwner: TComponent); override;

    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; override;
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; override;
{$IFDEF Delphi6}
    function CurrentYear: Word; override; { Overriden to be faster }
{$ENDIF}
    function IsLeapYear(Year: Word): Boolean; override;

    function DaysInAYear(const AYear: Word): Word; override;
    function DaysInAMonth(const AYear, AMonth: Word): Word; override;

  end;

implementation

{$R xcalGregorian.RES}

{ TGregorianCalendar }

const
  { This is actual days per year but you need to know if it's a leap year}
  DaysPerYear: array [Boolean] of Word = (365, 366);

  { Approximate values used to calculate year and month spans }
  cnstApproxDaysPerYear  = 365.25; { average over a 4 year span }
  cnstApproxDaysPerMonth = 30.4375;

constructor TGregorianCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ApproxDaysPerYear := cnstApproxDaysPerYear;
  ApproxDaysPerMonth := cnstApproxDaysPerMonth;

  try
    LoadSettingsFromResourceName(HInstance, 'GREGORIANCALENDARSETTINGS');
  except
  end;
end;

function TGregorianCalendar.TryEncodeDate(Year, Month, Day: Word;
  out Date: TDateTime): Boolean;
begin
{$IFDEF Delphi6}
  Result := SysUtils.TryEncodeDate(Year, Month, Day, Date);
{$ELSE}
  Result := False;
  try
    Date := SysUtils.EncodeDate(Year, Month, Day);
    Result := True;
  except
    on EConvertError do;
  end;
{$ENDIF}
end;

function TGregorianCalendar.DecodeDateFully(const DateTime: TDateTime;
  var Year, Month, Day, DOW: Word): Boolean;
begin
{$IFDEF Delphi6}
  Result := SysUtils.DecodeDateFully(DateTime, Year, Month, Day, DOW);
{$ELSE}
  SysUtils.DecodeDate(DateTime, Year, Month, Day);
  DOW := SysUtils.DayOfWeek(DateTime);
  Result := SysUtils.IsLeapYear(Year);
{$ENDIF}
end;

{$IFDEF Delphi6}
function TGregorianCalendar.CurrentYear: Word;
begin
  Result := SysUtils.CurrentYear;
end;
{$ENDIF}

function TGregorianCalendar.IsLeapYear(Year: Word): Boolean;
begin
  Result := SysUtils.IsLeapYear(Year);
end;

function TGregorianCalendar.DaysInAYear(const AYear: Word): Word;
begin
  Result := DaysPerYear[SysUtils.IsLeapYear(AYear)];
end;

function TGregorianCalendar.DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := SysUtils.MonthDays[(AMonth = 2) and SysUtils.IsLeapYear(AYear), AMonth];
end;

end.
