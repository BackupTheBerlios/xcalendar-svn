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

unit xcalAstroPersian;

{$I xcalDefs.inc}

interface

uses
  xcalClass, xcalPersian, SysUtils, Classes, Math;

type

  PYearCacheItem = ^TYearCacheItem;
  TYearCacheItem = record
    JDStart: Extended;
    JDEnd: Extended;
    Year: Integer;
    Leap: Boolean;
  end;

  TYearCache = class(TList)
  private
    FSize: Integer;
    procedure SetSize(const Value: Integer);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(const AJDStart: Extended; const AJDEnd: Extended;
      const AYear: Integer; const ALeap: Boolean): Integer;
    function FindValue(const AJD: Extended; out Year: Integer;
      out JDYearStart: Extended; out Leap: Boolean): Integer;
    property Size: Integer read FSize write SetSize;
  end;


  TAstroPersianCalendar = class(TPersianCalendar)
  private
    FLongitude: Extended;
    FCache: TYearCache;
    procedure FindPersianYear(const JD: Extended; out Year: Integer;
      out JDYearStart: Extended; out Leap: Boolean);
    function GetCacheSize: Integer;
    procedure SetCacheSize(const Value: Integer);
    procedure FlushCache;
    procedure SetLongitude(const Value: Extended);
    function IsLongitudeStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; override;
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; override;
    function IsLeapYear(Year: Word): Boolean; override;

  published
    property Longitude: Extended read FLongitude write SetLongitude stored IsLongitudeStored;
    property CacheSize: Integer read GetCacheSize write SetCacheSize default 3;
  end;


  
implementation

uses
  AAEquinox, AADynamicalTime, AAEquationOfTime, AADate;

const
  JDPersianEpoch = 1948320.5; { Julian day number of the epoch of the Persian calendar }
  VernalEquinoxYear = 365.242362; { Mean vernal-equinox year length calculated for the period 1500 to 2500 A.D. }

  { Approximate values used to calculate year and month spans }
  cnstApproxDaysPerYear = VernalEquinoxYear;
  cnstApproxDaysPerMonth = cnstApproxDaysPerYear / 12;

  { Just in case... }
  MAX_TRIALS = 1000;




{ Returns the julian day number of the time of the vernal equinox occuring in
  the specified gregorian year }
function VernalEquinox_JD(const GregorianYear: Integer; const Longitude: Extended): Extended;
begin
  { Equinox in dynamical time }
  Result := SpringEquinox(GregorianYear);

  { Correct for delta T to obtain universal time }
  Result := Result - (DynamicalTime_DeltaT(Result) / (24 * 60 * 60));
end;

{ Returns the julian day number of the time of true noon in the day specified by JD }
function TrueNoon_JD(const JD, Longitude: Extended): Extended;
var
  JDMidnight, JDE, NoonTime: Extended;
begin
  JDE := JD + (DynamicalTime_DeltaT(JD) / (24 * 60 * 60));
  NoonTime := 12 - (Longitude / 15) - (EquationOfTime(JDE) / 60);
  JDMidnight := Floor(JD - 0.5) + 0.5;
  Result := JDMidnight + (NoonTime / 24);
end;


{ TAstroPersianCalendar }

{ Sets Year to the persian year containing the specified julian day number, JD,
    according to the rule that if the vernal equinox occurs before true noon
    at the specified longitude, then the day becomes the first day of the new
    year, but if it happens after the true noon, then the next day becomes the
    first day of the new year.
  Sets JDYearStart to the julian day number of the start of the year (12:00AM in
    the first day of the month Farvardin).
  Sets Leap to True for leap years (years having 366 days); False otherwise. }
procedure TAstroPersianCalendar.FindPersianYear(const JD: Extended;
  out Year: Integer; out JDYearStart: Extended; out Leap: Boolean);
var
  AADate: TAADate;
  gyGuess: Integer;
  JDLastYearStart, JDNextYearStart: Extended;

        { Returns the julian day number of the persian year start occuring in the
            specified gregorian year (with respect to the longitude). }
        function PersianYearStart_JD(const GregorianYear: Integer): Extended;
        var
          JDEquinox, JDNoon: Extended;
        begin
          JDEquinox := VernalEquinox_JD(GregorianYear, FLongitude);
          JDNoon := TrueNoon_JD(JDEquinox, FLongitude);
          Result := Floor(JDEquinox - 0.5) + 0.5;
          if JDEquinox >= JDNoon then
            Result := Result + 1;
        end;

begin
  if FCache.FindValue(JD, Year, JDYearStart, Leap) >= 0 then
    Exit;

  AADate := TAADate.Create(JD, TAADate.AfterPapalReform(JD));
  try
    gyGuess := AADate.Year() - 1;
  finally
    AADate.Free;
  end;

  JDLastYearStart := PersianYearStart_JD(gyGuess);
  while JD < JDLastYearStart do
  begin
    Dec(gyGuess);
    JDLastYearStart := PersianYearStart_JD(gyGuess);
  end;

  JDNextYearStart := JDLastYearStart;
  while not ((JDLastYearStart <= JD) and (JD < JDNextYearStart)) do
  begin
    JDLastYearStart := JDNextYearStart;
    Inc(gyGuess);
    JDNextYearStart := PersianYearStart_JD(gyGuess);
  end;

  Year := Round((JDLastYearStart - JDPersianEpoch) / VernalEquinoxYear) + 1;
  JDYearStart := JDLastYearStart;
  Leap := (Round(JDNextYearStart - JDLastYearStart) > 365);

  FCache.Add(JDLastYearStart, JDNextYearStart, Year, Leap);
end;

constructor TAstroPersianCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCache := TYearCache.Create;
  FCache.Size := 3;

  FLongitude := 52.5;

  ApproxDaysPerYear  := cnstApproxDaysPerYear;
  ApproxDaysPerMonth := cnstApproxDaysPerMonth;
end;

destructor TAstroPersianCalendar.Destroy;
begin
  FCache.Free;
  
  inherited Destroy;
end;

function TAstroPersianCalendar.TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  JDGuess, JDYearStart: Extended;
  yGuess: Integer;
  nTrials: Integer;
  I: Integer;
  DayTable: PDayTable;
  Leap: Boolean;
begin
  Result := False;
  if (Year >= MinYear) and (Year <= MaxYear) and (Month >= 1) and (Month <= 12) then
  begin
    JDGuess := JDPersianEpoch + (VernalEquinoxYear * (Year - 1));
    nTrials := 0;
    while True do
    begin
      FindPersianYear(JDGuess, yGuess, JDYearStart, Leap);
      if yGuess = Year then
        Break
      else if yGuess < Year then
        JDGuess := JDYearStart + 367
      else
        JDGuess := JDYearStart - 1;

      Inc(nTrials);
      if nTrials > MAX_TRIALS then { Just in case... }
      begin
        Result := False;
        Break;
      end;
    end;

    DayTable := @XCalPersianMonthDays[Leap];
    if (Day >= 1) and (Day <= DayTable^[Month]) then
    begin
      for I := 1 to Month - 1 do
        Inc(Day, DayTable^[I]);

      Date := Trunc(JulianDayToDateTime(JDYearStart + Day - 1));
      Result := True;
    end;
  end;
end;

function TAstroPersianCalendar.DecodeDateFully(const DateTime: TDateTime;
  var Year, Month, Day, DOW: Word): Boolean;
var
  JD, JDYearStart: Extended;
  Y, M, D, I: Integer;
  DayTable: PDayTable;
begin
  JD := DateTimeToJulianDay(DateTime);
  JD := Floor(JD - 0.5) + 0.5;
  if JD < JDPersianEpoch then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
    Result := False;
  end else
  begin
    DOW := Trunc(JD + 1.5) mod 7 + 1;
    FindPersianYear(JD, Y, JDYearStart, Result);
    DayTable := @XCalPersianMonthDays[Result];
    D := Trunc(JD - JDYearStart);
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

function TAstroPersianCalendar.IsLeapYear(Year: Word): Boolean;
var
  JDGuess, JDYearStart: Extended;
  yGuess: Integer;
  nTrials: Integer;
begin
  Result := False;
  JDGuess := JDPersianEpoch + (VernalEquinoxYear * (Year - 1));
  nTrials := 0;
  while True do
  begin
    FindPersianYear(JDGuess, yGuess, JDYearStart, Result);
    if yGuess = Year then
      Break
    else if yGuess < Year then
      JDGuess := JDYearStart + VernalEquinoxYear
    else
      JDGuess := JDYearStart - 1;
      
    Inc(nTrials);
    if nTrials > MAX_TRIALS then { Just in case... }
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TAstroPersianCalendar.GetCacheSize: Integer;
begin
  Result := FCache.Size;
end;

procedure TAstroPersianCalendar.SetCacheSize(const Value: Integer);
begin
  FCache.Size := Value;
end;

procedure TAstroPersianCalendar.FlushCache;
begin
  FCache.Clear;
end;

procedure TAstroPersianCalendar.SetLongitude(const Value: Extended);
begin
  if Value <> FLongitude then
  begin
    FLongitude := Value;
    if not (csLoading in ComponentState) then
    begin
      FlushCache;
      Changed;
    end;
  end;
end;

function TAstroPersianCalendar.IsLongitudeStored: Boolean;
begin
  Result := (FLongitude <> 52.5);
end;

{ TYearCache }

function TYearCache.Add(const AJDStart, AJDEnd: Extended;
  const AYear: Integer; const ALeap: Boolean): Integer;
var
  p: PYearCacheItem;
begin
  if (FSize <= 0) then
  begin
    Result := -1;
    Exit;
  end;

  New(p);
  p^.JDStart := AJDStart;
  p^.JDEnd := AJDEnd;
  p^.Year := AYear;
  p^.Leap := ALeap;

  if (Count = FSize) then
    Delete(0);
  Result := inherited Add(p);
end;

function TYearCache.FindValue(const AJD: Extended; out Year: Integer;
  out JDYearStart: Extended; out Leap: Boolean): Integer;
var
  p: PYearCacheItem;
begin
  Result := Count - 1;
  while (Result >= 0) do
  begin
    p := Items[Result];
    if Assigned(p) and (p^.JDStart <= AJD) and (AJD < p^.JDEnd) then
    begin
      Year := p^.Year;
      JDYearStart := p^.JDStart;
      Leap := p^.Leap;
      Break;
    end;
    Dec(Result);
  end;
end;

procedure TYearCache.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    Dispose(PYearCacheItem(Ptr));
end;

procedure TYearCache.SetSize(const Value: Integer);
begin
  FSize := Value;
  if FSize < 0 then
    FSize := 0;

  while FSize < Count do
    Delete(0);
end;

end.
