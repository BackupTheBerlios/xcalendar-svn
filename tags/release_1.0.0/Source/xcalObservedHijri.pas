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

unit xcalObservedHijri;

{$I xcalDefs.inc}

interface

uses
  xcalClass, xcalHijri, SysUtils, Classes;

type

  TFindHijriLeapYearsEvent = procedure(const Year: Word; var IsLeap: Boolean) of object;

  TObservedHijriCalendar = class(THijriCalendar)
  private
    FOnFindLeapYears: TFindHijriLeapYearsEvent;
    FBaseYear: Word;
    FBaseYearAdjustment: ShortInt;
    FMinObservedYear, FMaxObservedYear: Word;

    FYearAdjustmentsAfter: packed array of ShortInt;
    FYearAdjustmentsBefore: packed array of ShortInt;
    FCacheMinYear, FCacheMaxYear: Word;
    FAutoLeapAfter, FAutoLeapBefore: Boolean;

    procedure FillYearAdjustmentsAfter;
    procedure FillYearAdjustmentsBefore;

    procedure SetOnFindLeapYears(const Value: TFindHijriLeapYearsEvent);
    procedure SetBaseYear(const Value: Word);
    procedure SetBaseYearAdjustment(const Value: ShortInt);
    procedure SetMaxObservedYear(const Value: Word);
    procedure SetMinObservedYear(const Value: Word);
  protected
    procedure SetLeapYearsKind(const Value: THijriLeapYearsKind); override;

    function GetYearAdjustment(const AYear: Word): ShortInt;
  public
    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; override;
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; override;
    function IsLeapYear(Year: Word): Boolean; override;

    procedure InitYearAdjustmentCache;
    procedure FlushYearAdjustmentCache;
  published
    { The base year for calculating year adjustments }
    property BaseYear: Word read FBaseYear write SetBaseYear default 0;
   { Specifies the offset of the conventional year start relative to the observed
     year start for the base year. }
    property BaseYearAdjustment: ShortInt read FBaseYearAdjustment write SetBaseYearAdjustment default 0;
    { Minimum year that you have observational information for }
    property MinObservedYear: Word read FMinObservedYear write SetMinObservedYear default 0;
    { Maximum year that you have observational information for }
    property MaxObservedYear: Word read FMaxObservedYear write SetMaxObservedYear default 0;

    { You should use this event to specify the observed common and leap years. }
    property OnFindLeapYears: TFindHijriLeapYearsEvent read FOnFindLeapYears write SetOnFindLeapYears;
  end;

  
implementation

{ TObservedHijriCalendar }

const
  { Number of days in a common year }
  D1 = 354;

  { Number of days in a 30-year cycle }
  D30 = D1 * 30 + 11;

  { Number of days between the epoch of Hijri Calendar and TDateTime basis (12/31/1899 A.D.) }
  TDateTimeDateDelta = 466580;

  { Number of days between TTimeStamp basis (1/1/0001 A.D.) and the epoch of Hijri Calendar }
  TTimeStampDateDelta = 227014;


function TObservedHijriCalendar.TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Word;
  Leap: Boolean;
  YearAdjustment: ShortInt;
  DS: Integer;
begin
  Result := False;
  Leap := IsLeapYear(Year);
  if (Year >= MinYear) and (Year <= MaxYear) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= GetDaysInAMonth(Year, Month, Leap)) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, GetDaysInAMonth(Year, I, Leap));

    YearAdjustment := GetYearAdjustment(Year);
    if YearAdjustment <> 0 then
    begin
      DS := Day;
      Dec(DS, YearAdjustment);
      if DS <= 0 then
      begin
        Dec(Year);
        Leap := inherited IsLeapYear(Year);
        Inc(DS, XCalHijriDaysPerYear[Leap]);
      end
      else
      begin
        Leap := inherited IsLeapYear(Year);
        if DS > XCalHijriDaysPerYear[Leap] then
        begin
          Inc(Year);
          Dec(DS, XCalHijriDaysPerYear[Leap]);
        end;
      end;
      Day := DS;
    end;

    DivMod(Year - 1, 30, Year, I);
    while (I > 0) do
    begin
      Inc(Day, D1);
      if inherited IsLeapYear(I) then
        Inc(Day);
      Dec(I);
    end; { maximum value for Day would be 354 + D30 = 10985 which fits in a Word }
    Date := Year * D30 + Day - TDateTimeDateDelta - HijriAdjustment;
    Result := True;
  end;
end;

function TObservedHijriCalendar.DecodeDateFully(const DateTime: TDateTime;
  var Year, Month, Day, DOW: Word): Boolean;
var
  Y, M, D, I: Word;
  T: Integer;
  YearAdjustment: ShortInt;
  DS: Integer;
begin
  T := DateTimeToTimeStamp(DateTime).Date;
  DOW := T mod 7 + 1;
  T := T - TTimeStampDateDelta + HijriAdjustment;
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
      Result := inherited IsLeapYear(I);
      if (Result and (D < D1 + 1)) or (D < D1) then Break;
      Dec(D, D1);
      if Result then
        Dec(D);
      Inc(Y);
    end;

    Result := IsLeapYear(Y);
    YearAdjustment := GetYearAdjustment(Y);
    if YearAdjustment <> 0 then
    begin
      DS := D;
      Inc(DS, YearAdjustment);
      if DS < 0 then
      begin
        Dec(Y);
        Result := IsLeapYear(Y);
        Inc(DS, XCalHijriDaysPerYear[Result]);
      end
      else if DS >= XCalHijriDaysPerYear[Result] then
      begin
        Inc(Y);
        Dec(DS, XCalHijriDaysPerYear[Result]);
        Result := IsLeapYear(Y);
      end;
      D := DS;
    end;

    M := 1;
    while True do
    begin
      I := GetDaysInAMonth(Y, M, Result);
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

function TObservedHijriCalendar.IsLeapYear(Year: Word): Boolean;
begin
  Result := inherited IsLeapYear(Year);
  if Year < FMinObservedYear then
  begin
    if Year >= FCacheMinYear then
      Result := FAutoLeapBefore;
  end
  else if Year > FMaxObservedYear then
  begin
    if Year <= FCacheMaxYear then
      Result := FAutoLeapAfter;
  end
  else
    if Assigned(FOnFindLeapYears) then
      FOnFindLeapYears(Year, Result);
end;

procedure TObservedHijriCalendar.SetOnFindLeapYears(const Value: TFindHijriLeapYearsEvent);
begin
  FOnFindLeapYears := Value;
  if not (csLoading in ComponentState) then
  begin
    FlushYearAdjustmentCache;
    Changed;
  end;
end;

procedure TObservedHijriCalendar.SetLeapYearsKind(const Value: THijriLeapYearsKind);
begin
  inherited;
  if not (csLoading in ComponentState) then
    FlushYearAdjustmentCache;
end;

procedure TObservedHijriCalendar.SetMaxObservedYear(const Value: Word);
begin
  if (Value < FBaseYear) or (Value < FMinObservedYear) then
    if not (csLoading in ComponentState) then
      Exit;

  if Value <> FMaxObservedYear then
  begin
    FMaxObservedYear := Value;
    if not (csLoading in ComponentState) then
    begin
      FlushYearAdjustmentCache;
      Changed;
    end;
  end;
end;

procedure TObservedHijriCalendar.SetMinObservedYear(const Value: Word);
begin
  if (Value > FBaseYear) or (Value > FMaxObservedYear) then
    if not (csLoading in ComponentState) then
      Exit;

  if Value <> FMinObservedYear then
  begin
    FMinObservedYear := Value;
    if not (csLoading in ComponentState) then
    begin
      FlushYearAdjustmentCache;
      Changed;
    end;
  end;
end;

procedure TObservedHijriCalendar.InitYearAdjustmentCache;
begin
  if (FBaseYear = 0) or (FMinObservedYear = 0) or (FMaxObservedYear = 0) then
    Exit;

  FillYearAdjustmentsAfter;
  FillYearAdjustmentsBefore;
end;

procedure TObservedHijriCalendar.FlushYearAdjustmentCache;
begin
  FCacheMinYear := 0;
  FCacheMaxYear := 0;
  SetLength(FYearAdjustmentsAfter, 0);
  SetLength(FYearAdjustmentsBefore, 0);
end;

procedure TObservedHijriCalendar.SetBaseYear(const Value: Word);
begin
  if Value <> FBaseYear then
  begin
    FBaseYear := Value;
    if FBaseYear > FMaxObservedYear then
      FMaxObservedYear := FBaseYear
    else if FBaseYear < FMinObservedYear then
      FMinObservedYear := FBaseYear;

    if not (csLoading in ComponentState) then
    begin
      FlushYearAdjustmentCache;
      Changed;
    end;
  end;
end;

procedure TObservedHijriCalendar.SetBaseYearAdjustment(const Value: ShortInt);
begin
  if Value <> FBaseYearAdjustment then
  begin
    FBaseYearAdjustment := Value;
    if not (csLoading in ComponentState) then
    begin
      FlushYearAdjustmentCache;
      Changed;
    end;
  end;
end;

procedure TObservedHijriCalendar.FillYearAdjustmentsAfter;
var
  Adj: ShortInt;
  I: Integer;
  Y: Word;
  CL, OL: Boolean;
  Count: Integer;
begin
  if Assigned(FOnFindLeapYears) then
  begin
    Y := FBaseYear;
    Adj := FBaseYearAdjustment;
    Count := FMaxObservedYear - FBaseYear;
    SetLength(FYearAdjustmentsAfter, Count + 6);
    for I := 0 to Count - 1 do
    begin
      CL := inherited IsLeapYear(Y);
      OL := CL;
      FOnFindLeapYears(Y, OL);
      if (CL <> OL) then
        if CL then
          Inc(Adj)
        else
          Dec(Adj);
      FYearAdjustmentsAfter[I] := Adj;
      Inc(Y);
    end;

    if Adj <> 0 then
    begin
      FAutoLeapAfter := (Adj > 0);
      I := Count;
      repeat
        CL := inherited IsLeapYear(Y);
        if (CL <> FAutoLeapAfter) then
          if CL then
            Inc(Adj)
          else
            Dec(Adj);
        if I >= Length(FYearAdjustmentsAfter) then
          SetLength(FYearAdjustmentsAfter, Length(FYearAdjustmentsAfter) + 30);
        FYearAdjustmentsAfter[I] := Adj;
        Inc(I);
        Inc(Y);
      until (Adj = 0);
      Dec(Y);
    end;
    FCacheMaxYear := Y;
    SetLength(FYearAdjustmentsAfter, FCacheMaxYear - FBaseYear);
  end;
end;

procedure TObservedHijriCalendar.FillYearAdjustmentsBefore;
var
  Adj: ShortInt;
  I: Integer;
  Y: Word;
  CL, OL: Boolean;
  Count: Integer;
begin
  if Assigned(FOnFindLeapYears) then
  begin
    Y := FBaseYear;
    Adj := FBaseYearAdjustment;
    Count := FBaseYear - FMinObservedYear;
    SetLength(FYearAdjustmentsBefore, Count + 6);
    for I := 0 to Count - 1 do
    begin
      Dec(Y);
      CL := inherited IsLeapYear(Y);
      OL := CL;
      FOnFindLeapYears(Y, OL);
      if (CL <> OL) then
        if CL then
          Dec(Adj)
        else
          Inc(Adj);
      FYearAdjustmentsBefore[I] := Adj;
    end;

    if Adj <> 0 then
    begin
      FAutoLeapBefore := (Adj < 0);
      I := Count;
      repeat
        Dec(Y);
        CL := inherited IsLeapYear(Y);
        if (CL <> FAutoLeapBefore) then
          if CL then
            Dec(Adj)
          else
            Inc(Adj);
        if I >= Length(FYearAdjustmentsBefore) then
          SetLength(FYearAdjustmentsBefore, Length(FYearAdjustmentsBefore) + 30);
        FYearAdjustmentsBefore[I] := Adj;
        Inc(I);
      until (Adj = 0);
      Inc(Y);
    end;
    FCacheMinYear := Y;
    SetLength(FYearAdjustmentsBefore, FBaseYear - FCacheMinYear);
  end;
end;

function TObservedHijriCalendar.GetYearAdjustment(const AYear: Word): ShortInt;
begin
  Result := 0;
  if (FBaseYear = 0) or (FMinObservedYear = 0) or (FMaxObservedYear = 0) then
    Exit;

  if AYear = FBaseYear then
    Result := FBaseYearAdjustment
  else if AYear > FBaseYear then
  begin
    if FCacheMaxYear = 0 then
      FillYearAdjustmentsAfter;
    if AYear <= FCacheMaxYear then
      Result := FYearAdjustmentsAfter[AYear - FBaseYear - 1];
  end
  else { AYear < FBaseYear }
  begin
    if FCacheMinYear = 0 then
      FillYearAdjustmentsBefore;
    if AYear >= FCacheMinYear then
      Result := FYearAdjustmentsBefore[FBaseYear - AYear - 1];
  end;
end;

end.

