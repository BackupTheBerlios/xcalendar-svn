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

unit xcalAstroEvents;

{$I xcalDefs.inc}

interface

uses
  Classes, xcalEvents;

type

  TXCalAstroEventEquinoxTitles = class(TPersistent)
  private
    FWinterSolstice: string;
    FSummerSolstice: string;
    FSpringEquinox: string;
    FAutumnEquinox: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property SpringEquinox: string read FSpringEquinox write FSpringEquinox;
    property SummerSolstice: string read FSummerSolstice write FSummerSolstice;
    property AutumnEquinox: string read FAutumnEquinox write FAutumnEquinox;
    property WinterSolstice: string read FWinterSolstice write FWinterSolstice;
  end;

  TXCalAstroEventEquinoxes = class(TPersistent)
  private
    FEnabled: Boolean;
    FTitles: TXCalAstroEventEquinoxTitles;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Titles: TXCalAstroEventEquinoxTitles read FTitles;
  end;


  TXCalAstroEventMoonPhaseTitles = class(TPersistent)
  private
    FNewMoon: string;
    FLastQuarter: string;
    FFullMoon: string;
    FFirstQuarter: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property NewMoon: string read FNewMoon write FNewMoon;
    property FirstQuarter: string read FFirstQuarter write FFirstQuarter;
    property FullMoon: string read FFullMoon write FFullMoon;
    property LastQuarter: string read FLastQuarter write FLastQuarter;
  end;

  TXCalAstroEventMoonPhases = class(TPersistent)
  private
    FEnabled: Boolean;
    FTitles: TXCalAstroEventMoonPhaseTitles;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Titles: TXCalAstroEventMoonPhaseTitles read FTitles;
  end;


  TXCalAstroEventEclipseTitles = class(TPersistent)
  private
    FSolarEclipseTotal: string;
    FSolarEclipsePartial: string;
    FSolarEclipseAnnular: string;
    FLunarEclipseTotal: string;
    FLunarEclipsePartial: string;
    FLunarEclipsePenumbral: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property SolarEclipseTotal: string read FSolarEclipseTotal write FSolarEclipseTotal;
    property SolarEclipsePartial: string read FSolarEclipsePartial write FSolarEclipsePartial;
    property SolarEclipseAnnular: string read FSolarEclipseAnnular write FSolarEclipseAnnular;
    property LunarEclipseTotal: string read FLunarEclipseTotal write FLunarEclipseTotal;
    property LunarEclipsePartial: string read FLunarEclipsePartial write FLunarEclipsePartial;
    property LunarEclipsePenumbral: string read FLunarEclipsePenumbral write FLunarEclipsePenumbral;
  end;

  TXCalAstroEventEclipses = class(TPersistent)
  private
    FEnabled: Boolean;
    FTitles: TXCalAstroEventEclipseTitles;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Titles: TXCalAstroEventEclipseTitles read FTitles;
  end;

  TXCalAstroEventType = ( aeSpringEquinox,
                          aeSummerSolstice,
                          aeAutumnEquinox,
                          aeWinterSolstice,
                          aeNewMoon,
                          aeFirstQuarterMoon,
                          aeFullMoon,
                          aeLastQuarterMoon,
                          aeSolarEclipseTotal,
                          aeSolarEclipsePartial,
                          aeSolarEclipseAnnular,
                          aeLunarEclipseTotal,
                          aeLunarEclipsePartial,
                          aeLunarEclipsePenumbral
                        );

  TXCalendarAstroEvents = class;
  
  TXCalAstroEventOccurEvent = procedure(Sender: TXCalendarAstroEvents; EventType: TXCalAstroEventType;
    var OccurenceDate: TDateTime; var DisplayText: string; var AllowOccurence: Boolean) of Object;


  TXCalendarAstroEvents = class(TCustomXCalendarEvents)
  private
    FEquinoxes: TXCalAstroEventEquinoxes;
    FMoonPhases: TXCalAstroEventMoonPhases;
    FEclipses: TXCalAstroEventEclipses;
    FTimeZone: Extended;
    FOnOccur: TXCalAstroEventOccurEvent;
    FFormatTitles: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FindIntervalEvents(const FromDate, ToDate: TDateTime;
      var EOL: TXCalEventOccurenceList); override;
  published
    property Equinoxes: TXCalAstroEventEquinoxes read FEquinoxes;
    property MoonPhases: TXCalAstroEventMoonPhases read FMoonPhases;
    property Eclipses: TXCalAstroEventEclipses read FEclipses;
    property TimeZone: Extended read FTimeZone write FTimeZone;
    property FormatTitles: Boolean read FFormatTitles write FFormatTitles default True;

    property OnOccur: TXCalAstroEventOccurEvent read FOnOccur write FOnOccur;
  end;


  
implementation

uses
  SysUtils, xcalClass, Math, AAEquinox, AADynamicalTime, AADate,
  AAMoonPhases, AAEclipses;

{ TXCalendarAstroEvents }

constructor TXCalendarAstroEvents.Create(AOwner: TComponent);
begin
  inherited;
  FEquinoxes := TXCalAstroEventEquinoxes.Create;
  FMoonPhases := TXCalAstroEventMoonPhases.Create;
  FEclipses := TXCalAstroEventEclipses.Create;
  FFormatTitles := True;
end;

destructor TXCalendarAstroEvents.Destroy;
begin
  FEquinoxes.Free;
  FMoonPhases.Free;
  FEclipses.Free;
  inherited;
end;

procedure TXCalendarAstroEvents.FindIntervalEvents(const FromDate,
  ToDate: TDateTime; var EOL: TXCalEventOccurenceList);
var
  JDStart, JDEnd, K, MaxK, F, JDE: Extended;
  AAD: TAADate;
  Y, Y1, Y2: Integer;
  MinJDE, MaxJDE: Extended;
  EventType: TXCalAstroEventType;
  SolarEclipseDetails: TAASolarEclipseDetails;
  LunarEclipseDetails: TAALunarEclipseDetails;
  SolarEclipseType: TAASolarEclipseType;
  LunarEclipseType: TAALunarEclipseType;
  I: Integer;

      function GetYear(const DT: TDateTime): Integer;
      var
        JD: Extended;
        AAD: TAADate;
      begin
        JD := DateTimeToJulianDay(DT);
        AAD := TAADate.Create(JD, TAADate.AfterPapalReform(JD));
        try
          Result := AAD.Year;
        finally
          AAD.Free;
        end;
      end;

      { Common codes when a JDE is calculated }
      procedure DoJDE(const JDE: Extended; AEventType: TXCalAstroEventType);
      var
        JD: Extended;
        DT: TDateTime;
        s: string;
        AllowOccurence: Boolean;
      begin
        if (MinJDE <= JDE) and (JDE < MaxJDE) then
        begin
          JD := JDE - (DynamicalTime_DeltaT(JDE) / (24 * 60 * 60));
          JD := JD + (FTimeZone / 24);
          if TryJulianDayToDateTime(JD, DT) then
          begin
            case AEventType of
              aeSpringEquinox:         s := FEquinoxes.Titles.SpringEquinox;
              aeSummerSolstice:        s := FEquinoxes.Titles.SummerSolstice;
              aeAutumnEquinox:         s := FEquinoxes.Titles.AutumnEquinox;
              aeWinterSolstice:        s := FEquinoxes.Titles.WinterSolstice;
              aeNewMoon:               s := FMoonPhases.Titles.NewMoon;
              aeFirstQuarterMoon:      s := FMoonPhases.Titles.FirstQuarter;
              aeFullMoon:              s := FMoonPhases.Titles.FullMoon;
              aeLastQuarterMoon:       s := FMoonPhases.Titles.LastQuarter;
              aeSolarEclipseTotal:     s := FEclipses.Titles.SolarEclipseTotal;
              aeSolarEclipsePartial:   s := FEclipses.Titles.SolarEclipseAnnular;
              aeSolarEclipseAnnular:   s := FEclipses.Titles.SolarEclipsePartial;
              aeLunarEclipseTotal:     s := FEclipses.Titles.LunarEclipseTotal;
              aeLunarEclipsePartial:   s := FEclipses.Titles.LunarEclipsePartial;
              aeLunarEclipsePenumbral: s := FEclipses.Titles.LunarEclipsePenumbral;
            end;

            AllowOccurence := True;
            if Assigned(FOnOccur) then
              FOnOccur(Self, AEventType, DT, s, AllowOccurence);

            if AllowOccurence then
            begin
              if FFormatTitles then
                s := FormatDateTime(s, DT);
                
              EOL.Add(DT, s, False, nil);
            end;
          end;
        end;
      end;

begin
  JDStart := DateTimeToJulianDay(Int(FromDate));
  MinJDE := JDStart + (DynamicalTime_DeltaT(JDStart) / (24 * 60 * 60));
  JDEnd := DateTimeToJulianDay(Int(ToDate) + 1);
  MaxJDE := JDEnd + (DynamicalTime_DeltaT(JDEnd) / (24 * 60 * 60));

  if FEquinoxes.Enabled then
  begin
    Y1 := GetYear(FromDate);
    if ToDate = FromDate then
      Y2 := Y1
    else
      Y2 := GetYear(ToDate);

    for Y := Y1 to Y2 do
    begin
      DoJDE(SpringEquinox(Y), aeSpringEquinox);
      DoJDE(SummerSolstice(Y), aeSummerSolstice);
      DoJDE(AutumnEquinox(Y), aeAutumnEquinox);
      DoJDE(WinterSolstice(Y), aeWinterSolstice);
    end;
  end;

  if FMoonPhases.Enabled then
  begin
    AAD := TAADate.Create(JDStart, TAADate.AfterPapalReform(JDStart));
    try
      K := MoonPhases_K(AAD.FractionalYear, mpWhichever) - 0.25;
    finally
      AAD.Free;
    end;

    F := Frac(K);
    if F < 0 then
      F := F + 1;

    I := 0;
    if F = 0.25 then
      I := 1
    else if F = 0.50 then
      I := 2
    else if F = 0.75 then
      I := 3;

    repeat
      JDE := MoonPhases_TruePhase(K);
      EventType := TXCalAstroEventType(I + Ord(aeNewMoon));
      DoJDE(JDE, EventType);
      K := K + 0.25;
      Inc(I);
      if I = 4 then
        I := 0;
    until (JDE >= MaxJDE);
  end;

  if FEclipses.Enabled then
  begin
    AAD := TAADate.Create(JDStart, TAADate.AfterPapalReform(JDStart));
    try
      K := MoonPhases_K(AAD.FractionalYear, mpWhichever) - 0.25;

      AAD._Set(JDEnd, TAADate.AfterPapalReform(JDEnd));
      MaxK := MoonPhases_K(AAD.FractionalYear, mpWhichever) + 0.25;
    finally
      AAD.Free;
    end;

    K := Int(K * 2) / 2;

    F := Frac(K);
    if F < 0 then
      F := F + 1;

    I := 0;
    if F = 0.50 then
      I := 1;

    repeat

      case I of
        0:
        begin
          SolarEclipseDetails := CalculateSolarEclipse(K);
          SolarEclipseType := GetSolarEclipseType(SolarEclipseDetails);
          if SolarEclipseType <> seNoEclipse then
          begin
            case SolarEclipseType of
              seCentralTotal, seAnnularTotal: EventType := aeSolarEclipseTotal;
              seCentralAnnular:               EventType := aeSolarEclipseAnnular;
              else {sePartial, seNonCentralTotalOrAnnular:}
                                              EventType := aeSolarEclipsePartial;
            end;

            DoJDE(SolarEclipseDetails.TimeOfMaximumEclipse, EventType);
          end;
        end;

        1:
        begin
          LunarEclipseDetails := CalculateLunarEclipse(K);
          LunarEclipseType := GetLunarEclipseType(LunarEclipseDetails);
          if LunarEclipseType <> leNoEclipse then
          begin
            case LunarEclipseType of
              leTotal:   EventType := aeLunarEclipseTotal;
              lePartial: EventType := aeLunarEclipsePartial;
              else {lePenumbral:}
                         EventType := aeLunarEclipsePenumbral;
            end;

            DoJDE(LunarEclipseDetails.TimeOfMaximumEclipse, EventType);
          end;
        end;
      end;

      K := K + 0.5;
      Inc(I);
      if I = 2 then
        I := 0;
    until (K >= MaxK);
  end;

end;

{ TXCalAstroEventEquinoxes }

constructor TXCalAstroEventEquinoxes.Create;
begin
  inherited;
  FEnabled := True;
  FTitles := TXCalAstroEventEquinoxTitles.Create;
end;

destructor TXCalAstroEventEquinoxes.Destroy;
begin
  FTitles.Free;
  inherited;
end;

{ TXCalAstroEventEclipses }

constructor TXCalAstroEventEclipses.Create;
begin
  inherited;
  FEnabled := True;
  FTitles := TXCalAstroEventEclipseTitles.Create;
end;

destructor TXCalAstroEventEclipses.Destroy;
begin
  FTitles.Free;
  inherited;
end;

{ TXCalAstroEventEquinoxTitles }

procedure TXCalAstroEventEquinoxTitles.Assign(Source: TPersistent);
begin
  if (Source is TXCalAstroEventEquinoxTitles) then
  begin
    FSpringEquinox := TXCalAstroEventEquinoxTitles(Source).FSpringEquinox;
    FSummerSolstice := TXCalAstroEventEquinoxTitles(Source).FSummerSolstice;
    FAutumnEquinox := TXCalAstroEventEquinoxTitles(Source).FAutumnEquinox;
    FWinterSolstice := TXCalAstroEventEquinoxTitles(Source).FWinterSolstice;
  end
  else
    inherited Assign(Source);
end;

constructor TXCalAstroEventEquinoxTitles.Create;
begin
  inherited;
  FSpringEquinox := '"Spring Equinox" (hh:nn)';
  FSummerSolstice := '"Summer Solstice" (hh:nn)';
  FAutumnEquinox := '"Autumn Equinox" (hh:nn)';
  FWinterSolstice := '"Winter Solstice" (hh:nn)';
end;

{ TXCalAstroEventEclipseTitles }

procedure TXCalAstroEventEclipseTitles.Assign(Source: TPersistent);
begin
  if (Source is TXCalAstroEventEclipseTitles) then
  begin
    FSolarEclipseTotal := TXCalAstroEventEclipseTitles(Source).FSolarEclipseTotal;
    FSolarEclipsePartial := TXCalAstroEventEclipseTitles(Source).FSolarEclipsePartial;
    FSolarEclipseAnnular := TXCalAstroEventEclipseTitles(Source).FSolarEclipseAnnular;
    FLunarEclipseTotal := TXCalAstroEventEclipseTitles(Source).FLunarEclipseTotal;
    FLunarEclipsePartial := TXCalAstroEventEclipseTitles(Source).FLunarEclipsePartial;
    FLunarEclipsePenumbral := TXCalAstroEventEclipseTitles(Source).FLunarEclipsePenumbral;
  end
  else
    inherited Assign(Source);
end;

constructor TXCalAstroEventEclipseTitles.Create;
begin
  inherited;
  FSolarEclipseTotal := '"Total Solar Eclipse" (hh:nn)';
  FSolarEclipsePartial := '"Partial Solar Eclipse" (hh:nn)';
  FSolarEclipseAnnular := '"Annular Solar Eclipse" (hh:nn)';
  FLunarEclipseTotal := '"Total Lunar Eclipse" (hh:nn)';
  FLunarEclipsePartial := '"Partial Lunar Eclipse" (hh:nn)';
  FLunarEclipsePenumbral := '"Penumbral Lunar Eclipse" (hh:nn)';
end;

{ TXCalAstroEventMoonPhaseTitles }

procedure TXCalAstroEventMoonPhaseTitles.Assign(Source: TPersistent);
begin
  if (Source is TXCalAstroEventMoonPhaseTitles) then
  begin
    FNewMoon := TXCalAstroEventMoonPhaseTitles(Source).FNewMoon;
    FFirstQuarter := TXCalAstroEventMoonPhaseTitles(Source).FFirstQuarter;
    FFullMoon := TXCalAstroEventMoonPhaseTitles(Source).FFullMoon;
    FLastQuarter := TXCalAstroEventMoonPhaseTitles(Source).FLastQuarter;
  end
  else
    inherited Assign(Source);
end;

constructor TXCalAstroEventMoonPhaseTitles.Create;
begin
  inherited;
  FNewMoon := '"New Moon" (hh:nn)';
  FFirstQuarter := '"First Quarter Moon" (hh:nn)';
  FFullMoon := '"Full Moon" (hh:nn)';
  FLastQuarter := '"Last Quarter Moon" (hh:nn)';
end;

{ TXCalAstroEventMoonPhases }

constructor TXCalAstroEventMoonPhases.Create;
begin
  inherited;
  FEnabled := True;
  FTitles := TXCalAstroEventMoonPhaseTitles.Create;
end;

destructor TXCalAstroEventMoonPhases.Destroy;
begin
  FTitles.Free;
  inherited;
end;

end.
