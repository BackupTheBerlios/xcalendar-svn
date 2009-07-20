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

unit AAEclipses;
// This unit provides for calculation of Solar and Lunar Eclipses. This refers to Chapter 54 in the book.

interface

type
  TAASolarEclipseDetails = record
    bEclipse: Boolean;
    TimeOfMaximumEclipse,
    F,
    u,
    gamma,
    GreatestMagnitude: Extended;
  end;

  TAALunarEclipseDetails = record
    bEclipse: Boolean;
    TimeOfMaximumEclipse,
    F,
    u,
    gamma,
    PenumbralRadii,
    UmbralRadii,
    PenumbralMagnitude,
    UmbralMagnitude,
    PartialPhaseSemiDuration,
    TotalPhaseSemiDuration,
    PartialPhasePenumbraSemiDuration: Extended;
  end;

  TAASolarEclipseType = (seNoEclipse, seCentralTotal, seCentralAnnular,
    seAnnularTotal, seNonCentralTotalOrAnnular, sePartial);

  TAALunarEclipseType = (leNoEclipse, leTotal, lePartial, lePenumbral);

function CalculateSolarEclipse(k: Extended): TAASolarEclipseDetails;
function GetSolarEclipseType(const SolarEclipseDetails: TAASolarEclipseDetails): TAASolarEclipseType;
function CalculateLunarEclipse(k: Extended): TAALunarEclipseDetails;
function GetLunarEclipseType(const LunarEclipseDetails: TAALunarEclipseDetails): TAALunarEclipseType;


implementation

uses
  AAMoonPhases,
  AACoordinateTransformation;

function Eclipses_Calculate(k: Extended; var Mdash: Extended): TAASolarEclipseDetails;
var
  T, T2, T3, T4, E, M, omega, F, Fdash, A1, DeltaJDE, P, Q, W, fgamma: Extended;
  bSolarEclipse: Boolean;
  
begin
  FillChar(Result, SizeOf(Result), 0);

  //Are we looking for a solar or lunar eclipse
  bSolarEclipse := (Frac(k) = 0);

  //convert from K to T
  T := k/1236.85;
  T2 := T*T;
  T3 := T2*T;
  T4 := T3*T;

  E := 1 - 0.002516*T - 0.0000074*T2;

  M := MapTo0To360Range(2.5534 + 29.10535670*k - 0.0000014*T2 - 0.00000011*T3);
  M := DegreesToRadians(M);

  Mdash := MapTo0To360Range(201.5643 + 385.81693528*k + 0.0107582*T2 + 0.00001238*T3 - 0.000000058*T4); 
  Mdash := DegreesToRadians(Mdash);

  omega := MapTo0To360Range(124.7746 - 1.56375588*k + 0.0020672*T2 + 0.00000215*T3);
  omega := DegreesToRadians(omega);

  F := MapTo0To360Range(160.7108 + 390.67050284*k - 0.0016118*T2 - 0.00000227*T3 + 0.00000001*T4);
  Result.F := F;
  Fdash := F - 0.02665*sin(omega);

  F := DegreesToRadians(F);
  Fdash := DegreesToRadians(Fdash);

  //Do the first check to see if we have an eclipse
  if (Abs(sin(F)) > 0.36) then
    Exit;

  A1 := MapTo0To360Range(299.77 + 0.107408*k - 0.009173*T2);
  A1 := DegreesToRadians(A1);

  Result.TimeOfMaximumEclipse := MoonPhases_MeanPhase(k);

  DeltaJDE := 0;
  if (bSolarEclipse) then
    DeltaJDE := DeltaJDE + -0.4075*sin(Mdash) +
                            0.1721*E*sin(M)
  else
    DeltaJDE := DeltaJDE + -0.4065*sin(Mdash) +
                            0.1727*E*sin(M);
               
  DeltaJDE := DeltaJDE +  0.0161*sin(2*Mdash) +
                       -0.0097*sin(2*Fdash) +
                        0.0073*E*sin(Mdash - M) +
                       -0.0050*E*sin(Mdash + M) +
                       -0.0023*sin(Mdash - 2*Fdash) +
                        0.0021*E*sin(2*M) +
                        0.0012*sin(Mdash + 2*Fdash) +
                        0.0006*E*sin(2*Mdash + M) +
                       -0.0004*sin(3*Mdash) +
                       -0.0003*E*sin(M + 2*Fdash) +
                        0.0003*sin(A1) +
                       -0.0002*E*sin(M - 2*Fdash) +
                       -0.0002*E*sin(2*Mdash - M) +
                       -0.0002*sin(omega);

  Result.TimeOfMaximumEclipse := Result.TimeOfMaximumEclipse + DeltaJDE;

  P := 0.2070*E*sin(M) +
       0.0024*E*sin(2*M) +
      -0.0392*sin(Mdash) +
       0.0116*sin(2*Mdash) +
      -0.0073*E*sin(Mdash + M) +
       0.0067*E*sin(Mdash - M) +
       0.0118*sin(2*Fdash);

  Q := 5.2207 +
      -0.0048*E*cos(M) +
       0.0020*E*cos(2*M) +
      -0.3299*cos(Mdash) +
      -0.0060*E*cos(Mdash + M) +
       0.0041*E*cos(Mdash - M);

  W := Abs(cos(Fdash));

  Result.gamma := (P*cos(Fdash) + Q*sin(Fdash))*(1 - 0.0048*W);

  Result.u := 0.0059 +
              0.0046*E*cos(M) +
             -0.0182*cos(Mdash) +
              0.0004*cos(2*Mdash) +
             -0.0005*cos(M + Mdash);

  //Check to see if the eclipse is visible from the Earth's surface
  if (Abs(Result.gamma) > (1.5433 + Result.u)) then
    Exit;

  //We have an eclipse at this time
  Result.bEclipse := True;

  //In the case of a partial eclipse, calculate its magnitude
  fgamma := Abs(Result.gamma);
  if (fgamma > 0.9972) and (fgamma < 1.5433 + Result.u) then
    Result.GreatestMagnitude := (1.5433 + Result.u - fgamma) / (0.5461 + 2*Result.u);
end;

{
function CalculateSolarEclipse(k: Extended): TAASolarEclipseDetails;

Return Value
A record containing the following values:
bEclipse True if a solar eclipse occurs at this New Moon.
TimeOfMaximumEclipse The date in Dynamical time of maximum eclipse.
F The moons argument of Latitude in degrees at the time of the eclipse.
u The U term for the eclipse.
gamma The gamma term for the eclipse.
GreatestMagnitude The greatest magnitude of the eclipse if the eclipse is partial.

Parameters
k The same K term as returned from AAMoonPhases.MoonPhases_K. For a solar eclipse, this value should be a value without any decimals as a solar eclipse refers to a New Moon.
}
function CalculateSolarEclipse(k: Extended): TAASolarEclipseDetails;
var
  Mdash: Extended;

begin
  Result := Eclipses_Calculate(k, Mdash);
end;

{
function GetSolarEclipseType(const SolarEclipseDetails: TAASolarEclipseDetails): TAASolarEclipseType;

Return Value
Returns the type of eclipse: 
  seNoEclipse: No eclipse
  seCentralTotal: Central total eclipse 
  seCentralAnnular: Central annular eclipse
  seAnnularTotal: Annular-total eclipse
  seNonCentralTotalOrAnnular: Non-central eclipse either total or annular
  sePartial: Partial eclipse

Parameters
SolarEclipseDetails The record returned from CalculateSolarEclipse to be analyzed
}
function GetSolarEclipseType(const SolarEclipseDetails: TAASolarEclipseDetails): TAASolarEclipseType;
var
  fgamma, omega: Extended;

begin
  Result := seNoEclipse;
  if not SolarEclipseDetails.bEclipse then
    Exit;

  fgamma := Abs(SolarEclipseDetails.gamma);

  if (fgamma < 0.9972) then
  { Central }
  begin
    if (SolarEclipseDetails.u < 0) then
      { Central Total }
      Result := seCentralTotal

    else if (SolarEclipseDetails.u > 0.0047) then
      { Central Annular }
      Result := seCentralAnnular

    else
    begin
      omega := 0.00464 * Sqrt(1 - fgamma * fgamma);
      if SolarEclipseDetails.u < omega then
        { Annular-Total }
        Result := seAnnularTotal
      else
        { Central Annular again }
        Result := seCentralAnnular;
    end;
  end
  else if (fgamma < 0.9972 + Abs(SolarEclipseDetails.u)) then
    { Non-central Total or Annular }
    Result := seNonCentralTotalOrAnnular

  else if (fgamma < 1.5433 + SolarEclipseDetails.u) then
    { Partial }
    Result := sePartial;

end;

{
function CalculateLunarEclipse(k: Extended): TAALunarEclipseDetails;

Return Value
A record containing the following values:
bEclipse True if a lunar eclipse occurs at this Full Moon.
TimeOfMaximumEclipse The date in Dynamical time of maximum eclipse.
F The moons argument of Latitude in degrees at the time of the eclipse.
u The U term for the eclipse.
gamma The gamma term for the eclipse.
PenumbralRadii The radii of the eclipse for the penumbra in equatorial earth radii.
UmbralRadii The radii of the eclipse for the umbra in equatorial earth radii.
PenumbralMagnitude The magnitude of the eclipse if the eclipse is penumbral.
UmbralMagnitude The magnitude of the eclipse if the eclipse is umbral.
PartialPhaseSemiDuration The semi-duration of the eclipse during the partial phase.
TotalPhaseSemiDuration The semi-duration of the eclipse during the total phase.
PartialPhasePenumbralSemiDuration The semi-duration of the partial phase in the penumbra.

Parameters
k The same K term as returned from AAMoonPhases.MoonPhases_K. For a lunar eclipse, this value should be an integer incremented by 0.5 as a lunar eclipse refers to a Full Moon.
}
function CalculateLunarEclipse(k: Extended): TAALunarEclipseDetails;
var
  Mdash, fgamma, p, t, n, gamma2, p2, t2, h, h2: Extended;
  solarDetails: TAASolarEclipseDetails;

begin
  FillChar(Result, SizeOf(Result), 0);

  solarDetails := Eclipses_Calculate(k, Mdash);

  Result.bEclipse := solarDetails.bEclipse;
  Result.F := solarDetails.F;
  Result.gamma := solarDetails.gamma;
  Result.TimeOfMaximumEclipse := solarDetails.TimeOfMaximumEclipse;
  Result.u := solarDetails.u;

  if (Result.bEclipse) then
  begin
    Result.PenumbralRadii := 1.2848 + Result.u;
    Result.UmbralRadii := 0.7403 - Result.u;
    fgamma := Abs(Result.gamma);
    Result.PenumbralMagnitude := (1.5573 + Result.u - fgamma) / 0.5450;
    Result.UmbralMagnitude := (1.0128 - Result.u - fgamma) / 0.5450;

    p := 1.0128 - Result.u;
    t := 0.4678 - Result.u;
    n := 0.5458 + 0.0400*cos(Mdash);

    gamma2 := Result.gamma*Result.gamma;
    p2 := p*p;
    if (p2 >= gamma2) then
      Result.PartialPhaseSemiDuration := 60/n*sqrt(p2 - gamma2);

    t2 := t*t;
    if (t2 >= gamma2) then
      Result.TotalPhaseSemiDuration := 60/n*sqrt(t2 - gamma2);

    h := 1.5573 + Result.u;
    h2 := h*h;
    if (h2 >= gamma2) then
      Result.PartialPhasePenumbraSemiDuration := 60/n*sqrt(h2 - gamma2);
  end;
end;

{
function GetLunarEclipseType(const LunarEclipseDetails: TAALunarEclipseDetails): TAALunarEclipseType;

Return Value
Returns the type of eclipse:
  leNoEclipse: No eclipse
  leTotal: Total eclipse
  lePartial: Partial eclipse
  lePenumbral: Penumbral eclipse

Parameters
LunarEclipseDetails The record returned from CalculateLunarEclipse to be analyzed
}
function GetLunarEclipseType(const LunarEclipseDetails: TAALunarEclipseDetails): TAALunarEclipseType;
begin
  Result := leNoEclipse;
  if not LunarEclipseDetails.bEclipse then
    Exit;
  if LunarEclipseDetails.PenumbralMagnitude <= 0 then
    Exit;

  if LunarEclipseDetails.UmbralMagnitude <= 0 then
    Result := lePenumbral
  else if LunarEclipseDetails.UmbralMagnitude < 1 then
    Result := lePartial
  else
    Result := leTotal;
end;


end.
