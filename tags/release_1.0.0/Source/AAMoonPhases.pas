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

unit AAMoonPhases;
// This unit provides algorithms which obtain the dates for the phases of the Moon. This refers to Chapter 49 in the book.

interface

type
  TAAMoonPhaseType = (mpWhichever, mpNewMoon, mpFirstQuarter, mpFullMoon, mpLastQuarter);

function MoonPhases_K(Year: Extended; MoonPhaseType: TAAMoonPhaseType = mpWhichever): Extended;
function MoonPhases_MeanPhase(k: Extended): Extended;
function MoonPhases_TruePhase(k: Extended): Extended;


implementation

uses
  AACoordinateTransformation,
  Math;

{
function MoonPhases_K(Year: Extended): Extended;

Return Value
Returns the approximate value of K (required by the other functions) for calculation of the specified phase.

Parameters
Year The Year including decimals to calculate the K value for.
MoonPhaseType The type of moon phase you want:
  mpWhichever: The type of moon phase doesn't matter
  mpNewMoon: New Moon
  mpFirstQuarter: First Quarter
  mpFullMoon: Full Moon
  mpLastQuarter: Last Quarter

Remarks
An integer value of k gives a New Moon,
an integer increased by 0.25 gives a First Quarter,
an integer increased by 0.50 gives a Full Moon,
an integer increased by 0.75 gives a Last Quarter.
}
function MoonPhases_K(Year: Extended; MoonPhaseType: TAAMoonPhaseType): Extended;
var
  ApproxK: Extended;

begin
  ApproxK := 12.3685*(Year - 2000);

  Result := 0;
  if MoonPhaseType = mpWhichever then
    Result := Round(ApproxK * 4) / 4 
  else
  begin
    case MoonPhaseType of
      mpNewMoon:      Result := Floor(ApproxK);
      mpFirstQuarter: Result := Floor(ApproxK - 0.25) + 0.25;
      mpFullMoon:     Result := Floor(ApproxK - 0.50) + 0.50;
      mpLastQuarter:  Result := Floor(ApproxK - 0.75) + 0.75;
    end;
    if (ApproxK - Result >= 0.5) then
      Result := Result + 1;
  end;
end;

{
function MoonPhases_MeanPhase(k: Extended): Extended;

Return Value
Returns the date in Dynamical time when the specified mean moon phase occurs.

Parameters
k The K value to calculate the phase for.
}
function MoonPhases_MeanPhase(k: Extended): Extended;
var
  T, T2, T3, T4: Extended;

begin
  //convert from K to T
  T := k/1236.85;
  T2 := T*T;
  T3 := T2*T;
  T4 := T3*T;

  Result := 2451550.09766 + 29.530588861*k + 0.00015437*T2 - 0.000000150*T3 + 0.00000000073*T4;
end;

{
function MoonPhases_TruePhase(k: Extended): Extended;

Return Value
Returns the date in Dynamical time when the specified true moon phase occurs.

Parameters
k The K value to calculate the phase for.
}
function MoonPhases_TruePhase(k: Extended): Extended;
var
  JDE, T, T2, T3, T4, E, E2, M, Mdash, F, omega,
  A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
  kfrac, DeltaJDE, W, DeltaJDE2: Extended;

begin
  //What will be the return value
  JDE := MoonPhases_MeanPhase(k);

  //convert from K to T
  T := k/1236.85;
  T2 := T*T;
  T3 := T2*T;
  T4 := T3*T;

  E := 1 - 0.002516*T - 0.0000074*T2;
  E2 := E*E;

  M := MapTo0To360Range(2.5534 + 29.10535670*k - 0.0000014*T2 - 0.00000011*T3);
  M := DegreesToRadians(M);
  Mdash := MapTo0To360Range(201.5643 + 385.81693528*k + 0.0107582*T2 + 0.00001238*T3 - 0.000000058*T4);
  Mdash := DegreesToRadians(Mdash);
  F := MapTo0To360Range(160.7108 + 390.67050284*k - 0.0016118*T2 - 0.00000227*T3 + 0.00000001*T4);
  F := DegreesToRadians(F);
  omega := MapTo0To360Range(124.7746 - 1.56375588*k + 0.0020672*T2 + 0.00000215*T3);
  omega := DegreesToRadians(omega);
  A1 := MapTo0To360Range(299.77 + 0.107408*k - 0.009173*T2);
  A1 := DegreesToRadians(A1);
  A2 := MapTo0To360Range(251.88 + 0.016321*k);
  A2 := DegreesToRadians(A2);
  A3 := MapTo0To360Range(251.83 + 26.651886*k);
  A3 := DegreesToRadians(A3);
  A4 := MapTo0To360Range(349.42 + 36.412478*k);
  A4 := DegreesToRadians(A4);
  A5 := MapTo0To360Range(84.66 + 18.206239*k);
  A5 := DegreesToRadians(A5);
  A6 := MapTo0To360Range(141.74 + 53.303771*k);
  A6 := DegreesToRadians(A6);
  A7 := MapTo0To360Range(207.14 + 2.453732*k);
  A7 := DegreesToRadians(A7);
  A8 := MapTo0To360Range(154.84 + 7.306860*k);
  A8 := DegreesToRadians(A8);
  A9 := MapTo0To360Range(34.52 + 27.261239*k);
  A9 := DegreesToRadians(A9);
  A10 := MapTo0To360Range(207.19 + 0.121824*k);
  A10 := DegreesToRadians(A10);
  A11 := MapTo0To360Range(291.34 + 1.844379*k);
  A11 := DegreesToRadians(A11);
  A12 := MapTo0To360Range(161.72 + 24.198154*k);
  A12 := DegreesToRadians(A12);
  A13 := MapTo0To360Range(239.56 + 25.513099*k);
  A13 := DegreesToRadians(A13);
  A14 := MapTo0To360Range(331.55 + 3.592518*k);
  A14 := DegreesToRadians(A14);

  //convert to radians
  kfrac := Frac(k);
  if (kfrac < 0) then
    kfrac := 1 + kfrac;
    
  if (kfrac = 0) then//New Moon
  begin
    DeltaJDE := -0.40720*sin(Mdash) +
                 0.17241*E*sin(M) +
                 0.01608*sin(2*Mdash) +
                 0.01039*sin(2*F) +
                 0.00739*E*sin(Mdash - M) +
                -0.00514*E*sin(Mdash + M) +
                 0.00208*E2*sin(2*M) +
                -0.00111*sin(Mdash - 2*F) +
                -0.00057*sin(Mdash + 2*F) +
                 0.00056*E*sin(2*Mdash + M) +
                -0.00042*sin(3*Mdash) +
                 0.00042*E*sin(M + 2*F) +
                 0.00038*E*sin(M - 2*F) +
                -0.00024*E*sin(2*Mdash - M) +
                -0.00017*sin(omega) +
                -0.00007*sin(Mdash + 2*M) +
                 0.00004*sin(2*Mdash - 2*F) +
                 0.00004*sin(3*M) +
                 0.00003*sin(Mdash + M - 2*F) +
                 0.00003*sin(2*Mdash + 2*F) +
                -0.00003*sin(Mdash + M + 2*F) +
                 0.00003*sin(Mdash - M + 2*F) +
                -0.00002*sin(Mdash - M - 2*F) +
                -0.00002*sin(3*Mdash + M) +
                 0.00002*sin(4*Mdash);
    JDE := JDE + DeltaJDE;
  end
  else if ((kfrac = 0.25) or (kfrac = 0.75)) then //First Quarter or Last Quarter
  begin
    DeltaJDE := -0.62801*sin(Mdash) +
                 0.17172*E*sin(M) +
                -0.01183*E*sin(Mdash + M) +
                 0.00862*sin(2*Mdash) +
                 0.00804*sin(2*F) +
                 0.00454*E*sin(Mdash - M) +
                 0.00204*E2*sin(2*M) +
                -0.00180*sin(Mdash - 2*F) +
                -0.00070*sin(Mdash + 2*F) +
                -0.00040*sin(3*Mdash) +
                -0.00034*E*sin(2*Mdash - M) +
                 0.00032*E*sin(M + 2*F) +
                 0.00032*E*sin(M - 2*F) +
                -0.00028*E2*sin(Mdash + 2*M) +
                 0.00027*E*sin(2*Mdash + M) +
                -0.00017*sin(omega) +
                -0.00005*sin(Mdash - M - 2*F) +
                 0.00004*sin(2*Mdash + 2*F) +
                -0.00004*sin(Mdash + M + 2*F) +
                 0.00004*sin(Mdash - 2*M) +
                 0.00003*sin(Mdash + M - 2*F) +
                 0.00003*sin(3*M) +
                 0.00002*sin(2*Mdash - 2*F) +
                 0.00002*sin(Mdash - M + 2*F) +
                -0.00002*sin(3*Mdash + M);
    JDE := JDE + DeltaJDE;
          
    W := 0.00306 - 0.00038*E*cos(M) + 0.00026*cos(Mdash) - 0.00002*cos(Mdash - M) + 0.00002*cos(Mdash + M) + 0.00002*cos(2*F);
    if (kfrac = 0.25) then //First quarter
      JDE := JDE + W
    else
      JDE := JDE - W;
  end
  else if (kfrac = 0.5) then //Full Moon
  begin
    DeltaJDE := -0.40614*sin(Mdash) +
                 0.17302*E*sin(M) +
                 0.01614*sin(2*Mdash) +
                 0.01043*sin(2*F) +
                 0.00734*E*sin(Mdash - M) +
                -0.00514*E*sin(Mdash + M) +
                 0.00209*E2*sin(2*M) +
                -0.00111*sin(Mdash - 2*F) +
                -0.00057*sin(Mdash + 2*F) +
                 0.00056*E*sin(2*Mdash + M) +
                -0.00042*sin(3*Mdash) +
                 0.00042*E*sin(M + 2*F) +
                 0.00038*E*sin(M - 2*F) +
                -0.00024*E*sin(2*Mdash - M) +
                -0.00017*sin(omega) +
                -0.00007*sin(Mdash + 2*M) +
                 0.00004*sin(2*Mdash - 2*F) +
                 0.00004*sin(3*M) +
                 0.00003*sin(Mdash + M - 2*F) +
                 0.00003*sin(2*Mdash + 2*F) +
                -0.00003*sin(Mdash + M + 2*F) +
                 0.00003*sin(Mdash - M + 2*F) +
                -0.00002*sin(Mdash - M - 2*F) +
                -0.00002*sin(3*Mdash + M) +
                 0.00002*sin(4*Mdash);
    JDE := JDE + DeltaJDE;
  end
  else
    Assert(False);

  //Additional corrections for all phases
  DeltaJDE2 := 0.000325*sin(A1) +
               0.000165*sin(A2) +
               0.000164*sin(A3) +
               0.000126*sin(A4) +
               0.000110*sin(A5) +
               0.000062*sin(A6) +
               0.000060*sin(A7) +
               0.000056*sin(A8) +
               0.000047*sin(A9) +
               0.000042*sin(A10) +
               0.000040*sin(A11) +
               0.000037*sin(A12) +
               0.000035*sin(A13) +
               0.000023*sin(A14);
  JDE := JDE + DeltaJDE2;

  Result := JDE;
end;


end.
