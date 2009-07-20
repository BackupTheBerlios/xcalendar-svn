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

unit AANutation;
// This unit provides for calculation of Nutation and the Obliquity of the Ecliptic. This refers to Chapter 22 and parts of Chapter 23 in the book.

interface

function NutationInLongitude(const JDE: Extended): Extended;
function NutationInObliquity(const JDE: Extended): Extended;
function MeanObliquityOfEcliptic(const JDE: Extended): Extended;
function TrueObliquityOfEcliptic(const JDE: Extended): Extended;
function NutationInRightAscension(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;
function NutationInDeclination(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;


implementation

uses
  AACoordinateTransformation,
  Math;

type
  NutationCoefficient = record
    D: Integer;
    M: Integer;
    Mprime: Integer;
    F: Integer;
    omega: Integer;
    sincoeff1: Integer;
    sincoeff2: Extended;
    coscoeff1: Integer;
    coscoeff2: Extended;
  end;

const
  g_NutationCoefficients: array[0..62] of NutationCoefficient =
  (
    (D:  0; M:   0; Mprime:   0; F:   0; omega:   1; sincoeff1:-171996; sincoeff2: -174.2; coscoeff1: 92025; coscoeff2:    8.9    ),
    (D: -2; M:   0; Mprime:   0; F:   2; omega:   2; sincoeff1: -13187; sincoeff2:   -1.6; coscoeff1:  5736; coscoeff2:   -3.1    ),
    (D:  0; M:   0; Mprime:   0; F:   2; omega:   2; sincoeff1:  -2274; sincoeff2:   -0.2; coscoeff1:   977; coscoeff2:   -0.5    ),
    (D:  0; M:   0; Mprime:   0; F:   0; omega:   2; sincoeff1:   2062; sincoeff2:    0.2; coscoeff1:  -895; coscoeff2:    0.5    ),
    (D:  0; M:   1; Mprime:   0; F:   0; omega:   0; sincoeff1:   1426; sincoeff2:   -3.4; coscoeff1:    54; coscoeff2:   -0.1    ),
    (D:  0; M:   0; Mprime:   1; F:   0; omega:   0; sincoeff1:    712; sincoeff2:    0.1; coscoeff1:    -7; coscoeff2:      0    ),
    (D: -2; M:   1; Mprime:   0; F:   2; omega:   2; sincoeff1:   -517; sincoeff2:    1.2; coscoeff1:   224; coscoeff2:   -0.6    ),
    (D:  0; M:   0; Mprime:   0; F:   2; omega:   1; sincoeff1:   -386; sincoeff2:   -0.4; coscoeff1:   200; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   1; F:   2; omega:   2; sincoeff1:   -301; sincoeff2:      0; coscoeff1:   129; coscoeff2:   -0.1    ),
    (D: -2; M:  -1; Mprime:   0; F:   2; omega:   2; sincoeff1:    217; sincoeff2:   -0.5; coscoeff1:   -95; coscoeff2:    0.3    ),
    (D: -2; M:   0; Mprime:   1; F:   0; omega:   0; sincoeff1:   -158; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   0; F:   2; omega:   1; sincoeff1:    129; sincoeff2:    0.1; coscoeff1:   -70; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:  -1; F:   2; omega:   2; sincoeff1:    123; sincoeff2:      0; coscoeff1:   -53; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   0; F:   0; omega:   0; sincoeff1:     63; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   1; F:   0; omega:   1; sincoeff1:     63; sincoeff2:    0.1; coscoeff1:   -33; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:  -1; F:   2; omega:   2; sincoeff1:    -59; sincoeff2:      0; coscoeff1:    26; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:  -1; F:   0; omega:   1; sincoeff1:    -58; sincoeff2:   -0.1; coscoeff1:    32; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   1; F:   2; omega:   1; sincoeff1:    -51; sincoeff2:      0; coscoeff1:    27; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   2; F:   0; omega:   0; sincoeff1:     48; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:  -2; F:   2; omega:   1; sincoeff1:     46; sincoeff2:      0; coscoeff1:   -24; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   0; F:   2; omega:   2; sincoeff1:    -38; sincoeff2:      0; coscoeff1:    16; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   2; F:   2; omega:   2; sincoeff1:    -31; sincoeff2:      0; coscoeff1:    13; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   2; F:   0; omega:   0; sincoeff1:     29; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   1; F:   2; omega:   2; sincoeff1:     29; sincoeff2:      0; coscoeff1:   -12; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   0; F:   2; omega:   0; sincoeff1:     26; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   0; F:   2; omega:   0; sincoeff1:    -22; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:  -1; F:   2; omega:   1; sincoeff1:     21; sincoeff2:      0; coscoeff1:   -10; coscoeff2:      0    ),
    (D:  0; M:   2; Mprime:   0; F:   0; omega:   0; sincoeff1:     17; sincoeff2:   -0.1; coscoeff1:     0; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:  -1; F:   0; omega:   1; sincoeff1:     16; sincoeff2:      0; coscoeff1:    -8; coscoeff2:      0    ),
    (D: -2; M:   2; Mprime:   0; F:   2; omega:   2; sincoeff1:    -16; sincoeff2:    0.1; coscoeff1:     7; coscoeff2:      0    ),
    (D:  0; M:   1; Mprime:   0; F:   0; omega:   1; sincoeff1:    -15; sincoeff2:      0; coscoeff1:     9; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   1; F:   0; omega:   1; sincoeff1:    -13; sincoeff2:      0; coscoeff1:     7; coscoeff2:      0    ),
    (D:  0; M:  -1; Mprime:   0; F:   0; omega:   1; sincoeff1:    -12; sincoeff2:      0; coscoeff1:     6; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   2; F:  -2; omega:   0; sincoeff1:     11; sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:  -1; F:   2; omega:   1; sincoeff1:    -10; sincoeff2:      0; coscoeff1:     5; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   1; F:   2; omega:   2; sincoeff1:    -8;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  0; M:   1; Mprime:   0; F:   2; omega:   2; sincoeff1:     7;  sincoeff2:      0; coscoeff1:    -3; coscoeff2:      0    ),
    (D: -2; M:   1; Mprime:   1; F:   0; omega:   0; sincoeff1:    -7;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:  -1; Mprime:   0; F:   2; omega:   2; sincoeff1:    -7;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   0; F:   2; omega:   1; sincoeff1:    -7;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   1; F:   0; omega:   0; sincoeff1:     6;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   2; F:   2; omega:   2; sincoeff1:     6;  sincoeff2:      0; coscoeff1:    -3; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   1; F:   2; omega:   1; sincoeff1:     6;  sincoeff2:      0; coscoeff1:    -3; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:  -2; F:   0; omega:   1; sincoeff1:    -6;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  2; M:   0; Mprime:   0; F:   0; omega:   1; sincoeff1:    -6;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  0; M:  -1; Mprime:   1; F:   0; omega:   0; sincoeff1:     5;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:  -1; Mprime:   0; F:   2; omega:   1; sincoeff1:    -5;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   0; F:   0; omega:   1; sincoeff1:    -5;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   2; F:   2; omega:   1; sincoeff1:    -5;  sincoeff2:      0; coscoeff1:     3; coscoeff2:      0    ),
    (D: -2; M:   0; Mprime:   2; F:   0; omega:   1; sincoeff1:     4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   1; Mprime:   0; F:   2; omega:   1; sincoeff1:     4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   1; F:  -2; omega:   0; sincoeff1:     4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -1; M:   0; Mprime:   1; F:   0; omega:   0; sincoeff1:    -4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -2; M:   1; Mprime:   0; F:   0; omega:   0; sincoeff1:    -4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  1; M:   0; Mprime:   0; F:   0; omega:   0; sincoeff1:    -4;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   1; F:   2; omega:   0; sincoeff1:     3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:  -2; F:   2; omega:   2; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D: -1; M:  -1; Mprime:   1; F:   0; omega:   0; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   1; Mprime:   1; F:   0; omega:   0; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:  -1; Mprime:   1; F:   2; omega:   2; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  2; M:  -1; Mprime:  -1; F:   2; omega:   2; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  0; M:   0; Mprime:   3; F:   2; omega:   2; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    ),
    (D:  2; M:  -1; Mprime:   0; F:   2; omega:   2; sincoeff1:    -3;  sincoeff2:      0; coscoeff1:     0; coscoeff2:      0    )
  );                                                                                                         


{
function NutationInLongitude(const JDE: Extended): Extended;

Return Value
The nutation in ecliptic longitude in arc seconds of a degree.

Parameters
JDE The date in Dynamical time to calculate for.
}
function NutationInLongitude(const JDE: Extended): Extended;
var
  T, Tsquared, Tcubed: Extended;
  D, M, Mprime, F, omega: Extended;
  i: Integer;
  argument: Extended;
begin
  T := (JDE - 2451545) / 36525;
  Tsquared := T*T;
  Tcubed := Tsquared*T;

  D := 297.85036 + 445267.111480*T - 0.0019142*Tsquared + Tcubed / 189474;
  D := MapTo0To360Range(D);

  M := 357.52772 + 35999.050340*T - 0.0001603*Tsquared - Tcubed / 300000;
  M := MapTo0To360Range(M);

  Mprime := 134.96298 + 477198.867398*T + 0.0086972*Tsquared + Tcubed / 56250;
  Mprime := MapTo0To360Range(Mprime);

  F := 93.27191 + 483202.017538*T - 0.0036825*Tsquared + Tcubed / 327270;
  F := MapTo0To360Range(F);

  omega := 125.04452 - 1934.136261*T + 0.0020708*Tsquared + Tcubed / 450000;
  omega := MapTo0To360Range(omega);

  Result := 0;
  for i:=0 to 62 do
  begin
    argument := g_NutationCoefficients[i].D * D + g_NutationCoefficients[i].M * M +
                g_NutationCoefficients[i].Mprime * Mprime + g_NutationCoefficients[i].F * F +
                g_NutationCoefficients[i].omega * omega;
    argument := DegreesToRadians(argument);
    Result := Result + (g_NutationCoefficients[i].sincoeff1 + g_NutationCoefficients[i].sincoeff2 * T) * sin(argument) * 0.0001;
  end;
end;

{
function NutationInObliquity(const JDE: Extended): Extended;

Return Value
The nutation in obliquity in arc seconds of a degree.

Parameters
JDE The date in Dynamical time to calculate for.
}
function NutationInObliquity(const JDE: Extended): Extended;
var
  T, Tsquared, Tcubed: Extended;
  D, M, Mprime, F, omega: Extended;
  i: Integer;
  argument: Extended;
begin
  T := (JDE - 2451545) / 36525;
  Tsquared := T*T;
  Tcubed := Tsquared*T;

  D := 297.85036 + 445267.111480*T - 0.0019142*Tsquared + Tcubed / 189474;
  D := MapTo0To360Range(D);

  M := 357.52772 + 35999.050340*T - 0.0001603*Tsquared - Tcubed / 300000;
  M := MapTo0To360Range(M);

  Mprime := 134.96298 + 477198.867398*T + 0.0086972*Tsquared + Tcubed / 56250;
  Mprime := MapTo0To360Range(Mprime);

  F := 93.27191 + 483202.017538*T - 0.0036825*Tsquared + Tcubed / 327270;
  F := MapTo0To360Range(F);

  omega := 125.04452 - 1934.136261*T + 0.0020708*Tsquared + Tcubed / 450000;
  omega := MapTo0To360Range(omega);

  Result := 0;
  for i:=0 to 62 do
  begin
    argument := g_NutationCoefficients[i].D * D + g_NutationCoefficients[i].M * M +
                g_NutationCoefficients[i].Mprime * Mprime + g_NutationCoefficients[i].F * F +
                g_NutationCoefficients[i].omega * omega;
    argument := DegreesToRadians(argument);
    Result := Result + (g_NutationCoefficients[i].coscoeff1 + g_NutationCoefficients[i].coscoeff2 * T) * cos(argument) * 0.0001;
  end;
end;

{
function MeanObliquityOfEcliptic(const JDE: Extended): Extended;

Return Value
The mean obliquity of the ecliptic in degrees.

Parameters
JDE The date in Dynamical time to calculate for.
}
function MeanObliquityOfEcliptic(const JDE: Extended): Extended;
var
  U, Usquared, Ucubed, U4, U5, U6, U7, U8, U9, U10: Extended;
begin
  U := (JDE - 2451545) / 3652500;
  Usquared := U*U;
  Ucubed := Usquared*U;
  U4 := Ucubed*U;
  U5 := U4*U;
  U6:= U5*U;
  U7 := U6*U;
  U8 := U7*U;
  U9 := U8*U;
  U10 := U9*U;

  Result := DMSToDegrees(23, 26, 21.448) - DMSToDegrees(0, 0, 4680.93) * U
                                         - DMSToDegrees(0, 0, 1.55) * Usquared
                                         + DMSToDegrees(0, 0, 1999.25) * Ucubed
                                         - DMSToDegrees(0, 0, 51.38) * U4
                                         - DMSToDegrees(0, 0, 249.67) * U5
                                         - DMSToDegrees(0, 0, 39.05) * U6 
                                         + DMSToDegrees(0, 0, 7.12) * U7
                                         + DMSToDegrees(0, 0, 27.87) * U8
                                         + DMSToDegrees(0, 0, 5.79) * U9
                                         + DMSToDegrees(0, 0, 2.45) * U10;
end;

{
function TrueObliquityOfEcliptic(const JDE: Extended): Extended;

Return Value
The true obliquity of the ecliptic in degrees.

Parameters
JDE The date in Dynamical time to calculate for.
}
function TrueObliquityOfEcliptic(const JDE: Extended): Extended;
begin
  Result := MeanObliquityOfEcliptic(JDE) + DMSToDegrees(0, 0, NutationInObliquity(JDE));
end;

{
function NutationInRightAscension(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;

Return Value
The nutation in right ascension in arc seconds of a degree.

Parameters
Alpha The right ascension of the position in hour angles.
Delta The declination of the position in degrees.
Obliquity The obliquity of the Ecliptic in degrees.
NutationInLongitude The nutation in longitude in arc seconds of a degree.
NutationInObliquity The nutation in obliquity in arc seconds of a degree.

Remarks
This refers to algorithm 23.1 on page 151.
}
function NutationInRightAscension(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;
begin
  //Convert to radians
  Alpha := HoursToRadians(Alpha);
  Delta := DegreesToRadians(Delta);
  Obliquity := DegreesToRadians(Obliquity);

  Result := (cos(Obliquity) + sin(Obliquity) * sin(Alpha) * tan(Delta)) * NutationInLongitude - cos(Alpha)*tan(Delta)*NutationInObliquity; 
end;

{
function NutationInDeclination(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;

Return Value
The nutation in declination in arc seconds of a degree.

Parameters
Alpha The right ascension of the position in hour angles.
Delta The declination of the position in degrees.
Obliquity The obliquity of the Ecliptic in degrees.
NutationInLongitude The nutation in longitude in arc seconds of a degree.
NutationInObliquity The nutation in obliquity in arc seconds of a degree.

Remarks
This refers to algorithm 23.1 on page 151.
}
function NutationInDeclination(Alpha, Delta, Obliquity, NutationInLongitude, NutationInObliquity: Extended): Extended;
begin
  //Convert to radians
  Alpha := HoursToRadians(Alpha);
//  Delta := DegreesToRadians(Delta);
  Obliquity := DegreesToRadians(Obliquity);

  Result := sin(Obliquity) * cos(Alpha) * NutationInLongitude + sin(Alpha)*NutationInObliquity;
end;


end.
