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

unit AAEquationOfTime;
// This unit provides for calculation of the Equation of Time. This refers to Chapter 28 in the book.

interface

function EquationOfTime(const JDE: Extended): Extended;


implementation

uses
  AACoordinates,
  AACoordinateTransformation,
  AASun,
  AANutation,
  Math;


{
function EquationOfTime(const JDE: Extended): Extended;

Return Value
the equation of time in decimal minutes.

Parameters
JDE The date in Dynamical time to calculate for.
}
function EquationOfTime(const JDE: Extended): Extended;
var
  rho, rhosquared, rhocubed, rho4, rho5: Extended;
  L0, SunLong, SunLat, epsilon: Extended;
  Equatorial: TAA2DCoordinate;
begin
  rho := (JDE - 2451545) / 365250;
  rhosquared := rho*rho;
  rhocubed := rhosquared*rho;
  rho4 := rhocubed*rho;
  rho5 := rho4*rho;

  //Calculate the Suns mean longitude
  L0 := MapTo0To360Range(280.4664567 + 360007.6982779*rho + 0.03032028*rhosquared +
                         rhocubed / 49931 - rho4 / 15300 - rho5 / 2000000);

  //Calculate the Suns apparent right ascension
  SunLong := Sun_ApparentEclipticLongitude(JDE);
  SunLat := Sun_ApparentEclipticLatitude(JDE);
  epsilon := TrueObliquityOfEcliptic(JDE);
  Equatorial := Ecliptic2Equatorial(SunLong, SunLat, epsilon);

  epsilon := DegreesToRadians(epsilon);
  Result := L0 - 0.0057183 - Equatorial.X*15 + DMSToDegrees(0, 0, NutationInLongitude(JDE))*cos(epsilon);
  if (Result > 180) then
    Result := -(360 - Result);
  Result := Result * 4; //Convert to minutes of time
end;


end.
