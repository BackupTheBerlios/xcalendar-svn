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

unit AARiseTransitSet;
// This unit provides for calculation of the time of rise, transit and set of a celestial body. This refers to Chapter 15 in the book.

interface

type
  TAARiseTransitSetDetails = record
    bValid: Boolean;
    Rise: Extended;
    Transit: Extended;
    _Set: Extended;
  end;

function RiseTransitSet_Calculate(const JDE, Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3,
  Longitude, Latitude, h0: Extended): TAARiseTransitSetDetails;


implementation

uses
  AASidereal,
  AACoordinateTransformation,
  AADynamicalTime,
  AAInterpolate,
  AACoordinates,
  Math;


{
function RiseTransitSet_Calculate(const JDE, Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3,
  Longitude, Latitude, h0: Extended): TAARiseTransitSetDetails;

Return Value
A record containing
bValid true if the object actually rises , transits and sets for the specified date. If false, this would imply that either the object is circumpolar or never rises high enough above the horizon.
Rise The time in decimal hours when the object rises
Transit The time in hours when the object transits
_Set The time in hours when the object sets

Parameters
JDE The Julian Day corresponding to that midnight Dynamical Time for the date when you want to perform the calculation.
Alpha1 The right ascension in hours of the object at time JDE - 1 day
Delta1 The declination in degrees of the object at time JDE - 1 day
Alpha2 The right ascension in hours of the object at time JDE
Delta2 The declination in degrees of the object at time JDE
Alpha3 The right ascension in hours of the object at time JDE + 1 day
Delta3 The declination in degrees of the object at time JDE + 1 day
Longitude The geographic longitude of the observer in degrees.
Latitude The geographic latitude of the observer in degrees.
h0 The "standard" altitude in degrees i.e. the geometric altitude of the centre of the body at the time of the apparent rising or setting.
}
function RiseTransitSet_Calculate(const JDE, Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3,
  Longitude, Latitude, h0: Extended): TAARiseTransitSetDetails;
var
  theta0, deltaT, Delta2Rad, LatitudeRad, h0Rad, cosH0, _H0,
  M0, M1, M2, theta1, n, Alpha, Delta, H, DeltaM: Extended;
  Horizontal: TAA2DCoordinate;
  i: Integer;
begin
  Result.bValid := false;

  //Calculate the sidereal time
  theta0 := ApparentGreenwichSiderealTime(JDE);
  theta0 := theta0 * 15; //Express it as degrees

  //Calculate deltat
  deltaT := DynamicalTime_DeltaT(JDE);

  //Convert values to radians
  Delta2Rad := DegreesToRadians(Delta2);
  LatitudeRad := DegreesToRadians(Latitude);

  //Convert the standard latitude to radians
  h0Rad := DegreesToRadians(h0);

  cosH0 := (sin(h0Rad) - sin(LatitudeRad)*sin(Delta2Rad)) / (cos(LatitudeRad) * cos(Delta2Rad));

  //Check that the object actually rises
  if ((cosH0 > 1) or (cosH0 < -1)) then
    Exit;

  _H0 := ArcCos(cosH0);
  _H0 := RadiansToDegrees(_H0);

  M0 := (Alpha2*15 + Longitude - theta0) / 360; 
  M1 := M0 - _H0/360;
  M2 := M0 + _H0/360;

  if (M0 > 1) then
    M0 := M0 - 1
  else if (M0 < 0) then
    M0 := M0 + 1;

  if (M1 > 1) then
    M1 := M1 - 1
  else if (M1 < 0) then
    M1 := M1 + 1;

  if (M2 > 1) then
    M2 := M2 - 1
  else if (M2 < 0) then
    M2 := M2 + 1;

  for i:=0 to 1 do
  begin
    //Calculate the details of rising

    theta1 := theta0 + 360.985647*M1;
    theta1 := MapTo0To360Range(theta1);

    n := M1 + deltaT/86400;

    Alpha := Interpolate(n, Alpha1, Alpha2, Alpha3);
    Delta := Interpolate(n, Delta1, Delta2, Delta3);

    H := theta1 - Longitude - Alpha*15;
    Horizontal := Equatorial2Horizontal(H/15, Delta, Latitude);

    DeltaM := (Horizontal.Y - h0) / (360*cos(DegreesToRadians(Delta))*cos(LatitudeRad)*sin(DegreesToRadians(H)));
    M1 := M1 + DeltaM;


    //Calculate the details of transit

    theta1 := theta0 + 360.985647*M0;
    theta1 := MapTo0To360Range(theta1);

    n := M0 + deltaT/86400;

    Alpha := Interpolate(n, Alpha1, Alpha2, Alpha3);

    H := theta1 - Longitude - Alpha*15;

    DeltaM := -H / 360;
    M0 := M0 + DeltaM;


    //Calculate the details of setting

    theta1 := theta0 + 360.985647*M2;
    theta1 := MapTo0To360Range(theta1);

    n := M2 + deltaT/86400;

    Alpha := Interpolate(n, Alpha1, Alpha2, Alpha3);
    Delta := Interpolate(n, Delta1, Delta2, Delta3);

    H := theta1 - Longitude - Alpha*15;
    Horizontal := Equatorial2Horizontal(H/15, Delta, Latitude);

    DeltaM := (Horizontal.Y - h0) / (360*cos(DegreesToRadians(Delta))*cos(LatitudeRad)*sin(DegreesToRadians(H)));
    M2 := M2 + DeltaM;
  end;

  //Finally before we exit, convert to hours
  Result.bValid := true;
  Result.Rise := M1 * 24;
  Result._Set := M2 * 24;
  Result.Transit := M0 * 24;
end;



end.
