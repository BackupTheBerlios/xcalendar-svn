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

unit AACoordinateTransformation;
// This unit provides for the transformations of the coordinates as well as helper angle methods. This refers to Chapter 13 and parts of Chapter 1 in the book.

interface

uses
  AACoordinates;


function DegreesToRadians(const Degrees: Extended): Extended;
function RadiansToDegrees(const Radians: Extended): Extended;
function RadiansToHours(const Radians: Extended): Extended;
function HoursToRadians(const Hours: Extended): Extended;
function HoursToDegrees(const Hours: Extended): Extended;
function DegreesToHours(const Degrees: Extended): Extended;
function PI: Extended;
function MapTo0To360Range(const Degrees: Extended): Extended;
function MapTo0To24Range(const HourAngle: Extended): Extended;
function Equatorial2Ecliptic(Alpha, Delta, Epsilon: Extended): TAA2DCoordinate;
function Ecliptic2Equatorial(Lambda, Beta, Epsilon: Extended): TAA2DCoordinate;
function Equatorial2Horizontal(LocalHourAngle, Delta, Latitude: Extended): TAA2DCoordinate;
function Horizontal2Equatorial(Azimuth, Altitude, Latitude: Extended): TAA2DCoordinate;
function Equatorial2Galactic(Alpha, Delta: Extended): TAA2DCoordinate;
function Galactic2Equatorial(l, b: Extended): TAA2DCoordinate;
function DMSToDegrees(const Degrees, Minutes, Seconds: Extended; bPositive: Boolean = True): Extended;


implementation

uses
  Math;

{
function DegreesToRadians(const Degrees: Extended): Extended;

Return Value
Returns the value in radians which was converted from degrees.

Parameters
Degrees The angular value to convert measured in degrees.
}
function DegreesToRadians(const Degrees: Extended): Extended;
begin
  Result := Degrees * 0.017453292519943295769236907684886;
end;

{
function RadiansToDegrees(const Radians: Extended): Extended;

Return Value
Returns the value in degrees which was converted from radians.

Parameters
Radians The angular value to convert measured in radians.
}
function RadiansToDegrees(const Radians: Extended): Extended;
begin
  Result := Radians * 57.295779513082320876798154814105;
end;

{
function RadiansToHours(const Radians: Extended): Extended;

Return Value
Returns the value expressed as an hour angle which was converted from radians.

Parameters
Radians The angular value to convert measured in radians.
}
function RadiansToHours(const Radians: Extended): Extended;
begin
  Result := Radians * 3.8197186342054880584532103209403;
end;

{
function HoursToRadians(const Hours: Extended): Extended;

Return Value
Returns the value in radians which was converted from hours.

Parameters
Hours The numeric value to convert measured in hours.
}
function HoursToRadians(const Hours: Extended): Extended;
begin
  Result := Hours * 0.26179938779914943653855361527329;
end;

{
function HoursToDegrees(const Hours: Extended): Extended;

Return Value
Returns the value in degrees which was converted from hours.

Parameters
Hours The numeric value to convert measured in hours.
}
function HoursToDegrees(const Hours: Extended): Extended;
begin
  Result := Hours * 15;
end;

{
function DegreesToHours(const Degrees: Extended): Extended;

Return Value
Returns the value in hours which was converted from degrees.

Parameters
Degrees The angular value to convert measured in degrees.
}
function DegreesToHours(const Degrees: Extended): Extended;
begin
  Result := Degrees / 15;
end;

{
function PI: Extended;

Return Value
Returns the constant value of pi i.e 3.1415926...
}
function PI: Extended;
begin
  Result := 3.1415926535897932384626433832795;
end;

{
function MapTo0To360Range(const Degrees: Extended): Extended;

Return Value
Maps an arbitrary angular value to the range 0 to 360. i.e. inputting the value -2 will return a value of 258.

Parameters
Degrees The angular value.
}
function MapTo0To360Range(const Degrees: Extended): Extended;
var
  Value: Extended;
begin
  Value := Degrees;

  //map it to the range 0 - 360
  while (Value < 0) do
    Value := Value + 360;
  while (Value > 360) do
    Value := Value - 360;

  Result := Value;
end;

{
function MapTo0To24Range(const HourAngle: Extended): Extended;

Return Value
Maps an arbitrary value to the range 0 to 24. i.e. inputting the value -2  will return a value of 22.

Parameters
HourAngle The hour angle.
}
function MapTo0To24Range(const HourAngle: Extended): Extended;
var
  Value: Extended;
begin
  Value := HourAngle;

  //map it to the range 0 - 24
  while (Value < 0) do
    Value := Value + 24;
  while (Value > 24) do
    Value := Value - 24;

  Result := Value;
end;

{
function Equatorial2Ecliptic(Alpha, Delta, Epsilon: Extended): TAA2DCoordinate;

Return Value
Returns the converted ecliptic coordinates in a TAA2DCoordinate record. The x value in the class corresponds to the ecliptic longitude in degrees and the y value corresponds to the ecliptic latitude in degrees.

Parameters
Alpha The right ascension expressed as an hour angle.
Delta The declination in degrees.
Epsilon The obliquity of the ecliptic in degrees.

Remarks
The transformation of coordinates from Equatorial to Ecliptic. This refers to algorithm 13.1 and 13.2 on page 93.
}
function Equatorial2Ecliptic(Alpha, Delta, Epsilon: Extended): TAA2DCoordinate;
begin
  Alpha := HoursToRadians(Alpha);
  Delta := DegreesToRadians(Delta);
  Epsilon := DegreesToRadians(Epsilon);

  Result.X := RadiansToDegrees(ArcTan2(sin(Alpha)*cos(Epsilon) + tan(Delta)*sin(Epsilon), cos(Alpha)));
  if (Result.X < 0) then
    Result.X := Result.X + 360;
  Result.Y := RadiansToDegrees(ArcSin(sin(Delta)*cos(Epsilon) - cos(Delta)*sin(Epsilon)*sin(Alpha)));
end;

{
function Ecliptic2Equatorial(Lambda, Beta, Epsilon: Extended): TAA2DCoordinate;

Return Value
Returns the converted equatorial coordinates in a TAA2DCoordinate record. The x value in the record corresponds to the equatorial longitude in hours and the y value corresponds to the equatorial latitude in degrees.

Parameters
Lambda The ecliptic longitude in degrees.
Beta The ecliptic latitude in degrees.
Epsilon the obliquity of the ecliptic in degrees.

Remarks
The transformation of coordinates from Ecliptic to Equatorial. This refers to algorithm 13.3 and 13.4 on page 93.
}
function Ecliptic2Equatorial(Lambda, Beta, Epsilon: Extended): TAA2DCoordinate;
begin
  Lambda := DegreesToRadians(Lambda);
  Beta := DegreesToRadians(Beta);
  Epsilon := DegreesToRadians(Epsilon);

  Result.X := RadiansToHours(ArcTan2(sin(Lambda)*cos(Epsilon) - tan(Beta)*sin(Epsilon), cos(Lambda)));
  if (Result.X < 0) then
    Result.X := Result.X + 24;
  Result.Y := RadiansToDegrees(ArcSin(sin(Beta)*cos(Epsilon) + cos(Beta)*sin(Epsilon)*sin(Lambda)));
end;

{
function Equatorial2Horizontal(LocalHourAngle, Delta, Latitude: Extended): TAA2DCoordinate;

Return Value
Returns the converted horizontal coordinates in a TAA2DCoordinate record. The x value in the record corresponds to the azimuth in degrees and the y value corresponds to the altitude in degrees.

Parameters
LocalHourAngle The local hour angle, measured westwards from the South.
Delta The declination in degrees.
Latitude The standard latitude of the position in degrees.

Remarks
The transformation of coordinates from Equatorial to Horizontal. This refers to algorithm 13.5 and 13.6 on page 93.
}
function Equatorial2Horizontal(LocalHourAngle, Delta, Latitude: Extended): TAA2DCoordinate;
begin
  LocalHourAngle := HoursToRadians(LocalHourAngle);
  Delta := DegreesToRadians(Delta);
  Latitude := DegreesToRadians(Latitude);

  Result.X := RadiansToDegrees(ArcTan2(sin(LocalHourAngle), cos(LocalHourAngle)*sin(Latitude) - tan(Delta)*cos(Latitude)));
  if (Result.X < 0) then
    Result.X := Result.X + 360;
  Result.Y := RadiansToDegrees(ArcSin(sin(Latitude)*sin(Delta) + cos(Latitude)*cos(Delta)*cos(LocalHourAngle)));
end;

{
function Horizontal2Equatorial(Azimuth, Altitude, Latitude: Extended): TAA2DCoordinate;

Return Value
Returns the converted equatorial coordinates in a TAA2DCoordinate record. The x value in the record corresponds to the equatorial longitude in hours and the y value corresponds to the equatorial latitude in degrees.

Parameters
A The azimuth in degrees.
h The altitude in degrees
Latitude The standard latitude of the position in degrees.

Remarks
The transformation of coordinates from Horizontal to Equatorial. This refers to the two algorithms on the top of page 94.
}
function Horizontal2Equatorial(Azimuth, Altitude, Latitude: Extended): TAA2DCoordinate;
begin
  Azimuth := DegreesToRadians(Azimuth);
  Altitude := DegreesToRadians(Altitude);
  Latitude := DegreesToRadians(Latitude);

  Result.X := RadiansToHours(ArcTan2(sin(Azimuth), cos(Azimuth)*sin(Latitude) + tan(Altitude)*cos(Latitude)));
  if (Result.X < 0) then
    Result.X := Result.X + 24;
  Result.Y := RadiansToDegrees(ArcSin(sin(Latitude)*sin(Altitude) - cos(Latitude)*cos(Altitude)*cos(Azimuth)));
end;

{
function Equatorial2Galactic(Alpha, Delta: Extended): TAA2DCoordinate;

Return Value
Returns the converted galactic coordinates in a TAA2DCoordinate record. The x value in the record corresponds to the galactic longitude in degrees and the y value corresponds to the galactic latitude in degrees.

Parameters
Alpha The right ascension expressed as an hour angle.
Delta The declination in degrees.

Remarks
The transformation of coordinates from Equatorial to Galactic. This refers to algorithm 13.7 and 13.8 on page 94.
}
function Equatorial2Galactic(Alpha, Delta: Extended): TAA2DCoordinate;
begin
  Alpha := 192.25 - HoursToDegrees(Alpha);
  Alpha := DegreesToRadians(Alpha);
  Delta := DegreesToRadians(Delta);

  Result.X := RadiansToDegrees(ArcTan2(sin(Alpha), cos(Alpha)*sin(DegreesToRadians(27.4)) - tan(Delta)*cos(DegreesToRadians(27.4))));
  Result.X := 303 - Result.X;
  if (Result.X >= 360) then
    Result.X := Result.X - 360;
  Result.Y := RadiansToDegrees(ArcSin(sin(Delta)*sin(DegreesToRadians(27.4)) + cos(Delta)*cos(DegreesToRadians(27.4))*cos(Alpha)));
end;

{
function Galactic2Equatorial(l, b: Extended): TAA2DCoordinate;

Return Value
Returns the converted equatorial coordinates in a TAA2DCoordinate record. The x value in the record corresponds to the equatorial longitude in hours and the y value corresponds to the equatorial latitude in degrees.

Parameters
l  The galactic longitude expressed in degrees.
b The galactic latitude expressed in degrees.

Remarks
The transformation of coordinates from Galactic to Equatorial. This refers to the last two algorithms on page 94.
}
function Galactic2Equatorial(l, b: Extended): TAA2DCoordinate;
begin
  l := l - 123;
  l := DegreesToRadians(l);
  b := DegreesToRadians(b);

  Result.X := RadiansToDegrees(ArcTan2(sin(l), cos(l)*sin(DegreesToRadians(27.4)) - tan(b)*cos(DegreesToRadians(27.4))));
  Result.X := Result.X + 12.25;
  if (Result.X < 0) then
    Result.X := Result.X + 360;
  Result.X := DegreesToHours(Result.X);
  Result.Y := RadiansToDegrees(ArcSin(sin(b)*sin(DegreesToRadians(27.4)) + cos(b)*cos(DegreesToRadians(27.4))*cos(l)));
end;

{
function DMSToDegrees(const Degrees, Minutes, Seconds: Extended; bPositive: Boolean = True): Extended;

Return Value
Returns the value in degrees which was converted from degrees, minutes and seconds.

Parameters
Degrees  The degree part of the angular value to convert.
Minutes The minute part of the angular value to convert.
Seconds The second part of the angular value to convert.
bPositive true if the input value corresponds to a non-negative value with false implying the value is positive

Remarks
To convert the angle 21° 44' 07" you would use DMSToDegrees(21, 44, 7, true).
To convert the angle -12° 47' 22" you would use DMSToDegrees(12, 47, 22, false) or DMSToDegrees(-12, -47, -22, true).
To convert the angle -0° 32' 41" you must use DMSToDegrees(0, 32, 41, false).
}
function DMSToDegrees(const Degrees, Minutes, Seconds: Extended; bPositive: Boolean): Extended;
begin
  //validate our parameters
  if (not bPositive) then
  begin
    Assert(Degrees >= 0);  //All parameters should be non negative if the "bPositive" parameter is False
    Assert(Minutes >= 0);
    Assert(Seconds >= 0);
  end;

  Result := Degrees + Minutes/60 + Seconds/3600;
  if (not bPositive) then
    Result := -Result;
end;

end.
