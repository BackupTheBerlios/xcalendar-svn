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

unit AASidereal;
// This unit provides for the calculation of sidereal time. This refers to Chapter 12 in the book.

interface

function MeanGreenwichSiderealTime(const JDE: Extended): Extended;
function ApparentGreenwichSiderealTime(const JDE: Extended): Extended;


implementation

uses
  AACoordinateTransformation,
  AANutation,
  Math;


{
function MeanGreenwichSiderealTime(const JDE: Extended): Extended;

Return Value
The Mean Greenwich Sidereal Time, that is, the Greenwich hour angle of the mean vernal point (the intersection of the ecliptic of the date with the mean equator of the date), expressed in hours.

Parameters
JDE The Julian Day in Universal time to calculate for.
}
function MeanGreenwichSiderealTime(const JDE: Extended): Extended;
var
  JDEMidnight, F: Extended;
  Hour, Minute: Integer;
  Second: Extended;
  T, TSquared, TCubed, Value: Extended;
begin
  //Get the Julian day for the same day at midnight
  JDEMidnight := Floor(JDE - 0.5) + 0.5;
  F := Frac(JDE) - 0.5;
  while F < 0 do
    F := F + 1;
  Hour := Trunc(F*24);
  Minute := Trunc((F - (Hour)/24.0)*1440.0);
  Second := (F - (Hour / 24.0) - (Minute / 1440.0)) * 86400.0;

  //Calculate the sidereal time at midnight
  T := (JDEMidnight - 2451545) / 36525;
  TSquared := T*T;
  TCubed := TSquared*T;
  Value := 100.46061837 + (36000.770053608*T) + (0.000387933*TSquared) - (TCubed/38710000);

  //Adjust by the time of day
  Value := Value + ( ((Hour * 15) + (Minute * 0.25) + (Second * 0.0041666666666666666666666666666667)) * 1.00273790935);

  Value := DegreesToHours(Value);

  Result := MapTo0To24Range(Value);
end;

{
function ApparentGreenwichSiderealTime(const JDE: Extended): Extended;

Return Value
The Apparent Greenwich Sidereal Time, that is, the Greenwich hour angle of the true vernal equinox, expressed in hours.

Parameters
JDE The Julian Day in Universal time to calculate for.
}
function ApparentGreenwichSiderealTime(const JDE: Extended): Extended;
var
  MeanObliquity, TrueObliquity, _NutationInLongitude: Extended;
begin
  MeanObliquity := MeanObliquityOfEcliptic(JDE);
  TrueObliquity := MeanObliquity + NutationInObliquity(JDE) / 3600;
  _NutationInLongitude := NutationInLongitude(JDE);

  Result := MeanGreenwichSiderealTime(JDE) + (_NutationInLongitude  * cos(DegreesToRadians(TrueObliquity)) / 54000);
  Result := MapTo0To24Range(Result);
end;


end.
