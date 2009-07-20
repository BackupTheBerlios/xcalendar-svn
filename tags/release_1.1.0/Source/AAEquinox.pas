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

unit AAEquinox;
// This unit provides the algorithms to calculate the dates of the Equinoxes and Solstices. This refers to Chapter 27 in the book.

interface

function SpringEquinox(Year: Integer): Extended;
function SummerSolstice(Year: Integer): Extended;
function AutumnEquinox(Year: Integer): Extended;
function WinterSolstice(Year: Integer): Extended;

implementation

uses
  AACoordinateTransformation,
  AASun;

  
{
function SpringEquinox(Year: Integer): Extended;

Return Value
The date in Dynamical time when the Spring Equinox occurs.

Parameters
Year The year to calculate the occurrence for.
}
function SpringEquinox(Year: Integer): Extended;
var
  Y, Y2, Y3, Y4, Correction, SunLongitude: Extended;
begin
  //calculate the approximate date
  if (Year <= 1000) then
  begin
    Y := Year / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 1721139.29189 + 365242.13740*Y + 0.06134*Y2 + 0.00111*Y3 - 0.00071*Y4;
  end
  else
  begin
    Y := (Year - 2000) / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 2451623.80984 + 365242.37404*Y + 0.05169*Y2 - 0.00411*Y3 - 0.00057*Y4;
  end;

  repeat
    SunLongitude := Sun_ApparentEclipticLongitude(Result);
    Correction := 58 * sin(DegreesToRadians(-SunLongitude));
    Result := Result + Correction;
  until (Abs(Correction) < 0.00001); //Corresponds to an error of 0.86 of a second
end;

{
function SummerSolstice(Year: Integer): Extended;

Return Value
The date in Dynamical time when the Summer Solstice occurs.

Parameters
Year The year to calculate the occurrence for.
}
function SummerSolstice(Year: Integer): Extended;
var
  Y, Y2, Y3, Y4, Correction, SunLongitude: Extended;
begin
  //calculate the approximate date
  if (Year <= 1000) then
  begin
    Y := Year / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 1721233.25401 + 365241.72562*Y - 0.05323*Y2 + 0.00907*Y3 + 0.00025*Y4;
  end
  else
  begin
    Y := (Year - 2000) / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 2451716.56767 + 365241.62603*Y + 0.00325*Y2 + 0.00888*Y3 - 0.00030*Y4;
  end;

  repeat
    SunLongitude := Sun_ApparentEclipticLongitude(Result);
    Correction := 58 * sin(DegreesToRadians(90 - SunLongitude));
    Result := Result + Correction;
  until (Abs(Correction) < 0.00001); //Corresponds to an error of 0.86 of a second
end;

{
function AutumnEquinox(Year: Integer): Extended;

Return Value
The date in Dynamical time when the Autumn Equinox occurs.

Parameters
Year The year to calculate the occurrence for.
}
function AutumnEquinox(Year: Integer): Extended;
var
  Y, Y2, Y3, Y4, Correction, SunLongitude: Extended;
begin
  //calculate the approximate date
  if (Year <= 1000) then
  begin
    Y := Year / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 1721325.70455 + 365242.49558*Y - 0.11677*Y2 - 0.00297*Y3 + 0.00074*Y4;
  end
  else
  begin
    Y := (Year - 2000) / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 2451810.21715 + 365242.01767*Y - 0.11575*Y2 + 0.00337*Y3 + 0.00078*Y4;
  end;

  repeat
    SunLongitude := Sun_ApparentEclipticLongitude(Result);
    Correction := 58 * sin(DegreesToRadians(180 - SunLongitude));
    Result := Result + Correction;
  until (Abs(Correction) < 0.00001); //Corresponds to an error of 0.86 of a second
end;

{
function WinterSolstice(Year: Integer): Extended;

Return Value
The date in Dynamical time when the Winter Solstice occurs.

Parameters
Year The year to calculate the occurrence for.
}
function WinterSolstice(Year: Integer): Extended;
var
  Y, Y2, Y3, Y4, Correction, SunLongitude: Extended;
begin
  //calculate the approximate date
  if (Year <= 1000) then
  begin
    Y := Year / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 1721414.39987 + 365242.88257*Y - 0.00769*Y2 - 0.00933*Y3 - 0.00006*Y4;
  end
  else
  begin
    Y := (Year - 2000) / 1000.0;
    Y2 := Y * Y;
    Y3 := Y2 * Y;
    Y4 := Y3 * Y;
    Result := 2451900.05952 + 365242.74049*Y - 0.06223*Y2 - 0.00823*Y3 + 0.00032*Y4;
  end;

  repeat
    SunLongitude := Sun_ApparentEclipticLongitude(Result);
    Correction := 58 * sin(DegreesToRadians(270 - SunLongitude));
    Result := Result + Correction;
  until (Abs(Correction) < 0.00001); //Corresponds to an error of 0.86 of a second
end;

end.
