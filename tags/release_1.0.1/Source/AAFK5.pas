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

unit AAFK5;
// This unit provides the algorithms to convert to the FK5 standard reference frame. This refers to parts of Chapter 26 and 32 in the book.

interface

uses
  AACoordinates;


function FK5_CorrectionInLongitude(Longitude, Latitude, JDE: Extended): Extended;
function FK5_CorrectionInLatitude(Longitude, JDE: Extended): Extended;
function FK5_ConvertVSOPToFK5J2000(const value: TAA3DCoordinate): TAA3DCoordinate;
function FK5_ConvertVSOPToFK5B1950(const value: TAA3DCoordinate): TAA3DCoordinate;
function FK5_ConvertVSOPToFK5AnyEquinox(const value: TAA3DCoordinate; const JDEEquinox: Extended): TAA3DCoordinate;


implementation

uses
  AACoordinateTransformation,
  AAEarth,
  Math;


{
function FK5_CorrectionInLongitude(Longitude, Latitude, JDE: Extended): Extended;

Return Value
The correction in degrees to convert a VSOP heliocentric longitude to the FK5 reference frame.

Parameters
Longitude The VSOP heliocentric longitude in degrees.
Latitude The VSOP heliocentric latitude in degrees.
JDE The date in Dynamical time to calculate for.
}
function FK5_CorrectionInLongitude(Longitude, Latitude, JDE: Extended): Extended;
var
  T, Ldash: Extended;
begin
  T := (JDE - 2451545) / 36525;
  Ldash := Longitude - 1.397*T - 0.00031*T*T;

  //Convert to radians
  Ldash := DegreesToRadians(Ldash);
  Latitude := DegreesToRadians(Latitude);

  Result := -0.09033 + 0.03916*(cos(Ldash) + sin(Ldash))*tan(Latitude);
  Result := DMSToDegrees(0, 0, Result);
end;

{
function FK5_CorrectionInLatitude(Longitude, JDE: Extended): Extended;

Return Value
The correction in degrees to convert a VSOP heliocentric latitude to the FK5 reference frame.

Parameters
Longitude The VSOP heliocentric longitude in degrees.
JDE The date in Dynamical time to calculate for.
}
function FK5_CorrectionInLatitude(Longitude, JDE: Extended): Extended;
var
  T, Ldash: Extended;
begin
  T := (JDE - 2451545) / 36525;
  Ldash := Longitude - 1.397*T - 0.00031*T*T;

  //Convert to radians
  Ldash := DegreesToRadians(Ldash);

  Result := 0.03916*(cos(Ldash) - sin(Ldash));
  Result := DMSToDegrees(0, 0, Result);
end;

{
function FK5_ConvertVSOPToFK5J2000(const value: TAA3DCoordinate): TAA3DCoordinate;

Return Value
A record containing the converted equatorial FK5 J2000 reference frame coordinates.

Parameters
value The geometric rectangular ecliptical coordinates of the object (e.g. the Sun) to convert from the dynamical reference frame (VSOP) to the equatorial FK5 J2000 reference frame.
}
function FK5_ConvertVSOPToFK5J2000(const value: TAA3DCoordinate): TAA3DCoordinate;
begin
  Result.X := value.X + 0.000000440360 * value.Y - 0.000000190919 * value.Z;
  Result.Y := -0.000000479966 * value.X + 0.917482137087 * value.Y - 0.397776982902 * value.Z;
  Result.Z := 0.397776982902 * value.Y + 0.917482137087 * value.Z;
end;

{
function FK5_ConvertVSOPToFK5B1950(const value: TAA3DCoordinate): TAA3DCoordinate;

Return Value
A record containing the converted equatorial FK5 B1950 reference frame coordinates.

Parameters
value The geometric rectangular ecliptical coordinates of the object (e.g. the Sun) to convert from the dynamical reference frame (VSOP) to the equatorial FK5 B1950 reference frame.
}
function FK5_ConvertVSOPToFK5B1950(const value: TAA3DCoordinate): TAA3DCoordinate;
begin
  Result.X := 0.999925702634 * value.X + 0.012189716217 * value.Y + 0.000011134016 * value.Z;
  Result.Y := -0.011179418036 * value.X + 0.917413998946 * value.Y - 0.397777041885 * value.Z;
  Result.Z := -0.004859003787 * value.X + 0.397747363646 * value.Y + 0.917482111428 * value.Z;
end;

{
function FK5_ConvertVSOPToFK5AnyEquinox(const value: TAA3DCoordinate; const JDEEquinox: Extended): TAA3DCoordinate;

Return Value
A record containing the converted equatorial FK5 reference frame coordinates.

Parameters
value The geometric rectangular ecliptical coordinates of the object (e.g. the Sun) to convert from the dynamical reference frame (VSOP) to the equatorial FK5 reference frame of JDEEquinox.
JDEEquinox The Julian day for which equatorial coordinates should be calculated for.
}
function FK5_ConvertVSOPToFK5AnyEquinox(const value: TAA3DCoordinate; const JDEEquinox: Extended): TAA3DCoordinate;
var
  t, tsquared, tcubed: Extended;
  sigma, zeta, phi: Extended;
  cossigma, coszeta, cosphi, sinsigma, sinzeta, sinphi: Extended;
  xx, xy, xz, yx, yy, yz, zx, zy, zz: Extended;
begin
  t := (JDEEquinox - 2451545.0) / 36525;
  tsquared := t*t;
  tcubed  := tsquared * t;

  sigma := 2306.2181*t + 0.30188*tsquared + 0.017988*tcubed;
  sigma := DegreesToRadians(DMSToDegrees(0, 0, sigma));

  zeta := 2306.2181*t + 1.09468*tsquared + 0.018203*tcubed;
  zeta := DegreesToRadians(DMSToDegrees(0, 0, zeta));

  phi := 2004.3109*t - 0.42665*tsquared - 0.041833*tcubed;
  phi := DegreesToRadians(DMSToDegrees(0, 0, phi));

  cossigma := cos(sigma);
  coszeta := cos(zeta);
  cosphi := cos(phi);
  sinsigma := sin(sigma);
  sinzeta := sin(zeta);
  sinphi := sin(phi);

  xx := cossigma * coszeta * cosphi -sinsigma*sinzeta;
  xy := sinsigma * coszeta + cossigma * sinzeta * cosphi;
  xz := cossigma * sinphi;
  yx := -cossigma * sinzeta - sinsigma * coszeta * cosphi;
  yy := cossigma * coszeta - sinsigma * sinzeta * cosphi;
  yz := -sinsigma * sinphi;
  zx := -coszeta * sinphi;
  zy := -sinzeta * sinphi;
  zz := cosphi;

  Result.X := xx * value.X + yx * value.Y + zx * value.Z;
  Result.Y := xy * value.X + yy * value.Y + zy * value.Z;
  Result.Z := xz * value.X + yz * value.Y + zz * value.Z;
end;

end.
