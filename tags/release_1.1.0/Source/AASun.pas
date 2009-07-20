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

unit AASun;
// This unit provides for calculation of the geocentric position of the Sun. This refers to Chapter 25 & 26 in the book.

interface

uses
  AACoordinates;


function Sun_GeometricEclipticLongitude(const JDE: Extended): Extended;
function Sun_GeometricEclipticLatitude(const JDE: Extended): Extended;
function Sun_GeometricEclipticLongitudeJ2000(const JDE: Extended): Extended;
function Sun_GeometricEclipticLatitudeJ2000(const JDE: Extended): Extended;
function Sun_GeometricFK5EclipticLongitude(const JDE: Extended): Extended;
function Sun_GeometricFK5EclipticLatitude(const JDE: Extended): Extended;
function Sun_ApparentEclipticLongitude(const JDE: Extended): Extended;
function Sun_ApparentEclipticLatitude(const JDE: Extended): Extended;
function Sun_EclipticRectangularCoordinatesMeanEquinox(const JDE: Extended): TAA3DCoordinate;
function Sun_EclipticRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;
function Sun_EquatorialRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;
function Sun_EquatorialRectangularCoordinatesB1950(const JDE: Extended): TAA3DCoordinate;
function Sun_EquatorialRectangularCoordinatesAnyEquinox(const JDE, JDEEquinox: Extended): TAA3DCoordinate;


implementation

uses
  AACoordinateTransformation,
  AAEarth,
  AAFK5,
  AANutation;

{
function Sun_GeometricEclipticLongitude(const JDE: Extended): Extended;

Return Value
the ecliptic longitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in the VSOP theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_GeometricEclipticLongitude(const JDE: Extended): Extended;
begin
  Result := MapTo0To360Range(Earth_EclipticLongitude(JDE) + 180);
end;

{
function Sun_GeometricEclipticLatitude(const JDE: Extended): Extended;

Return Value
the ecliptic latitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in the VSOP theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_GeometricEclipticLatitude(const JDE: Extended): Extended;
begin
  Result := -Earth_EclipticLatitude(JDE);
end;

function Sun_GeometricEclipticLongitudeJ2000(const JDE: Extended): Extended;
begin
  Result := MapTo0To360Range(Earth_EclipticLongitudeJ2000(JDE) + 180);
end;

function Sun_GeometricEclipticLatitudeJ2000(const JDE: Extended): Extended;
begin
  Result :=  -Earth_EclipticLatitudeJ2000(JDE);
end;

{
function Sun_GeometricFK5EclipticLongitude(const JDE: Extended): Extended;

Return Value
the ecliptic longitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_GeometricFK5EclipticLongitude(const JDE: Extended): Extended;
var
  Longitude, Latitude: Extended;
begin
  //Convert to the FK5 stystem
  Longitude := Sun_GeometricEclipticLongitude(JDE);
  Latitude := Sun_GeometricEclipticLatitude(JDE);
  Longitude := Longitude + FK5_CorrectionInLongitude(Longitude, Latitude, JDE);

  Result := Longitude;
end;

{
function Sun_GeometricFK5EclipticLatitude(const JDE: Extended): Extended;

Return Value
the ecliptic latitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_GeometricFK5EclipticLatitude(const JDE: Extended): Extended;
var
  Longitude, Latitude, SunLatCorrection: Extended;
begin
  //Convert to the FK5 stystem
  Longitude := Sun_GeometricEclipticLongitude(JDE);
  Latitude := Sun_GeometricEclipticLatitude(JDE);
  SunLatCorrection := FK5_CorrectionInLatitude(Longitude, JDE);
  Latitude := Latitude + SunLatCorrection;

  Result := Latitude;
end;

{
function Sun_ApparentEclipticLongitude(const JDE: Extended): Extended;

Return Value
the apparent ecliptic longitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in theFK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_ApparentEclipticLongitude(const JDE: Extended): Extended;
var
  Longitude, R: Extended;
begin
  Longitude := Sun_GeometricFK5EclipticLongitude(JDE);

  //Apply the correction in longitude due to nutation
  Longitude := Longitude + DMSToDegrees(0, 0, NutationInLongitude(JDE));

  //Apply the correction in longitude due to aberration
  R := Earth_RadiusVector(JDE);
  Longitude := Longitude - DMSToDegrees(0, 0, 20.4898 / R);

  Result := Longitude;
end;

{
function Sun_ApparentEclipticLatitude(const JDE: Extended): Extended;

Return Value
the apparent ecliptic latitude in degrees referred to the mean dynamical ecliptic and equinox of the date defined in the VSOP theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_ApparentEclipticLatitude(const JDE: Extended): Extended;
begin
  Result := Sun_GeometricFK5EclipticLatitude(JDE);
end;

function Sun_EclipticRectangularCoordinatesMeanEquinox(const JDE: Extended): TAA3DCoordinate;
var
  Longitude, Latitude, R, epsilon: Extended;
begin
  Longitude := DegreesToRadians(Sun_GeometricFK5EclipticLongitude(JDE));
  Latitude := DegreesToRadians(Sun_GeometricFK5EclipticLatitude(JDE));
  R := Earth_RadiusVector(JDE);
  epsilon := DegreesToRadians(MeanObliquityOfEcliptic(JDE));

  Result.X := R * cos(Latitude) * cos(Longitude);
  Result.Y := R * (cos(Latitude) * sin(Longitude) * cos(epsilon) - sin(Latitude) * sin(epsilon));
  Result.Z := R * (cos(Latitude) * sin(Longitude) * sin(epsilon) + sin(Latitude) * cos(epsilon));
end;

{
function Sun_EclipticRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;

Return Value
A record containing the ecliptic 3D rectangular coordinates in astronomical units referred to the J2000 equinox defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_EclipticRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;
var
  Longitude, Latitude, R, coslatitude: Extended;
begin
  Longitude := Sun_GeometricEclipticLongitudeJ2000(JDE);
  Longitude := DegreesToRadians(Longitude);
  Latitude := Sun_GeometricEclipticLatitudeJ2000(JDE);
  Latitude := DegreesToRadians(Latitude);
  R := Earth_RadiusVector(JDE);

  coslatitude := cos(Latitude);
  Result.X := R * coslatitude * cos(Longitude);
  Result.Y := R * coslatitude * sin(Longitude);
  Result.Z := R * sin(Latitude);
end;

{
function Sun_EquatorialRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;

Return Value
A record containing the equatorial 3D rectangular coordinates in astronomical units referred to the J2000 equinox defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_EquatorialRectangularCoordinatesJ2000(const JDE: Extended): TAA3DCoordinate;
begin
  Result := Sun_EclipticRectangularCoordinatesJ2000(JDE);
  Result := FK5_ConvertVSOPToFK5J2000(Result);
end;

{
function Sun_EquatorialRectangularCoordinatesB1950(const JDE: Extended): TAA3DCoordinate;

Return Value
A record containing the equatorial 3D rectangular coordinates in astronomical units referred to the B1950 equinox defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
}
function Sun_EquatorialRectangularCoordinatesB1950(const JDE: Extended): TAA3DCoordinate;
begin
  Result := Sun_EclipticRectangularCoordinatesJ2000(JDE);
  Result := FK5_ConvertVSOPToFK5B1950(Result);
end;

{
function Sun_EquatorialRectangularCoordinatesAnyEquinox(const JDE, JDEEquinox: Extended): TAA3DCoordinate;

Return Value
A record containing the equatorial 3D rectangular coordinates in astronomical units referred to the specified equinox defined in the FK5 theory.

Parameters
JDE The date in Dynamical time to calculate for.
JDEEquinox The date in Dynamical time specifying the equinox to use.
}
function Sun_EquatorialRectangularCoordinatesAnyEquinox(const JDE, JDEEquinox: Extended): TAA3DCoordinate;
begin
  Result := Sun_EquatorialRectangularCoordinatesJ2000(JDE);
  Result := FK5_ConvertVSOPToFK5AnyEquinox(Result, JDEEquinox);
end;

end.
