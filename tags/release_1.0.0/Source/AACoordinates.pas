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

unit AACoordinates;
// This unit provides the simple structures to encapsulate two and three dimensional coordinates

interface

type

  TAA2DCoordinate = record
    X: Extended;
    Y: Extended;
  end;

  TAA3DCoordinate = record
    X: Extended;
    Y: Extended;
    Z: Extended;
  end;

implementation

end.
