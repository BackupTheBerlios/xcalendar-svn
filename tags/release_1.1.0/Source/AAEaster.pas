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

unit AAEaster;
// This unit provides for calculation of the date of Easter in both the Julian and Gregorian calendars. This refers to Chapter 8 in the book.

interface

type
  TAAEasterDetails = record
    Month: Integer;
    Day: Integer;
  end;

function Easter_Calculate(nYear: Integer; bGregorianCalendar: Boolean): TAAEasterDetails;


implementation

{
function Easter_Calculate(nYear: Integer; GregorianCalendar: Boolean): TAAEasterDetails;

Return Value
A record containing
Month The month on which Easter Sunday occurs.
Day The day of the month on which Easter Sunday occurs.

Parameters
nYear The year to perform the calculation for.
bGregorianCalendar True if the calculation is to be performed for the Gregorian calendar, False implies the Julian Calendar
}
function Easter_Calculate(nYear: Integer; bGregorianCalendar: Boolean): TAAEasterDetails;
var
  a, b, c, d, e, f, g, h, i, k, l, m, n, p: Integer;
  
begin
  if (bGregorianCalendar) then
  begin
    a := nYear mod 19;
    b := nYear div 100;
    c := nYear mod 100;
    d := b div 4;
    e := b mod 4;
    f := (b+8) div 25;
    g := (b - f + 1) div 3;
    h := (19*a + b - d - g + 15) mod 30;
    i := c div 4;
    k := c mod 4;
    l := (32 + 2*e + 2*i - h -k) mod 7;
    m := (a + 11*h +22*l) div 451;
    n := (h + l - 7*m + 114) div 31;
    p := (h + l - 7*m + 114) mod 31;
    Result.Month := n;
    Result.Day := p + 1;
  end
  else
  begin
    a := nYear mod 4;
    b := nYear mod 7;
    c := nYear mod 19;
    d := (19*c + 15) mod 30;
    e := (2*a + 4*b - d + 34) mod 7;
    f := (d + e + 114) div 31;
    g := (d + e + 114) mod 31;
    Result.Month := f;
    Result.Day := g + 1;
  end;
end;

end.
