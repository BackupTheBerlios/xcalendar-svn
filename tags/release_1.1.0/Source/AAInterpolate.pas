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

unit AAInterpolate;
// This unit provides the algorithms for interpolation. This refers to Chapter 3 in the book.

interface

function Interpolate(const n, Y1, Y2, Y3: Extended): Extended; overload;
function Interpolate(const n, Y1, Y2, Y3, Y4, Y5: Extended): Extended; overload;
function InterpolateToHalves(const Y1, Y2, Y3, Y4: Extended): Extended;
function LagrangeInterpolate(const X: Extended; const n: Integer; const pX, pY: array of Extended): Extended;
function Interpolate_Extremum(const Y1, Y2, Y3: Extended; var nm: Extended): Extended; overload;
function Interpolate_Extremum(const Y1, Y2, Y3, Y4, Y5: Extended; var nm: Extended): Extended; overload;
function Interpolate_Zero(const Y1, Y2, Y3: Extended): Extended; overload;
function Interpolate_Zero(const Y1, Y2, Y3, Y4, Y5: Extended): Extended; overload;
function Interpolate_Zero2(const Y1, Y2, Y3: Extended): Extended; overload;
function Interpolate_Zero2(const Y1, Y2, Y3, Y4, Y5: Extended): Extended; overload;


implementation

{
function Interpolate(const n, Y1, Y2, Y3: Extended): Extended;
function Interpolate(const n, Y1, Y2, Y3, Y4, Y5: Extended): Extended;

Return Value
The interpolated Y value.

Parameters
n The interpolating factor.
Y1 The first Y value to interpolate from.
Y2 The second Y value to interpolate from.
Y3 The third Y value to interpolate from.
Y4 The fourth Y value to interpolate from.
Y5 The fifth Y value to interpolate from.

Remarks
Interpolates a function from 3 or 5 points.
}
function Interpolate(const n, Y1, Y2, Y3: Extended): Extended;
var
  a, b, c: Extended;
begin
  a := Y2 - Y1;
  b := Y3 - Y2;
  c := Y1 + Y3 - 2*Y2;

  Result := Y2 + n / 2 * (a + b + n*c);
end;

function Interpolate(const n, Y1, Y2, Y3, Y4, Y5: Extended): Extended;
var
  A, B, C, D, E, F, G, H, J, K: Extended;
  N2, N3, N4: Extended;
begin
  A := Y2 - Y1;
  B := Y3 - Y2;
  C := Y4 - Y3;
  D := Y5 - Y4;
  E := B - A;
  F := C - B;
  G := D - C;
  H := F - E;
  J := G - F;
  K := J - H;

  N2 := n*n;
  N3 := N2*n;
  N4 := N3*n;

  Result := Y3 + n*((B+C)/2 - (H+J)/12) + N2*(F/2 - K/24) + N3*((H+J)/12) + N4*(K/24);
end;


{
function InterpolateToHalves(const Y1, Y2, Y3, Y4: Extended): Extended;

Return Value
The interpolated Y value.

Parameters
Y1 The first Y value to interpolate from.
Y2 The second Y value to interpolate from.
Y3 The third Y value to interpolate from.
Y4 The fourth Y value to interpolate from.

Remarks
Interpolates a function to the middle location where 4 evenly spaced values are provided.
}
function InterpolateToHalves(const Y1, Y2, Y3, Y4: Extended): Extended;
begin
  Result := (9*(Y2 + Y3) - Y1 - Y4) / 16;
end;

{
function LagrangeInterpolate(const X: Extended; const n: Integer; const pX, pY: array of Extended): Extended;

Return Value
The interpolated Y value.

Parameters
X The X value to interpolate for.
n The size of the pX and pY arrays.
pX Pointer to the array of X coordinates to interpolate from.
pY Pointer to the array of Y coordinates to interpolate from.

Remarks
Interpolates a function using Lagrange's formula where an arbitrary number of values are provided.
}
function LagrangeInterpolate(const X: Extended; const n: Integer; const pX, pY: array of Extended): Extended;
var
  i, j: Integer;
  C: Extended;
begin
  Result := 0;
  for i:=1 to n do
  begin
    C := 1;
    for j:=1 to n do
    begin
      if (j <> i) then
        C := C*(X - pX[j-1]) / (pX[i-1] - pX[j-1]);
    end;

    Result := Result + C*pY[i - 1];
  end;
end;

{
function Interpolate_Extremum(const Y1, Y2, Y3: Extended; var nm: Extended): Extended;
function Interpolate_Extremum(const Y1, Y2, Y3, Y4, Y5: Extended; var nm: Extended): Extended;

Return Value
The extreme Y value.

Parameters
Y1 The first Y value to interpolate from.
Y2 The second Y value to interpolate from.
Y3 The third Y value to interpolate from.
Y4 The fourth Y value to interpolate from.
Y5 The fifth Y value to interpolate from.
nm Upon return will contain the corresponding value of the argument X where the extremum is reached.

Remarks
Interpolates a function to determine where the function reaches an extremum.
}
function Interpolate_Extremum(const Y1, Y2, Y3: Extended; var nm: Extended): Extended;
var
  a, b, c, ab: Extended;
begin
  a := Y2 - Y1;
  b := Y3 - Y2;
  c := Y1 + Y3 - 2*Y2;

  ab := a + b;

  nm := -ab/(2*c);
  Result := (Y2 - ((ab*ab)/(8*c)));
end;

function Interpolate_Extremum(const Y1, Y2, Y3, Y4, Y5: Extended; var nm: Extended): Extended;
var
  A, B, C, D, E, F, G, H, J, K: Extended;
  bRecalc: Boolean;
  nmprev, NMprev2, NMprev3: Extended;
begin
  A := Y2 - Y1;
  B := Y3 - Y2;
  C := Y4 - Y3;
  D := Y5 - Y4;
  E := B - A;
  F := C - B;
  G := D - C;
  H := F - E;
  J := G - F;
  K := J - H;

  bRecalc := True;
  nmprev := 0;
  nm := nmprev;
  while (bRecalc) do
  begin
    NMprev2 := nmprev*nmprev;
    NMprev3 := NMprev2*nmprev;
    nm := (6*B + 6*C - H - J +3*NMprev2*(H+J) + 2*NMprev3*K) / (K - 12*F);

    bRecalc := (Abs(nm - nmprev) > 1E-12);
    if (bRecalc) then
      nmprev := nm;
  end;

  Result := Interpolate(nm, Y1, Y2, Y3, Y4, Y5);
end;


{
function Interpolate_Zero(const Y1, Y2, Y3: Extended): Extended;
function Interpolate_Zero(const Y1, Y2, Y3, Y4, Y5: Extended): Extended;

Return Value
The value of the argument X for which the function y becomes zero.

Parameters
Y1 The first Y value to interpolate from.
Y2 The second Y value to interpolate from.
Y3 The third Y value to interpolate from.
Y4 The fourth Y value to interpolate from.
Y5 The fifth Y value to interpolate from.

Remarks
Finds where a function reaches zero when the function is "almost a straight line".
}
function Interpolate_Zero(const Y1, Y2, Y3: Extended): Extended;
var
  a, b, c: Extended;
  bRecalc: Boolean;
  n0prev, n0: Extended;
begin
  a := Y2 - Y1;
  b := Y3 - Y2;
  c := Y1 + Y3 - 2*Y2;

  bRecalc := True;
  n0prev := 0;
  n0 := n0prev;
  while (bRecalc) do
  begin
    n0 := -2*Y2/(a + b + c*n0prev);

    bRecalc := (Abs(n0 - n0prev) > 1E-12);
    if (bRecalc) then
      n0prev := n0;
  end;

  Result := n0;
end;

function Interpolate_Zero(const Y1, Y2, Y3, Y4, Y5: Extended): Extended;
var
  A, B, C, D, E, F, G, H, J, K: Extended;
  bRecalc: Boolean;
  n0prev, n0, n0prev2, n0prev3, n0prev4: Extended;
begin
  A := Y2 - Y1;
  B := Y3 - Y2;
  C := Y4 - Y3;
  D := Y5 - Y4;
  E := B - A;
  F := C - B;
  G := D - C;
  H := F - E;
  J := G - F;
  K := J - H;

  bRecalc := True;
  n0prev := 0;
  n0 := n0prev;
  while (bRecalc) do
  begin
    n0prev2 := n0prev*n0prev;
    n0prev3 := n0prev2*n0prev;
    n0prev4 := n0prev3*n0prev;

    n0 := (-24*Y3 + n0prev2*(K - 12*F) - 2*n0prev3*(H+J) - n0prev4*K)/(2*(6*B + 6*C - H - J));

    bRecalc := (Abs(n0 - n0prev) > 1E-12);
    if (bRecalc) then
      n0prev := n0;
  end;

  Result := n0;
end;


{
function Interpolate_Zero2(const Y1, Y2, Y3: Extended): Extended;
function Interpolate_Zero2(const Y1, Y2, Y3, Y4, Y5: Extended): Extended;

Return Value
The value of the argument X for which the function y becomes zero.

Parameters
Y1 The first Y value to interpolate from.
Y2 The second Y value to interpolate from.
Y3 The third Y value to interpolate from.
Y4 The fourth Y value to interpolate from.
Y5 The fifth Y value to interpolate from.

Remarks
Finds where a function reaches zero when the curvature of the function is important.
}
function Interpolate_Zero2(const Y1, Y2, Y3: Extended): Extended;
var
  a, b, c: Extended;
  bRecalc: Boolean;
  n0prev, n0, deltan0: Extended;
begin
  a := Y2 - Y1;
  b := Y3 - Y2;
  c := Y1 + Y3 - 2*Y2;

  bRecalc := True;
  n0prev := 0;
  n0 := n0prev;
  while (bRecalc) do
  begin
    deltan0 := - (2*Y2 + n0prev*(a + b + c*n0prev)) / (a + b + 2*c*n0prev);
    n0 := n0prev + deltan0;

    bRecalc := (Abs(deltan0) > 1E-12);
    if (bRecalc) then
      n0prev := n0;
  end;

  Result := n0;
end;

function Interpolate_Zero2(const Y1, Y2, Y3, Y4, Y5: Extended): Extended;
var
  A, B, C, D, E, F, G, H, J, K, M, N, P, Q: Extended;
  bRecalc: Boolean;
  n0prev, n0, n0prev2, n0prev3, n0prev4, deltan0: Extended;
begin
  A := Y2 - Y1;
  B := Y3 - Y2;
  C := Y4 - Y3;
  D := Y5 - Y4;
  E := B - A;
  F := C - B;
  G := D - C;
  H := F - E;
  J := G - F;
  K := J - H;
  M := K / 24;
  N := (H + J)/12;
  P := F/2 - M;
  Q := (B+C)/2 - N;

  bRecalc := True;
  n0prev := 0;
  n0 := n0prev;
  while (bRecalc) do
  begin
    n0prev2 := n0prev*n0prev;
    n0prev3 := n0prev2*n0prev;
    n0prev4 := n0prev3*n0prev;

    deltan0 := - (M * n0prev4 + N*n0prev3 + P*n0prev2 + Q*n0prev + Y3) / (4*M*n0prev3 + 3*N*n0prev2 + 2*P*n0prev + Q);
    n0 := n0prev + deltan0;

    bRecalc := (Abs(deltan0) > 1E-12);
    if (bRecalc) then
      n0prev := n0;
  end;

  Result := n0;
end;

end.
