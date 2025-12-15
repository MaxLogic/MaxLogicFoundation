unit max3DMath;
{ *******************************************************

  Digital Omerta 3D Engine

  Unit Author:   Pawel Piotrowski
  Copyright 	    Pawel Piotrowski

  ******************************************************* }
{$DEFINE CLOOTIE}

interface

uses
  windows, Math, SysUtils, classes,
  Direct3D9, D3DX9;

const
  Eps = 0.00001;

var
  ZeroMatrix: TD3DXMatrix;

type
  PVector = pD3DXVector3;

  TPolygon = record
    case boolean of
      False:
        (pts: array [0 .. 4] of TD3DXVector3
        );
      True:
        (p1, p2, p3, p4, Normal: TD3DXVector3);
  end;

function MakeVector(x, y, z: single): TD3DVector;

{ function MakeVertex(CONST Position: TD3DXVector3;CONST Normal: TD3DXVector3;
  U1, V1: single): TD3DVertex2; }

function Vectornormalize(CONST v1: TD3DXVector3): TD3DXVector3; overload;
function Vectornormalize(CONST v1: TD3DXVector2): TD3DXVector2; overload;

function Vectorcrossproduct(CONST v1, v2: TD3DXVector3): TD3DXVector3;

function Vectordotproduct(CONST v1, v2: TD3DXVector3): single; overload;
function Vectordotproduct(CONST v1, v2: Tpoint): single; overload;
function Vectordotproduct(CONST v1, v2: TD3DXVector2): single; overload;

function Vectoradd(CONST v1, v2: TD3DXVector3): TD3DXVector3; overload;
function Vectoradd(CONST v1, v2: TD3DXVector2): TD3DXVector2; overload;

function Vectorsub(CONST v1, v2: TD3DXVector3): TD3DXVector3; overload;
function Vectorsub(CONST v1, v2: TD3DXVector2): TD3DXVector2; overload;

function Vectormul(CONST v1, v2: TD3DXVector3): TD3DXVector3; overload;
function Vectormul(CONST v1, v2: TD3DXVector2): TD3DXVector2; overload;

function Vectormuls(CONST v1: TD3DXVector3; s: single): TD3DXVector3; overload;
function Vectormuls(CONST v1: TD3DXVector2; s: single): TD3DXVector2;
  overload; overload;

function Vectordiv(CONST v1, v2: TD3DXVector3): TD3DXVector3; overload;
function Vectordivs(CONST v1: TD3DXVector3; s: single): TD3DXVector3; overload;
function Vectormagnitude(CONST v1: TD3DXVector3): single; overload;
function BillboardLookAtY(CONST BillboardPos, BillboardNormal,
  CameraPosition: TD3DVector): TD3DXMatrix;
function BillboardLookAt(Const BillboardPosition, CameraPosition: TD3DXVector3)
  : TD3DXMatrix;
function CalcNormalCCW(CONST v1, v2, v3: TD3DXVector3): TD3DXVector3;
function CalcNormalCW(CONST v1, v2, v3: TD3DXVector3): TD3DXVector3;

procedure VectorAddInline(var desTD3DXVector3: TD3DXVector3;
  const AddVector: TD3DXVector3); overload;
procedure VectorSubInline(var desTD3DXVector3: TD3DXVector3;
  const SubVector: TD3DXVector3); overload;

procedure VectorMulInline(var desTD3DXVector3: TD3DXVector3;
  const Scale: single); overload;
procedure VectorDivInline(var desTD3DXVector3: TD3DXVector3;
  const Scale: single); overload;
function Vectormul(const desTD3DXVector3: TD3DXVector3; const Scale: single)
  : TD3DXVector3; overload;
function Vectordiv(const desTD3DXVector3: TD3DXVector3; const Scale: single)
  : TD3DXVector3; overload;

function VectorPlusScalar(const desTD3DXVector3: TD3DXVector3;
  const AddNum: single): TD3DXVector3;
procedure VectorPlusScalarInline(var desTD3DXVector3: TD3DXVector3;
  const AddNum: single);

function VectorSquare(const Vector1: TD3DXVector3): TD3DXVector3;
procedure VectorSquareInline(var Vector1: TD3DXVector3);

// returns a negated vector

procedure VectorNegateInline(var Vector1: TD3DXVector3);
function VectorNegate(const Vector1: TD3DXVector3): TD3DXVector3;

function VectorLength(const Vector1: TD3DXVector3): single; overload;
function VectorLength(const Vector1: TD3DXVector2): single; overload;
function VectorLengthSQRT(const Vector1: TD3DXVector3): single;
function Magnitude(const V: TD3DXVector3): single;

function Vectornormalise(const Vector1: TD3DXVector3): TD3DXVector3;
procedure VectornormaliseInline(var Vector1: TD3DXVector3);

function VectorNull: TD3DXVector3;
procedure VectorNullInline(out Vector1: TD3DXVector3);

function VectorAbs(const Vector1: TD3DXVector3): TD3DXVector3;
procedure VectorAbsInline(var Vector1: TD3DXVector3);

// calculates the cosine of the angle between Vector1 and Vector2 in radians
function CosAngle(const Vector1, Vector2: TD3DXVector3): single; overload;
function CosAngle(const Vector1, Vector2: TD3DXVector2): single; overload;
// in radians
Function VectorAngle(Const v1, v2: TD3DXVector3): single; overload;
Function VectorAngle(Const v1, v2: TD3DXVector2): single; overload;
Procedure VectorAngles(CONST BASE, Relative: TD3DXVector3;
  Out Angle: TD3DXVector3);

// result in degrees
// point 2 is the origin of the p1 and p3.. This function returns always the smallest angle, no matter which way the 2 points are given
Function VertexAngle(x1, y1, x2, y2, x3, y3: single): single; Overload;
Function VertexAngle(const Pnt1, Pnt2, Pnt3: TD3DXVector2): single; Overload;
// as the previous but returns the full reall angle from point 1 to point 2. co it can be > 180
Function VertexFullAngle(x1, y1, x2, y2, x3, y3: single): single;
Function IsEqual(Val1, Val2: single): boolean;

function VectorAngleIn2D(const BasisPoint, RelPoint: TD3DXVector2): single;
function DirectionToAngle(const dir: TD3DXVector2): single;
Function RotAxisToRotXYZ(Angle: single; Axis: TD3DXVector3): TD3DXVector3;

function DotProduct(const Vector1, Vector2: TD3DXVector3): single;
function CrossProduct(const Vector1, Vector2: TD3DXVector3): TD3DXVector3;

function MakePoly(const pt1, pt2, pt3, pt4: TD3DXVector3): TPolygon;

/// ////////////////////////////////////////////////////////////////////////////
// Vector Operations
/// ////////////////////////////////////////////////////////

function D3DVector(x, y, z: single): TD3DVector;
// Vector dominance
function VectorSmaller(v1, v2: TD3DVector): boolean;
function VectorSmallerEquel(v1, v2: TD3DVector): boolean;
// Bitwise equality
function VectorEquel(v1, v2: TD3DVector): boolean;
// Length-related functions
function VectorSquareMagnitude(V: TD3DVector): single;
// Return min/max component of the input Vector
function VectorMin(V: TD3DVector): single;
function VectorMax(V: TD3DVector): single;
// Return memberwise min/max of input Vectors
function VectorMinimize(const v1, v2: TD3DVector): TD3DVector;
function VectorMaximize(const v1, v2: TD3DVector): TD3DVector;
Function IsInFrontOfPlane(Const PlanePoint, Normal,
  D3DPoint: TD3DXVector3): boolean;
Function DistanceToPlane(const PlanePoint, PlaneNormal,
  Point3D: TD3DXVector3): single;
Function GetNormalByHitPoint(Const HitPoint, posA, normA, posB, normB, posC,
  normC: TD3DXVector3): TD3DXVector3;
Function RotateVec3(Const V, Angle: TD3DXVector3): TD3DXVector3; overload;
Function RotateVec3(Const V, Axis: TD3DXVector3; Const Angle: single)
  : TD3DXVector3; overload;

Function VectorRotate(V, RotXYZ: TD3DXVector3): TD3DXVector3;
Function VectorDegToRad(V: TD3DXVector3): TD3DXVector3;

// RotAng is in degrees., the origin is 0,0
Procedure Rotate(RotAng: double; x, y: single; out Nx, Ny: single); Overload;
Function Rotate(const RotAng: double; const Pnt: TD3DXVector2)
  : TD3DXVector2; Overload;

/// ////////////////////////////////////////////////////////////////////////////
// Matrix Operations
/// ////////////////////////////////////////////////////////

function MatrixMul(const a, b: TD3DXMatrix): TD3DXMatrix;
procedure MatrixAppend(var a: TD3DXMatrix; const b: TD3DXMatrix);
function MatrixVectorMul(const vec: TD3DVector; const mat: TD3DXMatrix)
  : TD3DVector; overload;
function MatrixInvert(var q: TD3DXMatrix; const a: TD3DXMatrix): HResult;

/// ////////////////////////////////////////////////////////////////////////////
// Matrix Transformations
/// ////////////////////////////////////////////////////////

function SetViewMatrix(var MatrixView: TD3DXMatrix;
  From, at, Worldup: TD3DVector): HResult;
function SetProjectionMatrix(var mat: TD3DXMatrix;
  fFOV, fAspect, fNearPlane, fFarPlane: single): HResult;
procedure SetTranslateMatrix(var mat: TD3DXMatrix; tx, ty, tz: single);
  overload;
procedure SetTranslateMatrix(var mat: TD3DXMatrix; vec: TD3DVector); overload;
procedure SetScaleMatrix(var mat: TD3DXMatrix; sx, sy, sz: single); overload;
procedure SetScaleMatrix(var mat: TD3DXMatrix; vec: TD3DVector); overload;
procedure SetRotateXMatrix(var mat: TD3DXMatrix; fRads: single);
procedure SetRotateYMatrix(var mat: TD3DXMatrix; fRads: single);
procedure SetRotateZMatrix(var mat: TD3DXMatrix; fRads: single);
procedure SetRotationMatrix(var mat: TD3DXMatrix; vDir: TD3DVector;
  fRads: single); overload;
function SetRotationMatrix(vecNormal, vecFront: TD3DXVector3;
  Const Position: TD3DXVector3): TD3DXMatrix; overload;
Function SetRotationMatrix(Const vecFront, vecRight, vecNormal,
  vecPosition: TD3DXVector3): TD3DXMatrix; overload;

Procedure MakeLocal(Const globalRayStart, globalRayDir: TD3DXVector3;
  Out LocalRayStart, LocalRayDir: TD3DXVector3; Const matWorld: TD3DXMatrix);

Procedure SetMatrix(VAR mat: TD3DXMatrix;
  Const Pos, Rot, Scal: TD3DXVector3); overload;
Procedure SetMatrix(VAR mat: TD3DXMatrix; Const Pos, Axis, Scal: TD3DXVector3;
  Angle: single); overload;
Function CreateD3DXMatrix(Pos, Rot, Scl: TD3DXVector3): TD3DXMatrix;

/// ////////////////////////////////////////////////////////////////////////////
// Misc Operations
/// ////////////////////////////////////////////////////////

Function VectorRadToDeg(V: TD3DXVector3): TD3DXVector3;

function GetBillboardMatrixNEOS(BillboardPosition, CameraPosition: TD3DXVector3)
  : TD3DXMatrix;
Function ReflectRay(CONST RayStart, RayDir, HitPoint, Normal: TD3DXVector3)
  : TD3DXVector3;
Function GetTriangleHighAtPos(CONST x, y: single;
  CONST dreieck: array of pD3DXVector3): single;
Function GetHeightAtPos(CONST Normal, AnyTrisPoint: TD3DXVector3;
  Const Point: TD3DXVector2): single;

function VectorDistance(Const vPoint1, vPoint2: TD3DXVector3): single; overload;
Function VectorDistance(const p1, p2: TD3DXVector2): single; overload;
Function PointDistance(const p1, p2: Tpoint): single;
// this one is much faster then the full vector distance, if you just need to compare distances and not the actual values
function VectorDistanceSQRT(Const vPoint1, vPoint2: TD3DXVector3)
  : single; overload;
function VectorDistanceSQRT(Const vPoint1, vPoint2: TD3DXVector2)
  : single; overload;

// Bezier Curve interpolation
// see http://astronomy.swin.edu.au/~pbourke/curves/
function BezierLerp(const p: array of TD3DXVector3; n: integer; mu: single)
  : TD3DXVector3;
function Bezier4Lerp(const p1, p2, p3, p4: TD3DXVector3; mu: single)
  : TD3DXVector3;
Function Bezier3Lerp(const p1, p2, p3: TD3DXVector3; mu: single): TD3DXVector3;

Function GetRotByLookAt(const Pos, LookAtPos: TD3DXVector3): single;

// calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const v1, v2: TD3DXVector3; t: single): TD3DXVector3;

// As stated, one radian is equal to 180/pi degrees. Thus, to convert from radians to degrees, multiply by 180/pi.
function DegToRad(Degrees: single): single;
function RadToDeg(Radians: single): single;

// : Calculates linear interpolation between start and stop at point t
function Lerp(const start, stop, t: single): single;

{ Higher level functions }
function DistPoint2Line(const Point, LineP1, LineP2: TD3DXVector3): single;
function DistPoint2LineSegment(const Point, LineStart,
  LineEnd: TD3DXVector3): single;
Function ClosestPointOnLine(Const LineStartPoint, LineEndPoint,
  vPoint: TD3DXVector3): TD3DXVector3; overload;
Function ClosestPointOnLine(Const LineStart, LineEnd, vPoint: TD3DXVector2)
  : TD3DXVector2; overload;

// result is in radians
function AngleD2(x, y: single): single;
// result is in radians
function MinAngleBetweenVectors(const p, q, R: TD3DXVector3): single;
function MidPoint(const p1, p2: TD3DXVector3): TD3DXVector3;
function TriangleArea(const a, b, BASE: single): single;
function TriangleHeight(const a, b, BASE: single): single;
// lerp
function PointOnLine(const p1, p2: TD3DXVector3; const t: single): TD3DXVector3;
{ Twist, Clockwise Z Spin }
{ Phi, top gets closer first (X Rot) }
{ Theta, right gets closer first Y rotation }
// all rotations in degrees
function RotatePoint(const p: TD3DXVector3; const APhi, ATheta, ATwist: integer)
  : TD3DXVector3;
function DirectionVector(const p1, p2: TD3DXVector3): TD3DXVector3;
function MultiplyByScalar(const C: single; const V: TD3DXVector3): TD3DXVector3;

// other functions
function Phase(a, b: single): single;
function PhaseCoord(x1, y1, x2, y2: single): single;
function Gleich0(s: single): boolean;
function Gleich(s1, s2: single): boolean;
function GleichEps(s1, s2, e: single): boolean;

const
  Pi90 = Pi / 2;
  Pi45 = Pi / 4;

  { Do not alter these !, values are assigned in initialization section }
var
  { 360 is included to make programming easier }
  SinTable: array [0 .. 360] of single;
  CosTable: array [0 .. 360] of single;

  D3DXMatrixIdentity: TD3DXMatrix;

  (* ***********[ Orientation Constants ]********* *)
const
  orientation_RightHand = +1;
  orientation_LeftHand = -1;
  orientation_Clockwise = orientation_RightHand;
  orientation_CounterClockwise = -orientation_LeftHand;

Function Orientation(x1, y1, x2, y2, Px, Py: single): integer; Overload;
Function Orientation(const LineStart, LineEnd, pt: TD3DXVector2)
  : integer; Overload;
Function Orientation(const LineStart, LineEnd, pt: Tpoint): integer; Overload;
// line segment intersection
Function Intersect(x1, y1, x2, y2, x3, y3, x4, y4: single): boolean; Overload;
Function Intersect(const Seg1Start, Seg1End, Seg2Start, Seg2End: TD3DXVector2)
  : boolean; Overload;

function D3DXVector3(x, y, z: single): TD3DXVector3; inline;
function D3DXVector2(x, y: single): TD3DXVector2; inline;

implementation

Const
  PI2 = 6.283185307179586476925286766559000;

Const
  PIDiv180 = 0.017453292519943295769236907684886;

Const
  _180DivPI = 57.295779513082320876798154814105000;

  /// ////////////////////////
  // From srBasics

function Gleich0(s: single): boolean;
begin
  Result := abs(s) < 0.00001;
end;

function Phase(a, b: single): single;
var
  C, WSin: single;
begin
  if not Gleich0(a) and not Gleich0(b) then
  begin
    C := sqrt(sqr(a) + sqr(b));
    WSin := ArcCos(a / C);
  end
  else if Gleich0(a) then
    WSin := Pi / 2
  else
    WSin := 0;
  if b < 0 then
    WSin := 2 * Pi - WSin;
  Result := WSin;
end;

function PhaseCoord(x1, y1, x2, y2: single): single;
begin
  Result := Phase(x2 - x1, y2 - y1);
end;

// ##############################################################################
// Zusatzfunktionen
// ##############################################################################
function MakeVector(x, y, z: single): TD3DVector;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

{
  function MakeVertex(CONST Position: TD3DXVector3; CONST Normal: TD3DXVector3;
  U1, V1: single): TD3DVertex2;
  begin
  Result.x := position.x;
  Result.y := position.y;
  Result.z := position.z;
  Result.nx := normal.x;
  Result.ny := normal.y;
  Result.nz := normal.z;
  Result.tU := u1;
  Result.tV := v1;
  end;
}

function Vectornormalize(CONST v1: TD3DXVector2): TD3DXVector2;
var
  Length: single;
begin
  Length := sqrt(sqr(v1.x) + sqr(v1.y));

  if Length = 0 then
  begin
    ZeroMemory(@Result, SizeOf(Result));
    exit;
  end;
  Result.x := v1.x / Length;
  Result.y := v1.y / Length;
end;

function Vectornormalize(CONST v1: TD3DXVector3): TD3DXVector3;
var
  Length: single;
begin
  Length := sqrt(sqr(v1.x) + sqr(v1.y) + sqr(v1.z));
  if Length = 0 then
  begin
    ZeroMemory(@Result, SizeOf(Result));
    exit;
  end;
  Result.x := v1.x / Length;
  Result.y := v1.y / Length;
  Result.z := v1.z / Length;
end;

function Vectorcrossproduct(CONST v1, v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := (v1.y * v2.z) - (v1.z * v2.y);
  Result.y := (v1.z * v2.x) - (v1.x * v2.z);
  Result.z := (v1.x * v2.y) - (v1.y * v2.x);
end;

function Vectordotproduct(CONST v1, v2: TD3DXVector2): single;
begin
  Result := (v1.x * v2.x) + (v1.y * v2.y)
end;

function Vectordotproduct(CONST v1, v2: Tpoint): single;
begin
  Result := (v1.x * v2.x) + (v1.y * v2.y)
end;

function Vectordotproduct(CONST v1, v2: TD3DXVector3): single;
begin
  Result := (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z);
end;

function Vectoradd(CONST v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
end;

function Vectoradd(CONST v1, v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

function Vectorsub(CONST v1, v2: TD3DXVector2): TD3DXVector2; overload;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
end;

function Vectorsub(CONST v1, v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
end;

function Vectormul(CONST v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x := v1.x * v2.x;
  Result.y := v1.y * v2.y;
end;

function Vectormul(CONST v1, v2: TD3DXVector3): TD3DXVector3;
begin
  Result.x := v1.x * v2.x;
  Result.y := v1.y * v2.y;
  Result.z := v1.z * v2.z;
end;

function Vectordiv(CONST v1, v2: TD3DXVector3): TD3DXVector3; overload;
begin
  Result.x := v1.x / v2.x;
  Result.y := v1.y / v2.y;
  Result.z := v1.z / v2.z;
end;

function Vectordivs(CONST v1: TD3DXVector3; s: single): TD3DXVector3;
begin
  Result.x := v1.x / s;
  Result.y := v1.y / s;
  Result.z := v1.z / s;
end;

function Vectormagnitude(CONST v1: TD3DXVector3): single;
begin
  Result := sqrt(sqr(v1.x) + sqr(v1.y) + sqr(v1.z));
end;

{ ------------------------------------------------------------------------------
  Name        :  CalcNormal
  Description :  By JDV
  JALE - Just another little Engine -
  ... its not a bug its a feature!
  http://www.JoachimDeVries.de
  For CCW Tris.
  Example     :
  CW    2
  /|
  /  |
  /     |
  0------1

  CCW    1
  /|
  /  |
  /     |
  0------2
  ------------------------------------------------------------------------------ }

function CalcNormalCCW(CONST v1, v2, v3: TD3DXVector3): TD3DXVector3;
var
  l1, l2: TD3DXVector3;
begin
  D3DXVec3Subtract(l1, v2, v1);
  D3DXVec3Subtract(l2, v3, v1);
  D3DXVec3Cross(Result, l1, l2);
  D3DXVec3Normalize(Result, Result);
end;

function CalcNormalCW(CONST v1, v2, v3: TD3DXVector3): TD3DXVector3;
var
  l1, l2: TD3DXVector3;
begin
  D3DXVec3Subtract(l1, v3, v1);
  D3DXVec3Subtract(l2, v2, v1);
  D3DXVec3Cross(Result, l1, l2);
  D3DXVec3Normalize(Result, Result);
end;

procedure VectorAddInline(var desTD3DXVector3: TD3DXVector3;
  const AddVector: TD3DXVector3);
begin
  with desTD3DXVector3 do
  begin
    x := x + AddVector.x;
    y := y + AddVector.y;
    z := z + AddVector.z;
  end;
end;

procedure VectorSubInline(var desTD3DXVector3: TD3DXVector3;
  const SubVector: TD3DXVector3);
begin
  with desTD3DXVector3 do
  begin
    x := x - SubVector.x;
    y := y - SubVector.y;
    z := z - SubVector.z;
  end;
end;

procedure VectorNegateInline(var Vector1: TD3DXVector3);
// returns a negated vector
begin
  with Vector1 do
  begin
    x := -x;
    y := -y;
    z := -z;
  end;
end;

function VectorNegate(const Vector1: TD3DXVector3): TD3DXVector3;
begin
  with Result do
  begin
    x := -Vector1.x;
    y := -Vector1.y;
    z := -Vector1.z;
  end;
end;

procedure VectorMulInline(var desTD3DXVector3: TD3DXVector3;
  const Scale: single);
begin
  with desTD3DXVector3 do
  begin
    x := x * Scale;
    y := y * Scale;
    z := z * Scale;
  end;
end;

procedure VectorDivInline(var desTD3DXVector3: TD3DXVector3;
  const Scale: single);
var
  fScale: single;
begin
  fScale := 1 / Scale;
  with desTD3DXVector3 do
  begin
    x := x * fScale;
    y := y * fScale;
    z := z * fScale;
  end;
end;

function Vectormul(const desTD3DXVector3: TD3DXVector3; const Scale: single)
  : TD3DXVector3;
begin
  with Result do
  begin
    x := desTD3DXVector3.x * Scale;
    y := desTD3DXVector3.y * Scale;
    z := desTD3DXVector3.z * Scale;
  end;
end;

function Vectordiv(const desTD3DXVector3: TD3DXVector3; const Scale: single)
  : TD3DXVector3;
var
  fScale: single;
begin
  fScale := 1 / Scale;
  with Result do
  begin
    x := desTD3DXVector3.x * fScale;
    y := desTD3DXVector3.y * fScale;
    z := desTD3DXVector3.z * fScale;
  end;
end;

function VectorPlusScalar(const desTD3DXVector3: TD3DXVector3;
  const AddNum: single): TD3DXVector3;

begin
  with Result do
  begin
    x := x + AddNum;
    y := y + AddNum;
    z := z + AddNum;
  end;
end;

procedure VectorPlusScalarInline(var desTD3DXVector3: TD3DXVector3;
  const AddNum: single);
begin
  with desTD3DXVector3 do
  begin
    x := x + AddNum;
    y := y + AddNum;
    z := z + AddNum;
  end;
end;

function VectorSquare(const Vector1: TD3DXVector3): TD3DXVector3;
begin
  with Result do
  begin
    x := Vector1.x * Vector1.x;
    y := Vector1.y * Vector1.y;
    z := Vector1.z * Vector1.z;
  end;
end;

procedure VectorSquareInline(var Vector1: TD3DXVector3);
begin
  with Vector1 do
  begin
    x := x * x;
    y := y * y;
    z := z * z;
  end;
end;

function VectorLength(const Vector1: TD3DXVector2): single;
begin
  with Vector1 do
    Result := sqrt((x * x) + (y * y));
end;

function VectorLength(const Vector1: TD3DXVector3): single;
begin
  with Vector1 do
    Result := sqrt((x * x) + (y * y) + (z * z));
end;

function VectorLengthSQRT(const Vector1: TD3DXVector3): single;
begin
  with Vector1 do
    Result := ((x * x) + (y * y) + (z * z));
end;

function Vectornormalise(const Vector1: TD3DXVector3): TD3DXVector3;
var
  fLength: single;
begin
  // if Length = 0 then
  // Result := VectorNull
  // else
  // begin
  fLength := 1.0 / VectorLength(Vector1);

  with Result do
  begin
    x := Vector1.x * fLength;
    y := Vector1.y * fLength;
    z := Vector1.z * fLength;
  end;
  // end;
end;

procedure VectornormaliseInline(var Vector1: TD3DXVector3);
var
  fLength: single;
begin
  fLength := 1.0 / VectorLength(Vector1);

  with Vector1 do
  begin
    x := x * fLength;
    y := y * fLength;
    z := z * fLength;
  end;
end;

function VectorNull: TD3DXVector3;
begin
  with Result do
  begin
    x := 0.0;
    y := 0.0;
    z := 0.0;
  end;
end;

procedure VectorNullInline(out Vector1: TD3DXVector3);
begin
  with Vector1 do
  begin
    x := 0.0;
    y := 0.0;
    z := 0.0;
  end;
end;

function VectorAbs(const Vector1: TD3DXVector3): TD3DXVector3;
begin
  with Result do
  begin
    if Vector1.x < 0 then
      x := -Vector1.x
    else
      x := Vector1.x;
    if Vector1.y < 0 then
      y := -Vector1.y
    else
      y := Vector1.y;
    if Vector1.z < 0 then
      z := -Vector1.z
    else
      z := Vector1.z;
  end;
end;

procedure VectorAbsInline(var Vector1: TD3DXVector3);
begin
  with Vector1 do
  begin
    if x < 0 then
      x := -x;
    if y < 0 then
      y := -y;
    if z < 0 then
      z := -z;
  end;
end;

function CosAngle(const Vector1, Vector2: TD3DXVector3): single;
// calculates the cosine of the angle between Vector1 and Vector2
begin
  Result := DotProduct(Vector1, Vector2) /
    (VectorLength(Vector1) * VectorLength(Vector2));
end;

function CosAngle(const Vector1, Vector2: TD3DXVector2): single;
begin
  Result := Vectordotproduct(Vector1, Vector2) /
    (VectorLength(Vector1) * VectorLength(Vector2));
end;

Function RotAxisToRotXYZ(Angle: single; Axis: TD3DXVector3): TD3DXVector3;
var
  m: TD3DXMatrix;
  vI, vO, p1, p2: TD3DXVector3;
  // TEST
  { matRot,matRotY,matRotX,matRotZ : TD3DXMatrix;
    vo2:TD3DXVector3;
  }
Begin
  { TODO :
    this function is not precise enought....
    find an other one }
  vI := D3DXVector3(10, 10, 10);
  D3DX9.D3DXMatrixRotationAxis(m, Axis, Angle);
  D3DX9.D3DXVec3TransformCoord(vO, vI, m);

  p2 := D3DXVector3(vI.x, 0, vI.z);
  p1 := D3DXVector3(vO.x, 0, vO.z);
  Result.y := ArcCos(CosAngle(p1, p2));

  p2 := D3DXVector3(0, vI.y, vI.z);
  p1 := D3DXVector3(0, vO.y, vO.z);
  Result.x := ArcCos(CosAngle(p1, p2));

  p2 := D3DXVector3(vI.x, vI.y, 0);
  p1 := D3DXVector3(vO.x, vO.y, 0);
  Result.z := ArcCos(CosAngle(p1, p2));
  {
    // TEST
    D3DXMatrixRotationX(matRotX, result.X);
    D3DXMatrixRotationY(matRotY, result.Y);
    D3DXMatrixRotationZ(matRotZ, result.Z);
    D3DXMatrixMultiply(matRot, matRotZ, matRotX);
    D3DXMatrixMultiply(matRot, matRot, matRotY);
    D3DX9.D3DXVec3TransformCoord(vO2,vI,matRot);
    vi.x := ABS(vo2.x) - ABS(vo.x);
    vi.y := ABS(vo2.y) - ABS(vo.y);
    vi.z := ABS(vo2.z) - ABS(vo.z);
    If (Vi.x>0.001)OR (vi.y>0.001)OR (vi.z>0.001) Then
    ShowMessage('RotAxisToRotXYZ precision error');
  }
end;

function DotProduct(const Vector1, Vector2: TD3DXVector3): single;
begin
  with Vector1 do
    Result := (x * Vector2.x) + (y * Vector2.y) + (z * Vector2.z);
end;

function CrossProduct(const Vector1, Vector2: TD3DXVector3): TD3DXVector3;
begin
  { 23 - 32
    31 - 13
    12 - 21 }
  with Result do
  begin
    x := (Vector1.y * Vector2.z) - (Vector1.z * Vector2.y);
    y := (Vector1.z * Vector2.x) - (Vector1.x * Vector2.z);
    z := (Vector1.x * Vector2.y) - (Vector1.y * Vector2.x);
  end;
end;

function MakePoly(const pt1, pt2, pt3, pt4: TD3DXVector3): TPolygon;
begin
  with Result do
  begin
    p1 := pt1;
    p2 := pt2;
    p3 := pt3;
    p4 := pt4;

    // Now calculate the normal Vector... assumes c-clockwise
    Normal := CrossProduct(Vectorsub(pt2, pt1), Vectorsub(pt4, pt1));

    // Normalise the Vector
    VectornormaliseInline(Normal);
  end;
end;

function D3DVector(x, y, z: single): TD3DVector;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function VectorSmaller(v1, v2: TD3DVector): boolean;
begin
  Result := (v1.x < v2.x) and (v1.y < v2.y) and (v1.z < v2.z);
end;

function VectorSmallerEquel(v1, v2: TD3DVector): boolean;
begin
  Result := (v1.x <= v2.x) and (v1.y <= v2.y) and (v1.z <= v2.z);
end;

function VectorEquel(v1, v2: TD3DVector): boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

function VectorSquareMagnitude(V: TD3DVector): single;
begin
  Result := (V.x * V.x) + (V.y * V.y) + (V.z * V.z);
end;

function VectorMin(V: TD3DVector): single;
var
  ret: single;
begin
  ret := V.x;
  if (V.y < ret) then
    ret := V.y;
  if (V.z < ret) then
    ret := V.z;
  Result := ret;
end;

function VectorMax(V: TD3DVector): single;
var
  ret: single;
begin
  ret := V.x;
  if (ret < V.y) then
    ret := V.y;
  if (ret < V.z) then
    ret := V.z;
  Result := ret;
end;

function VectorMinimize(const v1, v2: TD3DVector): TD3DVector;
begin
  if v1.x < v2.x then
    Result.x := v1.x
  else
    Result.x := v2.x;
  if v1.y < v2.y then
    Result.y := v1.y
  else
    Result.y := v2.y;
  if v1.z < v2.z then
    Result.z := v1.z
  else
    Result.z := v2.z;
end;

function VectorMaximize(const v1, v2: TD3DVector): TD3DVector;
begin
  if v1.x > v2.x then
    Result.x := v1.x
  else
    Result.x := v2.x;
  if v1.y > v2.y then
    Result.y := v1.y
  else
    Result.y := v2.y;
  if v1.z > v2.z then
    Result.z := v1.z
  else
    Result.z := v2.z;
end;

function MatrixMul(const a, b: TD3DXMatrix): TD3DXMatrix;
var
  i, j, k: integer;
begin
  Result := ZeroMatrix;
  for i := 0 to 3 do
    for j := 0 to 3 do
      for k := 0 to 3 do
        Result.m[i, j] := Result.m[i, j] + (a.m[k, j] * b.m[i, k]);
end;

procedure MatrixAppend(var a: TD3DXMatrix; const b: TD3DXMatrix);
var
  i, j, k: integer;
  mat: TD3DXMatrix;
begin
  mat := ZeroMatrix;
  for i := 0 to 3 do
    for j := 0 to 3 do
      for k := 0 to 3 do
        mat.m[i, j] := mat.m[i, j] + (a.m[k, j] * b.m[i, k]);
  a := mat;
end;

function MatrixVectorMul(const vec: TD3DVector; const mat: TD3DXMatrix)
  : TD3DVector;
begin
{$IFNDEF CLOOTIE}
  Result.x := mat._00 * vec.x + mat._01 * vec.y + mat._02 * vec.z + mat._03;
  Result.y := mat._10 * vec.x + mat._11 * vec.y + mat._12 * vec.z + mat._13;
  Result.z := mat._20 * vec.x + mat._21 * vec.y + mat._22 * vec.z + mat._23;
{$ELSE}
  Result.x := mat._11 * vec.x + mat._12 * vec.y + mat._13 * vec.z + mat._14;
  Result.y := mat._21 * vec.x + mat._22 * vec.y + mat._23 * vec.z + mat._24;
  Result.z := mat._31 * vec.x + mat._32 * vec.y + mat._33 * vec.z + mat._34;
{$ENDIF}
end;

function MatrixInvert(var q: TD3DXMatrix; const a: TD3DXMatrix): HResult;
var
  DetInv: single;
begin
  Result := E_INVALIDARG;

{$IFNDEF CLOOTIE}
  if (abs(a._33 - 1) > 0.001) or (abs(a._03) > 0.001) or (abs(a._13) > 0.001) or
    (abs(a._23) > 0.001) then
    exit;

  DetInv := 1 / (a._00 * (a._11 * a._22 - a._12 * a._21) - a._11 *
    (a._10 * a._22 - a._12 * a._20) + a._02 * (a._10 * a._21 - a._11 * a._20));

  q._00 := DetInv * (a._11 * a._22 - a._12 * a._21);
  q._01 := -DetInv * (a._01 * a._22 - a._02 * a._21);
  q._02 := DetInv * (a._01 * a._12 - a._02 * a._11);
  q._03 := 0;

  q._10 := -DetInv * (a._10 * a._22 - a._12 * a._20);
  q._11 := DetInv * (a._00 * a._22 - a._02 * a._20);
  q._12 := -DetInv * (a._00 * a._12 - a._02 * a._10);
  q._13 := 0;

  q._20 := DetInv * (a._10 * a._21 - a._11 * a._20);
  q._21 := -DetInv * (a._00 * a._21 - a._01 * a._20);
  q._22 := DetInv * (a._00 * a._11 - a._01 * a._10);
  q._23 := 0;

  q._30 := -(a._30 * q._00 + a._31 * q._10 + a._32 * q._20);
  q._31 := -(a._30 * q._01 + a._31 * q._11 + a._32 * q._21);
  q._32 := -(a._30 * q._02 + a._31 * q._12 + a._32 * q._22);
  q._33 := 1;

{$ELSE}
  if (abs(a._44 - 1) > 0.001) or (abs(a._14) > 0.001) or (abs(a._24) > 0.001) or
    (abs(a._34) > 0.001) then
    exit;

  DetInv := 1 / (a._11 * (a._22 * a._33 - a._23 * a._32) - a._22 *
    (a._21 * a._33 - a._23 * a._31) + a._13 * (a._21 * a._32 - a._22 * a._31));

  q._11 := DetInv * (a._22 * a._33 - a._23 * a._32);
  q._12 := -DetInv * (a._12 * a._33 - a._13 * a._32);
  q._13 := DetInv * (a._12 * a._23 - a._13 * a._22);
  q._14 := 0;

  q._21 := -DetInv * (a._21 * a._33 - a._23 * a._31);
  q._22 := DetInv * (a._11 * a._33 - a._13 * a._31);
  q._23 := -DetInv * (a._11 * a._23 - a._13 * a._21);
  q._24 := 0;

  q._31 := DetInv * (a._21 * a._32 - a._22 * a._31);
  q._32 := -DetInv * (a._11 * a._32 - a._12 * a._31);
  q._33 := DetInv * (a._11 * a._22 - a._12 * a._21);
  q._34 := 0;

  q._41 := -(a._41 * q._11 + a._42 * q._21 + a._43 * q._31);
  q._42 := -(a._41 * q._12 + a._42 * q._22 + a._43 * q._32);
  q._43 := -(a._41 * q._13 + a._42 * q._23 + a._43 * q._33);
  q._44 := 1;

{$ENDIF}
  Result := S_OK;
end;

function SetViewMatrix(var MatrixView: TD3DXMatrix;
  From, at, Worldup: TD3DVector): HResult;
var
  View: TD3DVector;
  Length: single;
  DotProduct: single;
  Up: TD3DVector;
  Right: TD3DVector;
begin
  // Get the z basis Vector, which points straight ahead. This is the
  // difference from the eyepoint to the lookat point.
  View := Vectorsub(at, From);
  Length := Vectormagnitude(View);
  if (Length < 1E-6) then
  begin
    Result := E_INVALIDARG;
    exit;
  end;
  // Normalize the z basis Vector
  View := Vectordivs(View, Length);
  // Get the dot product, and calculate the projection of the z basis
  // Vector onto the up Vector. The projection is the y basis Vector.
  DotProduct := Vectordotproduct(Worldup, View);
  Up := Vectorsub(Worldup, Vectormuls(View, DotProduct));
  // If this Vector has near-zero length because the input specified a
  // bogus up Vector, let's try a default up Vector
  Length := Vectormagnitude(Up);
  if (Length < 1E-6) then
  begin
    Up := Vectorsub(D3DVector(0, 1, 0), Vectormuls(View, View.y));
    // If we still have near-zero length, resort to a different axis.
    Length := Vectormagnitude(Up);
    if (Length < 1E-6) then
    begin
      Up := Vectorsub(D3DVector(0, 0, 1), Vectormuls(View, View.z));
      Length := Vectormagnitude(Up);
      if (Length < 1E-6) then
      begin
        Result := E_INVALIDARG;
        exit;
      end;
    end;
  end;
  // Normalize the y basis Vector
  Up := Vectordivs(Up, Length);
  // The x basis Vector is found simply with the cross product of the y
  // and z basis Vectors
  Right := Vectorcrossproduct(Up, View);
  // Start building the matrix. The first three rows contains the basis
  // Vectors used to rotate the view to point at the lookat point
{$IFNDEF CLOOTIE}
  MatrixView._00 := Right.x;
  MatrixView._01 := Up.x;
  MatrixView._02 := View.x;
  MatrixView._03 := 0;
  MatrixView._10 := Right.y;
  MatrixView._11 := Up.y;
  MatrixView._12 := View.y;
  MatrixView._13 := 0;
  MatrixView._20 := Right.z;
  MatrixView._21 := Up.z;
  MatrixView._22 := View.z;
  MatrixView._23 := 0;
  // Do the translation values (rotations are still about the eyepoint)
  MatrixView._30 := -Vectordotproduct(From, Right);
  MatrixView._31 := -Vectordotproduct(From, Up);
  MatrixView._32 := -Vectordotproduct(From, View);
  MatrixView._33 := 1;
{$ELSE}
  MatrixView._11 := Right.x;
  MatrixView._12 := Up.x;
  MatrixView._13 := View.x;
  MatrixView._14 := 0;
  MatrixView._21 := Right.y;
  MatrixView._22 := Up.y;
  MatrixView._23 := View.y;
  MatrixView._24 := 0;
  MatrixView._31 := Right.z;
  MatrixView._32 := Up.z;
  MatrixView._33 := View.z;
  MatrixView._34 := 0;
  // Do the translation values (rotations are still about the eyepoint)
  MatrixView._41 := -Vectordotproduct(From, Right);
  MatrixView._42 := -Vectordotproduct(From, Up);
  MatrixView._43 := -Vectordotproduct(From, View);
  MatrixView._44 := 1;
{$ENDIF}
  Result := S_OK;
end;

function SetProjectionMatrix(var mat: TD3DXMatrix;
  fFOV, fAspect, fNearPlane, fFarPlane: single): HResult;
var
  w, h, q: single;
begin
  if (abs(fFarPlane - fNearPlane) < 0.01) or (abs(sin(fFOV / 2)) < 0.01) then
  begin
    Result := E_INVALIDARG;
    exit;
  end;
  h := (cos(fFOV / 2) / sin(fFOV / 2));
  w := fAspect * h;
  q := fFarPlane / (fFarPlane - fNearPlane);
  ZeroMemory(@mat, SizeOf(mat));
{$IFNDEF CLOOTIE}
  mat._00 := w;
  mat._11 := h;
  mat._22 := q;
  mat._23 := 1.0;
  mat._32 := -q * fNearPlane;
{$ELSE}
  mat._11 := w;
  mat._22 := h;
  mat._33 := q;
  mat._34 := 1.0;
  mat._43 := -q * fNearPlane;
{$ENDIF}
  Result := S_OK;
end;

Function RotateVec3(Const V, Angle: TD3DXVector3): TD3DXVector3;
var
  m: TD3DXMatrix;
Begin
  D3DX9.D3DXMatrixRotationYawPitchRoll(m, Angle.y, Angle.x, Angle.z);
  D3DX9.D3DXVec3TransformCoord(Result, V, m);
end;

Function RotateVec3(Const V, Axis: TD3DXVector3; Const Angle: single)
  : TD3DXVector3;
var
  m: TD3DXMatrix;
Begin
  D3DX9.D3DXMatrixRotationAxis(m, Axis, Angle);
  D3DX9.D3DXVec3TransformCoord(Result, V, m);
end;

Procedure SetMatrix(VAR mat: TD3DXMatrix; Const Pos, Rot, Scal: TD3DXVector3);
var
  matRot, matRotX, matRotY, matRotZ, matScale, matPos: TD3DXMatrix;
Begin
  D3DXMatrixTranslation(matPos, Pos.x, Pos.y, Pos.z);
  D3DXMatrixScaling(matScale, Scal.x, Scal.y, Scal.z);

  IF Rot.x <> 0 then
    D3DXMatrixRotationX(matRotX, Rot.x);
  IF Rot.y <> 0 then
    D3DXMatrixRotationY(matRotY, Rot.y);
  IF Rot.z <> 0 then
    D3DXMatrixRotationZ(matRotZ, Rot.z);

  IF (Rot.x <> 0) and (Rot.z <> 0) then
  Begin
    If (Rot.x = 0) then
      matRot := matRotZ
    else if Rot.z = 0 then
      matRot := matRotX
    else
      D3DXMatrixMultiply(matRot, matRotZ, matRotX);
  end
  else
    matRot := D3DXMatrixIdentity;

  if (Rot.y <> 0) then
  Begin
    IF (Rot.x <> 0) and (Rot.z <> 0) then
      D3DXMatrixMultiply(matRot, matRot, matRotY)
    else
      matRot := matRotY;
  end;

  D3DXMatrixMultiply(mat, matRot, matPos);
  D3DXMatrixMultiply(mat, matScale, mat);
end;

Procedure SetMatrix(VAR mat: TD3DXMatrix; Const Pos, Axis, Scal: TD3DXVector3;
  Angle: single);
var
  matRot, matScale, matPos: TD3DXMatrix;
Begin
  D3DXMatrixTranslation(matPos, Pos.x, Pos.y, Pos.z);
  D3DXMatrixScaling(matScale, Scal.x, Scal.y, Scal.z);
  D3DXMatrixRotationAxis(matRot, Axis, Angle);

  D3DXMatrixMultiply(mat, matRot, matPos);
  D3DXMatrixMultiply(mat, matScale, mat);
end;

procedure SetTranslateMatrix(var mat: TD3DXMatrix; tx, ty, tz: single);
  overload;
begin
{$IFNDEF CLOOTIE}
  mat := D3DXMatrixIdentity;
  mat._30 := tx;
  mat._31 := ty;
  mat._32 := tz;
{$ELSE}
  mat := D3DXMatrixIdentity;
  mat._41 := tx;
  mat._42 := ty;
  mat._43 := tz;
{$ENDIF}
end;

procedure SetTranslateMatrix(var mat: TD3DXMatrix; vec: TD3DVector); overload;
begin
{$IFNDEF CLOOTIE}
  mat := D3DXMatrixIdentity;
  mat._30 := vec.x;
  mat._31 := vec.y;
  mat._32 := vec.z;
{$ELSE}
  mat := D3DXMatrixIdentity;
  mat._41 := vec.x;
  mat._42 := vec.y;
  mat._43 := vec.z;
{$ENDIF}
end;

procedure SetScaleMatrix(var mat: TD3DXMatrix; sx, sy, sz: single); overload;
begin
  mat := D3DXMatrixIdentity;
{$IFNDEF CLOOTIE}
  mat._00 := sx;
  mat._11 := sy;
  mat._22 := sz;
{$ELSE}
  mat._11 := sx;
  mat._22 := sy;
  mat._33 := sz;
{$ENDIF}
end;

procedure SetScaleMatrix(var mat: TD3DXMatrix; vec: TD3DVector); overload;
begin
  mat := D3DXMatrixIdentity;
{$IFNDEF CLOOTIE}
  mat._00 := vec.x;
  mat._11 := vec.y;
  mat._22 := vec.z;
{$ELSE}
  mat._11 := vec.x;
  mat._22 := vec.y;
  mat._33 := vec.z;
{$ENDIF}
end;

procedure SetRotateXMatrix(var mat: TD3DXMatrix; fRads: single);
begin
  mat := D3DXMatrixIdentity;
  mat._11 := cos(fRads);
  mat._12 := sin(fRads);
  mat._21 := -sin(fRads);
  mat._22 := cos(fRads);
end;

procedure SetRotateYMatrix(var mat: TD3DXMatrix; fRads: single);
begin
  mat := D3DXMatrixIdentity;
{$IFNDEF CLOOTIE}
  mat._00 := cos(fRads);
  mat._02 := -sin(fRads);
  mat._20 := sin(fRads);
  mat._22 := cos(fRads);
{$ELSE}
  mat._11 := cos(fRads);
  mat._13 := -sin(fRads);
  mat._31 := sin(fRads);
  mat._33 := cos(fRads);
{$ENDIF}
end;

procedure SetRotateZMatrix(var mat: TD3DXMatrix; fRads: single);
begin
  mat := D3DXMatrixIdentity;
{$IFNDEF CLOOTIE}
  mat._00 := cos(fRads);
  mat._01 := sin(fRads);
  mat._10 := -sin(fRads);
  mat._11 := cos(fRads);

{$ELSE}
  mat._11 := cos(fRads);
  mat._12 := sin(fRads);
  mat._21 := -sin(fRads);
  mat._22 := cos(fRads);
{$ENDIF}
end;

procedure SetRotationMatrix(var mat: TD3DXMatrix; vDir: TD3DVector;
  fRads: single);
var
  fCos, fSin: single;
  V: TD3DVector;
begin
  fCos := cos(fRads);
  fSin := sin(fRads);
  V := Vectornormalize(vDir);
  mat := ZeroMatrix;
{$IFNDEF CLOOTIE}
  mat._00 := (V.x * V.x) * (1.0 - fCos) + fCos;
  mat._01 := (V.x * V.y) * (1.0 - fCos) - (V.z * fSin);
  mat._02 := (V.x * V.z) * (1.0 - fCos) + (V.y * fSin);

  mat._10 := (V.y * V.x) * (1.0 - fCos) + (V.z * fSin);
  mat._11 := (V.y * V.y) * (1.0 - fCos) + fCos;
  mat._12 := (V.y * V.z) * (1.0 - fCos) - (V.x * fSin);

  mat._20 := (V.z * V.x) * (1.0 - fCos) - (V.y * fSin);
  mat._21 := (V.z * V.y) * (1.0 - fCos) + (V.x * fSin);
  mat._22 := (V.z * V.z) * (1.0 - fCos) + fCos;

  mat._33 := 1.0;
{$ELSE}
  mat._11 := (V.x * V.x) * (1.0 - fCos) + fCos;
  mat._12 := (V.x * V.y) * (1.0 - fCos) - (V.z * fSin);
  mat._13 := (V.x * V.z) * (1.0 - fCos) + (V.y * fSin);

  mat._21 := (V.y * V.x) * (1.0 - fCos) + (V.z * fSin);
  mat._22 := (V.y * V.y) * (1.0 - fCos) + fCos;
  mat._23 := (V.y * V.z) * (1.0 - fCos) - (V.x * fSin);

  mat._31 := (V.z * V.x) * (1.0 - fCos) - (V.y * fSin);
  mat._32 := (V.z * V.y) * (1.0 - fCos) + (V.x * fSin);
  mat._33 := (V.z * V.z) * (1.0 - fCos) + fCos;

  mat._44 := 1.0;
{$ENDIF}
end;

Function CreateD3DXMatrix(Pos, Rot, Scl: TD3DXVector3): TD3DXMatrix;
var
  matPos, matScale, matRot, matRotX, matRotY, matRotZ: TD3DXMatrix;
Begin
  D3DXMatrixRotationX(matRotX, Rot.x);
  D3DXMatrixRotationY(matRotY, Rot.y);
  D3DXMatrixRotationZ(matRotZ, Rot.z);
  D3DXMatrixMultiply(matRot, matRotZ, matRotX);
  D3DXMatrixMultiply(matRot, matRot, matRotY);

  D3DXMatrixTranslation(matPos, Pos.x, Pos.y, Pos.z);
  D3DXMatrixScaling(matScale, Scl.x, Scl.y, Scl.z);

  D3DXMatrixMultiply(Result, matRot, matPos);
  D3DXMatrixMultiply(Result, matScale, Result);
end;

Function VectorRotate(V, RotXYZ: TD3DXVector3): TD3DXVector3;
var
  matRot, matRotX, matRotY, matRotZ: TD3DXMatrix;
Begin
  D3DXMatrixRotationX(matRotX, RotXYZ.x);
  D3DXMatrixRotationY(matRotY, RotXYZ.y);
  D3DXMatrixRotationZ(matRotZ, RotXYZ.z);
  D3DXMatrixMultiply(matRot, matRotZ, matRotX);
  D3DXMatrixMultiply(matRot, matRot, matRotY);

  D3DX9.D3DXVec3TransformCoord(Result, V, matRot);
end;

Function VectorDegToRad(V: TD3DXVector3): TD3DXVector3;
Begin
  Result.x := DegToRad(V.x);
  Result.y := DegToRad(V.y);
  Result.z := DegToRad(V.z);
end;

Function VectorRadToDeg(V: TD3DXVector3): TD3DXVector3;
Begin
  Result.x := RadToDeg(V.x);
  Result.y := RadToDeg(V.y);
  Result.z := RadToDeg(V.z);
end;

function BillboardLookAt(Const BillboardPosition, CameraPosition: TD3DXVector3)
  : TD3DXMatrix;
var
  vRight, vUp, vDir: TD3DXVector3;
begin
  vUp := D3DXVector3(0, 1, 0);
  vDir := Vectornormalize(Vectorsub(CameraPosition, BillboardPosition));
  vRight := Vectornormalize(Vectorcrossproduct(vUp, vDir));
  vUp := Vectornormalize(Vectorcrossproduct(vDir, vRight));

  Result := SetRotationMatrix(vDir, vRight, vUp, BillboardPosition);
end;

function BillboardLookAtY(CONST BillboardPos, BillboardNormal,
  CameraPosition: TD3DVector): TD3DXMatrix;
var
  y: single;
  V: TD3DXVector3;
begin
  V := Vectorsub(CameraPosition, BillboardPos);
  y := PhaseCoord(BillboardPos.z, BillboardPos.x, V.z, V.x);
  D3DXMatrixRotationY(Result, y);
end;

Function GetTriangleHighAtPos(CONST x, y: single;
  CONST dreieck: array of pD3DXVector3): single;
var
  a, b, C, d: single;
Begin
  a := dreieck[3]^.x;
  b := dreieck[3]^.y;
  C := dreieck[3]^.z;
  d := -(a * dreieck[0]^.x + b * dreieck[0]^.y + C * dreieck[0]^.z);

  Result := (a * x + C * y + d) / -b;
End;

function GetBillboardMatrixNEOS(BillboardPosition, CameraPosition: TD3DXVector3)
  : TD3DXMatrix;
{ a little faster were:

  FROM : http://www.team-kropf.de/neobrothers/cgi-bin/ikonboard.cgi?s=3e4cde5e36a1ffff;act=ST;f=1;t=659;hl=new
  Da man das Objekt immer zur Kamera ausrichten will, muss man so tuen, als hätte sich die Kamera nie verändert (also Nullpunkt und Richtung Z Schauen) --> IdentityMatrix. Da sich die Kamera natürlich woanders befindet, muss man die Position des Objektes noch mit der aktuellen Viewmatrix transformieren. Pseudocode würde so aussehen:

  1. Vor dem Rendern ALLER Partikel/Billboards:
  D3DDevice.SetTransform( D3DTS_VIEW, IdentityMatrix );

  2. Nun in einer Schleife für jedes Objekt:
  D3DXVec3TransformCoord( EndgültigePosition, OrginalPosition, Viewmatrix );

  3. Rendern mit Identity Worldmatrix.

  Das wars.
}
var
  matrix: TD3DXMatrix;
  Up: TD3DXVector3;
begin
  Up := D3DXVector3(0, 1, 0);
  D3DXMatrixLookAtLH(matrix, BillboardPosition, CameraPosition, Up);
  D3DXMatrixInverse(matrix, nil, matrix);

  matrix._11 := -matrix._11;
  matrix._31 := -matrix._31;
  matrix._12 := -matrix._12;
  matrix._32 := -matrix._32;
  matrix._13 := -matrix._13;
  matrix._33 := -matrix._33;

  Result := matrix;
end;

{ -----------------------------------------------------------------------------
  Procedure: ReflectRay
  Author:    pawelp
  Date:      2003-02-21 18:36
  Arguments: CONST RayStart,RayDir,HitPoint,Normal
  Result:    TD3DXVector3
  DESCRIPTION:
  THis Func Reflects a given Ray oon a plane given by HitPoint(is the point
  where the ray hits the Object) and the Normal (the normal of the hiten obj
  at the HitPoint).


  HitPoint
  --------------X-------------------
  /|\
  /  | \
  /    |  \
  /   Normal \
  X            X
  RayStart      Result

  The Result Should have the same Length as the Dif between HitPoint and RayStart
  =   VectorLength(VectorSub(HitPoint,rayStart))  =  VectorLength(VectorSub(HitPoint,Result))
  EXAMPLE:
  ----------------------------------------------------------------------------- }

Function ReflectRay(CONST RayStart, RayDir, HitPoint, Normal: TD3DXVector3)
  : TD3DXVector3;
var
  mat: TD3DXMatrix;
  plane: TD3DXPlane;
Begin
  Result := Vectornormalize(Vectorcrossproduct(RayDir, Normal));
  D3DX9.D3DXPlaneFromPoints(plane, HitPoint, Vectoradd(HitPoint, Normal),
    Vectoradd(HitPoint, Result));
  mat := D3DXMatrixIdentity;
  D3DX9.D3DXMatrixReflect(mat, plane);
  D3DX9.D3DXVec3TransformCoord(Result, RayStart, mat);
end;

{ -----------------------------------------------------------------------------
  Procedure: GetHeightAtPos
  Author:    pawelp
  Date:      2003-05-31 15:17
  Arguments: None
  Result:    None
  DESCRIPTION:
  Der Algorithmus errechnet anhand des Normalenvektors des Dreieckes und ein
  wenig Ebenenberechnung die Höhe des Dreieckes über dem Punkt.

  A, B, C, D und Hoehe sind Gleitkommazahlen, z.B. single. DreieckNormal ist der Normalenvektor des Dreieckes. Dieses bekommt man über das Kreuzprodukt heraus. Vertices[1] ist EIN beliebiger Punkt des Dreieckes. Welcher es ist, spielt bei dem Algorithmus keine Rolle.
  Point.x und Point.y sind die Koordinaten, über dem die Höhe ermittelt werden soll.


  EXAMPLE:
  ----------------------------------------------------------------------------- }

Function GetHeightAtPos(CONST Normal, AnyTrisPoint: TD3DXVector3;
  Const Point: TD3DXVector2): single;
var
  a, b, C, d: single;
Begin
  a := Normal.x;
  b := Normal.y;
  C := Normal.z;
  d := -(a * AnyTrisPoint.x + b * AnyTrisPoint.y + C * AnyTrisPoint.z);

  Result := (a * Point.x + C * Point.y + d) / -b;

end;

Function ClosestPointOnLine(Const LineStart, LineEnd, vPoint: TD3DXVector2)
  : TD3DXVector2; overload;
var
  vVector1, vVector2, vVector3: TD3DXVector2;
  t, d: single;
begin // Create the Vector from end point vA to our point vPoint.
  vVector1 := Vectorsub(vPoint, LineStart);

  // Create a normalized direction Vector from end point vA to end point vB
  vVector2 := Vectornormalize(Vectorsub(LineEnd, LineStart));

  // Use the distance formula to find the distance of the line segment (or magnitude)
  d := VectorDistance(LineStart, LineEnd);
  // Using the dot product, we project the vVector1 onto the Vector vVector2.
  // This essentially gives us the distance from our projected Vector from vA.
  t := Vectordotproduct(vVector2, vVector1);
  // If our projected distance from vA, "t", is less than or equal to 0, it must
  // be closest to the end point vA.  We want to return this end point.
  if t <= 0 then
  begin
    Result := LineStart;
    exit;
  end;
  // If our projected distance from vA, "t", is greater than or equal to the magnitude
  // or distance of the line segment, it must be closest to the end point vB.  So, return vB.
  if t >= d then
  begin
    Result := LineEnd;
    exit;
  end;
  // Here we create a Vector that is of length t and in the direction of vVector2
  vVector3 := Vectormuls(vVector2, t);
  // To find the closest point on the line segment, we just add vVector3 to the original
  // end point vA.
  Result := Vectoradd(LineStart, vVector3);
end;

function ClosestPointOnLine(Const LineStartPoint, LineEndPoint,
  vPoint: TD3DXVector3): TD3DXVector3;
var
  vVector1, vVector2, vVector3: TD3DXVector3;
  t, d: single;
begin // Create the Vector from end point vA to our point vPoint.
  vVector1 := Vectorsub(vPoint, LineStartPoint);
  // Create a normalized direction Vector from end point vA to end point vB
  vVector2 := Vectornormalize(Vectorsub(LineEndPoint, LineStartPoint));
  // Use the distance formula to find the distance of the line segment (or magnitude)
  d := VectorDistance(LineStartPoint, LineEndPoint);
  // Using the dot product, we project the vVector1 onto the Vector vVector2.
  // This essentially gives us the distance from our projected Vector from vA.
  t := Vectordotproduct(vVector2, vVector1);
  // If our projected distance from vA, "t", is less than or equal to 0, it must
  // be closest to the end point vA.  We want to return this end point.
  if t <= 0 then
  begin
    Result := LineStartPoint;
    exit;
  end;
  // If our projected distance from vA, "t", is greater than or equal to the magnitude
  // or distance of the line segment, it must be closest to the end point vB.  So, return vB.
  if t >= d then
  begin
    Result := LineEndPoint;
    exit;
  end;
  // Here we create a Vector that is of length t and in the direction of vVector2
  vVector3 := Vectormuls(vVector2, t);
  // To find the closest point on the line segment, we just add vVector3 to the original
  // end point vA.
  Result := Vectoradd(LineStartPoint, vVector3);
end;

Function VectorDistance(const p1, p2: TD3DXVector2): single;
begin
  Result := sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y));
end;

Function PointDistance(const p1, p2: Tpoint): single;
begin
  Result := sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y));
end;

function VectorDistance(Const vPoint1, vPoint2: TD3DXVector3): single;
begin // This is the classic formula used in beginning algebra to return the
  // distance between 2 points.  Since it's 3D, we just add the z dimension:
  //
  // Distance = sqrt(  (P2.x - P1.x)^2 + (P2.y - P1.y)^2 + (P2.z - P1.z)^2 )
  //
  Result := sqrt((vPoint2.x - vPoint1.x) * (vPoint2.x - vPoint1.x) +
    (vPoint2.y - vPoint1.y) * (vPoint2.y - vPoint1.y) + (vPoint2.z - vPoint1.z)
    * (vPoint2.z - vPoint1.z));
end;

function VectorDistanceSQRT(Const vPoint1, vPoint2: TD3DXVector2): single;
begin // This is the classic formula used in beginning algebra to return the
  // distance between 2 points.  Since it's 3D, we just add the z dimension:
  //
  // Distance = sqrt(  (P2.x - P1.x)^2 + (P2.y - P1.y)^2 + (P2.z - P1.z)^2 )
  //
  Result := (vPoint2.x - vPoint1.x) * (vPoint2.x - vPoint1.x) +
    (vPoint2.y - vPoint1.y) * (vPoint2.y - vPoint1.y)
end;

function VectorDistanceSQRT(Const vPoint1, vPoint2: TD3DXVector3): single;
begin // This is the classic formula used in beginning algebra to return the
  // distance between 2 points.  Since it's 3D, we just add the z dimension:
  //
  // Distance = sqrt(  (P2.x - P1.x)^2 + (P2.y - P1.y)^2 + (P2.z - P1.z)^2 )
  //
  Result := (vPoint2.x - vPoint1.x) * (vPoint2.x - vPoint1.x) +
    (vPoint2.y - vPoint1.y) * (vPoint2.y - vPoint1.y) + (vPoint2.z - vPoint1.z)
    * (vPoint2.z - vPoint1.z);
end;

(* *

  This function will return the angle between the two verctors.
  both vectors are goven in world space.

  @param   BasisPoint as a TD3DXVector3 constant
  @param   RelPoint   as a TD3DXVector3 constant

  * *)
function VectorAngleIn2D(const BasisPoint, RelPoint: TD3DXVector2): single;
var
  a, b: single;
  C, WSin: single;
begin
  a := RelPoint.y - BasisPoint.y;
  b := RelPoint.x - BasisPoint.x;

  if not Gleich0(a) and not Gleich0(b) then
  begin
    C := sqrt(sqr(a) + sqr(b));
    WSin := ArcCos(a / C);
  end
  else if Gleich0(a) then
    WSin := Pi / 2
  else
    WSin := 0;
  if b < 0 then
    WSin := 2 * Pi - WSin;

  Result := WSin;
end;

(* *

  This fnc will translate the Direction to a angle.
  the angle is in 2d.



  @param   dir as a TD3DXVector3 constant
  @return  a single

  * *)
function DirectionToAngle(const dir: TD3DXVector2): single;
begin
  Result := VectorAngleIn2D(D3DXVector2(0, 0), dir);
end;

Function VectorAngle(Const v1, v2: TD3DXVector3): single;
Begin
  Result := Math.ArcCos(CosAngle(v1, v2))
end;

Function VectorAngle(Const v1, v2: TD3DXVector2): single; overload;
begin
  try
    Result := Math.ArcCos(CosAngle(v1, v2))
  except
    Result := 0;
  end;
end;

Function DistanceToPlane(const PlanePoint, PlaneNormal,
  Point3D: TD3DXVector3): single;
var
  distance: single;
begin
  distance := -D3DXVec3Dot(PlaneNormal, PlanePoint);
  Result := D3DXVec3Dot(PlaneNormal, Point3D) + distance;
end;

Function IsInFrontOfPlane(Const PlanePoint, Normal,
  D3DPoint: TD3DXVector3): boolean;
Begin
  Result := Vectordotproduct(Normal, Vectorsub(D3DPoint, PlanePoint)) >= 0;
end;

Function GetNormalByHitPoint(Const HitPoint, posA, normA, posB, normB, posC,
  normC: TD3DXVector3): TD3DXVector3;
var
  facA, facB, facC: single;
Begin
  // Normalvektor durchschnitt
  facA := 1 / sqrt(sqr(posA.x - HitPoint.x) + sqr(posA.y - HitPoint.y) +
    sqr(posA.z - HitPoint.z));
  facB := 1 / sqrt(sqr(posB.x - HitPoint.x) + sqr(posB.y - HitPoint.y) +
    sqr(posB.z - HitPoint.z));
  facC := 1 / sqrt(sqr(posC.x - HitPoint.x) + sqr(posC.y - HitPoint.y) +
    sqr(posC.z - HitPoint.z));
  Result.x := ((normA.x * facA) + (normB.x * facB) + (normC.x * facC)) /
    (facA + facB + facC);
  Result.y := ((normA.y * facA) + (normB.y * facB) + (normC.y * facC)) /
    (facA + facB + facC);
  Result.z := ((normA.z * facA) + (normB.z * facB) + (normC.z * facC)) /
    (facA + facB + facC);
  D3DXVec3Normalize(Result, Result);
end;

function SetRotationMatrix(vecNormal, vecFront: TD3DXVector3;
  Const Position: TD3DXVector3): TD3DXMatrix;
var
  vecRight: TD3DXVector3;
begin
  D3DXVec3Normalize(vecNormal, vecNormal);
  D3DXVec3Normalize(vecFront, vecFront);

  D3DXVec3Cross(vecRight, vecNormal, vecFront);
  D3DXVec3Normalize(vecRight, vecRight);

  D3DXVec3Cross(vecFront, vecRight, vecNormal);

  Result := SetRotationMatrix(vecFront, vecRight, vecNormal, Position);
end;

Function SetRotationMatrix(Const vecFront, vecRight, vecNormal,
  vecPosition: TD3DXVector3): TD3DXMatrix;
Begin
{$IFNDEF CLOOTIE}
  Result._00 := vecRight.x;
  Result._01 := vecRight.y;
  Result._02 := vecRight.z;
  Result._03 := 0;
  Result._10 := vecNormal.x;
  Result._11 := vecNormal.y;
  Result._12 := vecNormal.z;
  Result._13 := 0;
  Result._20 := vecFront.x;
  Result._21 := vecFront.y;
  Result._22 := vecFront.z;
  Result._23 := 0;
  Result._30 := vecPosition.x;
  Result._31 := vecPosition.y;
  Result._32 := vecPosition.z;
  Result._33 := 1;
{$ELSE}
  Result._11 := vecRight.x;
  Result._12 := vecRight.y;
  Result._13 := vecRight.z;
  Result._14 := 0;
  Result._21 := vecNormal.x;
  Result._22 := vecNormal.y;
  Result._23 := vecNormal.z;
  Result._24 := 0;
  Result._31 := vecFront.x;
  Result._32 := vecFront.y;
  Result._33 := vecFront.z;
  Result._34 := 0;
  Result._41 := vecPosition.x;
  Result._42 := vecPosition.y;
  Result._43 := vecPosition.z;
  Result._44 := 1;
{$ENDIF}
end;

Procedure MakeLocal(Const globalRayStart, globalRayDir: TD3DXVector3;
  Out LocalRayStart, LocalRayDir: TD3DXVector3; Const matWorld: TD3DXMatrix);
var
  m: TD3DXMatrix;
begin
  D3DX9.D3DXMatrixInverse(m, nil, matWorld);
  D3DX9.D3DXVec3TransformCoord(LocalRayStart, globalRayStart, m);
  D3DX9.D3DXVec3TransformCoord(LocalRayDir, Vectoradd(globalRayStart,
    globalRayDir), m);
  LocalRayDir := Vectornormalize(Vectorsub(LocalRayDir, LocalRayStart));
end;

{ *
  Three control point Bezier interpolation
  mu ranges from 0 to 1, start to end of the curve
  * }

Function Bezier3Lerp(const p1, p2, p3: TD3DXVector3; mu: single): TD3DXVector3;
var
  mum1, mum12, mu2: single;
  p: TD3DXVector3;
Begin
  mu2 := mu * mu;
  mum1 := 1 - mu;
  mum12 := mum1 * mum1;
  p.x := p1.x * mum12 + 2 * p2.x * mum1 * mu + p3.x * mu2;
  p.y := p1.y * mum12 + 2 * p2.y * mum1 * mu + p3.y * mu2;
  p.z := p1.z * mum12 + 2 * p2.z * mum1 * mu + p3.z * mu2;

  Result := p;
end;

{ *
  Four control point Bezier interpolation
  mu ranges from 0 to 1, start to end of curve
  * }
function Bezier4Lerp(const p1, p2, p3, p4: TD3DXVector3; mu: single)
  : TD3DXVector3;
var
  mum1, mum13, mu3: single;
  p: TD3DXVector3;
Begin
  mum1 := 1 - mu;
  mum13 := mum1 * mum1 * mum1;
  mu3 := mu * mu * mu;

  p.x := mum13 * p1.x + 3 * mu * mum1 * mum1 * p2.x + 3 * mu * mu * mum1 * p3.x
    + mu3 * p4.x;
  p.y := mum13 * p1.y + 3 * mu * mum1 * mum1 * p2.y + 3 * mu * mu * mum1 * p3.y
    + mu3 * p4.y;
  p.z := mum13 * p1.z + 3 * mu * mum1 * mum1 * p2.z + 3 * mu * mu * mum1 * p3.z
    + mu3 * p4.z;

  Result := p;
end;

{ *
  General Bezier curve
  Number of control points is n+1
  0 <= mu < 1    IMPORTANT, the last point is not computed
  * }
function BezierLerp(const p: array of TD3DXVector3; n: integer; mu: single)
  : TD3DXVector3;
var
  k, kn, nn, nkn: integer;
  blend, muk, munk: single;
  b: TD3DXVector3;
Begin
  b := D3DXVector3(0.0, 0.0, 0.0);

  muk := 1;
  munk := power(1 - mu, n);

  for k := 0 to n - 1 do
  Begin
    nn := n;
    kn := k;
    nkn := n - k;
    blend := muk * munk;
    muk := muk * mu;
    munk := munk / (1 - mu);
    while (nn >= 1) do
    Begin
      blend := blend * nn;
      dec(nn);
      if (kn > 1) then
      Begin
        blend := blend / kn;
        dec(kn);
      end;
      if (nkn > 1) then
      Begin
        blend := blend / nkn;
        dec(nkn);
      end;
    end;
    b.x := b.x + p[k].x * blend;
    b.y := b.y + p[k].y * blend;
    b.z := b.z + p[k].z * blend;
  end;

  Result := b;
end;

Function GetRotByLookAt(const Pos, LookAtPos: TD3DXVector3): single;
begin
  Result := PhaseCoord(Pos.z, Pos.x, LookAtPos.z, LookAtPos.x);
end;

function VectorLerp(const v1, v2: TD3DXVector3; t: single): TD3DXVector3;

// calculates linear interpolation between vector1 and vector2 at point t

begin
  Result.x := Lerp(v1.x, v2.x, t);
  Result.y := Lerp(v1.y, v2.y, t);
  Result.z := Lerp(v1.z, v2.z, t);
end;

function DegToRad(Degrees: single): single;

begin
  Result := Degrees * (Pi / 180);
end;

function RadToDeg(Radians: single): single;

begin
  Result := Radians * (180 / Pi);
end;

function Lerp(const start, stop, t: single): single;
begin
  Result := start + (stop - start) * t;
end;

function DistPoint2Line(const Point, LineP1, LineP2: TD3DXVector3): single;
var
  U: TD3DXVector3;
  mu: single;
  p: TD3DXVector3;
  PQ: TD3DXVector3;
  PQCrossU: TD3DXVector3;
begin
  { Bottom of Page 715 in Swokowski Calculus 5th Edition }
  U := DirectionVector(LineP1, LineP2);
  mu := Magnitude(U);
  if (mu < 0.0000000001) then
  begin
    Result := Magnitude(DirectionVector(Point, LineP1));
  end
  else
  begin
    p := LineP1;
    PQ := DirectionVector(p, Point);
    PQCrossU := Vectorcrossproduct(PQ, U);
    try
      Result := Magnitude(PQCrossU) / mu;
    except
      Result := Magnitude(DirectionVector(Point, LineP1));
    end;
  end;
end;

function DistPoint2LineSegment(const Point, LineStart,
  LineEnd: TD3DXVector3): single;
{
  Concept
  If the angle between either endpoint and the dot is greater than 90
  then the dot is outside of the line segment. I use a test and use
  the dot product because if the cos is negative then the angle is
  greater
  than 90. The dot product will be negative if the angle is greater than
  90
  See page 703 in Swokowski Calculus 5th Edition for dot product cos
  reason
}
begin
  if Vectordotproduct(DirectionVector(LineStart, Point),
    DirectionVector(LineStart, LineEnd)) < 0 then
    Result := Magnitude(DirectionVector(LineStart, Point))
  else if Vectordotproduct(DirectionVector(LineEnd, Point),
    DirectionVector(LineEnd, LineStart)) < 0 then
    Result := Magnitude(DirectionVector(LineEnd, Point))
  else
    Result := DistPoint2Line(Point, LineStart, LineEnd);
end;

function AngleD2(x, y: single): single;
var
  q: integer;
begin
  if x = 0 then
  begin
    if y > 0 then
      Result := Pi / 2
    else
    begin
      if y = 0 then
        Result := 0
      else
        Result := 3 * Pi / 2;
    end;
    exit;
  end;
  if y = 0 then
  begin
    if x > 0 then
      Result := 0
    else
      Result := Pi;
    exit;
  end;

  if (x > 0) then
  begin
    if y >= 0 then
      q := 1
    else
      q := 4;
  end
  else
  begin
    if y < 0 then
      q := 3
    else
      q := 2;
  end;
  try
    Result := ArcTan(abs(y / x));
  except
    Result := Pi / 2;
  end;
  case q of
    2:
      Result := (Pi) - Result;
    3:
      Result := Result + Pi;
    4:
      Result := 2 * Pi - Result;
  end;
end;

function MinAngleBetweenVectors(const p, q, R: TD3DXVector3): single;
var
  CosAngle: single;
  v1, v2: TD3DXVector3;
  Mag1, Mag2: single;
  MagMult: single;
begin
  v1 := DirectionVector(p, q);
  v2 := DirectionVector(p, R);
  Mag1 := Magnitude(v1);
  Mag2 := Magnitude(v2);
  MagMult := Mag1 * Mag2;
  if (MagMult = 0) then
    Result := 0
  else
  begin
    CosAngle := Vectordotproduct(v1, v2) / MagMult;
    { Round errors }
    if CosAngle > 1 then
      CosAngle := 1
    else if CosAngle < -1 then
      CosAngle := -1;
{$IFDEF ver80}
    Result := ArcTan(sqrt(1 - sqr(CosAngle)) / CosAngle);
{$ELSE}
    Result := ArcCos(CosAngle);
{$ENDIF}
  end;
end;

function MidPoint(const p1, p2: TD3DXVector3): TD3DXVector3;
const
  CHalf: single = 0.5;
begin
  Result.x := (p1.x + p2.x) * CHalf;
  Result.y := (p1.y + p2.y) * CHalf;
  Result.z := (p1.z + p2.z) * CHalf;
end;

function TriangleArea(const a, b, BASE: single): single;
var
  s: single;
begin
  s := (a + b + BASE) * 0.5;
  Result := sqrt(s * (s - a) * (s - b) * (s - BASE));
end;

function TriangleHeight(const a, b, BASE: single): single;
begin
  Result := (TriangleArea(a, b, BASE) * 2) / BASE;
end;

function PointOnLine(const p1, p2: TD3DXVector3; const t: single): TD3DXVector3;
begin
  Result := DirectionVector(p1, p2);
  Result := MultiplyByScalar(t, Result);
  Result := Vectoradd(p1, Result);
end;

function RotatePoint(const p: TD3DXVector3; const APhi, ATheta, ATwist: integer)
  : TD3DXVector3;
var
  TempX, TempY, TempZ: single;
begin
  Result := p;
  with Result do
  begin
    { Twist, Clockwise ZSpin }
    if (ATwist > 0) then
    begin
      TempX := CosTable[ATwist] * x - SinTable[ATwist] * y;
      TempY := SinTable[ATwist] * x + CosTable[ATwist] * y;
      x := TempX;
      y := TempY;
    end;

    { Phi, top gets closer first (XRot) }
    if (APhi > 0) then
    begin
      TempZ := CosTable[APhi] * z - SinTable[APhi] * y;
      TempY := SinTable[APhi] * z + CosTable[APhi] * y;
      z := TempZ;
      y := TempY;
    end;

    { Theta, right gets closer first }
    if (ATheta > 0) then
    begin
      TempX := CosTable[ATheta] * x - SinTable[ATheta] * z;
      TempZ := SinTable[ATheta] * x + CosTable[ATheta] * z;
      x := TempX;
      z := TempZ;
    end;
  end;
end;

procedure InitCosSinTables;
var
  i: integer;
begin
  for i := 0 to 360 do
  begin
    SinTable[i] := sin((i / 180) * Pi);
    CosTable[i] := cos((i / 180) * Pi);
  end;
end;

function DirectionVector(const p1, p2: TD3DXVector3): TD3DXVector3;
begin
  Result := Vectorsub(p2, p1);
end;

function Vectormuls(CONST v1: TD3DXVector2; s: single): TD3DXVector2;
begin
  Result.x := v1.x * s;
  Result.y := v1.y * s;
end;

function Vectormuls(CONST v1: TD3DXVector3; s: single): TD3DXVector3;
begin
  Result.x := v1.x * s;
  Result.y := v1.y * s;
  Result.z := v1.z * s;
end;

Procedure DoInit;
begin
  ZeroMemory(@ZeroMatrix, SizeOf(ZeroMatrix));
  InitCosSinTables;

{$IFNDEF CLOOTIE}
  D3DXMatrixIdentity := D3DX9.D3DXMatrixIdentity;
{$ELSE}
  D3DX9.D3DXMatrixIdentity(D3DXMatrixIdentity);
{$ENDIF}
end;

function GleichEps(s1, s2, e: single): boolean;
begin
  Result := abs(s1 - s2) < e;
end;

function Gleich(s1, s2: single): boolean;
begin
  Result := GleichEps(s1, s2, Eps);
end;

Procedure VectorAngles(CONST BASE, Relative: TD3DXVector3;
  Out Angle: TD3DXVector3);
Begin
  WITH Angle DO
  Begin
    x := PhaseCoord(BASE.y, BASE.z, Relative.y, Relative.z);
    y := PhaseCoord(BASE.x, BASE.z, Relative.x, Relative.z);
    z := PhaseCoord(BASE.y, BASE.x, Relative.y, Relative.x);
  end;
end;

function Magnitude(const V: TD3DXVector3): single;
begin
  Result := sqrt((V.x * V.x) + (V.y * V.y) + (V.z * V.z));
end;

function MultiplyByScalar(const C: single; const V: TD3DXVector3): TD3DXVector3;
begin
  Result.x := V.x * C;
  Result.y := V.y * C;
  Result.z := V.z * C;
end;

Function Intersect(const Seg1Start, Seg1End, Seg2Start, Seg2End: TD3DXVector2)
  : boolean; Overload;
Begin
  Result := Intersect(Seg1Start.x, Seg1Start.y, Seg1End.x, Seg1End.y,
    Seg2Start.x, Seg2Start.y, Seg2End.x, Seg2End.y);
End;

Function Intersect(x1, y1, x2, y2, x3, y3, x4, y4: single): boolean;
Begin
  Result := (Orientation(x1, y1, x2, y2, x3, y3) <> Orientation(x1, y1, x2, y2,
    x4, y4)) And

    (Orientation(x3, y3, x4, y4, x1, y1) <> Orientation(x3, y3, x4, y4,
    x2, y2));
End;

Function Orientation(x1, y1, x2, y2, Px, Py: single): integer;
Var
  Orin: single;
Begin
  // from proc PointinNConvexPoly
  Orin := (y1 * x2) - (x1 * y2) - (Py * x2) + (Px * y2) + (Py * x1) - (Px * y1);
  (* Linear determinant of the 3 points *)
  // Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);

  If Orin > 0.0 Then
    Result := orientation_LeftHand
  Else If Orin < 0.0 Then
    Result := orientation_RightHand
  Else
    Result := 0; (* Orientaion is neutral if result is 0 *)
End;

Function Orientation(const LineStart, LineEnd, pt: TD3DXVector2)
  : integer; Overload;
Begin
  Result := Orientation(LineStart.x, LineStart.y, LineEnd.x, LineEnd.y,
    pt.x, pt.y);
End;

Function Orientation(const LineStart, LineEnd, pt: Tpoint): integer;
Begin
  Result := Orientation(LineStart.x, LineStart.y, LineEnd.x, LineEnd.y,
    pt.x, pt.y);
End;

Function VertexFullAngle(x1, y1, x2, y2, x3, y3: single): single;
var
  side: integer;
begin
  Result := VertexAngle(x1, y1, x2, y2, x3, y3);
  side := Orientation(x2, y2, x1, y1, x3, y3);
  if side = orientation_LeftHand then
    Result := 180 - Result;
end;

Function VertexAngle(x1, y1, x2, y2, x3, y3: single): single;
Var
  Dist: single;
Begin
  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;

  Result := RadToDeg(VectorAngle(D3DXVector2(x1, y1), D3DXVector2(x3, y3)));
End;

Function VertexAngle(const Pnt1, Pnt2, Pnt3: TD3DXVector2): single;
Begin
  Result := VertexAngle(Pnt1.x, Pnt1.y, Pnt2.x, Pnt2.y, Pnt3.x, Pnt3.y);
End;

Function IsEqual(Val1, Val2: single): boolean;
Const
  Epsilon = 1.0E-6;
Var
  Delta: single;
Begin
  Delta := abs(Val1 - Val2);
  Result := (Delta <= Epsilon);
End;

Function Rotate(const RotAng: double; const Pnt: TD3DXVector2): TD3DXVector2;
Begin
  Rotate(RotAng, Pnt.x, Pnt.y, Result.x, Result.y);
End;

Procedure Rotate(RotAng: double; x, y: single; out Nx, Ny: single);
Var
  SinVal: double;
  CosVal: double;
Begin
  RotAng := RotAng * PIDiv180;
  SinVal := sin(RotAng);
  CosVal := cos(RotAng);
  Nx := x * CosVal - y * SinVal;
  Ny := y * CosVal + x * SinVal;
End;

function D3DXVector3(x, y, z: single): TD3DXVector3;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function D3DXVector2(x, y: single): TD3DXVector2;
begin
  Result.x := x;
  Result.y := y;
end;

INITIALIZATION

DoInit

end.
