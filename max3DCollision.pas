unit max3DCollision;

{ *******************************************************

  Digital Omerta 3D Engine

  Unit Author:   Pawel Piotrowski
  Copyright 	    Pawel Piotrowski
  Version: 2
  History:
  2013-10-09:
  - The Point in non convex polygon function is improved and faster

  ToDo:
  check
  Function D3DBoxInBox(const rkBox0, rkBox1 : TOBBox):Boolean;OverLoad;
  the optimalization with sphere checks is currently disabled

  ******************************************************* }

interface

{$I JEDI.INC}
{$DEFINE CLOOTIE}

uses
  Direct3D9, D3DX9, windows, max3DMath, sysUtils;

const
  //
  METER = 100;
  D3DFVF_BoundingBox = D3DFVF_XYZ or D3DFVF_DIFFUSE;
  DLLNAme = 'DoBasics.dll';

type
  pD3DXVector3 = ^TD3DXVector3;

  Triangle3 = record
    // Triangle points are tri(s,t) = b+s*e0+t*e1 where 0 <= s <= 1,
    // 0 <= t <= 1, and 0 <= s+t <= 1.

    // b - Origin
    // e0 - Edge0
    // e1 - Edge1
    // s - pfSParam (see func Distance)
    // t - pfTParam (see func Distance)
    Origin, Edge0, Edge1: TD3DXVector3;
  end;

  TPlane = record
  public
    distance: single;
    Normal: TD3DXVector3;
    procedure Construct(Const p0, p1, p2: TD3DXVector3); Overload;
    Procedure Construct(Const aNormal, aPoint: TD3DXVector3); Overload;
    procedure Normalize;
    function DistanceToPoint(pt: TD3DXVector3): single;
  end;

  TBoundingBoxVERTEX = record
    point: TD3DXVector3;
    color: TD3DColor; // Farbe
  end;

  pOBBox = ^TOBBox;

  TOBBox = record
  private

    FRadiusSphere: single;

    Procedure UpdateRadiusSphere;
    procedure SetRadiusDir(const Value: single);
    procedure SetRadiusRight(const Value: single);
    procedure SetRadiusUp(const Value: single);
    procedure CheckMinRadii;
  public
    Center: TD3DXVector3;
    AchsenV3: array [0 .. 2] of TD3DXVector3;
    Radii: array [0 .. 2] of single;

    Property vRight: TD3DXVector3 read AchsenV3[0] write AchsenV3[0];
    Property vUp: TD3DXVector3 read AchsenV3[1] write AchsenV3[1];
    Property vDir: TD3DXVector3 read AchsenV3[2] write AchsenV3[2];

    Property RadiusRight: single read Radii[0] write SetRadiusRight;
    Property RadiusUp: single read Radii[1] write SetRadiusUp;
    Property RadiusDir: single read Radii[2] write SetRadiusDir;

    Property RadiusSphere: single read FRadiusSphere;

    Procedure CopyFrom(Const Box: TOBBox);
    Procedure CreateFromMinMax(const Min, Max: TD3DXVector3);
    Procedure Init(const _Center: TD3DXVector3;
      const _Achsen: array of TD3DXVector3; const _Radii: array of single);
    Procedure Transform(Const Src: TOBBox; const Matrix: TD3DXMatrix);
    procedure TransformAndRecalc(const SrcBox: TOBBox;
      const Matrix: TD3DXMatrix);
    procedure GetEdges(var edge: array of TD3DXVector3);
  end;

type
  TOBBoxVertieces = Array [0 .. 7] of TD3DXVector3;

type
  pD3DBox8 = ^TD3DBox8;

  TD3DBox8 = record
    point: array [1 .. 8] of TD3DVector;
  end;

  TD3DBox12 = record
    point: array [1 .. 12] of TD3DVector;
  end;

  // LINE : Endlesss, 2 Points do define it
  TLine3 = packed Record
    Origin: TD3DXVector3;
    Direction: TD3DXVector3;
  end;

  // RAY : Have an Start Point and an Direction
  // Ray := p+t*d
  // where t >= 0
  TRay3 = packed Record
    Origin: TD3DXVector3;
    Direction: TD3DXVector3;
  end;

  // SEGMENT : Have and Start and an end point
  // StartPoint = Origion
  // Endpoint = Origion + Direction
  // Direction can be a not unit vector.
  // in some funcs a t is returned
  // endPoint := p+t*d
  // where 0 <= t >= 1
  TSegment3 = packed Record
    Origin: TD3DXVector3;
    Direction: TD3DXVector3;
  end;

  // Ist der Punkt in der Kugel ?
function D3DPunktinKugel(CONST p: TD3DVector; CONST M: TD3DVector;
  r: single): boolean;
function D3DPointInCircle(CONST p: TD3DXVector3; CONST M: TD3DXVector3;
  r: single): boolean;
function D2DPointInCircle(CONST p: TD3DXVector2; CONST M: TD3DXVector2;
  r: single): boolean;

// berührt die Kugel die Kugel ?
function D3DKugelinKugel(CONST M1, M2: TD3DVector; r1, r2: single): boolean;

// Ist der Punkt in der Ellipse (x,y,z=radien der Elips)
function D3DPunktinEllipse(CONST p: TD3DVector; x, y, z: single;
  CONST matWorld: TD3DXMatrix): boolean;

// Ist der Punkt in der Box ?
function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TD3DBox8)
  : boolean; overload;
function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TD3DBox12)
  : boolean; overload;
function D3DPunktinBox(CONST p: TD3DVector; CONST Min, Max: TD3DVector)
  : boolean; overload;
function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TOBBox)
  : boolean; overload;

// Ist die Box in der Box
function D3DBoxinBox(CONST box1, box2: TD3DBox8): boolean; overload;
function D3DBoxinBox(CONST box1, box2: TD3DBox12): boolean; overload;
function D3DBoxinBox(CONST Min1, Max1, Min2, Max2: TD3DVector)
  : boolean; overload;

// Min-Max-Werte einer Box in TD3DBox8 umwandeln wie von den anderen Routinen benötigt
function MinMaxToBox8(CONST Min, Max: TD3DVector): TD3DBox8; overload;
function MinMaxToBox8(CONST Min, Max: TD3DVector; CONST M: TD3DXMatrix)
  : TD3DBox8; overload;
function MinMaxToBox12(CONST Min, Max: TD3DVector): TD3DBox12; overload;
function MinMaxToBox12(CONST Min, Max: TD3DVector; CONST M: TD3DXMatrix)
  : TD3DBox12; overload;

// is the D3DPoint in Rect (x,z)?
function D3DPointinRect(CONST p: TD3DVector; CONST Rect: TRect): boolean;
function D3DBoxinRect(CONST Box: TD3DBox8; CONST Rect: TRect): boolean;

function PointInRect(const point: TPoint; const Rect: TRect): boolean;
// checks if the rect 1 is completly within rect 2
function RectInRect(const Rect1, Rect2: TRect): boolean;
Function RectIntersection(const r1, r2: TRect): boolean;



// Ray Functions

procedure GetRay(CONST _matProj, _matView: TD3DXMatrix; CONST _cursor: TPoint;
  DWIDTH, DHEIGHT: integer; var _rayDir, _rayOrig: TD3DVector); overload;
procedure GetRay(CONST _matProj, _matView: TD3DXMatrix;
  CONST _cursor: TD3DXVector2; DWIDTH, DHEIGHT: integer;
  var _rayDir, _rayOrig: TD3DVector); overload;

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox8): boolean; overload;
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox12): boolean; overload;
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox8; Out HitPoint, Normal: TD3DXVector3): boolean; overload;
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox12; Out HitPoint, Normal: TD3DXVector3): boolean; overload;

// other
function D3DSqrtDistance(CONST v1, v2: TD3DVector): single;
function D3DRealDistance(CONST v1, v2: TD3DVector): single;

// 2D Poly Funcs, all of them Igones the Y parameter
Function LineThruPolygon(LineFirstPoint, LineSecondPoint: TD3DXVector3;
  Const PollyArray: Array of TD3DXVector3; PolyCount: integer): boolean;
Function PointInPolygon(Const CheckPoint: TD3DXVector3;
  CONST PollyArray: Array of TD3DXVector3; PollyArrayLen: integer): boolean;
Function PolygonInInPolygon(Const PollyArray1, PollyArray2
  : Array of TD3DXVector3; PollyArray1Len, PollyArray2Len: integer): boolean;
Function PolygonInInRect(Const PollyArray: Array of TD3DXVector3;
  PollyArrayLen: integer; Const Rect: TRect): boolean;

// Triangle Functions
Function distance(const rkPoint: TD3DXVector3; const rkTri: Triangle3;
  pfSParam, pfTParam: pSingle): single; overload;
Function SqrDistance(const rkPoint: TD3DXVector3; const rkTri: Triangle3;
  pfSParam, pfTParam: pSingle): single; overload;

// some MISC func to Create Visual Bounding Boxes
Function CrateBoundingBoxForPoint(CONST point: TD3DXVector3; Size: single)
  : TD3DBox8;
Procedure ComputeVerticesForBoundingBox(CONST point: TD3DXVector3; Size: single;
  VAR BoundingBoxVertices: array of TBoundingBoxVERTEX;
  color: DWORD = $FFFFFFFF); overload;
Procedure ComputeVerticesForBoundingBox(CONST Box: TD3DBox8;
  VAR BoundingBoxVertices: array of TBoundingBoxVERTEX; color: DWORD); overload;
// intersection with a triangle
Function GetIntersectPoint(CONST RayStart, RayDir, v1, v2, V3: TD3DXVector3;
  VAR HitPoint: TD3DXVector3): boolean;

function RayCastPlaneIntersect(const RayStart, rayVector: TD3DXVector3;
  const planePoint, planeNormal: TD3DXVector3;
  intersectPoint: pD3DXVector3 = nil): boolean;

Function GetBoxCenter(CONST Box: TD3DBox8): TD3DXVector3;
function SphaereInBox(CONST SphereCenter: TD3DVector; SphereRadius: single;
  CONST Box: TD3DBox8): boolean; overload;
function SphaereInBox(CONST SphereCenter: TD3DVector; SphereRadius: single;
  CONST Box: TD3DBox12): boolean; overload;

// OOBs
function GetOBBVertieces(const Box: TOBBox): TOBBoxVertieces;
Procedure OBBToBox8(CONST OBB: TOBBox; Out Box8: TD3DBox8);

Function D3DBoxinBox(const rkBox0, rkBox1: TOBBox): boolean; OverLoad;
Function TestBoxSphrIntersection(const rkBox: TOBBox;
  const rkCen1: TD3DXVector3; const rfRadius1: single): boolean;

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TOBBox; Out HitPoint, Normal: TD3DXVector3): boolean; overload;
Procedure GetMinMaxFromOBBox(Out Min, Max: TD3DXVector3;
  Const Box: TOBBox); overload;
Procedure GetMinMaxFromOBBox(Out Min, Max: TD3DXVector3; Const Box: TOBBox;
  Const Matrix: TD3DXMatrix); overload;
Procedure GetRelativPointPos(CONST point: TD3DXVector3; CONST Box: TOBBox;
  VAR InFront, Right: boolean);
Procedure TransformMinMax(Var Min, Max: TD3DXVector3; Matrix: TD3DXMatrix);

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DXVector3;
  CONST Box: TOBBox; Out distance: single): boolean; overload;
function math3dLineIntersectOBB(CONST RayPos, RayDir: TD3DXVector3;
  fLaenge: single; CONST Box: TOBBox): boolean;
function IntersectBoundingBoxOld(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TOBBox): boolean; overload;

// This function cames from GamaSutra, but is a bit slower and not so precise as the other
// one (D3DBoxinBox)
Function OBBOverlap(CONST BoxA, BoxB: TOBBox): boolean;

// tests if a triangle intersects with the Orientated boundingBox
Function IntersectionTriBox(const apkTri: Array of TD3DXVector3;
  const rkBox: TOBBox): boolean;
// this is a func used by IntersectionTriBox
Procedure ProjectTriangle(const rkD: TD3DXVector3;
  const apkTri: array of TD3DXVector3; VAR rfMin, rfMax: single);
// this is a func used by IntersectionTriBox
Procedure ProjectBox(const rkD: TD3DXVector3; const rkBox: TOBBox;
  VAR rfMin, rfMax: single);

Function PointinNConvexPoly(Const aPoint: TPoint; Const Poly: array of TPoint;
  PolyMinMax: pRect): boolean;

Function InOnRight(Const point, LaneStart, LaneEnd: TD3DXVector3): boolean;


// -----------------------------------
// Line To Line DIst
// -----------------------------------

Function SqrDistance(Const rkLine0, rkLine1: TLine3; pfLinP0, pfLinP1: pSingle)
  : single; overload;
Function SqrDistance(const rkSeg0, rkSeg1: TSegment3; pfSegP0, pfSegP1: pSingle)
  : single; overload;

// -----------------------------------
// D3D Line Dist To Box
// -----------------------------------

Function SqrDistance(const rkPoint: TD3DXVector3; const rkBox: TOBBox;
  pfBParam0, pfBParam1, pfBParam2: pSingle): single; OVERLOAD;

Function distance(const rkPoint: TD3DXVector3; const rkBox: TOBBox;
  pfBParam0, pfBParam1, pfBParam2: pSingle): single; OVERLOAD;

Function SqrDistance(const rkPoint: TD3DXVector3;
  fXMin, fXMax, fYMin, fYMax, fZMin, fZMax: single; pkClosest: pD3DXVector3)
  : single; OVERLOAD;

Function distance(const rkPoint: TD3DXVector3; fXMin, fXMax, fYMin, fYMax,
  fZMin, fZMax: single; pkClosest: pD3DXVector3): single; OVERLOAD;

Function TestIntersection(const rkPlane: TPlane; const rkBox: TOBBox)
  : boolean; overload;
function TestIntersection(const akU, akV: array of TD3DXVector3)
  : boolean; overload;
Function Culled(const rkPlane: TPlane; const rkBox: TOBBox): boolean;
Function BoxInBounds(Const Box: TOBBox; Bounds: array of TPlane;
  BoundsCount: integer): boolean;

implementation

uses
  Math;

function D3DPunktinKugel(CONST p: TD3DVector; CONST M: TD3DVector;
  r: single): boolean;
begin
  Result := (sqr(p.x - M.x) + sqr(p.y - M.y) + sqr(p.z - M.z)) < sqr(r);
end;

function D3DPointInCircle(CONST p: TD3DXVector3; CONST M: TD3DXVector3;
  r: single): boolean;
begin
  Result := (sqr(p.x - M.x) + sqr(p.y - M.y)) < sqr(r);
end;

function D2DPointInCircle(CONST p: TD3DXVector2; CONST M: TD3DXVector2;
  r: single): boolean;
begin
  Result := (sqr(p.x - M.x) + sqr(p.y - M.y)) < sqr(r);
end;

function D3DKugelinKugel(CONST M1, M2: TD3DVector; r1, r2: single): boolean;
begin
  Result := (sqr(M1.x - M2.x) + sqr(M1.y - M2.y) + sqr(M1.z - M2.z)) <
    sqr(r1 + r2);
end;

function D3DPunktinEllipse(CONST p: TD3DVector; x, y, z: single;
  CONST matWorld: TD3DXMatrix): boolean;
var
  matScale: TD3DXMatrix;
  newposition: TD3DVector;
begin
  D3DXMatrixScaling(matScale, x, y, z);
  D3DXMatrixMultiply(matScale, matScale, matWorld);
  D3DXMatrixInverse(matScale, nil, matScale);
  D3DXVec3TransFormCoord(newposition, p, matScale);
  Result := sqr(newposition.x) + sqr(newposition.y) + sqr(newposition.z) < 1;
end;

function D3DPunktinBox(CONST p: TD3DVector; CONST Min, Max: TD3DVector)
  : boolean;
begin
  Result := D3DPunktinBox(p, MinMaxToBox8(Min, Max));
end;

function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TD3DBox8): boolean;
var
  Normal: TD3DVector;
begin
  // Sobald der Punkt auch nur bei einer Ebene außerhalb ist findet
  // KEINE Kollision statt
  Result := False;

  // Plane unten
  Normal := vectorcrossproduct(VectorSub(Box.point[2], Box.point[1]),
    VectorSub(Box.point[3], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  // Plane oben
  Normal := vectorcrossproduct(VectorSub(Box.point[7], Box.point[5]),
    VectorSub(Box.point[6], Box.point[5]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[5])) >= 0 then
    exit;

  // Plane vorne
  Normal := vectorcrossproduct(VectorSub(Box.point[7], Box.point[3]),
    VectorSub(Box.point[4], Box.point[3]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[3])) >= 0 then
    exit;

  // Plane hinten
  Normal := vectorcrossproduct(VectorSub(Box.point[5], Box.point[1]),
    VectorSub(Box.point[2], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  // Plane rechts
  Normal := vectorcrossproduct(VectorSub(Box.point[6], Box.point[2]),
    VectorSub(Box.point[3], Box.point[2]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[2])) >= 0 then
    exit;

  // Plane links
  Normal := vectorcrossproduct(VectorSub(Box.point[4], Box.point[1]),
    VectorSub(Box.point[5], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  Result := True;
end;

function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TD3DBox12): boolean;
var
  Normal: TD3DVector;
begin
  // Sobald der Punkt auch nur bei einer Ebene außerhalb ist findet
  // KEINE Kollision statt
  Result := False;

  // Plane unten
  Normal := vectorcrossproduct(VectorSub(Box.point[2], Box.point[1]),
    VectorSub(Box.point[3], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  // Plane oben
  Normal := vectorcrossproduct(VectorSub(Box.point[7], Box.point[5]),
    VectorSub(Box.point[6], Box.point[5]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[5])) >= 0 then
    exit;

  // Plane vorne
  Normal := vectorcrossproduct(VectorSub(Box.point[7], Box.point[3]),
    VectorSub(Box.point[4], Box.point[3]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[3])) >= 0 then
    exit;

  // Plane hinten
  Normal := vectorcrossproduct(VectorSub(Box.point[5], Box.point[1]),
    VectorSub(Box.point[2], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  // Plane rechts
  Normal := vectorcrossproduct(VectorSub(Box.point[6], Box.point[2]),
    VectorSub(Box.point[3], Box.point[2]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[2])) >= 0 then
    exit;

  // Plane links
  Normal := vectorcrossproduct(VectorSub(Box.point[4], Box.point[1]),
    VectorSub(Box.point[5], Box.point[1]));
  if vectordotproduct(Normal, VectorSub(p, Box.point[1])) >= 0 then
    exit;

  Result := True;
end;

function D3DBoxinBox(CONST Min1, Max1, Min2, Max2: TD3DVector): boolean;
var
  box1, box2: TD3DBox8;
begin
  box1 := MinMaxToBox8(Min1, Max1);
  box2 := MinMaxToBox8(Min2, Max2);
  Result := D3DBoxinBox(box1, box2);
end;

function D3DBoxinBox(CONST box1, box2: TD3DBox8): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to 8 do // Alle Punkte von Box1 mit Box2 testen
    if D3DPunktinBox(box1.point[i], box2) then
      exit;
  for i := 1 to 8 do
    // Wenn noch keine Collision, dann auch Box2 mit Box1 testen
    if D3DPunktinBox(box2.point[i], box1) then
      exit;
  Result := False;
end;

function D3DBoxinBox(CONST box1, box2: TD3DBox12): boolean; overload;
var
  i: integer;
  Center: TD3DXVector3;
begin
  Result := True;
  for i := 1 to 12 do // Alle Punkte von Box1 mit Box2 testen
    if D3DPunktinBox(box1.point[i], box2) then
      exit;

  for i := 1 to 12 do
    // Wenn noch keine Collision, dann auch Box2 mit Box1 testen
    if D3DPunktinBox(box2.point[i], box1) then
      exit;

  // check the Center
  Center := box1.point[1];
  For i := 2 To 8 Do
    Center := VectorAdd(box1.point[i], Center);
  Center := VectorDivs(Center, 8);
  if D3DPunktinBox(Center, box2) then
    exit;

  Result := False;
end;

function MinMaxToBox8(CONST Min, Max: TD3DVector; CONST M: TD3DXMatrix)
  : TD3DBox8;
var
  i: integer;
begin
  Result := MinMaxToBox8(Min, Max);
  for i := low(Result.point) to high(Result.point) do
    D3DXVec3TransFormCoord(Result.point[i], Result.point[i], M);
end;

function MinMaxToBox8(CONST Min, Max: TD3DVector): TD3DBox8;
begin
  fillchar(Result, sizeof(Result), 0);
  Result.point[1] := D3DXVector3(Min.x, Min.y, Min.z);
  Result.point[2] := D3DXVector3(Max.x, Min.y, Min.z);
  Result.point[3] := D3DXVector3(Max.x, Min.y, Max.z);
  Result.point[4] := D3DXVector3(Min.x, Min.y, Max.z);
  Result.point[5] := D3DXVector3(Min.x, Max.y, Min.z);
  Result.point[6] := D3DXVector3(Max.x, Max.y, Min.z);
  Result.point[7] := D3DXVector3(Max.x, Max.y, Max.z);
  Result.point[8] := D3DXVector3(Min.x, Max.y, Max.z);
end;

function MinMaxToBox12(CONST Min, Max: TD3DVector): TD3DBox12; overload;
begin
  fillchar(Result, sizeof(Result), 0);
  Result.point[1] := D3DXVector3(Min.x, Min.y, Min.z);
  Result.point[2] := D3DXVector3(Max.x, Min.y, Min.z);
  Result.point[3] := D3DXVector3(Max.x, Min.y, Max.z);
  Result.point[4] := D3DXVector3(Min.x, Min.y, Max.z);
  Result.point[5] := D3DXVector3(Min.x, Max.y, Min.z);
  Result.point[6] := D3DXVector3(Max.x, Max.y, Min.z);
  Result.point[7] := D3DXVector3(Max.x, Max.y, Max.z);
  Result.point[8] := D3DXVector3(Min.x, Max.y, Max.z);
  Result.point[9] := D3DXVector3((Min.x + Max.x) / 2,
    (Min.y + Max.y) / 2, Min.z);
  // vorne
  Result.point[10] := D3DXVector3(Max.x, (Min.y + Max.y) / 2,
    (Min.z + Max.z) / 2);
  // rechts
  Result.point[11] := D3DXVector3((Min.x + Max.x) / 2,
    (Min.y + Max.y) / 2, Max.z);
  // hinten
  Result.point[12] := D3DXVector3(Min.x, (Min.y + Max.y) / 2,
    (Min.z + Max.z) / 2);
  // hinten
end;

function MinMaxToBox12(CONST Min, Max: TD3DVector; CONST M: TD3DXMatrix)
  : TD3DBox12;
var
  i: integer;
begin
  Result := MinMaxToBox12(Min, Max);
  for i := low(Result.point) to high(Result.point) do
    D3DXVec3TransFormCoord(Result.point[i], Result.point[i], M);
end;

function D3DPointinRect(CONST p: TD3DVector; CONST Rect: TRect): boolean;
begin
  if (p.x > Rect.Left) and (p.x < Rect.Right) and (p.z > Rect.Bottom) and
    (p.z < Rect.Top) then
    Result := True
  else
    Result := False;
end;

function D3DBoxinRect(CONST Box: TD3DBox8; CONST Rect: TRect): boolean;
var
  box2: TD3DBox8;
begin
  { DONE -opawelp -coptimalization :
    D3DBoxinRect Need a lot of optimalization. }
  box2 := MinMaxToBox8(D3DXVector3(Rect.Left, -9999999999, Rect.Bottom),
    D3DXVector3(Rect.Right, $FFFFFF, Rect.Top));
  Result := D3DBoxinBox(Box, box2);
end;

procedure GetRay(CONST _matProj, _matView: TD3DXMatrix; CONST _cursor: TPoint;
  DWIDTH, DHEIGHT: integer; var _rayDir, _rayOrig: TD3DVector);
begin
  GetRay(_matProj, _matView, D3DXVector2(_cursor.x, _cursor.y), DWIDTH, DHEIGHT,
    _rayDir, _rayOrig);
end;

procedure GetRay(CONST _matProj, _matView: TD3DXMatrix;
  CONST _cursor: TD3DXVector2; DWIDTH, DHEIGHT: integer;
  var _rayDir, _rayOrig: TD3DVector);
var
  _m: TD3DXMatrix;
  _v: TD3DVector;
begin
  // Compute the vector of the pick ray in screen space
{$IFNDEF CLOOTIE}
  _v.x := (((2.0 * _cursor.x) / DWIDTH) - 1) / _matProj._00;
  _v.y := -(((2.0 * _cursor.y) / DHEIGHT) - 1) / _matProj._11;
  _v.z := 1.0;
{$ELSE}
  _v.x := (((2.0 * _cursor.x) / DWIDTH) - 1) / _matProj._11;
  _v.y := -(((2.0 * _cursor.y) / DHEIGHT) - 1) / _matProj._22;
  _v.z := 1.0;
{$ENDIF}
  // Get the inverse view matrix
  D3DXMatrixInverse(_m, nil, _matView);

  // Transform the screen space pick ray into 3D space
{$IFNDEF CLOOTIE}
  _rayDir.x := _v.x * _m._00 + _v.y * _m._10 + _v.z * _m._20;
  _rayDir.y := _v.x * _m._01 + _v.y * _m._11 + _v.z * _m._21;
  _rayDir.z := _v.x * _m._02 + _v.y * _m._12 + _v.z * _m._22;
  _rayOrig.x := _m._30;
  _rayOrig.y := _m._31;
  _rayOrig.z := _m._32;
{$ELSE}
  _rayDir.x := _v.x * _m._11 + _v.y * _m._21 + _v.z * _m._31;
  _rayDir.y := _v.x * _m._12 + _v.y * _m._22 + _v.z * _m._32;
  _rayDir.z := _v.x * _m._13 + _v.y * _m._23 + _v.z * _m._33;
  _rayOrig.x := _m._41;
  _rayOrig.y := _m._42;
  _rayOrig.z := _m._43;
{$ENDIF}
end;

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox8): boolean;
var
  U, V, Dist: single;
begin
  IF
  // Front
    D3DX9.D3DXIntersectTri(Box.point[1], Box.point[6], Box.point[2], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[1], Box.point[5],
    Box.point[6], RayPos, RayDir, U, V, Dist) OR
  // Right
    D3DX9.D3DXIntersectTri(Box.point[2], Box.point[7], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[2], Box.point[6],
    Box.point[7], RayPos, RayDir, U, V, Dist) OR
  // Left
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[5], Box.point[1], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[8],
    Box.point[5], RayPos, RayDir, U, V, Dist) OR
  // back
    D3DX9.D3DXIntersectTri(Box.point[3], Box.point[8], Box.point[4], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[3], Box.point[7],
    Box.point[8], RayPos, RayDir, U, V, Dist) OR
  // Top
    D3DX9.D3DXIntersectTri(Box.point[5], Box.point[7], Box.point[6], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[5], Box.point[8],
    Box.point[7], RayPos, RayDir, U, V, Dist) OR
  // Bottom
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[2], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[1],
    Box.point[2], RayPos, RayDir, U, V, Dist) Then
    Result := True
  else
    Result := False;
end;

{ -----------------------------------------------------------------------------
  Procedure: IntersectBoundingBox
  Author:    pawelp
  Date:      2003-02-24 16:32
  Arguments: CONST RayPos,RayDir: TD3DVector;CONST Box: TD3DBox12
  Result:    boolean
  DESCRIPTION:
  is Exact the same as the Box8 Version of this proc, because the 4 additional
  points in a D3DBox12 are just for more precisioned Collisions with other Boxes
  EXAMPLE:
  ----------------------------------------------------------------------------- }

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox12): boolean;
var
  U, V, Dist: single;
begin
  IF
  // Front
    D3DX9.D3DXIntersectTri(Box.point[1], Box.point[6], Box.point[2], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[1], Box.point[5],
    Box.point[6], RayPos, RayDir, U, V, Dist) OR
  // Right
    D3DX9.D3DXIntersectTri(Box.point[2], Box.point[7], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[2], Box.point[6],
    Box.point[7], RayPos, RayDir, U, V, Dist) OR
  // Left
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[5], Box.point[1], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[8],
    Box.point[5], RayPos, RayDir, U, V, Dist) OR
  // back
    D3DX9.D3DXIntersectTri(Box.point[3], Box.point[8], Box.point[4], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[3], Box.point[7],
    Box.point[8], RayPos, RayDir, U, V, Dist) OR
  // Top
    D3DX9.D3DXIntersectTri(Box.point[5], Box.point[7], Box.point[6], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[5], Box.point[8],
    Box.point[7], RayPos, RayDir, U, V, Dist) OR
  // Bottom
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[2], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[1],
    Box.point[2], RayPos, RayDir, U, V, Dist) Then
    Result := True
  else
    Result := False;
end;

// see also the D3DBox12 Version of theis proc
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox8; Out HitPoint, Normal: TD3DXVector3): boolean;
var
  U, V, Dist: single;
  d1: single;
  v1, v2, V3, v4, v5: TD3DXVector3;
begin
  Result := False;
  d1 := D3DX9.D3DX_DEFAULT_FLOAT;

  IF
  // Front
    D3DX9.D3DXIntersectTri(Box.point[1], Box.point[6], Box.point[2], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[1], Box.point[5],
    Box.point[6], RayPos, RayDir, U, V, Dist) then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := CalcNormalCCW(Box.point[1], Box.point[5], Box.point[6]);
      Result := True;
      d1 := Dist;
    end;
  IF
  // Right
    D3DX9.D3DXIntersectTri(Box.point[2], Box.point[7], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[2], Box.point[6],
    Box.point[7], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[2], Box.point[1]));
      Result := True;
      d1 := Dist;
    end;
  IF
  // Left
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[5], Box.point[1], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[8],
    Box.point[5], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[1], Box.point[2]));
      d1 := Dist;
      Result := True;
    end;
  IF
  // back
    D3DX9.D3DXIntersectTri(Box.point[3], Box.point[8], Box.point[4], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[3], Box.point[7],
    Box.point[8], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[4], Box.point[1]));
      v1 := CalcNormalCCW(Box.point[3], Box.point[8], Box.point[4]);
      d1 := Dist;
      Result := True;
    end;
  IF
  // Top
    D3DX9.D3DXIntersectTri(Box.point[5], Box.point[7], Box.point[6], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[5], Box.point[8],
    Box.point[7], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := CalcNormalCCW(Box.point[5], Box.point[8], Box.point[7]);
      Normal := vectornormalize(VectorSub(Box.point[5], Box.point[1]));
      d1 := Dist;
      Result := True;
    end;
  IF
  // Bottom
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[2], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[1],
    Box.point[2], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[1], Box.point[5]));
      d1 := Dist;
      Result := True;
    end;
end;

// see also the D3DBox8 Version of theis proc
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TD3DBox12; Out HitPoint, Normal: TD3DXVector3): boolean;
var
  U, V, Dist: single;
  d1: single;
  v1, v2, V3, v4, v5: TD3DXVector3;
begin
  Result := False;
  d1 := D3DX9.D3DX_DEFAULT_FLOAT;

  IF
  // Front
    D3DX9.D3DXIntersectTri(Box.point[1], Box.point[6], Box.point[2], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[1], Box.point[5],
    Box.point[6], RayPos, RayDir, U, V, Dist) then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := CalcNormalCCW(Box.point[1], Box.point[5], Box.point[6]);
      Result := True;
      d1 := Dist;
    end;
  IF
  // Right
    D3DX9.D3DXIntersectTri(Box.point[2], Box.point[7], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[2], Box.point[6],
    Box.point[7], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[2], Box.point[1]));
      Result := True;
      d1 := Dist;
    end;
  IF
  // Left
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[5], Box.point[1], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[8],
    Box.point[5], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[1], Box.point[2]));
      d1 := Dist;
      Result := True;
    end;
  IF
  // back
    D3DX9.D3DXIntersectTri(Box.point[3], Box.point[8], Box.point[4], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[3], Box.point[7],
    Box.point[8], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[4], Box.point[1]));
      v1 := CalcNormalCCW(Box.point[3], Box.point[8], Box.point[4]);
      d1 := Dist;
      Result := True;
    end;
  IF
  // Top
    D3DX9.D3DXIntersectTri(Box.point[5], Box.point[7], Box.point[6], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[5], Box.point[8],
    Box.point[7], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := CalcNormalCCW(Box.point[5], Box.point[8], Box.point[7]);
      Normal := vectornormalize(VectorSub(Box.point[5], Box.point[1]));
      d1 := Dist;
      Result := True;
    end;
  IF
  // Bottom
    D3DX9.D3DXIntersectTri(Box.point[4], Box.point[2], Box.point[3], RayPos,
    RayDir, U, V, Dist) OR D3DX9.D3DXIntersectTri(Box.point[4], Box.point[1],
    Box.point[2], RayPos, RayDir, U, V, Dist) Then
    If Dist < d1 Then
    Begin
      HitPoint := VectorAdd(RayPos, VectorMuls(RayDir, Dist));
      Normal := vectornormalize(VectorSub(Box.point[1], Box.point[5]));
      d1 := Dist;
      Result := True;
    end;
end;

{ -----------------------------------------------------------------------------
  Procedure: D3DSqrtDistance
  Author:    pawelp
  Date:      2003-01-02 15:39
  Arguments: v1, v2: TD3DVector
  Result:    single
  DESCRIPTION:
  This function is used to test if a ray goes throught a BoundingBox
  so we dont need to call separate funcs for every Triangle...


  is Exact the same as the Box12 Version of this proc, because the 4 additional
  points in a D3DBox12 are just for more precisioned Collisions with other Boxes
  EXAMPLE:
  ----------------------------------------------------------------------------- }
function D3DSqrtDistance(CONST v1, v2: TD3DVector): single;
begin
  // real Distance = sqrt( sqr(v2.x-v1.x) + sqr(V2.y-v1.y) + sqr(V2.z-v1.z) )
  Result := (sqr(v1.x - v2.x) + sqr(v1.y - v2.y) + sqr(v1.z - v2.z));
end;

function D3DRealDistance(CONST v1, v2: TD3DVector): single;
Begin
  Result := sqrt(D3DSqrtDistance(v1, v2));
end;

{
  Needs ClockWise Pol;ygons
  THis is a SLOW Function!!!
  (1,1)
  (5,1)
  (3,4)
  (0,6) }

Function LineThruPolygon_outdated(CONST LineFirstPoint, LineSecondPoint: TPoint;
  CONST PollyArray: Array of TPoint): boolean;
Var
  X1, Y1: integer;
  Left, Right: boolean;
  F1, F2: integer;
  LRS: single;
  i: integer;
  LastPointIndex: integer;
Begin
  // line thru convex polygon or not
  // test all polygon points for being left or right (or on) a given line
  // if some points are on the left and some on the right -> line intersects polygon

  Result := False; // default :

  Left := False; // point lies either left or right (or on) a given line
  Right := False;

  F1 := LineSecondPoint.x - LineFirstPoint.x;
  // fixed factors of the multiplication
  F2 := LineSecondPoint.y - LineFirstPoint.y;

  LastPointIndex := High(PollyArray);

  // take every point in turn and check on which "side" of the line it lies.
  // if we find a point on the line -> intersects
  // if we find at least 1 point on the left and at least 1 point on the right -> intersects
  For i := 0 to LastPointIndex do
  Begin
    X1 := PollyArray[i].x;
    Y1 := PollyArray[i].y;

    LRS := F1 * (Y1 - LineFirstPoint.y) - (X1 - LineFirstPoint.x) * F2;
    If LRS = 0 Then // point is on the line -> intersects
    Begin
      Result := True;
      Break;
    end
    else
    begin
      // Point lies either left or right of the line
      If LRS < 0 Then
        Left := True
      else
        Right := True;

      // check if we already have a point to the left and to the right
      If ((Left = True) And (Right = True)) Then
      Begin
        // we have at least 1 point on the left and 1 on the right of our line -> intersects
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ -----------------------------------------------------------------------------
  Procedure:   LineThruPolygon
  Author:      pawelp
  Date:        2002-12-08 19:43
  Arguments:   const LineFirstPoint,LineSecondPoint : TD3DXVector3; Const Min,Max : TD3DXVector3
  Result:      Boolean
  DESCRIPTION: Important Remark, this funktion dos work only in 2D, it totaly ignore the Y axis....
  EXAMPLE:
  ----------------------------------------------------------------------------- }
Function LineThruPolygon(LineFirstPoint, LineSecondPoint: TD3DXVector3;
  Const PollyArray: Array of TD3DXVector3; PolyCount: integer): boolean;
Var
  X1, Y1: single;
  Left, Right: boolean;
  F1, F2: single;
  LRS: single;
  i: integer;
  LastPointIndex: integer;

Begin
  // line thru convex polygon or not
  // test all polygon points for being left or right (or on) a given line
  // if some points are on the left and some on the right -> line intersects polygon

  Result := False; // default :

  Left := False; // point lies either left or right (or on) a given line
  Right := False;

  F1 := LineSecondPoint.x - LineFirstPoint.x;
  // fixed factors of the multiplication
  F2 := LineSecondPoint.z - LineFirstPoint.z;

  LastPointIndex := PolyCount - 1;

  // take every point in turn and check on which "side" of the line it lies.
  // if we find a point on the line -> intersects
  // if we find at least 1 point on the left and at least 1 point on the right -> intersects
  For i := 0 to LastPointIndex do
  Begin
    X1 := PollyArray[i].x;
    Y1 := PollyArray[i].z;

    LRS := F1 * (Y1 - LineFirstPoint.z) - (X1 - LineFirstPoint.x) * F2;
    If LRS = 0 Then // point is on the line -> intersects
    Begin
      Result := True;
      Break;
    end
    else
    begin
      // Point lies either left or right of the line
      If LRS < 0 Then
        Left := True
      else
        Right := True;

      // check if we already have a point to the left and to the right
      If ((Left = True) And (Right = True)) Then
      Begin
        // we have at least 1 point on the left and 1 on the right of our line -> intersects
        Result := True;
        Break;
      end;
    end;
  end;
end;

Function PolygonInInRect(Const PollyArray: Array of TD3DXVector3;
  PollyArrayLen: integer; Const Rect: TRect): boolean;
var
  x, y: integer;
  X1, Y1: single;
  Left, Right: boolean;
  F1, F2: single;
  LRS: single;
  LineSecondPoint, LineFirstPoint: TD3DXVector3;
Begin
  Result := False;

  // test the Points
  for x := 0 to PollyArrayLen - 1 DO
    if (PollyArray[x].x < Rect.Right) and (PollyArray[x].x > Rect.Left) and
      (PollyArray[x].z > Rect.Top) and (PollyArray[x].z < Rect.Bottom) then
    Begin
      Result := True;
      exit;
    end;

  // now check Line Intersections
  for x := 0 to 3 DO
  Begin
    CASE x of
      0:
        Begin
          LineFirstPoint.x := Rect.Left;
          LineFirstPoint.z := Rect.Top;
          LineSecondPoint.x := Rect.Right;
          LineSecondPoint.z := Rect.Top;
        end;

      1:
        Begin
          LineFirstPoint.x := Rect.Right;
          LineFirstPoint.z := Rect.Top;
          LineSecondPoint.x := Rect.Right;
          LineSecondPoint.z := Rect.Bottom;
        end;

      2:
        Begin
          LineFirstPoint.x := Rect.Right;
          LineFirstPoint.z := Rect.Bottom;
          LineSecondPoint.x := Rect.Left;
          LineSecondPoint.z := Rect.Bottom;
        end;
      3:
        Begin
          LineFirstPoint.x := Rect.Left;
          LineFirstPoint.z := Rect.Bottom;
          LineSecondPoint.x := Rect.Left;
          LineSecondPoint.z := Rect.Top;
        end;
    end;

    Left := False; // point lies either left or right (or on) a given line
    Right := False;

    F1 := LineSecondPoint.x - LineFirstPoint.x;
    // fixed factors of the multiplication
    F2 := LineSecondPoint.z - LineFirstPoint.z;

    For y := 0 to PollyArrayLen - 1 DO
    Begin
      X1 := PollyArray[y].x;
      Y1 := PollyArray[y].z;

      LRS := F1 * (Y1 - LineFirstPoint.z) - (X1 - LineFirstPoint.x) * F2;
      If LRS = 0 Then // point is on the line -> intersects
      Begin
        Result := True;
        exit;
      end
      else
      begin
        // Point lies either left or right of the line
        If LRS < 0 Then
          Left := True
        else
          Right := True;

        // check if we already have a point to the left and to the right
        If ((Left = True) And (Right = True)) Then
        Begin
          // we have at least 1 point on the left and 1 on the right of our line -> intersects
          Result := True;
          exit;
        end;
      end;
    end;
  end;
end;

Function PolygonInInPolygon(Const PollyArray1, PollyArray2
  : Array of TD3DXVector3; PollyArray1Len, PollyArray2Len: integer): boolean;
var
  x, y: integer;
  X1, Y1: single;
  Left, Right: boolean;
  F1, F2: single;
  LRS: single;
  LineSecondPoint, LineFirstPoint: pD3DXVector3;
Begin
  Result := False;

  // test the Points
  for x := 0 to PollyArray1Len - 1 DO
    if PointInPolygon(PollyArray1[x], PollyArray2, PollyArray2Len) then
    Begin
      Result := True;
      exit;
    end;

  for x := 0 to PollyArray2Len - 1 DO
    if PointInPolygon(PollyArray2[x], PollyArray1, PollyArray1Len) then
    Begin
      Result := True;
      exit;
    end;

  // now check Line Intersections
  for x := 0 to PollyArray1Len DO
  Begin
    if x = PollyArray1Len then
    Begin
      LineFirstPoint := @PollyArray1[x];
      LineSecondPoint := @PollyArray1[0];
    end
    else
    Begin
      LineFirstPoint := @PollyArray1[x];
      LineSecondPoint := @PollyArray1[x + 1];
    end;

    Left := False; // point lies either left or right (or on) a given line
    Right := False;

    F1 := LineSecondPoint.x - LineFirstPoint.x;
    // fixed factors of the multiplication
    F2 := LineSecondPoint.z - LineFirstPoint.z;

    For y := 0 to PollyArray2Len - 1 DO
    Begin
      X1 := PollyArray2[y].x;
      Y1 := PollyArray2[y].z;

      LRS := F1 * (Y1 - LineFirstPoint.z) - (X1 - LineFirstPoint.x) * F2;
      If LRS = 0 Then // point is on the line -> intersects
      Begin
        Result := True;
        exit;
      end
      else
      begin
        // Point lies either left or right of the line
        If LRS < 0 Then
          Left := True
        else
          Right := True;

        // check if we already have a point to the left and to the right
        If ((Left = True) And (Right = True)) Then
        Begin
          // we have at least 1 point on the left and 1 on the right of our line -> intersects
          Result := True;
          exit;
        end;
      end;
    end;
  end;
end;

// the poly have to be CCW
Function PointInPolygon(Const CheckPoint: TD3DXVector3;
  CONST PollyArray: Array of TD3DXVector3; PollyArrayLen: integer): boolean;
Var
  X1, Y1, X2, Y2: single;
  i: integer;
Begin
  Result := False;
  // Decide whether or not a point lies within a convex polygon
  // Polygon coordinate points must be given in clockwise fashion

  // take every "line" in turn and check on which "side" of that line lies
  // the point to check. If for every line-pair the point to check is on
  // the "inside" than the point lies in the polygon.
  DEC(PollyArrayLen);
  For i := 0 to PollyArrayLen do
  Begin
    // First point of the line
    X1 := PollyArray[i].x;
    Y1 := PollyArray[i].z;
    // Next point
    If i <> PollyArrayLen Then
    Begin
      X2 := PollyArray[i + 1].x;
      Y2 := PollyArray[i + 1].z;
    end
    else
    begin
      X2 := PollyArray[0].x;
      Y2 := PollyArray[0].z;
    end;
    // this line does it all !
    Result := (Y1 * X2) - (X1 * Y2) - (CheckPoint.z * X2) + (CheckPoint.x * Y2)
      + (CheckPoint.z * X1) - (CheckPoint.x * Y1) <= 0;

    If Result = False Then
      Break;
  end;
end;

Function CrateBoundingBoxForPoint(CONST point: TD3DXVector3; Size: single)
  : TD3DBox8;
var
  Min, Max: TD3DXVector3;
Begin
  Min := D3DXVector3(point.x - Size, point.y - Size, point.z - Size);
  Max := D3DXVector3(point.x + Size, point.y + Size, point.z + Size);
  Result := MinMaxToBox8(Min, Max);
end;

Procedure ComputeVerticesForBoundingBox(CONST point: TD3DXVector3; Size: single;
  VAR BoundingBoxVertices: array of TBoundingBoxVERTEX;
  color: DWORD = $FFFFFFFF);
var
  Box: TD3DBox8;
Begin
  Box := CrateBoundingBoxForPoint(point, Size);
  ComputeVerticesForBoundingBox(Box, BoundingBoxVertices, color);
end;

Procedure ComputeVerticesForBoundingBox(CONST Box: TD3DBox8;
  VAR BoundingBoxVertices: array of TBoundingBoxVERTEX; color: DWORD);
var
  x: integer;
Begin
  BoundingBoxVertices[0].point := Box.point[1];
  BoundingBoxVertices[1].point := Box.point[2];

  BoundingBoxVertices[2].point := Box.point[2];
  BoundingBoxVertices[3].point := Box.point[3];

  BoundingBoxVertices[4].point := Box.point[3];
  BoundingBoxVertices[5].point := Box.point[4];

  BoundingBoxVertices[6].point := Box.point[4];
  BoundingBoxVertices[7].point := Box.point[1];

  BoundingBoxVertices[8].point := Box.point[1];
  BoundingBoxVertices[9].point := Box.point[5];

  BoundingBoxVertices[10].point := Box.point[2];
  BoundingBoxVertices[11].point := Box.point[6];

  BoundingBoxVertices[12].point := Box.point[3];
  BoundingBoxVertices[13].point := Box.point[7];

  BoundingBoxVertices[14].point := Box.point[4];
  BoundingBoxVertices[15].point := Box.point[8];

  BoundingBoxVertices[16].point := Box.point[5];
  BoundingBoxVertices[17].point := Box.point[6];

  BoundingBoxVertices[18].point := Box.point[6];
  BoundingBoxVertices[19].point := Box.point[7];

  BoundingBoxVertices[20].point := Box.point[7];
  BoundingBoxVertices[21].point := Box.point[8];

  BoundingBoxVertices[22].point := Box.point[8];
  BoundingBoxVertices[23].point := Box.point[5];
  For x := 0 To 23 DO
    BoundingBoxVertices[x].color := color;
end;

function RayCastPlaneIntersect(const RayStart, rayVector: TD3DXVector3;
  const planePoint, planeNormal: TD3DXVector3;
  intersectPoint: pD3DXVector3 = nil): boolean;
  const
    // some very small numbers
  EPSILON  : Single = 1e-100;
  EPSILON2 : Single = 1e-50;
var
   sp : TD3DXVector3;
   t, d : Single;
begin
   d:=VectorDotProduct(rayVector, planeNormal);
   Result:=((d>EPSILON2) or (d<-EPSILON2));

   if Result and Assigned(intersectPoint)  then
   begin
      sp:= Vectorsub(planePoint, rayStart);
      d:=1/d; // will keep one FPU unit busy during dot product calculation
      t:=VectorDotProduct(sp, planeNormal)*d;
      if t>0 then
      begin
         intersectPoint^:= rayStart;
         intersectPoint^:= Vectoradd(
         intersectPoint^, Vectormuls(rayVector, t));

      end else
      Result:=False;
   end;
end;



Function GetIntersectPoint(CONST RayStart, RayDir, v1, v2, V3: TD3DXVector3;
  VAR HitPoint: TD3DXVector3): boolean;
var
  U, V, Dist: single;
Begin
  IF D3DX9.D3DXIntersectTri(v1, v2, V3, RayStart, RayDir, U, V, Dist) Then
  Begin
{$IFNDEF CLOOTIE}
    D3DX9.D3DXVec3BaryCentric(@HitPoint, @v1, @v2, @V3, U, V);
{$ELSE}
    D3DX9.D3DXVec3BaryCentric(HitPoint, v1, v2, V3, U, V);
{$ENDIF}
    Result := True;
  end
  else
    Result := False;
end;

Function GetBoxCenter(CONST Box: TD3DBox8): TD3DXVector3;
var
  x: integer;
Begin
  Result := Box.point[1];
  For x := 2 To 8 DO
    D3DX9.D3DXVec3Add(Result, Result, Box.point[x]);
  Result := VectorDivs(Result, 8);
end;

function SphaereInBox(CONST SphereCenter: TD3DVector; SphereRadius: single;
  CONST Box: TD3DBox8): boolean;
var
  x: integer;
Begin
  IF D3DPunktinBox(SphereCenter, Box) Then
    Result := True
  else
  Begin
    // get the nearest Point of the Sphere towortds the BoxCenter
    For x := 1 TO 8 DO
      if D3DPunktinKugel(Box.point[x], SphereCenter, SphereRadius) Then
      begin
        Result := True;
        exit;
      end;
    Result := False;
  end;
end;

function SphaereInBox(CONST SphereCenter: TD3DVector; SphereRadius: single;
  CONST Box: TD3DBox12): boolean;
var
  x: integer;
Begin
  IF D3DPunktinBox(SphereCenter, Box) Then
    Result := True
  else
  Begin
    // get the nearest Point of the Sphere towortds the BoxCenter
    For x := 1 TO 12 DO
      if D3DPunktinKugel(Box.point[x], SphereCenter, SphereRadius) Then
      begin
        Result := True;
        exit;
      end;
    Result := False;
  end;
end;

Procedure OBBToBox8(CONST OBB: TOBBox; Out Box8: TD3DBox8);
  function AddVectors(const Vectors: Array of TD3DXVector3;
    signs: Array of single): TD3DXVector3;
  var
    i: integer;
  begin
    Result := VectorAdd(Vectors[0], VectorMuls(Vectors[1], signs[0]));
    Result := VectorAdd(Result, VectorMuls(Vectors[2], signs[1]));
    Result := VectorAdd(Result, VectorMuls(Vectors[3], signs[2]));
  end; (* AddVectors *)

var
  RadAchsen: Array [0 .. 2] of TD3DXVector3;
  i: integer;
begin
  WITH Box8, OBB DO
  BEGIN
    // Achsen strecken
    RadAchsen[0] := VectorMuls(vDir, RadiusDir);
    RadAchsen[1] := VectorMuls(vUp, RadiusUp);
    RadAchsen[2] := VectorMuls(vRight, RadiusRight);

    point[1] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [-1, -1, -1]);
    point[2] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [-1, -1, +1]);
    point[3] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [+1, -1, +1]);
    point[4] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [+1, -1, -1]);
    point[5] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [-1, +1, -1]);
    point[6] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [-1, +1, +1]);
    point[7] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [+1, +1, +1]);
    point[8] := AddVectors([Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]],
      [+1, +1, -1])
  END;
end;

{ -----------------------------------------------------------------------------
  Procedure: GetOBBVertieces
  Author:    pawelp
  Date:      2004-06-14 10:50
  Arguments: const Box : TOBBox
  Result:    TOBBoxVertieces
  DESCRIPTION:
  EXAMPLE:
  ----------------------------------------------------------------------------- }

function GetOBBVertieces(const Box: TOBBox): TOBBoxVertieces;
  function AddVectors(const Vectors: Array of TD3DXVector3;
    signs: Array of single): TD3DXVector3;
  var
    i: integer;
  begin
    Result := VectorAdd(Vectors[0], VectorMuls(Vectors[1], signs[0]));
    Result := VectorAdd(Result, VectorMuls(Vectors[2], signs[1]));
    Result := VectorAdd(Result, VectorMuls(Vectors[3], signs[2]));
  end; (* AddVectors *)

var
  RadAchsen: Array [0 .. 2] of TD3DXVector3;
  i: integer;
begin
  // Achsen strecken
  RadAchsen[0] := VectorMuls(Box.vDir, Box.RadiusDir);
  RadAchsen[1] := VectorMuls(Box.vUp, Box.RadiusUp);
  RadAchsen[2] := VectorMuls(Box.vRight, Box.RadiusRight);

  Result[0] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [-1, -1, -1]);
  Result[1] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [-1, -1, +1]);
  Result[2] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [+1, -1, +1]);
  Result[3] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [+1, -1, -1]);
  Result[4] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [-1, +1, -1]);
  Result[5] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [-1, +1, +1]);
  Result[6] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [+1, +1, +1]);
  Result[7] := AddVectors([Box.Center, RadAchsen[0], RadAchsen[1], RadAchsen[2]
    ], [+1, +1, -1])
end; (* GetOBBVertieces *)

function D3DPunktinBox(CONST p: TD3DVector; CONST Box: TOBBox): boolean;
Begin
  Result := False;
  WITH Box DO
  BEGIN
    // fronplane
    if vectordotproduct(vDir, VectorSub(p, VectorAdd(Center, VectorMuls(vDir,
      RadiusDir)))) >= 0 then
      exit;
    if vectordotproduct(VectorMuls(vDir, -1),
      VectorSub(p, VectorAdd(Center, VectorMuls(vDir, -RadiusDir)))) >= 0 then
      exit;

    if vectordotproduct(vUp, VectorSub(p, VectorAdd(Center, VectorMuls(vUp,
      RadiusUp)))) >= 0 then
      exit;
    if vectordotproduct(VectorMuls(vUp, -1),
      VectorSub(p, VectorAdd(Center, VectorMuls(vUp, -RadiusUp)))) >= 0 then
      exit;

    if vectordotproduct(vRight, VectorSub(p, VectorAdd(Center,
      VectorMuls(vRight, RadiusRight)))) >= 0 then
      exit;
    if vectordotproduct(VectorMuls(vRight, -1),
      VectorSub(p, VectorAdd(Center, VectorMuls(vRight, -RadiusRight)))) >= 0
    then
      exit;
  END;
  Result := True;
end;

Procedure GetMinMaxFromOBBox(Out Min, Max: TD3DXVector3; Const Box: TOBBox);
var
  vertices: TOBBoxVertieces;
  x: integer;
Begin
  vertices := GetOBBVertieces(Box);

  Min := TD3DXVector3(vertices[0]);
  Max := TD3DXVector3(vertices[0]);
  for x := 1 To 7 DO
  Begin
    D3DX9.D3DXVec3Minimize(Min, Min, TD3DXVector3(vertices[x]));
    D3DX9.D3DXVec3Maximize(Max, Max, TD3DXVector3(vertices[x]));
  end;
end;

Procedure GetMinMaxFromOBBox(Out Min, Max: TD3DXVector3; Const Box: TOBBox;
  Const Matrix: TD3DXMatrix);
var
  vertices: TOBBoxVertieces;
  x: integer;
Begin
  vertices := GetOBBVertieces(Box);

  For x := 0 To 7 DO
    D3DXVec3TransFormCoord(TD3DXVector3(vertices[x]),
      TD3DXVector3(vertices[x]), Matrix);

  Min := TD3DXVector3(vertices[0]);
  Max := TD3DXVector3(vertices[0]);
  for x := 1 To 7 DO
  Begin
    D3DX9.D3DXVec3Maximize(Max, Max, TD3DXVector3(vertices[x]));
    D3DX9.D3DXVec3Minimize(Min, Min, TD3DXVector3(vertices[x]));
  end;
end;

function IntersectBoundingBox(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TOBBox; Out HitPoint, Normal: TD3DXVector3): boolean; overload;
var
  Box8: TD3DBox8;
  Dist: single;
Begin
  IF NOT IntersectBoundingBox(RayPos, RayDir, Box, Dist) then
    Result := False
  else
  Begin
    OBBToBox8(Box, Box8);
    Result := IntersectBoundingBox(RayPos, RayDir, Box, HitPoint, Normal);
  end;
end;

Function IntersectBoundingBoxOld(CONST RayPos, RayDir: TD3DVector;
  CONST Box: TOBBox): boolean;
var
  fWdU, fAWdU, fDdU, fADdU, fAWxDdU: array [0 .. 2] of single;
  fRhs: single;
  kDiff, kWxD: TD3DXVector3;
Begin
  kDiff := VectorSub(RayPos, Box.Center);

  fWdU[0] := vectordotproduct(RayDir, Box.vDir);
  fAWdU[0] := Abs(fWdU[0]);

  fDdU[0] := vectordotproduct(kDiff, Box.vDir);
  fADdU[0] := Abs(fDdU[0]);
  if (fADdU[0] > Box.RadiusRight) AND (fDdU[0] * fWdU[0] >= 0) Then
  Begin
    Result := False;
    exit;
  end;

  fWdU[1] := vectordotproduct(RayDir, Box.vUp);
  fAWdU[1] := Abs(fWdU[1]);
  fDdU[1] := vectordotproduct(kDiff, Box.vUp);
  fADdU[1] := Abs(fDdU[1]);
  if (fADdU[1] > Box.RadiusUp) AND (fDdU[1] * fWdU[1] >= 0) Then
  Begin
    Result := False;
    exit;
  end;

  fWdU[2] := vectordotproduct(RayDir, Box.vRight);
  fAWdU[2] := Abs(fWdU[2]);
  fDdU[2] := vectordotproduct(kDiff, Box.vRight);
  fADdU[2] := Abs(fDdU[2]);
  if (fADdU[2] > Box.RadiusDir) AND (fDdU[2] * fWdU[2] >= 0) Then
  Begin
    Result := False;
    exit;
  end;

  kWxD := vectorcrossproduct(RayDir, kDiff);

  fAWxDdU[0] := Abs(vectordotproduct(kWxD, Box.vDir));
  fRhs := Box.RadiusUp * fAWdU[2] + Box.RadiusDir * fAWdU[1];
  if (fAWxDdU[0] > fRhs) Then
  Begin
    Result := False;
    exit;
  end;

  fAWxDdU[1] := Abs(vectordotproduct(kWxD, Box.vUp));
  fRhs := Box.RadiusRight * fAWdU[2] + Box.RadiusDir * fAWdU[0];
  if (fAWxDdU[1] > fRhs) Then
  Begin
    Result := False;
    exit;
  end;

  fAWxDdU[2] := Abs(vectordotproduct(kWxD, Box.vRight));
  fRhs := Box.RadiusRight * fAWdU[1] + Box.RadiusUp * fAWdU[0];
  if (fAWxDdU[2] > fRhs) Then
  Begin
    Result := False;
    exit;
  end;

  Result := True;
end;

Procedure GetRelativPointPos(CONST point: TD3DXVector3; CONST Box: TOBBox;
  VAR InFront, Right: boolean);
Begin
  // InFront := vectordotproduct(PlaneNormal, vectorsub(Point, PlanePoint)) >= 0;
  InFront := vectordotproduct(Box.vDir, VectorSub(point, Box.Center)) >= 0;
  Right := vectordotproduct(Box.vRight, VectorSub(point, Box.Center)) >= 0;
end;

// as a remember this func is form Magissoftware.com
Function D3DBoxinBox(const rkBox0, rkBox1: TOBBox): boolean;
var
  fCutoff: single;
  bExistsParallelPair: boolean;
  i: integer;

  akA, akB: array [0 .. 2] of TD3DXVector3;
  afEA, afEB: array [0 .. 2] of single;
  DiffOfBoxCenters: TD3DXVector3;

  aafC, aafAbsC: array [0 .. 2, 0 .. 2] Of single;
  // matrix C := A^T B, c_BeginijEnd; := Dot(A_i,B_j)
  afAD: array [0 .. 2] Of single; // Dot(A_i,D)
  fR0, fR1, fR: single; // interval radii and distance between centers
  fR01: single; // := R0 + R1
Begin
  // if not D3DKugelInKugel(rkBox0.Center, rkBox1.Center, rkBox0.fRadiusSphere, rkBox1.fRadiusSphere) then
  // begin
  // Result := false;
  // Exit;
  // end;

  // Cutoff for cosine of angles between box axes.  This is used to catch
  // the cases when at least one pair of axes are parallel.  If this happens,
  // there is no need to test for separation along the Cross(A[i],B[j])
  // directions.
  fCutoff := 0.999999;
  bExistsParallelPair := False;

  // convenience variables
  akA[0] := rkBox0.vRight;
  akA[1] := rkBox0.vUp;
  akA[2] := rkBox0.vDir;

  akB[0] := rkBox1.vRight;
  akB[1] := rkBox1.vUp;
  akB[2] := rkBox1.vDir;

  afEA[0] := rkBox0.RadiusRight;
  afEA[1] := rkBox0.RadiusUp;
  afEA[2] := rkBox0.RadiusDir;

  afEB[0] := rkBox1.RadiusRight;
  afEB[1] := rkBox1.RadiusUp;
  afEB[2] := rkBox1.RadiusDir;

  // compute difference of box centers, D := C1-C0
  DiffOfBoxCenters := VectorSub(rkBox1.Center, rkBox0.Center);

  // axis C0+t*A0
  for i := 0 TO 2 DO
  Begin
    aafC[0][i] := vectordotproduct(akA[0], akB[i]);
    aafAbsC[0][i] := Abs(aafC[0][i]);
    if (aafAbsC[0][i] > fCutoff) Then
      bExistsParallelPair := True;
  End;
  afAD[0] := vectordotproduct(akA[0], DiffOfBoxCenters);
  fR := Abs(afAD[0]);
  fR1 := afEB[0] * aafAbsC[0][0] + afEB[1] * aafAbsC[0][1] + afEB[2] *
    aafAbsC[0][2];
  fR01 := afEA[0] + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A1
  for i := 0 TO 2 DO
  Begin
    aafC[1][i] := vectordotproduct(akA[1], akB[i]);
    aafAbsC[1][i] := Abs(aafC[1][i]);
    if (aafAbsC[1][i] > fCutoff) Then
      bExistsParallelPair := True;
  End;
  afAD[1] := vectordotproduct(akA[1], DiffOfBoxCenters);
  fR := Abs(afAD[1]);
  fR1 := afEB[0] * aafAbsC[1][0] + afEB[1] * aafAbsC[1][1] + afEB[2] *
    aafAbsC[1][2];
  fR01 := afEA[1] + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A2
  for i := 0 TO 2 DO
  Begin
    aafC[2][i] := vectordotproduct(akA[2], akB[i]);
    aafAbsC[2][i] := Abs(aafC[2][i]);
    if (aafAbsC[2][i] > fCutoff) Then
      bExistsParallelPair := True;
  End;
  afAD[2] := vectordotproduct(akA[2], DiffOfBoxCenters);
  fR := Abs(afAD[2]);
  fR1 := afEB[0] * aafAbsC[2][0] + afEB[1] * aafAbsC[2][1] + afEB[2] *
    aafAbsC[2][2];
  fR01 := afEA[2] + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*B0
  fR := Abs(vectordotproduct(akB[0], DiffOfBoxCenters));
  fR0 := afEA[0] * aafAbsC[0][0] + afEA[1] * aafAbsC[1][0] + afEA[2] *
    aafAbsC[2][0];
  fR01 := fR0 + afEB[0];
  if (fR > fR01) then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*B1
  fR := Abs(vectordotproduct(akB[1], DiffOfBoxCenters));
  fR0 := afEA[0] * aafAbsC[0][1] + afEA[1] * aafAbsC[1][1] + afEA[2] *
    aafAbsC[2][1];
  fR01 := fR0 + afEB[1];
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*B2
  fR := Abs(vectordotproduct(akB[2], DiffOfBoxCenters));
  fR0 := afEA[0] * aafAbsC[0][2] + afEA[1] * aafAbsC[1][2] + afEA[2] *
    aafAbsC[2][2];
  fR01 := fR0 + afEB[2];
  if (fR > fR01) Then
  begin
    Result := False;
    exit;
  end;

  // At least one pair of box _Achsen was parallel, so the separation is
  // effectively in 2D where checking the "edge" normals is sufficient for
  // the separation of the boxes.
  if (bExistsParallelPair) Then
  Begin
    Result := True;
    exit;
  end;

  // axis C0+t*A0xB0
  fR := Abs(afAD[2] * aafC[1][0] - afAD[1] * aafC[2][0]);
  fR0 := afEA[1] * aafAbsC[2][0] + afEA[2] * aafAbsC[1][0];
  fR1 := afEB[1] * aafAbsC[0][2] + afEB[2] * aafAbsC[0][1];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A0xB1
  fR := Abs(afAD[2] * aafC[1][1] - afAD[1] * aafC[2][1]);
  fR0 := afEA[1] * aafAbsC[2][1] + afEA[2] * aafAbsC[1][1];
  fR1 := afEB[0] * aafAbsC[0][2] + afEB[2] * aafAbsC[0][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  begin
    Result := False;
    exit;
  end;

  // axis C0+t*A0xB2
  fR := Abs(afAD[2] * aafC[1][2] - afAD[1] * aafC[2][2]);
  fR0 := afEA[1] * aafAbsC[2][2] + afEA[2] * aafAbsC[1][2];
  fR1 := afEB[0] * aafAbsC[0][1] + afEB[1] * aafAbsC[0][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A1xB0
  fR := Abs(afAD[0] * aafC[2][0] - afAD[2] * aafC[0][0]);
  fR0 := afEA[0] * aafAbsC[2][0] + afEA[2] * aafAbsC[0][0];
  fR1 := afEB[1] * aafAbsC[1][2] + afEB[2] * aafAbsC[1][1];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A1xB1
  fR := Abs(afAD[0] * aafC[2][1] - afAD[2] * aafC[0][1]);
  fR0 := afEA[0] * aafAbsC[2][1] + afEA[2] * aafAbsC[0][1];
  fR1 := afEB[0] * aafAbsC[1][2] + afEB[2] * aafAbsC[1][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  begin
    Result := False;
    exit;
  end;

  // axis C0+t*A1xB2
  fR := Abs(afAD[0] * aafC[2][2] - afAD[2] * aafC[0][2]);
  fR0 := afEA[0] * aafAbsC[2][2] + afEA[2] * aafAbsC[0][2];
  fR1 := afEB[0] * aafAbsC[1][1] + afEB[1] * aafAbsC[1][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  begin
    Result := False;
    exit;
  end;

  // axis C0+t*A2xB0
  fR := Abs(afAD[1] * aafC[0][0] - afAD[0] * aafC[1][0]);
  fR0 := afEA[0] * aafAbsC[1][0] + afEA[1] * aafAbsC[0][0];
  fR1 := afEB[1] * aafAbsC[2][2] + afEB[2] * aafAbsC[2][1];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  begin
    Result := False;
    exit;
  end;

  // axis C0+t*A2xB1
  fR := Abs(afAD[1] * aafC[0][1] - afAD[0] * aafC[1][1]);
  fR0 := afEA[0] * aafAbsC[1][1] + afEA[1] * aafAbsC[0][1];
  fR1 := afEB[0] * aafAbsC[2][2] + afEB[2] * aafAbsC[2][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  // axis C0+t*A2xB2
  fR := Abs(afAD[1] * aafC[0][2] - afAD[0] * aafC[1][2]);
  fR0 := afEA[0] * aafAbsC[1][2] + afEA[1] * aafAbsC[0][2];
  fR1 := afEB[0] * aafAbsC[2][1] + afEB[1] * aafAbsC[2][0];
  fR01 := fR0 + fR1;
  if (fR > fR01) Then
  Begin
    Result := False;
    exit;
  end;

  Result := True;
End;

// this is a debug func for speed and precision tests

// check if two oriented bounding boxes overlap
// This function cames from GamaSutra, but is a bit slower and not so precise as the other one (D3DBoxinBox)
Function OBBOverlap(CONST BoxA, BoxB: TOBBox): boolean;
var
  V: TD3DXVector3;
  T: Array [0 .. 2] of single;

  // B's basis with respect to A's local frame
  r: array [0 .. 2, 0 .. 2] OF single;
  ra, rb, t2: single;
  i, k: integer;
Begin
  // translation, in parent frame
  V := VectorSub(BoxA.Center, BoxB.Center);
  // translation, in A's frame
  T[0] := vectordotproduct(V, BoxA.vRight);
  T[1] := vectordotproduct(V, BoxA.vUp);
  T[2] := vectordotproduct(V, BoxA.vDir);

  // calculate rotation matrix
  for i := 0 TO 2 DO
    for k := 0 To 2 DO
      r[i][k] := vectordotproduct(BoxA.AchsenV3[i], BoxB.AchsenV3[k]);
  { ALGORITHM: Use the separating axis test for all 15 potential
    separating axes. If a separating axis could not be found, the two
    boxes overlap. }

  // A's basis vectors
  for i := 0 To 2 DO
  Begin
    ra := BoxA.Radii[i];
    rb := BoxB.RadiusRight * Abs(r[i][0]) + BoxB.RadiusUp * Abs(r[i][1]) +
      BoxB.RadiusDir * Abs(r[i][2]);

    t2 := Abs(T[i]);

    if t2 > ra + rb Then
    Begin
      Result := False;
      exit;
    end;
  end;

  // B's basis vectors
  for k := 0 To 2 DO
  Begin
    ra := BoxA.RadiusRight * Abs(r[0][k]) + BoxA.RadiusUp * Abs(r[1][k]) +
      BoxA.RadiusDir * Abs(r[2][k]);
    rb := BoxB.Radii[k];

    t2 := Abs(T[0] * r[0][k] + T[1] * r[1][k] + T[2] * r[2][k]);

    if t2 > ra + rb Then
    Begin
      Result := False;
      exit;
    end;
  end;

  // 9 cross products

  // L := A0 x B0
  ra := BoxA.RadiusUp * Abs(r[2][0]) + BoxA.RadiusDir * Abs(r[1][0]);

  rb := BoxB.RadiusUp * Abs(r[0][2]) + BoxB.RadiusDir * Abs(r[0][1]);

  t2 := Abs(T[2] * r[1][0] - T[1] * r[2][0]);

  if t2 > ra + rb Then
  Begin
    Result := False;
    exit;
  End;

  // L := A0 x B1
  ra := BoxA.RadiusUp * Abs(r[2][1]) + BoxA.RadiusDir * Abs(r[1][1]);

  rb := BoxB.RadiusRight * Abs(r[0][2]) + BoxB.RadiusDir * Abs(r[0][0]);

  t2 := Abs(T[2] * r[1][1] - T[1] * r[2][1]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A0 x B2
  ra := BoxA.RadiusUp * Abs(r[2][2]) + BoxA.RadiusDir * Abs(r[1][2]);

  rb := BoxB.RadiusRight * Abs(r[0][1]) + BoxB.RadiusUp * Abs(r[0][0]);

  t2 := Abs(T[2] * r[1][2] - T[1] * r[2][2]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  end;

  // L := A1 x B0
  ra := BoxA.RadiusRight * Abs(r[2][0]) + BoxA.RadiusDir * Abs(r[0][0]);

  rb := BoxB.RadiusUp * Abs(r[1][2]) + BoxB.RadiusDir * Abs(r[1][1]);

  t2 := Abs(T[0] * r[2][0] - T[2] * r[0][0]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A1 x B1
  ra := BoxA.RadiusRight * Abs(r[2][1]) + BoxA.RadiusDir * Abs(r[0][1]);

  rb := BoxB.RadiusRight * Abs(r[1][2]) + BoxB.RadiusDir * Abs(r[1][0]);

  t2 := Abs(T[0] * r[2][1] - T[2] * r[0][1]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A1 x B2
  ra := BoxA.RadiusRight * Abs(r[2][2]) + BoxA.RadiusDir * Abs(r[0][2]);

  rb := BoxB.RadiusRight * Abs(r[1][1]) + BoxB.RadiusUp * Abs(r[1][0]);

  t2 := Abs(T[0] * r[2][2] - T[2] * r[0][2]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A2 x B0
  ra := BoxA.RadiusRight * Abs(r[1][0]) + BoxA.RadiusUp * Abs(r[0][0]);

  rb := BoxB.RadiusUp * Abs(r[2][2]) + BoxB.RadiusDir * Abs(r[2][1]);

  t2 := Abs(T[1] * r[0][0] - T[0] * r[1][0]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A2 x B1
  ra := BoxA.RadiusRight * Abs(r[1][1]) + BoxA.RadiusUp * Abs(r[0][1]);

  rb := BoxB.RadiusRight * Abs(r[2][2]) + BoxB.RadiusDir * Abs(r[2][0]);

  t2 := Abs(T[1] * r[0][1] - T[0] * r[1][1]);

  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  End;

  // L := A2 x B2
  ra := BoxA.RadiusRight * Abs(r[1][2]) + BoxA.RadiusUp * Abs(r[0][2]);
  rb := BoxB.RadiusRight * Abs(r[2][1]) + BoxB.RadiusUp * Abs(r[2][0]);

  t2 := Abs(T[1] * r[0][2] - T[0] * r[1][2]);
  if (t2 > ra + rb) Then
  Begin
    Result := False;
    exit;
  end;

  { no separating axis found,
    the two boxes overlap }

  Result := True;
end;

Procedure ProjectTriangle(const rkD: TD3DXVector3;
  const apkTri: array of TD3DXVector3; VAR rfMin, rfMax: single);
var
  fDot: single;
Begin
  rfMin := vectordotproduct(rkD, apkTri[0]);
  rfMax := rfMin;

  fDot := vectordotproduct(rkD, apkTri[1]);
  if (fDot < rfMin) then
    rfMin := fDot
  else if (fDot > rfMax) then
    rfMax := fDot;

  fDot := vectordotproduct(rkD, apkTri[2]);
  if (fDot < rfMin) then
    rfMin := fDot
  else if (fDot > rfMax) then
    rfMax := fDot;
end;

Procedure ProjectBox(const rkD: TD3DXVector3; const rkBox: TOBBox;
  VAR rfMin, rfMax: single);
var
  fDdC, fR: single;
Begin
  fDdC := vectordotproduct(rkD, rkBox.Center);
  fR := rkBox.RadiusRight * Abs(vectordotproduct(rkD, rkBox.vRight)) +
    rkBox.RadiusUp * Abs(vectordotproduct(rkD, rkBox.vUp)) + rkBox.RadiusDir *
    Abs(vectordotproduct(rkD, rkBox.vDir));
  rfMin := fDdC - fR;
  rfMax := fDdC + fR;
end;

Function IntersectionTriBox(const apkTri: Array of TD3DXVector3;
  const rkBox: TOBBox): boolean;
var
  fMin0, fMax0, fMin1, fMax1: single;
  kD: TD3DXVector3;
  akE: array [0 .. 2] of TD3DXVector3;
  fDdC: single;
  i, i0, i1: integer;
Begin
  // test direction of triangle normal
  akE[0] := VectorSub(apkTri[1], apkTri[0]);
  akE[1] := VectorSub(apkTri[2], apkTri[0]);
  kD := vectorcrossproduct(akE[0], akE[1]);
  fMin0 := vectordotproduct(kD, apkTri[0]);
  fMax0 := fMin0;
  ProjectBox(kD, rkBox, fMin1, fMax1);
  if ((fMax1 < fMin0) OR (fMax0 < fMin1)) Then
  Begin
    Result := False;
    exit;
  end;

  // test direction of box faces
  for i := 0 TO 2 DO
  Begin
    kD := rkBox.AchsenV3[i];
    ProjectTriangle(kD, apkTri, fMin0, fMax0);
    fDdC := vectordotproduct(kD, rkBox.Center);
    fMin1 := fDdC - rkBox.Radii[i];
    fMax1 := fDdC + rkBox.Radii[i];
    if ((fMax1 < fMin0) OR (fMax0 < fMin1)) then
    Begin
      Result := False;
      exit;
    end;
  end;

  // test direction of triangle-box edge cross products
  akE[2] := VectorSub(akE[1], akE[0]);
  for i0 := 0 TO 2 DO
  Begin
    for i1 := 0 TO 2 DO
    Begin
      kD := vectorcrossproduct(akE[i0], rkBox.AchsenV3[i1]);
      ProjectTriangle(kD, apkTri, fMin0, fMax0);
      ProjectBox(kD, rkBox, fMin1, fMax1);
      if ((fMax1 < fMin0) OR (fMax0 < fMin1)) then
      Begin
        Result := False;
        exit;
      end;
    end;
  end;

  Result := True;
END;

/// /////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
/// //
/// // Testet, ob der Strahl von vOrig in Richtung vDir mit der OBB kollidiert
/// // ist. Strahl und OBB müssen im Weltkoordinatenraum angegeben sein. Bei
/// // einer Kollision wird die Entfernung zum Kollisionspunkt im letzten
/// // Parameter gespeichert, falls gewünscht.[Slapmethode, s.Möller, Haines]
/// //
/// /////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
function IntersectBoundingBox(CONST RayPos, RayDir: TD3DXVector3;
  CONST Box: TOBBox; Out distance: single): boolean;
var
  e, f, t1, t2, temp, tmin, tmax: single;
  i: integer;
  vP: TD3DXVector3;
begin
  Result := False;
  tmin := -99999.9;
  tmax := +99999.9;
  vP := VectorSub(Box.Center, RayPos);
  for i := 0 to 2 do
  begin
    e := vectordotproduct(Box.AchsenV3[i], vP);
    f := vectordotproduct(Box.AchsenV3[i], RayDir);
    if Abs(f) > 0.00001 then
    begin
      t1 := (e + Box.Radii[i]) / f;
      t2 := (e - Box.Radii[i]) / f;
      if t1 > t2 then
      begin
        temp := t1;
        t1 := t2;
        t2 := temp;
      end;
      if t1 > tmin then
        tmin := t1;
      if t2 < tmax then
        tmax := t2;
      if (tmin > tmax) or (tmax < 0) then
        exit;
    end
    else if (-e - Box.Radii[i] > 0) or (-e + Box.Radii[i] < 0) then
      exit;
  end;
  if tmin > 0 then
    distance := tmin
  else
    distance := tmax;

  Result := True;
end;

function math3dLineIntersectOBB(CONST RayPos, RayDir: TD3DXVector3;
  fLaenge: single; CONST Box: TOBBox): boolean;
var
  fARayDir_vA, fAvDif_vA, fADxD_A: array [0 .. 2] of single;
  _vHL_, fRhs: single;
  vDxD, vHL, vLM, vDiff: TD3DXVector3;
begin
  Result := False;
  // Halbe Länge und Mittelpunkt der Box
  vHL := VectorMuls(RayDir, 0.5 * fLaenge);
  vLM := VectorAdd(RayPos, vHL);
  vDiff := VectorSub(vLM, Box.Center);

  fARayDir_vA[0] := Abs(vectordotproduct(vHL, Box.vRight));
  fAvDif_vA[0] := Abs(vectordotproduct(vDiff, Box.vRight));
  fRhs := Box.RadiusRight + fARayDir_vA[0];
  if fAvDif_vA[0] > fRhs then
    exit;

  fARayDir_vA[1] := Abs(vectordotproduct(vHL, Box.vUp));
  fAvDif_vA[1] := Abs(vectordotproduct(vDiff, Box.vUp));
  fRhs := Box.RadiusUp + fARayDir_vA[1];
  if fAvDif_vA[1] > fRhs then
    exit;

  fARayDir_vA[2] := Abs(vectordotproduct(vHL, Box.vDir));
  fAvDif_vA[2] := Abs(vectordotproduct(vDiff, Box.vDir));
  fRhs := Box.RadiusDir + fARayDir_vA[2];
  if fAvDif_vA[2] > fRhs then
    exit;

  vDxD := vectorcrossproduct(vHL, vDiff);

  _vHL_ := 1.0; // xxxD3DMathe_VBetrag(vHL);

  fADxD_A[0] := Abs(vectordotproduct(vDxD, Box.vRight)) / _vHL_;
  fRhs := (Box.RadiusUp * fARayDir_vA[2] + Box.RadiusDir * fARayDir_vA
    [1]) / _vHL_;
  if fADxD_A[0] > fRhs then
    exit;

  fADxD_A[1] := Abs(vectordotproduct(vDxD, Box.vUp)) / _vHL_;
  fRhs := (Box.RadiusRight * fARayDir_vA[2] + Box.RadiusDir * fARayDir_vA
    [0]) / _vHL_;
  if fADxD_A[1] > fRhs then
    exit;

  fADxD_A[2] := Abs(vectordotproduct(vDxD, Box.vDir)) / _vHL_;
  fRhs := (Box.RadiusRight * fARayDir_vA[1] + Box.RadiusUp * fARayDir_vA
    [0]) / _vHL_;
  if fADxD_A[2] > fRhs then
    exit;

  Result := True;
end;

{ -----------------------------------------------------------------------------
  Procedure: PointinNConvexPoly
  Author:    pawelp
  Date:      2003-10-06 18:31
  Arguments: Const Point : TPoint; Const Poly : array of TPoint
  Result:    Boolean
  DESCRIPTION:
  this is a 2D funtion.
  Note, there are sometimes problem when the Point is far outside of the poly,
  so it is a good Idea to perform a ptInRect test first.
  EXAMPLE:
  ----------------------------------------------------------------------------- }

Function PointinNConvexPoly(Const aPoint: TPoint; Const Poly: array of TPoint;
  PolyMinMax: pRect): boolean;
Var
  i: integer;
  V: TD3DXVector2;
  x: integer;
  r: TRect;
  IntersectionCount: integer;
  p1, p2: TPoint;
  LineEnd: TPoint;
  Projection: integer;
Begin
  if PolyMinMax = nil then
  begin
    r.TopLeft := Poly[0];
    r.BottomRight := Poly[0];
    for x := 1 to Length(Poly) - 1 do
    BEGIN
      if r.Left > Poly[x].x then
        r.Left := Poly[x].x
      ELSE if r.Right < Poly[x].x then
        r.Right := Poly[x].x;

      if r.Top > Poly[x].y then
        r.Top := Poly[x].y
      ELSE if r.Bottom < Poly[x].y then
        r.Bottom := Poly[x].y;

    END;
  end
  else
    r := PolyMinMax^;

  if not PointInRect(aPoint, r) then
    exit(False);

  // now we will project a line from the point to the right. This line can be endless but four our purpose it is fine if it is just longer then the width of the MinMax rect of the polygon
  // then we count how many intersections with the lines of the polygon the new projected line has
  // if the number of intersections is od, then the point must be inside.

  // find the learst border to project the line there
  if (aPoint.x - r.Left) > (r.Right - aPoint.x) then
  begin
    Projection := r.Right - aPoint.x;
    LineEnd.x := aPoint.x + (r.Width + 1000) * 10;
    LineEnd.y := aPoint.y;
  end
  else
  begin
    Projection := aPoint.x - r.Left;
    LineEnd.x := aPoint.x - (r.Width + 1000) * 10;
    LineEnd.y := aPoint.y;
  end;
  if Projection > (r.Bottom - aPoint.y) then
  begin
    Projection := r.Bottom - aPoint.y;
    LineEnd.x := aPoint.x;
    LineEnd.y := aPoint.y + (r.Height + 1000) * 10;
  end;
  if Projection > (aPoint.y - r.Top) then
  begin
    LineEnd.x := aPoint.x;
    LineEnd.y := aPoint.y - (r.Height + 1000) * 10;
  end;

  IntersectionCount := 0;

  // the poly is a closed one, there is a line from the last point to the first point in teh array
  p1 := Poly[Length(Poly) - 1];
  for x := 0 to Length(Poly) - 1 do
  begin
    p2 := Poly[x];
    // line segment intersection
    if max3DMath.Intersect(aPoint.x, aPoint.y, LineEnd.x, LineEnd.y, p1.x, p1.y,
      p2.x, p2.y) then
      inc(IntersectionCount);

    p1 := p2;
  end;
  // bit wise operations are always faster
  // first, clear any byte except for the first., that is performing by the and operation with 1
  // any odd number has a 1 as the first bit.
  Result := (IntersectionCount and 1) = 1;
end;

{ -----------------------------------------------------------------------------
  Procedure: InOnRight
  Author:    pawelp
  Date:      2003-10-06 18:44
  Arguments: Const Point, LaneStart,LaneEnd : TD3DXVector3
  Result:    Boolean
  DESCRIPTION:
  LaneEnd
  |
  |
  x2 |
  |   x1
  |
  LaneStart

  x1 is Right,
  x2 is NOT right
  EXAMPLE:
  ----------------------------------------------------------------------------- }

Function InOnRight(Const point, LaneStart, LaneEnd: TD3DXVector3): boolean;
Begin
  Result := (LaneStart.z * LaneEnd.x) - (LaneStart.x * LaneEnd.z) -
    (point.z * LaneEnd.x) + (point.x * LaneEnd.z) + (point.z * LaneStart.x) -
    (point.x * LaneStart.z) <= 0;
end;

// -----------------------------------
// D3D Line Dist To Box
// -----------------------------------
// the pfBParam0, pfBParam1, pfBParam2 gives the nearest point on the Boundingbox (x,y,z)
Function SqrDistance(const rkPoint: TD3DXVector3; const rkBox: TOBBox;
  pfBParam0, pfBParam1, pfBParam2: pSingle): single;
var
  kClosest, kDiff: TD3DXVector3;
  fDelta, fSqrDistance: single;

  _dist: single;
  rkTri: Triangle3;
  pfSParam, pfTParam: single;
  x: integer;
  vertices: TOBBoxVertieces;
  s, T, _s, _t: single;
  V: TD3DXVector3;

  Procedure TestTri(Edge0, Edge1: byte);
  begin
    rkTri.Edge0 := VectorSub(vertices[Edge0], rkTri.Origin);
    rkTri.Edge1 := VectorSub(vertices[Edge1], rkTri.Origin);

    _dist := SqrDistance(rkPoint, rkTri, @s, @T);
    if _dist < Result then
    begin
      if (pfBParam0 <> NIL) then
      begin
        // Triangle points are tri(s,t) = b+s*e0+t*e1
        V := VectorAdd(VectorAdd(rkTri.Origin, VectorMuls(rkTri.Edge0, s)),
          VectorMuls(rkTri.Edge1, T));
        pfBParam0^ := V.x;
        pfBParam1^ := V.y;
        pfBParam2^ := V.z;
      end;

      Result := _dist;
    end;
  end;

Begin
  Result := D3DX9.D3DX_DEFAULT;

  vertices := GetOBBVertieces(rkBox);
  rkTri.Origin := vertices[0];
  TestTri(1, 5);
  TestTri(5, 4);
  TestTri(4, 7);
  TestTri(7, 3);
  TestTri(1, 2);
  TestTri(2, 3);

  rkTri.Origin := vertices[6];
  TestTri(7, 4);
  TestTri(4, 5);
  TestTri(5, 1);
  TestTri(1, 2);
  TestTri(7, 3);
  TestTri(3, 2);

  if
  // IsInFrontOfPlane(rkPoint, rkBox.vUp,VectorAdd(rkBox.Center,VectorMuls(rkBox.vUp,rkBox.RadiusUp))) and
  // IsInFrontOfPlane(rkPoint, rkBox.vUp,VectorAdd(rkBox.Center,VectorMuls(rkBox.vUp,-rkBox.RadiusUp))) and
  //
  // IsInFrontOfPlane(rkPoint, rkBox.vDir,VectorAdd(rkBox.Center,VectorMuls(rkBox.vDir,rkBox.RadiusUp))) and
  // IsInFrontOfPlane(rkPoint, rkBox.vDir,VectorAdd(rkBox.Center,VectorMuls(rkBox.vDir,-rkBox.RadiusUp))) and
  //
  // IsInFrontOfPlane(rkPoint, rkBox.vRight,VectorAdd(rkBox.Center,VectorMuls(rkBox.vRight,rkBox.RadiusUp))) and
  // IsInFrontOfPlane(rkPoint, rkBox.vRight,VectorAdd(rkBox.Center,VectorMuls(rkBox.vRight,-rkBox.RadiusUp)))
    D3DPunktinBox(rkPoint, rkBox) then
    Result := -Result;

  /// ////////////////////////////
  exit;
  // compute coordinates of point in box coordinate system
  kDiff := VectorSub(rkPoint, rkBox.Center);
  kClosest := D3DXVector3(vectordotproduct(kDiff, rkBox.vRight),
    vectordotproduct(kDiff, rkBox.vUp), vectordotproduct(kDiff, rkBox.vDir));

  // project test point onto box
  fSqrDistance := 0.0;

  if (kClosest.x < -rkBox.RadiusRight) then
  Begin
    fDelta := kClosest.x + rkBox.RadiusRight;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.x := -rkBox.RadiusRight;
  end
  else if (kClosest.x > rkBox.RadiusRight) then
  Begin
    fDelta := kClosest.x - rkBox.RadiusRight;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.x := rkBox.RadiusRight;
  end;

  if (kClosest.y < -rkBox.RadiusUp) then
  Begin
    fDelta := kClosest.y + rkBox.RadiusUp;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.y := -rkBox.RadiusUp;
  end
  else if (kClosest.y > rkBox.RadiusUp) then
  Begin
    fDelta := kClosest.y - rkBox.RadiusUp;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.y := rkBox.RadiusUp;
  end;

  if (kClosest.z < -rkBox.RadiusDir) then
  Begin
    fDelta := kClosest.z + rkBox.RadiusDir;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.z := -rkBox.RadiusDir;
  end
  else if (kClosest.z > rkBox.RadiusDir) then
  Begin
    fDelta := kClosest.z - rkBox.RadiusDir;
    fSqrDistance := fSqrDistance + fDelta * fDelta;
    kClosest.z := rkBox.RadiusDir;
  end;

  if (pfBParam0 <> nil) then
  Begin
    pfBParam0^ := kClosest.x;
    pfBParam1^ := kClosest.y;
    pfBParam2^ := kClosest.z;
  end;

  Result := fSqrDistance;
end;

{ -----------------------------------------------------------------------------
  Procedure: Distance
  Author:    pawelp
  Date:      2004-03-31 17:04
  Arguments:
  const rkPoint : TD3DXvector3;
  const rkBox : TOBBox;
  pfBParam0, - Closest X
  pfBParam1, - Closest Y
  pfBParam2 : pSingle - CLosest Z, can be nill

  Result:    Single
  DESCRIPTION:
  Distance 3D Point To TObBox
  EXAMPLE:
  ----------------------------------------------------------------------------- }
// ----------------------------------------------------------------------------
Function distance(const rkPoint: TD3DXVector3; const rkBox: TOBBox;
  pfBParam0, pfBParam1, pfBParam2: pSingle): single;
Begin
  Result := sqrt(SqrDistance(rkPoint, rkBox, pfBParam0, pfBParam1, pfBParam2));
end;

// ----------------------------------------------------------------------------
Function SqrDistance(const rkPoint: TD3DXVector3;
  fXMin, fXMax, fYMin, fYMax, fZMin, fZMax: single;
  pkClosest: pD3DXVector3): single;
var
  fDelta, fSqrDistance: single;
Begin
  fSqrDistance := 0.0;

  if (pkClosest <> NIL) then
  Begin
    if (rkPoint.x < fXMin) then
    Begin
      fDelta := rkPoint.x - fXMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.x := fXMin;
    end
    else if (rkPoint.x > fXMax) then
    Begin
      fDelta := rkPoint.x - fXMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.x := fXMax;
    end
    else
    Begin
      pkClosest^.x := rkPoint.x;
    end;

    if (rkPoint.y < fYMin) then
    Begin
      fDelta := rkPoint.y - fYMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.y := fYMin;
    end
    else if (rkPoint.y > fYMax) then
    Begin
      fDelta := rkPoint.y - fYMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.y := fYMax;
    end
    else
    Begin
      pkClosest^.y := rkPoint.y;
    end;

    if (rkPoint.z < fZMin) then
    Begin
      fDelta := rkPoint.z - fZMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.z := fZMin;
    end
    else if (rkPoint.z > fZMax) then
    Begin
      fDelta := rkPoint.z - fZMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
      pkClosest^.z := fZMax;
    end
    else
    Begin
      pkClosest^.z := rkPoint.z;
    end;
  end
  else
  Begin
    if (rkPoint.x < fXMin) then
    Begin
      fDelta := rkPoint.x - fXMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end
    else if (rkPoint.x > fXMax) then
    Begin
      fDelta := rkPoint.x - fXMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end;

    if (rkPoint.y < fYMin) then
    Begin
      fDelta := rkPoint.y - fYMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end
    else if (rkPoint.y > fYMax) then
    Begin
      fDelta := rkPoint.y - fYMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end;

    if (rkPoint.z < fZMin) then
    Begin
      fDelta := rkPoint.z - fZMin;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end
    else if (rkPoint.z > fZMax) then
    Begin
      fDelta := rkPoint.z - fZMax;
      fSqrDistance := fSqrDistance + fDelta * fDelta;
    end;
  end;

  Result := fSqrDistance;
end;

{ -----------------------------------------------------------------------------
  Procedure: Distance
  Author:    pawelp
  Date:      2004-03-31 17:04
  Arguments: const rkPoint : TD3DXVector3; fXMin, fXMax, fYMin, fYMax, fZMin, fZMax : Single; pkClosest : pD3DXVector3
  Result:    Single
  DESCRIPTION:
  Distance 3D Point To 3D Line.
  EXAMPLE:
  ----------------------------------------------------------------------------- }
// ----------------------------------------------------------------------------
Function distance(const rkPoint: TD3DXVector3; fXMin, fXMax, fYMin, fYMax,
  fZMin, fZMax: single; pkClosest: pD3DXVector3): single;
Begin
  Result := sqrt(SqrDistance(rkPoint, fXMin, fXMax, fYMin, fYMax, fZMin, fZMax,
    pkClosest));
end;

// ----------------------------------------------------------------------------
Function TestIntersection(const rkPlane: TPlane; const rkBox: TOBBox): boolean;
var
  fTMP: array [0 .. 2] of single;
  fPseudoDistance, fRadius: single;
Begin
  fTMP[0] := rkBox.RadiusRight * vectordotproduct(rkPlane.Normal, rkBox.vRight);
  fTMP[1] := rkBox.RadiusUp * vectordotproduct(rkPlane.Normal, rkBox.vUp);
  fTMP[2] := rkBox.RadiusDir * vectordotproduct(rkPlane.Normal, rkBox.vDir);

  fRadius := Abs(fTMP[0]) + Abs(fTMP[1]) + Abs(fTMP[2]);

  fPseudoDistance := rkPlane.DistanceToPoint(rkBox.Center);
  Result := Abs(fPseudoDistance) <= fRadius;
End;

// ----------------------------------------------------------------------------
// Tests if the Box is in front of the plane
Function Culled(const rkPlane: TPlane; const rkBox: TOBBox): boolean;
var
  fTMP: array [0 .. 2] of single;
  fPseudoDistance, fRadius: single;
Begin
  fTMP[0] := rkBox.RadiusRight * vectordotproduct(rkPlane.Normal, rkBox.vRight);
  fTMP[1] := rkBox.RadiusUp * vectordotproduct(rkPlane.Normal, rkBox.vUp);
  fTMP[2] := rkBox.RadiusDir * vectordotproduct(rkPlane.Normal, rkBox.vDir);

  fRadius := Abs(fTMP[0]) + Abs(fTMP[1]) + Abs(fTMP[2]);

  fPseudoDistance := rkPlane.DistanceToPoint(rkBox.Center);
  Result := (fPseudoDistance <= -fRadius) // total in front of plane
    OR (Abs(fPseudoDistance) <= fRadius); // Intersects Plane
End;
// ----------------------------------------------------------------------------

Function BoxInBounds(Const Box: TOBBox; Bounds: array of TPlane;
  BoundsCount: integer): boolean;
var
  x: integer;
Begin
  for x := 0 to BoundsCount - 1 do
    if NOT Culled(Bounds[x], Box) then
    Begin
      Result := False;
      exit;
    end;
  Result := True;
end;

// ##############################################################################
// Erstellen einer Ebene (wird für Viewing Frustrum detecting benötigt)
// ##############################################################################
procedure TPlane.Construct(Const p0, p1, p2: TD3DXVector3);
begin
  Normal := vectorcrossproduct(VectorSub(p1, p0), VectorSub(p2, p0));
  distance := -D3DXVec3Dot(Normal, p0);
end;

Procedure TPlane.Construct(Const aNormal, aPoint: TD3DXVector3);
begin
  Normal := aNormal;
  distance := -D3DXVec3Dot(aNormal, aPoint);
end;

// ##############################################################################
// Ebene normalisieren (wird für Viewing Frustrum detecting benötigt)
// ##############################################################################
procedure TPlane.Normalize;
var
  denom: single;
begin
  denom := 1 / sqrt(sqr(Normal.x) + sqr(Normal.y) + sqr(Normal.z));
  Normal.x := Normal.x * denom;
  Normal.y := Normal.y * denom;
  Normal.z := Normal.z * denom;
  distance := distance * denom;
end;

// ##############################################################################
// Gibt Entfernung zu Punkt an (wird für Viewing Frustrum detecting benötigt)
// ##############################################################################
function TPlane.DistanceToPoint(pt: TD3DXVector3): single;
begin
  Result := D3DXVec3Dot(Normal, pt) + distance;
end;

Procedure TransformMinMax(Var Min, Max: TD3DXVector3; Matrix: TD3DXMatrix);
var
  ob1, ob2: TOBBox;
Begin
  ob1.CreateFromMinMax(Min, Max);
  ob2.Transform(ob1, Matrix);
  GetMinMaxFromOBBox(Min, Max, ob2);
end;

/// //////////////////////
/// / Sphere Box Collisions
/// ////////////////////

// ----------------------------------------------------------------------------

// assumption 1: afExtent0 contains only positive values
// if not, need to take the abs of afExtent0 here.
// assumption 2: akAxis contains only normalized values
// if not, need to convert radius to axis coordinates as well
// assumption 3: rfRadius is positive
// if not, need to take its abs before doing calculations
// assumption 4: box is solid

// box and sphere are stationary
Function TestBoxSphrIntersection(const rkBox: TOBBox;
  const rkCen1: TD3DXVector3; const rfRadius1: single): boolean;
var
  kCDiff: TD3DXVector3;
  fDx, fDy, fDz, fAX, fAY, fAZ: single;
Begin
  // box at origin
  kCDiff := VectorSub(rkCen1, rkBox.Center);

  // move sphere into box's coordinate frame
  fAX := Abs(vectordotproduct(rkBox.vRight, kCDiff));
  fAY := Abs(vectordotproduct(rkBox.vUp, kCDiff));
  fAZ := Abs(vectordotproduct(rkBox.vDir, kCDiff));

  fDx := fAX - rkBox.RadiusRight;
  fDy := fAY - rkBox.RadiusUp;
  fDz := fAZ - rkBox.RadiusDir;

  if (fAX <= rkBox.RadiusRight) then
  Begin
    if (fAY <= rkBox.RadiusUp) then
    Begin
      if (fAZ <= rkBox.RadiusDir) then
      Begin
        // sphere center inside box
        Result := True;
        exit;
      End
      else
      Begin
        // potential sphere-face intersection with face z
        Result := (fDz <= rfRadius1);
        exit;
      End;
    End
    else
    Begin
      if (fAZ <= rkBox.RadiusDir) then
      Begin
        // potential sphere-face intersection with face y
        Result := (fDy <= rfRadius1);
        exit;
      End
      else
      Begin
        // potential sphere-edge intersection with edge formed
        // by faces y and z
        Result := (fDy * fDy + fDz * fDz <= rfRadius1 * rfRadius1);
        exit;
      End;
    End;
  End
  else
  Begin
    if (fAY <= rkBox.RadiusUp) then
    Begin
      if (fAZ <= rkBox.RadiusDir) then
      Begin
        // potential sphere-face intersection with face x
        Result := (fDx <= rfRadius1);
        exit;
      End
      else
      Begin
        // potential sphere-edge intersection with edge formed
        // by faces x and z
        Result := (fDx * fDx + fDz * fDz <= rfRadius1 * rfRadius1);
        exit;
      End;
    End
    else
    Begin
      if (fAZ <= rkBox.RadiusDir) then
      Begin
        // potential sphere-edge intersection with edge formed
        // by faces x and y
        Result := (fDx * fDx + fDy * fDy <= rfRadius1 * rfRadius1);
        exit;
      End
      else
      Begin
        // potential sphere-vertex intersection at corner formed
        // by faces x,y,z
        Result := (fDx * fDx + fDy * fDy + fDz * fDz <= rfRadius1 * rfRadius1);
        exit;
      End;
    End;
  End;
End;

// Function GetNearestPoint(Const p : TD3DXVector; Const Box : TOBBox):TD3DXVector3;
// begin
//
// end;

const
  gs_fTolerance = 1E-05;

Function SqrDistance(Const rkLine0, rkLine1: TLine3;
  pfLinP0, pfLinP1: pSingle): single;
var
  kDiff: TD3DXVector3;
  fInvDet, fB1, fS, fT, fSqrDist, fDet, fC, fB0, fA00, fA01, fA11, fA10: single;
Begin
  kDiff := VectorSub(rkLine0.Origin, rkLine1.Origin);
  fA00 := VectorLengthSQRT(rkLine0.Direction);
  fA01 := -vectordotproduct(rkLine0.Direction, rkLine1.Direction);
  fA11 := VectorLengthSQRT(rkLine1.Direction);
  fB0 := vectordotproduct(kDiff, rkLine0.Direction);
  fC := VectorLengthSQRT(kDiff);
  fDet := Abs(fA00 * fA11 - fA01 * fA01);

  if (fDet >= gs_fTolerance) then
  Begin
    // lines are not parallel
    fB1 := -vectordotproduct(kDiff, rkLine1.Direction);
    fInvDet := 1.0 / fDet;
    fS := (fA01 * fB1 - fA11 * fB0) * fInvDet;
    fT := (fA01 * fB0 - fA00 * fB1) * fInvDet;
    fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
      (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
  End
  else
  Begin
    // lines are parallel, select any closest pair of points
    fS := -fB0 / fA00;
    fT := 0.0;
    fSqrDist := fB0 * fS + fC;
  End;

  if (pfLinP0 <> NIL) then
    pfLinP0^ := fS;

  if (pfLinP1 <> NIL) then
    pfLinP1^ := fT;

  Result := Abs(fSqrDist);
End;

// ----------------------------------------------------------------------------
Function SqrDistance(const rkSeg0, rkSeg1: TSegment3;
  pfSegP0, pfSegP1: pSingle): single;
var
  kDiff: TD3DXVector3;
  fInvDet, fB1, fS, fT, fSqrDist, fTMP, fDet, fC, fB0, fA00, fA01, fA11: single;
Begin
  kDiff := VectorSub(rkSeg0.Origin, rkSeg1.Origin);
  fA00 := VectorLengthSQRT(rkSeg0.Direction);
  fA01 := -vectordotproduct(rkSeg0.Direction, rkSeg1.Direction);
  fA11 := VectorLengthSQRT(rkSeg1.Direction);
  fB0 := vectordotproduct(kDiff, rkSeg0.Direction);
  fC := VectorLengthSQRT(kDiff);
  fDet := Abs(fA00 * fA11 - fA01 * fA01);

  if (fDet >= gs_fTolerance) then
  Begin
    // line segments are not parallel
    fB1 := -vectordotproduct(kDiff, rkSeg1.Direction);
    fS := fA01 * fB1 - fA11 * fB0;
    fT := fA01 * fB0 - fA00 * fB1;

    if (fS >= 0.0) then
    Begin
      if (fS <= fDet) then
      Begin
        if (fT >= 0.0) then
        Begin
          if (fT <= fDet) then // region 0 (interior)
          Begin
            // minimum at two interior points of 3D lines
            fInvDet := 1.0 / fDet;
            fS := fS * fInvDet;
            fT := fInvDet;
            fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
              (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
          End
          else // region 3 (side)
          Begin
            fT := 1.0;
            fTMP := fA01 + fB0;
            if (fTMP >= 0.0) then
            Begin
              fS := 0.0;
              fSqrDist := fA11 + 2.0 * fB1 + fC;
            End
            else if (-fTMP >= fA00) then
            Begin
              fS := 1.0;
              fSqrDist := fA00 + fA11 + fC + 2.0 * (fB1 + fTMP);
            End
            else
            Begin
              fS := -fTMP / fA00;
              fSqrDist := fTMP * fS + fA11 + 2.0 * fB1 + fC;
            End;
          End;
        End
        else // region 7 (side)
        Begin
          fT := 0.0;
          if (fB0 >= 0.0) then
          Begin
            fS := 0.0;
            fSqrDist := fC;
          End
          else if (-fB0 >= fA00) then
          Begin
            fS := 1.0;
            fSqrDist := fA00 + 2.0 * fB0 + fC;
          End
          else
          Begin
            fS := -fB0 / fA00;
            fSqrDist := fB0 * fS + fC;
          End;
        End;
      End
      else
      Begin
        if (fT >= 0.0) then
        Begin
          if (fT <= fDet) then // region 1 (side)
          Begin
            fS := 1.0;
            fTMP := fA01 + fB1;
            if (fTMP >= 0.0) then
            Begin
              fT := 0.0;
              fSqrDist := fA00 + 2.0 * fB0 + fC;
            End
            else if (-fTMP >= fA11) then
            Begin
              fT := 1.0;
              fSqrDist := fA00 + fA11 + fC + 2.0 * (fB0 + fTMP);
            End
            else
            Begin
              fT := -fTMP / fA11;
              fSqrDist := fTMP * fT + fA00 + 2.0 * fB0 + fC;
            End;
          End
          else // region 2 (corner)
          Begin
            fTMP := fA01 + fB0;
            if (-fTMP <= fA00) then
            Begin
              fT := 1.0;

              if (fTMP >= 0.0) then
              Begin
                fS := 0.0;
                fSqrDist := fA11 + 2.0 * fB1 + fC;
              End
              else
              Begin
                fS := -fTMP / fA00;
                fSqrDist := fTMP * fS + fA11 + 2.0 * fB1 + fC;
              End;
            End
            else
            Begin
              fS := 1.0;
              fTMP := fA01 + fB1;
              if (fTMP >= 0.0) then
              Begin
                fT := 0.0;
                fSqrDist := fA00 + 2.0 * fB0 + fC;
              End
              else if (-fTMP >= fA11) then
              Begin
                fT := 1.0;
                fSqrDist := fA00 + fA11 + fC + 2.0 * (fB0 + fTMP);
              End
              else
              Begin
                fT := -fTMP / fA11;
                fSqrDist := fTMP * fT + fA00 + 2.0 * fB0 + fC;
              End;
            End;
          End;
        End
        else // region 8 (corner)
        Begin
          if (-fB0 < fA00) then
          Begin
            fT := 0.0;
            if (fB0 >= 0.0) then
            Begin
              fS := 0.0;
              fSqrDist := fC;
            End
            else
            Begin
              fS := -fB0 / fA00;
              fSqrDist := fB0 * fS + fC;
            End;
          End
          else
          Begin
            fS := 1.0;
            fTMP := fA01 + fB1;
            if (fTMP >= 0.0) then
            Begin
              fT := 0.0;
              fSqrDist := fA00 + 2.0 * fB0 + fC;
            End
            else if (-fTMP >= fA11) then
            Begin
              fT := 1.0;
              fSqrDist := fA00 + fA11 + fC + 2.0 * (fB0 + fTMP);
            End
            else
            Begin
              fT := -fTMP / fA11;
              fSqrDist := fTMP * fT + fA00 + 2.0 * fB0 + fC;
            End;
          End;
        End;
      End;
    End
    else
    Begin
      if (fT >= 0.0) then
      Begin
        if (fT <= fDet) then // region 5 (side)
        Begin
          fS := 0.0;
          if (fB1 >= 0.0) then
          Begin
            fT := 0.0;
            fSqrDist := fC;
          End
          else if (-fB1 >= fA11) then
          Begin
            fT := 1.0;
            fSqrDist := fA11 + 2.0 * fB1 + fC;
          End
          else
          Begin
            fT := -fB1 / fA11;
            fSqrDist := fB1 * fT + fC;
          End;
        End
        else
        Begin
          // region 4 (corner)
          fTMP := fA01 + fB0;
          if (fTMP < 0.0) then
          Begin
            fT := 1.0;
            if (-fTMP >= fA00) then
            Begin
              fS := 1.0;
              fSqrDist := fA00 + fA11 + fC + 2.0 * (fB1 + fTMP);
            End
            else
            Begin
              fS := -fTMP / fA00;
              fSqrDist := fTMP * fS + fA11 + 2.0 * fB1 + fC;
            End;
          End
          else
          Begin
            fS := 0.0;
            if (fB1 >= 0.0) then
            Begin
              fT := 0.0;
              fSqrDist := fC;
            End
            else if (-fB1 >= fA11) then
            Begin
              fT := 1.0;
              fSqrDist := fA11 + 2.0 * fB1 + fC;
            End
            else
            Begin
              fT := -fB1 / fA11;
              fSqrDist := fB1 * fT + fC;
            End;
          End;
        End;
      End
      else
      Begin
        // region 6 (corner)
        if (fB0 < 0.0) then
        Begin
          fT := 0.0;
          if (-fB0 >= fA00) then
          Begin
            fS := 1.0;
            fSqrDist := fA00 + 2.0 * fB0 + fC;
          End
          else
          Begin
            fS := -fB0 / fA00;
            fSqrDist := fB0 * fS + fC;
          End;
        End
        else
        Begin
          fS := 0.0;
          if (fB1 >= 0.0) then
          Begin
            fT := 0.0;
            fSqrDist := fC;
          End
          else if (-fB1 >= fA11) then
          Begin
            fT := 1.0;
            fSqrDist := fA11 + 2.0 * fB1 + fC;
          End
          else
          Begin
            fT := -fB1 / fA11;
            fSqrDist := fB1 * fT + fC;
          End;
        End;
      End;
    End;
  End
  else
  Begin
    // line segments are parallel
    if (fA01 > 0.0) then
    Begin
      // direction vectors form an obtuse angle
      if (fB0 >= 0.0) then
      Begin
        fS := 0.0;
        fT := 0.0;
        fSqrDist := fC;
      End
      else if (-fB0 <= fA00) then
      Begin
        fS := -fB0 / fA00;
        fT := 0.0;
        fSqrDist := fB0 * fS + fC;
      End
      else
      Begin
        fB1 := -vectordotproduct(kDiff, rkSeg1.Direction);
        fS := 1.0;
        fTMP := fA00 + fB0;
        if (-fTMP >= fA01) then
        Begin
          fT := 1.0;
          fSqrDist := fA00 + fA11 + fC + 2.0 * (fA01 + fB0 + fB1);
        End
        else
        Begin
          fT := -fTMP / fA01;
          fSqrDist := fA00 + 2.0 * fB0 + fC + fT *
            (fA11 * fT + 2.0 * (fA01 + fB1));
        End;
      End;
    End
    else
    Begin
      // direction vectors form an acute angle
      if (-fB0 >= fA00) then
      Begin
        fS := 1.0;
        fT := 0.0;
        fSqrDist := fA00 + 2.0 * fB0 + fC;
      End
      else if (fB0 <= 0.0) then
      Begin
        fS := -fB0 / fA00;
        fT := 0.0;
        fSqrDist := fB0 * fS + fC;
      End
      else
      Begin
        fB1 := -vectordotproduct(kDiff, rkSeg1.Direction);
        fS := 0.0;
        if (fB0 >= -fA01) then
        Begin
          fT := 1.0;
          fSqrDist := fA11 + 2.0 * fB1 + fC;
        End
        else
        Begin
          fT := -fB0 / fA01;
          fSqrDist := fC + fT * (2.0 * fB1 + fA11 * fT);
        End;
      End;
    End;
  End;

  if (pfSegP0 <> NIL) then
    pfSegP0^ := fS;

  if (pfSegP1 <> NIL) then
    pfSegP1^ := fT;

  Result := Abs(fSqrDist);
End;

{ B
  |\
  |  \
  |____\
  A     C

  rkTri.Origin = A
  rkTri.Edge0 = B-A
  rkTri.Edge1 = C-A

  Triangle points are tri(s,t) = b+s*e0+t*e1 where 0 <= s <= 1,
  0 <= t <= 1, and 0 <= s+t <= 1.

  b = Origin
  e0 = Edge0
  e1 = Edge1
  s = pfSParam (see func Distance)
  t = pfTParam (see func Distance)
}
Function SqrDistance(const rkPoint: TD3DXVector3; const rkTri: Triangle3;
  pfSParam, pfTParam: pSingle): single;
var
  kDiff: TD3DXVector3;
  fSqrDist, fA00, fA01, fA11, fB0, fB1, fC, fDet, fS, fT: single;
  fInvDet, fTmp0, fTmp1, fNumer, fDenom: single;
begin
  kDiff := VectorSub(rkTri.Origin, rkPoint);
  fA00 := VectorLengthSQRT(rkTri.Edge0);
  fA01 := vectordotproduct(rkTri.Edge0, rkTri.Edge1);
  fA11 := VectorLengthSQRT(rkTri.Edge1);
  fB0 := vectordotproduct(kDiff, rkTri.Edge0);
  fB1 := vectordotproduct(kDiff, rkTri.Edge1);
  fC := VectorLengthSQRT(kDiff);
  fDet := Abs(fA00 * fA11 - fA01 * fA01);
  fS := fA01 * fB1 - fA11 * fB0;
  fT := fA01 * fB0 - fA00 * fB1;

  if (fS + fT <= fDet) then
  begin
    if (fS < 0.0) then
    begin
      if (fT < 0.0) then // region 4
      begin
        if (fB0 < 0.0) then
        begin
          fT := 0.0;
          if (-fB0 >= fA00) then
          begin
            fS := 1.0;
            fSqrDist := fA00 + 2.0 * fB0 + fC;
          end
          else
          begin
            fS := -fB0 / fA00;
            fSqrDist := fB0 * fS + fC;
          end;
        end
        else
        begin
          fS := 0.0;
          if (fB1 >= 0.0) then
          begin
            fT := 0.0;
            fSqrDist := fC;
          end
          else if (-fB1 >= fA11) then
          begin
            fT := 1.0;
            fSqrDist := fA11 + 2.0 * fB1 + fC;
          end
          else
          begin
            fT := -fB1 / fA11;
            fSqrDist := fB1 * fT + fC;
          end;
        end;
      end
      else // region 3
      begin
        fS := 0.0;
        if (fB1 >= 0.0) then
        begin
          fT := 0.0;
          fSqrDist := fC;
        end
        else if (-fB1 >= fA11) then
        begin
          fT := 1.0;
          fSqrDist := fA11 + 2.0 * fB1 + fC;
        end
        else
        begin
          fT := -fB1 / fA11;
          fSqrDist := fB1 * fT + fC;
        end;
      end;
    end
    else if (fT < 0.0) then // region 5
    begin
      fT := 0.0;
      if (fB0 >= 0.0) then
      begin
        fS := 0.0;
        fSqrDist := fC;
      end
      else if (-fB0 >= fA00) then
      begin
        fS := 1.0;
        fSqrDist := fA00 + 2.0 * fB0 + fC;
      end
      else
      begin
        fS := -fB0 / fA00;
        fSqrDist := fB0 * fS + fC;
      end;
    end
    else // region 0
    begin
      // minimum at interior point
      fInvDet := 1.0 / fDet;
      fS := fS * fInvDet;
      fT := fT * fInvDet;
      fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
        (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
    end;
  end
  else
  begin
    if (fS < 0.0) then // region 2
    begin
      fTmp0 := fA01 + fB0;
      fTmp1 := fA11 + fB1;
      if (fTmp1 > fTmp0) then
      begin
        fNumer := fTmp1 - fTmp0;
        fDenom := fA00 - 2.0 * fA01 + fA11;
        if (fNumer >= fDenom) then
        begin
          fS := 1.0;
          fT := 0.0;
          fSqrDist := fA00 + 2.0 * fB0 + fC;
        end
        else
        begin
          fS := fNumer / fDenom;
          fT := 1.0 - fS;
          fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
            (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
        end;
      end
      else
      begin
        fS := 0.0;
        if (fTmp1 <= 0.0) then
        begin
          fT := 1.0;
          fSqrDist := fA11 + 2.0 * fB1 + fC;
        end
        else if (fB1 >= 0.0) then
        begin
          fT := 0.0;
          fSqrDist := fC;
        end
        else
        begin
          fT := -fB1 / fA11;
          fSqrDist := fB1 * fT + fC;
        end;
      end;
    end
    else if (fT < 0.0) then // region 6
    begin
      fTmp0 := fA01 + fB1;
      fTmp1 := fA00 + fB0;
      if (fTmp1 > fTmp0) then
      begin
        fNumer := fTmp1 - fTmp0;
        fDenom := fA00 - 2.0 * fA01 + fA11;
        if (fNumer >= fDenom) then
        begin
          fT := 1.0;
          fS := 0.0;
          fSqrDist := fA11 + 2.0 * fB1 + fC;
        end
        else
        begin
          fT := fNumer / fDenom;
          fS := 1.0 - fT;
          fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
            (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
        end;
      end
      else
      begin
        fT := 0.0;
        if (fTmp1 <= 0.0) then
        begin
          fS := 1.0;
          fSqrDist := fA00 + 2.0 * fB0 + fC;
        end
        else if (fB0 >= 0.0) then
        begin
          fS := 0.0;
          fSqrDist := fC;
        end
        else
        begin
          fS := -fB0 / fA00;
          fSqrDist := fB0 * fS + fC;
        end;
      end;
    end
    else // region 1
    begin
      fNumer := fA11 + fB1 - fA01 - fB0;
      if (fNumer <= 0.0) then
      begin
        fS := 0.0;
        fT := 1.0;
        fSqrDist := fA11 + 2.0 * fB1 + fC;
      end
      else
      begin
        fDenom := fA00 - 2.0 * fA01 + fA11;
        if (fNumer >= fDenom) then
        begin
          fS := 1.0;
          fT := 0.0;
          fSqrDist := fA00 + 2.0 * fB0 + fC;
        end
        else
        begin
          fS := fNumer / fDenom;
          fT := 1.0 - fS;
          fSqrDist := fS * (fA00 * fS + fA01 * fT + 2.0 * fB0) + fT *
            (fA01 * fS + fA11 * fT + 2.0 * fB1) + fC;
        end;
      end;
    end;
  end;

  if (pfSParam <> nil) then
    pfSParam^ := fS;

  if (pfTParam <> nil) then
    pfTParam^ := fT;

  Result := Abs(fSqrDist);
end;

// ----------------------------------------------------------------------------
Function distance(const rkPoint: TD3DXVector3; const rkTri: Triangle3;
  pfSParam, pfTParam: pSingle): single;
begin
  Result := sqrt(SqrDistance(rkPoint, rkTri, pfSParam, pfTParam));
end;

{ TOBBox }

procedure TOBBox.CheckMinRadii;
var
  x: integer;
begin
  For x := 0 to 2 do
    IF self.Radii[x] <= METER / 100 then
      Radii[x] := METER / 100;
end;

procedure TOBBox.CopyFrom(const Box: TOBBox);
var
  x: integer;
begin
  Center := Box.Center;
  for x := 0 to 2 do
    AchsenV3[x] := Box.AchsenV3[x];

  for x := 0 to 2 do
    Radii[x] := Box.Radii[x];

  FRadiusSphere := Box.RadiusSphere;
end;

procedure TOBBox.CreateFromMinMax(const Min, Max: TD3DXVector3);
var
  V: TD3DXVector3;
  x: integer;
  s: single;
Begin
  self.Center := VectorDivs(VectorAdd(Min, Max), 2);

  self.AchsenV3[2] := D3DXVector3(0, 0, 1);
  self.AchsenV3[1] := D3DXVector3(0, 1, 0);
  self.AchsenV3[0] := D3DXVector3(1, 0, 0);

  self.Radii[2] := Abs(Max.z - Min.z) / 2;
  self.Radii[1] := Abs(Max.y - Min.y) / 2;
  self.Radii[0] := Abs(Max.x - Min.x) / 2;

  CheckMinRadii;

  UpdateRadiusSphere;
end;

procedure TOBBox.GetEdges(var edge: array of TD3DXVector3);
var
  U, d, uf, ub, df, db, vr, vl: TD3DXVector3;
begin
  U := VectorAdd(Center, VectorMuls(vUp, RadiusUp));
  d := VectorAdd(Center, VectorMuls(vUp, -RadiusUp));

  uf := VectorAdd(U, VectorMuls(vDir, RadiusDir));
  ub := VectorAdd(U, VectorMuls(vDir, -RadiusDir));

  df := VectorAdd(d, VectorMuls(vDir, RadiusDir));
  db := VectorAdd(d, VectorMuls(vDir, -RadiusDir));

  vr := VectorMuls(vRight, RadiusRight);
  vl := VectorMuls(vRight, -RadiusRight);

  edge[0] := VectorAdd(uf, vl);
  edge[1] := VectorAdd(uf, vr);
  edge[2] := VectorAdd(ub, vr);
  edge[3] := VectorAdd(ub, vl);

  edge[4] := VectorAdd(df, vl);
  edge[5] := VectorAdd(df, vr);
  edge[6] := VectorAdd(db, vr);
  edge[7] := VectorAdd(db, vl);

end;

procedure TOBBox.Init(const _Center: TD3DXVector3;
  const _Achsen: array of TD3DXVector3; const _Radii: array of single);
var
  x: integer;
begin
  self.Center := _Center;

  for x := 0 to 2 do
    self.AchsenV3[x] := _Achsen[x];

  for x := 0 to 2 do
    Radii[x] := _Radii[x];
  CheckMinRadii;

  self.UpdateRadiusSphere;
end;

procedure TOBBox.SetRadiusDir(const Value: single);
begin
  Radii[2] := Value;
  UpdateRadiusSphere;
end;

procedure TOBBox.SetRadiusRight(const Value: single);
begin
  Radii[0] := Value;
  UpdateRadiusSphere;
end;

procedure TOBBox.SetRadiusUp(const Value: single);
begin
  Radii[1] := Value;
  UpdateRadiusSphere;
end;

procedure TOBBox.Transform(Const Src: TOBBox; const Matrix: TD3DXMatrix);
var
  x: integer;
  V: TD3DXVector3;
begin
  // self.TransformAndRecalc(src, matrix);
  // exit;

  for x := 0 to 2 do
  begin
    D3DXVec3TransformNormal(self.AchsenV3[x], VectorMuls(Src.AchsenV3[x],
      Src.Radii[x]), Matrix);
    self.Radii[x] := VectorLength(self.AchsenV3[x]);
    self.AchsenV3[x] := vectornormalize(self.AchsenV3[x]);
  end;

  D3DXVec3TransFormCoord(Center, Src.Center, Matrix);
end;

procedure TOBBox.TransformAndRecalc(const SrcBox: TOBBox;
  const Matrix: TD3DXMatrix);
const
  Right = 0;
  Left = 1;
  up = 2;
  down = 3;
  dir = 4;
  back = 5;
  // var
  // x,i: integer;
  // c,v : TD3DXVector3;
  // vertices : array[0..7] of TD3DXVector3;
  // sides : array[0..6] of TD3DXVector3;
  // Min, max : TD3DXVector3;
var
  i: integer;
  c, V: TD3DXVector3;
begin
  c := SrcBox.Center;
  D3DXVec3TransFormCoord(Center, SrcBox.Center, Matrix);

  for i := 0 to 2 do
  Begin
    V := VectorAdd(c, VectorMuls(SrcBox.AchsenV3[i], SrcBox.Radii[i]));
    D3DXVec3TransFormCoord(V, V, Matrix);
    V := VectorSub(V, Center);
    AchsenV3[i] := vectornormalize(V);
    Radii[i] := VectorLength(V);
  end;
  exit;

  // D3DXVec3TransformCoord(self.center, SrcBox.center, Matrix);
  //
  // Sides[right] := VectorMuls(SrcBox.AchsenV3[0], +SrcBox.Radii[0]);
  // Sides[left] := VectorMuls(SrcBox.AchsenV3[0], -SrcBox.Radii[0]);
  //
  //
  // Sides[up] := VectorMuls(SrcBox.AchsenV3[1], +SrcBox.Radii[1]);
  // Sides[down] := VectorMuls(SrcBox.AchsenV3[1], -SrcBox.Radii[1]);
  //
  // Sides[dir] := VectorMuls(SrcBox.AchsenV3[2], +SrcBox.Radii[2]);
  // Sides[back] := VectorMuls(SrcBox.AchsenV3[2], -SrcBox.Radii[2]);
  //
  // Vertices[0] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[dir], VectorAdd(Sides[up], Sides[right])),3) );
  // Vertices[1] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[back], VectorAdd(Sides[up], Sides[right])),3) );
  // Vertices[2] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[back], VectorAdd(Sides[up], Sides[left])),3) );
  // Vertices[3] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[dir], VectorAdd(Sides[up], Sides[left])),3) );
  //
  // Vertices[4] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[dir], VectorAdd(Sides[up], Sides[right])),3) );
  // Vertices[5] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[back], VectorAdd(Sides[up], Sides[right])),3) );
  // Vertices[6] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[back], VectorAdd(Sides[up], Sides[left])),3) );
  // Vertices[7] := VectorAdd(SrcBox.center, VectorDivs( VectorAdd(Sides[dir], VectorAdd(Sides[up], Sides[left])),3) );
  //
  // for x := 0 to 7 do
  // D3DXVec3TransformCoord(Vertices[x], Vertices[x], Matrix);
  //
  // Min := Vertices[0];
  // max := Vertices[0];
  // for x := 1 to 7 do
  // begin
  // D3Dx9.D3DXVec3Maximize(max, max, vertices[x]);
  // D3Dx9.D3DXVec3Minimize(Min, Min, vertices[x]);
  // end;
  //
  // CreateFromMinMax(min, max);
end;

procedure TOBBox.UpdateRadiusSphere;
var
  V: TD3DXVector3;
begin
  FRadiusSphere := VectorLength(VectorAdd(VectorMuls(vRight, RadiusRight),

    VectorAdd(VectorMuls(vDir, RadiusDir), VectorMuls(vUp, RadiusUp))));
end;




// ----------------------------------------------------------------------------

procedure Projection(const rkD: TD3DXVector3; akV: array of TD3DXVector3;
  var rfMin, rfMax: single);
var
  afDot: array [0 .. 3] of single;
begin
  afDot[0] := vectordotproduct(rkD, akV[0]);
  afDot[1] := vectordotproduct(rkD, akV[1]);
  afDot[2] := vectordotproduct(rkD, akV[2]);

  rfMin := afDot[0];
  rfMax := rfMin;

  if (afDot[1] < rfMin) then
    rfMin := afDot[1]
  else if (afDot[1] > rfMax) then
    rfMax := afDot[1];

  if (afDot[2] < rfMin) then
    rfMin := afDot[2]
  else if (afDot[2] > rfMax) then
    rfMax := afDot[2];
end;

function TestIntersection(const akU, akV: array of TD3DXVector3): boolean;
const
  fEpsilon = 1E-06; // cutoff: sin(Angle(N,M)) < epsilon
var
  akF, akE: array [0 .. 3] of TD3DXVector3;
  kNxM, kM, kN, kDir: TD3DXVector3;
  fMdV0, fNdU0, fUMin, fUMax, fVMin, fVMax: single;
  i0, i1: integer;
begin
  // direction N

  akE[0] := VectorSub(akU[1], akU[0]);
  akE[1] := VectorSub(akU[2], akU[1]);
  akE[2] := VectorSub(akU[0], akU[2]);

  kN := vectorcrossproduct(akE[0], akE[1]);
  fNdU0 := vectordotproduct(kN, akU[0]);
  Projection(kN, akV, fVMin, fVMax);
  if (fNdU0 < fVMin) or (fNdU0 > fVMax) then
  begin
    Result := False;
    exit;
  end;

  // direction M
  akF[0] := VectorSub(akV[1], akV[0]);
  akF[1] := VectorSub(akV[2], akV[1]);
  akF[2] := VectorSub(akV[0], akV[2]);
  kM := vectorcrossproduct(akF[0], akF[1]);

  kNxM := vectorcrossproduct(kN, kM);
  if (vectordotproduct(kNxM, kNxM) >= (fEpsilon * vectordotproduct(kN,
    VectorMuls(kN, vectordotproduct(kM, kM))))) then
  begin
    // triangles are not parallel
    fMdV0 := vectordotproduct(kM, akV[0]);
    Projection(kM, akU, fUMin, fUMax);
    if (fMdV0 < fUMin) or (fMdV0 > fUMax) then
    begin
      Result := False;
      exit;
    end;

    // directions E[i0]xF[i1]
    for i1 := 0 to 2 do
    begin
      for i0 := 0 to 2 do
      begin
        kDir := vectorcrossproduct(akE[i0], akF[i1]);
        Projection(kDir, akU, fUMin, fUMax);
        Projection(kDir, akV, fVMin, fVMax);
        if (fUMax < fVMin) or (fVMax < fUMin) then
        begin
          Result := False;
          exit;
        end;
      end;
    end;
  end
  else // triangles are parallel (and, in fact, coplanar)
  begin
    // directions NxE[i0]
    for i0 := 0 to 2 do
    begin
      kDir := vectorcrossproduct(kN, akE[i0]);
      Projection(kDir, akU, fUMin, fUMax);
      Projection(kDir, akV, fVMin, fVMax);
      if (fUMax < fVMin) or (fVMax < fUMin) then
      begin
        Result := False;
        exit;
      end;
    end;

    // directions NxF[i1]
    for i1 := 0 to 2 do
    begin
      kDir := vectorcrossproduct(kM, akF[i1]);
      Projection(kDir, akU, fUMin, fUMax);
      Projection(kDir, akV, fVMin, fVMax);
      if (fUMax < fVMin) or (fVMax < fUMin) then
      begin
        Result := False;
        exit;
      end;
    end;
  end;

  Result := True;
end;

function PointInRect(const point: TPoint; const Rect: TRect): boolean;
begin
  Result := (point.x >= Rect.Left) and (point.x <= Rect.Right) and
    (point.y >= Rect.Top) and (point.y <= Rect.Bottom);
end;

Function RectIntersection(const r1, r2: TRect): boolean;
begin
  Result := not((r1.BottomRight.x < r2.TopLeft.x) or
    (r1.BottomRight.y < r2.TopLeft.y) or (r2.BottomRight.x < r1.TopLeft.x) or
    (r2.BottomRight.y < r1.TopLeft.y));
end;

function RectInRect(const Rect1, Rect2: TRect): boolean;
begin
  Result := (Rect1.Left >= Rect2.Left) and (Rect1.Right <= Rect2.Right) and
    (Rect1.Top >= Rect2.Top) and (Rect1.Bottom <= Rect2.Bottom);
end;

end.
