unit AdvancedShapes;
{ *******************************************************

  Digital Omerta 3D Engine

  Unit Author:   Pawel Piotrowski
  Copyright 	    Pawel Piotrowski

  Version: 3.9
  History
  2018-10-06: shapes can be cloned now
  2013-10-11L The Ellipse now uses a bit matrix to store its area, previously it was stored in a monochrome bitmap. The Bitmatrix ist around 1/2 of the memory size and around 30 times faster.
  2013-10-09:
  - Polygons can now be open, see the Closed property.
  - The Point in non convex polygon function is improved and faster
  2013-07-22:
  * Added Properties: name, Userobject, UserInterface
  * fixed a bug in calculating the center of the shape
  2013-06-18: added a function OverlapWithRect, to implement a fast rect overlaping check
  2013-06-05: aded TadvShape.tag: integer property }

{$I JEDI.INC}


{ Version: 1
  History:
  2013-05-22: moved all geometry related code from the ExperimentData.pas here }
interface

uses
  Windows, Classes, SysUtils,
  XMLLib,
  Graphics, D3DX9, Generics.Collections, max3DMath;

type
  TControlPoint = (cpNone, cpMove, cpTop, cpLeft, cpBottom, cpRight, cpTopLeft, cpTopRight, cpBottomleft, cpBottomRight, cpPolyPoint, cpPolyHalfPoint);

  TPointArray = TArray<Tpoint>;
  TShapeKind = (skRect = 0, skEllipse = 1, skPolygon = 2);

  // forward declrations
  TBitMatrix = class;
  TEllipse = class;
  TAdvPolygon = class;

  TAdvShape = class
  private
    FTag: integer;
    Fname: string;
    FUserInterface: iInterface;
    FOwnsUserObject: Boolean;
    FUserObject: TObject;
    function GetCenter: TD3DXVector2;
    procedure SetTyp(const Value: TShapeKind);
    procedure SetTag(const Value: integer);
    procedure Setname(const Value: string);
    procedure SetOwnsUserObject(const Value: Boolean);
    procedure SetUserInterface(const Value: iInterface);
    procedure SetUserObject(const Value: TObject);
    function GetControlPoint(cp: TControlPoint): Tpoint;
    function GetCenter2d: Tpoint;
  protected
    fTyp: TShapeKind;
    fMinMax: Trect;
    procedure SetRect(const Value: Trect); virtual;

    procedure LoadFromNode(node: TXMLNode); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    class Function CreateRectShape(const r: Trect): TAdvShape;
    class Function CreateEllipseShape(const r: Trect): TEllipse;
    class Function CreatePolygonShape(const aPoints: TPointArray; PolyIsClosed: Boolean = true): TAdvPolygon;

    // note: will clone the userObject oly if it is not owned
    function Clone: TAdvShape; virtual;

    procedure SaveToXMLNode(node: TXMLNode); virtual;
    class function CreateFromXMLNode(node: TXMLNode): TAdvShape;

    Function PointInside(const p: Tpoint): Boolean; virtual;

    // this functions returns the smallest distance, or 0 if the point or poly is inside the bounds
    Function Distance(const p: Tpoint): single; overload;
    Function Distance(Bounds: TAdvShape): single; overload;

    function DistanceToPoint(const p: Tpoint): single; virtual;
    Function DistanceToShape(Shape: TAdvShape): single;

    Function GetNearestPoint2d(const p: Tpoint): TD3DXVector2; virtual;
    Function GetNearestPoint(const p: Tpoint): Tpoint;

    Function OverlapWithRect(const r: Trect): Boolean;

    function CalcDistanceToRect(const r: Trect): single; virtual;
    function CalcDistanceToEllipse(const aEllipse: TEllipse): single; virtual;
    function CalcDistanceToPolygon(Poly: TAdvPolygon): single; virtual;

    property Typ: TShapeKind Read fTyp write SetTyp;
    // THIS ONE WILL RETURN A BOUNDING RECT. depending on the type of the Zone, it will either will be the rect itself or the MinMax Rect of the circle or poly
    property Rect: Trect Read fMinMax Write SetRect;

    property Center: TD3DXVector2 read GetCenter;
    property Center2d: Tpoint read GetCenter2d;

    Property ControlPoint[cp: TControlPoint]: Tpoint read GetControlPoint;
    // for storing of custom user data
    property Tag: integer read FTag write SetTag;
    property name: string read Fname write Setname;
    property UserObject: TObject read FUserObject write SetUserObject;
    property OwnsUserObject: Boolean read FOwnsUserObject write SetOwnsUserObject;
    property UserInterface: iInterface read FUserInterface write SetUserInterface;
  end;

  TAdvPolygon = class(TAdvShape)
  private
    fPoints: TPointArray;
    fClosed: Boolean;
    procedure SetClosed(const Value: Boolean);
    function GetClosed: Boolean;
  protected
    procedure SetRect(const Value: Trect); override;
    procedure LoadFromNode(node: TXMLNode); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure AfterChanged;

    procedure SaveToXMLNode(node: TXMLNode); override;
    procedure InsertPoint(index: integer; const p: Tpoint);
    procedure DeletePoint(index: integer);

    Function PointInside(const p: Tpoint): Boolean; override;
    Function DistanceToPoint(const p: Tpoint): single; override;

    function CalcDistanceToRect(const r: Trect): single; override;
    function CalcDistanceToEllipse(const aEllipse: TEllipse): single; override;
    function CalcDistanceToPolygon(Poly: TAdvPolygon): single; override;

    Function GetNearestPoint2d(const p: Tpoint): TD3DXVector2; override;

    property Points: TPointArray read fPoints;

    property Closed: Boolean read GetClosed write SetClosed;
  end;

  TEllipse = class(TAdvShape)
  private
    fBitMatrix: TBitMatrix;

    fAsPolygon: TAdvPolygon;
    procedure CalculateValuesFromRect(const r: Trect);
    procedure PrepareMap;
    procedure PreparePolygon;
  protected
    procedure SetRect(const Value: Trect); override;
  public

    // semi major axis
    a: single;
    // major axis
    _2a: integer;
    // semi minor axis
    b: single;
    // semi axis
    _2b: integer;
    Center: TD3DXVector2;
    // distance from center to the foci points
    c: single;
    // eccentricity
    e: single;

    constructor Create;
    destructor Destroy; override;

    function Width: integer;
    function height: integer;
    function IsUnitCircle: Boolean;

    function Foci1: TD3DXVector2;
    function foci2: TD3DXVector2;
    Function Vertex1: TD3DXVector2;
    Function Vertex2: TD3DXVector2;
    function coVertex1: TD3DXVector2;
    Function CoVertex2: TD3DXVector2;

    Function Radius: single;
    function RatioWidthToHeight: single;

    Function PointInside(const WorldCoordPoint: Tpoint): Boolean; override;
    Function PointInside2d(const WorldCoordPoint: TD3DXVector2): Boolean;
    Function DistanceToPoint(const p: Tpoint): single; override;

    function CalcDistanceToRect(const r: Trect): single; override;
    function CalcDistanceToEllipse(const aEllipse: TEllipse): single; override;
    function CalcDistanceToPolygon(Poly: TAdvPolygon): single; override;

    Function GetNearestPoint2d(const p: Tpoint): TD3DXVector2; override;

    property AsPolygon: TAdvPolygon read fAsPolygon;
  end;

  TBitMatrix = class
  private
    fBuffer: pByte;
    fSize: integer;
    fWidth: integer;
    fHeight: integer;
    function GetValue(x, y: integer): Boolean;
    procedure SetValue(x, y: integer; const Value: Boolean);
    Function RoundUp(const Value: double): integer;
    function GetByte(x, y: integer; out pb: pByte; out iBit: byte): Boolean;
  public
    Constructor Create;
    Destructor Destroy; override;
    // black is false, any other color is true
    Procedure BuildFromBitMap(bmp: TBitMap; TruColor: tcolor);
    procedure Clear(NewWidth, NewHeight: integer);

    property Value[x, y: integer]: Boolean read GetValue write SetValue;
  end;

  TLine = record
    StartPoint, EndPoint: Tpoint;
  end;

Procedure RectToPoints(const r: Trect; out Points: TPointArray);

Function DistanceToRect(const p: Tpoint; const r: Trect): single;

Function DistanceToSegment(const p: Tpoint; const LineStartPoint, LineEndPoint: Tpoint): single; overload;
Function DistanceToSegment(const vp, LineStartPoint, LineEndPoint: TD3DXVector2): single; overload;

// will return 0 if there is an overlaping or if the one is in the other
Function DistanceSegment2Segment(const Seg1Start, Seg1End, Seg2Start, Seg2End: Tpoint): single;
Function DistanceSegmentToSegments(const s1, s2: Tpoint; const Points: TPointArray): single;
Function DistanceSegmentsToSegments(const Points1, Points2: TPointArray; Poly1IsClosed, Poly2isClosed: Boolean): single;
function distPointToSegments(const p: TD3DXVector2; const Points: TPointArray; PolyIsClosed: Boolean): single;
function DistRect2Rect(const r1, r2: Trect): single;

function ClosestPointOnLine(Const LineStart, LineEnd: Tpoint; const vPoint: TD3DXVector2): TD3DXVector2; overload;
function ClosestPointOnLine(Const LineStart, LineEnd, vPoint: Tpoint): TD3DXVector2; overload;

Function Vector2(const p: Tpoint): TD3DXVector2; overload;
Function Vector2(const x, y: integer): TD3DXVector2; overload;
Function Vector2(const x, y: single): TD3DXVector2; overload;
function Vector2ToPoint(const p: TD3DXVector2): Tpoint;

// It returns 1 for strictly interior points, 0 for strictly exterior, and 0 or 1 for points on the boundary.
function PointInPolygon(var xp, yp: array of single; x, y: single): Boolean;

function PointInRect(const Point: Tpoint; const Rect: Trect): Boolean;
// this will calculate the angle in degrees between the View point and the object. Or in other words the angle between the two lines Body <-> head and body <-> Object
function getViewpoint(ptBody, ptHead: Tpoint; const ptObjectCenter: TD3DXVector2): single;
Function Equal(const p1, p2: Tpoint): Boolean;

implementation

uses

  GDIPOBJ, GDIPAPI,
  pawel1, bsUTILS, Dialogs, zLib, math, max3DCollision, ClipBrd, XmlLibHelper, MaxLogic.QuickPixel, mmsystem;

function getViewpoint(ptBody, ptHead: Tpoint; const ptObjectCenter: TD3DXVector2): single;
begin
  // if one of the lines (head <-> body or body <-> objectCenter) is invalid, then we can't calculate the angle
  if (ptHead = ptBody) or
    D3DXVector2Equal(D3DXVector2(ptBody.x, ptBody.y), ptObjectCenter) then
    exit(0);

  result := max3DMath.VertexAngle(
    ptHead.x, ptHead.y,
    ptBody.x, ptBody.y,
    ptObjectCenter.x, ptObjectCenter.y);

end;

function DistRect2Rect(const r1, r2: Trect): single;
var
  Points1, Points2: TPointArray;
  x: integer;
begin
  result := 0;
  if RectIntersection(r1, r2) then
    exit;

  RectToPoints(r1, Points1);
  RectToPoints(r2, Points2);

  result := DistanceSegmentsToSegments(Points1, Points2, true, true);

  Points1 := NIL;
  Points2 := NIL;
end;

function distPointToSegments(const p: TD3DXVector2; const Points: TPointArray; PolyIsClosed: Boolean): single;
var
  x: integer;
  dist: single;
  v: TD3DXVector2;
  s1, s2: Tpoint;
begin
  result := 0;
  for x := 0 to Length(Points) - 1 do
  begin
    s1 := Points[x];
    if x + 1 < Length(Points) then
      s2 := Points[x + 1]
    else
    begin
      if not PolyIsClosed then
        break
      else
        s2 := Points[0];
    end;

    v := ClosestPointOnLine(s1, s2, p);
    dist := VectorDistance(v, p);
    if (x = 0) or (result > dist) then
      result := dist;
  end;

end;

Function DistanceSegmentsToSegments(const Points1, Points2: TPointArray; Poly1IsClosed, Poly2isClosed: Boolean): single;
var
  x, y: integer;
  dist: single;
  s1, s2, s3, s4: Tpoint;
begin
  result := 0;
  for x := 0 to Length(Points1) - 1 do
  begin
    s1 := Points1[x];
    if x + 1 < Length(Points1) then
      s2 := Points1[x + 1]
    else
    begin
      if not Poly1IsClosed then
        break
      else
        s2 := Points1[0];
    end;

    for y := 0 to Length(Points2) - 1 do
    begin
      s3 := Points2[y];
      if y + 1 < Length(Points2) then
        s4 := Points2[y + 1]
      else
      begin
        if not Poly2isClosed then
          break
        else
          s4 := Points2[0];
      end;
      dist := DistanceSegment2Segment(s1, s2, s3, s4);
      if ((x = 0) and (y = 0)) OR (dist < result) then
        result := dist;

    end;
  end;
end;

Function DistanceSegmentToSegments(const s1, s2: Tpoint; const Points: TPointArray): single;
var
  y: integer;
  dist: single;
  s3, s4: Tpoint;
begin
  result := 0;

  for y := 0 to Length(Points) - 1 do
  begin
    s3 := Points[y];
    if y + 1 < Length(Points) then
      s4 := Points[y + 1]
    else
      s4 := Points[0];
    dist := DistanceSegment2Segment(s1, s2, s3, s4);
    if (y = 0) OR (dist < result) then
      result := dist;

  end;
end;

Function DistanceSegment2Segment(const Seg1Start, Seg1End, Seg2Start, Seg2End: Tpoint): single;
var
  s1, s2, s3, s4, v1: TD3DXVector2;
  dist: single;
begin
  s1 := Vector2(Seg1Start);
  s2 := Vector2(Seg1End);
  s3 := Vector2(Seg2Start);
  s4 := Vector2(Seg2End);
  result := 0;
  if Intersect(s1, s2, s3, s4) then
    exit;

  v1 := ClosestPointOnLine(s1, s2, s3);
  result := VectorDistance(v1, s3);

  v1 := ClosestPointOnLine(s1, s2, s4);
  dist := VectorDistance(v1, s4);
  if dist < result then
    result := dist;

  // check the other line with the end point of the first one
  v1 := ClosestPointOnLine(s3, s4, s1);
  dist := VectorDistance(v1, s1);
  if dist < result then
    result := dist;

  v1 := ClosestPointOnLine(s3, s4, s2);
  dist := VectorDistance(v1, s2);
  if dist < result then
    result := dist;
end;

Procedure RectToPoints(const r: Trect; out Points: TPointArray);
begin
  setlength(Points, 4);
  Points[0] := r.TopLeft;
  Points[1] := Point(r.Right, r.Top);
  Points[2] := r.BottomRight;
  Points[3] := Point(r.left, r.Bottom);
end;

Function Equal(const p1, p2: Tpoint): Boolean;
begin
  result := (p1.x = p2.x) and
    (p1.y = p2.y)
end;

Function DistanceToRect(const p: Tpoint; const r: Trect): single;
var
  dist: single;
  x: integer;
begin
  if PointInRect(p, r) then
    result := 0
  else
  begin
    result := DistanceToSegment(p,
      Point(r.left, r.Top),
      Point(r.Right, r.Top));
    dist := DistanceToSegment(p,
      Point(r.Right, r.Top),
      Point(r.Right, r.Bottom));
    if dist < result then
      result := dist;

    dist := DistanceToSegment(p,
      Point(r.Right, r.Bottom),
      Point(r.left, r.Bottom));
    if dist < result then
      result := dist;

    dist := DistanceToSegment(p,
      Point(r.left, r.Bottom),
      Point(r.left, r.Top));
    if dist < result then
      result := dist;

  end;
end;

function PointInRect(const Point: Tpoint; const Rect: Trect): Boolean;
begin
  result := (Point.x >= Rect.left) and
    (Point.x <= Rect.Right) and
    (Point.y >= Rect.Top) and
    (Point.y <= Rect.Bottom);
end;

Function DistanceToSegment(const p: Tpoint; const LineStartPoint, LineEndPoint: Tpoint): single;
begin
  result := DistanceToSegment(Vector2(p), Vector2(LineStartPoint), Vector2(LineEndPoint));
end;

Function DistanceToSegment(const vp, LineStartPoint, LineEndPoint: TD3DXVector2): single;
var
  NearestPoint: TD3DXVector2;
begin
  NearestPoint := ClosestPointOnLine(LineStartPoint, LineEndPoint, vp);
  result := VectorDistance(NearestPoint, vp);
end;

function ClosestPointOnLine(Const LineStart, LineEnd, vPoint: Tpoint): TD3DXVector2;
begin
  result := max3DMath.ClosestPointOnLine(Vector2(LineStart), Vector2(LineEnd),
    Vector2(vPoint));
end;

function ClosestPointOnLine(Const LineStart, LineEnd: Tpoint; const vPoint: TD3DXVector2): TD3DXVector2;
begin
  result := max3DMath.ClosestPointOnLine(Vector2(LineStart), Vector2(LineEnd),
    vPoint);
end;

function PointInPolygon(var xp, yp: array of single; x, y: single): Boolean;
// It returns 1 for strictly interior points, 0 for strictly exterior, and 0 or 1 for points on the boundary.
var
  I, J: integer;
begin

  result := False;
  if High(xp) = High(yp) then
  begin
    J := High(xp);
    for I := 0 to High(xp) do
    begin
      if ((((yp[I] <= y) and (y < yp[J])) or ((yp[J] <= y) and (y < yp[I])))
        and (x < (xp[J] - xp[I]) * (y - yp[I]) / (yp[J] - yp[I]) + xp[I])) then
        result := not result;
      J := I;
    end;
  end;
end;

Function Vector2(const p: Tpoint): TD3DXVector2; overload;
begin
  with p do
  begin
    result.x := x;
    result.y := y;
  end;
end;

function Vector2ToPoint(const p: TD3DXVector2): Tpoint;
begin
  result.x := round(p.x);
  result.y := round(p.y);
end;

Function Vector2(const x, y: single): TD3DXVector2;
begin
  result.x := x;
  result.y := y;
end;

Function Vector2(const x, y: integer): TD3DXVector2; overload;
begin
  result.x := x;
  result.y := y;
end;

{ TEllipse }

function TEllipse.CalcDistanceToEllipse(const aEllipse: TEllipse): single;
begin
  result := 0;

  if self.IsUnitCircle and aEllipse.IsUnitCircle then
  begin
    result := VectorDistance(self.Center, aEllipse.Center);
    result := result - (self.Radius + aEllipse.Radius);
    if result < 0 then
      result := 0;
  end
  else
    if (not self.IsUnitCircle) and (not aEllipse.IsUnitCircle) then
    result := self.AsPolygon.CalcDistanceToPolygon(aEllipse.AsPolygon)
  else
    if (self.IsUnitCircle) and (not aEllipse.IsUnitCircle) then
    result := self.CalcDistanceToPolygon(aEllipse.AsPolygon)
  else
    if (not self.IsUnitCircle) and (aEllipse.IsUnitCircle) then
    result := aEllipse.CalcDistanceToPolygon(self.AsPolygon);
end;

function TEllipse.CalcDistanceToPolygon(Poly: TAdvPolygon): single;
begin
  result := Poly.CalcDistanceToEllipse(self)
end;

function TEllipse.CalcDistanceToRect(const r: Trect): single;
var
  Poly: TAdvPolygon;
  dist: single;
begin
  result := 0;
  if PointInRect(Center2d, r) then
    exit;

  Poly := TAdvPolygon.Create;
  result := CalcDistanceToPolygon(Poly);
  Poly.Free;
end;

procedure TEllipse.CalculateValuesFromRect;
var
  c2: single;
begin
  if Width > height then
  begin
    _2a := Width;
    _2b := height;
  end else begin
    _2a := height;
    _2b := Width;
  end;

  a := _2a / 2;
  b := _2b / 2;

  Center.x := r.left + Width / 2;
  Center.y := r.Top + height / 2;


  // calculate now c
  // a2 – c2 = b2 or
  // b2 + c2 = a2 or
  // c2 = a2 - b2

  c2 := (a * a) - (b * b);
  c := sqrt(c2);

  // eccentricity
  // e = c/a.
  e := c / a;

end;

function TEllipse.coVertex1: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.y := result.y - b
  else
    result.x := result.x - b
end;

function TEllipse.CoVertex2: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.y := result.y + b
  else
    result.x := result.x + b
end;

constructor TEllipse.Create;
begin
  inherited;
  fTyp := skEllipse;
end;

destructor TEllipse.Destroy;
begin
  fAsPolygon.Free;
  fBitMatrix.Free;
  inherited;
end;

procedure TEllipse.PreparePolygon;
var
  CirclePoints: array of TD3DXVector2;
  x, y: integer;
  v: TD3DXVector2;
  Angle: double;
  pt: Tpoint;

  EllipsePointCount: integer;
  ratio: TD3DXVector2;

  procedure Add(v: TD3DXVector2);
  var
    len: integer;
  begin
    // scale, to squich it into the elipse
    v := Vectormul(v, ratio);
    // now move it from the 0,0 origin to the position of the ellipse
    v := Vectoradd(v, self.Center);

    pt := Vector2ToPoint(v);
    len := Length(fAsPolygon.fPoints);
    if len = 0 then
    begin
      setlength(fAsPolygon.fPoints, 1);
      fAsPolygon.fPoints[0] := pt;
    end else begin
      if not Equal(pt, fAsPolygon.fPoints[len - 1]) then
        if not Equal(pt, fAsPolygon.fPoints[0]) then
        begin
          setlength(fAsPolygon.fPoints, len + 1);
          fAsPolygon.fPoints[len] := pt;
        end;
    end;

  end;

var
  d: dword;
  ia, StepCount: integer;
  IncStep: double;
begin
  d := GetTickCount;
  if not Assigned(fAsPolygon) then
    fAsPolygon := TAdvPolygon.Create;

  setlength(fAsPolygon.fPoints, 0);

  if Width > height then
  begin
    ratio.x := 1;
    ratio.y := height / Width;
  end else begin
    ratio.y := 1;
    ratio.x := Width / height;
  end;

  StepCount := Max(Width, height) * 2;
  if StepCount < 360 then
    StepCount := 360;

  IncStep := 360 / StepCount;
  for ia := 0 to StepCount do
  begin
    Angle := IncStep * ia;
    v := Vector2(0, a);
    v := Rotate(Angle, v);

    Add(v);
  end;

  d := GetTickCount - d;
  // Clipboard.AsText := IntToStr(d);
  fAsPolygon.AfterChanged;
end;

procedure TEllipse.PrepareMap;
var
  bmp: TBitMap;
begin
  bmp := TBitMap.Create;
  bmp.Monochrome := true;
  bmp.SetSize(Width, height);

  fBitMatrix := TBitMatrix.Create;

  bmp.Canvas.Pen.Color := clwhite;
  bmp.Canvas.Brush.Color := clwhite;
  bmp.Canvas.FillRect(Trect.Create(0, 0, bmp.Width, bmp.height));

  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Ellipse(Trect.Create(0, 0, Width, height));

  fBitMatrix.BuildFromBitMap(bmp, clBlack);
  bmp.Free;
end;

function TEllipse.Foci1: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.x := result.x - c
  else
    result.y := result.y - c;

end;

function TEllipse.foci2: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.x := result.x + c
  else
    result.y := result.y + c;
end;

function TEllipse.GetNearestPoint2d(const p: Tpoint): TD3DXVector2;
begin
  result := AsPolygon.GetNearestPoint2d(p);
end;

function TEllipse.height: integer;
begin
  result := fMinMax.height;
end;

function TEllipse.IsUnitCircle: Boolean;
begin
  result := Width = height;
end;

function TEllipse.PointInside(const WorldCoordPoint: Tpoint): Boolean;
var
  x, y: integer;
begin
  result := inherited PointInside(WorldCoordPoint);
  if result then
  begin
    x := WorldCoordPoint.x - fMinMax.left;
    y := WorldCoordPoint.y - fMinMax.Top;

    if (x >= 0) and (x < Width) and (y >= 0) and (y < height) then
      result := fBitMatrix.Value[x, y]
    else
      result := False;
  end;
end;

Function TEllipse.DistanceToPoint(const p: Tpoint): single;
begin
  result := 0;
  if PointInside(p) then
    exit(0);

  if IsUnitCircle then
  begin
    result := VectorDistance(Center, Vector2(p));
    result := result - Radius;
    if result < 0 then
      result := 0;
  end else begin
    // it is not a unit circle, now it is an ellipse
    result := AsPolygon.DistanceToPoint(p);
  end;
end;

function TEllipse.PointInside2d(const WorldCoordPoint: TD3DXVector2): Boolean;
var
  p: Tpoint;
begin
  with WorldCoordPoint do
  begin
    p.x := round(x);
    p.y := round(y);
  end;
  result := PointInside(p);
end;

function TEllipse.Radius: single;
begin
  result := Width / 2;
end;

function TEllipse.RatioWidthToHeight: single;
begin
  result := Width / height;
end;

procedure TEllipse.SetRect(const Value: Trect);
begin
  inherited;

  CalculateValuesFromRect(Value);
  PrepareMap;
  PreparePolygon;
end;

function TEllipse.Vertex1: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.x := result.x - a
  else
    result.y := result.y - a
end;

function TEllipse.Vertex2: TD3DXVector2;
begin
  result := Center;
  if Width > height then
    result.x := result.x + a
  else
    result.y := result.y + a
end;

function TEllipse.Width: integer;
begin
  result := fMinMax.Width;
end;

{ TZoneBounds2 }

function TAdvShape.CalcDistanceToPolygon(Poly: TAdvPolygon): single;
begin
  result := Poly.CalcDistanceToRect(self.Rect)
end;

function TAdvShape.CalcDistanceToRect(const r: Trect): single;
begin
  result := DistRect2Rect(r, self.Rect);
end;

function TAdvShape.CalcDistanceToEllipse(const aEllipse: TEllipse): single;
begin
  result := aEllipse.CalcDistanceToRect(self.Rect);
end;

constructor TAdvShape.Create;
begin
  inherited;
  fTyp := skRect;
end;

class function TAdvShape.CreateEllipseShape(const r: Trect): TEllipse;
begin
  result := TEllipse.Create;
  result.Rect := r;
end;

class function TAdvShape.CreatePolygonShape(const aPoints: TPointArray; PolyIsClosed: Boolean = true): TAdvPolygon;
var
  x: integer;
begin
  result := TAdvPolygon.Create;
  setlength(result.fPoints, Length(aPoints));
  for x := 0 to Length(aPoints) - 1 do
    result.fPoints[x] := aPoints[x];

  if result.fPoints[0] = result.fPoints[Length(result.fPoints) - 1] then
    setlength(result.fPoints, Length(result.fPoints) - 1);

  result.Closed := PolyIsClosed;
  result.AfterChanged;
end;

class function TAdvShape.CreateRectShape(const r: Trect): TAdvShape;
begin
  result := TAdvShape.Create;
  result.Rect := r;
end;

destructor TAdvShape.Destroy;
begin

  if Assigned(FUserObject) then
    if FOwnsUserObject then
      FreeAndNIL(FUserObject);

  inherited;
end;

function TAdvShape.DistanceToPoint(const p: Tpoint): single;
begin
  result := DistanceToRect(p, self.Rect);
end;

function TAdvShape.DistanceToShape(Shape: TAdvShape): single;
begin
  case Shape.Typ of
    skRect:
      result := CalcDistanceToRect(Shape.Rect);
    skEllipse:
      result := CalcDistanceToEllipse(TEllipse(Shape));
    skPolygon:
      result := CalcDistanceToPolygon(TAdvPolygon(Shape));

  else
    begin
      result := 0;
      raise Exception.Create('unknown bounding type in function TZoneBounds2.Distance(Bounds: TZoneBounds2): single');
    end;
  end;
end;

function TAdvShape.Distance(const p: Tpoint): single;
begin
  result := DistanceToPoint(p);
end;

function TAdvShape.Distance(Bounds: TAdvShape): single;
begin
  result := DistanceToShape(Bounds);
end;

function TAdvShape.GetCenter: TD3DXVector2;
begin
  result.x := fMinMax.left + fMinMax.Width / 2;
  result.y := fMinMax.Top + fMinMax.height / 2;
end;

function TAdvShape.GetCenter2d: Tpoint;
begin
  with Center do
  begin
    result.x := round(x);
    result.y := round(y);
  end;
end;

function TAdvShape.GetControlPoint(cp: TControlPoint): Tpoint;
var
  r: Trect;
begin
  r := self.Rect;
  case cp of
    cpTop:
      result := Point(r.left + r.Width div 2, r.Top);
    cpLeft:
      result := Point(r.left, r.Top + r.height div 2);
    cpBottom:
      result := Point(r.left + r.Width div 2, r.Bottom);
    cpRight:
      result := Point(r.Right, r.Top + r.height div 2);
    cpTopLeft:
      result := r.TopLeft;
    cpTopRight:
      result := Point(r.Right, r.Top);
    cpBottomleft:
      result := Point(r.left, r.Bottom);
    cpBottomRight:
      result := r.BottomRight;
  end;
end;

function TAdvShape.GetNearestPoint(const p: Tpoint): Tpoint;
var
  v: TD3DXVector2;
begin
  v := GetNearestPoint2d(p);
  result.x := round(v.x);
  result.y := round(v.y);
end;

function TAdvShape.GetNearestPoint2d(const p: Tpoint): TD3DXVector2;
var
  Poly: TAdvPolygon;
begin
  Poly := TAdvPolygon.Create;
  Poly.Rect := self.Rect;
  result := Poly.GetNearestPoint2d(p);
  Poly.Free;
end;

class function TAdvShape.CreateFromXMLNode(node: TXMLNode): TAdvShape;
var
  c, x: integer;
  t: TShapeKind;
begin
  result := nil;

  t := TShapeKind(node.getNode('TYP', nhCreateNode).Value.AsInteger);
  case t of
    skRect:
      result := TAdvShape.Create;
    skEllipse:
      result := TEllipse.Create;
    skPolygon:
      result := TAdvPolygon.Create;
  end;
  result.LoadFromNode(node)
end;

function TAdvShape.PointInside(const p: Tpoint): Boolean;
begin
  result := PointInRect(p, self.Rect);
end;

procedure TAdvShape.LoadFromNode(node: TXMLNode);
var
  rec: Trect;
begin
  r(node, 'P0X', rec.left);
  r(node, 'P0Y', rec.Top);
  r(node, 'P1X', rec.Right);
  r(node, 'P1Y', rec.Bottom);
  Rect := rec;
end;

function TAdvShape.OverlapWithRect(const r: Trect): Boolean;
begin
  result :=
    CalcDistanceToRect(r) = 0;
end;

procedure TAdvShape.SaveToXMLNode(node: TXMLNode);

begin
  w(node, 'TYP', integer(fTyp));

  w(node, 'POINT-COUNT', 2);
  w(node, 'P0X', fMinMax.left);
  w(node, 'P0Y', fMinMax.Top);
  w(node, 'P1X', fMinMax.Right);
  w(node, 'P1Y', fMinMax.Bottom);
end;

procedure TAdvShape.Setname(const Value: string);
begin
  Fname := Value;
end;

procedure TAdvShape.SetOwnsUserObject(const Value: Boolean);
begin
  FOwnsUserObject := Value;
end;

procedure TAdvShape.SetRect(const Value: Trect);
begin
  fMinMax := Value;
  fMinMax.NormalizeRect;
end;

procedure TAdvShape.SetTag(const Value: integer);
begin
  FTag := Value;
end;

procedure TAdvShape.SetTyp(const Value: TShapeKind);
begin
  fTyp := Value;
end;

procedure TAdvShape.SetUserInterface(const Value: iInterface);
begin
  FUserInterface := Value;
end;

procedure TAdvShape.SetUserObject(const Value: TObject);
begin
  FUserObject := Value;
end;

{ TadvPolygon }

procedure TAdvPolygon.AfterChanged;
var
  I: integer;
  r: Trect;
begin
  if Length(fPoints) = 0 then
  begin
    r.TopLeft := Point(0, 0);
    r.BottomRight := Point(0, 0);
    fMinMax := r;
    exit;
  end;

  r.TopLeft := fPoints[0];
  r.BottomRight := fPoints[0];

  for I := 1 to Length(fPoints) - 1 do
  begin
    with r.TopLeft do
    begin
      x := Min(x, fPoints[I].x);
      y := Min(y, fPoints[I].y);
    end;

    with r.BottomRight do
    begin
      x := Max(x, fPoints[I].x);
      y := Max(y, fPoints[I].y);
    end;

  end;
  fMinMax := r;
end;

function TAdvPolygon.CalcDistanceToEllipse(const aEllipse: TEllipse): single;
var
  v2: TD3DXVector2;
  dist: single;
  x: integer;
  s1, s2: Tpoint;
begin
  result := 0;

  // if any of the points are inside the Ellipse, we have an intersection and the distance is 0
  for x := 0 to Length(fPoints) - 1 do
    if aEllipse.PointInside(fPoints[x]) then
      exit;

  // if the center of the Ellipse is inside the polygon, we have an intersection, and the distance is 0
  if Closed then
    if PointInside(aEllipse.Center2d) then
      exit;

  // if we are here, then there is most likely no  intersection
  if aEllipse.IsUnitCircle then
  begin
    result := DistanceToPoint(aEllipse.Center2d);
    result := result - aEllipse.Radius;
    if result < 0 then
      result := 0;
  end else begin
    result := CalcDistanceToPolygon(aEllipse.AsPolygon);
  end;
end;

function TAdvPolygon.CalcDistanceToPolygon(Poly: TAdvPolygon): single;
var
  x: integer;
begin
  result := 0;

  // if at least one point of poly1 is inside poly2 we have an intersection
  if Poly.Closed then
    for x := 0 to Length(fPoints) - 1 do
      if Poly.PointInside(fPoints[x]) then
        exit;

  if Closed then
    for x := 0 to Length(Poly.fPoints) - 1 do
      if self.PointInside(Poly.fPoints[x]) then
        exit;

  result := DistanceSegmentsToSegments(fPoints, Poly.fPoints, Closed, Poly.Closed);
end;

function TAdvPolygon.CalcDistanceToRect(const r: Trect): single;
var
  Points1: TPointArray;
  x: integer;
begin
  result := 0;
  // if at least 1 point of the poly is inside the rect, then we have an intersection.
  for x := 0 to Length(fPoints) - 1 do
    if PointInRect(fPoints[x], r) then
      exit(0);

  RectToPoints(r, Points1);

  // if at least one point of the rect is inside the poly, we have an intersection
  if Closed then
  begin
    for x := 0 to Length(Points1) - 1 do
      if self.PointInside(Points1[x]) then
      begin
        Points1 := NIL;
        exit;
      end;
  end;

  result := DistanceSegmentsToSegments(Points1, fPoints, true, Closed);
  Points1 := NIL;
end;

constructor TAdvPolygon.Create;
begin
  inherited;
  Closed := true;
  fTyp := skPolygon;
  fPoints := NIL;
end;

procedure TAdvPolygon.DeletePoint(index: integer);
var
  x: integer;
begin
  if (index < 0) or (index >= Length(fPoints)) or (Length(fPoints) <= 2) then
    exit;
  for x := index + 1 to Length(fPoints) - 1 do
    fPoints[x - 1] := fPoints[x];
  setlength(fPoints, Length(fPoints) - 1);
  AfterChanged;

end;

destructor TAdvPolygon.Destroy;
begin
  fPoints := NIL;
  inherited;
end;

function TAdvPolygon.DistanceToPoint(const p: Tpoint): single;
var
  dist: single;
  x: integer;
  pv, v1, v2: TD3DXVector2;
begin
  result := 0;
  if PointInside(p) then
    exit(0);

  pv := Vector2(p);

  for x := 0 to Length(fPoints) - 1 do
  begin
    if x + 1 < Length(fPoints) then
      v1 := Vector2(fPoints[x + 1])
    else
    begin
      if not Closed then
        break
      else
        v1 := Vector2(fPoints[0]);
    end;
    v2 := Vector2(fPoints[x]);

    dist := DistanceToSegment(pv, v1, v2);
    if (x = 0) or (dist < result) then
      result := dist;
  end;

end;

function TAdvPolygon.GetClosed: Boolean;
begin
  result := fClosed;
  if fClosed and (Length(fPoints) < 3) then
    result := False;
end;

function TAdvPolygon.GetNearestPoint2d(const p: Tpoint): TD3DXVector2;
var
  vp, v, s1, s2: TD3DXVector2;
  dist1, dist2: single;
  x: integer;
begin
  vp := Vector2(p);

  dist1 := -1;
  for x := 0 to Length(fPoints) - 1 do
  begin
    s1 := Vector2(fPoints[x]);
    if x + 1 < Length(fPoints) then
      s2 := Vector2(fPoints[x + 1])
    else
    begin
      if not Closed then
        break
      else
        s2 := Vector2(fPoints[0]);
    end;

    v := ClosestPointOnLine(s1, s2, vp);
    dist2 := VectorDistanceSQRT(v, vp);
    if (dist1 = -1) or (dist1 > dist2) then
    begin
      dist1 := dist2;
      result := v;
    end;
  end;
end;

procedure TAdvPolygon.InsertPoint(index: integer; const p: Tpoint);
var
  count: integer;
  x: integer;
begin
  count := Length(fPoints);
  setlength(fPoints, count + 1);
  if index >= count then
    fPoints[count] := p
  else
  begin
    for x := count downto index + 1 do
      fPoints[x] := fPoints[x - 1];

    fPoints[index] := p;
  end;
  AfterChanged;
end;

procedure TAdvPolygon.LoadFromNode(node: TXMLNode);
var
  x, c: integer;
begin
  inherited;
  r(node, 'IsClosed', fClosed);
  r(node, 'POINT-COUNT', c);
  setlength(fPoints, c);
  for x := 0 to c - 1 do
  begin
    w(node, 'P' + IntToStr(x) + 'X', fPoints[x].x);
    w(node, 'P' + IntToStr(x) + 'y', fPoints[x].x);
  end;
  AfterChanged;
end;

function TAdvPolygon.PointInside(const p: Tpoint): Boolean;
begin
  if not Closed then
    exit(False);

  result := inherited PointInside(p);
  if result then
    result := max3DCollision.PointinNConvexPoly(p, fPoints, @fMinMax);

end;

procedure TAdvPolygon.SaveToXMLNode(node: TXMLNode);
var
  x: integer;
begin
  inherited;
  w(node, 'IsClosed', fClosed);
  w(node, 'POINT-COUNT', Length(Points));
  for x := 0 to Length(fPoints) - 1 do
  begin
    w(node, 'P' + IntToStr(x) + 'X', fPoints[x].x);
    w(node, 'P' + IntToStr(x) + 'y', fPoints[x].x);
  end;

end;

procedure TAdvPolygon.SetClosed(const Value: Boolean);
begin
  fClosed := Value;
end;

procedure TAdvPolygon.SetRect(const Value: Trect);
begin
  inherited;
  setlength(fPoints, 4);
  fPoints[0] := fMinMax.TopLeft;
  fPoints[1] := Point(fMinMax.Right, fMinMax.Top);
  fPoints[2] := fMinMax.BottomRight;
  fPoints[3] := Point(fMinMax.left, fMinMax.Bottom);
  Closed := true;
end;

{ TBitMatrix }

procedure TBitMatrix.BuildFromBitMap;
var
  qp: TQuickPixel;
  y: integer;
  x: integer;
  c: tcolor;
begin
  Clear(bmp.Width, bmp.height);

  qp := TQuickPixel.Create(bmp);
  for y := 0 to bmp.height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      // c := bmp.Canvas.Pixels[x,y];
      c := qp.Pixel[x, y];
      Value[x, y] := c = TruColor;
    end;

  qp.Free;
end;

procedure TBitMatrix.Clear(NewWidth, NewHeight: integer);
var
  ReqSize: integer;
begin
  fWidth := NewWidth;
  fHeight := NewHeight;
  ReqSize := RoundUp((fWidth * fHeight) / 8);
  if ReqSize <> fSize then
  begin
    fSize := ReqSize;
    ReAllocMem(fBuffer, fSize);
  end;
  ZeroMemory(fBuffer, fSize);
end;

constructor TBitMatrix.Create;
begin
  inherited;

end;

destructor TBitMatrix.Destroy;
begin
  if fSize > 0 then
    Freemem(fBuffer, fSize);
  inherited;
end;

function TBitMatrix.GetByte(x, y: integer; out pb: pByte; out iBit: byte): Boolean;
var
  I, iByte: integer;
begin
  if (x >= 0) and (y >= 0) and (x < fWidth) and (y < fHeight) then
  begin
    I := fWidth * y + x;
    iByte := I shr 3;
    iBit := I - (iByte shl 3);
    pb := fBuffer;
    inc(pb, iByte);
    result := true;
  end
  else
    result := False;

end;

function TBitMatrix.GetValue(x, y: integer): Boolean;
var
  pb: pByte;
  mask, b: byte;
  iBit: byte;
begin
  if not GetByte(x, y, pb, iBit) then
    raise Exception.Create('TBitMatrix - out of bounds: x=' + IntToStr(x) + '; y=' + IntToStr(y) + '; width=' + IntToStr(fWidth) + '; height=' + IntToStr(fHeight))
  else
  begin
    mask := (1 shl (7 - iBit));
    result := (pb^ and mask) = mask;
  end;
end;

function TBitMatrix.RoundUp(const Value: double): integer;
begin
  result := Trunc(Value);
  if frac(Value) > 0 then
    inc(result);
end;

procedure TBitMatrix.SetValue(x, y: integer; const Value: Boolean);
var
  pb: pByte;
  mask, b: byte;
  iBit: byte;
  CurValue: Boolean;
begin
  if not GetByte(x, y, pb, iBit) then
    raise Exception.Create('TBitMatrix - out of bounds: x=' + IntToStr(x) + '; y=' + IntToStr(y) + '; width=' + IntToStr(fWidth) + '; height=' + IntToStr(fHeight))
  else
  begin
    mask := (1 shl (7 - iBit));
    CurValue := (pb^ and mask) = mask;
    if Value = CurValue then
      exit;

    if not Value then
    begin
      // set all bits to 1 except of the significant bit
      mask := not mask;
      pb^ := pb^ and mask;
    end else begin
      // all bits in the mask are 0 except for the significant bit. when doing a or operation, the significant bit will be set to one, all others will noe change at all.
      pb^ := pb^ or mask;
    end;

  end;
end;

function TAdvShape.Clone: TAdvShape;
begin
  case Typ of
    skRect:
      result := CreateRectShape(self.fMinMax);
    skEllipse:
      result := CreateEllipseShape(fMinMax);
    skPolygon:
      result := CreatePolygonShape(TAdvPolygon(self).fPoints, TAdvPolygon(self).Closed);
  else
    begin
      result := nil;
      raise Exception.Create('unsupported typeunk');
    end;
  end;

  result.FTag := FTag;
  result.Fname := Fname;
  result.FUserInterface := FUserInterface;

  if not FOwnsUserObject then
    result.FUserObject := FUserObject;
end;

end.
