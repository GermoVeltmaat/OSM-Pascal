unit U_OSM_Rect;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, U_OSM_Coordinaat, U_OSM_Base;

Type
  { TOsmRect }
  TOsmRect = Class
  Private
    MyLeft,
    MyRight,
    MyBottom,
    MyTop : Double;
    Procedure Check;
    Function  GetBottomLeft : TOsmCoordinate;
    Function  GetTopRight   : TOsmCoordinate;
  Public
    Constructor Create;
    Function Display : String;

// Create TOsmRect with given boundaries
    Constructor CreateWithBounds( LeftLongitude,
                                  TopLatitude,
                                  RightLongitude,
                                  BottomLatitude : Double);

// Create TOsmRect with given TOsmCoords as corner points
    Constructor CreateWithCorners( BottomLeft,
                                   TopRight : TOsmCoordinate);

// Create TOsmRect With size of SpanLongitude and SpanLatitude
//                 With MidPoint in the Center
    Constructor CreateAroundMidPoint( MidPoint : TOsmCoordinate;
                                      SpanLongitude,
                                      SpanLatitude : Double);

// Create TOsmRect for Area of Tile
    Constructor CreateForTile(TileX, TileY, TileZoom : Integer);

// Functions for checking
// Check if TOsmCoordinate is within Area
    Function Contains(Test : TOsmCoordinate) : Boolean;
// Check if TOsmRect is within Area
    Function ConTains(Test : TOsmRect) : Boolean;
// Calculate Midpoint for Area
    Function MidPoint : TOsmCoordinate;

// Properties for read-access
// TOsmCoordinates
    Property BottomLeft : TOsmCoordinate Read GetBottomLeft;
    Property TopRight   : TOsmCoordinate Read GetTopRight;
// Values
    Property Left   : Double Read MyLeft;
    Property Top    : Double Read MyTop;
    Property Right  : Double Read MyRight;
    Property Bottom : Double Read MyBottom;
    End;

Implementation
{ TOsmRect }
// Helper function for swapping 2 values (left<>right, bottom<>top)
Procedure Exchange(Var D1,D2 : Double);
Var       Dt : Double;
Begin     Dt := D1; D1 := D2; D2 := Dt; End;

Function TOsmRect.Display : String;
begin
  Result := Format('%1.5f,%1.5f..%1.5f,%1.5f',[MyLeft,MyBottom,MyRight,MyTop]);
  end;

Constructor TOsmRect.Create;
begin
  Inherited Create;
  end;

// Function for checking if left<right and bottom<top
//          Exchange values if necessary
Procedure TOsmRect.Check;
begin
  If MyLeft > MyRight Then Exchange(MyLeft,MyRight);
  If MyBottom > MyTop Then Exchange(MyBottom,MyTop);
  end;

Constructor TOsmRect.CreateWithBounds( LeftLongitude,
                                       TopLatitude,
                                       RightLongitude,
                                       BottomLatitude : Double);
begin
  Inherited Create;
  MyLeft   := LeftLongitude;
  MyRight  := RightLongitude;
  MyBottom := BottomLatitude;
  MyTop    := TopLatitude;
  Check;
  end;

Constructor TOsmRect.CreateWithCorners( BottomLeft,
                                        TopRight : TOsmCoordinate);
begin
  Inherited Create;
  MyLeft   := BottomLeft.Longitude;
  MyRight  := TopRight.Longitude;
  MyBottom := BottomLeft.Latitude;
  MyTop    := TopRight.Latitude;
  end;

Constructor TOsmRect.CreateAroundMidPoint( MidPoint : TOsmCoordinate;
                                           SpanLongitude,
                                           SpanLatitude : Double);
begin
  Inherited Create;
  MyLeft   := MidPoint.Longitude - (SpanLongitude/2);
  MyRight  := MidPoint.Longitude + (SpanLongitude/2);
  MyBottom := MidPoint.Latitude - (SpanLatitude/2);
  MyTop    := MidPoint.Latitude + (SpanLatitude/2);
  Check;
  end;

Constructor TOsmRect.CreateForTile( TileX,
                                    TileY,
                                    TileZoom : Integer);
begin
  Inherited Create;
  MyLeft   := SlippyToLongitude(TileX,TileZoom);
  MyRight  := SlippyToLongitude(TileX+1,TileZoom);
// Let OP! : Y telt van boven naar beneden => omdraaien
  MyBottom := SlippyToLatitude(TileY+1,TileZoom);
  MyTop    := SlippyToLatitude(TileY,TileZoom);
  Check;
  End;


// Test Functions
Function TOsmRect.Contains(Test : TOsmCoordinate) : Boolean;
begin
  Result := (Test.Longitude > MyLeft) And
            (Test.Longitude < MyRight) And
            (Test.Latitude  > MyBottom) And
            (Test.Latitude  < MyTop);
  end;

Function TOsmRect.ConTains(Test : TOsmRect) : Boolean;
begin
  Result := (Test.Left   > MyLeft) And
            (Test.Right  < MyRight) And
            (Test.Bottom > MyBottom) And
            (Test.Top    < MyTop);
  end;

// Helper Functions
Function TOsmRect.GetBottomLeft : TOsmCoordinate;
begin
  Result := TOsmCoordinate.CreateByValues(MyLeft,MyBottom);
  end;

Function TOsmRect.GetTopRight : TOsmCoordinate;
begin
  Result := TOsmCoordinate.CreateByValues(MyRight,MyTop);
  end;

Function TOsmRect.MidPoint : TOsmCoordinate;
begin
  Result := TOsmCoordinate.CreateByValues((MyLeft+MyRight)/2,(MyBottom+MyTop)/2);
  end;


end.

