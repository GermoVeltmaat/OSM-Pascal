unit U_OSM_Coordinaat;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Math;

Type
  TTileCorner = (tcTopLeft,tcBottomRight);

  { TOsmCoordinate }
  TOsmCoordinate = Class
  Private
    MyLongitude : Double;
    MyLatitude : Double;
  Public
    Constructor Create;
// Create a new TOsmCoordinate with the given Longitude and Latitude values
    Constructor Create(NewLon,NewLat : Double);
// Create a new TOsmCoordinate for the corner of Given Tile
    Constructor Create(x,y,zoom : Integer; WhichCorner : TTileCorner = tcTopLeft);
// Create a new TOsmCoordinate by copying the values from another TOsmCoordinate
    Constructor Create(Other : TOsmCoordinate);
// Create a new TOsmCoordinate as midpoint of the given coords
    Constructor CreateFrom(oc1,oc2 : TOsmCoordinate);

// Copy values from another TOsmCoordinate
    Procedure CopyFrom(Other : TOsmCoordinate);
// X index for Tile containing Coordinate with given zoomlevel
    Function TileXForZoom(Zoom : Integer) : Integer;
// Y index for Tile containing Coordinate with given zoomlevel
    Function TileYForZoom(Zoom : Integer) : Integer;
// Property for filling with another TOsmCoordinate
    Property Coordinate : TOsmCoordinate Write CopyFrom;
    Property Longitude : Double Read MyLongitude;
    Property Latitude  : Double Read MyLatitude;
// Human readable tekst for value
    Function Display : String;
    end;


  Function SlippyToLongitude(x,z : Integer) : Double;
  Function SlippyToLatitude(y,z : Integer) : Double;

implementation

Function SlippyToLongitude(x, z : Integer) : Double;
begin
  Result := ((x / Power(2,z)) * 360.0) - 180.0;
  End;

Function SlippyToLatitude(y, z : Integer) : Double;
begin
  Result := RadtoDeg(Arctan(Sinh(Pi * (1 - 2 * y / Power(2,z)))));
  End;


{ TOsmCoordinate }
Function TOsmCoordinate.Display : String;
begin
  Result := Format('Lon:%1.5f,Lat:%1.5f',[MyLongitude,MyLatitude]);
  end;

Constructor TOsmCoordinate.Create;
begin
  Inherited Create;
  end;

Constructor TOsmCoordinate.Create(NewLon, NewLat : Double);
begin
  Inherited Create;
  MyLongitude := NewLon;
  MyLatitude := NewLat;
  end;

Constructor TOsmCoordinate.Create(x, y, zoom : Integer;
                                  WhichCorner : TTileCorner);
begin
  Inherited Create;

  If WhichCorner = tcTopLeft Then Begin
    MyLongitude := SlippyToLongitude(X,Zoom);
    MyLatitude := SlippyToLatitude(Y+1,Zoom);
    End
  Else Begin
    MyLongitude := SlippyToLongitude(X+1,Zoom);
    MyLatitude := SlippyToLatitude(Y,Zoom);
    End;
  end;

Constructor TOsmCoordinate.Create(Other : TOsmCoordinate);
begin
  Inherited Create;
  CopyFrom(Other);
  end;

Constructor TOsmCoordinate.CreateFrom(oc1, oc2 : TOsmCoordinate);
begin
  Inherited Create;
  MyLongitude := (oc1.Longitude+oc2.Longitude) / 2;
  MyLatitude := (oc1.Latitude+oc2.Latitude) / 2;
  end;



Procedure TOsmCoordinate.CopyFrom(Other : TOsmCoordinate);
begin
  MyLatitude := Other.MyLatitude;
  MyLongitude := Other.MyLongitude;
  end;

Function TOsmCoordinate.TileXForZoom(Zoom : Integer) : Integer;
Var
  n : Double;
begin
  n := Power(2, zoom);
  Result := Trunc(((MyLongitude + 180) / 360) * n);
  End;

Function TOsmCoordinate.TileYForZoom(Zoom : Integer) : Integer;
Var
  Lat_Rad,n : Double;
begin
  lat_rad := DegToRad(MyLatitude);
  n := Power(2, zoom);
  Result := Trunc((1 - (ln(Tan(lat_rad) + (1 /Cos(lat_rad))) / Pi)) / 2 * n);
  End;


end.

