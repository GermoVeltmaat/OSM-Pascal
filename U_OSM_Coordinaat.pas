unit U_OSM_Coordinaat;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, U_Osm_Base;

Type
  { TOsmCoordinate }
  TOsmCoordinate = Class
  Private
    MyLongitude : Double;
    MyLatitude  : Double;
  Public
    Constructor Create;
    // Human readable tekst for debug
    Function Display : String;

// Create a new TOsmCoordinate with the given Longitude and Latitude values
    Constructor CreateByValues(Longitude,Latitude : Double);

// Create a new TOsmCoordinate by copying the values from another TOsmCoordinate
    Constructor CreateFromOther(Other : TOsmCoordinate);

// Create a new TOsmCoordinate as midpoint of the given coords
    Constructor CreateMidPoint(oc1,oc2 : TOsmCoordinate);

// Helper functions for Slippy Tiles
// X index for Tile containing Coordinate with given zoomlevel
    Function TileXForZoom(Zoom : Integer) : Integer;
// Y index for Tile containing Coordinate with given zoomlevel
    Function TileYForZoom(Zoom : Integer) : Integer;

// Property for read-acces
    Property Longitude : Double Read MyLongitude;
    Property Latitude  : Double Read MyLatitude;
    end;

implementation

{ TOsmCoordinate }
Function TOsmCoordinate.Display : String;
begin
  Result := Format('Lon:%1.5f,Lat:%1.5f',[MyLongitude,MyLatitude]);
  end;

Constructor TOsmCoordinate.Create;
begin
  Inherited Create;
  end;

Constructor TOsmCoordinate.CreateByValues(Longitude, Latitude : Double);
begin
  Inherited Create;
  MyLongitude := Longitude;
  MyLatitude  := Latitude;
  end;

Constructor TOsmCoordinate.CreateFromOther(Other : TOsmCoordinate);
begin
  Inherited Create;
  MyLatitude := Other.MyLatitude;
  MyLongitude := Other.MyLongitude;
  end;

Constructor TOsmCoordinate.CreateMidPoint(oc1, oc2 : TOsmCoordinate);
begin
  Inherited Create;
  MyLongitude := (oc1.Longitude+oc2.Longitude) / 2;
  MyLatitude := (oc1.Latitude+oc2.Latitude) / 2;
  end;


// Slippy functions
Function TOsmCoordinate.TileXForZoom(Zoom : Integer) : Integer;
Begin
  Result := LongitudeToSlippy(MyLongitude,Zoom);
  end;

Function TOsmCoordinate.TileYForZoom(Zoom : Integer) : Integer;
Begin
  Result := LatitudeToSlippy(MyLatitude,Zoom);
  end;

end.

