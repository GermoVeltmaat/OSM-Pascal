unit U_OSM_Base;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math;

// Calculate Longitude and Latitide for Given Tile in Given ZoomLevel
Function SlippyToLongitude(TileX, TileZoom : Integer) : Double;
Function SlippyToLatitude(TileY, TileZoom : Integer) : Double;

// Calculate TileX and TileY for Given Coordinate in Given ZoomLevel
Function LongitudeToSlippy(Longitude : Double; TileZoom : Integer) : Integer;
Function LatitudeToSlippy(Latitude : Double; TileZoom : Integer) : Integer;

implementation

Function SlippyToLongitude(TileX, TileZoom : Integer) : Double;
begin
  Result := ((TileX / Power(2,TileZoom)) * 360.0) - 180.0;
  End;

Function SlippyToLatitude(TileY, TileZoom : Integer) : Double;
begin
  Result := RadtoDeg(Arctan(Sinh(Pi * (1 - 2 * TileY / Power(2,TileZoom)))));
  End;

Function LongitudeToSlippy(Longitude : Double; TileZoom : Integer) : Integer;
begin
  Result := Trunc(((Longitude + 180) / 360) * Power(2, TileZoom));
  End;

Function LatitudeToSlippy(Latitude : Double; TileZoom : Integer) : Integer;
begin
  Result := Trunc((1 - (ln(Tan(DegToRad(Latitude)) + (1 /Cos(DegToRad(Latitude)))) / Pi)) / 2 * Power(2,TileZoom));
  End;

end.

