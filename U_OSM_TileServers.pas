unit U_OSM_TileServers;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,
  Graphics,
  fphttpclient;

// Fetch 1 tile van server
Function FetchTileFromServer(TileName : String; TileX, TileY, TileZoom : Integer) : TPicture;

// Info over Tile Servers
Type
  TTileSource = Record
    TileName      : String;
    Description   : String;
    MinZoomLevel  : Integer;
    MaxZoomLevel  : Integer;
    TileWidth     : Integer;
    TileHeight    : Integer;
    CachePrefix   : String;
    TileServer    : String;
    FileExtension : String;
    end;

// TStringList met namen van servers
Function TileSourcesList : TStringList;
// inforecord over TileSource
Function TileSourceInfo(TileName : String) : TTileSource;

implementation

Const
  TileSources : Array[1..15] of TTileSource = (
    ( //
    TileName      : 'GrayScale';
    Description   : 'GrayScale OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'GrayScale';
    TileServer    : 'https://tiles.wmflabs.org/bw-mapnik/';
    FileExtension : '.png';
    ),( //
    TileName      : 'Standard';
    Description   : 'Standard OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'Standard';
    TileServer    : 'https://a.tile.openstreetmap.org/';
    FileExtension : '.png';
    ),( //
    TileName      : 'Wikimedia';
    Description   : 'Wikimedia OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'Wikimedia';
    TileServer    : 'https://maps.wikimedia.org/osm-intl/';
    FileExtension : '.png';
    ),( //
    TileName      : 'OpenCycleMap';
    Description   : 'OpenCycleMap OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'OpenCycleMap';
    TileServer    : 'http://tile.thunderforest.com/cycle/';
    FileExtension : '.png';
    ),( //
    TileName      : 'Humanitarian';
    Description   : 'Humanitarian OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'Humanitarian';
    TileServer    : 'http://a.tile.openstreetmap.fr/hot/';
    FileExtension : '.png';
    ),( //
    TileName      : 'France';
    Description   : 'France OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'France';
    TileServer    : 'http://a.tile.openstreetmap.fr/osmfr/';
    FileExtension : '.png';
    ),( //
    TileName      : 'WayMarkedTrailsHiking';
    Description   : 'WayMarkedTrailsHiking OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'WayMarkedTrailsHiking';
    TileServer    : 'https://tile.waymarkedtrails.org/hiking/';
    FileExtension : '.png';
    ),( //
    TileName      : 'WayMarkedTrailsCycling';
    Description   : 'WayMarkedTrailsCycling OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'WayMarkedTrailsCycling';
    TileServer    : 'https://tile.waymarkedtrails.org/cycling/';
    FileExtension : '.png';
    ),( // Black ??
    TileName      : 'HillShading';
    Description   : 'HillShading OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'HillShading';
    TileServer    : 'http://c.tiles.wmflabs.org/hillshading/';
    FileExtension : '.png';
    ),( //
    TileName      : 'OsmNoLabels';
    Description   : 'OsmNoLabels OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'OsmNoLabels';
    TileServer    : 'https://tiles.wmflabs.org/osm-no-labels/';
    FileExtension : '.png';
    ),( //
    TileName      : 'StamenToner';
    Description   : 'StamenToner OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'StamenToner';
    TileServer    : 'http://a.tile.stamen.com/toner/';
    FileExtension : '.png';
    ),( //
    TileName      : 'TransportMap';
    Description   : 'TransportMap OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'TransportMap';
    TileServer    : 'http://tile.thunderforest.com/transport/';
    FileExtension : '.png';
    ),( // Api key required
    TileName      : 'ThunderForestLandscape';
    Description   : 'ThunderForestLandscape OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'ThunderForestLandscape';
    TileServer    : 'http://tile.thunderforest.com/landscape/';
    FileExtension : '.png';
    ),( // Api key required
    TileName      : 'ThunderForestOutdoors';
    Description   : 'ThunderForestOutdoors OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'ThunderForestOutdoors';
    TileServer    : 'http://tile.thunderforest.com/outdoors/';
    FileExtension : '.png';
    ),( //
    TileName      : 'OpenPtMap';
    Description   : 'OpenPtMap OpenStreetMap Tiles';
    MinZoomLevel  : 0;
    MaxZoomLevel  : 18;
    TileWidth     : 256;
    TileHeight    : 256;
    CachePrefix   : 'OpenPtMap';
    TileServer    : 'http://www.openptmap.org/tiles/';
    FileExtension : '.png';
    )
    );

Function TileSourcesList : TStringList;
Var
  ts : TTileSource;
Begin
  Result := TStringList.Create;
  For ts in TileSources Do
    Result.Add(ts.TileName);
  end;

Function TileSourceInfo(TileName : String) : TTileSource;
Var
  ts : TTileSource;
Begin
  For ts in TileSources Do Begin
    If Ts.TileName = TileName Then Begin // This is the one we want
      Result := Ts;
      Exit;
      End;
    End;
  End;

// Maak Url voor ophalen tile
Function UrlFor(TileName : String; TileX, TileY, TileZoom : Integer) : String;
Var
  ts : TTileSource;
Begin
  For ts in TileSources Do Begin
    If Ts.TileName = TileName Then Begin // This is the one we want
      Result := Format('%s%d/%d/%d%s',      // 'htt.../z/x/y.png'
                       [ts.TileServer,TileZoom,TileX,TileY,ts.FileExtension]);
      Exit;
      End;
    End;
  Result := '';
  end;

// Helper function to load Tile from the internet
Function FetchTileFromServer( TileName : String;
                              TileX, TileY, TileZoom : Integer) : TPicture;
var
  Url : String;
  LMemoryStream: TMemoryStream;
begin
  Url := UrlFor(TileName,TileX,TileY,TileZoom);
  If Url = '' Then Exit;

  LMemoryStream := TMemoryStream.Create;
  try
    TFPHTTPClient.SimpleGet(Url, LMemoryStream);
    LMemoryStream.Position := 0;
    Result := TPicture.Create;
    Result.LoadFromStream(LMemoryStream);
  Except
    end;
  FreeAndNil(LMemoryStream);
  end;

end.

