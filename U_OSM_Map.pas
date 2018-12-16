unit U_OSM_Map;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types,
  Graphics, FPCanvas,
  U_OSM_Coordinaat, U_OSM_Rect, U_OSM_Tile, U_OSM_TileList;

Type

  { TOsmMap }

  TOsmMap = Class
  // Vaste gegevens van tile provider
    BaseNaam : String;
  const
    MinZoom : Integer = 0;
    MaxZoom : Integer = 18;
    TileSizeWidth : Integer = 256;
    TileSizeHeight : Integer = 256;
  Var
    MapZoom    : Integer;
    MapRect    : TOsmRect;
    MapFacLon,
    MapFacLat  : Double;

    MapPicture : TPicture;
    MapWidthDegrees,
    MapHeightDegrees : Double;

    MyTilesList : TTilesList;

    Constructor Create(Soort : String);
    Destructor Destroy; OVERRIDE;

    Function MapForArea(Area : TOsmRect;
                        InSize : TSize) : Boolean;
    Function MapForPoint(MidPoint : TOsmCoordinate;
                         ZoomLevel : Integer;
                         InSize : TSize) : Boolean;
    Function LoadTiles : Boolean;
    end;

implementation

{ TOsmMap }
Constructor TOsmMap.Create(Soort : String);
begin
  Inherited Create;
  BaseNaam := Soort;
  MyTilesList := TTilesList.Create;
  MapPicture := TPicture.Create;
  end;

Destructor TOsmMap.Destroy;
begin
  FreeAndNil(MapPicture);
  While MyTilesList.Count > 0 Do MyTilesList.RemoveTile(0);
  FreeAndNil(MyTilesList);
  Inherited Destroy;
  end;


Function TOsmMap.LoadTiles : Boolean;
// hier gaan we een tile geheel of gedeeltelijk in mappicture plakken
Procedure PlakTileInMapAt(Tile : TOsmTile; X,Y : Integer);
Var
  //FacLon,FacLat : Double;
  MinLon,MaxLon,MinLat,MaxLat : Double; // posities tile in wcs
  DCoor : Double; DPix : Integer;
  SourceRect,DestRect : TRect;
Begin
  SourceRect := Rect(0,0,Tile.Picture.Width,Tile.Picture.Height);
  DestRect := Rect(0,0,Tile.Picture.Width,Tile.Picture.Height);
// Bewaar randen van tile (wcs)
  MinLon := Tile.Area.Left;
  MaxLon := Tile.Area.Right;
  MinLat := Tile.Area.Bottom;
  MaxLat := Tile.Area.Top;
// Kijk of er grenzen zijn en bereken positie in mappic
// Left
  DCoor := MinLon - MapRect.Left;
  DPix := Trunc(Abs(DCoor) / MapFacLon);
  If DCoor < 0 Then Begin
    SourceRect.Left := DPix;
    DestRect.Width := TileSizeWidth - DPix;
    End
  Else Begin // Schuif naar rechts op goede positie
    DestRect.Left := DPix;
    DestRect.Width := TileSizeWidth;
    End;
// Right
  DCoor := MapRect.Right - MaxLon;
  DPix := Trunc(Abs(DCoor) / MapFacLon);
  If DCoor < 0 Then Begin
    SourceRect.Width := TileSizeWidth - DPix;
    DestRect.Width := TileSizeWidth - DPix;
    End;
// Top
  DCoor := MapRect.Top - MaxLat;
  DPix := Trunc(Abs(DCoor) / MapFacLat);
  If DCoor < 0 Then Begin
    SourceRect.Top := DPix;
    DestRect.Height := TileSizeHeight - DPix;
    End
  Else Begin // schuif naar beneden op goede positie
    DestRect.Top := DPix;
    DestRect.Height := TileSizeHeight;
    End;
// Bottom
  DCoor := MinLat - MapRect.Bottom;
  DPix := Trunc(Abs(DCoor) / MapFacLat);
  If DCoor < 0 Then Begin
    SourceRect.Height := TileSizeHeight - DPix;
    DestRect.Height := TileSizeHeight - DPix;
    End;

//Writeln(Format(
//  '%d,%d:%d,%d => %d,%d:%d,%d',[
//    SourceRect.Left,SourceRect.Top,SourceRect.Right,SourceRect.Bottom,
//    DestRect.Left,DestRect.Top,DestRect.Right,DestRect.Bottom
//    ]));

// Plak tile in pic
  MapPicture.Bitmap.Canvas.CopyRect(DestRect,Tile.Picture.Bitmap.Canvas,SourceRect);
  end;

Var
  Minx,Maxx,MinY,MaxY : Integer;
  Tx,Ty : Integer;
  Tile : TOsmTile;
begin
  MinX := MapRect.BottomLeft.TileXForZoom(MapZoom);
  MaxX := MapRect.TopRight.TileXForZoom(MapZoom);//
  MinY := MapRect.TopRight.TileYForZoom(MapZoom); // Y is omgekeerd !!
  MaxY := MapRect.BottomLeft.TileYForZoom(MapZoom);//
//Writeln(Format('%d..%d %d..%d',[MinX,MaxX,MinY,MaxY]));
  For Tx := MinX To MaxX Do Begin
    For Ty := MinY To MaxY Do Begin
      Tile := TOsmTile.Create(Tx,Ty,MapZoom);
      Tile.TryToLoad(BaseNaam);
      If Tile.Loaded in [lsDisk,lsWeb] Then Begin
        PlakTileInMapAt(Tile,Tx,Ty);
        FreeAndNil(Tile);
        End
      Else Begin
        FreeAndNil(Tile);
        Result := False;
        Exit;
        End;
      End;
    End;
  Result := True;
  End;


Function TOsmMap.MapForArea(Area : TOsmRect; InSize : TSize) : Boolean;
Var
  MidPoint : TOsmCoordinate;
  Start : TOsmTile;
begin
  MapPicture.Bitmap.SetSize(InSize.Width,InSize.Height);
// Bereken middelpunt
  MidPoint := Area.MidPoint;
// uitzoeken bij welke zoom het hele gebied erin past
  MapZoom := MaxZoom;
  Repeat
    Start := TOsmTile.Create(MidPoint.Longitude,MidPoint.Latitude,MapZoom);
    MapFacLon := Start.LonPerPixel;
    MapFacLat := Start.LatPerPixel;
    FreeAndNil(Start);
// grootte van gebied berekenen
    MapWidthDegrees := MapFacLon * InSize.Width;
    MapHeightDegrees := MapFacLat * InSize.Height;
//Coordinaat grenzen berekenen
    MapRect := TOsmRect.CreateAroundMidPoint(MidPoint,MapWidthDegrees,MapHeightDegrees);
    If MapRect.Contains(Area) Then Begin
      // Nu de Tiles laden
      LoadTiles;
      Result := True;
      FreeAndNil(MidPoint);
      Exit;
      End;
    Dec(MapZoom);
    Until MapZoom < MinZoom;
  FreeAndNil(MidPoint);
  end;

Function TOsmMap.MapForPoint( MidPoint : TOsmCoordinate;
                              ZoomLevel : Integer;
                              InSize : TSize) : Boolean;
Var
  Start : TOsmTile;
begin
  MapPicture.Bitmap.SetSize(InSize.Width,InSize.Height);
  MapZoom := ZoomLevel;
// Maak proeftile voor factoren (Vooral voor Y !)
  Start := TOsmTile.Create(MidPoint.Longitude,MidPoint.Latitude,ZoomLevel);
  MapFacLon := Start.LonPerPixel;
  MapFacLat := Start.LatPerPixel;
  FreeAndNil(Start);
// grootte van gebied berekenen
  MapWidthDegrees := MapFacLon * InSize.Width;
  MapHeightDegrees := MapFacLat * InSize.Height;
//Coordinaat grenzen berekenen
  MapRect := TOsmRect.CreateAroundMidPoint(MidPoint,MapWidthDegrees,MapHeightDegrees);
// nu de tiles laden
  LoadTiles;
  Result := True;
  end;

end.

