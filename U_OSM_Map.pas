unit U_OSM_Map;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types,
  Graphics, FPCanvas, ExtCtrls,
  U_OSM_Coordinaat, U_OSM_Rect, U_OSM_Tile,
  U_OSM_MapTileImage, U_OSM_MapTileImageList;

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

    MapWidthDegrees,
    MapHeightDegrees : Double;

    MyTilesList : TMapTileImageList;

    Constructor Create(Soort : String);
    Destructor Destroy; OVERRIDE;

    Function MapForArea(Area : TOsmRect;
                        InSize : TSize) : Boolean;
    Function MapForPoint(MidPoint : TOsmCoordinate;
                         ZoomLevel : Integer;
                         InSize : TSize) : Boolean;
    Function LoadTiles : Boolean;

    Function MTIFor(BaseName : String; x,y,z : Integer) : TOsmMapTileImage;
    end;

Var
  MapPanel : TComponent;
implementation

{ TOsmMap }
Constructor TOsmMap.Create(Soort : String);
begin
  Inherited Create;
  BaseNaam := Soort;
  MyTilesList := TMapTileImageList.Create;
  end;

Destructor TOsmMap.Destroy;
begin
  While MyTilesList.Count > 0 Do MyTilesList.RemoveTile(0);
  FreeAndNil(MyTilesList);
  Inherited Destroy;
  end;


Function TOsmMap.LoadTiles : Boolean;
Var
  Minx,Maxx,MinY,MaxY : Integer;
  Tx,Ty,Idx : Integer;
  FirstX,FirstY,Ox,Oy : Integer;
  MTI : TOsmMapTileImage;

begin
// Alle tiles onzichtbaar maken
  For Idx := 1 to MyTilesList.Count Do
    TOsmMapTileImage(MyTilesList.Objects[Idx-1]).Visible := False;
// check welke tiles we nodig hebben
  MinX := MapRect.BottomLeft.TileXForZoom(MapZoom);
  MaxX := MapRect.TopRight.TileXForZoom(MapZoom);//
  MinY := MapRect.TopRight.TileYForZoom(MapZoom); // Y is omgekeerd !!
  MaxY := MapRect.BottomLeft.TileYForZoom(MapZoom);//
//Writeln(Format('%d..%d %d..%d',[MinX,MaxX,MinY,MaxY]));
// Pak eerste tile om ofset mee te bepalen
  MTI := MTIFor(BaseNaam,MinX,MinY,MapZoom);
// Left
  FirstX := Trunc((MTI.Tile.Area.Left - MapRect.Left) / MTI.Tile.LonPerPixel);
// Top (y werkt omgekeerd !)
  FirstY := Trunc((MapRect.Top - MTI.Tile.Area.Top) / MTI.Tile.LatPerPixel);

// Reset positie eerste x tile
  Ox := FirstX;
  For Tx := MinX To MaxX Do Begin
// reset positie eerste y tile
    Oy := FirstY;
    For Ty := MinY To MaxY Do Begin
      // pak (of maak) map-tile-image
      MTI := MTIFor(BaseNaam,tx,ty,MapZoom);

      Mti.Left := Ox;
      Mti.Top := Oy;
      Mti.Visible := True;
// TODO: tilegroote gebruiken
      Inc(Oy,256);
      End;
    Inc(Ox,256);
    End;
  Result := True;
  End;


Function TOsmMap.MapForArea(Area : TOsmRect; InSize : TSize) : Boolean;
Var
  MidPoint : TOsmCoordinate;
  Start : TOsmTile;
begin
// Bereken middelpunt
  MidPoint := Area.MidPoint;
// uitzoeken bij welke zoom het hele gebied erin past
  MapZoom := MaxZoom;
  Repeat
    Start := TOsmTile.Create('',MidPoint,MapZoom);
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
  MapZoom := ZoomLevel;
// Maak proeftile voor factoren (Vooral voor Y !)
  Start := TOsmTile.Create('',MidPoint,ZoomLevel);
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

// Geeft MapTileImage voor opgegeven waarden
{  Zoekt of er al een MapTileImage is? Zo ja => geeft die
                                       Zo nee => maakt nieuwe en geeft die

  }
Function TOsmMap.MTIFor( BaseName : String;
                         x, y, z : Integer) : TOsmMapTileImage;
Var
  Naam : String;
  MTI : TOsmMapTileImage;
  NewTile : TOsmTile;
begin
  Naam := MakeUniqueTileName(BaseName,x,y,z);
  MTI := MyTilesList.ZoekTile(Naam);
  If MTI = nil Then Begin
// New tile maken
    NewTile := TOsmTile.Create(BaseName,x,y,z);
// TODO: nu laden, later eigen thread e.d.
    //NewTile.TryToLoad();

// maak image met tile
    MTI := TOsmMapTileImage.CreateForTile(MapPanel,NewTile);
// Vertel Tile waar het z'n plaatje mag laten verwerken
    NewTile.TileLoaded := @MTI.UpdatePicture;
// Start laden tile
    NewTile.LoadASynced;
// Zet in lijst
    MyTilesList.AddTile(MTI);
    End;
  Result := MTI;
  end;

end.

