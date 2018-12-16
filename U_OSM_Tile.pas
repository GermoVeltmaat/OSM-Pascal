unit U_OSM_Tile;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  Graphics,
  U_OSM_Coordinaat, U_OSM_Rect, U_OSM_TileServers,
  LazFileUtils;

Const
   TilesCachePath : String = '';
   //DefaultTileSource : String = 'Standard';
   DefaultTileSource : String = 'GrayScale';

Type
  TLoadState = (lsNone, lsBusy, lsError, lsWeb, lsDisk);
  TLoadStates = Set Of TLoadState;

  { TOsmTile }
  TOsmTile = class
    MyTileZoom,
    MyTileX,
    MyTileY : Integer;

    MyArea : TOsmRect;
  Const
    MyTileBase   : String = '';
    MyTileHeight : Integer = 256;
    MyTileWidth  : Integer = 256;
  Var
    MyLoaded   : TLoadState;
    MyPicture  : TPicture;
    MyBirth    : TDateTime;
    Function    LoadFromWeb(Basis : String) : Boolean;

    Procedure   SaveToCache;
    Function    LoadFromCache(Basis : String) : Boolean;
  Public
    Constructor Create;
    Destructor  Destroy; OVERRIDE;
// Create a TOsmTile which contains given Coordinate
    Constructor Create(Lon, Lat : Double; Z : Integer);
// Create a TOsmTile which contains given TOsmCoordinate
    Constructor Create(Position : TOsmCoordinate; Z : Integer);
// Create a TOsmTile for given Tile-index
    Constructor Create(x,y,z : Integer);

// Information Functions
// Amount of degrees in Long-direction for 1 pixel
//   Equal for all X-es in one zoomlevel
    Function    LonPerPixel : Double;
// Amount of degrees in Lat-direction for 1 pixel
//   Varies for all Y-s
    Function    LatPerPixel : Double;

// Load Tile synchronously
    Function    TryToLoad(Basis : String;
                          UseCache : Boolean = True;
                          DoSaveToCache : Boolean = True ) : Boolean;
// TODO: load asynchroon
// Load Tile asynchronously
    Property Picture : TPicture Read MyPicture;
    Property Area : TOsmRect Read MyArea;
    Property Loaded : TLoadState Read MyLoaded;

  end;

implementation

{ TOsmTile }
Constructor TOsmTile.Create;
begin
  Inherited Create;
  end;

Destructor TOsmTile.Destroy;
begin
  If MyPicture <> nil Then FreeAndNil(MyPicture);
  FreeAndNil(MyArea);
  inherited Destroy;
  end;

Constructor TOsmTile.Create(Lon,Lat : Double; Z : Integer);
Var
  Start : TOsmCoordinate;
begin
  Inherited Create;
  MyTileZoom := Z;

  Start := TOsmCoordinate.CreateByValues(Lon,Lat);
  MyTileX := Start.TileXForZoom(MyTileZoom);
  MyTileY := Start.TileYForZoom(MyTileZoom);
  FreeAndNil(Start);

  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);
  end;

Constructor TOsmTile.Create(Position : TOsmCoordinate; Z : Integer);
begin
  Inherited Create;
  MyTileZoom := Z;

  MyTileX := Position.TileXForZoom(MyTileZoom);
  MyTileY := Position.TileYForZoom(MyTileZoom);

  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);
  end;

Constructor TOsmTile.Create(x, y, z : Integer);
begin
  Inherited Create;
  MyTileZoom := Z;
  MyTileX := X;
  MyTileY := Y;
  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);
  end;

Function TOsmTile.LonPerPixel : Double;
begin
  Result := (MyArea.Right-MyArea.Left) / MyTileWidth;
  end;

Function TOsmTile.LatPerPixel : Double;
begin
  Result := (MyArea.Top-MyArea.Bottom) / MyTileHeight;
  end;


Function TOsmTile.TryToLoad( Basis : String;
                             UseCache : Boolean;
                             DoSaveToCache : Boolean) : Boolean;
begin
  MyLoaded := lsBusy;
  If UseCache Then Begin
    If LoadFromCache(Basis) Then Begin
      Result := True;
      MyLoaded := lsDisk;
      Exit;
      End;
    end;
  If LoadFromWeb(Basis) Then Begin
    If DoSaveToCache Then
      SaveToCache;
    MyLoaded := lsWeb;
    Result := True;
    End
  Else Begin
    MyLoaded := lsError;
    Result := False;
    End;
  end;


Function TOsmTile.LoadFromWeb(Basis : String) : Boolean;
begin
  MyTileBase := Basis;
  MyPicture := FetchTileFromServer(MyTileBase,MyTileX,MyTileY,MyTileZoom);
  If MyPicture <> nil Then Begin
    MyBirth := Now;
    Result := True;
    end;
  end;


// Helper functions for writing/reading tiles in cache
Function CreateCachePathFor(Basis : String; x,y,z : Integer) : String;
Var
  cp : String;
Begin
  cp := Format('%s/%s/%d/%d',[TilesCachePath,Basis,z,x]);
  ForceDirectory(cp);
  Result := Format('%s/%d.png',[cp,y]);
  end;

Function CachePathFor(Basis : String; x,y,z : Integer) : String;
Begin
  Result := Format('%s/%s/%d/%d/%d.png',[TilesCachePath,Basis,z,x,y]);
  end;

Function TOsmTile.LoadFromCache(Basis : String) : Boolean;
Var
  pp : String;
  p : TPicture;
begin
  MyTileBase := Basis;
  pp := CachePathFor(MyTileBase,MyTileX,MyTileY,MyTileZoom);
  p := TPicture.Create;
  Try
    P.LoadFromFile(pp);
    MyPicture := p;
    MyBirth := FileDateToDateTime(FileAge(pp));
    Result := True;
  Except
    FreeAndNil(P);
    Result := False;
    end;
  end;

Procedure TOsmTile.SaveToCache;
Var
  pp : String;
begin
  pp := CreateCachePathFor(MyTileBase,MyTileX,MyTileY,MyTileZoom);
  MyPicture.SaveToFile(pp);
  end;

initialization
  TilesCachePath := AppendPathDelim(GetUserDir + 'OsmTiles');

end.

