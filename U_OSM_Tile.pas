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
  //TLoadTileThread = class;

  TLoadState = (lsNone, lsBusy, lsError, lsWeb, lsDisk);
  TLoadStates = Set Of TLoadState;
  TUpdatePicture = Procedure of Object;

  { TOsmTile }
  TOsmTile = class
  Private
    MyTileZoom,
    MyTileX,
    MyTileY : Integer;

    MyArea : TOsmRect;

    FLoadTileThread : TThread; // TLoadTileThread;
    FUpdatePicture : TUpdatePicture;
  Const
    MyTileBase   : String = '';
    MyTileHeight : Integer = 256;
    MyTileWidth  : Integer = 256;
  Var
    MyLoaded   : TLoadState;
    MyPicture  : TPicture;
    MyBirth    : TDateTime;
    Procedure   LoadResource(I : Integer);

    Function    LoadFromWeb(Basis : String) : Boolean;

    Procedure   SaveToCache;
    Function    LoadFromCache(Basis : String) : Boolean;
  Public
    Constructor Create;
    Destructor  Destroy; OVERRIDE;
    Function    UniqueTileName : String;
// Create a TOsmTile which contains given Coordinate
    Constructor Create(Basis : String; Lon, Lat : Double; Z : Integer);
// Create a TOsmTile which contains given TOsmCoordinate
    Constructor Create(Basis : String; Position : TOsmCoordinate; Z : Integer);
// Create a TOsmTile for given Tile-index
    Constructor Create(Basis : String; x,y,z : Integer);

// Information Functions
// Amount of degrees in Long-direction for 1 pixel
//   Equal for all X-es in one zoomlevel
    Function    LonPerPixel : Double;
// Amount of degrees in Lat-direction for 1 pixel
//   Varies for all Y-s
    Function    LatPerPixel : Double;

// Load Tile (synchronously)
    Function    TryToLoad( UseCache : Boolean = True;
                           DoSaveToCache : Boolean = True ) : Boolean;
// TODO: load asynchroon (uses thread to call TryToLoad)
    Procedure   LoadASynced;
    Procedure   UpdateTile;
// Load Tile asynchronously
    Property Picture : TPicture Read MyPicture;
    Property Area : TOsmRect Read MyArea;
    Property Loaded : TLoadState Read MyLoaded;
    Property TileLoaded : TUpdatePicture Read FUpdatePicture Write FUpdatePicture;

  end;

Function MakeUniqueTileName(TileBase : String; TileX, TileY, TileZoom : integer) : string;

implementation

Function MakeUniqueTileName( TileBase : String;
                         TileX, TileY, TileZoom : integer) : string;
begin
  Result := Format('%s_%d_%d_%d',
                   [TileBase,TileZoom,TileX,TileY]);
  End;


{ TLoadTileThread }
Type
  TLoadTileThread = class(TThread)
  private
    FOwnerTile : TOsmTile;
    procedure UpdateTile;
  protected
    procedure Execute; override;
  public
    Constructor Create(Owner : TOsmTile);
    end;

Constructor TLoadTileThread.Create(Owner : TOsmTile);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOwnerTile := Owner;
  Start;
  end;

procedure TLoadTileThread.UpdateTile;
begin
  FOwnerTile.UpdateTile;
  end;

procedure TLoadTileThread.Execute;
Var
  Tries : Integer = 10;
begin
  Repeat
    Dec(Tries);
    until FOwnerTile.TryToLoad Or
          (Tries < 1);
  Synchronize(@UpdateTile);
  end;




{ TOsmTile }
Constructor TOsmTile.Create;
begin
  Inherited Create;
  end;

Destructor TOsmTile.Destroy;
begin
  If FLoadTileThread <> nil Then FLoadTileThread.Terminate;
  If MyPicture <> nil Then FreeAndNil(MyPicture);
  FreeAndNil(MyArea);
  inherited Destroy;
  end;

Function TOsmTile.UniqueTileName : String;
begin
  Result := MakeUniqueTileName(MyTileBase,MyTileX,MyTileY,MyTileZoom);
  End;


Constructor TOsmTile.Create(Basis : String; Lon,Lat : Double; Z : Integer);
Var
  Start : TOsmCoordinate;
begin
  Inherited Create;
  MyTileBase := Basis;
  MyTileZoom := Z;

  Start := TOsmCoordinate.CreateByValues(Lon,Lat);
  MyTileX := Start.TileXForZoom(MyTileZoom);
  MyTileY := Start.TileYForZoom(MyTileZoom);
  FreeAndNil(Start);

  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);

  LoadResource(1);
  end;

Constructor TOsmTile.Create(Basis : String; Position : TOsmCoordinate; Z : Integer);
begin
  Inherited Create;
  MyTileBase := Basis;
  MyTileZoom := Z;

  MyTileX := Position.TileXForZoom(MyTileZoom);
  MyTileY := Position.TileYForZoom(MyTileZoom);

  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);

  LoadResource(2);
  end;

Constructor TOsmTile.Create(Basis : String; x, y, z : Integer);
begin
  Inherited Create;
  MyTileBase := Basis;
  MyTileZoom := Z;
  MyTileX := X;
  MyTileY := Y;
  MyArea := TOsmRect.CreateForTile(MyTileX,MyTileY,MyTileZoom);
  LoadResource(4);
  end;

Procedure TOsmTile.LoadResource(I : Integer);
begin
  If MyPicture = nil Then
    MyPicture := TPicture.Create;
  Case I of
    1 : MyPicture.LoadFromResourceName(HINSTANCE,'texture01');
    2 : MyPicture.LoadFromResourceName(HINSTANCE,'texture02');
    3 : MyPicture.LoadFromResourceName(HINSTANCE,'texture03');
    4 : MyPicture.LoadFromResourceName(HINSTANCE,'texture04');
    End;
  End;

Function TOsmTile.LonPerPixel : Double;
begin
  Result := (MyArea.Right-MyArea.Left) / MyTileWidth;
  end;

Function TOsmTile.LatPerPixel : Double;
begin
  Result := (MyArea.Top-MyArea.Bottom) / MyTileHeight;
  end;

// Thread om te laden
Procedure TOsmTile.LoadASynced;
begin
  FLoadTileThread := TLoadTileThread.Create(Self);
  end;

Procedure TOsmTile.UpdateTile;
begin
  If Assigned(FUpdatePicture) Then Begin
    FUpdatePicture;
    End;
  end;

// kijk waar we een plaatje kunnen krijgen (cache of web)
Function TOsmTile.TryToLoad( UseCache : Boolean;
                             DoSaveToCache : Boolean) : Boolean;
begin
  MyLoaded := lsBusy;
  If UseCache Then Begin
    If LoadFromCache(MyTileBase) Then Begin
      Result := True;
      MyLoaded := lsDisk;
      Exit;
      End;
    end;
  If LoadFromWeb(MyTileBase) Then Begin
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
  Result := False;
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

