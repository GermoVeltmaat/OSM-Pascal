unit U_OSM_MapTileImageList;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  U_OSM_MapTileImage;

Type
  TMapTileImageList = class(TStringList)
    Function AddTile(MapTileImage : TOsmMapTileImage) : Integer;
    Function ZoekTile(UTileName : String) : TOsmMapTileImage;
    Procedure RemoveTile(idx : Integer);
    end;

implementation

{ TTilesList }

Function TMapTileImageList.AddTile(MapTileImage : TOsmMapTileImage) : Integer;
begin
  Result := AddObject(MapTileImage.Tile.UniqueTileName,MapTileImage);
  end;

Procedure TMapTileImageList.RemoveTile(idx : Integer);
Var
  t : TOsmMapTileImage;
begin
  t := TOsmMapTileImage(Objects[Idx]);
  If t<>nil Then Begin
    FreeAndNil(t);
    Objects[Idx] := nil;
    End;
  Delete(Idx);
  end;

Function TMapTileImageList.ZoekTile(UTileName : String) : TOsmMapTileImage;
Var
  Idx : Integer;
begin
  Idx := IndexOf(UTileName);
  If Idx >= 0 Then Result := TOsmMapTileImage(Objects[Idx])
              Else Result := nil;
  end;



end.

