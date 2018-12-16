unit U_OSM_TileList;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  U_OSM_Tile;
  //fgl;

Type
//  TTilesList = Specialize TFPGList<TOsmTile>;
  TTilesList = class(TStringList)
    Function AddTile(Tile : TOsmTile) : Integer;
    Function ZoekTile(UTileName : String) : TOsmTile;
    Procedure RemoveTile(idx : Integer);
    end;

implementation

{ TTilesList }

Function TTilesList.AddTile(Tile : TOsmTile) : Integer;
begin
  Result := AddObject(Tile.UniqueTileName,Tile);
  end;

Procedure TTilesList.RemoveTile(idx : Integer);
Var
  t : TOsmTile;
begin
  t := TOsmTile(Objects[Idx]);
  If t<>nil Then Begin
    FreeAndNil(t);
    Objects[Idx] := nil;
    End;
  Delete(Idx);
  end;

Function TTilesList.ZoekTile(UTileName : String) : TOsmTile;
Var
  Idx : Integer;
begin
  Idx := IndexOf(UTileName);
  If Idx >= 0 Then Result := TOsmTile(Objects[Idx]);
  end;



end.

