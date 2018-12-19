unit U_OSM_MapTileImage;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, ExtCtrls, Controls,
  U_OSM_Tile;

Type
// Object voor weergave van tile op map
// (TImage heeft bijna alles daarvoor)

  { TOsmMapTileImage }

  TOsmMapTileImage = class(TImage)
  Private
    MyTile : TOsmTile;
  Public
    Constructor CreateForTile( NewOwner : TComponent;
                        Tile : TOsmTile);

    Property  Tile : TOsmTile Read MyTile;
    Procedure UpdatePicture;
    end;

implementation

{ TOsmMapTileImage }

Constructor TOsmMapTileImage.CreateForTile(NewOwner : TComponent;
  Tile : TOsmTile);
begin
  Inherited Create(NewOwner);
  Parent := TWinControl(NewOwner);
  Height := 256;
  Width := 256;
  MyTile := Tile;
  Picture := Tile.Picture;
  SendToBack;
  end;

Procedure TOsmMapTileImage.UpdatePicture;
begin
  If Assigned(Tile.Picture) Then
    Picture := Tile.Picture;
  end;

end.

