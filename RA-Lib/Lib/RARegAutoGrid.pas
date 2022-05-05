{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : TRegAutoGrid
       description : component for saving grid columns
       required    : TRegAuto

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm

************************************************************}

{$INCLUDE RA.INC}

unit RARegAutoGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Grids,
  RARegAuto;

type
  TRegAutoGrid = class(TComponent)
  private
    FRegAuto : TRegAuto;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RegAuto : TRegAuto read FRegAuto write FRegAuto;
  end;

implementation

constructor TRegAutoGrid.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  with Owner do
    for i := 0 to ComponentCount-1 do
      if Components[i] is TRegAuto then begin
        FRegAuto := Components[i] as TRegAuto;
        break;
      end;
end;

procedure TRegAutoGrid.Loaded;
var
  i, j : integer;
begin
  inherited Loaded;
  with Owner do
    for j := 0 to ComponentCount-1 do
      if Components[j] is TCustomGrid then
        with TDrawGrid(Components[j] as TCustomGrid) do
          for i := 0 to ColCount-1 do
            ColWidths[i] := RegAuto.ReadInteger(Owner.Name+'\'+Name, 'ColumnWidth'+IntToStr(i), ColWidths[i]);
end;

procedure TRegAutoGrid.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : integer;
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TCustomGrid then
    with TDrawGrid(AComponent as TCustomGrid) do
      for i := 0 to ColCount-1 do
        RegAuto.WriteInteger(Owner.Name+'\'+Name, 'ColumnWidth'+IntToStr(i), ColWidths[i]);
end;

end.
