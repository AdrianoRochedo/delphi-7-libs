{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       class       : TRAAlignPalette
       description : Align Palette

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDAlignPalette;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Buttons, ExtCtrls, RARegAuto;

type
  TRAAlignPalette = class(TForm)
    Panel1: TPanel;
    bnLeft: TSpeedButton;
    AlignPalPopup: TPopupMenu;
    Stayontop: TMenuItem;
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure RestoreDesktop(raProject: TRegAuto);
    procedure WriteDesktop(raProject: TRegAuto);
  end;

  procedure Create;
  procedure Free;

var
  AlignPalette: TRAAlignPalette;

implementation

uses RAUtils, RAFDDesigner, RAFD, RAFDDesktop;

{$R *.dfm}

procedure Create;
begin
  if AlignPalette = nil then
    AlignPalette := TRAAlignPalette.Create(Application);
end;

procedure Free;
begin
  AlignPalette.Free;
  AlignPalette := nil
end;

procedure TRAAlignPalette.ButtonClick(Sender: TObject);
begin
  if ActiveDesigner <> nil then
    ActiveDesigner.AlignSelection(TAlignSelection((Sender as TComponent).Tag));
end;

procedure TRAAlignPalette.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Name := 'AlignPalette';
  ClientWidth := Panel1.Width;
  ClientHeight := Panel1.Height;
  Caption := ResStr(deAlign, Caption);
  for i := 0 to Panel1.ControlCount - 1 do    { Iterate }
    Panel1.Controls[i].Hint := ResStr(deAlign + 1 + Panel1.Controls[i].Tag,
      TSpeedButton(Panel1.Controls[i]).Caption)
end;

procedure TRAAlignPalette.RestoreDesktop(raProject: TRegAuto);
begin
  FDDesktop.RestoreWindowPos(raProject, 'AlignmentPalette', Self);
end;

procedure TRAAlignPalette.WriteDesktop(raProject: TRegAuto);
begin
  FDDesktop.WriteWindowPos(raProject, 'AlignmentPalette', Self);
end;

initialization
finalization
  Free;
end.
