{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       class       : TRAFDDesktop
       description : utilities for saving/restoring desktop

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDDesktop;

interface

uses Windows, SysUtils, Classes, Controls, Buttons, Forms, RARegAuto,
  TypInfo, Menus;

type

  TRAFDDesktop = class
  public
    procedure WriteWindowPos(raProject: TRegAuto; Section: string; Form: TForm);
    procedure RestoreWindowPos(raProject: TRegAuto; Section: string;
      Form: TForm);
   procedure RestoreButtons(raProject: TRegAuto; Section: string;
     Parent: TWinControl);
  end;

var
  FDDesktop: TRAFDDesktop;

implementation

uses RAUtils, RACnst;

procedure TRAFDDesktop.WriteWindowPos(raProject: TRegAuto; Section: string;
  Form: TForm);
begin
  raProject.WriteInteger(Section, 'Left', Form.Left);
  raProject.WriteInteger(Section, 'Top', Form.Top);
  raProject.WriteInteger(Section, 'Width', Form.Width);
  raProject.WriteInteger(Section, 'Height', Form.Height);
  raProject.WriteInteger(Section, 'ClientWidth', Form.ClientWidth);
  raProject.WriteInteger(Section, 'ClientHeight', Form.ClientHeight);
  raProject.WriteBool(Section, 'Visible', Form.Visible);
end;

procedure TRAFDDesktop.RestoreWindowPos(raProject: TRegAuto; Section: string;
  Form: TForm);
begin
  Form.SetBounds(raProject.ReadInteger(Section, 'Left', Form.Left),
    raProject.ReadInteger(Section, 'Top', Form.Top),
    raProject.ReadInteger(Section, 'Width', Form.Width),
    raProject.ReadInteger(Section, 'Height', Form.Height));
  Form.ClientWidth := raProject.ReadInteger(Section, 'ClientWidth',
    Form.ClientWidth);
  Form.ClientHeight := raProject.ReadInteger(Section, 'ClientHeight',
    Form.ClientHeight);
  Form.Visible := raProject.ReadBool(Section, 'Visible', Form.Visible);
end;

procedure TRAFDDesktop.RestoreButtons(raProject: TRegAuto; Section: string;
  Parent: TWinControl);

  function CaptionToHint(const Caption: string): string;
  begin
    Result := ReplaceString(Caption, '&', '');
    if Copy(Result, Length(Result) - 2, 3) = '...' then
      Delete(Result, Length(Result) - 2, 3);
  end;

const
  ButtonWidth = 25;
  ButtonHeight = 25;
  ButtonSpace = 5;
var
  i: Integer;
  Item: string;
  Button: TSpeedButton;
  Row: Integer;
  L: array[0..9] of Integer;
  BitmapResource: string;
  ItemType: string;
  MenuItem: TMenuItem;
begin
  for i := Low(L) to High(L) do
    L[i] := 3;
  for i := 0 to raProject.ReadInteger(Section, 'Count', 0) - 1 do
  begin
    Item := raProject.ReadString(Section, IntToStr(i), '');
    if Trim(Item) = '' then Continue;
    Row := StrToInt(Trim(SubStr(Item, 0, ',')));
    ItemType := Trim(SubStr(Item, 1, ','));
    if Cmp(ItemType, 'button') then
    begin
      MenuItem := Parent.Owner.FindComponent(Trim(SubStr(Item, 2, ','))) as
        TMenuItem;
      Button := TSpeedButton.Create(Parent.Owner);
      Button.SetBounds(L[Row], 5 + Row * ButtonHeight, ButtonWidth, ButtonHeight);
      inc(L[Row], ButtonHeight);
      if MenuItem <> nil then
      begin
        if MenuItem.Hint <> '' then
          Button.Hint := MenuItem.Hint
        else
          Button.Hint := CaptionToHint(MenuItem.Caption);
        Button.OnClick := MenuItem.OnClick;
      end;
      BitmapResource := Trim(SubStr(Item, 3, ','));
      if (Length(BitmapResource) > 0) and
         (BitmapResource[1] in StConstSymbols10) then
        Button.Glyph.Handle := LoadBitmap(hInstance,
          PChar(StrToInt(BitmapResource)))
      else
        Button.Glyph.Handle := LoadBitmap(hInstance, PChar(BitmapResource));
      if Button.Glyph.Width > Button.Glyph.Height then
        Button.NumGlyphs := 2;
      Button.Flat := True;
      Button.Parent := Parent;
    end
    else if Cmp(ItemType, 'space') then
      inc(L[Row], ButtonSpace)
    else if Cmp(ItemType, 'spacebutton') then
      inc(L[Row], ButtonWidth);
  end;
end;

(*
procedure TRAFDDesktop.RestoreButtons(raProject: TRegAuto; Section: string; Parent: TWinControl);
const
  ButtonWidth = 25;
  ButtonHeight = 25;
  ButtonSpace = 5;
var
  i: Integer;
  Item: string;
  Button: TSpeedButton;
  Row: Integer;
  L: array[0..1] of Integer;
  Method: TMethod;
  BitmapResource: string;
  ItemType: string;
begin
  L[0] := 3;
  L[1] := 3;
  for i := 0 to raProject.ReadInteger(Section, 'Count', 0) - 1 do
  begin
    Item := raProject.ReadString(Section, IntToStr(i), '');
    if Trim(Item) = '' then Continue;
    Row := StrToInt(Trim(SubStr(Item, 0, ',')));
    ItemType := Trim(SubStr(Item, 1, ','));
    if Cmp(ItemType, 'button') then
    begin
      Button := TSpeedButton.Create(Parent);
      Button.SetBounds(L[Row], 5 + Row * ButtonWidth, ButtonWidth, ButtonHeight);
      inc(L[Row], ButtonHeight);
      Button.Hint := Trim(SubStr(Item, 2, ','));
      Button.Flat := True;
      //Button.OnClick := TNotifyEvent(GetPropMethod(Parent.Owner, SubStr(Item, 0, ',')));
      Method.Data := Parent.Owner;
      Method.Code := Parent.Owner.MethodAddress(Trim(SubStr(Item, 3, ',')));
      if Method.Code <> nil then
        Button.OnClick := TNotifyEvent(Method);
      BitmapResource := Trim(SubStr(Item, 4, ','));
      if (Length(BitmapResource) > 0) and
         (BitmapResource[1] in StConstSymbols10) then
        Button.Glyph.Handle := LoadBitmap(hInstance, PChar(StrToInt(BitmapResource)))
      else
        Button.Glyph.Handle := LoadBitmap(hInstance, PChar(BitmapResource));
      Button.Glyph := Button.Glyph; { update NumGlyphs property }
      Button.Parent := Parent;
    end
    else if Cmp(ItemType, 'space') then
      inc(L[Row], ButtonSpace);
  end;
end;
*)

initialization
  FDDesktop := TRAFDDesktop.Create;
finalization
  FDDesktop.Free;
end.
