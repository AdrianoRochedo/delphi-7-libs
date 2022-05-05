{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : none
       description : Dephi IDE enhancement tool

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}
{$UNDEF RA_D5H}

unit Zoom;

interface

uses
  Windows, Classes, SysUtils, Forms, Dialogs, Menus;

  procedure RegisterZoom;

implementation


type

  TEEditorZoom = class
   {$IFDEF RA_D5H}
    (TNotifierObject, IUnknown, IOTAKeyboardBinding)
   {$ENDIF RA_D5H}
  private
    procedure Zoom(Sender : TObject);
  public
   {$IFDEF RA_D5H}
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
   {$ENDIF RA_D5H}
  end;


{$IFDEF RA_D5H}
procedure TEEditorZoom.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('B'), [ssCtrl])], BufferListProc, nil);
end;
{$ENDIF RA_D5H}

procedure Unregister;
var
  F : TForm;
  MenuItem : TMenuItem;
begin
  F := Application.FindComponent('AppBuilder') as TForm;
  if F <> nil then begin
    MenuItem := F.FindComponent('RAZoomEditor') as TMenuItem;
    if MenuItem <> nil then MenuItem.Free;
    MenuItem := F.FindComponent('RAZoomEditor2') as TMenuItem;
    if MenuItem <> nil then MenuItem.Free;
  end;
end;

procedure RegisterZoom;
var
  F : TForm;
  ViewsMenu, ViewNewEditorItem : TMenuItem;
  MenuItem : TMenuItem;
  Zoom : TEEditorZoom;
begin
  Unregister;
  Zoom := nil; {avoid warning}
  F := Application.FindComponent('AppBuilder') as TForm;
  if F <> nil then
  begin
   // ShowMessage('Found AppBuilder');
    ViewsMenu := F.FindComponent('ViewsMenu') as TMenuItem;
    if ViewsMenu = nil then exit; {error}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      Caption := 'Zoom Edit Window';
      ShortCut := Menus.ShortCut(ord('Z'), [ssAlt]);
      Name := 'RAZoomEditor';
      OnClick := Zoom.Zoom;
    end;
    ViewNewEditorItem := F.FindComponent('ViewNewEditorItem') as TMenuItem;
    if ViewNewEditorItem <> nil then
      ViewsMenu.Insert(ViewNewEditorItem.MenuIndex+1, MenuItem)
    else
      ViewsMenu.Add(MenuItem);
   {Additional shortcut}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      ShortCut := Menus.ShortCut(ord('1'), [ssAlt]);
      Name := 'RAZoomEditor2';
      OnClick := Zoom.Zoom;
      Visible := false;
    end;
    ViewsMenu.Add(MenuItem);
  end;
  {$IFDEF RA_D5H}
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TEEditorZoom.Create);
  {$ENDIF RA_D5H}
end;


procedure TEEditorZoom.Zoom(Sender : TObject);
var
  F : TForm;
  i : integer;
 // MenuItem: TMenuItem;
begin
  F := Screen.ActiveForm;
  if not F.ClassNameIs('TEditWindow') then begin
    F := nil;
    for i := 0 to Screen.FormCount - 1 do
      if Screen.Forms[i].ClassNameIs('TEditWindow') then begin
        F := Screen.Forms[i];
        break;
      end;
  end;
  if F <> nil then begin
    if F.WindowState <> wsMaximized then
      F.WindowState := wsMaximized
    else
      F.WindowState := wsNormal;
  end;
 { MenuItem := F.FindComponent('RAZoomEditor') as TMenuItem;
  if MenuItem <> nil then
    MenuItem.ShortCut := Menus.ShortCut(ord('Z'), [ssAlt]); }
end;


initialization
finalization
  Unregister;
end.
