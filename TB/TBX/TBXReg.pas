unit TBXReg;

// TBX Package
// Copyright 2001 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXReg.pas,v 1.31 2003/05/12 04:44:32 Alex Exp $

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, Controls, SysUtils, Graphics, ImgList,
  {$IFDEF JR_D6} DesignIntf, DesignEditors, VCLEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB2Reg, TB2Toolbar, TB2Item, TBX, TBXMDI, TBXSwitcher, TB2DsgnItemEditor,
  TBXExtItems, TBXLists, TBXDkPanels, TBXToolPals, TBXStatusBars;

{$DEFINE NEWCOLORPROPERTY}
{*$DEFINE COMPATIBLE}

procedure Register;

type
  TThemeProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TMLStringProperty = class(TCaptionProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{$IFDEF JR_D5}
  TTBXLinkImageIndexPropertyEditor = class(TTBImageIndexPropertyEditor)
  public
    function GetImageListAt (Index: Integer): TCustomImageList; override;
  end;
{$ENDIF}

  TTBXColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
{$IFDEF JR_D5}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);{$IFNDEF JR_D6} override;{$ENDIF}
{$ENDIF}
  end;

  TTBXStatusBarEditor = class(TDefaultEditor)
  protected
{$IFDEF JR_D6}
    procedure GetPanelsProp(const Prop: IProperty);
{$ELSE}
    procedure GetPanelsProp(Prop: TPropertyEditor);
{$ENDIF}
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  Forms, TBXThemes, TBXStrEdit, TBXUtils, TypInfo;

type
  TTBXLinkAccess = class(TTBXCustomLink);


{ TThemeProperty }

function TThemeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TThemeProperty.GetValues(Proc: TGetStrProc);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  GetAvailableTBXThemes(SL);
  for I := 0 to SL.Count - 1 do Proc(SL[I]);
  SL.Free;
end;

{ TMLStringProperty }

function WordCount(const S: string; const Delims: TSysCharSet): Integer;
var
  L, I: Cardinal;
begin
  Result := 0;
  I := 1;
  L := Length(S);
  while I <= L do
  begin
    while (I <= L) and (S[I] in Delims) do Inc(I);
    if I <= L then Inc(Result);
    while (I <= L) and not(S[I] in Delims) do Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    { skip over delimiters }
    while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not(S[I] in WordDelims) do begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

procedure TMLStringProperty.Edit;
var
  Temp: string;
  Component: TPersistent;
  I, N: Integer;
begin
  with TStrEditDlg.Create(Application) do
  try
    Component := GetComponent(0);
    if Component is TComponent then Caption := TComponent(Component).Name + '.' + GetName
    else Caption := GetName;

    Temp := GetStrValue;
    N := WordCount(Temp, [#13, #10]);
    for I := 1 to N do Memo.Lines.Add(ExtractWord(I, Temp, [#13, #10]));

    Memo.MaxLength := GetEditLimit;
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

function TMLStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{$IFDEF JR_D5}
{ TTBXLinkImageIndexPropertyEditor }

function TTBXLinkImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  C: TPersistent;
begin
  Result := nil;
  C := GetComponent(Index);
  if C is TTBXCustomLink then
    Result := TTBXLinkAccess(C).Images;
end;
{$ENDIF}

{ TTBXColorProperty }

function TTBXColorProperty.GetValue: string;
begin
  Result := TBXColorToString(TColor(GetOrdValue));
end;

procedure TTBXColorProperty.GetValues(Proc: TGetStrProc);
begin
  TBXGetColorValues(Proc);
end;

procedure TTBXColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(TBXStringToColor(Value));
end;

{$IFDEF JR_D5}
procedure TTBXColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  begin
    if IsDarkColor(AColor) then
    begin
      Result := AColor;
      SetContrast(Result, AColor, 40);
    end
    else Result := clBlack;
  end;

var
  R: TRect;
  C: TColor;
  OldPenColor, OldBrushColor: TColor;
begin
  R := ARect;
  with ACanvas do
  try
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(R);
    R.Right := (ARect.Bottom - ARect.Top) + ARect.Left;
    InflateRect(R, -1, -1);
    C := TBXStringToColor(Value);
    if C <> clNone then
    begin
      Brush.Color := C;
      Pen.Color := ColorToBorderColor(ColorToRGB(C));
      Rectangle(R);
    end
    else
    begin
      Brush.Color := clWindow;
      Pen.Color := clBtnShadow;
      Rectangle(R);
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Right - 1, R.Top);
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Bottom);
    end;
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
  finally
    R.Left := R.Right;
    R.Right := ARect.Right;
    ACanvas.TextRect(R, R.Left + 1, R.Top + 1, Value);
  end;
end;
{$ENDIF}

{ TTBXStatusBarEditor }

procedure TTBXStatusBarEditor.Edit;
var
{$IFDEF JR_D6}
  Components: IDesignerSelections;
{$ELSE}
  {$IFDEF JR_D5}
  Components: TDesignerSelectionList;
  {$ELSE}
  Components: TComponentList;
  {$ENDIF}
{$ENDIF}
begin
{$IFDEF JR_D6}
  Components := CreateSelectionList;
{$ELSE}
  {$IFDEF JR_D5}
  Components := TDesignerSelectionList.Create;
  {$ELSE}
  Components := TComponentList.Create;
  {$ENDIF}
{$ENDIF}
  try
    Components.Add(Component);
    GetComponentProperties(Components, [tkClass], Designer, GetPanelsProp);
  finally
{$IFNDEF JR_D6}
    Components.Free;
{$ENDIF}
  end;
end;

procedure TTBXStatusBarEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TTBXStatusBarEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := '&Panels Editor...';
end;

function TTBXStatusBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF JR_D6}
procedure TTBXStatusBarEditor.GetPanelsProp(const Prop: IProperty);
begin
  if SameText(Prop.GetName, 'Panels') then Prop.Edit;
end;
{$ELSE}
procedure TTBXStatusBarEditor.GetPanelsProp(Prop: TPropertyEditor);
begin
  if CompareText(Prop.GetName, 'Panels') = 0 then Prop.Edit;
end;
{$ENDIF}


{ THookObj }

type
  THookObj = class
    procedure HookProc(Sender: TTBItemEditForm);
  end;

var O: THookObj;

procedure THookObj.HookProc(Sender: TTBItemEditForm);
var
  TB: TTBToolbar;
  Item: TTBCustomItem;
  NewItem: TTBItem;
  S: string;
  I: Integer;
begin
  TB := TTBToolbar.Create(Sender);
  TB.Top := Sender.Height;
  TB.Parent := Sender;
  TB.Align := alTop;
  TB.Images := Sender.ToolbarItems.SubMenuImages;
  TB.ShowHint := True;

  for I := 0 to Sender.MoreMenu.Count - 1 do
  begin
    Item := Sender.MoreMenu.Items[I];
    if Item is TTBCustomItem then
    begin
      S := TTBCustomItemClass(Item.Tag).ClassName;
      if StrLComp(PChar(S), 'TTBX', 4) = 0 then
      begin
        NewItem := TTBItem.Create(TB);
        TB.Items.Add(NewItem);
        NewItem.Caption := Item.Caption;
        NewItem.ImageIndex := Item.ImageIndex;
        NewItem.Tag := Item.Tag;
        NewItem.Hint := S;
        NewItem.OnClick := Item.OnClick;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Toolbar2000', [TTBXDock, TTBXMultiDock, TTBXToolbar,
    TTBXToolWindow, TTBXDockablePanel, TTBXPopupMenu, TTBXSwitcher, TTBXMRUList,
    TTBXMDIHandler, TTBXPageScroller, TTBXColorSet, TTBXAlignmentPanel,
    TTBXLabel, TTBXLink, TTBXCheckBox, TTBXRadioButton, TTBXStatusBar]);
  RegisterNoIcon([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXVisibilityToggleItem, TTBXLabelItem, TTBXMRUListItem, TTBXColorItem,
    TTBXMDIWindowItem, TTBXEditItem, TTBXSpinEditItem, TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXUndoList, TTBXToolPalette, TTBXColorPalette]);
{$IFDEF COMPATIBLE}
  RegisterNoIcon([TTBXList, TTBXComboItem, TTBXComboList]);
{$ENDIF}

  RegisterClasses([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXVisibilityToggleItem, TTBXLabelItem, TTBXMRUListItem, TTBXColorItem,
    TTBXMDIWindowItem, TTBXEditItem, TTBXSpinEditItem, TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXUndoList, TTBXToolPalette, TTBXColorPalette]);
{$IFDEF COMPATIBLE}
  RegisterClasses([TTBXList, TTBXComboItem, TTBXComboList]);
{$ENDIF}


  RegisterPropertyEditor(TypeInfo(string), TTBXCustomItem, 'Caption', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXLabelItem, 'Caption', TCaptionProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBToolbar, 'ChevronHint', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXToolbar, 'ChevronHint', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXSwitcher, 'Theme', TThemeProperty);
{$IFDEF JR_D5}
  RegisterPropertyEditor (TypeInfo(TImageIndex), TTBXCustomLink, 'ImageIndex', TTBXLinkImageIndexPropertyEditor);
{$ENDIF}
{$IFDEF NEWCOLORPROPERTY}
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TTBXColorProperty);
{$ENDIF}

  RegisterComponentEditor(TTBXStatusBar, TTBXStatusBarEditor);

  TBRegisterItemClass(TTBXItem, 'New &TBX Item', HInstance);
  TBRegisterItemClass(TTBXSubMenuItem, 'New TBX Submenu Item', HInstance);
  TBRegisterItemClass(TTBXSeparatorItem, 'New TBX Separator Item', HInstance);
  TBRegisterItemClass(TTBXVisibilityToggleItem, 'New TBX Visibility Toggle Item', HInstance);
  TBRegisterItemClass(TTBXLabelItem, 'New TBX Label Item', HInstance);
  TBRegisterItemClass(TTBXMRUListItem, 'New TBX MRU List Item', HInstance);
  TBRegisterItemClass(TTBXColorItem, 'New TBX Color Item', HInstance);
  TBRegisterItemClass(TTBXMDIWindowItem, 'New TBX MDI Window Item', HInstance);
  TBRegisterItemClass(TTBXEditItem, 'New TBX Edit Item', HInstance);
  TBRegisterItemClass(TTBXSpinEditItem, 'New TBX Spin Edit Item', HInstance);
  TBRegisterItemClass(TTBXDropDownItem, 'New TBX Drop Down Item', HInstance);
  TBRegisterItemClass(TTBXComboBoxItem, 'New TBX Combo Box Item', HInstance);
  TBRegisterItemClass(TTBXStringList, 'New TBX String List', HInstance);
  TBRegisterItemClass(TTBXUndoList, 'New TBX Undo List', HInstance);
  TBRegisterItemClass(TTBXToolPalette, 'New TBX Tool Palette', HInstance);
  TBRegisterItemClass(TTBXColorPalette, 'New TBX Color Palette', HInstance);
{$IFDEF COMPATIBLE}
  TBRegisterItemClass(TTBXComboItem, 'New TBX Combo Item (use TBX DropDown instead)', HInstance);
  TBRegisterItemClass(TTBXList, 'New TBX List (use TBX String List instead)', HInstance);
  TBRegisterItemClass(TTBXComboList, 'New TBX Combo List (use TBX Combo Box Instead)', HInstance);
{$ENDIF}

end;

initialization
  O := THookObj.Create;
  TBRegisterDsgnEditorHook(O.HookProc);

finalization
  TBRegisterDsgnEditorHook(O.HookProc);
  O.Free;

end.
