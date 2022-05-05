{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       description : Menu Builder

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit fMenuBuilder;

interface

{$DEFINE PROPEDIT}

{.$DEFINE TESTAPP}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, DsgnIntf, RARegAuto
  {$IFNDEF TESTAPP} , ExptIntf, DsgnWnds {$ENDIF};

const
  WM_DROP             = WM_USER + 1;
  WM_UPDATEASELECTION = WM_USER + 2;

type

  TMenuItemWin = class;
  TMenuWin = class;

 {$IFDEF TESTAPP}
  TDesignWindow = class(TForm)
  public
    procedure FormModified; virtual; abstract;
    procedure FormClosed(AForm: TCustomForm); virtual; abstract;
  end;
 {$ENDIF}

  TMenuBuilder = class(TDesignWindow)
    MnuBuildPopup: TPopupMenu;
    InsertItem: TMenuItem;
    DeleteItem: TMenuItem;
    N1: TMenuItem;
    CreateSubmenu1: TMenuItem;
    PopupMenu1SelectMenu: TMenuItem;
    PopupMenu1SaveAsTemplate: TMenuItem;
    PopupMenu1InsertFromTemplate: TMenuItem;
    PopupMenu1DeleteTemplates: TMenuItem;
    InsertResourceItem: TMenuItem;
    N2: TMenuItem;
    RegAuto1: TRegAuto;
    TestMenu: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Reopen1: TMenuItem;
    File2: TMenuItem;
    New2: TMenuItem;
    Open2: TMenuItem;
    Save2: TMenuItem;
    SaveAs2: TMenuItem;
    N7: TMenuItem;
    Print2: TMenuItem;
    PrintSetup2: TMenuItem;
    N6: TMenuItem;
    Exit2: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Repeatcommand1: TMenuItem;
    N10: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    N9: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    N8: TMenuItem;
    Links1: TMenuItem;
    Object1: TMenuItem;
    Window2: TMenuItem;
    NewWindow2: TMenuItem;
    Tile2: TMenuItem;
    Cascade2: TMenuItem;
    ArrangeAll2: TMenuItem;
    N11: TMenuItem;
    Help3: TMenuItem;
    Contents3: TMenuItem;
    SearchforHelpOn3: TMenuItem;
    HowtoUseHelp3: TMenuItem;
    About3: TMenuItem;
    Hide2: TMenuItem;
    Show2: TMenuItem;
    Help2: TMenuItem;
    Contents2: TMenuItem;
    Index2: TMenuItem;
    Commands2: TMenuItem;
    Procedures2: TMenuItem;
    Keyboard2: TMenuItem;
    SearchforHelpOn2: TMenuItem;
    Tutorial2: TMenuItem;
    HowtoUseHelp2: TMenuItem;
    About2: TMenuItem;
    File11: TMenuItem;
    File21: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N4: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    N5: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    NewItem1: TMenuItem;
    SetCaption1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure InsertItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure CreateSubmenu1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetCaption1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    MenuFont: TFont;
    bmpArrow, bmpSelArrow: TBitmap;
    bmpCheck, bmpSelCheck: TBitmap;
    bmpBullet, bmpSelBullet: TBitmap;
    tmpBitmap: TBitmap;
    FOwner: TComponent;
    Menu: TComponent;
    procedure MakeMenuItems(AMenu: TMenu);
    procedure MenuMetricsChanged;
    procedure UpdateSelection;
    procedure WMUpdateaSelection(var Message: TMessage); message WM_UPDATEASELECTION;
   {$IFNDEF PROPEDIT}
   // procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
   {$ENDIF PROPEDIT}
  public
    procedure FormModified; override;
    procedure FormClosed(AForm: TCustomForm); override;
  end;

  TWhatCalcWidth = (cwCaption, cwShortCut, cwAll);

  TMenuItemWin = class(TCustomControl)
  private
    FSelected: Boolean;
    FMenuItem: TMenuItem;
    FSubMenu: TMenuWin;
    XX, YY: Integer;
    FIndex: Integer;
    procedure SetSelected(Value: Boolean);
    function GetIndex: integer;
    procedure SetIndex(const Value: integer);
    procedure WMDrop(var Message: TMessage); message WM_DROP;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure Changed;
  public
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalcWidth(const What: TWhatCalcWidth): integer;
    procedure ShowSubMenu;
    procedure HideSubMenu;
    function IsMainMenu: boolean;
    property Selected: Boolean read FSelected write SetSelected;
    property Index: Integer read FIndex write SetIndex;
  end;

  TMenuWin = class(TWinControl)
  private
    FShortCutWidth: Integer;
    FMainMenu: Boolean;
    ParentItemWin: TMenuItemWin;
    FMenuItem: TMenuItem;
    function GetItem(Index: Integer): TMenuItemWin;
    function GetItemCount: integer;
   {$IFNDEF PROPEDIT}
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
   {$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DragCanceled; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
   {$IFNDEF PROPEDIT}
    procedure StartTracking;
    procedure CancelTracking;
    function Tracking: Boolean;
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopulateMenu;
    procedure RefreshItems;
    function AddItem(AMenuItem: TMenuItem): TMenuItemWin;
    function InsertItem(const AIndex: Integer; AMenuItem: TMenuItem)
      : TMenuItemWin;
    procedure Unselect;
    procedure HideInactiveSubMenus;
    function HasMenuItemWin(MenuItemWin: TMenuItemWin): boolean;
   {$IFNDEF PROPEDIT}
    procedure HideMenu;
   {$ENDIF PROPEDIT}
    property ItemCount: Integer read GetItemCount;
    property Items [Index: Integer]: TMenuItemWin read GetItem;
  end;

 {$IFDEF TESTAPP}
  procedure TestMenuDesigner;
 {$ELSE}

 {$IFDEF RA_D3}
  procedure ShowMenuDesigner(ADesigner: TFormDesigner; AMenu: TMenu);
 {$ELSE}
  procedure ShowMenuDesigner(ADesigner: IFormDesigner; AMenu: TMenu);
 {$ENDIF}

 {$ENDIF TESTAPP}

implementation

uses RAUtils, packconst, iMTracer;

{$R *.dfm}

var
  MenuBuilder: TMenuBuilder;
  RootMenu: TMenuWin;
  MenuHeight: Integer;
  CurItem: TMenuItemWin;
  DragWinList: TList;
  DragItemList: TList;
  FLeftDown: Boolean;
  HFrame: Integer;

type

  TStubItem = class(TMenuItem)
  private
    InMenuChanged: Boolean;
    FMenuItemWin: TMenuItemWin;
  protected
    procedure MenuChanged(Rebuild: Boolean); override;
  end;


{$IFDEF TESTAPP}

procedure TestMenuDesigner;
begin
  Application.CreateForm(TMenuBuilder, MenuBuilder);
  with MenuBuilder do
  begin
    FOwner := TestMenu.Owner;
    MakeMenuItems(TestMenu);
    RootMenu.Visible := True;
    Show;
  end;    { with }
end;    { TestMenuBuilder }

{$ELSE}

{$IFDEF RA_D3}
procedure ShowMenuDesigner(ADesigner: TFormDesigner; AMenu: TMenu);
{$ELSE}
procedure ShowMenuDesigner(ADesigner: IFormDesigner; AMenu: TMenu);
{$ENDIF}
begin
  if not Assigned(MenuBuilder) then
    MenuBuilder := TMenuBuilder.Create(Application);
  with MenuBuilder do
  begin
    Designer := ADesigner;
    FOwner := AMenu.Owner;
    MakeMenuItems(AMenu);
    RootMenu.Visible := True;
    Show;
  end;    { with }
end;
{$ENDIF TESTAPP}

procedure DrawBitmap(Canvas: TCanvas; const X, Y: Integer;
  Bitmap, SelBitmap: TBitmap; const Selected: Boolean);
begin
  if Selected then
    Canvas.Draw(X, Y, SelBitmap) else
    Canvas.Draw(X, Y, Bitmap);
end;    { DrawBitmap }

procedure TStubItem.MenuChanged(Rebuild: Boolean);
var
  AMenuItem: TMenuItem;
begin
  inherited MenuChanged(Rebuild);
  if InMenuChanged then Exit;
  InMenuChanged := True;
  try
   { change stub to menu item }
    AMenuItem := TMenuItem.Create(MenuBuilder.FOwner);
   { AMenuItem.Assign(Self); }
    AMenuItem.Caption     := Caption    ;
    AMenuItem.Name        := Name       ;
    AMenuItem.Break       := Break      ;
    AMenuItem.Checked     := Checked    ;
    AMenuItem.Default     := Default    ;
    AMenuItem.Enabled     := Enabled    ;
    AMenuItem.GroupIndex  := GroupIndex ;
    AMenuItem.HelpContext := HelpContext;
    AMenuItem.Hint        := Hint       ;
    AMenuItem.RadioItem   := RadioItem  ;
    AMenuItem.ShortCut    := ShortCut   ;
    AMenuItem.Tag         := Tag        ;
    AMenuItem.Visible     := Visible    ;
    FMenuItemWin.Selected := False;
    with Owner as TMenuWin do
    begin
      FMenuItem.Add(AMenuItem);
      CurItem := InsertItem(FMenuItemWin.Index, AMenuItem);
      CurItem.Selected := True;
      FMenuItemWin.Index := ItemCount - 1;
      PostMessage(MenuBuilder.Handle, WM_UPDATEASELECTION, 0, 0);
    end;    { with }
   { restore default properties }
    Caption     := '';
    Name        := '';
    Break       := mbNone;
    Checked     := False;
    Default     := False;
    Enabled     := True;
    GroupIndex  := 0;
    HelpContext := 0;
    Hint        := '';
    RadioItem   := False;
    ShortCut    := 0;
    Tag         := 0;
    Visible     := True;
  finally
    InMenuChanged := False;
  end;    { try/finally }
end;


{ TMenuWin }
constructor TMenuWin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clMenu;
end;    { Create }

procedure TMenuWin.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not FMainMenu then
    with Params do
    begin
     {$IFDEF PROPEDIT}
      Style := Style or WS_DLGFRAME;
     {$ELSE}
      Style := WS_POPUP;
      ExStyle := WS_EX_DLGMODALFRAME;
     {$ENDIF PROPEDIT}
    end;
end;

function TMenuWin.GetItem(Index: Integer): TMenuItemWin;
begin
  Result := Controls[Index] as TMenuItemWin;
end;

function TMenuWin.GetItemCount: integer;
begin
  Result := ControlCount;
end;    {  }

procedure TMenuWin.AlignControls(AControl: TControl; var Rect: TRect);

  procedure AdjustBounds;
  var
    MaxWidth: integer;
    i: integer;
  begin
      if FMainMenu then MaxWidth := 0 else MaxWidth := 50;
      FShortCutWidth := MenuHeight;
      for i := 0 to ItemCount - 1 do
        with Items[i] do
        begin
          if FMainMenu then
          begin
            Width := CalcWidth(cwCaption);
            inc(MaxWidth, Width);
          end
          else
          begin
            MaxWidth := Max(MaxWidth, CalcWidth(cwCaption));
            FShortCutWidth := Max(FShortCutWidth, CalcWidth(cwShortCut));
          end;
        end;
      if FMainMenu then
      begin
        Width := MaxWidth;
        Height := MenuHeight;
      end
      else
      begin
        Width := MaxWidth + FShortCutWidth + MenuHeight;
        Height := ItemCount * MenuHeight + HFrame * 2;
      end;
  end;    { AdjustBounds }

var
  i: Integer;
  L1: Integer;
begin
  AdjustBounds;
  L1 := 0;
  for i := 0 to ItemCount - 1 do
  begin
    with Items[i] do
    begin
      Index := i;
      if FMainMenu then
      begin
        SetBounds(L1, 0, Width, MenuHeight);
        inc(L1, Width);
      end
      else
        SetBounds(0, MenuHeight * i, Self.ClientWidth, MenuHeight);
    end;
  end;    { for }
end;

function TMenuWin.AddItem(AMenuItem: TMenuItem): TMenuItemWin;
begin
  Result := InsertItem(ItemCount, AMenuItem);
end;

function TMenuWin.InsertItem(const AIndex: Integer; AMenuItem: TMenuItem)
  : TMenuItemWin;
begin
  if not Assigned(AMenuItem) then
    raise Exception.Create('MenuItem can''t be nil');
  Result := TMenuItemWin.Create(RootMenu);
  with Result do
  begin
    FMenuItem := AMenuItem;
    Parent := Self;
    Index := Min(AIndex, Self.ItemCount);
  end;    { with }
end;    { InsertItem }

procedure TMenuWin.PopulateMenu;
var
  i: integer;
 {$IFDEF PROPEDIT}
  StubItem: TStubItem;
 {$ENDIF PROPEDIT}
begin
  DisableAlign;
  try
    while ControlCount > 0 do Controls[0].Free;
    for i := 0 to FMenuItem.Count - 1 do
      AddItem(FMenuItem[i]);
   {$IFDEF PROPEDIT}
    if (FMenuItem.Count = 0) or (FMenuItem[FMenuItem.Count - 1].Caption <> '') then
    begin
      StubItem := TStubItem.Create(Self);
     // FMenuItem.Add(StubItem);
      StubItem.FMenuItemWin := AddItem(StubItem);
    end;
   {$ENDIF PROPEDIT}
  finally
    EnableAlign;
  end;    { try/finally }
end;    { PopulateMenu }

procedure TMenuWin.RefreshItems;
begin
  while ItemCount > 0 do Items[0].Free;
 {$IFDEF PROPEDIT}
  Parent := MenuBuilder;
 {$ELSE}
  { avoid scrollbar on Parent }
  ParentWindow := MenuBuilder.Handle;
 {$ENDIF PROPEDIT}
  PopulateMenu;
end;    { RefreshItems }

procedure TMenuWin.DragCanceled;
begin
  inherited DragCanceled;
  FLeftDown := False;
end;

procedure TMenuWin.Unselect;
var
  i: integer;
begin
  for i := 0 to ItemCount - 1 do
    with Items[i] do
    begin
      Selected := False;
      if Assigned(FSubMenu) then FSubMenu.Unselect;
    end;
end;    { Unselect }

procedure TMenuWin.HideInactiveSubMenus;
var
  i: integer;
  MenuItemWin: TMenuItemWin;
begin
  for i := 0 to ItemCount - 1 do
  begin
    MenuItemWin := Items[i];
    if (MenuItemWin <> CurItem) and Assigned(MenuItemWin.FSubMenu) then
    begin
      if not Assigned(CurItem) or
         (CurItem.Parent <> MenuItemWin.Parent) then
        MenuItemWin.Selected := False;
      MenuItemWin.FSubMenu.HideInactiveSubMenus;
      if not MenuItemWin.FSubMenu.HasMenuItemWin(CurItem) then
        MenuItemWin.HideSubMenu;
    end;
  end;
end;    { HideInactiveSubMenus }

{$IFNDEF PROPEDIT}
procedure TMenuWin.HideMenu;
var
  i: integer;
  MenuItemWin: TMenuItemWin;
begin
  for i := 0 to ItemCount - 1 do
  begin
    MenuItemWin := Items[i];
    if Assigned(MenuItemWin.FSubMenu) then
    begin
      MenuItemWin.Selected := False;
      MenuItemWin.FSubMenu.HideMenu;
      MenuItemWin.HideSubMenu;
    end;
  end;
  if not FMainMenu then Hide;
end;
{$ENDIF PROPEDIT}

function TMenuWin.HasMenuItemWin(MenuItemWin: TMenuItemWin): boolean;
var
  i: Integer;
begin
  Result := ContainsControl(MenuItemWin);
  if not Result then
  begin
    for i := 0 to ItemCount - 1 do
      with Items[i] do
      begin
        if Assigned(FSubMenu) then
        begin
          Result := FSubMenu.HasMenuItemWin(MenuItemWin);
          if Result then Exit;
        end;
      end;
  end;
end;    { HasMenuItemWin }

{$IFNDEF PROPEDIT}
{ ******************* Tracking mouse *******************  }

function BoolToStr(const Value: Boolean): string;
const
  Bool: array[Boolean] of string = ('False', 'True');
begin
  Result := Bool[Value];
end;    {  }

procedure TMenuWin.WMLButtonUp(var Message: TWMLButtonUp);
var
  WinControl: TWinControl;
  P: TPoint;
begin
  ODS('TMenuWin.MouseUp MouseCapture = ' + BoolToStr(RootMenu.MouseCapture));

  P := ClientToScreen(Point(Message.XPos, Message.YPos));
  WinControl := FindVCLWindow(ClientToScreen(
    Point(Message.XPos, Message.YPos)));
  if (WinControl is TMenuItemWin) or (WinControl is TMenuWin) then
  begin
    P := WinControl.ScreenToClient(P);
      WinControl.Perform(WM_LBUTTONUP, TMessage(Message).WParam, P.X);
  end
  else
    CancelTracking;
end;

procedure TMenuWin.WMMouseMove(var Message: TWMMouseMove);
var
  WinControl: TWinControl;
  P: TPoint;
begin
  if Tracking then
  begin
    ODS('TMenuWin.MouseMove MouseCapture = ' + BoolToStr(RootMenu.MouseCapture));
    P := ClientToScreen(Point(Message.XPos, Message.YPos));
    WinControl := FindVCLWindow(P);
    if (WinControl is TMenuItemWin) or (WinControl is TMenuWin) then
    begin
      P := WinControl.ScreenToClient(P);
      WinControl.Perform(WM_MOUSEMOVE, TMessage(Message).WParam, P.X);
    end;
  end;
end;

procedure TMenuWin.StartTracking;
begin
  MouseCapture := True;
end;

procedure TMenuWin.CancelTracking;
begin
  MouseCapture := False;
  HideMenu;
end;    { CancelTracking }

function TMenuWin.Tracking: Boolean;
begin
  Result := MouseCapture;
end;

{ ################### Tracking mouse ###################  }
{$ENDIF PROPEDIT}


{ TMenuItemWin }
constructor TMenuItemWin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  Font := MenuBuilder.MenuFont;
  Height := MenuHeight;
end;    { Create }

destructor TMenuItemWin.Destroy;
begin
  DragWinList.Remove(Self);
  inherited Destroy;
end;    { Destroy }

function TMenuItemWin.GetIndex: integer;
var
  i : integer;
begin
  Result := FIndex;
  if Parent <> nil then
  begin
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] = Self then
      begin
        Result := i;
        break;
      end;
  end;
end;

procedure TMenuItemWin.SetIndex(const Value: integer);
begin
  if FIndex <> Value then
  begin
    FIndex := Value;
    if Parent <> nil then
      TMenuWin(Parent).SetChildOrder(Self, Value);
    FIndex := GetIndex;
    RequestAlign;
  end;
end;

procedure TMenuItemWin.Paint;
var
  R: TRect;
  ShortCutText: string;
  Bitmap, SelBitmap: TBitmap;
begin
  inherited Paint;
  R := ClientRect;
  Canvas.Font := Font;
  if FSelected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end
  else
  begin
    Canvas.Font.Color := clMenuText;
    Canvas.Brush.Color := clMenu;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);
  if FMenuItem.Caption = '-' then
  begin
    inc(R.Top, ClientHeight div 2 - 1);
    DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_TOP);
  end
  else
  begin
    if IsMainMenu then
      inc(R.Left, MenuHeight div 2) else
      inc(R.Left, MenuHeight);
    DrawText(Canvas.Handle, PChar(FMenuItem.Caption), -1, R, 0);
    if not IsMainMenu then
    begin
      ShortCutText := ShortCutToText(FMenuItem.ShortCut);
      if ShortCutText <> '' then
      begin
        with Parent as TMenuWin do
          R.Left := R.Right - FShortCutWidth;
        DrawText(Canvas.Handle, PChar(ShortCutText), -1, R, 0);
      end;
      if FMenuItem.Count > 0 then
        DrawBitmap(Canvas, ClientWidth - MenuBuilder.bmpArrow.Width,
          (ClientHeight - MenuBuilder.bmpArrow.Height) div 2,
          MenuBuilder.bmpArrow, MenuBuilder.bmpSelArrow, FSelected);
      if FMenuItem.Checked then
      begin
        if FMenuItem.RadioItem then
        begin
          Bitmap := MenuBuilder.bmpBullet;
          SelBitmap := MenuBuilder.bmpSelBullet;
        end
        else
        begin
          Bitmap := MenuBuilder.bmpCheck;
          SelBitmap := MenuBuilder.bmpSelCheck;
        end;
        DrawBitmap(Canvas, Bitmap.Width div 4,
          (ClientHeight - Bitmap.Height) div 2,
          Bitmap, SelBitmap, FSelected);
      end;
    end;
  end;
 {$IFDEF PROPEDIT}
  if FMenuItem.Caption = '' then
  begin
    R := ClientRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
 {$ENDIF PROPEDIT}
end;

function TMenuItemWin.IsMainMenu: boolean;
begin
  Result := (Parent as TMenuWin).FMainMenu;
end;

function TMenuItemWin.CalcWidth(const What: TWhatCalcWidth): integer;

  function CutAmps(const S: string): string;
  var
    i: Integer;
  begin
    Result := '';
    if S <> '' then
    begin
      for i := 1 to Length(S) - 1 do
        if not ((S[i] = '&') and (S[i+1] <> '&')) then
          Result := Result + S[i];
      Result := Result + S[Length(S)];
    end;
  end;

begin
  Canvas.Font := Font;
  Result := 0;
  if Caption <> '-' then
  begin
    if What in [cwCaption, cwAll] then
    begin
      Result := MenuHeight + Canvas.TextWidth(CutAmps(FMenuItem.Caption));
      if FMenuItem.Caption = '' then
        inc(Result,  MenuHeight);
    end;
    if What = cwAll then
      inc(Result, 2 * MenuHeight);
    if (What in [cwShortCut, cwAll]) and (FMenuItem.ShortCut <> 0) and
       (FMenuItem.Caption <> '-') then
      inc(Result, MenuHeight + Canvas.TextWidth(
        ShortCutToText(FMenuItem.ShortCut)));
  end;
end;    {  }

procedure TMenuItemWin.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    CurItem := Self;
    RootMenu.HideInactiveSubMenus;
    if ssCtrl in Shift then
      Selected := not Selected
    else if not Selected then
    begin
      RootMenu.Unselect;
      Selected := True;
    end;
    if FMenuItem.Count > 0 then
      ShowSubMenu;
    XX := X;
    YY := Y;
    FLeftDown := True;
   {$IFNDEF PROPEDIT}
    RootMenu.StartTracking;
    ODS('MouseDown MouseCapture = ' + BoolToStr(RootMenu.MouseCapture));
  //  SetCapture(RootMenu.Handle);
   {$ENDIF PROPEDIT}
  end;
end;

procedure TMenuItemWin.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure AddSelected(MenuWin: TMenuWin);
  var
    i: Integer;
  begin
    for i := 0 to MenuWin.ItemCount - 1 do
    begin
      if MenuWin.Items[i].Selected then
      begin
        DragWinList.Add(MenuWin.Items[i]);
        DragItemList.Add(MenuWin.Items[i].FMenuItem);
      end;
      if Assigned(MenuWin.Items[i].FSubMenu) then
        AddSelected(MenuWin.Items[i].FSubMenu);
    end;
  end;    { AddSelected }

begin
 {$IFDEF PROPEDIT}
  if FLeftDown and ((Abs(XX - X) > 4) or (Abs(YY - Y) > 4)) then
  begin
    DragWinList.Clear;
    DragItemList.Clear;
    AddSelected(RootMenu);
    RootMenu.BeginDrag(True);
  end;
 {$ELSE}
  ODS('MouseMove');
  if RootMenu.Tracking then
  begin
    CurItem := Self;
    if not Selected then
    begin
      RootMenu.Unselect;
      Selected := True;
      RootMenu.HideInactiveSubMenus;
      if (FMenuItem.Count > 0) and (not Assigned(FSubMenu) or
         not (FSubMenu.Visible)) then
        ShowSubMenu;
    end;
  end;
 {$ENDIF}
end;

procedure TMenuItemWin.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
 {$IFDEF PROPEDIT}
  if Button = mbLeft then
  begin
    if not (ssCtrl in Shift) then
    begin
      RootMenu.Unselect;
      Selected := True;
    end;
  end;
  FLeftDown := False;
  MenuBuilder.UpdateSelection;
 {$ELSE}
  if FMenuItem.Count = 0 then
    RootMenu.CancelTracking;
 {$ENDIF PROPEDIT}
end;

procedure TMenuItemWin.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  CurItem := Self;
  if not Selected then
  begin
    RootMenu.Unselect;
    Selected := True;
    RootMenu.HideInactiveSubMenus;
    if (FMenuItem.Count > 0) and (not Assigned(FSubMenu) or
       not (FSubMenu.Visible)) then
      ShowSubMenu;
  end;
  Accept := DragItemList.IndexOf(FMenuItem) = -1;
 { BUGBUG: circulate not checked }
end;

procedure TMenuItemWin.DragDrop(Source: TObject; X, Y: Integer);
begin
  FLeftDown := False;
  PostMessage(Handle, WM_DROP, 0, 0);
end;

procedure TMenuItemWin.WMDrop;
var
  i: integer;
  MenuItem: TMenuItem;
  MenuItemWin: TMenuItemWin;
begin
  for i := 0 to DragItemList.Count - 1 do
  begin
    MenuItem := TMenuItem(DragItemList[i]);
   { stub items can't be moved }
    if Assigned(MenuItem.Parent) then
    begin
      MenuItem.Parent.Remove(MenuItem);
      if Assigned(FMenuItem.Parent) then
        (Parent as TMenuWin).FMenuItem.Insert(FMenuItem.MenuIndex, MenuItem)
      else
        with (Parent as TMenuWin).FMenuItem do Insert(Count, MenuItem);
    end;
  end;    { for }
  Selected := False;
  CurItem := Self;
  for i := 0 to DragWinList.Count - 1 do
  begin
    MenuItemWin := TMenuItemWin(DragWinList[i]);
   { stub items can't be moved }
    if Assigned(MenuItemWin.FMenuItem.Parent) then
    begin
      CurItem := MenuItemWin;
      MenuItemWin.FSelected := True;
      MenuItemWin.Visible := False;
      MenuItemWin.Parent := Parent;
      MenuItemWin.Index := MenuItemWin.FMenuItem.MenuIndex;
      MenuItemWin.Visible := True;
    end;
  end;    { for }
  RootMenu.HideInactiveSubMenus;

  DragWinList.Clear;
  DragItemList.Clear;
  MenuBuilder.UpdateSelection;
end;

procedure TMenuItemWin.Changed;
begin
  Invalidate;
end;    { Changed }

procedure TMenuItemWin.ShowSubMenu;
var
 {$IFNDEF PROPEDIT}
  R: TRect;
 {$ENDIF PROPEDIT}
  P: TPoint;
begin
  Application.ProcessMessages; { update selection painting }

  if not Assigned(FSubMenu) then
  begin
    FSubMenu := TMenuWin.Create(Self);
    FSubMenu.FMenuItem := FMenuItem;
    FSubMenu.ParentItemWin := Self;
    FSubMenu.Visible := False;
    FSubMenu.RefreshItems;
  end;
  if IsMainMenu then
    P := Point(Parent.BoundsRect.Left + Left, Parent.BoundsRect.Bottom)
  else
    P := Point(Parent.BoundsRect.Right - 4, Parent.BoundsRect.Top + Top);
 {$IFNDEF PROPEDIT}
  if Parent = RootMenu then
    P := Point(Parent.ClientOrigin.X + P.X, Parent.ClientOrigin.Y + P.Y);
  R := Rect(0, 0, 0, 0);
  FSubMenu.AlignControls(nil, R);
 {$ENDIF PROPEDIT}
  FSubMenu.SetBounds(P.X, P.Y, FSubMenu.Width, FSubMenu.Height);
  FSubMenu.Visible := True;
end;    { ShowSubMenu }

procedure TMenuItemWin.HideSubMenu;
begin
  if Assigned(FSubMenu) then
  begin
    FSubMenu.Visible := False;
    if not RootMenu.Dragging then
    begin
      FSubMenu.Free;
      FSubMenu := nil;
    end;
  end;
end;    { HideSubMenu }

procedure TMenuItemWin.SetSelected(Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Changed;
  end;
end;



{ TMenuBuilder }
procedure TMenuBuilder.FormCreate(Sender: TObject);
begin
  InsertItem.Caption := SMDInsertItem;
  DeleteItem.Caption := SMDDeleteItem;
  CreateSubmenu1.Caption := SMDCreateSubmenuItem;
  DragWinList := TList.Create;
  DragItemList := TList.Create;
  RootMenu := TMenuWin.Create(Self);
  RootMenu.Parent := Self;
  RootMenu.Visible := False;
  MenuFont := TFont.Create;
  bmpArrow := TBitmap.Create;
  bmpCheck := TBitmap.Create;
  bmpBullet := TBitmap.Create;
  bmpSelArrow := TBitmap.Create;
  bmpSelCheck := TBitmap.Create;
  bmpSelBullet := TBitmap.Create;
  tmpBitmap := TBitmap.Create;
 {$IFNDEF TESTAPP}
  if Assigned(ToolServices) then
    RegAuto1.RegPath := ToolServices.GetBaseRegistryKey
  else
    RegAuto1.UseReg := False;
 {$ENDIF TESTAPP}
  MenuMetricsChanged;
end;

procedure TMenuBuilder.FormDestroy(Sender: TObject);
begin
  MenuFont.Free;
  bmpArrow.Free;
  bmpCheck.Free;
  bmpBullet.Free;
  bmpSelArrow.Free;
  bmpSelCheck.Free;
  bmpSelBullet.Free;
  tmpBitmap.Free;
  DragWinList.Free;
  DragItemList.Free;
  MenuBuilder := nil;
end;

procedure TMenuBuilder.MenuMetricsChanged;

  procedure Colorize(Bitmap: TBitmap; Color: TColor);
  var
    R: TRect;
  begin
    R := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
    tmpBitmap.Canvas.Brush.Color := Color;
    tmpBitmap.Canvas.FillRect(R);
    Bitmap.Canvas.CopyMode := cmSrcErase;
    Bitmap.Canvas.CopyRect(R, tmpBitmap.Canvas, R);
  end;

var
  R: TRect;
  M: TNONCLIENTMETRICS;
begin
  M.cbSize := sizeof(M);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @M, 0);
  MenuFont.Name := M.lfMenuFont.lfFaceName;
  MenuFont.Height := M.lfMenuFont.lfHeight;
  MenuFont.Style := [];
  if M.lfMenuFont.lfItalic = 1 then MenuFont.Style := MenuFont.Style + [fsItalic];
  if M.lfMenuFont.lfWeight >= FW_BOLD then MenuFont.Style := MenuFont.Style + [fsBold];
  if M.lfMenuFont.lfUnderline = 1 then MenuFont.Style := MenuFont.Style + [fsUnderline];
  if M.lfMenuFont.lfStrikeOut = 1 then MenuFont.Style := MenuFont.Style + [fsStrikeOut];
  MenuFont.Charset := TFontCharset(M.lfMenuFont.lfCharSet);
  MenuHeight := GetSystemMetrics(SM_CYMENUSIZE);

  bmpArrow.Width := GetSystemMetrics(SM_CXMENUCHECK);
  bmpArrow.Height := GetSystemMetrics(SM_CYMENUCHECK);
  bmpCheck.Assign(bmpArrow); { set size }
  bmpBullet.Assign(bmpArrow); { set size }
  tmpBitmap.Assign(bmpArrow); { set size }
  R := Bounds(0, 0, bmpArrow.Width, bmpArrow.Height);
  DrawFrameControl(bmpArrow.Canvas.Handle, R, DFC_MENU, DFCS_MENUARROW);
  DrawFrameControl(bmpCheck.Canvas.Handle, R, DFC_MENU, DFCS_MENUCHECK);
  DrawFrameControl(bmpBullet.Canvas.Handle, R, DFC_MENU, DFCS_MENUBULLET);
  bmpArrow.Transparent := True;
  bmpCheck.Transparent := True;
  bmpBullet.Transparent := True;
  bmpSelArrow.Assign(bmpArrow);
  bmpSelCheck.Assign(bmpCheck);
  bmpSelBullet.Assign(bmpBullet);
  if Integer(GetSysColor(COLOR_MENUTEXT)) <> clBlack then
  begin
    Colorize(bmpArrow, clMenuText);
    Colorize(bmpCheck, clMenuText);
    Colorize(bmpBullet, clMenuText);
  end;
  Colorize(bmpSelArrow, clHighLightText);
  Colorize(bmpSelCheck, clHighLightText);
  Colorize(bmpSelBullet, clHighLightText);

  HFrame := GetSystemMetrics(SM_CYFIXEDFRAME);
end;    { UpdateMenu }

procedure TMenuBuilder.MakeMenuItems(AMenu: TMenu);
begin
  while RootMenu.ItemCount > 0 do
    RootMenu.Items[0].Free;
  Menu := AMenu;
  RootMenu.FMainMenu := Menu is TMainMenu;
  if RootMenu.FMainMenu then
  begin
    RootMenu.SetBounds(0, 0, 100, 100);
    RootMenu.FMenuItem := (Menu as TMainMenu).Items;
  end
  else
  begin
    RootMenu.SetBounds(20, 20, 100, 100);
    RootMenu.FMenuItem := (Menu as TPopupMenu).Items;
  end;
  RootMenu.PopulateMenu;
end;    { MakeMenuItems }

procedure TMenuBuilder.InsertItemClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  MenuItemWin: TMenuItemWin;
begin
  if not Assigned(CurItem) then Exit;
  MenuItem := TMenuItem.Create(FOwner);
  MenuItemWin := (CurItem.Parent as TMenuWin).InsertItem(
    CurItem.Index, MenuItem);
  (CurItem.Parent as TMenuWin).FMenuItem.Insert(MenuItemWin.Index, MenuItem);
  CurItem := MenuItemWin;
  RootMenu.Unselect;
  MenuItemWin.Selected := True;
  MenuItemWin.Changed;
 {$IFNDEF TESTAPP}
  Designer.Modified;
 {$ENDIF TESTAPP}
  UpdateSelection;
end;

procedure TMenuBuilder.DeleteItemClick(Sender: TObject);

  procedure DeleteSelected(MenuWin: TMenuWin);
  var
    i: Integer;
    MenuItemWin: TMenuItemWin;
  begin
    i := 0;
    while i <= MenuWin.ItemCount - 1 do
    begin
      MenuItemWin := MenuWin.Items[i];
      if MenuItemWin.Selected and Assigned(MenuItemWin.FMenuItem.Parent) then
      begin
        MenuItemWin.FMenuItem.Free;
        MenuItemWin.Free;
       {$IFNDEF TESTAPP}
        Designer.Modified;
       {$ENDIF TESTAPP}
      end
      else
      begin
        if Assigned(MenuItemWin.FSubMenu) then
          DeleteSelected(MenuItemWin.FSubMenu);
        inc(i);
      end;
    end;
    if MenuWin.ItemCount = 0 then MenuWin.Visible := False;
  end;    {  }

begin
  DeleteSelected(RootMenu);
end;

procedure TMenuBuilder.CreateSubmenu1Click(Sender: TObject);
var
  StubItem: TMenuItem;
begin
  if not Assigned(CurItem) or (CurItem.FMenuItem.Count > 0) or
     not Assigned(CurItem.FMenuItem.Parent) then Exit;
  StubItem := TMenuItem.Create(FOwner);
  CurItem.FMenuItem.Add(StubItem);
  CurItem.Changed;
  CurItem.ShowSubMenu;
end;

procedure TMenuBuilder.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMenuBuilder.UpdateSelection;
begin
 {$IFNDEF TESTAPP}
  if Assigned(CurItem) then
    Designer.SelectComponent(CurItem.FMenuItem) else
    Designer.SelectComponent(Menu);
 {$ENDIF TESTAPP}
end;    { UpdateSelection }

procedure TMenuBuilder.WMUpdateaSelection(var Message: TMessage);
begin
  UpdateSelection;
end;

procedure TMenuBuilder.FormModified;

  procedure InvalidateSelected(MenuWin: TMenuWin);
  var
    i: Integer;
    MenuItemWin: TMenuItemWin;
  begin
    MenuWin.Realign;
    for i := 0 to MenuWin.ItemCount - 1 do
    begin
      MenuItemWin := MenuWin.Items[i];
      if MenuItemWin.Selected then
        MenuItemWin.Invalidate;
      if Assigned(MenuItemWin.FSubMenu) then
        InvalidateSelected(MenuItemWin.FSubMenu);
    end;    { for }
  end;    {  }

begin
  InvalidateSelected(RootMenu);
end;

procedure TMenuBuilder.FormClosed(AForm: TCustomForm);
begin
  if AForm = Designer.Form then
    Close;
end;

procedure TMenuBuilder.SetCaption1Click(Sender: TObject);
const
  Num: Integer = 1;
begin
 { DEBUG!!! }
  if Assigned(CurItem) then
    CurItem.FMenuItem.Caption := 'Caption' + IntToStr(Num);
  inc(Num);  
end;

procedure TMenuBuilder.FormShow(Sender: TObject);
begin
  RegAuto1.Load;
end;

procedure TMenuBuilder.FormHide(Sender: TObject);
begin
  RegAuto1.Save;
end;

{$IFNDEF PROPEDIT}
{
procedure TMenuBuilder.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_INACTIVE then
    RootMenu.HideMenu;
end;
}
{$ENDIF PROPEDIT}

end.
