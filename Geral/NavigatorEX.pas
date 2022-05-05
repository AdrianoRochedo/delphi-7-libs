unit NavigatorEX;

interface
uses SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms,
  Graphics, Menus, StdCtrls, ExtCtrls, DB, DBTables, Mask, Buttons;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
  SpaceSize       =  5;   { size of space between special buttons }

type
  TNavButton = class;
  TNavDataLink = class;

  TNavGlyph = (ngEnabled, ngDisabled);
  TNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
                  nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh);
  TButtonSet = set of TNavigateBtn;
  TNavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  ENavClick = procedure (Sender: TObject; Button: TNavigateBtn) of object;

{ TDBNavigatorEX }

  TDBNavigatorEX = class (TCustomPanel)
  private
    FDataLink: TNavDataLink;
    FVisibleButtons: TButtonSet;
    FEnabledButtons: TButtonSet;
    FHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FocusedButton: TNavigateBtn;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure InitButtons;
    procedure InitHints;
    procedure Click(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetVisible(Value: TButtonSet);
    procedure SetEnabled(Value: TButtonSet);
    procedure AdjustSize (var W: Integer; var H: Integer);
    procedure SetHints(Value: TStrings);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    Buttons: array[TNavigateBtn] of TNavButton;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TNavigateBtn);
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property EnabledButtons: TButtonSet read FEnabledButtons write SetEnabled
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property Hints: TStrings read FHints write SetHints;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
  end;

{ TNavButton }

  TNavButton = class(TSpeedButton)
  private
    FIndex: TNavigateBtn;
    FNavStyle: TNavButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property NavStyle: TNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TNavigateBtn read FIndex write FIndex;
  end;

{ TNavDataLink }

  TNavDataLink = class(TDataLink)
  private
    FNavigator: TDBNavigatorEX;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDBNavigatorEX);
    destructor Destroy; override;
  end;

  procedure Register;

Const
  cNavigationBtn : TButtonSet = [nbFirst, nbPrior, nbNext, nbLast];
  cCTRLBtn : TButtonSet = [nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];

implementation
uses DBIErrs, DBITypes, Clipbrd, VDBConsts, Dialogs;

procedure Register;
begin
  RegisterComponents('DB2', [TDBNavigatorEX]);
end;

{ TDBNavigatorEX }

const
  BtnStateName: array[TNavGlyph] of PChar = ('EN', 'DI');
  BtnTypeName: array[TNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT',
    'LAST', 'INSERT', 'DELETE', 'EDIT', 'POST', 'CANCEL', 'REFRESH');
{$IFDEF WIN32}
  BtnHintId: array[TNavigateBtn] of STRING = (SFirstRecord, SPriorRecord,
    SNextRecord, SLastRecord, SInsertRecord, SDeleteRecord, SEditRecord,
    SPostEdit, SCancelEdit, SRefreshRecord);
{$ELSE}
  BtnHintId: array[TNavigateBtn] of Word = (SFirstRecord, SPriorRecord,
    SNextRecord, SLastRecord, SInsertRecord, SDeleteRecord, SEditRecord,
    SPostEdit, SCancelEdit, SRefreshRecord);
{$ENDIF}
constructor TDBNavigatorEX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  FDataLink := TNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FEnabledButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  InitButtons;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
end;

destructor TDBNavigatorEX.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBNavigatorEX.InitButtons;
var
  I: TNavigateBtn;
  Btn: TNavButton;
  X: Integer;
  ResName: array[0..40] of Char;
begin
  MinBtnSize := Point(20, 18);
  X := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TNavButton.Create (Self);
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := I in FEnabledButtons;
    Btn.SetBounds (X, 0, MinBtnSize.X, MinBtnSize.Y);
    Btn.Glyph.Handle := LoadBitmap(HInstance,
        StrFmt(ResName, 'dbn_%s', [BtnTypeName[I]]));
    Btn.NumGlyphs := 2;
    Btn.OnClick := Click;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  InitHints;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TDBNavigatorEX.InitHints;
var
  I: Integer;
  J: TNavigateBtn;
begin
  for J := Low(Buttons) to High(Buttons) do
{$IFDEF WIN32}
    Buttons[J].Hint := BtnHintId[J];
{$ELSE}
    Buttons[J].Hint := LoadStr (BtnHintId[J]);
{$ENDIF}

  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then Buttons[J].Hint := FHints.Strings[I];
    if J = High(Buttons) then Exit;
    Inc(J);
  end;
end;

procedure TDBNavigatorEX.SetHints(Value: TStrings);
begin
  FHints.Assign(Value);
  InitHints;
end;

procedure TDBNavigatorEX.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBNavigatorEX.SetVisible(Value: TButtonSet);
var
  I: TNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Visible := I in FVisibleButtons;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  Invalidate;
end;

procedure TDBNavigatorEX.SetEnabled(Value: TButtonSet);
var
  I: TNavigateBtn;
begin
  FEnabledButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Enabled := I in FEnabledButtons;
  Invalidate;
end;

procedure TDBNavigatorEX.AdjustSize (var W: Integer; var H: Integer);
var
  Count: Integer;
  MinW: Integer;
  I: TNavigateBtn;
  LastBtn: TNavigateBtn;
  Space, Temp, Remain: Integer;
  X: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  Count := 0;
  LastBtn := High(Buttons);
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Inc(Count);
      LastBtn := I;
    end;
  end;
  if Count = 0 then Inc(Count);

  MinW := Count * (MinBtnSize.X - 1) + 1;
  if W < MinW then
    W := MinW;
  if H < MinBtnSize.Y then
    H := MinBtnSize.Y;

  ButtonWidth := ((W - 1) div Count) + 1;
  Temp := Count * (ButtonWidth - 1) + 1;
  if Align = alNone then
    W := Temp;

  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec (Temp, Remain);
        if Temp < 0 then
        begin
          Inc (Temp, Count);
          Space := 1;
        end;
      end;
      Buttons[I].SetBounds (X, 0, ButtonWidth + Space, Height);
      Inc (X, ButtonWidth - 1 + Space);
      LastBtn := I;
    end
    else
      Buttons[I].SetBounds (Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TDBNavigatorEX.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TDBNavigatorEX.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TDBNavigatorEX.Click(Sender: TObject);
begin
  BtnClick (TNavButton (Sender).Index);
end;

procedure TDBNavigatorEX.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TNavigateBtn;
  Form: TForm;
begin
  OldFocus := FocusedButton;
  FocusedButton := TNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
end;

procedure TDBNavigatorEX.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    with DataSource.DataSet do
    begin
      case Index of
        nbPrior: Prior;
        nbNext: Next;
        nbFirst: First;
        nbLast: Last;
        nbInsert: Insert;
        nbEdit: Edit;
        nbCancel: Cancel;

        nbPost:
          Begin
          if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
             FOnNavClick(Self, Index);
          Post;
          End;

        nbRefresh: Refresh;
        nbDelete:
          begin
            if not FConfirmDelete or
                (MessageDlg(
                    {$IFDEF WIN32}
                    SDeleteRecordQuestion,
                    {$ELSE}
                    LoadStr(SDeleteRecordQuestion),
                    {$ENDIF}
                    mtConfirmation, mbOKCancel, 0) <> idCancel) then
              Delete;
          end;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) and (index <> nbPost) then
    FOnNavClick(Self, Index);
end;

procedure TDBNavigatorEX.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavigatorEX.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavigatorEX.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TNavigateBtn;
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus < High(Buttons) then
            NewFocus := Succ(NewFocus);
        until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
          Buttons[FocusedButton].Click;
      end;
  end;
end;

procedure TDBNavigatorEX.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDBNavigatorEX.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and 
    FDataLink.DataSet.CanModify and 
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TDBNavigatorEX.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled := not (FDataLink.DataSet is TQuery);
end;

procedure TDBNavigatorEX.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDBNavigatorEX.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TDBNavigatorEX.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

function TDBNavigatorEX.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBNavigatorEX.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;

{TNavButton}

destructor TNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TNavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
     (FIndex = TDBNavigatorEX (Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{ TNavDataLink }

constructor TNavDataLink.Create(ANav: TDBNavigatorEX);
begin
  inherited Create;
  FNavigator := ANav;
end;

destructor TNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

end.
 
