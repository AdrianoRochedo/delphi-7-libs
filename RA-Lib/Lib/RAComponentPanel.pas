{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : TRAComponentPanel
       description : Component panel for GUI developers

       authors     : Roman Tkachev, Andrei Prygounkov
       e-mail      : whiteman@infa.ru, black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAComponentPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, RAButtons;

type
  TButtonClick = procedure (Sender : TObject; Button : integer) of object;

  TRAComponentPanel = class(TCustomPanel)
  private
    FButtonW       : integer;
    FButtonH       : integer;
    FButtons       : TList;
    FOnClick       : TButtonClick;
    FOnDblClick    : TButtonClick;
    FButtonPointer : TSpeedButton;
  {  FButtonLeft    : TSpeedButton;
    FButtonRight   : TSpeedButton; }
    FButtonLeft    : TRANoFrameButton;
    FButtonRight   : TRANoFrameButton;
    FFirstVisible  : integer;
    FLockUpdate : integer;
    {***** Для property начало *****}
    function GetButton(Index : integer) : TSpeedButton;
    function GetButtonCount : integer;
    procedure SetButtonCount(AButtonCount : integer);
    procedure SetButtonW(AButtonW : integer);
    procedure SetButtonH(AButtonH : integer);
    procedure SetFirstVisible(AButton : integer);
    {##### Для property конец #####}
  protected
    FSelectButton : TSpeedButton;
    procedure OnMoveClick(Sender : TObject);
    procedure OnBClick(Sender : TObject);
    procedure OnBDblClick(Sender : TObject);
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure  RecreateButtons;
    procedure  SetMainButton;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Buttons[Index : integer] : TSpeedButton read GetButton; default;
    property FirstVisible : integer      read FFirstVisible write SetFirstVisible;
  published
    property Align;
    property OnClick      : TButtonClick read FOnClick        write FOnClick;
    property OnDblClick   : TButtonClick read FOnDblClick     write FOnDblClick;
    property ButtonWidth  : integer      read FButtonW        write SetButtonW;
    property ButtonHeight : integer      read FButtonH        write SetButtonH;
    property ButtonCount  : integer      read GetButtonCount write SetButtonCount;
   {$IFDEF RA_D4H}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
   {$ENDIF RA_D4H}
  end;

implementation

{$R RAComponentPanel.res}

{************************ TRAComponentPanel Начало ************************}
constructor TRAComponentPanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  FButtons := TList.Create;
  FFirstVisible := 0;
  FButtonW := 28;
  FButtonH := 28;
 { FButtonLeft    := TSpeedButton.Create(Self);
  FButtonRight   := TSpeedButton.Create(Self); }
  FButtonLeft    := TRANoFrameButton.Create(Self);
  FButtonRight   := TRANoFrameButton.Create(Self);
  FButtonPointer := TSpeedButton.Create(Self);
  with FButtonLeft do begin
   // Flat   := true;
    Parent := Self;
    Tag    := 0;
    Width  := 12;
    Top    := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPLEFT');
    NumGlyphs := 2;
   // Layout  := blGlyphRight;
    OnClick := OnMoveClick;
  end;
  with FButtonRight do begin
  //  Flat   := true;
    Parent := Self;
    Tag    := 1;
    Width  := 12;
    Top    := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPRIGHT');
    NumGlyphs := 2;
   // Layout := blGlyphLeft;
    OnClick := OnMoveClick;
  end;
  with FButtonPointer do begin
   {$IFDEF RA_D3H}
    Flat   := true;
   {$ENDIF RA_D3H}
    Parent := Self;
    Top    := 0;
    Glyph.LoadFromResourceName(HInstance, 'RACPPOINTER');
    GroupIndex := 1;
    OnClick    := OnBClick;
  end;
  SetMainButton;
end;

destructor TRAComponentPanel.Destroy;
var
  i : integer;
begin
  for i := 0 to FButtons.Count -1 do TSpeedButton(FButtons[i]).Free;
  FButtons.Free;
  FButtonPointer.Free;
  FButtonRight.Free;
  FButtonLeft.Free;
  inherited Destroy;
end;

procedure TRAComponentPanel.Invalidate;
begin
  if FLockUpdate = 0 then
    inherited Invalidate;
end;

procedure TRAComponentPanel.RecreateButtons;
var
  i      : integer;
  TmpNum : integer;
begin
  TmpNum := FButtons.Count;
  for i := 0 to FButtons.Count -1 do TSpeedButton(FButtons[i]).Free;
  FButtons.Clear;
  FFirstVisible := 0;
  SetButtonCount(TmpNum);
end;

procedure TRAComponentPanel.SetMainButton;
begin
  FButtonPointer.Down := true;
  FSelectButton := FButtonPointer;
end;

function TRAComponentPanel.GetButton(Index : integer) : TSpeedButton;
begin
  if (Index < 0) or (Index > FButtons.Count - 1) then
    Result := nil else
    Result := TSpeedButton(FButtons[Index]);
end;

function TRAComponentPanel.GetButtonCount : integer;
begin
  Result := FButtons.Count;
end;

procedure TRAComponentPanel.SetButtonCount(AButtonCount : integer);
var
  TmpButton : TSpeedButton;
begin
  if (AButtonCount < 0) or (AButtonCount > 100) then
    raise Exception.Create('Invalid ButtonCount');
  BeginUpdate;
  try
    SetMainButton;
    while FButtons.Count > AButtonCount do
    begin
      TSpeedButton(FButtons[FButtons.Count -1]).Free;
      FButtons.Delete(FButtons.Count -1);
    end;
    while FButtons.Count < AButtonCount do
    begin
      TmpButton := TSpeedButton.Create(Self);
      with TmpButton do begin
       {$IFDEF RA_D3H}
        Flat    := true;
       {$ENDIF RA_D3H}
        Parent  := Self;
        Top     := 0;
        GroupIndex := 1;
        OnClick    := OnBClick;
        OnDblClick := OnBDblClick;
      end;
      FButtons.Add(TmpButton);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRAComponentPanel.SetButtonW(AButtonW : integer);
begin
  if FButtonW <> AButtonW then begin
    FButtonW := AButtonW;
    Resize;
  end;
end;

procedure TRAComponentPanel.SetButtonH(AButtonH : integer);
begin
  if FButtonH <> AButtonH then begin
    FButtonH := AButtonH;
    Resize;
  end;
end;

procedure TRAComponentPanel.OnMoveClick(Sender : TObject);
begin
  case TSpeedButton(Sender).Tag of
    0 : dec(FFirstVisible);
    1 : inc(FFirstVisible);
  end;
  Resize;
end;

procedure TRAComponentPanel.OnBClick(Sender : TObject);
begin
  if FSelectButton <> Sender then begin
    FSelectButton := TSpeedButton(Sender);
    if Assigned(FOnClick) then FOnClick(Sender, FButtons.IndexOf(FSelectButton));
  end;
end;

procedure TRAComponentPanel.OnBDblClick(Sender : TObject);
begin
  if Assigned(FOnDblClick) then FOnDblClick(Sender, FButtons.IndexOf(Sender));
end;

procedure TRAComponentPanel.WMSetText(var Message: TWMSetText);
begin
  inherited;
  Caption := '';
end;

procedure TRAComponentPanel.WMSize(var Message: TWMSize);
begin
  inherited;
  Resize;
end;

procedure TRAComponentPanel.Resize;
var
  VisibleCount : integer;
  i : integer;
begin
  Height := FButtonH;
  FButtonPointer.Height := FButtonH;
  FButtonPointer.Width  := FButtonW;
  FButtonLeft   .Height := FButtonH;
  FButtonRight  .Height := FButtonH;
  VisibleCount := (Width - (12+12+FButtonW)) div FButtonW;
  FButtonPointer.Left := 0;
  FButtonLeft   .Left := FButtonW + 6;
  FButtonRight  .Left := (FButtonW + 12 + 6) + VisibleCount * FButtonW;
  if FFirstVisible = 0 then
    FButtonLeft.Enabled := false
  else
    FButtonLeft.Enabled := true;
  if FButtons.Count > FFirstVisible + VisibleCount then
    FButtonRight.Enabled := true
  else
    FButtonRight.Enabled := false;
  for i := 0 to FButtons.Count -1 do begin
    TSpeedButton(FButtons[i]).Width  := FButtonW;
    TSpeedButton(FButtons[i]).Height := FButtonH;
    if (i >= FFirstVisible) and (i < FFirstVisible + VisibleCount) then
      TSpeedButton(FButtons[i]).Left := (FButtonW + 12 + 6) + (i - FFirstVisible) * FButtonW
    else
      TSpeedButton(FButtons[i]).Left := -100;
  end;    
end;

procedure TRAComponentPanel.SetFirstVisible(AButton : integer);
begin
  if FFirstVisible <> AButton then
  begin
    FFirstVisible := AButton;
    Resize;
  end;
end;

procedure TRAComponentPanel.BeginUpdate;
begin
  inc(FLockUpdate);
  DisableAlign;
end;    { BeginUpdate }

procedure TRAComponentPanel.EndUpdate;
begin
  dec(FLockUpdate);
  if FLockUpdate = 0 then
  begin
    Resize;
    EnableAlign;
  end;
end;    { EndUpdate }

{######################## TRAComponentPanel Конец ########################}

end.
