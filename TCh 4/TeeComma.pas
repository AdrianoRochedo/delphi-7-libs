{**********************************************}
{   TTeeCommander Component                    }
{   Copyright (c) 1998 by David Berneda        }
{**********************************************}
{$I teedefs.inc}
unit TeeComma;

interface

uses Classes, Controls, StdCtrls, Buttons, TeeProcs, ExtCtrls,
     Forms, Graphics, Series, TeeEdit, Chart
     {$IFDEF D1}
     ,Menus, WinProcs
     {$ENDIF}
     ;

type
  TCustomTeeCommander=class(TCustomPanel)
  private
    FPanel         : TCustomTeePanel;
    FPanelMouseDown: TMouseEvent;
    FPanelMouseMove: TMouseMoveEvent;
    FPanelMouseUp  : TMouseEvent;
  protected
    Procedure CheckPanel;
    Procedure DoMouseDown(X,Y:Integer); virtual;
    Procedure DoMouseMove(X,Y:Integer); virtual;
    Procedure DoMouseUp; virtual;
    Function DoPanelMouse:Boolean; virtual;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
    procedure SetPanel(Const Value: TCustomTeePanel); virtual;
    Function TeePanelClass:String;
    Procedure ShowHideControls(Value:Boolean); virtual;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    Procedure CreateBevel(APos:Integer);
    Function CreateBitButton( ALeft:Integer;
                              AProc:TNotifyEvent; AHint:String):TBitBtn;
    Function CreateButton( ALeft:Integer; AProc:TNotifyEvent;
                           AHint:String):TSpeedButton;
    Function CreateLabel(APos:Integer; AColor:TColor):TLabel;

    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
                          Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
  published
    { Published declarations }
    property OnChartMouseDown:TMouseEvent read FPanelMouseDown write FPanelMouseDown;
    property OnChartMouseMove:TMouseMoveEvent read FPanelMouseMove write FPanelMouseMove;
    property OnChartMouseUp:TMouseEvent read FPanelMouseUp write FPanelMouseUp;
    property Panel:TCustomTeePanel read FPanel write SetPanel;

    { TPanel properties }
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF D5}
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragKind;
    {$ENDIF}

    { TPanel events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF D3}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF D5}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}
  end;

type
  {$IFDEF TEE5}
  TTeeCommander=class;

  TTeeEditedChartEvent=procedure(Sender:TTeeCommander; AChart:TCustomChart) of object;
  {$ENDIF}

  TTeeCommander=class(TCustomTeeCommander)
  private
    { Private declarations }
    FButtonCopy    : TBitBtn;
    FButtonDepth   : TSpeedButton;
    FButtonEdit    : TBitBtn;
    FButtonMove    : TSpeedButton;
    FButtonNormal  : TSpeedButton;
    FButtonPrint   : TBitBtn;
    FButtonRotate  : TSpeedButton;
    FButtonZoom    : TSpeedButton;
    FEditor        : TCustomChartEditor;
    FLabel         : TLabel;
    FLabelValues   : Boolean;

    FDragging      : Boolean;
    FDraggingIndex : Integer;
    FOldX          : Integer;
    FOldY          : Integer;

    {$IFDEF TEE5}
    FOnEditedChart : TTeeEditedChartEvent;
    {$ENDIF}
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonDepthClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonMoveClick(Sender: TObject);
    procedure ButtonNormalClick(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ButtonRotateClick(Sender: TObject);
    procedure ButtonZoomClick(Sender: TObject);
    procedure SetLabelCaption(Const ACaption:String);
    procedure SetLabelValues(Value:Boolean);
    Function FirstSeriesPie:TPieSeries;
  protected
    { Protected declarations }
    Procedure DoMouseDown(X,Y:Integer); override;
    Procedure DoMouseMove(X,Y:Integer); override;
    Procedure DoMouseUp; override;
    Function DoPanelMouse:Boolean; override;
    Procedure ShowHideControls(Value:Boolean); override;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;

    property ButtonCopy    : TBitBtn read FButtonCopy;
    property ButtonDepth   : TSpeedButton read FButtonDepth;
    property ButtonEdit    : TBitBtn read FButtonEdit;
    property ButtonMove    : TSpeedButton read FButtonMove;
    property ButtonNormal  : TSpeedButton read FButtonNormal;
    property ButtonPrint   : TBitBtn read FButtonPrint;
    property ButtonRotate  : TSpeedButton read FButtonRotate;
    property ButtonZoom    : TSpeedButton read FButtonZoom;
    property ChartEditor   : TCustomChartEditor read FEditor write FEditor;
    property LabelText     : TLabel read FLabel;
    Procedure ShowValues;
  published
    property LabelValues:Boolean read FLabelValues write SetLabelValues default True;
    {$IFDEF TEE5}
    property OnEditedChart:TTeeEditedChartEvent read FOnEditedChart
                                                write FOnEditedChart;
    {$ENDIF}
  end;

  TTeeCommanderChain=Procedure(Sender:TCustomTeeCommander);

Const TeeCommanderChain:TTeeCommanderChain=nil;

implementation

{$IFDEF D1}
{$R TEECOMMA.R16}
{$ELSE}
{$R TEECOMMA.RES}
{$ENDIF}

Uses Teengine,TeCanvas,SysUtils,EditChar,TeePrevi,TeeProCo;

{ TCustomTeeCommander }
Constructor TCustomTeeCommander.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  SetPanel(nil);
  Height:=33;
  Width:=400;
  if (csDesigning in ComponentState) and
     (not (csLoading in Owner.ComponentState)) then Align:=alTop;
end;

procedure TCustomTeeCommander.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
     if Assigned(FPanel) and (AComponent=FPanel) then
        Panel:=nil;
end;

procedure TCustomTeeCommander.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(X,Y);
  if Assigned(FPanelMouseDown) and DoPanelMouse then
     FPanelMouseDown(Sender,Button,Shift,X,Y);
end;

procedure TCustomTeeCommander.PanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp;
  if Assigned(FPanelMouseUp)  and DoPanelMouse then
     FPanelMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TCustomTeeCommander.PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DoMouseMove(X,Y);
  if Assigned(FPanelMouseMove)  and DoPanelMouse then
     FPanelMouseMove(Sender,Shift,X,Y);
end;

Procedure TCustomTeeCommander.DoMouseDown(X,Y:Integer);
begin
end;

Procedure TCustomTeeCommander.DoMouseMove(X,Y:Integer);
begin
end;

Procedure TCustomTeeCommander.DoMouseUp;
begin
end;

Procedure TCustomTeeCommander.ShowHideControls(Value:Boolean);
Var t:Integer;
begin
  if not( csDestroying in ComponentState) then
     for t:=0 to ControlCount-1 do
         if Controls[t].Owner=Self then Controls[t].Enabled:=Value;
end;

Procedure TCustomTeeCommander.CheckPanel;
begin
  if Assigned(FPanel) then
  begin
    if FPanel is TDraw3D then
    With TDraw3D(FPanel) do
    begin
      OnMouseDown :=PanelMouseDown;
      OnMouseMove :=PanelMouseMove;
      OnMouseUp   :=PanelMouseUp;
    end
    else
    if FPanel is TCustomChart then
    With TCustomChart(FPanel) do
    begin
      OnMouseDown :=PanelMouseDown;
      OnMouseMove :=PanelMouseMove;
      OnMouseUp   :=PanelMouseUp;
    end
    else
    if FPanel is TCustomTeePanel then
    With TCustomTeePanel(FPanel) do
    begin
      TCustomTeePanel(FPanel).OnMouseDown :=PanelMouseDown;
      OnMouseMove :=PanelMouseMove;
      OnMouseUp   :=PanelMouseUp;
    end;
  end;
  ShowHideControls(Assigned(FPanel));
end;

procedure TCustomTeeCommander.Loaded;
begin
  inherited Loaded;
  CheckPanel;
  if Assigned(TeeCommanderChain) then TeeCommanderChain(Self);
end;

procedure TCustomTeeCommander.SetPanel(const Value: TCustomTeePanel);
begin
  FPanel := Value;
  CheckPanel;
end;

Function TCustomTeeCommander.CreateButton( ALeft:Integer;
                                           AProc:TNotifyEvent;
                                           AHint:String):TSpeedButton;
{$IFDEF D1}
Var tmpSt:Array[0..255] of Char;
{$ENDIF}
begin
  result:=TSpeedButton.Create(Self);
  With result do
  begin
    OnClick:=AProc;
    Left := ALeft;
    Top := 4;
    Width := 25;
    Height := 25;
    Hint := AHint;
    Down := True;
    {$IFDEF D3}
    Flat := True;
    {$ENDIF}
    ParentShowHint := False;
    ShowHint := True;
    {$IFDEF D1}
    Glyph.Handle:=LoadBitmap(Hinstance,StrPCopy(tmpSt,'Tee'+AHint));
    {$ELSE}
    Glyph.LoadFromResourceName(HInstance,'Tee'+AHint);
    {$ENDIF}
    Parent:=Self;
    GroupIndex := 1;
  end;
end;

Function TCustomTeeCommander.DoPanelMouse:Boolean;
begin
  result:=True;
end;

Function TCustomTeeCommander.CreateBitButton(ALeft:Integer; AProc:TNotifyEvent; AHint:String):TBitBtn;
{$IFDEF D1}
Var tmpSt:Array[0..255] of Char;
{$ENDIF}
begin
  result:=TBitBtn.Create(Self);
  With result do
  begin
    OnClick:=AProc;
    Left := ALeft;
    TabStop:=False;
    Top := 3;
    Width := 27;
    Height := 27;
    Hint := AHint;
    Caption:='';
    ParentShowHint := False;
    ShowHint := True;
    {$IFDEF D1}
    Glyph.Handle:=LoadBitmap(Hinstance,StrPCopy(tmpSt,'Tee'+AHint));
    {$ELSE}
    Glyph.LoadFromResourceName(HInstance,'Tee'+AHint);
    {$ENDIF}
    Parent:=Self;
  end;
end;

Procedure TCustomTeeCommander.CreateBevel(APos:Integer);
begin
  With TBevel.Create(Self) do
  begin
    Shape:=bsLeftLine;
    Width:=2;
    Height:=Self.Height-2;
    Top:=1;
    Left:=APos;
    Parent:=Self;
  end;
end;

Function TCustomTeeCommander.CreateLabel(APos:Integer; AColor:TColor):TLabel;
begin
  result:=TLabel.Create(Self);
  With result do
  begin
    Left:=APos;
    Top:=12;
    Font.Name:=GetDefaultFontName;
    Font.Color:=AColor;
    Font.Size:=GetDefaultFontSize;
    Caption:='';
    Parent:=Self;
  end;
end;

Function TCustomTeeCommander.TeePanelClass:String;
begin
  if FPanel is TCustomAxisPanel then result:=TeeCommanMsg_Chart
                                else result:=TeeCommanMsg_Panel;
end;

Destructor TCustomTeeCommander.Destroy;
var Num : Integer;
    tmp : Integer;
    t   : Integer;
begin
  Repeat
    Num:=0;
    tmp:=-1;
    for t:=0 to ControlCount-1 do
    if Controls[t].Owner=Self then
    begin
      Inc(Num);
      tmp:=t;
    end;
    if tmp<>-1 then Controls[tmp].Free;
  Until Num=0;
  inherited Destroy;
end;

{ TTeeCommander }
Constructor TTeeCommander.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FEditor:=nil;
  FDragging:=False;
  FLabelValues:=True;

  FButtonNormal := CreateButton(  4,ButtonNormalClick,TeeCommanMsg_Normal);
  CreateBevel(32);
  FButtonRotate := CreateButton( 36,ButtonRotateClick,TeeCommanMsg_Rotate);
  FButtonMove   := CreateButton( 61,ButtonMoveClick,TeeCommanMsg_Move);
  FButtonZoom   := CreateButton( 86,ButtonZoomClick,TeeCommanMsg_Zoom);
  FButtonDepth  := CreateButton(111,ButtonDepthClick,TeeCommanMsg_Depth);
  CreateBevel(137);
  FButtonEdit   := CreateBitButton(142,ButtonEditClick,TeeCommanMsg_Edit);
  FButtonPrint  := CreateBitButton(172,ButtonPrintClick,TeeCommanMsg_Print);
  FButtonCopy   := CreateBitButton(202,ButtonCopyClick,TeeCommanMsg_Copy);
  FLabel        := CreateLabel(234,clNavy);

  FButtonNormal.Down:=True;
end;

Procedure TTeeCommander.DoMouseDown(X,Y:Integer);
var tmpSeries:TPieSeries;
begin
  tmpSeries:=FirstSeriesPie;
  if Assigned(tmpSeries) or (not FButtonNormal.Down) then
  begin
    FDragging:=True;
    FOldX:=X;
    FOldY:=Y;
    if Assigned(tmpSeries) then FDraggingIndex:=tmpSeries.Clicked(X,Y)
                           else FDraggingIndex:=-1;
    if FPanel is TCustomAxisPanel then
       TCustomAxisPanel(FPanel).CancelMouse:=True;
  end;
end;

Function TTeeCommander.DoPanelMouse:Boolean;
begin
  result:=FButtonNormal.Down;
end;

Procedure TTeeCommander.DoMouseUp;
begin
  FDragging:=False;
  FDraggingIndex:=-1;
end;

Function TTeeCommander.FirstSeriesPie:TPieSeries;
var t:Integer;
begin
  result:=nil;
  if FPanel is TCustomChart then
  With TCustomChart(FPanel) do
  begin
    for t:=0 to SeriesCount-1 do
    if (Series[t] is TPieSeries) and (Series[t].Active) then
    begin
      result:=Series[t] as TPieSeries;
      exit;
    end;
  end;
end;

Procedure TTeeCommander.DoMouseMove(X,Y:Integer);

    Procedure DoRotate;

      Function CorrectAngle(Const AAngle:Integer):Integer;
      begin
        result:=AAngle;
        if result>360 then result:=result-360 else
        if result<0 then result:=360+result;
      end;

    Var tmpMinAngle : Integer;
        tmp         : Integer;
        tmpSeries   : TPieSeries;
    begin
      With FPanel,View3DOptions do
      begin
        View3D:=True;
        Orthogonal:=False;
        tmp:=Round(90.0*(X-FOldX)/Width);
        tmpSeries:=FirstSeriesPie;
        if Canvas.SupportsFullRotation then Rotation:=CorrectAngle(Rotation+tmp)
        else
        if Assigned(tmpSeries) then
        begin
          Rotation:=360;
          if tmp<>0 then
          With tmpSeries do RotationAngle:=CorrectAngle(RotationAngle+tmp);
        end
        else
          if tmp>0 then Rotation:=MinLong(360,Rotation+tmp)
                   else
                   begin
                     if Canvas.SupportsFullRotation then
                        tmpMinAngle:=0
                     else
                        tmpMinAngle:=270;
                     Rotation:=MaxLong(tmpMinAngle,Rotation+tmp);
                   end;
        tmp:=Round(90.0*(FOldY-Y)/Height);
        if Assigned(tmpSeries) then tmp:=-tmp;
        if Canvas.SupportsFullRotation then Elevation:=CorrectAngle(Elevation+tmp)
        else
        if tmp>0 then Elevation:=MinLong(360,Elevation+tmp)
                 else
                 begin
                   if Canvas.SupportsFullRotation then
                      tmpMinAngle:=0
                   else
                      tmpMinAngle:=270;
                   Elevation:=MaxLong(tmpMinAngle,Elevation+tmp);
                 end;
        FOldX:=X;
        FOldY:=Y;
      end;
    end;

    Procedure DoMove;
    begin
      FPanel.View3D:=True;
      With FPanel.View3DOptions do
      begin
        HorizOffset:=HorizOffset+(X-FOldX);
        VertOffset:=VertOffset+(Y-FOldY);
      end;
      FOldX:=X;
      FOldY:=Y;
    end;

    Procedure DoZoom;
    var tmp : Integer;
    begin
      With FPanel,View3DOptions do
      begin
        View3D:=True;
        tmp:=Round(10.0*(FOldY-Y)/ChartHeight);
        Zoom:=Zoom+tmp;
        if Zoom<5 then Zoom:=5;
      end;
    end;

    Function CalcDistPercent(APercent,AWidth,AHeight:Integer):Integer;
    Var Dist      : Longint;
        ChartDiag : Longint;
    begin
      Dist:=Round(Sqrt(Sqr(1.0*FOldX-X)+Sqr(1.0*FOldY-Y)));
      ChartDiag:=Round(Sqrt(Sqr(1.0*AWidth)+Sqr(1.0*AHeight)));
      result:=Round(1.0*APercent*Dist/ChartDiag);
    end;

    Procedure DoDepth;
    var tmp : Longint;
    begin
      if FPanel is TCustomAxisPanel then
      With TCustomAxisPanel(FPanel) do
      begin
        View3D:=True;
        tmp:=CalcDistPercent(200,ChartWidth,ChartHeight);
        if (tmp>=1) then Chart3DPercent:=MinLong(100,tmp);
      end;
    end;

    Procedure DoNormal;
    Var tmpSeries : TPieSeries;
        tmp       : Integer;
    begin
      if FDraggingIndex<>-1 then
      begin
        tmpSeries:=FirstSeriesPie;
        if Assigned(tmpSeries) then
        With tmpSeries do
        begin
          tmp:=MinLong(100,CalcDistPercent(100,CircleWidth,CircleHeight));
          ExplodedSlice.Value[FDraggingIndex]:= tmp;
        end;
      end;
    end;

begin
  if FDragging then
  begin
    FDragging:=False;
    if FButtonRotate.Down then DoRotate else
    if FButtonMove.Down then DoMove else
    if FButtonZoom.Down then DoZoom else
    if FButtonDepth.Down then DoDepth else
    if FButtonNormal.Down then DoNormal;
    if FLabelValues then ShowValues;
    FDragging:=True;
  end;
end;

procedure TTeeCommander.SetLabelCaption(Const ACaption:String);
begin
  if FLabelValues then
     FLabel.Caption:=Format(ACaption,[TeePanelClass])
  else
     FLabel.Visible:=False;
end;

procedure TTeeCommander.ButtonRotateClick(Sender: TObject);
begin
  SetLabelCaption(TeeCommanMsg_RotateLabel);
end;

procedure TTeeCommander.ButtonMoveClick(Sender: TObject);
begin
  SetLabelCaption(TeeCommanMsg_MoveLabel);
end;

procedure TTeeCommander.ButtonZoomClick(Sender: TObject);
begin
  SetLabelCaption(TeeCommanMsg_ZoomLabel);
end;

procedure TTeeCommander.ButtonDepthClick(Sender: TObject);
begin
  SetLabelCaption(TeeCommanMsg_DepthLabel);
end;

procedure TTeeCommander.ButtonEditClick(Sender: TObject);
begin
  if Assigned(FEditor) then FEditor.Execute
  else
  if Assigned(FPanel) and (FPanel is TCustomChart) then
     EditChart(nil,FPanel as TCustomChart);
  {$IFDEF TEE5}
  if Assigned(FOnEditedChart) then FOnEditedChart(Self,FPanel as TCustomChart);
  {$ENDIF}
end;

procedure TTeeCommander.ButtonPrintClick(Sender: TObject);
begin
  if Assigned(FPanel) then ChartPreview(nil,FPanel);
end;

procedure TTeeCommander.ButtonCopyClick(Sender: TObject);
begin
  if Assigned(FPanel) then FPanel.CopyToClipboardBitmap;
end;

procedure TTeeCommander.ButtonNormalClick(Sender: TObject);
begin
  if FirstSeriesPie=nil then SetLabelCaption(TeeCommanMsg_NormalLabel)
                        else SetLabelCaption(TeeCommanMsg_NormalPieLabel)
end;

Procedure TTeeCommander.ShowHideControls(Value:Boolean);
begin
  if not Assigned(FButtonRotate) then Exit;
  FButtonRotate.Enabled:=Value;
  FButtonMove.Enabled:=Value;
  FButtonZoom.Enabled:=Value;
  FButtonNormal.Enabled:=Value;
  FButtonCopy.Enabled:=Value;
  FButtonPrint.Enabled:=Value;
  Value:=Value and (FPanel is TCustomAxisPanel);
  FButtonDepth.Enabled:=Value;
  FButtonEdit.Enabled:=Value;
end;

procedure TTeeCommander.SetLabelValues(Value:Boolean);
begin
  if FLabelValues<>Value then
  begin
    FLabelValues:=Value;
    FLabel.Visible:=FLabelValues;
  end;
end;

Procedure TTeeCommander.ShowValues;
var tmpSeries:TPieSeries;
begin
  With FPanel.View3DOptions do
  if FButtonRotate.Down then
     FLabel.Caption:=Format(TeeCommanMsg_Rotating,[Rotation,Elevation])
  else
  if FButtonMove.Down then
     FLabel.Caption:=Format(TeeCommanMsg_Moving,[HorizOffset,VertOffset])
  else
  if FButtonZoom.Down then
     FLabel.Caption:=Format(TeeCommanMsg_Zooming,[Zoom])
  else
  if FButtonDepth.Down then
  begin
    if FPanel is TCustomAxisPanel then
    With TCustomAxisPanel(FPanel) do
      FLabel.Caption:=Format(TeeCommanMsg_Depthing,[Chart3DPercent])
  end
  else
  if FButtonNormal.Down and (FDraggingIndex<>-1) then
  begin
    tmpSeries:=FirstSeriesPie;
    if tmpSeries=nil then
       FLabel.Caption:=''
    else
       FLabel.Caption:=Format( TeeCommanMsg_PieExploding,
                               [FDraggingIndex,tmpSeries.ExplodedSlice.Value[FDraggingIndex]]);
  end
  else FLabel.Caption:='';
  FLabel.Update;
end;

end.
