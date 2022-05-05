{**********************************************}
{   TeeChart Gallery Dialog                    }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeeGally;

interface

uses
  WinProcs,WinTypes, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, TeEngine, Chart, TeeFunci
  {$IFDEF D1}
  ,Tabs
  {$ELSE}
  ,ComCtrls
  {$ENDIF}
  ;

Const clTeeGallery1=clRed;
      clTeeGallery2=clBlue;

type
  {$IFDEF D1}
  TeeTabControlClass=TTabSet;
  {$ELSE}
  TeeTabControlClass=TTabControl;
  {$ENDIF}

  TTeeTabControl=class( TeeTabControlClass )
  public
    Procedure AlignTab;
    Procedure SetTabEvent(Value:TNotifyEvent);
  end;

  TTeeGalleryPanel=class(TPanel)
  private
    FView3D:Boolean;
    FOnSelectedChart,
    FOnChangeChart:TNotifyEvent;
    Procedure CheckShowLabels(AChart:TCustomChart);
    Function GetChart(Index:Longint):TCustomChart;
    Procedure SetView3D(Value:Boolean);
    Procedure SetMargins(AChart:TCustomChart);
  protected
    procedure Resize; override;
  public
    SelectedChart  : TCustomChart;
    SelectedSeries : TChartSeries;
    RowHeight      : Longint;
    ColWidth       : Longint;
    NumRows        : Longint;
    NumCols        : Longint;
    ChartList      : TList;
    CheckSeries    : Boolean;
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
    Procedure ResizeCharts;
    Procedure CalcChartWidthHeight;
    Procedure SetRowCols(ARows,ACols:Longint);
    Procedure ResizeChart(AChart:TCustomChart);
    Procedure GetChartXY(AChart:TCustomChart; Var x,y:Longint);
    Procedure RemoveCharts;
    Procedure CreateChart(Const tmpType:TTeeSeriesType; Index:Longint);
    procedure ChartOnClick(Sender: TObject);
    procedure ChartOnDblClick(Sender: TObject);
    Procedure ShowSelectedChart;
    procedure FindSelectedChart;
    property View3D:Boolean read FView3D write SetView3D;
    Procedure ProcessKeyDown(Sender:TObject; Var Key:Word; Shift:TShiftState);
    Procedure CreateChartList(Const ASeriesList:Array of TChartSeriesClass);
    property Chart[Index:Longint]:TCustomChart read GetChart;
  published
    property OnSelectedChart:TNotifyEvent read FOnSelectedChart write FOnSelectedChart;
    property OnChangeChart:TNotifyEvent read FOnChangeChart write FOnChangeChart;
  end;

  TTeeGallery = class(TForm)
    P1: TPanel;
    BOk: TButton;
    BCancel: TButton;
    CB3D: TCheckBox;
    procedure CB3DClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    TabControl:TTeeTabControl;
  public
    { Public declarations }
    GalleryPanel:TTeeGalleryPanel;
    CreatingForm,
    FunctionsVisible:Boolean;
    OnChangeChart:TNotifyEvent;
    Procedure CreateGallery(APage:Integer);
    Procedure HideButtons;
    Procedure HideFunctions;
    Function CalcNumSeriesPage(Const APage:String):Longint;
    procedure TabControl1Change(Sender: TObject);
    procedure GalleryPanelOnSelected(Sender: TObject);
    Function ValidSeries(Const ASeriesType:TTeeSeriesType; Const APage:String):Boolean;
    Procedure CreateGalleryList(Const ASeriesList:Array of TChartSeriesClass);
  end;

{ Shows the gallery and asks the user a Series type.
  If user double clicks a chart or presses Ok, a new Series
  is created and returned. The new Series is owned by AOwner
  parameter (usually the Form).
}
Function CreateNewSeriesGallery( AOwner:TComponent;
                                 OldSeries:TChartSeries;
                                 tmpChart:TCustomChart;
                                 AllowSameType,
                                 ShowFunctions:Boolean ):TChartSeries;

{ Shows the gallery and asks the user a Series type. Then
  changes tmpSeries to the new type. }
procedure ChangeSeriesTypeGallery(AOwner:TComponent; Var tmpSeries:TChartSeries);

{ Shows the Gallery Dialog and lets the user choose a Series type.
  Returns True if user pressed OK.
  The "tmpClass" parameter returns the choosen type.
}
Function GetChartGalleryClass( AOwner:TComponent;
                               OldSeries:TChartSeries;
                               ShowFunctions:Boolean;
                               Var tmpClass:TChartSeriesClass;
                               Var Show3D:Boolean;
                               Var tmpFunctionClass:TTeeFunctionClass
                              ):Boolean;

{ Returns the name of a Series in it´s "gallery" style:
   TLineSeries returns "Line"
   TPieSeries returns "Pie"
   etc.
}
Function GetGallerySeriesName(ASeries:TChartSeries):String;

{ Shows the gallery and asks the user a Series type. Then
  changes all Series in AChart to the new type. }
procedure ChangeAllSeriesGallery( AOwner:TComponent; AChart:TCustomChart );

implementation

{$R *.DFM}

Uses TeeProcs, TeeConst, Series, TeCanvas;

Const clTeeSelected=$0080FFFF;   { <-- color use to select charts }
      TeeDefault_GalleryCols=4;

Function FindSeriesClassType(ASeriesClass:TChartSeriesClass; Var AType:TTeeSeriesType):Boolean;
var t:Integer;
begin
  result:=False;
  With TeeSeriesTypes do
  for t:=0 to Count-1 do
  begin
    AType:=SeriesType[t];
    if (AType.SeriesClass=ASeriesClass) and (AType.FunctionClass=nil) then
    begin
      result:=True;
      Exit;
    end;
  end;
end;

{ TeeGalleryPanel }
Constructor TTeeGalleryPanel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SelectedChart:=nil;
  SelectedSeries:=nil;
  OnSelectedChart:=nil;
  OnChangeChart:=nil;
  NumCols:=0;
  NumRows:=0;
  ChartList:=TList.Create;
  SetRowCols(3,4);
end;

Destructor TTeeGalleryPanel.Destroy;
begin
  RemoveCharts;
  ChartList.Free;
  inherited Destroy;
end;

Procedure TTeeGalleryPanel.SetRowCols(ARows,ACols:Longint);
begin
  NumRows:=ARows;
  NumCols:=ACols;
  CalcChartWidthHeight;
end;

Procedure TTeeGalleryPanel.GetChartXY(AChart:TCustomChart; Var x,y:Longint);
begin
  y:=AChart.Tag div NumCols;
  x:=AChart.Tag mod NumCols;
end;

Procedure TTeeGalleryPanel.RemoveCharts;
var t   : Integer;
    tmp : TCustomChart;
begin
  for t:=0 to ChartList.Count-1 do
  begin
    tmp:=Chart[t];
    tmp.Free;
  end;
  ChartList.Clear;
end;

procedure TTeeGalleryPanel.ChartOnDblClick(Sender: TObject);
begin
  SelectedChart:=TCustomChart(Sender);
  if Assigned(FOnSelectedChart) then OnSelectedChart(Self);
end;

Procedure TTeeGalleryPanel.CalcChartWidthHeight;
var tmp:Integer;
begin
  if (NumRows>0) and (NumCols>0) then
  begin
    tmp:=BevelWidth+BorderWidth;
    RowHeight:=(Height-tmp) div NumRows;
    ColWidth:=(Width-tmp) div NumCols;
  end;
end;

Function GetDefaultGalleryFontSize:Integer;
begin
  result:=StrToInt(TeeMsg_DefaultGalleryFontSize);
  {$IFDEF D3}
  CheckJapaneseFontSize(result);
  {$ENDIF}
end;

Procedure TTeeGalleryPanel.CheckShowLabels(AChart:TCustomChart);
var Placement: TWindowPlacement;
begin
  With AChart do
  if AxisVisible then
  begin
    Placement.length := SizeOf(TWindowPlacement);
    GetWindowPlacement((Self.Owner as TForm).Handle, @Placement);
    LeftAxis.Labels:=Placement.showCmd=SW_SHOWMAXIMIZED;
    BottomAxis.Labels:=Placement.showCmd=SW_SHOWMAXIMIZED;
{      View3DOptions.ZoomText:=False; }
  end;
end;

Procedure TTeeGalleryPanel.CreateChart(Const tmpType:TTeeSeriesType; Index:Longint);
Var AChart   : TCustomChart;
    tmpClass : TChartSeriesClass;

  Procedure CreateSeries;
  Var tmp:Longint;
  begin
    for tmp:=1 to MaxLong(1,tmpType.NumGallerySeries) do
    With CreateNewSeries(Self,AChart,tmpClass,tmpType.FunctionClass) do
    begin
      case tmp of
        1: SeriesColor:=clTeeGallery1;
        2: SeriesColor:=clTeeGallery2;
      end;
    end;
  end;

var t : Longint;
    DisabledSeries:Boolean;
begin
  tmpClass:=tmpType.SeriesClass;
  if not Assigned(tmpClass) then tmpClass:=TLineSeries;
  AChart:=TCustomChart.Create(Self);
  ChartList.Add(AChart);
  With AChart do
  begin
    Tag:=Index;
    Name:=TeeMsg_GalleryChartName+IntToStr(Tag);
    ResizeChart(AChart);
    Parent:=Self;
    Legend.Visible:=False;

    LeftAxis.Labels:=False;
    BottomAxis.Labels:=False;

    Title.Text.Add(tmpType.Description);
    Title.Font.Size:=GetDefaultGalleryFontSize;
    Title.Font.Color:=clNavy;
    AnimatedZoom:=True;
    With View3DOptions do
    begin
      Orthogonal:=False;
      Zoom       :=70;
      Perspective:=60;
      Rotation   :=335;
      Elevation  :=345;
    end;
    Chart3DPercent:=100;
    ClipPoints:=False;

    Frame.Visible:=False;
    BevelWidth:=2;
    BevelOuter:=bvNone;
    LeftWall.Size:=4;
    BottomWall.Size:=4;
    LeftWall.Color:=clWhite;
    BottomWall.Color:=clWhite;
    CreateSeries;
    DisabledSeries:=CheckSeries and
                    Assigned(SelectedSeries) and
                    ( not Series[0].IsValidSourceOf(SelectedSeries) );
    if DisabledSeries then
    begin
      Cursor:=crNoDrop;
      OriginalCursor:=Cursor;
      OnClick:=nil;
      OnDblClick:=nil;
      Title.Font.Color:=clGray;
      LeftWall.Pen.Color:=clGray;
      BottomWall.Pen.Color:=clGray;
      LeftAxis.Axis.Width:=1;
      LeftAxis.Axis.Color:=clWhite;
      BottomAxis.Axis.Width:=1;
      BottomAxis.Axis.Color:=clWhite;
    end
    else
    begin
      Cursor:=crTeeHand;
      OriginalCursor:=Cursor;
      OnClick:=ChartOnClick;
      OnDblClick:=ChartOnDblClick;
      OnEnter:=ChartOnClick;
    end;
    Series[0].GalleryChanged3D(Self.FView3D);
    for t:=0 to SeriesCount-1 do
        Series[t].PrepareForGallery(not DisabledSeries);
    SetMargins(AChart);
    CheckShowLabels(AChart);
  end;
end;

Function TTeeGalleryPanel.GetChart(Index:Longint):TCustomChart;
begin
  result:=TCustomChart(ChartList[Index]);
end;

Procedure TTeeGalleryPanel.ProcessKeyDown(Sender:TObject; Var Key:Word; Shift:TShiftState);

    Function FindChartXY(x,y:Longint):TCustomChart;
    var tmpX : Longint;
        tmpY : Longint;
        t    : Longint;
    begin
      for t:=0 to ChartList.Count-1 do
      begin
        result:=Chart[t];
        GetChartXY(result,tmpX,tmpY);
        if (x=tmpx) and (y=tmpy) then exit;
      end;
      result:=nil;
    end;

var x,y : Longint;
    tmp : TCustomChart;
begin
  GetChartXY(SelectedChart,x,y);
  Case Key of
    VK_LEFT:   if x>0 then Dec(x);
    VK_RIGHT:  if x<NumCols then Inc(x);
    VK_UP:     if y>0 then Dec(y);
    VK_DOWN:   if y<NumRows then Inc(y);
    VK_RETURN: ChartOnDblClick(SelectedChart);
  end;
  tmp:=FindChartXY(x,y);
  if Assigned(tmp) and (tmp.Cursor=crTeeHand) then ChartOnClick(tmp);
end;

Procedure TTeeGalleryPanel.ResizeChart(AChart:TCustomChart);
var tmp       : Longint;
    tmpCol    : Longint;
    tmpRow    : Longint;
    tmpLeft   : Longint;
    tmpTop    : Longint;
    tmpWidth  : Longint;
    tmpHeight : Longint;
begin
  if (NumCols>0) and (NumRows>0) then
  begin
    GetChartXY(AChart,tmpCol,tmpRow);
    tmp:=BevelWidth+BorderWidth;
    tmpLeft:=tmp+(tmpCol*ColWidth);
    tmpTop:=tmp+(tmpRow*RowHeight);
    tmpWidth:=ColWidth;
    tmpHeight:=RowHeight;
    if tmpLeft+tmpWidth>(Width-tmp) then tmpWidth:=(Width-tmp)-tmpLeft;
    if tmpTop+RowHeight>(Height-tmp) then tmpHeight:=(Height-tmp)-tmpTop;
    AChart.SetBounds( tmpLeft,tmpTop,tmpWidth,tmpHeight );
    CheckShowLabels(AChart);
  end;
end;

Procedure TTeeGalleryPanel.ShowSelectedChart;
begin
  if not Assigned(SelectedChart) then
  if ChartList.Count>0 then SelectedChart:=Chart[0];
  if Assigned(SelectedChart) then
  With SelectedChart do
  begin
{    Color:=clTeeSelected; }
    With Gradient do
    begin
      Visible:=True;
      Direction:=gdFromTopLeft;
      StartColor:=clTeeSelected;
      EndColor:=clWhite;
    end;
    View3DOptions.Rotation:=345;
    if SeriesCount>0 then Series[0].GalleryChanged3D(Self.FView3D);
    With Title.Font do
    begin
      Style:=[fsBold];
      Color:=clBlack;
      Size:=10;
    end;
    BevelOuter:=bvRaised;
    SetFocus;
    if Assigned(FOnChangeChart) then OnChangeChart(Self);
  end;
end;

procedure TTeeGalleryPanel.FindSelectedChart;
var t:Longint;
begin
  SelectedChart:=nil;
  if Assigned(SelectedSeries) then
  begin
    for t:=0 to ChartList.Count-1 do
    With Chart[t][0] do
    if (ClassType=SelectedSeries.ClassType) and (FunctionType=nil) then
    begin
      SelectedChart:=Chart[t];
      break;
    end;
  end;
  ShowSelectedChart;
end;

procedure TTeeGalleryPanel.ChartOnClick(Sender: TObject);
var t:Longint;
begin
  SelectedChart:=TCustomChart(Sender);
  ShowSelectedChart;
  for t:=0 to ChartList.Count-1 do
  if ChartList[t]<>Sender then
  with Chart[t] do
  if Gradient.Visible  then
  begin
{    Color:=clBtnFace; }
    Gradient.Visible:=False;
    BevelOuter:=bvNone;
    With Title.Font do
    begin
      Style:=[];
      Color:=clNavy;
      Size:=GetDefaultGalleryFontSize;
    end;
    View3DOptions.Rotation:=335;
    if SeriesCount>0 then
       Series[0].GalleryChanged3D(Self.FView3D);
  end;
end;

procedure TTeeGalleryPanel.SetView3D(Value:Boolean);
var t:Integer;
begin
  if Value<>FView3D then
  begin
    FView3D:=Value;
    for t:=0 to ChartList.Count-1 do
    begin
      Chart[t].Series[0].GalleryChanged3D(FView3D);
      SetMargins(Chart[t]);
    end;
  end;
end;

Procedure TTeeGalleryPanel.SetMargins(AChart:TCustomChart);
Var tmp:Integer;
begin
  With AChart do
  begin
    if View3D then tmp:=2 else tmp:=6;
    MarginTop    :=tmp;
    MarginBottom :=tmp;
    MarginLeft   :=tmp;
    MarginRight  :=tmp;
  end;
end;

Procedure TTeeGalleryPanel.CreateChartList(Const ASeriesList:Array of TChartSeriesClass);
var t     : Integer;
    AType : TTeeSeriesType;
begin
  RemoveCharts;
  for t:=Low(ASeriesList) to High(ASeriesList) do
      if FindSeriesClassType(ASeriesList[t],AType) then CreateChart(AType,t);
end;

Procedure TTeeGalleryPanel.ResizeCharts;
var t : Integer;
begin
  CalcChartWidthHeight;
  for t:=0 to ChartList.Count-1 do ResizeChart(Chart[t]);
end;

procedure TTeeGalleryPanel.Resize;
begin
  inherited Resize;
  ResizeCharts;
end;

{ TeeTabControl  }
Procedure TTeeTabControl.AlignTab;
begin
  {$IFDEF D1}
  Align:=alBottom;
  {$ELSE}
  Align:=alClient;
  {$ENDIF}
end;

Procedure TTeeTabControl.SetTabEvent(Value:TNotifyEvent);
begin
  {$IFDEF D1}
  OnClick:=Value;
  {$ELSE}
  OnChange:=Value;
  {$ENDIF}
end;

{ TeeGallery }
Procedure TTeeGallery.HideFunctions;
begin
  TabControl.Tabs.Delete(1);
  FunctionsVisible:=False;
end;

Procedure TTeeGallery.HideButtons;
begin
  P1.Visible:=False;
end;

Function TTeeGallery.ValidSeries(Const ASeriesType:TTeeSeriesType; Const APage:String):Boolean;
begin
  result:= (ASeriesType.GalleryPage=APage) and
           (FunctionsVisible or (ASeriesType.FunctionClass=nil));
end;

Function TTeeGallery.CalcNumSeriesPage(Const APage:String):Longint;
var t:Longint;
begin
  result:=0;
  With TeeSeriesTypes do
  for t:=0 to Count-1 do if ValidSeries(SeriesType[t],APage) then Inc(result);
end;

Procedure TTeeGallery.CreateGallery(APage:Integer);
Var tmp    : TTeeSeriesType;
    tmpSt  : String;
    t      : Longint;
    tmpRow : Longint;
    tmpNum : Longint;
begin
  With GalleryPanel do
  begin
    RemoveCharts;
    tmpSt:=TabControl.Tabs[APage];
    { Adjust gallery grid Rows and Columns }
    SetRowCols(3,4);
    tmpNum:=CalcNumSeriesPage(tmpSt);
    if (tmpNum>0) then
      if (NumRows*NumCols) < tmpNum then
      begin
        tmpRow:=tmpNum div 4;
        if tmpRow>0 then
        begin
          if (tmpRow*4) <> tmpNum then Inc(tmpRow);
          SetRowCols(tmpRow,4);
        end;
      end;

    View3D:=CB3D.Checked;
    CheckSeries:=not FunctionsVisible;
    With TeeSeriesTypes do
    for t:=0 to Count -1 do
    begin
      tmp:=SeriesType[t];
      if ValidSeries(tmp,tmpSt) then CreateChart(tmp,ChartList.Count);
    end;
  end;
end;

procedure TTeeGallery.GalleryPanelOnSelected(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TTeeGallery.CB3DClick(Sender: TObject);
begin
  GalleryPanel.View3D:=CB3D.Checked;
end;

procedure TTeeGallery.FormShow(Sender: TObject);
var t : Integer;
begin
  if GalleryPanel.ChartList.Count=0 then
  begin
    With TabControl do
    With TeeSeriesTypes do
    for t:=0 to Count-1 do
    With SeriesType[t] do
      if NumGallerySeries>0 then
         if Tabs.IndexOf(GalleryPage)=-1 then
            if (FunctionsVisible or (FunctionClass=nil)) then
               Tabs.Add(GalleryPage);
    CreateGallery(0);
  end;
  GalleryPanel.FindSelectedChart;
end;

procedure TTeeGallery.FormResize(Sender: TObject);
begin
  if Assigned(GalleryPanel) then GalleryPanel.ResizeCharts;
end;

procedure TTeeGallery.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  TeeEraseBack:=False;
  FunctionsVisible:=True;
{  Width :=389{432};
{  Height:=344{356};
  TabControl:=TTeeTabControl.Create(Self);
  With TabControl do
  begin
    Parent:=Self;
    AlignTab;
    Tabs.Add(TeeMsg_GalleryStandard);
    Tabs.Add(TeeMsg_GalleryFunctions);
    SetTabEvent(TabControl1Change);
    TabIndex:=0;
  end;
  GalleryPanel:=TTeeGalleryPanel.Create(Self);
  With GalleryPanel do
  begin
   {$IFDEF D1}
    Parent:=Self;
   {$ELSE}
    Parent:=TabControl;
   {$ENDIF}
    Align:=alClient;
    BevelOuter:=bvNone;
    OnSelectedChart:=GalleryPanelOnSelected;
  end;
  CreatingForm:=False;
end;

procedure TTeeGallery.TabControl1Change(Sender: TObject);
begin
  if not CreatingForm then
  begin
    CreateGallery(TabControl.TabIndex);
    GalleryPanel.FindSelectedChart;
  end;
end;

procedure TTeeGallery.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GalleryPanel.ProcessKeyDown(Sender,Key,Shift);
end;

Procedure TTeeGallery.CreateGalleryList(Const ASeriesList:Array of TChartSeriesClass);
begin
  With GalleryPanel do
  begin
    RemoveCharts;
    View3D:=CB3D.Checked;
    CheckSeries:=not FunctionsVisible;
    CreateChartList(ASeriesList);
  end;
end;

{ Helper functions }
Function GetGallerySeriesName(ASeries:TChartSeries):String;
var AType:TTeeSeriesType;
begin
  if FindSeriesClassType(TChartSeriesClass(ASeries.ClassType),AType) then
     result:=AType.Description
  else
     result:=ASeries.ClassName;
end;

Function GetChartGalleryClass( AOwner:TComponent;
                               OldSeries:TChartSeries;
                               ShowFunctions:Boolean;
                               Var tmpClass:TChartSeriesClass;
                               Var Show3D:Boolean;
                               Var tmpFunctionClass:TTeeFunctionClass
                              ):Boolean;
var tmpSeries  : TChartSeries;
    tmpGallery : TTeeGallery;
begin
  result:=False;
  tmpGallery:=TTeeGallery.Create(nil);
  With tmpGallery do
  try
    CB3D.Checked:=Show3D;
    GalleryPanel.SelectedSeries:=OldSeries;
    if not ShowFunctions then HideFunctions;
    if ShowModal=mrOk then
    begin
      if Assigned(GalleryPanel.SelectedChart) then
      begin
        tmpSeries:=GalleryPanel.SelectedChart[0];
        tmpClass:=TChartSeriesClass(tmpSeries.ClassType);
        if tmpSeries.FunctionType<>nil then
           tmpFunctionClass:=TTeeFunctionClass(tmpSeries.FunctionType.ClassType)
        else
           tmpFunctionClass:=nil;
        Show3D:=GalleryPanel.SelectedChart.View3D;
        result:=True;
      end;
    end;
  finally
    tmpGallery.Free;
  end;
end;

Function CreateNewSeriesGallery( AOwner:TComponent;
                                 OldSeries:TChartSeries;
                                 tmpChart:TCustomChart;
                                 AllowSameType,
                                 ShowFunctions:Boolean ):TChartSeries;
var Show3D   : Boolean;
    tmpClass : TChartSeriesClass;
    tmpFunctionClass:TTeeFunctionClass;
begin
  result:=nil;
  Show3D:=tmpChart.View3D;
  if GetChartGalleryClass(AOwner,OldSeries,AllowSameType,tmpClass,Show3D,tmpFunctionClass) then
  begin
    tmpChart.View3D:=Show3D;
    if (not Assigned(OldSeries)) or
       (AllowSameType or (tmpClass<>OldSeries.ClassType)) then
         result:=CreateNewSeries(AOwner,tmpChart,tmpClass,tmpFunctionClass);
  end;
end;

procedure ChangeSeriesTypeGallery(AOwner:TComponent; Var tmpSeries:TChartSeries);
var NewSeries:TChartSeries;
begin
  NewSeries:=CreateNewSeriesGallery(AOwner,tmpSeries,TCustomChart(tmpSeries.ParentChart),False,False);
  if Assigned(NewSeries) then
  begin
    AssignSeries(tmpSeries,NewSeries);
    tmpSeries:=NewSeries;
  end;
end;

procedure ChangeAllSeriesGallery( AOwner:TComponent; AChart:TCustomChart );
var NewSeries : TChartSeries;
    tmpSeries : TChartSeries;
begin
  if AChart.SeriesCount>0 then
  begin
    tmpSeries:=AChart[0];
    NewSeries:=CreateNewSeriesGallery(AOwner,tmpSeries,AChart,False,False);
    if Assigned(NewSeries) then
    begin
      AssignSeries(tmpSeries,NewSeries);
      ChangeAllSeriesType(AChart,TChartSeriesClass(NewSeries.ClassType));
    end;
  end;
end;

end.
