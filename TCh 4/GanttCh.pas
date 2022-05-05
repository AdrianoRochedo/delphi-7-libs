{**********************************************}
{   TGanttSeries (derived from TPointSeries)   }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit GanttCh;

interface

{ This unit shows how a new Chart Series component can be easily created.
  TGanttSeries derives from standard TPointSeries.
  Each point in the series is drawn like a Gantt horizontal bar.
  Each point has a Start and End values that are used to draw the Gantt bar
  with its corresponding screen length in the horizontal plane. }
Uses Classes,Graphics,Chart,Series,Teengine,WinTypes,TeCanvas;

Type
  TGanttSeries=class(TPointSeries)
  private
    { The Gantt Start values are implicit stored in XValues }
    FEndValues     : TChartValueList; { <-- Gantt bar's end values storage }
    FNextTask      : TChartValueList;  { <-- Used to connect lines }
    FConnectingPen : TChartPen;
    Procedure SetConnectingPen(Value:TChartPen);
    Procedure SetEndValues(Value:TChartValueList);
    Procedure SetStartValues(Value:TChartValueList);
    Procedure SetNextTask(Value:TChartValueList);
    Function GetStartValues:TChartValueList;
  protected
    procedure DrawValue(ValueIndex:Longint); override; { <-- main draw method }
    Procedure DrawMark(ValueIndex:Longint; Const St:String; APosition:TSeriesMarkPosition); override;
    Function ClickedPointer( ValueIndex,tmpX,tmpY:Longint;
                             x,y:Longint):Boolean; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function AddGantt(Const AStart,AEnd,AY:Double; Const AXLabel:String{$IFDEF D5}=''{$ENDIF}):Longint;
    Function AddGanttColor( Const AStart,AEnd,AY:Double;
                            Const AXLabel:String{$IFDEF D5}=''{$ENDIF};
                            AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF} ):Longint;
    Procedure Assign(Source:TPersistent); override;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
    Procedure ClearTempValue(ValueList:TChartValueList); override;
    Procedure FillSampleValues(NumValues:Longint); override; { <-- to add random end values }
    Function GetEditorClass:String; override;
    Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
    Function MandatoryValueList:TChartValueList; override;
    Function MaxXValue:Double; override;  { <-- adds end values }
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    property ConnectingPen:TChartPen read FConnectingPen write SetConnectingPen;
    property StartValues:TChartValueList read GetStartValues write SetStartValues;
    property EndValues:TChartValueList read FEndValues write SetEndValues;
    property NextTask:TChartValueList read FNextTask write SetNextTask;
  end;

implementation

Uses SysUtils,TeeProcs,TeeConst;

{ TGanttSeries }
Constructor TGanttSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  ClickableLine:=False; { only allow click on Pointer (Gantt Bar) }
  CalcVisiblePoints:=False; { draw all points }
  XValues.Name:=TeeMsg_ValuesGanttStart;
  XValues.DateTime:=True;
  ColorEachPoint:=True;
  FEndValues :=TChartValueList.Create(Self,TeeMsg_ValuesGanttEnd);
  FEndValues.DateTime:=True;
  FNextTask  :=TChartValueList.Create(Self,TeeMsg_ValuesGanttNextTask);
  Pointer.Style:=psRectangle; { <-- a Horizontal Gantt Bar (by default) }
  FConnectingPen:=CreateChartPen;
end;

Destructor TGanttSeries.Destroy;
Begin
  FConnectingPen.Free;
  inherited Destroy;
End;

Procedure TGanttSeries.SetEndValues(Value:TChartValueList);
Begin
  SetChartValueList(FEndValues,Value); { standard method }
end;

Procedure TGanttSeries.SetNextTask(Value:TChartValueList);
Begin
  SetChartValueList(FNextTask,Value); { standard method }
end;

Procedure TGanttSeries.SetConnectingPen(Value:TChartPen);
Begin
  FConnectingPen.Assign(Value);
end;

{ Helper method, special to Gantt bar series }
Function TGanttSeries.AddGanttColor( Const AStart,AEnd,AY:Double;
                                     Const AXLabel:String;
                                     AColor:TColor ):Longint;
begin
  result:=AddXY(AStart,AY,AXLabel,AColor); { standard add X,Y }
  FEndValues.TempValue:=AEnd;
  FNextTask.TempValue:=-1;
  AddValue(result);
end;

{ Helper method, special to Gantt bar series }
Function TGanttSeries.AddGantt( Const AStart,AEnd,AY:Double;
                                Const AXLabel:String):Longint;
Begin
  result:=AddGanttColor(AStart,AEnd,AY,AXLabel {$IFNDEF D5},clTeeColor{$ENDIF});
end;

Procedure TGanttSeries.FillSampleValues(NumValues:Longint);

  Function GanttSampleStr(Index:Longint):String;
  begin
    Case Index of
      0: result:=TeeMsg_GanttSample1;
      1: result:=TeeMsg_GanttSample2;
      2: result:=TeeMsg_GanttSample3;
      3: result:=TeeMsg_GanttSample4;
      4: result:=TeeMsg_GanttSample5;
      5: result:=TeeMsg_GanttSample6;
      6: result:=TeeMsg_GanttSample7;
      7: result:=TeeMsg_GanttSample8;
      8: result:=TeeMsg_GanttSample9;
     else
        result:=TeeMsg_GanttSample10;
     end;
  end;

Const NumGanttSamples=10;
Var Added        : Longint;
    t            : Longint;
    tmpY         : Longint;
    tt           : Longint;
    tmpStartTask : TDateTime;
    tmpEndTask   : TDateTime;
Begin
  Clear;
  { some sample values to see something at design mode }
  for t:=0 to MinLong( NumValues, NumGanttSamples+Random(20) ) do
  begin
    tmpStartTask:=Date+t*3+Random(5);
    tmpEndTask:=tmpStartTask+9+Random(16);
    tmpY:=(t mod 10);
    Added:=AddGantt( tmpStartTask, { Start }
                     tmpEndTask,   { End }
                     tmpY,         { Y value }
                     GanttSampleStr(tmpY) { some sample label text }
                     );
    { Connect Gantt points: }
    for tt:=0 to added-1 do
    if (NextTask[tt]=-1) and (tmpStartTask>EndValues[tt]) then
    begin
      NextTask[tt]:=added;
      break;
    end;
  end;
  RefreshSeries;
end;

Function TGanttSeries.ClickedPointer( ValueIndex,tmpX,tmpY:Longint;
                                      x,y:Longint):Boolean;
begin
  result:=(x>=tmpX) and (x<=CalcXPosValue(EndValues[ValueIndex])) and
          (Abs(tmpY-Y)<Pointer.VertSize);
end;

procedure TGanttSeries.DrawValue(ValueIndex:Longint);
var x1              : Longint;
    x2              : Longint;
    Y               : Longint;
    tmpHalfHorizSize: Longint;
    HalfWay         : Longint;
    tmpNextTask     : Longint;
    xNext           : Longint;
    YNext           : Longint;
    tmpStyle        : TSeriesPointerStyle;
Begin
{ This overrided method is the main paint for Gantt bar points. }
  if Pointer.Visible then
  With ParentChart.Canvas do
  Begin
    Pointer.PrepareCanvas(ValueColor[ValueIndex]);
    X1:=CalcXPos(ValueIndex);
    X2:=CalcXPosValue(EndValues[ValueIndex]);
    tmpHalfHorizSize:=(x2-x1) div 2;
    Y:=CalcYPos(ValueIndex);
    {$IFNDEF D1}
    if Assigned(OnGetPointerStyle) then
       tmpStyle:=OnGetPointerStyle(Self,ValueIndex)
    else
    {$ENDIF}
       tmpStyle:=Pointer.Style;
    Pointer.DrawPointer( ParentChart.View3D,
                         x1+tmpHalfHorizSize,
                         Y,
                         tmpHalfHorizSize,
                         Pointer.VertSize,
                         ValueColor[ValueIndex],tmpStyle);
    if FConnectingPen.Visible then
    Begin
      tmpNextTask:=Round(NextTask[ValueIndex]);
      if (tmpNextTask>=0) and (tmpNextTask<Count) then
      Begin
        Pen.Assign(FConnectingPen);
        Brush.Style:=bsClear;
        XNext:=CalcXPos(tmpNextTask);
        HalfWay:=X2+((XNext-X2) div 2);
        YNext:=CalcYPos(tmpNextTask);
        LineWithZ(X2,Y,HalfWay,Y,MiddleZ);
        LineTo3D(HalfWay,YNext,MiddleZ);
        LineTo3D(XNext,YNext,MiddleZ);
      End;
    end;
  end;
end;

Function TGanttSeries.MaxXValue:Double;
Begin
  result:=MaxDouble(inherited MaxXValue,FEndValues.MaxValue);
end;

Procedure TGanttSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                 APosition:TSeriesMarkPosition);
Begin
  With APosition do
  begin
    Inc(LeftTop.X,(CalcXPosValue(EndValues[ValueIndex])-ArrowFrom.X) div 2);
    Inc(LeftTop.Y,Height div 2);
  end;
  inherited DrawMark(ValueIndex,St,APosition);
End;

Procedure TGanttSeries.ClearTempValue(ValueList:TChartValueList);
Begin
  if ValueList=FNextTask then FNextTask.TempValue:=-1
                         else inherited ClearTempValue(ValueList);
End;

Function TGanttSeries.MandatoryValueList:TChartValueList;
Begin
  Result:=StartValues;  { <-- this is used in DataSource Expert Dialog }
End;

Function TGanttSeries.GetStartValues:TChartValueList;
Begin
  result:=XValues;
end;

Procedure TGanttSeries.SetStartValues(Value:TChartValueList);
Begin
  SetXValues(Value);
end;

Procedure TGanttSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  ColorEachPoint:=IsEnabled;
  Pointer.VertSize:=3;
end;

Function TGanttSeries.GetEditorClass:String;
begin
  result:='TGanttSeriesEditor';  { <-- dont translate ! }
end;

Procedure TGanttSeries.Assign(Source:TPersistent);
begin
  if Source is TGanttSeries then
     FConnectingPen.Assign(TGanttSeries(Source).FConnectingPen);
  inherited Assign(Source);
end;

Function TGanttSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TGanttSeries; { Only Gantt can be assigned to Gantt }
end;

Procedure TGanttSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin); { Fix Delphi 1 "Top Axis" }
end;

initialization
 RegisterTeeSeries(TGanttSeries,TeeMsg_GalleryGantt,TeeMsg_GalleryStandard,1);
end.
