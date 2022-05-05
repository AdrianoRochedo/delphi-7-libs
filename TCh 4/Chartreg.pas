{*****************************************}
{   TeeChart 4.0 Standard version         }
{                                         }
{ Component Registration Unit             }
{                                         }
{ Comps:   TChart                         }
{          TDBChart                       }
{                                         }
{ Series:  TLineSeries                    }
{          TAreaSeries                    }
{          TPointSeries                   }
{          TBarSeries                     }
{          THorizBarSeries                }
{          TPieSeries                     }
{          TBubbleSeries                  }
{          TGanttSeries                   }
{          TChartShape                    }
{          TArrowSeries                   }
{          TFastLineSeries                }
{                                         }
{   Copyright (c) 1996-98 David Berneda   }
{   All Rights Reserved                   }
{*****************************************}

{$I teedefs.inc}
unit ChartReg;

interface

Uses DesignIntf,TeEngine,Chart,TeCanvas, DesignEditors;

{$IFDEF D1}
{$R TEECHART.D16}
{$ELSE}
{$R TEECHART.RES}
{$ENDIF}

Type
  TChartClassProperty=class(TClassProperty)
  protected
    procedure InternalEditPage(AChart:TCustomChart; APage:Integer);
    procedure InternalEditSeries(ASeries:TChartSeries);
  public
    function GetAttributes : TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TChartCompEditor=class(TComponentEditor)
  protected
    procedure ChartExecuteVerb( Index : Integer; AChart:TCustomChart );
  public
    procedure Edit; override;
    procedure ExecuteVerb( Index : Integer ); override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
  end;

  TDBChartCompEditor=class(TChartCompEditor)
  public
    procedure ExecuteVerb( Index : Integer ); override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
    procedure Edit; override;
  end;

  TChartPenProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TChartBrushProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

procedure EditChartDesign(AChart:TCustomChart); { for QrTeeReg.pas }

procedure Register;

implementation

Uses TypInfo,Classes,EditChar,TeePrevi,TeeProcs,TeeConst,DBChart,DB,
     WinTypes,WinProcs,SysUtils,Graphics,Clipbrd,TeeAbout,
{$IFDEF D1}
     IEdit16,
{$ELSE}
     IEditCha,IEdiSeri,IEdiGene,
{$ENDIF}
     DBEditCh,CustEdit,Series,PieEdit,AreaEdit,BarEdit,FLineEdi,
     Forms,PenDlg,BrushDlg,Dialogs,BubbleCh,
     ArrowCha,ArrowEdi,GanttCh,GanttEdi,
     TeeShape,ShapeEdi,TeExport;

type
  TChartLegendProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TCustomChartAxisProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TChartAxisTitleProperty=class(TClassProperty)
  public
    function GetValue: string; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

  TChartSeriesListProperty=class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

  TSeriesPointerProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TChartTitleProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TChartWallProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TChartGradientProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TSeriesMarksProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

  TView3DOptionsProperty=class(TChartClassProperty)
  public
    procedure Edit; override;
  end;

{$IFNDEF D1}
type TFake=class
	FAddCurrent   : Boolean;
	FItems        : TStrings;
	FFormDesigner :{$IFDEF D5}IDesigner{$ELSE}TFormDesigner{$ENDIF};
	FProc         : TAddComponentDataSource;
	Procedure AddDataSource(Const St:String);
     end;

Procedure TFake.AddDataSource(Const St:String);
Var tmpComp:TComponent;
begin
  if St<>'' then
  begin
    tmpComp:=FFormDesigner.GetComponent(St);
    if tmpComp<>nil then
       if Assigned(FProc) then FProc(tmpComp,FItems,FAddCurrent);
  end;
end;

Procedure DesignTimeOnGetDesignerNames( AProc      : TAddComponentDataSource;
					ASeries    : TChartSeries;
					AItems     : TStrings;
					AddCurrent : Boolean );
Var tmpForm:{$IFDEF D3} TCustomForm {$ELSE} TForm {$ENDIF};
begin
  tmpForm:=TForm(GetParentForm(ASeries.ParentChart));
  if (tmpForm<>nil) and (tmpForm.Designer<>nil) then
  {$IFDEF D5}
  With tmpForm.Designer as IDesigner do
  {$ELSE}
  With TFormDesigner(tmpForm.Designer) do
  {$ENDIF}
  begin
    With TFake.Create do
    try
      FProc:=AProc;
      FItems:=AItems;
      FAddCurrent:=AddCurrent;
      {$IFDEF D5}
      FFormDesigner:=tmpForm.Designer as IDesigner;
      {$ELSE}
      FFormDesigner:=TFormDesigner(tmpForm.Designer);
      {$ENDIF}
      GetComponentNames(GetTypeData(TDataSource.ClassInfo),AddDataSource);
      GetComponentNames(GetTypeData(TDataSet.ClassInfo),AddDataSource);
      GetComponentNames(GetTypeData(TChartSeries.ClassInfo),AddDataSource);
    finally
      Free;
    end;
  end;
end;
{$ENDIF}

{ Chart Editor }
procedure EditChartDesign(AChart:TCustomChart);
var Part:TChartClickedPart;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  With AChart do CalcClickedPart(GetCursorPos,Part);
  EditChartPart(nil,AChart,Part);
end;

procedure TChartCompEditor.Edit;
begin
  EditChartDesign(TCustomChart(Component));
  Designer.Modified;
end;

procedure TChartCompEditor.ChartExecuteVerb( Index : Integer; AChart:TCustomChart );
begin
  Case Index of
    3: TeeShowAboutBox({$IFDEF TEETRIAL}True{$ELSE}False{$ENDIF});
    4: Edit;
    5: ChartPreview(nil,AChart);
    6: ChartExport(nil,AChart);
  else
    inherited ExecuteVerb(Index);
  end;
end;

procedure TChartCompEditor.ExecuteVerb( Index : Integer );
begin
  ChartExecuteVerb(Index,TCustomChart(Component));
end;

function TChartCompEditor.GetVerbCount : Integer;
begin
  Result := 7;
end;

function TChartCompEditor.GetVerb( Index : Integer ) : string;
begin
  result:='';
  Case Index of
    0: result:=TeeMsg_Version;
    1: result:=TeeMsg_Copyright;
    2: result:='-';  { <--- do not change or translate... }
    3: result:=TeeMsg_About;
    4: result:=TeeMsg_EditChart;
    5: result:=TeeMsg_PrintPreview;
    6: result:=TeeMsg_ExportChart;
  end;
end;

{ Generic Chart Class Editor (for chart sub-components) }
function TChartClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties,paDialog];
end;

function TChartClassProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TChartClassProperty.InternalEditPage( AChart:TCustomChart;
						APage:Integer);
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChartPage(nil,AChart,APage);
  Designer.Modified;
end;

procedure TChartClassProperty.InternalEditSeries(ASeries:TChartSeries);
Begin
  {$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
  {$ENDIF}
  EditSeries(nil,ASeries);
  Designer.Modified;
end;

{ Chart Legend Editor }
procedure TChartLegendProperty.Edit;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChartLegend(nil,TCustomChart(TChartLegend(GetOrdValue).ParentChart));
  Designer.Modified;
end;

{ Axis Chart Editor }
procedure TCustomChartAxisProperty.Edit;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChartAxis(nil,TCustomChartAxis(GetOrdValue));
  Designer.Modified;
end;

{ Chart Series Editor }
function TChartSeriesListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TChartSeriesListProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TChartSeriesListProperty.Edit;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChart(nil,TCustomChart(TChartSeriesList(GetOrdValue).Owner));
  Designer.Modified;
end;

{ Chart Axis Title Editor }
function TChartAxisTitleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

function TChartAxisTitleProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ Chart Title Editor }
procedure TChartTitleProperty.Edit;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChartTitle(nil,TChartTitle(GetOrdValue));
  Designer.Modified;
end;

{ Chart Wall Editor }
procedure TChartWallProperty.Edit;
begin
{$IFNDEF D1}
  InternalOnGetDesignerNames:=DesignTimeOnGetDesignerNames;
{$ENDIF}
  EditChartWall(nil,TChartWall(GetOrdValue));
  Designer.Modified;
end;

{ Series Pointer Editor }
procedure TSeriesPointerProperty.Edit;
begin
  InternalEditSeries(TSeriesPointer(GetOrdValue).ParentSeries);
end;

{ ChartPen Editor }
procedure TChartPenProperty.Edit;
begin
  EditChartPen(nil,TChartPen(GetOrdValue));
  Designer.Modified;
end;

{ ChartBrush Editor }
procedure TChartBrushProperty.Edit;
begin
  EditChartBrush(nil,TBrush(GetOrdValue));
  Designer.Modified;
end;

{ Chart Series Marks Editor }
procedure TSeriesMarksProperty.Edit;
var ASeries      : TChartSeries;
    ASeriesMarks : TSeriesMarks;
begin
  ASeriesMarks:=TSeriesMarks(GetOrdValue);
  if Assigned(ASeriesMarks) then
  Begin
    ASeries:=ASeriesMarks.ParentSeries;
    if Assigned(ASeries) then InternalEditSeries(ASeries);
  end;
end;

{ Axis Chart Editor }
procedure TChartGradientProperty.Edit;
begin
  InternalEditPage(TCustomChart(TChartGradient(GetOrdValue).Owner),teeEditPanelPage);
end;

{ TSeriesDataSource Property }
type
  TSeriesDataSourceProperty = class(TComponentProperty)
  private
    {$IFNDEF D1}
    FAddDataSetProc:TGetStrProc;
    procedure AddDataSource(Const S:String);
    {$ENDIF}
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFNDEF D1}
procedure TSeriesDataSourceProperty.AddDataSource(Const S:String);
Var tmpSeries:TChartSeries;
    tmpComp:TComponent;
begin
  if S<>'' then
  begin
    tmpComp:=Designer.GetComponent(S);
    if tmpComp<>nil then
    begin
      tmpSeries:=TChartSeries(GetComponent(0));
      if tmpSeries.ParentChart.IsValidDataSource(tmpSeries,tmpComp) then
	 FAddDataSetProc(S);
    end;
  end;
end;
{$ENDIF}

procedure TSeriesDataSourceProperty.GetValues(Proc: TGetStrProc);
Var tmpSeries:TChartSeries;

  {$IFDEF D1}
  Procedure FillSourcesForm(AOwner:TComponent; Const AFormName:String);
  var t:Longint;
      AComponent:TComponent;
  begin
    if Assigned(AOwner) then
    for t:=0 to AOwner.ComponentCount-1 do
    begin
      AComponent:=AOwner.Components[t];
      if (AComponent.Name<>'') and
	 tmpSeries.ParentChart.IsValidDataSource(tmpSeries,AComponent) then
	 if AFormName='' then Proc(AComponent.Name)
			 else Proc(AFormName+'.'+AComponent.Name);
    end;
  end;
  {$ENDIF}

begin
  tmpSeries:=TChartSeries(GetComponent(0));
  if tmpSeries.ParentChart<>nil then
  Begin
    {$IFNDEF D1}
    FAddDataSetProc:=Proc;
    Designer.GetComponentNames(GetTypeData(TDataSource.ClassInfo),AddDataSource);
    Designer.GetComponentNames(GetTypeData(TDataSet.ClassInfo),AddDataSource);
    Designer.GetComponentNames(GetTypeData(TChartSeries.ClassInfo),AddDataSource);
    {$ELSE}
    FillSourcesForm(Designer.Form,'');
    {$ENDIF}
  end;
end;

{ TValueSource Property }
type
  TValueSourceProperty=class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TValueSourceProperty.GetAttributes : TPropertyAttributes;
Begin
  result:=inherited GetAttributes+[paValueList];
end;

procedure TValueSourceProperty.GetValues(Proc: TGetStrProc);
Var AValueList:TChartValueList;
begin
  AValueList:=TChartValueList(GetComponent(0));
  With AValueList.Owner do
  if (ParentChart<>nil) then
      TCustomChart(ParentChart).FillValueSourceItems(AValueList,Proc);
end;

{ TSeriesSource Property }
type
  TSeriesSourceProperty=class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TSeriesSourceProperty.GetValues(Proc: TGetStrProc);
Var ASeries:TChartSeries;
begin
  ASeries:=TChartSeries(GetComponent(0));
  With ASeries do
  if (ParentChart<>nil) then
     TCustomChart(ParentChart).FillSeriesSourceItems(ASeries,Proc);
end;

function TSeriesSourceProperty.GetAttributes : TPropertyAttributes;
Begin
  result:=inherited GetAttributes+[paValueList];
end;

{ DBChart Editor }
procedure TDBChartCompEditor.ExecuteVerb( Index : Integer );
begin
  if Index+1=GetVerbCount then
     TCustomDBChart(Component).RefreshData
  else
     inherited ExecuteVerb(Index);
end;

function TDBChartCompEditor.GetVerbCount : Integer;
begin
  Result := inherited GetVerbCount+1;
end;

function TDBChartCompEditor.GetVerb( Index : Integer ) : string;
begin
  if Index+1=GetVerbCount then
     result:=TeeMsg_RefreshData
  else
     result:=inherited GetVerb(Index);
end;

procedure TDBChartCompEditor.Edit;
begin
  EditChartDesign(TCustomDBChart(Component));
  Designer.Modified;
end;

{ View3DOptions Editor }
procedure TView3DOptionsProperty.Edit;
begin
  InternalEditPage(TCustomChart(TView3DOptions(GetOrdValue).Parent),teeEdit3DPage);
end;

procedure Register;
begin
  RegisterNoIcon( [ TChartSeries, TTeeFunction ]);

  RegisterComponents( tcAdditional, [TChart] );
  RegisterComponentEditor(TCustomChart,TChartCompEditor);

  RegisterComponents( tcDControls, [TDBChart] );
  RegisterComponentEditor(TCustomDBChart,TDBChartCompEditor);

  {$IFDEF TEENOREGAX}
  RegisterNonActiveX( [ TCustomTeePanel,
			TCustomAxisPanel,
			TCustomChart,TChart,
			TCustomDBChart,TDBChart] , axrIncludeDescendants );
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TSeriesMarks),TChartSeries,
				  'Marks', TSeriesMarksProperty);
  RegisterPropertyEditor(TypeInfo(TSeriesPointer), TCustomSeries, 'Pointer',
						   TSeriesPointerProperty);

  {$IFDEF D3}
  RegisterPropertyEditor(TypeInfo(TChartWall),nil,
				  '', TChartWallProperty);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TChartWall),TCustomChart,
				  'LeftWall', TChartWallProperty);
  RegisterPropertyEditor(TypeInfo(TChartWall),TCustomChart,
				  'BottomWall', TChartWallProperty);
  RegisterPropertyEditor(TypeInfo(TChartWall),TCustomChart,
				  'BackWall', TChartWallProperty);
  {$ENDIF}

  {$IFDEF D3}
  RegisterPropertyEditor(TypeInfo(TChartTitle),TCustomChart,
				  '', TChartTitleProperty);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TChartTitle),TCustomChart,
				  'Title', TChartTitleProperty);
  RegisterPropertyEditor(TypeInfo(TChartTitle),TCustomChart,
				  'Foot', TChartTitleProperty);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCustomChartLegend),TCustomChart,
				  'Legend', TChartLegendProperty);

  {$IFDEF D3}
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  '', TCustomChartAxisProperty);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  'LeftAxis', TCustomChartAxisProperty);
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  'RightAxis', TCustomChartAxisProperty);
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  'TopAxis', TCustomChartAxisProperty);
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  'BottomAxis', TCustomChartAxisProperty);
  RegisterPropertyEditor(TypeInfo(TCustomChartAxis),TCustomChart,
				  'DepthAxis', TCustomChartAxisProperty);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TChartSeriesList), TCustomChart, 'SeriesList',
						     TChartSeriesListProperty);
  RegisterPropertyEditor(TypeInfo(TChartAxisTitle), TCustomChartAxis, 'Title',
						    TChartAxisTitleProperty);
  RegisterPropertyEditor(TypeInfo(TChartGradient),TCustomChart,
				  'Gradient', TChartGradientProperty);

  RegisterPropertyEditor( TypeInfo(TComponent),
			  TChartSeries,'DataSource',TSeriesDataSourceProperty);

  RegisterPropertyEditor( TypeInfo(String),
			  TChartValueList,'ValueSource',TValueSourceProperty);

  RegisterPropertyEditor( TypeInfo(String),
			  TChartSeries,'ColorSource',TSeriesSourceProperty);
  RegisterPropertyEditor( TypeInfo(String),
			  TChartSeries,'XLabelsSource',TSeriesSourceProperty);

  {$IFDEF D3}
  RegisterPropertyEditor(TypeInfo(TChartPen), nil, '',TChartPenProperty);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TChartPen), TChartLegend, 'Frame',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TChartTitle,  'Frame',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChartAxis,   'Ticks',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChartAxis,   'MinorTicks',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChartAxis,   'TicksInner',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChartAxis,   'Axis',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChartAxis,   'Grid',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomChart, 'Frame',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TSeriesMarks, 'Frame',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TSeriesMarks, 'Arrow',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomSeries,'LinePen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomSeries,'AreaLinesPen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TSeriesPointer, 'Pen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TBarSeries,   'BarPen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCircledSeries,'PiePen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TGanttSeries, 'ConnectingPen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TChartWall,   'Pen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TFastLineSeries, 'LinePen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TChartShape, 'Pen',TChartPenProperty);
  {$ENDIF}

  {$IFDEF D3}
  RegisterPropertyEditor(TypeInfo(TChartBrush), nil, '',TChartBrushProperty);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TChartBrush), TChartShape, 'Brush',TChartBrushProperty);
  RegisterPropertyEditor(TypeInfo(TChartBrush), TCustomBarSeries, 'BarBrush',TChartBrushProperty);
  RegisterPropertyEditor(TypeInfo(TChartBrush), TChartTitle, 'Brush',TChartBrushProperty);
  RegisterPropertyEditor(TypeInfo(TChartBrush), TChartWall, 'Brush',TChartBrushProperty);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TView3DOptions),TCustomChart,
				  'View3DOptions', TView3DOptionsProperty);
end;

end.
