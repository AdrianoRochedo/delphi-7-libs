{****************************************}
{       TDBChart Component               }
{ Copyright (c) 1995-98 by David Berneda }
{   All rights Reserved                  }
{****************************************}
{$I teedefs.inc}
unit DBChart;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Chart, DB, TeeProcs, Menus, TeCanvas,
 {$IFNDEF D3}
  DBTables,DBIErrs,
 {$ENDIF}
  StdCtrls,Teengine;

type
  DBChartException=class(Exception);

  TListOfDataSources=class(TList)
  private
    procedure SetDataSource(Index:Integer; DataSource:TDataSource);
    function GetDataSource(Index:Integer):TDataSource;
  public
    property DataSource[Index:Integer]:TDataSource read GetDataSource
                                                   write SetDataSource; {$IFNDEF D1}default;{$ENDIF}
  end;

  TCustomDBChart=class;

  TProcessRecordEvent=Procedure(Sender:TCustomDBChart; DataSet:TDataSet) of object;

  TCustomDBChart = class(TCustomChart)
  private
    FAutoRefresh     : Boolean;
    FRefreshInterval : Longint;
    FShowGlassCursor : Boolean;
    FOnProcessRecord : TProcessRecordEvent;
    { internal }
    IUpdating        : Boolean; 
    ITimer           : TTimer;
    IDataSources     : TListOfDataSources;
    Procedure CheckDataSet(ADataSet:TDataSet; ASeries:TChartSeries);
    Procedure CheckNewDataSource(ADataSet:TDataSet; SingleRow:Boolean);
    Procedure DataSourceRowChange(Sender:TObject; Field:TField);
    Procedure DataSourceStateChange(Sender:TObject);
    Procedure DataSourceUpdateData(Sender:TObject);
    Procedure SetRefreshInterval(Value:Longint);
    Procedure CheckTimer;
    Procedure OnRefreshTimer(Sender:TObject);
  protected
    procedure RemovedDataSource( ASeries: TChartSeries;
                                 AComponent: TComponent ); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Procedure CheckDatasource(ASeries:TChartSeries); override;
    Function IsValidDataSource(ASeries:TChartSeries; AComponent:TComponent):Boolean; override;

    Procedure FillValueSourceItems( AValueList:TChartValueList; Proc:TGetStrProc); override;
    Procedure FillSeriesSourceItems( ASeries:TChartSeries; Proc:TGetStrProc); override;

    Procedure RefreshDataSet(ADataSet:TDataSet; ASeries:TChartSeries);
    Procedure RefreshData;
    { properties }
    property AutoRefresh:Boolean read FAutoRefresh write FAutoRefresh default True;
    property RefreshInterval:Longint read FRefreshInterval write SetRefreshInterval default 0;
    property ShowGlassCursor:Boolean read FShowGlassCursor write FShowGlassCursor default True;
    { events }
    property OnProcessRecord:TProcessRecordEvent read FOnProcessRecord
                                                 write FOnProcessRecord;
  published
  end;

  TDBChart=class(TCustomDBChart)
  published
    { TCustomDBChart properties }
    property AutoRefresh;
    property RefreshInterval;
    property ShowGlassCursor;
    { TCustomDBChart events }
    property OnProcessRecord;

    { TCustomChart Properties }
    property AllowPanning;
    property AllowZoom;
    property AnimatedZoom;
    property AnimatedZoomSteps;
    property BackImage;
    property BackImageInside;
    property BackImageMode;
    property BackWall;
    property BottomWall;
    property Foot;
    property Gradient;
    property LeftWall;
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property PrintProportional;
    property Title;

    { TCustomChart Events }
    property OnAllowScroll;
    property OnClickAxis;
    property OnClickLegend;
    property OnClickSeries;
    property OnClickBackground;
    property OnGetLegendPos;
    property OnGetLegendRect;
    property OnScroll;
    property OnUndoZoom;
    property OnZoom;

    { TCustomAxisPanel properties }
    property AxisVisible;
    property BackColor;
    property BottomAxis;
    property Chart3DPercent;
    property ClipPoints;
    property DepthAxis;
    property Frame;
    property LeftAxis;
    property Legend;
    property MaxPointsPerPage;
    property Monochrome;
    property Page;
    property RightAxis;
    property ScaleLastPage;
    property SeriesList;
    property TopAxis;
    property View3D;
    property View3DOptions;
    property View3DWalls;

    { TCustomAxisPanel events }
    property OnAfterDraw;
    property OnBeforeDrawAxes;
    property OnBeforeDrawSeries;
    property OnGetAxisLabel;
    property OnGetLegendText;
    property OnGetNextAxisLabel;
    property OnPageChange;

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

implementation

Uses TeeConst;

type
  TDBChartDataSource=class(TDataSource)
  private
    FWasActive:Boolean;
    Procedure SetDataSet(Value:TDataSet);
  end;

Procedure TDBChartDataSource.SetDataSet(Value:TDataSet);
Begin
  DataSet:=Value;
  FWasActive:=DataSet.Active;
end;

procedure TListOfDataSources.SetDataSource(Index:Integer; DataSource:TDataSource);
begin
  inherited Items[Index]:=DataSource;
end;

function TListOfDataSources.GetDataSource(Index:Integer):TDataSource;
begin
  result:=TDataSource(inherited Items[Index]);
end;

{ TDBChart }
Constructor TCustomDBChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IDataSources:=TListOfDataSources.Create;
  FAutoRefresh:=True;
  FShowGlassCursor:=True;
  FRefreshInterval:=0;
  ITimer:=nil;
  IUpdating:=False;
end;

Destructor TCustomDBChart.Destroy;
var t:Longint;
begin
  ITimer.Free;
  for t:=0 to IDataSources.Count-1 do IDataSources.DataSource[t].Free;
  IDataSources.Free;
  inherited Destroy;
end;

procedure TCustomDBChart.RemovedDataSource( ASeries: TChartSeries;
                                            AComponent: TComponent );
var t:Longint;
    tmp:TDataSet;
begin
  inherited RemovedDataSource(ASeries,AComponent);
  if AComponent is TDataSet then
  for t:=0 to IDataSources.Count-1 do
  begin
    tmp:=IDataSources.DataSource[t].DataSet;
    if (not Assigned(tmp)) or (tmp=AComponent) then
    begin
      IDataSources.DataSource[t].Free;
      IDataSources.Delete(t);
      break;
    end;
  end;
end;

Procedure TCustomDBChart.CheckTimer;
Begin
  if Assigned(ITimer) then ITimer.Enabled:=False;
  if (FRefreshInterval>0) and (not (csDesigning in ComponentState) ) then
  Begin
    if not Assigned(ITimer) then
    Begin
      ITimer:=TTimer.Create(Self);
      ITimer.Enabled:=False;
      ITimer.OnTimer:=OnRefreshTimer;
    end;
    ITimer.Interval:=FRefreshInterval*1000;
    ITimer.Enabled:=True;
  end;
End;

Procedure TCustomDBChart.OnRefreshTimer(Sender:TObject);
var t:Longint;
Begin
  ITimer.Enabled:=False;  { no try..finally here ! }
  for t:=0 to IDataSources.Count-1 do
  With IDataSources.DataSource[t] do
  Begin
    if DataSet.Active then
    Begin
    {$IFNDEF D3}
      if DataSet is TQuery then
      begin
        DataSet.Close;
        DataSet.Open;
      end
      else
    {$ENDIF}
        DataSet.Refresh;
      CheckDataSet(DataSet,nil);
    end;
  end;
  ITimer.Enabled:=True;
end;

Procedure TCustomDBChart.SetRefreshInterval(Value:Longint);
Begin
  if (Value<0) or (Value>60) then
     Raise DBChartException.Create(TeeMsg_RefreshInterval);
  FRefreshInterval:=Value;
  CheckTimer;
End;

Function TCustomDBChart.IsValidDataSource(ASeries:TChartSeries; AComponent:TComponent):Boolean;
Begin
  result:=inherited IsValidDataSource(ASeries,AComponent);
  if not Result then result:=(AComponent is TDataSet) or (AComponent is TDataSource);
end;

Procedure TCustomDBChart.DataSourceRowChange(Sender:TObject; Field:TField);
begin
  if not IUpdating then
  With TDBChartDataSource(Sender) do CheckDataSet(DataSet,nil);
end;

Procedure TCustomDBChart.DataSourceUpdateData(Sender:TObject);
Begin
  With TDBChartDataSource(Sender) do
  if State=dsBrowse then CheckDataSet(DataSet,nil)
                    else FWasActive:=False;
End;

Procedure TCustomDBChart.DataSourceStateChange(Sender:TObject);
var t:Longint;
Begin
  With TDBChartDataSource(Sender) do
  if State=dsInactive then
  Begin
    FWasActive:=False;
    if FAutoRefresh then
       for t:=0 to SeriesCount-1 do
           if Series[t].DataSource=DataSet then Series[t].Clear;
  end
  else
  if (State=dsBrowse) and (not FWasActive) then
  Begin
    CheckDataSet(DataSet,nil);
    FWasActive:=True;
  end;
end;

Procedure TCustomDBChart.CheckNewDataSource(ADataSet:TDataSet; SingleRow:Boolean);
Var tmpDataSource:TDBChartDataSource;
Begin
  if IDataSources.IndexOf(ADataSet)<>-1 then Exit;
  tmpDataSource:=TDBChartDataSource.Create(Self);
  With tmpDataSource do
  begin
    SetDataSet(ADataSet);
    OnStateChange := DataSourceStateChange;
    OnUpdateData  := DataSourceUpdateData;
    if SingleRow then OnDataChange  := DataSourceRowChange;
  end;
  IDataSources.Add(tmpDataSource);
end;

Procedure TCustomDBChart.CheckDatasource(ASeries:TChartSeries);
Begin
  if Assigned(ASeries) then
  With ASeries do
  Begin
    if ParentChart=Self then
    Begin
      if DataSource<>nil then
      Begin
        ASeries.Clear;
        if DataSource is TDataSet then
        Begin
          CheckNewDataSource(TDataSet(DataSource),False);
          CheckDataSet(TDataSet(DataSource),ASeries);
        end
        else
        if DataSource is TDataSource then
        begin
          CheckNewDataSource(TDataSource(DataSource).DataSet,True);
          CheckDataSet(TDataSource(DataSource).DataSet,ASeries);
        end
        else inherited CheckDataSource(ASeries);
      end;
    end
    else Raise ChartException.Create(TeeMsg_SeriesParentNoSelf);
  end;
end;

Procedure TCustomDBChart.CheckDataSet(ADataSet:TDataSet; ASeries:TChartSeries);
Begin
  if FAutoRefresh then RefreshDataSet(ADataSet,ASeries);
end;

Procedure TCustomDBChart.RefreshDataSet( ADataSet:TDataSet; ASeries:TChartSeries );
Var HasAnyDataSet:Boolean;

  Procedure ProcessRecord(tmpSeries:TChartSeries);
  var tmpxLabel:String;
      tmpColor:TColor;
      tmpX,tmpY:Double;

      Procedure AddToSeries(DestSeries:TChartSeries);
      Var t,tmpIndex:Longint;
      begin
        With DestSeries do
        begin
          With ValuesLists do
          for t:=2 to Count-1 do
          Begin
            With ValueList[t] do
            if ValueSource<>'' then TempValue:=ADataSet.FieldByName(ValueSource).AsFloat
                               else ClearTempValue(ValueList[t]);
          end;
          if YMandatory then
          Begin
            if XValues.ValueSource<>'' then
               tmpIndex:=AddXY(tmpX,tmpY,tmpxLabel,tmpColor)
            else
               tmpIndex:=AddY(tmpY,tmpXLabel,tmpColor);
          end
          else
          Begin
            if YValues.ValueSource<>'' then
               tmpIndex:=AddXY(tmpX,tmpY,tmpxLabel,tmpColor)
            else
               tmpIndex:=AddX(tmpX,tmpXLabel,tmpColor);
          end;
          if tmpIndex<>-1 then AddValue(tmpIndex);
        end;
      end;

      Procedure GetFloatField(Const FieldName:String; Var res:Double);
      begin
        if FieldName='' then res:=0
                        else res:=ADataSet.FieldByName(FieldName).AsFloat
      end;

  var t,tmpNumFields:Integer;
      tmpFieldName:String;
  Begin
    With ADataSet,tmpSeries do
    Begin
      if XLabelsSource<>'' then tmpxLabel:=FieldByName(XLabelsSource).DisplayText { <-- fix }
                           else tmpxLabel:='';

      if ColorSource='' then
      begin
        if (MandatoryValueList.ValueSource<>'') and
           FieldByName(TeeExtractField(MandatoryValueList.ValueSource,1)).IsNull then
           tmpColor:=clNone
        else
           tmpColor:=clTeeColor;
      end
      else tmpColor:=FieldByName(ColorSource).AsInteger;

      tmpNumFields:=TeeNumFields(MandatoryValueList.ValueSource);
      if tmpNumFields=1 then
      begin
        tmpFieldName:=MandatoryValueList.ValueSource;
        if (not HasAnyDataSet) and (XLabelsSource='') then tmpXLabel:=tmpFieldName;
        if YMandatory then
        begin
          GetFloatField(XValues.ValueSource, tmpX);
          GetFloatField(tmpFieldName, tmpY);
        end
        else
        begin
          GetFloatField(tmpFieldName, tmpX);
          GetFloatField(YValues.ValueSource, tmpY);
        end;
        AddToSeries(tmpSeries);
      end
      else
      for t:=1 to tmpNumFields do
      begin
        tmpFieldName:=TeeExtractField(MandatoryValueList.ValueSource,t);
        if XLabelsSource='' then tmpXLabel:=tmpFieldName;
        if YMandatory then GetFloatField(tmpFieldName, tmpY)
                      else GetFloatField(tmpFieldName, tmpX);
        AddToSeries(tmpSeries);
      end;
    end;
  end;

Var FListSeries:TChartSeriesList;

  Procedure FillTempSeriesList;

   Function IsDataSet(ASeries:TChartSeries):Boolean;
   begin
     With ASeries do
     if DataSource is TDataSet then
     begin
       HasAnyDataSet:=True;
       result:=DataSource=ADataSet
     end
     else
     if DataSource is TDataSource then result:=TDataSource(DataSource).DataSet=ADataSet
     else
        result:=False;
   end;

  var t:Longint;
    tmpSeries:TChartSeries;
  begin
    FListSeries.Clear;
    if Assigned(ASeries) then
    begin
      FListSeries.Add(ASeries);
      HasAnyDataSet:=ASeries.DataSource=ADataSet;
    end
    else
    for t:=0 to SeriesCount-1 do
    Begin
      tmpSeries:=Series[t];
      if IsDataSet(tmpSeries) and
         (tmpSeries.MandatoryValueList.ValueSource<>'') then
             FListSeries.Add(tmpSeries);
    end;
  end;

  Procedure TraverseDataSet;
  Var b:TBookMark;
      t:Integer;
  begin
    With ADataSet do
    begin
      DisableControls;
      try
        b:=GetBookMark;
        try
          First;
          While not EOF do
          try
            if Assigned(FOnProcessRecord) then FOnProcessRecord(Self,ADataSet);
            for t:=0 to FListSeries.Count-1 do
                if FListSeries.Series[t].DataSource=ADataSet then
                   ProcessRecord(FListSeries[t]);
            Next;
          except
            on EAbort do break; { <-- exit while loop !!! }
          end;
        finally
          try
            try
              GotoBookMark(b);
            except
            {$IFNDEF D3}
              on E:EDBEngineError do
              if E.ErrorCount>0 then
              begin
                if E.Errors[0].ErrorCode<>DBIERR_KEYORRECDELETED then
                   raise;
              end
              else
            {$ENDIF}
                 raise;
            end;
          finally
            FreeBookMark(b);
          end;
        end;
      finally
        EnableControls;
      end;
    end;
  end;

Var OldCursor:TCursor;
    t:Integer;
Begin
  if not IUpdating then
  With ADataSet do
  Begin
    if Active then
    Begin
      IUpdating:=True;
      FListSeries:=TChartSeriesList.Create;
      HasAnyDataSet:=False;
      try
        FillTempSeriesList;
        if FListSeries.Count>0 then
        Begin
          OldCursor:=Screen.Cursor;
          if FShowGlassCursor then Screen.Cursor:=crHourGlass;
          try
            for t:=0 to FListSeries.Count-1 do TChartSeries(FListSeries[t]).Clear;
            if HasAnyDataSet then TraverseDataSet
            else
            begin
              if Assigned(FOnProcessRecord) then FOnProcessRecord(Self,ADataSet);
              for t:=0 to FListSeries.Count-1 do
                  if TDataSource(FListSeries.Series[t].DataSource).DataSet=ADataSet then
                     ProcessRecord(FListSeries[t]);
            end;
            for t:=0 to FListSeries.Count-1 do TChartSeries(FListSeries[t]).RefreshSeries;
          finally
            if FShowGlassCursor then Screen.Cursor:=OldCursor;
          end;
        end;
      finally
        FListSeries.Free;
        IUpdating:=False;
      end;
    end;
  end;
end;

Procedure TCustomDBChart.RefreshData;
var t:Longint;
Begin
  for t:=0 to IDataSources.Count-1 do
      RefreshDataSet(IDataSources.DataSource[t].DataSet,nil);
End;

Procedure TCustomDBChart.FillSeriesSourceItems(ASeries:TChartSeries; Proc:TGetStrProc);
Var t:Integer;
    tmpDataSet:TDataSet;
Begin
  tmpDataSet:=nil;
  With ASeries do
  if DataSource<>nil then
  begin
    if DataSource is TDataSource then tmpDataSet:=TDataSource(DataSource).DataSet
    else
    if DataSource is TDataSet then tmpDataSet:=TDataSet(DataSource);
    with tmpDataSet do
    Begin
      if FieldCount > 0 then
      Begin
        for t:=0 to FieldCount-1 do Proc(Fields[t].FieldName);
      end
      else
      Begin
        FieldDefs.Update;
        for t:=0 to FieldDefs.Count-1 do Proc(FieldDefs[t].Name);
      end;
   end;
  end;
end;

Procedure TCustomDBChart.FillValueSourceItems(AValueList:TChartValueList; Proc:TGetStrProc);
Begin
  With AValueList.Owner do
  if (DataSource<>nil) then
  Begin
    if (DataSource is TDataSet) or (DataSource is TDataSource) then
       FillSeriesSourceItems(AValueList.Owner,Proc)
    else
       inherited FillValueSourceItems(AValueList,Proc);
  end;
end;

Procedure TCustomDBChart.Assign(Source:TPersistent);
begin
  if Source is TCustomDBChart then
  With TCustomDBChart(Source) do
  begin
    Self.AutoRefresh    :=AutoRefresh;
    Self.RefreshInterval:=RefreshInterval;
    Self.ShowGlassCursor:=ShowGlassCursor;
  end;
  inherited Assign(Source);
end;

end.
