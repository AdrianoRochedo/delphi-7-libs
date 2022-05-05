{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiSeri;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Teengine;

type
  TTeeDataSourceStyle=( dsStyle_NoData ,
                        dsStyle_Random ,
                        dsStyle_Function,
                        dsStyle_Dataset,
                        dsStyle_DataSource  );

  {$IFNDEF D1}
  { special case for TFormDesigner in Charteg unit }
  TAddComponentDataSource=Procedure( Const AComponent:TComponent;
                                     AItems:TStrings;
                                     AddCurrent:Boolean) of object;

  { special case for TFormDesigner in Charteg unit }
  TOnGetDesignerNamesEvent=Procedure( AProc:TAddComponentDataSource;
                                      ASeries:TChartSeries;
                                      AItems:TStrings;
                                      AddCurrent:Boolean );
  {$ENDIF}

  TFormTeeSeries=class;

  TOnChartFillFields=Procedure(AForm:TFormTeeSeries);
  TOnChartGetSourceStyle=Function(ASeries:TChartSeries):TTeeDataSourceStyle;
  TOnChartIsDateTimeSource=Procedure( AComponent:TComponent;
                                      Const ValueSource:String;
                                      Var IsDateTime:Boolean);

  TOnChartIsValidComponentSource=Function( AComponent:TComponent;
                                           DataSourceStyle:TTeeDataSourceStyle):Boolean;

  TFormTeeSeries = class(TForm)
    CBSeries: TComboBox;
    ImageSeries: TImage;
    CBPersistent: TCheckBox;
    PageSeries: TPageControl;
    TheTabSheet: TTabSheet;
    TabGeneral: TTabSheet;
    GB5: TGroupBox;
    L27: TLabel;
    CBShowInLegend: TCheckBox;
    CBSeriesCursor: TComboBox;
    GB2: TGroupBox;
    L15: TLabel;
    L21: TLabel;
    EValueformat: TEdit;
    EPercentFormat: TEdit;
    RGHorizAxis: TRadioGroup;
    RGVertAxis: TRadioGroup;
    CBXDateTime: TCheckBox;
    CBYDateTime: TCheckBox;
    TabMarks: TTabSheet;
    CBMarksVisible: TCheckBox;
    GB3: TGroupBox;
    L32: TLabel;
    SEArrowLength: TEdit;
    BMarkLinCol: TButton;
    RGMarkStyle: TRadioGroup;
    GB11: TGroupBox;
    BMarksBackColor: TButton;
    CBTransparent: TCheckBox;
    BMarkFont: TButton;
    BMarksFrame: TButton;
    CBMarkClip: TCheckBox;
    TabDataSource: TTabSheet;
    CBDataSourcestyle: TComboBox;
    PCData: TPageControl;
    TabDatabase: TTabSheet;
    L16: TLabel;
    CBListDataSets: TComboBox;
    TabFunctions: TTabSheet;
    L34: TLabel;
    CBFunctions: TComboBox;
    GB9: TGroupBox;
    L22: TLabel;
    L24: TLabel;
    LBAvailSeries: TListBox;
    LBSelectedSeries: TListBox;
    BRightOne: TButton;
    BRightAll: TButton;
    BLeftOne: TButton;
    BLeftAll: TButton;
    BEditFunc: TButton;
    LabelSeriesClass: TLabel;
    PCDataStyle: TPageControl;
    TBDataSet: TTabSheet;
    TBDataSource: TTabSheet;
    GroupFields: TScrollBox;
    LabelLabels: TLabel;
    CBLabelsField: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    LBAvailFields: TListBox;
    LBSelFields: TListBox;
    BROneDS: TButton;
    BRAllDS: TButton;
    BLOneDS: TButton;
    BLAllDS: TButton;
    procedure RGHorizAxisClick(Sender: TObject);
    procedure BMarksBackColorClick(Sender: TObject);
    procedure EValueformatChange(Sender: TObject);
    procedure EPercentFormatChange(Sender: TObject);
    procedure PageSeriesChange(Sender: TObject);
    procedure CBSeriesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGMarkStyleClick(Sender: TObject);
    procedure BEditFuncClick(Sender: TObject);
    procedure CBPersistentClick(Sender: TObject);
    procedure CBYDateTimeClick(Sender: TObject);
    procedure CBXDateTimeClick(Sender: TObject);
    procedure CBDataSourcestyleChange(Sender: TObject);
    procedure CBFunctionsChange(Sender: TObject);
    procedure LBSelectedSeriesDblClick(Sender: TObject);
    procedure LBAvailSeriesDblClick(Sender: TObject);
    procedure BLeftAllClick(Sender: TObject);
    procedure BRightAllClick(Sender: TObject);
    procedure BLeftOneClick(Sender: TObject);
    procedure BRightOneClick(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
    procedure CBListDataSetsChange(Sender: TObject);
    procedure CBShowInLegendClick(Sender: TObject);
    procedure RGVertAxisClick(Sender: TObject);
    procedure CBSeriesCursorChange(Sender: TObject);
    procedure BMarkLinColClick(Sender: TObject);
    procedure BMarksFrameClick(Sender: TObject);
    procedure BMarkFontClick(Sender: TObject);
    procedure SEArrowLengthChange(Sender: TObject);
    procedure CBMarkClipClick(Sender: TObject);
    procedure CBTransparentClick(Sender: TObject);
    procedure CBMarksVisibleClick(Sender: TObject);
    procedure PageSeriesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    TheSeriesForms       : Array[1..4] of TForm;
    NumSeriesForms       : Integer;
    ApplyChangesFunction : Boolean;
    ApplyChangesDataSet  : Boolean;
    CreatingForm         : Boolean;
    FromList             : TListBox;
    ToList               : TListBox;
    Procedure AddSeriesForm(AForm:TForm; ATab:TTabSheet; ATag:Longint);
    Procedure AddComponentDataSource( Const AComponent:TComponent;
                                      AItems:TStrings;
                                      AddCurrent:Boolean);
    procedure CBValuesChange(Sender: TObject);
    procedure CBDateTimeClick(Sender: TObject);
    Procedure ChangeApply;
    procedure CheckApplyChanges;
    procedure DestroySeriesForms;
    Function DataSourceStyle:TTeeDataSourceStyle;
    procedure EnableDataSourceControls(IsDatabase,IsFunction:Boolean);
    procedure EnableListButtons;
    Procedure FillFields;  { virtual }
    procedure FillSourceSeries;
    Procedure FillSources(AItems:TStrings; AddCurrent:Boolean);
    Procedure IsDateTimeSource( AComponent:TComponent;
                                Const ValueSource:String;
                                Var IsDateTime:Boolean);
    Function IsValidComponentSource(AComponent:TComponent):Boolean;
    procedure ProcGetCursors(const S: string);
    procedure SetCurrentDataSource;
    procedure SetSelectedSourceDatabase;
    procedure SetSelectedSourceSeries;
    procedure SetTabSeriesDataSource;
    procedure SetTabSeriesFunctions;
    Procedure SetTextItemIndex(Combo:TComboBox);
  public
    { Public declarations }
    TheSeries:TChartSeries;
    IsDssGraph:Boolean;
    ShowTabDataSource:Boolean;
    ShowTabGeneral:Boolean;
    ShowTabMarks:Boolean;
    OnGetDesignerNames:TOnGetDesignerNamesEvent;
    OnChartFillFields:TOnChartFillFields;
    OnChartGetSourceStyle:TOnChartGetSourceStyle;
    OnChartIsDateTimeSource:TOnChartIsDateTimeSource;
    OnChartIsValidComponentSource:TOnChartIsValidComponentSource;
    Function GetSourceCombo(Index:Integer):TComboBox;
    Function GetSelectedSource:TComponent;
    Procedure SetCBSeries;
    Function InsertSeriesForm( EditorClass:TFormClass;
                               Position:Integer;
                               Const EditorTabName:String;
                               AnObject:TPersistent):TForm;
    { For ax }
    procedure DoApplyChangesDataset;
    procedure DoApplyChangesFunction;
    procedure FillSourceDatasets;
  end;

implementation

{$R *.DFM}
Uses TeeConst,Chart,PenDlg,TeeProcs,TeCanvas,TeeGally,TeeLisB,BrushDlg,
     IEdiPeri, Series {<-because TPointSeries editor trick} ;

Const MaxValueSources=16;

type
  TChartEditSource=class
  public
    SelectedValueSource:String;
    CBValues:TComboBox;
    LabelValues:TLabel;
    CBDateTime:TCheckBox;
    Destructor Destroy; override;
  end;

  TChartEditSources=class
  public
    ParentForm:TForm;
    SelectedDataSource:TComponent;
    TheSeries:TChartSeries;
    SelectedXLabelsSource:String;
    Values:Array[0..MaxValueSources-1] of TChartEditSource;
    Constructor Create(AParent:TForm; ASeries:TChartSeries);
    Destructor Destroy; override;
    procedure ClearValueSources;
    procedure SetSeries(TheSeries:TChartSeries);
    procedure ClearCombos(EnableCombos:Boolean);
    procedure SetSeriesDatabaseSource(TheSeries:TChartSeries);
    procedure ClearSources;
  end;

Var TheSource:TChartEditSources;

{ Helper functions }
procedure FillTeeFunctions(AList:TStrings);
var t:Longint;
begin
  AList.Clear;
  AList.Add(TeeMsg_FunctionNone);
  With TeeSeriesTypes do
  for t:=0 to Count-1 do
    With SeriesType[t] do
    if Assigned(FunctionClass) and
       (not Assigned(SeriesClass)) and
       ( AList.IndexOfObject(TObject(FunctionClass))=-1) then
          AList.InsertObject(1,Description,TObject(FunctionClass));
end;

{ TChartEditSource }
Destructor TChartEditSource.Destroy;
begin
  CBValues.Free;
  CBValues:=nil;
  LabelValues.Free;
  LabelValues:=nil;
  CBDateTime.Free;
  CBDateTime:=nil;
  SelectedValueSource:='';
  inherited Destroy;
end;

{ TChartEditSources }
Constructor TChartEditSources.Create(AParent:TForm; ASeries:TChartSeries);
var t:Longint;
begin
  inherited Create;
  ParentForm:=AParent;
  TheSeries:=ASeries;
  for t:=0 to MaxValueSources-1 do Values[t]:=TChartEditSource.Create;
  ClearValueSources;
end;

procedure TChartEditSources.ClearSources;
var t:Longint;
begin
  SelectedXLabelsSource:='';
  for t:=0 to TheSeries.ValuesLists.Count-1 do
  With Self.Values[t] do
  begin
    CBValues.ItemIndex:=-1;
    SelectedValueSource:='';
  end;
end;

procedure TChartEditSources.SetSeriesDatabaseSource(TheSeries:TChartSeries);
var t:Longint;
begin
  for t:=0 to TheSeries.ValuesLists.Count-1 do
  With Self.Values[t] do
  begin
    CBValues.ItemIndex:=CBValues.Items.IndexOf(SelectedValueSource);
    CBDateTime.Checked:=TheSeries.ValuesLists[t].DateTime;
  end;
end;

procedure TChartEditSources.ClearCombos(EnableCombos:Boolean);
var t:Longint;
begin
  for t:=0 to TheSeries.ValuesLists.Count-1 do
  With Self.Values[t] do
  begin
    CBValues.Items.Clear;
    CBValues.Enabled:=EnableCombos;
  end;
end;

procedure TChartEditSources.SetSeries(TheSeries:TChartSeries);
var t:Longint;
begin
  With TheSeries do
  begin
    SelectedDataSource:=DataSource;
    SelectedXLabelsSource:=XLabelsSource;
    for t:=0 to MaxValueSources-1 do
    begin
      Values[t].Free;
      Values[t]:=TChartEditSource.Create;
    end;
    for t:=0 to ValuesLists.Count-1 do
        Values[t].SelectedValueSource:=ValuesLists[t].ValueSource;
  end;
end;

procedure TChartEditSources.ClearValueSources;
var t:Longint;
begin
  SelectedDataSource:=nil;
  for t:=0 to MaxValueSources-1 do Values[t].SelectedValueSource:='';
  SelectedXLabelsSource:='';
end;

Destructor TChartEditSources.Destroy;
var t:Longint;
begin
  for t:=0 to MaxValueSources-1 do Values[t].Free;
  inherited Destroy;
end;

{ TFormTeeSeries}
procedure TFormTeeSeries.RGHorizAxisClick(Sender: TObject);
begin
  TheSeries.HorizAxis:=THorizAxis(RGHorizAxis.ItemIndex);
end;

procedure TFormTeeSeries.BMarksBackColorClick(Sender: TObject);
begin
  With TheSeries.Marks do BackColor:=EditColor(Self,BackColor);
  CBTransparent.Checked:=False;
end;

procedure TFormTeeSeries.EValueformatChange(Sender: TObject);
begin
  TheSeries.ValueFormat:=LocalToDelphiFormat(EValueFormat.Text);
end;

procedure TFormTeeSeries.EPercentFormatChange(Sender: TObject);
begin
  TheSeries.PercentFormat:=LocalToDelphiFormat(EPercentFormat.Text);
end;

Procedure TFormTeeSeries.SetCBSeries;
begin
  TheSource.TheSeries:=TheSeries;
  if Assigned(TheSeries) then
  begin
    CBSeries.ItemIndex:=CBSeries.Items.IndexOfObject(TheSeries);
    CBSeriesChange(Self);
  end
  else PageSeries.Visible:=False;
end;

Const TeeCursorPrefix='cr';

Function DeleteCursorPrefix(Const S:String):String;
begin
  result:=S;
  if Copy(result,1,2)=TeeCursorPrefix then Delete(result,1,2);
end;

procedure TFormTeeSeries.ProcGetCursors(const S: string);
begin
  CBSeriesCursor.Items.Add(DeleteCursorPrefix(S));
end;

procedure TFormTeeSeries.PageSeriesChange(Sender: TObject);

  procedure SetTabSeriesMarks;
  begin
    With TheSeries.Marks do
    begin
      RGMarkStyle.ItemIndex  :=Ord(Style);
      CBMarksVisible.Checked :=Visible;
      SEArrowLength.Text     :=IntToStr(ArrowLength);
      CBTransparent.Checked  :=Transparent;
      CBMarkClip.Checked     :=Clip;
    end;
  end;

  procedure SetTabSeriesGeneral;
  var tmpSt:String;
  begin
    With TheSeries do
    begin
      CBShowInLegend.Checked   :=ShowInLegend;
      EValueFormat.Text        :=DelphiToLocalFormat(ValueFormat);
      EPercentFormat.Text      :=DelphiToLocalFormat(PercentFormat);
      With RGHorizAxis do
      begin
        Enabled:=TheSeries.HorizAxis<>aCustomHorizAxis;
        if Enabled then ItemIndex:=Ord(TheSeries.HorizAxis);
      end;
      With RGVertAxis do
      begin
        Enabled:=TheSeries.VertAxis<>aCustomVertAxis;
        if Enabled then ItemIndex:=Ord(TheSeries.VertAxis);
      end;
      CBXDateTime.Checked:=XValues.DateTime;
      CBYDateTime.Checked:=YValues.DateTime;
    end;
    With CBSeriesCursor do
    begin
      Items.BeginUpdate;
      Clear;
      GetCursorValues(ProcGetCursors);
      ProcGetCursors(TeeMsg_TeeHand);
      Items.EndUpdate;
    end;
    With CBSeriesCursor do
    if TeeCursorToIdent(TheSeries.Cursor,tmpSt) then
       ItemIndex:=Items.IndexOf(DeleteCursorPrefix(tmpSt))
    else
       ItemIndex:=-1;
  end;

begin
  With PageSeries.ActivePage do
  if PageIndex=TabGeneral.PageIndex then SetTabSeriesGeneral else
  if PageIndex=TabMarks.PageIndex   then SetTabSeriesMarks   else
  if PageIndex=TabDataSource.PageIndex then SetTabSeriesDataSource;
end;

Procedure TFormTeeSeries.AddSeriesForm(AForm:TForm; ATab:TTabSheet; ATag:Longint);
var OldVisibleFlag: Boolean;
begin
  With AForm do
  begin
    Position:=poDesigned;
    BorderStyle:=bsNone;
    BorderIcons:=[];
    Tag:=ATag;
    Parent:=ATab;
    Left:=((ATab.PageControl.ClientWidth-ClientWidth) div 2);
    Top:=MinLong(8,Abs(ATab.PageControl.ClientHeight-ClientHeight) div 2);
    OldVisibleFlag:=Parent.Visible;
    Parent.Visible:=True;
    Show;
    {$IFDEF D2C1}
    ClientWidth:=ATab.PageControl.ClientWidth;
    {$ENDIF}
    Parent.Visible:=OldVisibleFlag;
  end;
end;

Function TFormTeeSeries.InsertSeriesForm( EditorClass:TFormClass;
                                          Position:Integer;
                                          Const EditorTabName:String;
                                          AnObject:TPersistent):TForm;
var tmpPage  : TTabSheet;
begin
  tmpPage:=TTabSheet.Create(Self);
  With tmpPage do
  begin
    PageControl:=PageSeries;
    PageIndex:=Position;
    Caption:=EditorTabName;
  end;
  result:=EditorClass.Create(Self);
  Inc(NumSeriesForms);
  TheSeriesForms[NumSeriesForms]:=result;
  AddSeriesForm(result,tmpPage,Longint(AnObject));
end;

procedure TFormTeeSeries.DestroySeriesForms;
var t       : Integer;
    tmpPage : TTabSheet;
begin
  for t:=1 to NumSeriesForms do
  if Assigned(TheSeriesForms[t]) then
  begin
    tmpPage:=(TheSeriesForms[t].Parent as TTabSheet);
    TheSeriesForms[t].Free;
    TheSeriesForms[t]:=nil;
    if tmpPage.PageIndex>0 then
       if not (csDestroying in tmpPage.PageControl.ComponentState) then
          tmpPage.Free;
  end;
  NumSeriesForms:=0;
end;

procedure TFormTeeSeries.CBSeriesChange(Sender: TObject);

    Procedure CreateTheSeriesForm;
    var tmpClass : TFormClass;
    begin
      tmpClass:=TFormClass(GetClass(TheSeries.GetEditorClass));
      if Assigned(tmpClass) then
      begin
        Inc(NumSeriesForms);
        TheSeriesForms[NumSeriesForms]:=tmpClass.Create(Self);
        AddSeriesForm(TheSeriesForms[NumSeriesForms],TheTabSheet,Longint(TheSeries));
      end
      else DestroySeriesForms;
    end;

    Procedure HideSeriesPage;
    begin
      ImageSeries.Visible:=False;
      LabelSeriesClass.Caption:='';
      PageSeries.Visible:=False;
    end;

var OldTab    : TTabSheet;
    tmpBitmap : TBitmap;
begin
  CreatingForm:=True;
  OldTab:=PageSeries.ActivePage;
  PageSeries.ActivePage:=TheTabSheet;
  CheckApplyChanges;
  With CBSeries do
  if ItemIndex<>-1 then
  begin
    TheSeries:=TChartSeries(CBSeries.Items.Objects[ItemIndex]);
    if Assigned(TheSeries) then
    begin
      tmpBitmap:=TBitmap.Create;
      try
        TheSeries.GetBitmapEditor(tmpBitmap);
        {$IFDEF D3}
        ImageSeries.Transparent:=True;
        {$ENDIF}
        ImageSeries.Picture.Assign(tmpBitmap);
      finally
        tmpBitmap.Free;
      end;
      LabelSeriesClass.Caption:=GetGallerySeriesName(TheSeries)+': '+TheSeries.Name; { <-- don't translate }
      ImageSeries.Visible:=True;
      PageSeries.Visible:=True;
      TabGeneral.TabVisible:=ShowTabGeneral;
      TabMarks.TabVisible:=ShowTabMarks;
      {$IFDEF D3}
      TabDataSource.TabVisible:=ShowTabDataSource and
                                ( (not (tssIsTemplate in TheSeries.Style)) and
                                  (not (tssHideDataSource in TheSeries.Style)) );
      {$ENDIF}
      if (OldTab=nil) or
         ((OldTab=TabDataSource) and (not TabDataSource.TabVisible)) then
         PageSeries.ActivePage:=TheTabSheet
      else
         PageSeries.ActivePage:=OldTab;
      PageSeriesChange(Self);
      ShowControls(TheSeries.UseAxis,[ RGHorizAxis,RGVertAxis,
                                       CBXDateTime,CBYDateTime]);
      DestroySeriesForms;
      CreateTheSeriesForm;
      if PageSeries.ActivePage=nil then PageSeries.ActivePage:=TheTabSheet;
      SetTabSeriesDataSource;
      {$IFDEF D3}
      CBPersistent.Visible:=IsDssGraph and (not (tssIsTemplate in TheSeries.Style));
      if CBPersistent.Visible then
         CBPersistent.Checked:=(tssIsPersistent in TheSeries.Style);
      {$ENDIF}
    end
    else HideSeriesPage;
  end
  else HideSeriesPage;
  CreatingForm:=False;
end;

{ special case at design time }
Function TFormTeeSeries.IsValidComponentSource(AComponent:TComponent):Boolean;
begin
  result:= (CBDataSourceStyle.ItemIndex=Ord(dsStyle_Function)) and
           (AComponent is TChartSeries);
  if not result then
     if CBDataSourceStyle.ItemIndex>Ord(dsStyle_Function) then
        if Assigned(OnChartIsValidComponentSource) then
           result:=OnChartIsValidComponentSource(AComponent,
                 TTeeDataSourceStyle(CBDataSourceStyle.ItemIndex));
end;

Procedure TFormTeeSeries.AddComponentDataSource( Const AComponent:TComponent;
                                                 AItems:TStrings;
                                                 AddCurrent:Boolean);
Var tmp,tmpFormName:String;
begin
  if AddCurrent or (TheSeries.DataSources.IndexOf(AComponent)=-1) then
  if IsValidComponentSource(AComponent) then
     if TheSeries.ParentChart.IsValidDataSource(TheSeries,AComponent) then
     begin
       if AComponent is TChartSeries then
       With TChartSeries(AComponent) do
       begin
         if Title<>'' then tmp:=Title
                      else tmp:=Name;
       end
       else
       begin
         tmp:=AComponent.Name;
         if Pos('ODBC',tmp)=1 then tmp:=pString(pointer(AComponent.Tag))^;
       end;
       if (TheSeries.Owner<>AComponent.Owner) and
          (AComponent.Owner<>nil) then
       begin
         tmpFormName:=AComponent.Owner.Name;
         if tmpFormName<>'' then tmp:=tmpFormName+'.'+tmp;
       end;
       AItems.AddObject(tmp,AComponent);
     end;
end;

Procedure TFormTeeSeries.FillSources(AItems:TStrings; AddCurrent:Boolean);

  Procedure FillSourcesForm(AOwner:TComponent);
  var t:Longint;
  begin
    if Assigned(AOwner) then
    With AOwner do
    for t:=0 to ComponentCount-1 do
        AddComponentDataSource(Components[t],AItems,AddCurrent);
  end;

var t:Longint;
begin
  if (csDesigning in TheSeries.ComponentState) and
     Assigned(OnGetDesignerNames) then
        OnGetDesignerNames(AddComponentDataSource,TheSeries,AItems,AddCurrent)
  else
  begin
    With Screen do
    for t:=0 to DataModuleCount-1 do FillSourcesForm(DataModules[t]);
    FillSourcesForm(TheSeries.ParentChart.Owner);
    if DataSourceStyle<>dsStyle_DataSource then
       FillSourcesForm(TheSeries.ParentChart);
  end;
end;

procedure TFormTeeSeries.FillSourceDatasets;
begin
  CBListDataSets.Items.Clear;
  FillSources(CBListDataSets.Items,True);
  SetCurrentDataSource;
  ApplyChangesDataSet:=True;
end;

procedure TFormTeeSeries.EnableDataSourceControls(IsDatabase,IsFunction:Boolean);

  procedure SetTabSeriesDatabase;
  var t       : Longint;
      tmpName : String;
  begin
    L16.Caption:=TeeMsg_AskDataSet;
    GroupFields.Visible:=False;
    FillSourceDatasets;
    TheSource.SetSeries(TheSeries);
    With TheSeries,TheSource do
    begin
      for t:=0 to ValuesLists.Count-1 do
      With TheSource.Values[t] do
      begin
        tmpName:=ValuesLists[t].Name;
        CBValues:=TComboBox.Create(ParentForm);
        With CBValues do
        begin
          Parent:=GroupFields;
          Left:=CBLabelsField.Left;
          Style:=csDropDown;
          HelpContext:=178;
          Width:=CBLabelsField.Width;
          Top:=2+CBLabelsField.Top+CBLabelsField.Height+((CBValues.Height+4)*t+1);
          OnChange:=CBValuesChange;
          Tag:=t;
          Visible:=tmpName<>'';
        end;
        LabelValues:=TLabel.Create(ParentForm);
        With LabelValues do
        begin
          Alignment:=taRightJustify;
          Parent:=GroupFields;
          Top:=CBValues.Top+4;
          AutoSize:=False;
          Left:=LabelLabels.Left;
          Width:=LabelLabels.Width;
          Caption:=tmpName+':';
          Visible:=tmpName<>'';
        end;
        CBDateTime:=TCheckBox.Create(ParentForm);
        With CBDateTime do
        begin
          Parent:=GroupFields;
          Left:=CBLabelsField.Left+CBLabelsField.Width+6;
          Top:=CBValues.Top;
          HelpContext:=178;
          Caption:=TeeMsg_DateTime;
          Width:=Canvas.TextWidth(Caption + 'www'); { <-- ugly... }
          Tag:=t;
          Visible:=tmpName<>'';
          OnClick:=CBDateTimeClick;
        end;
        SelectedValueSource:=ValuesLists[t].ValueSource;
      end;
    end;
    TheSource.SetSeriesDatabaseSource(TheSeries);
    SetCurrentDataSource;
    SetSelectedSourceDatabase;
    GroupFields.Visible:=True;
    PCDataStyle.ActivePage:=TBDataSet;
    ApplyChangesDataSet:=False;
  end;

  procedure SetTabDataSource;
  begin
    L16.Caption:=TeeMsg_AskDataSource;
    FillSourceDatasets;
    TheSource.SetSeries(TheSeries);
    SetCurrentDataSource;
    SetSelectedSourceDatabase;
    PCDataStyle.ActivePage:=TBDataSource;
    FromList:=LBAvailFields;
    ToList:=LBSelFields;
    EnableListButtons;
    ApplyChangesDataSet:=False;
  end;

Var tmp:Boolean;
begin
  tmp:=False;
  Case DataSourceStyle of
    dsStyle_DataSet: begin
                       SetTabSeriesDatabase;
                       PCData.ActivePage:=TabDatabase;
                       tmp:=True;
                     end;
    dsStyle_DataSource: begin
                       SetTabDataSource;
                       PCData.ActivePage:=TabDatabase;
                       tmp:=True;
                     end;
    dsStyle_Function: begin
                       SetTabSeriesFunctions;
                       PCData.ActivePage:=TabFunctions;
                       tmp:=True;
                     end;
  end;
  PCData.Visible:=tmp;
end;

procedure TFormTeeSeries.SetTabSeriesDataSource;
Var Old:Boolean;
begin
  Old:=TabDataSource.Visible;
  TabDataSource.Visible:=False;
  if TheSeries.DataSource=nil then
  begin
    if TheSeries.FunctionType<>nil then
    begin
      CBDataSourceStyle.ItemIndex:=Ord(dsStyle_Function);
      EnableDataSourceControls(False,True);
    end
    else
    begin
      if TheSeries.Count>0 then
         CBDataSourceStyle.ItemIndex:=Ord(dsStyle_Random)
      else
         CBDataSourceStyle.ItemIndex:=Ord(dsStyle_NoData);
      EnableDataSourceControls(False,False);
    end;
  end
  else
  if TheSeries.DataSource is TChartSeries then
  begin
    CBDataSourceStyle.ItemIndex:=Ord(dsStyle_Function);
    EnableDataSourceControls(False,True);
  end
  else
  begin
    if Assigned(OnChartGetSourceStyle) then { 4.01 }
       CBDataSourceStyle.ItemIndex:=Ord(OnChartGetSourceStyle(TheSeries))
    else
       CBDataSourceStyle.ItemIndex:=0;
    EnableDataSourceControls(True,False);
  end;
  TabDataSource.Visible:=Old;
end;

procedure TFormTeeSeries.CheckApplyChanges;
begin
  if ApplyChangesDataSet then DoApplyChangesDataSet
  else
  if ApplyChangesFunction then DoApplyChangesFunction;
end;

procedure TFormTeeSeries.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  NumSeriesForms:=0;
  ShowTabDataSource:=True;
  ShowTabGeneral:=True;
  ShowTabMarks:=True;
  IsDssGraph:=False;
  OnGetDesignerNames:=nil;
  OnChartFillFields:=nil;
  OnChartGetSourceStyle:=nil;
  OnChartIsDateTimeSource:=nil;
  OnChartIsValidComponentSource:=nil;
  TheSource:=TChartEditSources.Create(Self,nil);
  PageSeries.ActivePage:=TheTabSheet;
  {$IFDEF D3}
  PageSeries.HotTrack:=True;
  {$ENDIF}
end;

procedure TFormTeeSeries.RGMarkStyleClick(Sender: TObject);
begin
  TheSeries.Marks.Style:=TSeriesMarksStyle(RGMarkStyle.ItemIndex);
end;

procedure TFormTeeSeries.BEditFuncClick(Sender: TObject);
begin
  if ApplyChangesFunction then DoApplyChangesFunction;
  if TheSeries.FunctionType<>nil then
  With TFormPeriod.Create(Self) do
  try
    With TheSeries.FunctionType do
    begin
      ThePeriod      :=Period;
      ThePeriodStyle :=PeriodStyle;
      TheCanRange    :=not MovingFunction;
      TheAlignment   :=PeriodAlign;
      LCalc.Caption  :=Format(TeeMsg_CalcPeriod,[CBFunctions.Text]);
    end;
    With TheSeries do
    if ( DataSource<>nil ) and (DataSource is TChartSeries) then
      TheIsDateTime:=TChartSeries(DataSource).XValues.DateTime
    else
      TheIsDateTime:=False;
    if ShowModal=mrOk then
    With TheSeries.FunctionType do
    begin
      BeginUpdate;
      PeriodStyle:=ThePeriodStyle;
      Period     :=ThePeriod;
      PeriodAlign:=TheAlignment;
      EndUpdate;
    end;
  finally
    Free;
  end;
end;

procedure TFormTeeSeries.CBPersistentClick(Sender: TObject);
begin
  {$IFDEF D3}
  With TheSeries do
  if CBPersistent.Checked then Style:=Style+[tssIsPersistent]
                          else Style:=Style-[tssIsPersistent];
  {$ENDIF}
end;

procedure TFormTeeSeries.CBYDateTimeClick(Sender: TObject);
begin
  TheSeries.YValues.DateTime:=CBYDateTime.Checked;
end;

procedure TFormTeeSeries.CBXDateTimeClick(Sender: TObject);
begin
  TheSeries.XValues.DateTime:=CBXDateTime.Checked;
end;

Procedure TFormTeeSeries.IsDateTimeSource( AComponent:TComponent;
                                           Const ValueSource:String;
                                           Var IsDateTime:Boolean);
begin
  if Assigned(OnChartIsDateTimeSource) then
     OnChartIsDateTimeSource(AComponent,ValueSource,IsDateTime);
end;

procedure TFormTeeSeries.CBDataSourcestyleChange(Sender: TObject);
begin
  PCData.Visible:= (CBDataSourceStyle.ItemIndex<>Ord(dsStyle_NoData)) and
                   (CBDataSourceStyle.ItemIndex<>Ord(dsStyle_Random));
  Case TTeeDataSourceStyle(CBDataSourceStyle.ItemIndex) of
    dsStyle_NoData:
       begin
         TheSource.ClearValueSources;
         EnableDataSourceControls(False,False);
         TheSeries.DataSource:=nil;
         TheSeries.Clear;
       end;
    dsStyle_Random:
       begin
         TheSource.ClearValueSources;
         EnableDataSourceControls(False,False);
         With TheSeries do
         begin
           DataSource:=nil;
           SetFunction(nil);
           FillSampleValues(NumSampleValues);
         end;
       end;
   dsStyle_Function: EnableDataSourceControls(False,True);
  else
    {dsStyle_Dataset:} EnableDataSourceControls(True,False);
  end;
end;

procedure TFormTeeSeries.DoApplyChangesFunction;
var t        : Longint;
    tmp      : TChartSeries;
    tmpClass : TTeeFunctionClass;
begin
  { Set datasources... }
  if LBSelectedSeries.Items.Count=0 then TheSeries.DataSource:=nil
  else
  begin
    for t:=0 to LBSelectedSeries.Items.Count-1 do
    begin
      tmp:=TChartSeries(LBSelectedSeries.Items.Objects[t]);
      TheSeries.CheckOtherSeries(tmp);
    end;
    With TheSeries.DataSources do
    begin
      for t:=0 to Count-1 do
          TChartSeries(TheSeries.DataSources[t]).RemoveLinkedSeries(TheSeries);
      Clear;
      for t:=0 to LBSelectedSeries.Items.Count-1 do
      begin
        tmp:=TChartSeries(LBSelectedSeries.Items.Objects[t]);
        Add(tmp);
        TChartSeries(tmp).AddLinkedSeries(TheSeries);
      end;
    end;
  end;
  { Set function... }
  if CBFunctions.ItemIndex>0 then
  begin
    tmpClass:=TTeeFunctionClass(CBFunctions.Items.Objects[CBFunctions.ItemIndex]);
    if (TheSeries.FunctionType=nil) or
       (TheSeries.FunctionType.ClassType<>tmpClass) then
          CreateNewTeeFunction(TheSeries,tmpClass)
    else
          TheSeries.CheckDataSource;
  end
  else TheSeries.SetFunction(nil);
  ApplyChangesFunction:=False;
end;

Function TFormTeeSeries.GetSelectedSource:TComponent;
begin
  result:=TheSource.SelectedDataSource;
end;

Function TFormTeeSeries.GetSourceCombo(Index:Integer):TComboBox;
begin
  result:=TheSource.Values[Index].CBValues;
end;

procedure TFormTeeSeries.CBFunctionsChange(Sender: TObject);
begin
  ApplyChangesFunction:=True;
  BEditFunc.Enabled:=CBFunctions.ItemIndex>0;
end;

procedure TFormTeeSeries.LBSelectedSeriesDblClick(Sender: TObject);
begin
  BLeftOneClick(Self);
end;

procedure TFormTeeSeries.LBAvailSeriesDblClick(Sender: TObject);
begin
  BRightOneClick(Self);
end;

procedure TFormTeeSeries.BLeftAllClick(Sender: TObject);
begin
  MoveListAll(ToList,FromList);
  EnableListButtons;
  ApplyChangesFunction:=True;
end;

Procedure TFormTeeSeries.ChangeApply;
begin
  Case DataSourceStyle of
    dsStyle_Function: ApplyChangesFunction:=True;
    dsStyle_Dataset,
    dsStyle_DataSource: ApplyChangesDataset:=True;
  end;
end;

procedure TFormTeeSeries.BRightAllClick(Sender: TObject);
begin
  MoveListAll(FromList,ToList);
  EnableListButtons;
  ChangeApply;
end;

procedure TFormTeeSeries.BLeftOneClick(Sender: TObject);
begin
  MoveList(ToList,FromList);
  EnableListButtons;
  ChangeApply;
end;

procedure TFormTeeSeries.BRightOneClick(Sender: TObject);
begin
  MoveList(FromList,ToList);
  EnableListButtons;
  ChangeApply;
end;

procedure TFormTeeSeries.EnableListButtons;
begin
  Case DataSourceStyle of
    dsStyle_Function: begin
     BRightOne.Enabled:=FromList.Items.Count>0;
     BRightAll.Enabled:=BRightOne.Enabled;
     BLeftOne.Enabled :=ToList.Items.Count>0;
     BLeftAll.Enabled :=BLeftOne.Enabled;
  end;
  dsStyle_DataSource: begin
    BROneDS.Enabled:=FromList.Items.Count>0;
    BRAllDS.Enabled:=BROneDS.Enabled;
    BLOneDS.Enabled :=ToList.Items.Count>0;
    BLAllDS.Enabled :=BLOneDS.Enabled;
  end;
  end;
end;

Procedure TFormTeeSeries.FillFields;  { virtual }
begin
  if Assigned(OnChartFillFields) then OnChartFillFields(Self);
end;

procedure TFormTeeSeries.CBLabelsFieldChange(Sender: TObject);
begin
  SetTextItemIndex(CBLabelsField);
  With CBLabelsField do
  if ItemIndex=-1 then
     TheSource.SelectedXLabelsSource:=''
  else
     TheSource.SelectedXLabelsSource:=Items[ItemIndex];
  ApplyChangesDataSet:=True;
end;

Procedure TFormTeeSeries.SetTextItemIndex(Combo:TComboBox);
var tmp : Integer;
begin
  With Combo do
  begin
    tmp:=Items.IndexOf(Text);
    if tmp<>ItemIndex then ItemIndex:=tmp;
  end;
end;

procedure TFormTeeSeries.CBValuesChange(Sender: TObject);
var tmp:Boolean;
begin
  SetTextItemIndex(TComboBox(Sender));
  With TComboBox(Sender),TheSource.Values[Tag] do
  begin
    if ItemIndex=-1 then SelectedValueSource:=''
                    else SelectedValueSource:=Items[ItemIndex];
    if SelectedValueSource<>'' then
    begin
      tmp:=CBDateTime.Checked;
      IsDateTimeSource(TheSource.SelectedDataSource,SelectedValueSource,tmp);
      CBDateTime.Checked:=tmp;
    end;
  end;
  ApplyChangesDataSet:=True;
end;

procedure TFormTeeSeries.CBDateTimeClick(Sender: TObject);
begin
  ApplyChangesDataSet:=True;
end;

procedure TFormTeeSeries.DoApplyChangesDataset;

  Procedure CheckFieldIsBlank(Const AFieldName:String);
  begin
    if AFieldName<>'' then
       Raise ChartException.CreateFmt(TeeMsg_FieldNotFound,[AFieldName]);
  end;

  Procedure CheckValidFields;
  var t:Integer;
  begin
    for t:=0 to TheSeries.ValuesLists.Count-1 do
    With TheSource.Values[t],CBValues do
    begin
      SetTextItemIndex(CBValues);
      if ItemIndex=-1 then CheckFieldIsBlank(Text);
    end;
    SetTextItemIndex(CBLabelsField);
    With CBLabelsField do
    if ItemIndex=-1 then CheckFieldIsBlank(Text);
  end;

var t     : Integer;
    tmpSt : String;
begin
  if Assigned(TheSeries) then
  With TheSeries do
  begin
    if TheSource.SelectedDataSource=nil then
       CBDataSourceStyle.ItemIndex:=Ord(dsStyle_NoData)
    else
    begin
      if DataSourceStyle=dsStyle_DataSet then
         CheckValidFields; { raise exception if non valid user input }
      DataSource:=nil;
      if DataSourceStyle=dsStyle_DataSet then
      begin
        for t:=0 to ValuesLists.Count-1 do
        With ValuesLists[t],TheSource.Values[t] do
        begin
          if CBValues.ItemIndex=-1 then ValueSource:=''
                                   else ValueSource:=SelectedValueSource;
          DateTime:=CBDateTime.Checked;
        end;
        With CBLabelsField do
        if ItemIndex=-1 then XLabelsSource:=''
                        else XLabelsSource:=TheSource.SelectedXLabelsSource;
      end
      else
      begin
        if LBSelFields.Items.Count>0 then
        begin
           tmpSt:=LBSelFields.Items[0];
           for t:=1 to LBSelFields.Items.Count-1 do
               tmpSt:=tmpSt+';'+LBSelFields.Items[t];
        end
        else tmpSt:='';
        MandatoryValueList.ValueSource:=tmpSt;
      end;
      DataSource:=TheSource.SelectedDataSource;
    end;
  end;
  ApplyChangesDataSet:=False;
end;

procedure TFormTeeSeries.CBListDataSetsChange(Sender: TObject);
begin
  SetSelectedSourceDatabase;
  ApplyChangesDataset:=True;
end;

procedure TFormTeeSeries.CBShowInLegendClick(Sender: TObject);
begin
  TheSeries.ShowInLegend:=CBShowInLegend.Checked;
end;

procedure TFormTeeSeries.RGVertAxisClick(Sender: TObject);
begin
  TheSeries.VertAxis:=TVertAxis(RGVertAxis.ItemIndex);
end;

procedure TFormTeeSeries.CBSeriesCursorChange(Sender: TObject);
var tmpCursor:Longint;
begin
  if TeeIdentToCursor(TeeCursorPrefix+
                      CBSeriesCursor.Items[CBSeriesCursor.ItemIndex],
                      tmpCursor) then
     TheSeries.Cursor:=tmpCursor;
end;

procedure TFormTeeSeries.BMarkLinColClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.Marks.Arrow);
end;

procedure TFormTeeSeries.BMarksFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.Marks.Frame);
end;

procedure TFormTeeSeries.BMarkFontClick(Sender: TObject);
begin
  With TheSeries.Marks do Font:=InternalEditFont(Self,Font);
end;

procedure TFormTeeSeries.SEArrowLengthChange(Sender: TObject);
begin
  if (not CreatingForm) and (SEArrowLength.Text<>'') then
     TheSeries.Marks.ArrowLength:=StrToInt(SEArrowLength.Text);
end;

procedure TFormTeeSeries.CBMarkClipClick(Sender: TObject);
begin
  TheSeries.Marks.Clip:=CBMarkClip.Checked;
end;

procedure TFormTeeSeries.CBTransparentClick(Sender: TObject);
begin
  TheSeries.Marks.Transparent:=CBTransparent.Checked;
end;

procedure TFormTeeSeries.CBMarksVisibleClick(Sender: TObject);
begin
  TheSeries.Marks.Visible:=CBMarksVisible.Checked;
end;

procedure TFormTeeSeries.SetCurrentDataSource;
begin
  if TheSource.SelectedDataSource=nil then
     CBListDataSets.ItemIndex:=-1
  else
     CBListDataSets.ItemIndex:=CBListDataSets.Items.IndexOfObject(TheSource.SelectedDataSource);
end;

Function TFormTeeSeries.DataSourceStyle:TTeeDataSourceStyle;
begin
  result:=TTeeDataSourceStyle(CBDataSourceStyle.ItemIndex);
end;

procedure TFormTeeSeries.SetSelectedSourceDatabase;
begin
  if DataSourceStyle=dsStyle_DataSet then
  begin
    CBLabelsField.Items.Clear;
    CBLabelsField.Enabled:= (CBListDataSets.ItemIndex<>-1);
    TheSource.ClearCombos(CBListDataSets.ItemIndex<>-1);
  end;
  if CBListDataSets.ItemIndex=-1 then
  begin
    TheSource.SelectedDataSource:=nil;
    if DataSourceStyle=dsStyle_DataSource then
    begin
      LBAvailFields.Clear;
      LBSelFields.Clear;
    end;
  end
  else
  begin
    TheSource.SelectedDataSource:=TComponent(CBListDataSets.Items.Objects[CBListDataSets.ItemIndex]);
    if Assigned(TheSource.SelectedDataSource) then FillFields;
  end;
  if DataSourceStyle=dsStyle_DataSet then
  begin
    CBLabelsField.ItemIndex:=CBLabelsField.Items.IndexOf(TheSource.SelectedXLabelsSource);
    TheSource.SetSeriesDatabaseSource(TheSeries);
  end;
end;

procedure TFormTeeSeries.FillSourceSeries;
begin
  FillTeeFunctions(CBFunctions.Items);
  LBAvailSeries.Items.Clear;
  FillSources(LBAvailSeries.Items,False);
  if TheSource.SelectedDataSource=nil then
     LBAvailSeries.ItemIndex:=-1
  else
     LBAvailSeries.ItemIndex:=LBAvailSeries.Items.IndexOfObject(TheSource.SelectedDataSource);
  SetSelectedSourceSeries;
end;

procedure TFormTeeSeries.SetTabSeriesFunctions;
var t:Integer;
    tmpSeries:TChartSeries;
    tmpSt:String;
begin
  FillSourceSeries;
  TheSource.SetSeries(TheSeries);
  With LBSelectedSeries do
  begin
    Items.BeginUpdate;
    Clear;
    if (TheSeries.DataSource<>nil) then
    for t:=0 to TheSeries.DataSources.Count-1 do
    begin
      if TComponent(TheSeries.DataSources[t]) is TChartSeries then
      begin
        tmpSeries:=TChartSeries(TheSeries.DataSources[t]);
        With tmpSeries do
        begin
          if Title<>'' then tmpSt:=Title else tmpSt:=Name;
          Items.AddObject(tmpSt,tmpSeries);
        end;
      end;
    end;
    Items.EndUpdate;
  end;
  With CBFunctions do
  if TheSeries.FunctionType<>nil then
     ItemIndex:=Items.IndexOfObject(TObject(TTeeFunction(TheSeries.FunctionType).ClassType))
  else
     ItemIndex:=0;
  BEditFunc.Enabled:=CBFunctions.ItemIndex>0;
  ApplyChangesFunction:=False;
  FromList:=LBAvailSeries;
  ToList:=LBSelectedSeries;
  EnableListButtons;
end;

procedure TFormTeeSeries.SetSelectedSourceSeries;
begin
end;

procedure TFormTeeSeries.PageSeriesChanging(Sender: TObject;
  var AllowChange: Boolean);

  Function YesNoCancel(Const Message:String):Integer;
  Begin
    Screen.Cursor:=crDefault;
    result:=MessageDlg(Message,mtConfirmation,mbYesNoCancel,0);
  End;

begin
  AllowChange:=False;
  if (PageSeries.ActivePage=TabDataSource) then
  begin
    if ApplyChangesDataset or ApplyChangesFunction then
       Case YesNoCancel(TeeMsg_SureToApply) of
         mrYes   : begin
                     try
                       if ApplyChangesDataset then DoApplyChangesDataset
                                              else DoApplyChangesFunction;
                       AllowChange:=True;
                     except
                       on E:Exception do Application.ShowException(E);
                     end;
                   end;
         mrNo    : begin
                     ApplyChangesDataSet:=False;
                     ApplyChangesFunction:=False;
                     AllowChange:=True;
                   end;
       end
    else AllowChange:=TheSeries<>nil;
  end
  else AllowChange:=True;
end;

procedure TFormTeeSeries.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ApplyChangesDataset then DoApplyChangesDataset
  else
  if ApplyChangesFunction then DoApplyChangesFunction;
end;

procedure TFormTeeSeries.FormDestroy(Sender: TObject);
begin
  TheSource.Free;
  DestroySeriesForms;
end;

procedure TFormTeeSeries.FormShow(Sender: TObject);
begin
  CreatingForm:=False;
end;

end.
