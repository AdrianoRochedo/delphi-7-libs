{**********************************************}
{ TeeChart Editor Dialog for Delphi 1.0 16bit  }
{ Copyright (c) 1995-1998 by David Berneda     }
{ All Rights Reserved                          }
{**********************************************}
unit IEdit16;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Chart, Buttons, Spin, AxMaxMin, AxisIncr, TeEngine, TeePrevi,
  StdCtrls, ExtCtrls, TeeFunci, TeeProcs, TabNotBk;

Const MaxValueSources=16;

      teeEditMainPage    =0;
      teeEditGeneralPage =1;
      teeEditAxisPage    =2;
      teeEditTitlePage   =3;
      teeEditLegendPage  =4;
      teeEditPanelPage   =5;
      teeEditPagingPage  =6;
      teeEditWallsPage   =7;
      teeEdit3DPage      =8;

      dsStyle_NoData     =0;
      dsStyle_Random     =1;
      dsStyle_Function   =2;
      dsStyle_Dataset    =3;

type
  TChartEditorTab=( cetMain,
                    cetGeneral,
                    cetAxis,
                    cetTitles,
                    cetLegend,
                    cetPanel,
                    cetPaging,
                    cetWalls,
                    cet3D,
                    cetSeriesGeneral,
                    cetSeriesMarks
                   );

  TChartEditForm=class;

  TChartEditorOption=( ceAdd, ceDelete, ceChange, ceClone, ceDataSource,
                       ceTitle  );

  TChartEditorOptions=set of TChartEditorOption;

  TChartEditorHiddenTabs=set of TChartEditorTab;

  TOnChartFillFields=Procedure(AEditChartForm:TChartEditForm);
  TOnChartIsDateTimeSource=Procedure( AComponent:TComponent;
                                      Const ValueSource:String;
                                      Var IsDateTime:Boolean);
  TOnChartIsValidComponentSource=Function(AComponent:TComponent):Boolean;

  TChartEditForm = class(TForm)
    MainPage: TTabbedNotebook;
    Notebook1: TTabbedNotebook;
    RGWhatAxis: TRadioGroup;
    PageControlAxis: TTabbedNotebook;
    Label5: TLabel;
    Label8: TLabel;
    LAxisMax: TLabel;
    LAxisMin: TLabel;
    Label6: TLabel;
    LAxisIncre: TLabel;
    CBAutomatic: TCheckBox;
    CBLogarithmic: TCheckBox;
    CBInverted: TCheckBox;
    CBAutoMax: TCheckBox;
    CBAutoMin: TCheckBox;
    BAxisMax: TButton;
    BAxisMin: TButton;
    BAxisIncre: TButton;
    Label14: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    EAxisTitle: TEdit;
    BTitleFont: TButton;
    SEAxisTitleAngle: TSpinEdit;
    SEAxisTitleSize: TSpinEdit;
    Label20: TLabel;
    LabelAxisFormat: TLabel;
    Label26: TLabel;
    Label23: TLabel;
    CBAxisLabels: TCheckBox;
    CBLabelsOnAxis: TCheckBox;
    CBRoundFirstLabel: TCheckBox;
    EAxisValuesFormat: TEdit;
    SEAxisLabelSepar: TSpinEdit;
    BitBtn2: TButton;
    SELabelsSize: TSpinEdit;
    SEAxisLabelsAngle: TSpinEdit;
    RGAxisLabelStyle: TRadioGroup;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    BAxisPen: TButton;
    BAxisTickPen: TButton;
    BAxisTickInner: TButton;
    SEAxisTickLength: TSpinEdit;
    SEInnerTicksLength: TSpinEdit;
    BAxisTickMinor: TButton;
    BAxisGrid: TButton;
    SEAxisMinorTickLen: TSpinEdit;
    SEMinorCount: TSpinEdit;
    CBAxisVisible: TCheckBox;
    GBMargins: TGroupBox;
    SETopMa: TSpinEdit;
    SELeftMa: TSpinEdit;
    SEBotMa: TSpinEdit;
    SERightMa: TSpinEdit;
    BPrint: TButton;
    CBClipPoints: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    CBShowLeg: TCheckBox;
    BLegColor: TButton;
    BLegFont: TButton;
    SETopLegPos: TSpinEdit;
    SELegColWi: TSpinEdit;
    CBLegStyle: TComboBox;
    cbLegendStyle: TComboBox;
    BLegendFrame: TButton;
    CBResizeChart: TCheckBox;
    CBLegendInverted: TCheckBox;
    GroupBox1: TGroupBox;
    Label25: TLabel;
    BLegShadowCol: TButton;
    SELegShadowSize: TSpinEdit;
    Label2: TLabel;
    Label19: TLabel;
    RGBevelIn: TRadioGroup;
    RGBevelOut: TRadioGroup;
    CBPanelBorder: TCheckBox;
    SEPanelWi: TSpinEdit;
    BPanelColor: TButton;
    SEPanelBor: TSpinEdit;
    Label17: TLabel;
    LabelPages: TLabel;
    Label18: TLabel;
    SEPointsPerPage: TSpinEdit;
    ButtonPrevious: TBitBtn;
    ButtonNext: TBitBtn;
    CBScaleLast: TCheckBox;
    BLastPage: TBitBtn;
    BFirstPage: TBitBtn;
    CBShowAxis: TCheckBox;
    GroupBox4: TGroupBox;
    CBGradientVisible: TCheckBox;
    RGGradientDirection: TRadioGroup;
    PaintBox1: TShape;
    BGradientStart: TButton;
    PaintBox2: TShape;
    BGradientEnd: TButton;
    BClose: TButton;
    BExport: TButton;
    CBTickOnLabels: TCheckBox;
    RGPanning: TRadioGroup;
    TabSubWalls: TTabbedNotebook;
    CBView3dWalls: TCheckBox;
    GBZoom: TGroupBox;
    CBAllowZoom: TCheckBox;
    CBAnimatedZoom: TCheckBox;
    LAniZoomSteps: TLabel;
    SEAniZoomSteps: TSpinEdit;
    GroupBox7: TGroupBox;
    RBLegendLeft: TRadioButton;
    RBLegendRight: TRadioButton;
    RBLegendBottom: TRadioButton;
    RBLegendTop: TRadioButton;
    Label1: TLabel;
    SELegMargin: TSpinEdit;
    Button1: TButton;
    CBTitleVisible: TCheckBox;
    CBTitleAdjust: TCheckBox;
    BTitFont: TButton;
    BTitleFrame: TButton;
    BTitleBrush: TButton;
    RGTitalign: TRadioGroup;
    MTitle: TMemo;
    CBTitles: TComboBox;
    PageControlSeries: TTabbedNotebook;
    GroupBox5: TGroupBox;
    Label27: TLabel;
    CBShowInLegend: TCheckBox;
    CBSeriesCursor: TComboBox;
    GroupBox2: TGroupBox;
    Label15: TLabel;
    Label21: TLabel;
    EValueformat: TEdit;
    EPercentFormat: TEdit;
    RGHorizAxis: TRadioGroup;
    RGVertAxis: TRadioGroup;
    CBMarksVisible: TCheckBox;
    GroupBox3: TGroupBox;
    Label32: TLabel;
    SEArrowLength: TSpinEdit;
    BMarkLinCol: TButton;
    RGMarkStyle: TRadioGroup;
    GroupBox11: TGroupBox;
    BMarksBackColor: TButton;
    CBTransparent: TCheckBox;
    BMarkFont: TButton;
    BMarksFrame: TButton;
    CBMarkClip: TCheckBox;
    CBDataSourcestyle: TComboBox;
    PageControl2: TNotebook;
    Label16: TLabel;
    CBListDataSets: TComboBox;
    GroupFields: TScrollBox;
    LabelLabels: TLabel;
    CBLabelsField: TComboBox;
    Label34: TLabel;
    CBFunctions: TComboBox;
    GroupBox9: TGroupBox;
    Label22: TLabel;
    Label24: TLabel;
    LBAvailSeries: TListBox;
    LBSelectedSeries: TListBox;
    BRightOne: TButton;
    BRightAll: TButton;
    BLeftOne: TButton;
    BLeftAll: TButton;
    CBSeries: TComboBox;
    ImageSeries: TImage;
    LabelSeriesClass: TLabel;
    LBSeries: TListBox;
    HeaderControl1: THeader;
    BMoveUP: TBitBtn;
    BMoveDown: TBitBtn;
    BAddSeries: TButton;
    BDeleteSeries: TButton;
    BRenameSeries: TButton;
    BCloneSeries: TButton;
    BChangeTypeSeries: TButton;
    GroupBox6: TGroupBox;
    RGBitmap: TRadioGroup;
    BBrowseImage: TButton;
    CBImageInside: TCheckBox;
    CBXDateTime: TCheckBox;
    CBYDateTime: TCheckBox;
    Panel1: TPanel;
    Label33: TLabel;
    BWallColor: TButton;
    SHWallColor: TShape;
    BWallPen: TButton;
    BWallBrush: TButton;
    SEWallSize: TSpinEdit;
    BWallColor2: TButton;
    SHWallColor2: TShape;
    BWallPen2: TButton;
    BWallBrush2: TButton;
    SEWallSize2: TSpinEdit;
    Label4: TLabel;
    SHWallColor3: TShape;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    SEWallSize3: TSpinEdit;
    Label35: TLabel;
    CBMultiLine: TCheckBox;
    CBGridCenter: TCheckBox;
    CBLeftWallTrans: TCheckBox;
    CBBotWallTrans: TCheckBox;
    CBBackWallTrans: TCheckBox;
    GB3D: TGroupBox;
    Label13: TLabel;
    Label36: TLabel;
    CBView3d: TCheckBox;
    SE3d: TSpinEdit;
    CBOrthogonal: TCheckBox;
    SBZoom: TScrollBar;
    Label37: TLabel;
    SBRot: TScrollBar;
    Label38: TLabel;
    SBElev: TScrollBar;
    Label39: TLabel;
    Label40: TLabel;
    SBHorizOf: TScrollBar;
    SBVertOf: TScrollBar;
    LZoom: TLabel;
    LRot: TLabel;
    LElev: TLabel;
    LHorizOf: TLabel;
    LVertOf: TLabel;
    Label41: TLabel;
    SEAxisPos: TSpinEdit;
    Label42: TLabel;
    SEAxisStart: TSpinEdit;
    Label43: TLabel;
    SEAxisEnd: TSpinEdit;
    CBZoomFonts: TCheckBox;
    CBWallDark: TCheckBox;
    CBWallDarkBot: TCheckBox;
    CBWallDarkBack: TCheckBox;
    Button5: TButton;
    Label44: TLabel;
    SBPerspec: TScrollBar;
    LPerspec: TLabel;
    procedure SELegMarginChange(Sender: TObject);
    procedure cbLegendStyleChange(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure BTitFontClick(Sender: TObject);
    procedure BColGridClick(Sender: TObject);
    procedure RGHorizAxisClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit9Change(Sender: TObject);
    procedure SETopLegPosChange(Sender: TObject);
    procedure RGBevelInClick(Sender: TObject);
    procedure RGBevelOutClick(Sender: TObject);
    procedure CBPanelBorderClick(Sender: TObject);
    procedure SEPanelWiChange(Sender: TObject);
    procedure RGWhatAxisClick(Sender: TObject);
    procedure CBAxisVisibleClick(Sender: TObject);
    procedure SEAxisTickLengthChange(Sender: TObject);
    procedure BLegFontClick(Sender: TObject);
    procedure BPrintClick(Sender: TObject);
    procedure BLegColorClick(Sender: TObject);
    procedure BMarksBackColorClick(Sender: TObject);
    procedure CBAutomaticClick(Sender: TObject);
    procedure CBLegStyleChange(Sender: TObject);
    procedure BBackColClick(Sender: TObject);
    procedure CBShowLegClick(Sender: TObject);
    procedure CBAllowZoomClick(Sender: TObject);
    procedure CBClipPointsClick(Sender: TObject);
    procedure CBTitleVisibleClick(Sender: TObject);
    procedure EAxisTitleChange(Sender: TObject);
    procedure BChartFrameClick(Sender: TObject);
    procedure BLegendFrameClick(Sender: TObject);
    procedure BTitleFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MTitleChange(Sender: TObject);
    procedure BAxisMaxClick(Sender: TObject);
    procedure BAxisMinClick(Sender: TObject);
    procedure BAxisIncreClick(Sender: TObject);
    procedure BPanelColorClick(Sender: TObject);
    procedure CBLogarithmicClick(Sender: TObject);
    procedure BInnerTicksClick(Sender: TObject);
    procedure SEInnerTicksLengthChange(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure SEPointsPerPageChange(Sender: TObject);
    procedure CBScaleLastClick(Sender: TObject);
    procedure BGradientStartClick(Sender: TObject);
    procedure BGradientEndClick(Sender: TObject);
    procedure RGGradientDirectionClick(Sender: TObject);
    procedure CBGradientVisibleClick(Sender: TObject);
    procedure BLastPageClick(Sender: TObject);
    procedure BFirstPageClick(Sender: TObject);
    procedure CBAnimatedZoomClick(Sender: TObject);
    procedure CBResizeChartClick(Sender: TObject);
    procedure BTitleFrameClick(Sender: TObject);
    procedure CBFootAdjustClick(Sender: TObject);
    procedure CBTitleAdjustClick(Sender: TObject);
    procedure BTitleBrushClick(Sender: TObject);
    procedure BMinorTicksClick(Sender: TObject);
    procedure SEAxisMinorTickLenChange(Sender: TObject);
    procedure SEMinorCountChange(Sender: TObject);
    procedure SEPanelBorChange(Sender: TObject);
    procedure CBShowAxisClick(Sender: TObject);
    procedure CBAutoMaxClick(Sender: TObject);
    procedure CBAutoMinClick(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBLegendInvertedClick(Sender: TObject);
    procedure BLegShadowColClick(Sender: TObject);
    procedure SELegShadowSizeChange(Sender: TObject);
    procedure RGBitmapClick(Sender: TObject);
    procedure SEAxisTitleAngleChange(Sender: TObject);
    procedure SEAxisTitleSizeChange(Sender: TObject);
    procedure CBAxisLabelsClick(Sender: TObject);
    procedure SEAxisLabelsAngleChange(Sender: TObject);
    procedure RGAxisLabelStyleClick(Sender: TObject);
    procedure SELabelsSizeChange(Sender: TObject);
    procedure CBLabelsOnAxisClick(Sender: TObject);
    procedure SEAxisLabelSeparChange(Sender: TObject);
    procedure CBRoundFirstLabelClick(Sender: TObject);
    procedure EAxisValuesFormatChange(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CBView3dWallsClick(Sender: TObject);
    procedure BWallPenClick(Sender: TObject);
    procedure BWallBrushClick(Sender: TObject);
    procedure BWallColorClick(Sender: TObject);
    procedure BBottomWallPenClick(Sender: TObject);
    procedure BBottomWallBrushClick(Sender: TObject);
    procedure SEWallSizeChange(Sender: TObject);
    procedure EValueformatChange(Sender: TObject);
    procedure EPercentFormatChange(Sender: TObject);
    procedure CBMarksVisibleClick(Sender: TObject);
    procedure CBTransparentClick(Sender: TObject);
    procedure CBMarkClipClick(Sender: TObject);
    procedure SEArrowLengthChange(Sender: TObject);
    procedure BMarkFontClick(Sender: TObject);
    procedure BMarksFrameClick(Sender: TObject);
    procedure BMarkLinColClick(Sender: TObject);
    procedure BAxisPenClick(Sender: TObject);
    procedure CBSeriesCursorChange(Sender: TObject);
    procedure CBView3dClick(Sender: TObject);
    procedure RGTitalignClick(Sender: TObject);
    procedure RGVertAxisClick(Sender: TObject);
    procedure BAxisTickPenClick(Sender: TObject);
    procedure CBShowInLegendClick(Sender: TObject);
    procedure CBListDataSetsChange(Sender: TObject);
    procedure CBLabelsFieldChange(Sender: TObject);
    procedure CBTickOnLabelsClick(Sender: TObject);
    procedure BExportClick(Sender: TObject);
    procedure RGMarkStyleClick(Sender: TObject);
    procedure RGPanningClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SHWallColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SEAniZoomStepsChange(Sender: TObject);
    procedure RBLegendTopClick(Sender: TObject);
    procedure RBLegendLeftClick(Sender: TObject);
    procedure RBLegendRightClick(Sender: TObject);
    procedure RBLegendBottomClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure PageControlSeriesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure MainPageChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BRightOneClick(Sender: TObject);
    procedure BLeftOneClick(Sender: TObject);
    procedure BRightAllClick(Sender: TObject);
    procedure BLeftAllClick(Sender: TObject);
    procedure LBAvailSeriesDblClick(Sender: TObject);
    procedure LBSelectedSeriesDblClick(Sender: TObject);
    procedure CBFunctionsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBDataSourcestyleChange(Sender: TObject);
    procedure CBTitlesChange(Sender: TObject);
    procedure CBSeriesChange(Sender: TObject);
    procedure BMoveUPClick(Sender: TObject);
    procedure BMoveDownClick(Sender: TObject);
    procedure BAddSeriesClick(Sender: TObject);
    procedure BDeleteSeriesClick(Sender: TObject);
    procedure BRenameSeriesClick(Sender: TObject);
    procedure BCloneSeriesClick(Sender: TObject);
    procedure BChangeTypeSeriesClick(Sender: TObject);
    procedure LBSeriesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure LBSeriesDblClick(Sender: TObject);
    procedure LBSeriesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBSeriesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBSeriesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LBSeriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BBrowseImageClick(Sender: TObject);
    procedure CBImageInsideClick(Sender: TObject);
    procedure CBXDateTimeClick(Sender: TObject);
    procedure CBYDateTimeClick(Sender: TObject);
    procedure LBSeriesClick(Sender: TObject);
    procedure SE3dChange(Sender: TObject);
    procedure SERightMaChange(Sender: TObject);
    procedure SETopMaChange(Sender: TObject);
    procedure SEBotMaChange(Sender: TObject);
    procedure SELeftMaChange(Sender: TObject);
    procedure Notebook1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PageControlAxisChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure MainPageChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PageControlSeriesChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TabSubWallsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure CBOrthogonalClick(Sender: TObject);
    procedure SBZoomChange(Sender: TObject);
    procedure CBMultiLineClick(Sender: TObject);
    procedure CBGridCenterClick(Sender: TObject);
    procedure CBLeftWallTransClick(Sender: TObject);
    procedure SBRotChange(Sender: TObject);
    procedure SBElevChange(Sender: TObject);
    procedure SBHorizOfChange(Sender: TObject);
    procedure SBVertOfChange(Sender: TObject);
    procedure SEAxisPosChange(Sender: TObject);
    procedure SEAxisStartChange(Sender: TObject);
    procedure SEAxisEndChange(Sender: TObject);
    procedure CBZoomFontsClick(Sender: TObject);
    procedure CBWallDarkBackClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SBPerspecChange(Sender: TObject);
  private
    { Private declarations }
    TheSeriesForms       : Array[1..4] of TForm;
    NumSeriesForms       : Integer;
    procedure Refresh3DControls;
  protected
    Function IsDataSet:Boolean; virtual;
    Procedure FillFields; virtual;
    Procedure FillSources(AItems:TStrings; AddCurrent:Boolean);
    Function IsValidComponentSource(AComponent:TComponent):Boolean; virtual;
    Procedure IsDateTimeSource( AComponent:TComponent;
                                Const ValueSource:String;
                                Var IsDateTime:Boolean); virtual;
    Procedure AddSeriesForm(AForm:TForm; ATabIndex:Integer; ATag:Longint);
    procedure DestroySeriesForms;
  public
    { Public declarations }
    CreatingForm:Boolean;

    TheChart           : TCustomChart;
    TheAxis            : TCustomChartAxis;
    TheTitle           : TChartTitle;
    TheWall            : TChartWall;
    TheSeries          : TChartSeries;
    TheEditSeries      : TChartSeries;
    TheActivePageIndex : Longint;
    TheHiddenTabs      : TChartEditorHiddenTabs;

    ApplyChangesFunction : Boolean;
    ApplyChangesDataSet  : Boolean;

    EditorOptions        : TChartEditorOptions;
    ComingFromDoubleClick: Boolean;

    OnChartFillFields:TOnChartFillFields;
    OnChartIsDateTimeSource:TOnChartIsDateTimeSource;
    OnChartIsValidComponentSource:TOnChartIsValidComponentSource;

    Procedure SetAxisLabels;
    Procedure SetAxisTicks;
    Procedure SetAxisTitle;
    Procedure SetAxisScales;
    Procedure SetAxisPosition;
    procedure SetTabSeriesMarks;
    procedure SetTabSeriesGeneral;
    procedure SetTabSeriesDataSource;
    procedure SetTabSeriesDatabase;
    procedure SetTabSeriesFunctions;

    procedure SetTabTitle;
    procedure SetTabWalls(AWall:TChartWall);

    Function AxisTitleOrName(Axis:TCustomChartAxis):String;
    procedure TheChartPageChange(Sender: TObject);
    procedure RepaintGradientColors;
    Procedure CheckGradientVisible;
    Function GetDateTimeStepText(tmp:TDateTimeStep):String;
    procedure ProcGetCursors(const S: string);
    procedure SetSelectedSourceDatabase;
    procedure SetSelectedSourceSeries;
    procedure FillSourceDataSets;
    procedure FillSourceSeries;
    procedure CBDateTimeClick(Sender: TObject);
    procedure CBValuesChange(Sender: TObject);
    Function SelectedTitle:TChartTitle;
    Function SelectedWall:TChartWall;
    Procedure EnableLegendMarginControls;
    procedure EnableDataSourceControls(IsDatabase,IsFunction:Boolean);
    procedure EnableListButtons;
    Function GetSelectedSource:TComponent;
    Function GetSourceCombo(Index:Integer):TComboBox;
    procedure DoApplyChangesDataset;
    procedure DoApplyChangesFunction;
    procedure SetCurrentDataSource;
    procedure SetTabSeries;
    Procedure RefreshButtons;
    procedure SwapSeries(tmp1,tmp2:Longint);
    Function SelectedSeriesIndex:Integer;
    Function SelectedSeries:TChartSeries;
    procedure FillSeries(OldSeries:TChartSeries);
    Function SeriesAtMousePos(Var p:TPoint):Longint;
    Function PointInSection(Const P:TPoint; ASection:Longint):Boolean;
    procedure BEditSeriesClick;
    procedure CheckApplyChanges;
    Procedure RefreshWallDark(AWall:TChartWall);
    Procedure RefreshWallColorShape(AWall:TChartWall);
    procedure EnableImageControls;
    Function ChangeMargin(AEdit:TSpinEdit; APos,OtherSide:Integer):Integer;
    Function InsertSeriesForm( EditorClass:TFormClass;
                               Position:Integer;
                               Const EditorTabName:String;
                               AnObject:TPersistent):TForm;
  end;

  TChartEditFormClass=class of TChartEditForm;

Type TOnCreateEditSeries=Procedure(Sender:TChartEditForm; AChart:TCustomChart);
Var InternalOnCreateEditSeries:TOnCreateEditSeries;

Const
  eoAll=[ ceAdd, ceDelete, ceChange, ceClone, ceDataSource ];

procedure FillTeeFunctions(AList:TStrings);

Function GetOwnerForm(AControl:TControl):TForm;

{ Show the Export dialog }
Procedure ChartExport(AOwner:TForm; AChart:TCustomChart);

implementation

{$R *.DFM}

uses Printers,Dialogs,PenDlg,BrushDlg,TeeGally,TeExport,TypInfo,
     TeeConst,TeeLisB,Series,TeCanvas,TeeStore;

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
    ParentForm:TChartEditForm;
    SelectedDataSource:TComponent;
    SelectedXLabelsSource:String;
    Values:Array[0..MaxValueSources-1] of TChartEditSource;
    Constructor Create(AParent:TChartEditForm);
    Destructor Destroy; override;
    procedure ClearValueSources;
    procedure SetSeries(TheSeries:TChartSeries);
    procedure ClearCombos(EnableCombos:Boolean);
    procedure SetSeriesDatabaseSource(TheSeries:TChartSeries);
    procedure ClearSources;
  end;

Var TheSource:TChartEditSources;

{ Helper functions }
Function GetOwnerForm(AControl:TControl):TForm;
begin
  While (AControl<>nil) and (not (AControl is TForm)) do
        AControl:=AControl.Owner as TControl;
  if AControl=nil then result:=nil else result:=AControl as TForm;
end;

procedure FillTeeFunctions(AList:TStrings);
var t:Longint;
begin
  AList.Clear;
  AList.Add(TeeMsg_FunctionNone);
  With TeeSeriesTypes do
  for t:=0 to Count-1 do
    With SeriesType[t] do
    if (FunctionClass<>nil) and
       (SeriesClass=nil) and
       ( AList.IndexOfObject(TObject(FunctionClass))=-1) then
          AList.InsertObject(1,Description,TObject(FunctionClass));
end;

Procedure TeeExportSaveChart(ExportPanel:TCustomTeePanel; Const AFileName:String); far;
begin
  if ExportPanel is TCustomChart then
     SaveChartToFile(TCustomChart(ExportPanel),AFileName)
  else
     SaveTeeToFile(ExportPanel,AFileName);
end;

Procedure ChartExport(AOwner:TForm; AChart:TCustomChart);
begin
  With TTeeExportForm.Create(AOwner) do
  try
    ExportPanel:=AChart;
    TeeExportSave:=TeeExportSaveChart;
    try
      ShowModal;
    finally
      TeeExportSave:=nil;
    end;
  finally
    Free;
  end;
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
Constructor TChartEditSources.Create(AParent:TChartEditForm);
var t:Longint;
begin
  inherited Create;
  ParentForm:=AParent;
  for t:=0 to MaxValueSources-1 do Values[t]:=TChartEditSource.Create;
  ClearValueSources;
end;

procedure TChartEditSources.ClearSources;
var t:Longint;
begin
  SelectedXLabelsSource:='';
  for t:=0 to ParentForm.TheSeries.ValuesLists.Count-1 do
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
    CBDateTime.Checked:=TheSeries.ValuesLists.ValueList[t].DateTime;
  end;
end;

procedure TChartEditSources.ClearCombos(EnableCombos:Boolean);
var t:Longint;
begin
  for t:=0 to ParentForm.TheSeries.ValuesLists.Count-1 do
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
        Values[t].SelectedValueSource:=ValuesLists.ValueList[t].ValueSource;
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

{ TChartEditForm }
procedure TChartEditForm.SELegMarginChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart.Legend do
  Case Alignment of
    laTop,laBottom: VertMargin:=SELegMargin.Value;
    laLeft,laRight: HorizMargin:=SELegMargin.Value;
  end;
end;

procedure TChartEditForm.cbLegendStyleChange(Sender: TObject);
begin
  if not CreatingForm then
  TheChart.Legend.LegendStyle:=TLegendStyle(CBLegendStyle.ItemIndex);
end;

Function TChartEditForm.AxisTitleOrName(Axis:TCustomChartAxis):String;
Begin
  result:=Axis.Title.Caption;
  if result='' then
  With Axis do
  begin
    if Horizontal then
      if OtherSide then result:=TeeMsg_TopAxis
                   else result:=TeeMsg_BottomAxis
    else
      if OtherSide then result:=TeeMsg_RightAxis
                   else result:=TeeMsg_LeftAxis;
  end;
End;

Function TChartEditForm.GetDateTimeStepText(tmp:TDateTimeStep):String;
begin
  result:='';
  With TAxisIncrement.Create(Self) do
  try
    result:=CBSteps.Items[Ord(tmp)];
  finally
    Free;
  end;
end;

Function DelphiToLocalFormat(Const Format:String):String;
var t:Longint;
begin
  result:=Format;
  for t:=1 to Length(result) do
      if result[t]=',' then result[t]:=ThousandSeparator else
      if result[t]='.' then result[t]:=DecimalSeparator;
end;

Function LocalToDelphiFormat(Const Format:String):String;
var t:Longint;
begin
  result:=Format;
  for t:=1 to Length(result) do
      if result[t]=ThousandSeparator then result[t]:=',' else
      if result[t]=DecimalSeparator then result[t]:='.';
end;

Procedure TChartEditForm.SetAxisPosition;
begin
  With TheAxis do
  Begin
    SEAxisPos.Value:=Round(PositionPercent);
    SEAxisStart.Value:=Round(StartPosition);
    SEAxisEnd.Value:=Round(EndPosition);
    EnableControls(not TheAxis.IsDepthAxis,[SEAxisPos,SEAxisStart,SEAxisEnd]);
  end;
end;

Procedure TChartEditForm.SetAxisLabels;
var tmp:String;
begin
       { Axis Labels  }
  With TheAxis do
  Begin
    CBAxisLabels.Checked:=Labels;
    RGAxisLabelStyle.ItemIndex:=Ord(LabelStyle);
    CBLabelsOnAxis.Checked:=LabelsOnAxis;
    CBRoundFirstLabel.Checked:=RoundFirstLabel;
    SEAxisLabelsAngle.Value:=LabelsAngle;
    SEAxisLabelSepar.Value:=LabelsSeparation;
    SELabelsSize.Value:=LabelsSize;
    CBMultiLine.Checked:=LabelsMultiLine;
    if IsDateTime then
    begin
      LabelAxisFormat.Caption:=TeeMsg_DateTimeFormat;
      tmp:=DateTimeFormat;
      if tmp='' then tmp:=DateTimeDefaultFormat(Maximum-Minimum);
      EAxisValuesFormat.Text:=tmp;
    end
    else
    begin
      LabelAxisFormat.Caption:=TeeMsg_ValuesFormat;
      tmp:=AxisValuesFormat;
      if tmp='' then tmp:=TeeMsg_DefValueFormat;
      EAxisValuesFormat.Text:=DelphiToLocalFormat(tmp);
    end;
  end;
end;

Procedure TChartEditForm.SetAxisTicks;
begin
  With TheAxis do
  Begin
    SEAxisTickLength.Value:=TickLength;
    SEInnerTicksLength.Value:=TickInnerLength;
    SEAxisMinorTickLen.Value:=MinorTickLength;
    SEMinorCount.Value:=MinorTickCount;
    CBTickOnLabels.Checked:=TickOnLabelsOnly;
  end;
end;

Procedure TChartEditForm.SetAxisTitle;
begin
  With TheAxis do
  Begin
    EAxisTitle.Text:=Title.Caption;
    SEAxisTitleAngle.Value:=Title.Angle;
    SEAxisTitleSize.Value:=TitleSize;
  end;
end;

Procedure TChartEditForm.SetAxisScales;
var tmp:TDateTimeStep;
Begin
  CreatingForm:=True;
  With TheAxis do
  Begin
    if IsDateTime then
    begin
      tmp:=FindDateTimeStep(Increment);
      if ExactDateTime and (tmp<>dtNone) then
         LAxisIncre.Caption:=GetDateTimeStepText(tmp)
      else
      if Increment<=0 then
         LAxisIncre.Caption:=TimeToStr(DateTimeStep[dtOneSecond])
      else
      if Increment<=1 then
         LAxisIncre.Caption:=TimeToStr(Increment)
      else
      Begin
        LAxisIncre.Caption:=FloatToStr(Int(Increment));
        if Frac(Increment)<>0 then
           LAxisIncre.Caption:=LAxisIncre.Caption+' '+TimeToStr(Frac(Increment));
      end;
      if Minimum>=1 then LAxisMin.Caption:=DateTimeToStr(Minimum)
                    else LAxisMin.Caption:=TimeToStr(Minimum);
      if Maximum>=1 then LAxisMax.Caption:=DateTimeToStr(Maximum)
                    else LAxisMax.Caption:=TimeToStr(Maximum);
    end
    else
    begin
      LAxisIncre.Caption:=FormatFloat(AxisValuesFormat,Increment);
      LAxisMin.Caption:=FormatFloat(AxisValuesFormat,Minimum);
      LAxisMax.Caption:=FormatFloat(AxisValuesFormat,Maximum);
    end;
    CBAutomatic.Checked:=Automatic;
    CBAutoMax.Checked:=AutomaticMaximum;
    CBAutoMin.Checked:=AutomaticMinimum;
    CBLogarithmic.Checked:=Logarithmic;
    CBInverted.Checked:=Inverted;
    CBLogarithmic.Enabled:=not IsDepthAxis;
    { enable controls... }
    BAxisMax.Enabled:=(not Automatic) and (not AutomaticMaximum);
    BAxisMin.Enabled:=(not Automatic) and (not AutomaticMinimum);
    EnableControls(not Automatic,[CBAutoMax,CBAutoMin]);
  end;
  CreatingForm:=False;
end;

procedure TChartEditForm.SpinEdit2Change(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.ColorWidth:=SELegColWi.Value;
end;

Function TChartEditForm.SelectedTitle:TChartTitle;
begin
  if CBTitles.ItemIndex=0 then result:=TheChart.Title
                          else result:=TheChart.Foot;
end;

Function TChartEditForm.SelectedWall:TChartWall;
begin
  Case TabSubWalls.PageIndex of
    0: result:=TheChart.LeftWall;
    1: result:=TheChart.BottomWall;
  else result:=TheChart.BackWall;
  end;
end;

procedure TChartEditForm.BTitFontClick(Sender: TObject);
begin
  With SelectedTitle do
  Begin
    Font:=InternalEditFont(Self,Font);
    MTitle.Font:=Font;
    if MTitle.Font.Color=clWhite then MTitle.Color:=clDkGray
                                 else MTitle.Color:=clWhite;
  end;
end;

procedure TChartEditForm.BColGridClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Grid);
end;

procedure TChartEditForm.RGHorizAxisClick(Sender: TObject);
begin
  Case RGHorizAxis.ItemIndex of
    0: TheSeries.HorizAxis:=aTopAxis;
    1: TheSeries.HorizAxis:=aBottomAxis;
    2: TheSeries.HorizAxis:=aBothHorizAxis;
  end;
end;

Procedure TChartEditForm.EnableLegendMarginControls;
begin
  With TheChart.Legend do
  Case Alignment of
    laTop,laBottom: SELegMargin.Value:=VertMargin;
    laLeft,laRight: SELegMargin.Value:=HorizMargin;
  end;
end;

Function GetEditingCaption(AChart:TCustomChart):String;
begin
  FmtStr(result,TeeMsg_Editing,[AChart.Name]);
end;

procedure TChartEditForm.BEditSeriesClick;
var tmp:Boolean;
begin
  if LBSeries.ItemIndex<>-1 then
  begin
    MainPage.PageIndex:=1;
    PageControlSeries.PageIndex:=0;
    MainPageChange(Self,MainPage.PageIndex,tmp);
  end;
end;

procedure TChartEditForm.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  CreatingForm:=True;
  if Caption='' then Caption:=GetEditingCaption(TheChart);
  With TheChart do
  begin
    if not Assigned(TheAxis) then TheAxis:=LeftAxis;
    if TheAxis=LeftAxis   then RGWhatAxis.ItemIndex:=0    else
    if TheAxis=RightAxis  then RGWhatAxis.ItemIndex:=1    else
    if TheAxis=TopAxis    then RGWhatAxis.ItemIndex:=2    else
    if TheAxis=BottomAxis then RGWhatAxis.ItemIndex:=3    else
    if TheAxis=DepthAxis  then RGWhatAxis.ItemIndex:=4;
    CBTitles.ItemIndex:=0;
    if Assigned(TheTitle) then
    begin
      if TheTitle=Foot then CBTitles.ItemIndex:=1;
    end
    else
    if Assigned(TheWall) then
       if TheWall=LeftWall then TabSubWalls.PageIndex:=0 else
       if TheWall=BottomWall then TabSubWalls.PageIndex:=1 else
          TabSubWalls.PageIndex:=2;
  end;
  if TheActivePageIndex<>-1 then Notebook1.PageIndex:=TheActivePageIndex;
  CreatingForm:=False;
{  TabDataSource.TabVisible:=ceDataSource in EditorOptions;}
  FillSeries(TheEditSeries);
  if Assigned(TheEditSeries) then BEditSeriesClick;
end;

procedure TChartEditForm.SpinEdit9Change(Sender: TObject);
begin
  if not CreatingForm then TheChart.Chart3DPercent:=SE3D.Value;
end;

procedure TChartEditForm.SETopLegPosChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.TopPos:=SETopLegPos.Value;
end;

procedure TChartEditForm.RGBevelInClick(Sender: TObject);
begin
  if not CreatingForm then
     if TheChart.BevelInner<>TPanelBevel(RGBevelIn.ItemIndex) then
        TheChart.BevelInner:=TPanelBevel(RGBevelIn.ItemIndex);
end;

procedure TChartEditForm.RGBevelOutClick(Sender: TObject);
begin
  if not CreatingForm then
     if TheChart.BevelOuter<>TPanelBevel(RGBevelOut.ItemIndex) then
        TheChart.BevelOuter:=TPanelBevel(RGBevelOut.ItemIndex);
end;

procedure TChartEditForm.CBPanelBorderClick(Sender: TObject);
begin
  if not CreatingForm then
  if CBPanelBorder.Checked then
     TheChart.BorderStyle:=bsSingle
  else
     TheChart.BorderStyle:=bsNone;
end;

procedure TChartEditForm.SEPanelWiChange(Sender: TObject);
var tmp:Longint;
begin
  if not CreatingForm then
  begin
    tmp:=SEPanelWi.Value;
    if (tmp>=0) and (tmp<=100) then
    begin
       if TheChart.BevelWidth<>tmp then
          TheChart.BevelWidth:=tmp;
    end;
  end;
end;

procedure TChartEditForm.RGWhatAxisClick(Sender: TObject);
var tmp:Boolean;
begin
  if not CreatingForm then
  Begin
    With TheChart do
    Case TRadioGroup(Sender).ItemIndex of
      0: TheAxis:=LeftAxis;
      1: TheAxis:=RightAxis;
      2: TheAxis:=TopAxis;
      3: TheAxis:=BottomAxis;
      4: TheAxis:=DepthAxis;
    end;
    CBAxisVisible.Checked:=TheAxis.Visible;
    PageControlAxisChange(Self,PageControlAxis.PageIndex,tmp);
  end;
end;

procedure TChartEditForm.CBAxisVisibleClick(Sender: TObject);
begin
  if not CreatingForm then
  TheAxis.Visible:=CBAxisVisible.Checked;
end;

procedure TChartEditForm.SEAxisTickLengthChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisTickLength.Text<>'' then
  TheAxis.TickLength:=SEAxisTickLength.Value;
end;

procedure TChartEditForm.BLegFontClick(Sender: TObject);
begin
  With TheChart.Legend do Font:=InternalEditFont(Self,Font);
end;

procedure TChartEditForm.BPrintClick(Sender: TObject);
begin
  ChartPreview(Self,TheChart);
end;

procedure TChartEditForm.BLegColorClick(Sender: TObject);
begin
  with TheChart.Legend do Color:=EditColor(Self,Color);
end;

procedure TChartEditForm.BMarksBackColorClick(Sender: TObject);
begin
  With TheSeries.Marks do BackColor:=EditColor(Self,BackColor);
  CBTransparent.Checked:=False;
end;

procedure TChartEditForm.CBAutomaticClick(Sender: TObject);
begin
  if not CreatingForm then
  With TheAxis do
  Begin
    Automatic:=CBAutomatic.Checked;
    if Automatic then AdjustMaxMin
    else
    begin
      AutomaticMaximum:=False;
      AutomaticMinimum:=False;
    end;
    SetAxisScales;
  end;
end;

procedure TChartEditForm.CBLegStyleChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.TextStyle:=TLegendTextStyle(CBLegStyle.ItemIndex);
end;

procedure TChartEditForm.BBackColClick(Sender: TObject);
begin
  With TheChart do BackColor:=EditColor(Self,BackColor);
end;

procedure TChartEditForm.CBShowLegClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.Visible:=CBShowLeg.Checked;
end;

procedure TChartEditForm.CBAllowZoomClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.AllowZoom:=CBAllowZoom.Checked;
end;

procedure TChartEditForm.CBClipPointsClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.ClipPoints:=CBClipPoints.Checked;
end;

procedure TChartEditForm.CBTitleVisibleClick(Sender: TObject);
begin
  if not CreatingForm then SelectedTitle.Visible:=CBTitleVisible.Checked;
end;

procedure TChartEditForm.EAxisTitleChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.Title.Caption:=EAxisTitle.Text;
end;

procedure TChartEditForm.BChartFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheChart.Frame);
end;

procedure TChartEditForm.BLegendFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheChart.Legend.Frame);
end;

procedure TChartEditForm.BTitleFontClick(Sender: TObject);
begin
  With TheAxis.Title do Font:=InternalEditFont(Self,Font);
end;

procedure TChartEditForm.FormCreate(Sender: TObject);
begin
  TheActivePageIndex:=-1;
  NumSeriesForms:=0;
  EditorOptions:=[ceAdd,ceDelete,ceChange,ceClone,ceDataSource,ceTitle];
  OnChartFillFields:=nil;
  OnChartIsDateTimeSource:=nil;
  OnChartIsValidComponentSource:=nil;
  TheAxis  :=nil;
  TheSeries:=nil;
  TheEditSeries:=nil;
  TheChart :=nil;
  TheTitle :=nil;
  TheWall  :=nil;
  ComingFromDoubleClick:=False;
  TheSource:=TChartEditSources.Create(Self);
  MainPage.PageIndex:=0;
  NoteBook1.PageIndex:=0;
  PageControlSeries.PageIndex:=0;
end;

Function EqualStrings(A,B:TStrings):Boolean;
var t:Longint;
begin
  result:=A.Count=B.Count;
  if result then
  begin
    for t:=0 to A.Count-1 do
    begin
      result:=A[t]=B[t];
      if not result then break;
    end;
  end;
end;

procedure TChartEditForm.MTitleChange(Sender: TObject);
begin
  if not CreatingForm then
  With SelectedTitle do
  if not EqualStrings(Text,MTitle.Lines) then Text:=MTitle.Lines;
end;

procedure TChartEditForm.BAxisMaxClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Maximum+' '+AxisTitleOrName(TheAxis);
    IsDateTime:=TheAxis.IsDateTime;
    MaxMin:=TheAxis.Maximum;
    if ShowModal=mrOk then
    Begin
      TheAxis.Maximum:=MaxMin;
      CBAutoMax.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TChartEditForm.BAxisMinClick(Sender: TObject);
begin
  With TAxisMaxMin.Create(Self) do
  try
    Caption:=TeeMsg_Minimum+' '+AxisTitleOrName(TheAxis);
    IsDateTime:=TheAxis.IsDateTime;
    MaxMin:=TheAxis.Minimum;
    if ShowModal=mrOk then
    Begin
      TheAxis.Minimum:=MaxMin;
      CBAutoMin.Checked:=False;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TChartEditForm.BAxisIncreClick(Sender: TObject);
begin
  With TAxisIncrement.Create(Self) do
  try
    Caption:=Format(TeeMsg_DesiredIncrement,[AxisTitleOrName(TheAxis)]);
    IsDateTime  := TheAxis.IsDateTime;
    IsExact     := TheAxis.ExactDateTime;
    Increment   := TheAxis.Increment;
    IStep       := FindDateTimeStep(Increment);
    if ShowModal=mrOk then
    Begin
      TheAxis.Increment:=Increment;
      TheAxis.ExactDateTime:=IsExact;
      SetAxisScales;
    end;
  finally
    Free;
  end;
end;

procedure TChartEditForm.BPanelColorClick(Sender: TObject);
begin
  With TheChart do Color:=EditColor(Self,Color);
end;

procedure TChartEditForm.CBLogarithmicClick(Sender: TObject);
begin
  if not CreatingForm then
  try
    TheAxis.Logarithmic:=CBLogarithmic.Checked;
  except
    on AxisException do
    Begin
      TheAxis.Logarithmic:=False;
      CBLogarithmic.Checked:=False;
      Raise;
    end;
  end;
end;

procedure TChartEditForm.BInnerTicksClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.TicksInner);
end;

procedure TChartEditForm.SEInnerTicksLengthChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEInnerTicksLength.Text<>'' then
  TheAxis.TickInnerLength:=SEInnerTicksLength.Value;
end;

procedure TChartEditForm.ButtonNextClick(Sender: TObject);
begin
  TheChart.NextPage;  { <-- goto next chart page }
  TheChartPageChange(Self);
end;

procedure TChartEditForm.ButtonPreviousClick(Sender: TObject);
begin
  TheChart.PreviousPage;  { <-- goto next chart page }
  TheChartPageChange(Self);
end;

procedure TChartEditForm.TheChartPageChange(Sender: TObject);
begin
  { show the current page number and the total number of pages }
  { (like a report) }
  LabelPages.Caption:=IntToStr(TheChart.Page)+'/'+IntToStr(TheChart.NumPages);
  { enable or disable buttons }
  ButtonPrevious.Enabled:=TheChart.Page > 1;
  BFirstPage.Enabled:=ButtonPrevious.Enabled;
  ButtonNext.Enabled:=TheChart.Page < TheChart.NumPages;
  BLastPage.Enabled:=ButtonNext.Enabled;
end;

procedure TChartEditForm.SEPointsPerPageChange(Sender: TObject);
begin
  if SEPointsPerPage.Text='' then
     TheChart.MaxPointsPerPage:=0
  else
     TheChart.MaxPointsPerPage:=SEPointsPerPage.Value;
  TheChartPageChange(TheChart); { <-- repaint page / number of pages }
end;

procedure TChartEditForm.CBScaleLastClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.ScaleLastPage:=CBScaleLast.Checked;
end;

procedure TChartEditForm.BGradientStartClick(Sender: TObject);
begin
  TheChart.Gradient.StartColor:=EditColor(Self,TheChart.Gradient.StartColor);
  RepaintGradientColors;
end;

procedure TChartEditForm.RepaintGradientColors;
Begin
  With PaintBox1 do
  Begin
    Pen.Style:=psSolid;
    Pen.Color:=clBlack;
    Brush.Style:=bsSolid;
    Brush.Color:=TheChart.Gradient.StartColor;
  end;
  With PaintBox2 do
  Begin
    Pen.Style:=psSolid;
    Pen.Color:=clBlack;
    Brush.Style:=bsSolid;
    Brush.Color:=TheChart.Gradient.EndColor;
  end;
End;

procedure TChartEditForm.BGradientEndClick(Sender: TObject);
begin
  TheChart.Gradient.EndColor:=EditColor(Self,TheChart.Gradient.EndColor);
  RepaintGradientColors;
end;

procedure TChartEditForm.RGGradientDirectionClick(Sender: TObject);
begin
  TheChart.Gradient.Direction:=TGradientDirection(RGGradientDirection.ItemIndex);
end;

procedure TChartEditForm.CheckGradientVisible;
Begin
  RGGradientDirection.ItemIndex:=Ord(TheChart.Gradient.Direction);
  EnableControls(TheChart.Gradient.Visible,[ RGGradientDirection,
                                             BGradientStart,
                                             BGradientEnd,
                                             PaintBox1,
                                             PaintBox2]);
  RepaintGradientColors;
end;

procedure TChartEditForm.CBGradientVisibleClick(Sender: TObject);
begin
  if not CreatingForm then
     TheChart.Gradient.Visible:=CBGradientVisible.Checked;
  CheckGradientVisible;
end;

procedure TChartEditForm.BLastPageClick(Sender: TObject);
begin
  TheChart.Page:=TheChart.NumPages;  { <-- goto Last chart page }
  TheChartPageChange(Self);
end;

procedure TChartEditForm.BFirstPageClick(Sender: TObject);
begin
  TheChart.Page:=1;  { <-- goto first chart page }
  TheChartPageChange(Self);
end;

procedure TChartEditForm.CBAnimatedZoomClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.AnimatedZoom:=CBAnimatedZoom.Checked;
end;

procedure TChartEditForm.CBResizeChartClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.ResizeChart:=CBResizeChart.Checked;
end;

procedure TChartEditForm.BTitleFrameClick(Sender: TObject);
begin
  EditChartPen(Self,SelectedTitle.Frame);
end;

procedure TChartEditForm.CBFootAdjustClick(Sender: TObject);
begin
  TheChart.Foot.AdjustFrame:=TCheckBox(Sender).Checked;
end;

procedure TChartEditForm.CBTitleAdjustClick(Sender: TObject);
begin
  SelectedTitle.AdjustFrame:=TCheckBox(Sender).Checked;
end;

procedure TChartEditForm.BTitleBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,SelectedTitle.Brush);
end;

procedure TChartEditForm.BMinorTicksClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.MinorTicks);
end;

procedure TChartEditForm.SEAxisMinorTickLenChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisMinorTickLen.Text<>'' then
  TheAxis.MinorTickLength:=SEAxisMinorTickLen.Value;
end;

procedure TChartEditForm.SEMinorCountChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEMinorCount.Text<>'' then
  TheAxis.MinorTickCount:=SEMinorCount.Value;
end;

procedure TChartEditForm.SEPanelBorChange(Sender: TObject);
var tmp:Longint;
begin
  if not CreatingForm then
  begin
    tmp:=SEPanelBor.Value;
    if (tmp>=0) and (tmp<=100) then
    begin
      if TheChart.BorderWidth<>tmp then
         TheChart.BorderWidth:=tmp;
    end;
  end;
end;

procedure TChartEditForm.CBShowAxisClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.AxisVisible:= TCheckBox(Sender).Checked;
end;

procedure TChartEditForm.CBAutoMaxClick(Sender: TObject);
begin
  if not CreatingForm then
  Begin
    TheAxis.AutomaticMaximum:=CBAutoMax.Checked;
    TheAxis.AdjustMaxMin;
    SetAxisScales;
  end;
end;

procedure TChartEditForm.CBAutoMinClick(Sender: TObject);
begin
  if not CreatingForm then
  Begin
    TheAxis.AutomaticMinimum:=CBAutoMin.Checked;
    TheAxis.AdjustMaxMin;
    SetAxisScales;
  end;
end;

procedure TChartEditForm.CBInvertedClick(Sender: TObject);
begin
  if not CreatingForm then TheAxis.Inverted:=CBInverted.Checked;
end;

procedure TChartEditForm.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BGradientStartClick(Self);
end;

procedure TChartEditForm.PaintBox2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BGradientEndClick(Self);
end;

procedure TChartEditForm.CBLegendInvertedClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.Legend.Inverted:=CBLegendInverted.Checked;
end;

procedure TChartEditForm.BLegShadowColClick(Sender: TObject);
begin
  with TChartLegend(TheChart.Legend) do ShadowColor:=EditColor(Self,ShadowColor);
end;

procedure TChartEditForm.SELegShadowSizeChange(Sender: TObject);
begin
  if not CreatingForm then
     TChartLegend(TheChart.Legend).ShadowSize:=SELegShadowSize.Value;
end;

procedure TChartEditForm.RGBitmapClick(Sender: TObject);
begin
  TheChart.BackImageMode:=TTeeBackImageMode(RGBitmap.ItemIndex);
end;

procedure TChartEditForm.SEAxisTitleAngleChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisTitleAngle.Text<>'' then
  TheAxis.Title.Angle:=SEAxisTitleAngle.Value;
end;

procedure TChartEditForm.SEAxisTitleSizeChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisTitleSize.Text<>'' then
  TheAxis.TitleSize:=SEAxisTitleSize.Value;
end;

procedure TChartEditForm.CBAxisLabelsClick(Sender: TObject);
begin
  if not CreatingForm then TheAxis.Labels:=CBAxisLabels.Checked;
end;

procedure TChartEditForm.SEAxisLabelsAngleChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisLabelsAngle.Text<>'' then
  TheAxis.LabelsAngle:=SEAxisLabelsAngle.Value;
end;

procedure TChartEditForm.RGAxisLabelStyleClick(Sender: TObject);
begin
  if not CreatingForm then
  TheAxis.LabelStyle:=TAxisLabelStyle(RGAxisLabelStyle.ItemIndex);
end;

procedure TChartEditForm.SELabelsSizeChange(Sender: TObject);
begin
  if not CreatingForm then
  if SELabelsSize.Text<>'' then
  TheAxis.LabelsSize:=SELabelsSize.Value;
end;

procedure TChartEditForm.CBLabelsOnAxisClick(Sender: TObject);
begin
  if not CreatingForm then
  TheAxis.LabelsOnAxis:=CBLabelsOnAxis.Checked;
end;

procedure TChartEditForm.SEAxisLabelSeparChange(Sender: TObject);
begin
  if not CreatingForm then
  if SEAxisLabelSepar.Text<>'' then
  TheAxis.LabelsSeparation:=SEAxisLabelSepar.Value;
end;

procedure TChartEditForm.CBRoundFirstLabelClick(Sender: TObject);
begin
  if not CreatingForm then
     TheAxis.RoundFirstLabel:=CBRoundFirstLabel.Checked;
end;

procedure TChartEditForm.EAxisValuesFormatChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheAxis do
  if IsDateTime then
     DateTimeFormat:=EAxisValuesFormat.Text
  else
     AxisValuesFormat:=LocalToDelphiFormat(EAxisValuesFormat.Text);
end;

procedure TChartEditForm.BitBtn2Click(Sender: TObject);
begin
  With TheAxis do LabelsFont:=InternalEditFont(Self,LabelsFont);
end;

procedure TChartEditForm.CBView3dWallsClick(Sender: TObject);
begin
  TheChart.View3dWalls:=CBView3dWalls.Checked;
  TabSubWalls.Enabled:=TheChart.View3dWalls;
end;

procedure TChartEditForm.BWallPenClick(Sender: TObject);
begin
  EditChartPen(Self,SelectedWall.Pen);
end;

procedure TChartEditForm.BWallBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,SelectedWall.Brush);
end;

procedure TChartEditForm.BWallColorClick(Sender: TObject);
begin
  With SelectedWall do Color:=EditColor(Self,Color);
  RefreshWallColorShape(SelectedWall);
end;

procedure TChartEditForm.BBottomWallPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheChart.BottomWall.Pen);
end;

procedure TChartEditForm.BBottomWallBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,TheChart.BottomWall.Brush);
end;

procedure TChartEditForm.SEWallSizeChange(Sender: TObject);
begin
  With Sender as TSpinEdit do
  if Text<>'' then
  begin
    SelectedWall.Size:=Value;
    RefreshWallDark(SelectedWall);
  end;
end;

procedure TChartEditForm.EValueformatChange(Sender: TObject);
begin
  TheSeries.ValueFormat:=LocalToDelphiFormat(EValueFormat.Text);
end;

procedure TChartEditForm.EPercentFormatChange(Sender: TObject);
begin
  TheSeries.PercentFormat:=LocalToDelphiFormat(EPercentFormat.Text);
end;

procedure TChartEditForm.SetTabSeriesMarks;
begin
  With TheSeries.Marks do
  begin
    RGMarkStyle.ItemIndex  :=Ord(Style);
    CBMarksVisible.Checked :=Visible;
    SEArrowLength.Value    :=ArrowLength;
    CBTransparent.Checked  :=Transparent;
    CBMarkClip.Checked     :=Clip;
  end;
end;

procedure TChartEditForm.SetTabSeriesDatabase;
var t:Longint;
    tmpName:String;
begin
  GroupFields.Visible:=False;
  FillSourceDatasets;
  TheSource.SetSeries(TheSeries);
  With TheSeries,TheSource do
  begin
    for t:=0 to ValuesLists.Count-1 do
    With TheSource.Values[t] do
    begin
      tmpName:=ValuesLists.ValueList[t].Name;
      CBValues:=TComboBox.Create(ParentForm);
      With CBValues do
      begin
        Parent:=ParentForm.GroupFields;
        Left:=ParentForm.CBLabelsField.Left;
        Style:=csDropDown;
        Width:=ParentForm.CBLabelsField.Width;
        Top:=2+ParentForm.CBLabelsField.Top+ParentForm.CBLabelsField.Height+(25*t+1);
        OnChange:=ParentForm.CBValuesChange;
        Tag:=t;
        Visible:=tmpName<>'';
      end;
      LabelValues:=TLabel.Create(ParentForm);
      With LabelValues do
      begin
        Alignment:=taRightJustify;
        Parent:=ParentForm.GroupFields;
        Top:=CBValues.Top+4;
        AutoSize:=False;
        Left:=ParentForm.LabelLabels.Left;
        Width:=ParentForm.LabelLabels.Width;
        Caption:=tmpName+':';
        Visible:=tmpName<>'';
      end;
      CBDateTime:=TCheckBox.Create(ParentForm);
      With CBDateTime do
      begin
        Parent:=ParentForm.GroupFields;
        Left:=ParentForm.CBLabelsField.Left+ParentForm.CBLabelsField.Width+6;
        Width:=90;
        Top:=CBValues.Top;
        Caption:=TeeMsg_DateTime;
        Tag:=t;
        Visible:=tmpName<>'';
        OnClick:=ParentForm.CBDateTimeClick;
      end;
      SelectedValueSource:=ValuesLists.ValueList[t].ValueSource;
    end;
  end;
  TheSource.SetSeriesDatabaseSource(TheSeries);
  SetCurrentDataSource;
  SetSelectedSourceDatabase;
  GroupFields.Visible:=True;
  ApplyChangesDataSet:=False;
end;

procedure TChartEditForm.SetTabSeriesFunctions;
var t:Longint;
    tmpSeries:TChartSeries;
begin
  FillSourceSeries;
  TheSource.SetSeries(TheSeries);
  LBSelectedSeries.Items.BeginUpdate;
  LBSelectedSeries.Clear;
  if (TheSeries.DataSource<>nil) then
  for t:=0 to TheSeries.DataSources.Count-1 do
  begin
    if TComponent(TheSeries.DataSources[t]) is TChartSeries then
    begin
      tmpSeries:=TChartSeries(TheSeries.DataSources[t]);
      With tmpSeries do
           if Title<>'' then LBSelectedSeries.Items.AddObject(Title,tmpSeries)
                        else LBSelectedSeries.Items.AddObject(Name,tmpSeries);
    end;
  end;
  LBSelectedSeries.Items.EndUpdate;
  With CBFunctions do
  if TheSeries.FunctionType<>nil then
     ItemIndex:=Items.IndexOfObject(TObject(TTeeFunction(TheSeries.FunctionType).ClassType))
  else
     ItemIndex:=0;
  ApplyChangesFunction:=False;
  EnableListButtons;
end;

procedure TChartEditForm.SetTabSeriesDataSource;
begin
  if TheSeries.DataSource=nil then
  begin
    if TheSeries.FunctionType<>nil then
    begin
      CBDataSourceStyle.ItemIndex:=dsStyle_Function;
      EnableDataSourceControls(False,True);
    end
    else
    begin
      if TheSeries.Count>0 then
         CBDataSourceStyle.ItemIndex:=dsStyle_Random
      else
         CBDataSourceStyle.ItemIndex:=dsStyle_NoData;
      EnableDataSourceControls(False,False);
    end;
  end
  else
  if TheSeries.DataSource is TChartSeries then
  begin
    CBDataSourceStyle.ItemIndex:=dsStyle_Function;
    EnableDataSourceControls(False,True);
  end
  else
  begin
    CBDataSourceStyle.ItemIndex:=dsStyle_Dataset;
    EnableDataSourceControls(True,False);
  end;
end;

procedure TChartEditForm.CBMarksVisibleClick(Sender: TObject);
begin
  TheSeries.Marks.Visible:=CBMarksVisible.Checked;
end;

procedure TChartEditForm.CBTransparentClick(Sender: TObject);
begin
  TheSeries.Marks.Transparent:=CBTransparent.Checked;
end;

procedure TChartEditForm.CBMarkClipClick(Sender: TObject);
begin
  TheSeries.Marks.Clip:=CBMarkClip.Checked;
end;

procedure TChartEditForm.SEArrowLengthChange(Sender: TObject);
begin
  if SEArrowLength.Text<>'' then
     TheSeries.Marks.ArrowLength:=SEArrowLength.Value;
end;

procedure TChartEditForm.SetTabTitle;
begin
  With SelectedTitle do
  begin
    Case Alignment of
      taLeftJustify:  RGTitAlign.ItemIndex:=0;
      taCenter:       RGTitAlign.ItemIndex:=1;
      taRightJustify: RGTitAlign.ItemIndex:=2;
    end;
    CBTitleVisible.Checked :=Visible;
    CBTitleAdjust.Checked  :=AdjustFrame;
    MTitle.Font.Assign(Font);
    CreatingForm:=True;
    MTitle.Lines:=Text;
    CreatingForm:=False;
  end;
end;

Procedure TChartEditForm.RefreshWallColorShape(AWall:TChartWall);
begin
  if AWall=TheChart.LeftWall then
     SHWallColor.Brush.Color:=AWall.Color
  else
  if AWall=TheChart.BottomWall then
     SHWallColor2.Brush.Color:=AWall.Color
  else
     SHWallColor3.Brush.Color:=AWall.Color
end;

Procedure TChartEditForm.RefreshWallDark(AWall:TChartWall);
var tmp:TCheckBox;
begin
  if AWall=TheChart.LeftWall then tmp:=CBWallDark else
  if AWall=TheChart.BottomWall then tmp:=CBWallDarkBot else
     tmp:=CBWallDarkBack;
  tmp.Checked:=AWall.Dark3D;
  tmp.Enabled:=AWall.Size>0;
end;

procedure TChartEditForm.SetTabWalls(AWall:TChartWall);
var tmp:TCheckBox;
    tmpSE:TSpinEdit;
begin
  CBView3dWalls.Checked:=TheChart.View3dWalls;
  if AWall=TheChart.LeftWall then tmpSE:=SEWallSize else
  if AWall=TheChart.BottomWall then tmpSE:=SEWallSize2 else
     tmpSE:=SEWallSize3;
  tmpSE.Value:=AWall.Size;
  RefreshWallDark(AWall);
  RefreshWallColorShape(AWall);
  if AWall=TheChart.LeftWall then tmp:=CBLeftWallTrans else
  if AWall=TheChart.BottomWall then tmp:=CBBotWallTrans else
     tmp:=CBBackWallTrans;
  CreatingForm:=True;
  tmp.Checked:=AWall.Brush.Style=bsClear;
  CreatingForm:=False;
end;

procedure TChartEditForm.EnableImageControls;
begin
  RGBitmap.Enabled:=(TheChart.BackImage.Graphic<>nil);
  CBImageInside.Enabled:=RGBitmap.Enabled;
  if TheChart.BackImage.Graphic<>nil then
     BBrowseImage.Caption:=TeeMsg_ClearImage
  else
     BBrowseImage.Caption:=TeeMsg_BrowseImage;
end;

Type
  TFakeWinControl = Class(TWinControl)
  public
    procedure FreeHandle;
  end;

procedure TFakeWinControl.FreeHandle;
begin
  DestroyHandle;
end;

procedure TChartEditForm.Notebook1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);

  Procedure SetTab3D;
  begin
    With TheChart do
    begin
      CBView3D.Checked       :=View3d;
      SE3D.Value             :=Chart3dPercent;
      With View3DOptions do
      begin
        CBOrthogonal.Checked   :=Orthogonal;
        SBZoom.Position        :=Zoom;
        SBRot.Position         :=Rotation;
        SBElev.Position        :=Elevation;
        SBHorizOf.Position     :=HorizOffset;
        SBVertOf.Position      :=VertOffset;
        CBZoomFonts.Checked    :=ZoomText;
        SBPerspec.Position     :=Perspective;
      end;
      Refresh3DControls;
    end;
  end;

  Procedure SetTabGeneral;
  begin
    With TheChart do
    begin
      RGPanning.ItemIndex    :=Ord(AllowPanning);
      CBClipPoints.Checked   :=ClipPoints;
      SETopMa.Value          :=MarginTop;
      SELeftMa.Value         :=MarginLeft;
      SEBotMa.Value          :=MarginBottom;
      SERightMa.Value        :=MarginRight;
      CBAllowZoom.Checked    :=AllowZoom;
      CBAnimatedZoom.Checked :=AnimatedZoom;
      SEAniZoomSteps.Value   :=AnimatedZoomSteps;
    end;
  end;

  procedure SetTabLegend;
  begin
    With TheChart do
    begin
      SETopLegPos.Value      :=Legend.TopPos;
      SELegColWi.Value       :=Legend.ColorWidth;
      SELegShadowSize.Value  :=TChartLegend(Legend).ShadowSize;
      CBResizeChart.Checked  :=Legend.ResizeChart;
      CBLegendStyle.ItemIndex:=Ord(Legend.LegendStyle);
      CBLegStyle.ItemIndex   :=Ord(Legend.TextStyle);
      CBShowLeg.Checked      :=Legend.Visible;
      Case Legend.Alignment of
        laTop   : RBLegendTop.Checked:=True;
        laBottom: RBLegendBottom.Checked:=True;
        laLeft  : RBLegendLeft.Checked:=True;
        laRight : RBLegendRight.Checked:=True;
      end;
      EnableLegendMarginControls;
    end;
  end;

  procedure SetTabPanel;
  begin
    With TheChart do
    begin
      RGBevelIn.ItemIndex    :=Ord(BevelInner);
      RGBevelOut.ItemIndex   :=Ord(BevelOuter);
      CBPanelBorder.Checked  :=BorderStyle=bsSingle;
      SEPanelWi.Value        :=BevelWidth;
      SEPanelBor.Value       :=BorderWidth;
      CBGradientVisible.Checked:=Gradient.Visible;
      CheckGradientVisible;
      RGBitmap.ItemIndex     :=Ord(BackImageMode);
      CBImageInside.Checked  :=BackImageInside;
      EnableImageControls;
    end;
  end;

  procedure SetTabPaging;
  begin
    With TheChart do
    begin
      SEPointsPerPage.Value  :=MaxPointsPerPage;
      CBScaleLast.Checked    :=ScaleLastPage;
      TheChartPageChange(Self);
    end;
  end;

  Procedure SetTabAxis;
  var tmp:Boolean;
  begin
    CBShowAxis.Checked   :=TheAxis.ParentChart.AxisVisible;
    CBAxisVisible.Checked:=TheAxis.Visible;
    PageControlAxis.PageIndex:=0;
    PageControlAxisChange(Self,PageControlAxis.PageIndex,tmp);
  end;

begin
  CreatingForm:=True;
  With Notebook1 do  { fix resource comsumption }
       TFakeWinControl(Pages.Objects[PageIndex]).FreeHandle;
  Case NewTab of
    1: SetTabGeneral;
    2: SetTabAxis;
    3: SetTabTitle;
    4: SetTabLegend;
    5: SetTabPanel;
    6: SetTabPaging;
    7: SetTabWalls(SelectedWall);
    8: SetTab3D;
  end;
  CreatingForm:=False;
end;

type TGetStrProc = procedure(const S: string) of object;

procedure TChartEditForm.ProcGetCursors(const S: string);
begin
  CBSeriesCursor.Items.Add(S);
end;

procedure TChartEditForm.SetTabSeriesGeneral;
var tmpSt:String;
begin
  With TheSeries do
  begin
    CBShowInLegend.Checked   :=ShowInLegend;
    EValueFormat.Text        :=DelphiToLocalFormat(ValueFormat);
    EPercentFormat.Text      :=DelphiToLocalFormat(PercentFormat);
    Case HorizAxis of
      aTopAxis   : RGHorizAxis.ItemIndex:=0;
      aBottomAxis: RGHorizAxis.ItemIndex:=1;
      aBothHorizAxis: RGHorizAxis.ItemIndex:=2;
    end;
    Case VertAxis of
      aLeftAxis  : RGVertAxis.ItemIndex:=0;
      aRightAxis : RGVertAxis.ItemIndex:=1;
      aBothVertAxis: RGVertAxis.ItemIndex:=2;
    end;
    CBXDateTime.Checked:=XValues.DateTime;
    CBYDateTime.Checked:=YValues.DateTime;
  end;
  With CBSeriesCursor do
  begin
    Items.BeginUpdate;
    Clear;
    GetCursorValues(ProcGetCursors);
    Items.Add(TeeMsg_TeeHand);
    Items.EndUpdate;
  end;
  if TeeCursorToIdent(TheSeries.Cursor,tmpSt) then
     CBSeriesCursor.ItemIndex:=CBSeriesCursor.Items.IndexOf(tmpSt)
  else
     CBSeriesCursor.ItemIndex:=-1;
end;

procedure TChartEditForm.BMarkFontClick(Sender: TObject);
begin
  With TheSeries.Marks do Font:=InternalEditFont(Self,Font);
end;

procedure TChartEditForm.BMarksFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.Marks.Frame);
end;

procedure TChartEditForm.BMarkLinColClick(Sender: TObject);
begin
  EditChartPen(Self,TheSeries.Marks.Arrow);
end;

procedure TChartEditForm.BAxisPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Axis);
end;

procedure TChartEditForm.CBSeriesCursorChange(Sender: TObject);
var tmpCursor:Longint;
begin
  if TeeIdentToCursor(CBSeriesCursor.Items[CBSeriesCursor.ItemIndex], tmpCursor) then
     TheSeries.Cursor:=tmpCursor;
end;

procedure TChartEditForm.CBView3dClick(Sender: TObject);
begin
  if not CreatingForm then
  Begin
    TheChart.View3d:=CBView3d.Checked;
    Refresh3DControls;
  end;
end;

procedure TChartEditForm.Refresh3DControls;
begin
  With TheChart do
  begin
    SBRot.Enabled:=View3D and (not View3DOptions.Orthogonal);
    SBElev.Enabled:=SBRot.Enabled;
    SBPerspec.Enabled:=SBRot.Enabled;
    EnableControls(View3D,[ SE3D,CBOrthogonal,SBZoom,CBZoomFonts,
                            SBHorizOf,SBVertOf]);
  end;
end;

procedure TChartEditForm.RGTitalignClick(Sender: TObject);
begin
  if not CreatingForm then
  With SelectedTitle do
  Case RGTitAlign.ItemIndex of
    0: Alignment:=taLeftJustify;
    1: Alignment:=taCenter;
    2: Alignment:=taRightJustify;
  end;
end;

procedure TChartEditForm.RGVertAxisClick(Sender: TObject);
begin
  Case RGVertAxis.ItemIndex of
    0: TheSeries.VertAxis:=aLeftAxis;
    1: TheSeries.VertAxis:=aRightAxis;
    2: TheSeries.VertAxis:=aBothVertAxis;
  end;
end;

procedure TChartEditForm.BAxisTickPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheAxis.Ticks);
end;

procedure TChartEditForm.CBShowInLegendClick(Sender: TObject);
begin
  TheSeries.ShowInLegend:=CBShowInLegend.Checked;
end;

procedure TChartEditForm.SetCurrentDataSource;
begin
  if TheSource.SelectedDataSource=nil then
     CBListDataSets.ItemIndex:=-1
  else
     CBListDataSets.ItemIndex:=CBListDataSets.Items.IndexOfObject(TheSource.SelectedDataSource);
end;

procedure TChartEditForm.FillSourceDatasets;
begin
  CBListDataSets.Items.Clear;
  FillSources(CBListDataSets.Items,True);
  SetCurrentDataSource;
  ApplyChangesDataSet:=True;
end;

procedure TChartEditForm.FillSourceSeries;
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

procedure TChartEditForm.EnableDataSourceControls(IsDatabase,IsFunction:Boolean);
begin
  if IsDatabase then
  begin
    SetTabSeriesDatabase;
    PageControl2.PageIndex:=0;
  end
  else
  if IsFunction then
  begin
    SetTabSeriesFunctions;
    PageControl2.PageIndex:=1;
  end;
  PageControl2.Visible:=IsDatabase or IsFunction;
end;

procedure TChartEditForm.SetSelectedSourceSeries;
begin
end;

procedure TChartEditForm.SetSelectedSourceDatabase;
begin
  CBLabelsField.Items.Clear;
  CBLabelsField.Enabled:= (CBListDataSets.ItemIndex<>-1) and
                          (CBDataSourceStyle.ItemIndex=dsStyle_Dataset);
  TheSource.ClearCombos(CBListDataSets.ItemIndex<>-1);
  if CBListDataSets.ItemIndex=-1 then TheSource.SelectedDataSource:=nil
  else
  begin
    TheSource.SelectedDataSource:=TComponent(CBListDataSets.Items.Objects[CBListDataSets.ItemIndex]);
    if Assigned(TheSource.SelectedDataSource) then FillFields;
  end;
  CBLabelsField.ItemIndex:=CBLabelsField.Items.IndexOf(TheSource.SelectedXLabelsSource);
  TheSource.SetSeriesDatabaseSource(TheSeries);
end;

procedure TChartEditForm.CBListDataSetsChange(Sender: TObject);
begin
  SetSelectedSourceDatabase;
  ApplyChangesDataset:=True;
end;

procedure TChartEditForm.DoApplyChangesDataset;

  Procedure CheckFieldIsBlank(Const AFieldName:String);
  begin
    if AFieldName<>'' then
       Raise ChartException.CreateFmt(TeeMsg_FieldNotFound,[AFieldName]);
  end;

  Procedure CheckValidFields;
  var t:Longint;
  begin
    for t:=0 to TheSeries.ValuesLists.Count-1 do
    With TheSource.Values[t].CBValues do
         if ItemIndex=-1 then CheckFieldIsBlank(Text);
    With CBLabelsField do if ItemIndex=-1 then CheckFieldIsBlank(Text);
  end;

var t,tmp:Longint;
begin
  if Assigned(TheSeries) then
  With TheSeries do
  begin
    if TheSource.SelectedDataSource=nil then
       CBDataSourceStyle.ItemIndex:=dsStyle_NoData
    else
    begin
      CheckValidFields; { raise exception if non valid user input }
      DataSource:=nil;
      for t:=0 to ValuesLists.Count-1 do
      With ValuesLists.ValueList[t],TheSource.Values[t] do
      begin
        if CBValues.ItemIndex=-1 then ValueSource:=''
                                 else ValueSource:=SelectedValueSource;
        DateTime:=CBDateTime.Checked;
      end;
      With CBLabelsField do
      begin
        tmp:=Items.IndexOf(Text);
        if tmp<>ItemIndex then
        begin
          ItemIndex:=tmp;
          TheSource.SelectedXLabelsSource:=Items[ItemIndex];
        end;
        if ItemIndex=-1 then XLabelsSource:=''
                        else XLabelsSource:=TheSource.SelectedXLabelsSource;
      end;
      DataSource:=TheSource.SelectedDataSource;
    end;
  end;
  ApplyChangesDataSet:=False;
end;

procedure TChartEditForm.CBDateTimeClick(Sender: TObject);
begin
  ApplyChangesDataSet:=True;
end;

procedure TChartEditForm.CBValuesChange(Sender: TObject);
var tmp:Boolean;
begin
  With TComboBox(Sender),TheSource.Values[Tag] do
  begin
    if ItemIndex=-1 then SelectedValueSource:=''
                    else SelectedValueSource:=Items[ItemIndex];
    tmp:=CBDateTime.Checked;
    IsDateTimeSource(TheSource.SelectedDataSource,SelectedValueSource,tmp);
    CBDateTime.Checked:=tmp;
  end;
  ApplyChangesDataSet:=True;
end;

procedure TChartEditForm.CBLabelsFieldChange(Sender: TObject);
begin
  if CBLabelsField.ItemIndex=-1 then
     TheSource.SelectedXLabelsSource:=''
  else
     TheSource.SelectedXLabelsSource:=CBLabelsField.Items[CBLabelsField.ItemIndex];
  ApplyChangesDataSet:=True;
end;

procedure TChartEditForm.CBTickOnLabelsClick(Sender: TObject);
begin
  if not CreatingForm then
     TheAxis.TickOnLabelsOnly:=CBTickOnLabels.Checked;
end;

procedure TChartEditForm.BExportClick(Sender: TObject);
begin
  ChartExport(Self,TheChart);
end;

Procedure TChartEditForm.FillFields;  { virtual }
begin
  if Assigned(OnChartFillFields) then OnChartFillFields(Self);
end;

Procedure TChartEditForm.FillSources(AItems:TStrings; AddCurrent:Boolean);

  Procedure FillSourcesForm(AOwner:TComponent; Const AFormName:String);
  var t:Longint;
      AComponent:TComponent;
      tmp:String;
  begin
    if Assigned(AOwner) then
    for t:=0 to AOwner.ComponentCount-1 do
    begin
      AComponent:=AOwner.Components[t];
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
           else tmp:=AComponent.Name;
           if AFormName='' then
              AItems.AddObject(tmp,AComponent)
           else
              AItems.AddObject(AFormName+'.'+tmp,AComponent);
         end;
    end;
  end;

var t:Longint;
begin
  FillSourcesForm(TheSeries.ParentChart.Owner,'');
  FillSourcesForm(TheSeries.ParentChart,'');
end;

Function TChartEditForm.IsValidComponentSource(AComponent:TComponent):Boolean;
begin
  result:= (CBDataSourceStyle.ItemIndex=dsStyle_Function) and
           (AComponent is TChartSeries);
  if (not result) and (CBDataSourceStyle.ItemIndex=dsStyle_DataSet) then
     if Assigned(OnChartIsValidComponentSource) then
        result:=OnChartIsValidComponentSource(AComponent);
end;

Function TChartEditForm.IsDataSet:Boolean;
begin
  result:=False;
end;

procedure TChartEditForm.RGMarkStyleClick(Sender: TObject);
begin
  TheSeries.Marks.Style:=TSeriesMarksStyle(RGMarkStyle.ItemIndex);
end;

procedure TChartEditForm.RGPanningClick(Sender: TObject);
begin
  if not CreatingForm then TheChart.AllowPanning:=TPanningMode(RGPanning.ItemIndex);
end;

procedure TChartEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TheSource.Free;
  DestroySeriesForms;
end;

procedure TChartEditForm.SHWallColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  With SelectedWall do Color:=EditColor(Self,Color);
  RefreshWallColorShape(SelectedWall);
end;

procedure TChartEditForm.SEAniZoomStepsChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.AnimatedZoomSteps:=SEAniZoomSteps.Value;
end;

procedure TChartEditForm.RBLegendTopClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.Legend.Alignment:=laTop;
    EnableLegendMarginControls;
  end;
end;

procedure TChartEditForm.RBLegendLeftClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.Legend.Alignment:=laLeft;
    EnableLegendMarginControls;
  end;
end;

procedure TChartEditForm.RBLegendRightClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.Legend.Alignment:=laRight;
    EnableLegendMarginControls;
  end;
end;

procedure TChartEditForm.RBLegendBottomClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.Legend.Alignment:=laBottom;
    EnableLegendMarginControls;
  end;
end;

procedure TChartEditForm.CheckApplyChanges;
begin
  if ApplyChangesDataSet then DoApplyChangesDataSet
  else
  if ApplyChangesFunction then DoApplyChangesFunction;
end;

procedure TChartEditForm.BCloseClick(Sender: TObject);
begin
  CheckApplyChanges;
  ModalResult:=mrCancel;
end;

Function YesNoCancel(Const Message:String):Integer;
Begin
  Screen.Cursor:=crDefault;
  result:=MessageDlg(Message,mtConfirmation,mbYesNoCancel,0);
End;

procedure TChartEditForm.PageControlSeriesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange:=False;
  if (PageControlSeries.PageIndex=3) then
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
    else
    if {(Sender=TabSeries) and }(TheSeries=nil) then AllowChange:=False
                                                else AllowChange:=True;
  end
  else AllowChange:=True;
end;

procedure TChartEditForm.MainPageChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  PageControlSeriesChanging(Sender,AllowChange);
end;

procedure TChartEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  PageControlSeriesChanging(Sender,CanClose);
end;

procedure TChartEditForm.EnableListButtons;
begin
  EnableControls(LBAvailSeries.Items.Count>0,[BRightOne,BRightAll]);
  EnableControls(LBSelectedSeries.Items.Count>0,[BLeftOne,BLeftAll]);
end;

procedure TChartEditForm.BRightOneClick(Sender: TObject);
begin
  MoveList(LBAvailSeries,LBSelectedSeries);
  EnableListButtons;
  ApplyChangesFunction:=True;
end;

procedure TChartEditForm.BLeftOneClick(Sender: TObject);
begin
  MoveList(LBSelectedSeries,LBAvailSeries);
  EnableListButtons;
  ApplyChangesFunction:=True;
end;

procedure TChartEditForm.BRightAllClick(Sender: TObject);
begin
  MoveListAll(LBAvailSeries,LBSelectedSeries);
  EnableListButtons;
  ApplyChangesFunction:=True;
end;

procedure TChartEditForm.BLeftAllClick(Sender: TObject);
begin
  MoveListAll(LBSelectedSeries,LBAvailSeries);
  EnableListButtons;
  ApplyChangesFunction:=True;
end;

procedure TChartEditForm.LBAvailSeriesDblClick(Sender: TObject);
begin
  BRightOneClick(Self);
end;

procedure TChartEditForm.LBSelectedSeriesDblClick(Sender: TObject);
begin
  BLeftOneClick(Self);
end;

procedure TChartEditForm.CBFunctionsChange(Sender: TObject);
begin
  ApplyChangesFunction:=True;
end;

procedure TChartEditForm.DoApplyChangesFunction;
var t:Longint;
    tmp:TChartSeries;
    tmpClass:TTeeFunctionClass;
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

Function TChartEditForm.GetSelectedSource:TComponent;
begin
  result:=TheSource.SelectedDataSource;
end;

Function TChartEditForm.GetSourceCombo(Index:Integer):TComboBox;
begin
  result:=TheSource.Values[Index].CBValues;
end;

procedure TChartEditForm.Button1Click(Sender: TObject);
begin
  EditChartPen(Self,TChartLegend(TheChart.Legend).DividingLines);
end;

procedure TChartEditForm.CBDataSourceStyleChange(Sender: TObject);
begin
  PageControl2.Visible:= (CBDataSourceStyle.ItemIndex<>dsStyle_NoData) and
                         (CBDataSourceStyle.ItemIndex<>dsStyle_Random);
  Case CBDataSourceStyle.ItemIndex of
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
         TheSeries.DataSource:=nil;
         TheSeries.FillSampleValues(TheSeries.NumSampleValues);
       end;
   dsStyle_Function: EnableDataSourceControls(False,True);
    dsStyle_Dataset: EnableDataSourceControls(True,False);
  end;
end;

procedure TChartEditForm.CBTitlesChange(Sender: TObject);
begin
  SetTabTitle;
end;

procedure TChartEditForm.CBSeriesChange(Sender: TObject);

    Procedure CreateTheSeriesForms;
    var tmpClass:TFormClass;
    begin
      tmpClass:=TFormClass(GetClass(TheSeries.GetEditorClass));
      if Assigned(tmpClass) then
      begin
        Inc(NumSeriesForms);
        TheSeriesForms[NumSeriesForms]:=tmpClass.Create(Self);
        AddSeriesForm(TheSeriesForms[NumSeriesForms],0,Longint(TheSeries));
      end
      else DestroySeriesForms;
    end;

    Procedure HideSeriesPage;
    begin
      ImageSeries.Visible:=False;
      LabelSeriesClass.Caption:='';
      PageControlSeries.Visible:=False;
    end;

var tmpBitmap : TBitmap;
    OldTab    : Integer;
    tmp       : Boolean;
begin
  CheckApplyChanges;
  With CBSeries do
  if ItemIndex<>-1 then
  begin
    OldTab:=PageControlSeries.PageIndex;
    TheSeries:=TChartSeries(CBSeries.Items.Objects[ItemIndex]);
    if Assigned(TheSeries) then
    begin
      tmpBitmap:=TBitmap.Create;
      try
        TheSeries.GetBitmapEditor(tmpBitmap);
        ImageSeries.Picture.Assign(tmpBitmap);
      finally
        tmpBitmap.Free;
      end;
      LabelSeriesClass.Caption:=GetGallerySeriesName(TheSeries)+': '+TheSeries.Name; { <-- don't translate }
      ImageSeries.Visible:=True;
      if OldTab=-1 then PageControlSeries.PageIndex:=0
                   else PageControlSeries.PageIndex:=OldTab;
      PageControlSeriesChange(Self,PageControlSeries.PageIndex,tmp);
      PageControlSeries.Visible:=False;
      RGHorizAxis.Visible:=TheSeries.UseAxis;
      RGVertAxis.Visible:=TheSeries.UseAxis;
      CBXDateTime.Visible:=TheSeries.UseAxis;
      CBYDateTime.Visible:=TheSeries.UseAxis;
      DestroySeriesForms;
      CreateTheSeriesForms;
      SetTabSeriesDataSource;
      PageControlSeries.Visible:=True;
    end
    else HideSeriesPage;
  end
  else HideSeriesPage;
end;

Function TChartEditForm.SelectedSeriesIndex:Integer;
begin
  result:=LBSeries.ItemIndex;
end;

Procedure TChartEditForm.RefreshButtons;
var tmp:Boolean;
begin
  tmp:=TheChart.SeriesCount>0;
  BAddSeries.Enabled:=(ceAdd in EditorOptions);
  BDeleteSeries.Enabled:=tmp and (ceDelete in EditorOptions);
  BRenameSeries.Enabled:=tmp and (LBSeries.SelCount<2);
  BChangeTypeSeries.Enabled:=tmp and (ceChange in EditorOptions);
  BCloneSeries.Enabled:=tmp and (LBSeries.SelCount<2) and (ceClone in EditorOptions);
  if tmp and (LBSeries.SelCount<=1) then
  begin
    BMoveDown.Enabled:=SelectedSeriesIndex<LBSeries.Items.Count-1;
    BMoveUp.Enabled:=SelectedSeriesIndex>0;
  end
  else
  begin
    BMoveDown.Enabled:=False;
    BMoveUp.Enabled:=False;
  end;
end;

procedure TChartEditForm.SwapSeries(tmp1,tmp2:Longint);
begin
  with LBSeries do
  begin
    Items.Exchange(tmp1,tmp2);
    CBSeries.Items.Exchange(tmp1,tmp2);
    TheChart.ExchangeSeries(tmp1,tmp2);
    Self.ActiveControl:=LBSeries;
    LBSeries.Selected[tmp2]:=True;
    LBSeries.Repaint;
    RefreshButtons;
  end;
end;

procedure TChartEditForm.BMoveUPClick(Sender: TObject);
begin
  With LBSeries do
  if ItemIndex>0 then SwapSeries(ItemIndex,ItemIndex-1);
end;

procedure TChartEditForm.BMoveDownClick(Sender: TObject);
begin
  With LBSeries do
  if (ItemIndex<>-1) and (SelectedSeriesIndex<Items.Count-1) then
     SwapSeries(ItemIndex,ItemIndex+1);
end;

Function TChartEditForm.SelectedSeries:TChartSeries;
begin
  if (LBSeries.ItemIndex<>-1) and
     (LBSeries.ItemIndex<TheChart.SeriesCount) then
     result:=TChartSeries(LBSeries.Items.Objects[LBSeries.ItemIndex])
  else
     result:=nil;
end;

procedure TChartEditForm.FillSeries(OldSeries:TChartSeries);
var t,tmpSelectedIndex:Longint;
    tmpSeries:TChartSeries;
    tmpSt:String;
Begin
  With TheChart do
  Begin
    LBSeries.Items.Clear;
    CBSeries.Items.Clear;
    tmpSelectedIndex:=-1;
    for t:=0 to SeriesCount-1 do
    begin
      tmpSeries:=Series[t];
      if tmpSeries.Title<>'' then tmpSt:=tmpSeries.Title
                             else tmpSt:=tmpSeries.Name;
      LBSeries.Items.AddObject(tmpSt,tmpSeries);
      CBSeries.Items.AddObject(tmpSt,tmpSeries);
      if Assigned(OldSeries) and (tmpSeries=OldSeries) then
         tmpSelectedIndex:=t;
    end;
    if tmpSelectedIndex=-1 then
       if SeriesCount>0 then tmpSelectedIndex:=0;
    if tmpSelectedIndex<>-1 then
    begin
      LBSeries.Selected[tmpSelectedIndex]:=True;
    end
    else PageControlSeries.Visible:=False;
    RefreshButtons;
  end;
end;

procedure TChartEditForm.BAddSeriesClick(Sender: TObject);
var tmpSeries:TChartSeries;
    tmp:Boolean;
begin
  tmpSeries:=CreateNewSeriesGallery(TheChart.Owner,SelectedSeries,TheChart,True,True);
  if Assigned(tmpSeries) then
  begin
    FillSeries(tmpSeries);
    if tmpSeries.FunctionType<>nil then
    begin
      TheSeries:=tmpSeries;
      SetTabSeries;
      MainPage.PageIndex:=1;
      PageControlSeries.PageIndex:=3;
      PageControlSeriesChange(Self,PageControlSeries.PageIndex,tmp);
    end;
  end;
end;

procedure TChartEditForm.BDeleteSeriesClick(Sender: TObject);
var NewFocusSeries:TChartSeries;
    t,i:Longint;
    tmpSt:String;
begin
  if LBSeries.SelCount>0 then
  begin
    if LBSeries.SelCount=1 then tmpSt:=SelectedSeries.Name
                           else tmpSt:=TeeMsg_SelectedSeries;
    if MessageDlg( Format(TeeMsg_SureToDeleteSeries,[tmpSt]),
                   mtConfirmation,[mbYes,mbNo],0)=mrYes then
    begin
      NewFocusSeries:=nil;
      t:=0;
      for i:=0 to LBSeries.Items.Count-1 do
      begin
        if LBSeries.Selected[i] then
        begin
         TheChart[t].Free;
         if t>(TheChart.SeriesCount-1) then t:=TheChart.SeriesCount-1;
         if (t>=0) and (t<TheChart.SeriesCount) then
            NewFocusSeries:=TheChart[t]
         else
            NewFocusSeries:=nil;
        end
        else inc(t);
      end;
      FillSeries(NewFocusSeries);
    end;
  end;
end;

procedure TChartEditForm.BRenameSeriesClick(Sender: TObject);
var tmpSt:String;
    tmpSeries:TChartSeries;
    tmp:Longint;
begin
  tmp:=LBSeries.ItemIndex;
  if tmp<>-1 then
  begin
    tmpSeries:=SelectedSeries;
    tmpSt:=tmpSeries.Title;
    if tmpSt='' then tmpSt:=tmpSeries.Name;
    if InputQuery( TeeMsg_ChangeSeriesTitle,
                   TeeMsg_NewSeriesTitle,tmpSt) then
    if tmpSt<>'' then
    begin
      if tmpSt<>tmpSeries.Name then tmpSeries.Title:=tmpSt;
      LBSeries.Items[tmp]:=tmpSt;
      CBSeries.Items[tmp]:=tmpSt;
    end;
    Self.ActiveControl:=LBSeries;
    LBSeries.Selected[tmp]:=True;
  end;
end;

procedure TChartEditForm.BCloneSeriesClick(Sender: TObject);
begin
  if LBSeries.ItemIndex<>-1 then
     FillSeries(CloneChartSeries(SelectedSeries));
end;

procedure TChartEditForm.BChangeTypeSeriesClick(Sender: TObject);
var tmpSeries:TChartSeries;
    NewClass:TChartSeriesClass;
    t:Longint;
    FirstTime:Boolean;
begin
  if LBSeries.SelCount>0 then
  begin
    FirstTime:=True;
    NewClass:=nil;
    for t:=0 to LBSeries.Items.Count-1 do
    if LBSeries.Selected[t] then
    begin
      tmpSeries:=TheChart[t];
      if FirstTime then
      begin
        ChangeSeriesTypeGallery(tmpSeries.Owner,tmpSeries);
        NewClass:=TChartSeriesClass(tmpSeries.ClassType);
        FirstTime:=False;
      end
      else ChangeSeriesType(tmpSeries,NewClass);
    end;
    FillSeries(tmpSeries);
  end;
end;

Function TChartEditForm.SeriesAtMousePos(Var p:TPoint):Longint;
begin
  GetCursorPos(p);
  p:=LBSeries.ScreenToClient(p);
  result:=LBSeries.ItemAtPos(p,True);
end;

Function TChartEditForm.PointInSection(Const P:TPoint; ASection:Longint):Boolean;
Var tmpPos:Longint;
begin
  Case ASection of
    0: tmpPos:=0;
    1: tmpPos:=28;
    2: tmpPos:=44;
    3: tmpPos:=58;
  end;
  result:=(p.x>tmpPos) and (p.x<tmpPos+HeaderControl1.SectionWidth[ASection]);
end;

procedure TChartEditForm.LBSeriesDblClick(Sender: TObject);
var p:TPoint;
    tmp:Longint;
begin
  ComingFromDoubleClick:=True;
  tmp:=SeriesAtMousePos(p);
  if (tmp<>-1) and LBSeries.Selected[tmp] then
  begin
    if PointInSection(p,0) then BChangeTypeSeriesClick(Self)
    else
    if PointInSection(p,2) then
    begin
      With TheChart[tmp] do
      if not ColorEachPoint then
      begin
        SeriesColor:=EditColor(Self,SeriesColor);
        LBSeries.Repaint;
      end;
    end
    else
    if PointInSection(p,3) then BEditSeriesClick;
  end;
end;

procedure TChartEditForm.LBSeriesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp1,tmp2:Longint;
begin
  With TListBox(Sender) do
  if ItemIndex<>-1 then
  begin
    tmp1:=ItemIndex;
    tmp2:=ItemAtPos(Point(X,Y),True);
    if (tmp2<>-1) and (tmp1<>tmp2) then SwapSeries(tmp1,tmp2);
  end;
end;

procedure TChartEditForm.LBSeriesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var tmp:Integer;
    tmpBitmap:TBitmap;
    tmpSeries:TChartSeries;
    tmpR,CBRect:TRect;
    tmpCanvas:TCanvas;
    OldCanvas:TCanvas;
begin
  tmpCanvas:=LBSeries.Canvas;
  With tmpCanvas do
  begin
    tmpSeries:=TheChart[Index];

    if odSelected in State then
       Brush.Color:=clHighLight
    else
       Brush.Color:=clWindow;
    FillRect(Rect);

    Brush.Color:=TheChart.Legend.Color;
    Brush.Style:=bsSolid;
    tmpR.Top    :=Rect.Top;
    tmpR.Bottom :=Rect.Bottom;
    tmpR.Left   :=Rect.Left;
    tmpR.Right  :=46{HeaderControl1.Sections[2].Left}+HeaderControl1.SectionWidth[2]-4;
    FillRect(tmpR);

    tmp:=2{HeaderControl1.Sections[0].Left};
    tmpBitmap:=TBitmap.Create;
    try
      tmpSeries.GetBitmapEditor(tmpBitmap);
      Draw(tmp,Rect.Top+2,tmpBitmap);
    finally
      tmpBitmap.Free;
    end;

    if not tmpSeries.ColorEachPoint then
    begin
      tmp:=44{HeaderControl1.Sections[2].Left}-2;
      tmpR:=Classes.Rect(tmp,Rect.Top,tmp+HeaderControl1.SectionWidth[2],Rect.Bottom);
      InflateRect(tmpR,-4,-4);

      Brush.Style:=bsSolid;
      Brush.Color:=TheChart.Legend.Color;
      With TheChart do
      begin
        OldCanvas:=Canvas.ReferenceCanvas;
        Canvas.ReferenceCanvas:=tmpCanvas;
        try
          Series[Index].DrawLegend(-1,tmpR);
        finally
          Canvas.ReferenceCanvas:=OldCanvas;
        end;
      end;
    end;

    tmp:=28{HeaderControl1.Sections[1].Left};
    CBRect:=Classes.Rect(tmp,Rect.Top+6,tmp+12,Rect.Top+18);
    Brush.Style:=bsClear;
    SetBkMode(Handle,Transparent);
    Pen.Color:=clBlack;
    Pen.Style:=psSolid;
    With CBRect do Rectangle(Left,Top,Right,Bottom);
    if tmpSeries.Active then
    With CBRect do
    begin
      MoveTo(Left,Top);
      LineTo(Right,Bottom);
      MoveTo(Left,Bottom-1);
      LineTo(Right,Top-1);
    end;

    tmp:=80;
    if odSelected in State then
       Font.Color:=clHighlightText
    else
       Font.Color:=clBlack;
    Brush.Style:=bsClear;
    TextOut(tmp,Rect.Top+6,LBSeries.Items[Index]);
  end;
end;

procedure TChartEditForm.LBSeriesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Sender=Source;
end;

procedure TChartEditForm.LBSeriesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_DELETE then BDeleteSeriesClick(Self);
end;

procedure TChartEditForm.SetTabSeries;
begin
  if TheChart.SeriesCount=0 then
  begin
    TheSeries:=nil;
    PageControlSeries.Visible:=False;
  end
  else
  begin
    TheSeries:=SelectedSeries;
    CBSeries.ItemIndex:=CBSeries.Items.IndexOfObject(TheSeries);
    CBSeriesChange(Self);
    if Assigned(InternalOnCreateEditSeries) then InternalOnCreateEditSeries(Self,TheChart);
  end;
end;

Procedure TChartEditForm.IsDateTimeSource( AComponent:TComponent;
                                           Const ValueSource:String;
                                           Var IsDateTime:Boolean);
begin
  if Assigned(OnChartIsDateTimeSource) then
     OnChartIsDateTimeSource(AComponent,ValueSource,IsDateTime);
end;

procedure TChartEditForm.LBSeriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp:Longint;
    p:TPoint;
begin
  if (LBSeries.ItemIndex<>-1) then
  begin
    tmp:=SeriesAtMousePos(p);
    if (tmp<>-1) and (PointInSection(p,1)) then
    begin
      with TheChart[tmp] do Active:=not Active;
      LBSeries.Repaint;
    end
    else
    if PointInSection(p,3) and (not ComingFromDoubleClick) and
       (not (ssShift in Shift)) and
       (not (ssCtrl in Shift)) then
            LBSeries.BeginDrag(False);
    ComingFromDoubleClick:=False;
    CBSeries.ItemIndex:=LBSeries.ItemIndex;
  end;
end;

procedure TChartEditForm.BBrowseImageClick(Sender: TObject);
begin
  if TheChart.BackImage.Graphic<>nil then
     TheChart.BackImage.Assign(nil)
  else
  With TOpenDialog.Create(Self) do
  try
    DefaultExt:=TeeGetImageExtension(0);
    Filter:=GraphicFilter(TBitmap);
    Options:=Options+[ofHideReadOnly];
    if Execute then
       TheChart.BackImage.LoadFromFile(FileName);
    EnableImageControls;
  finally
    Free;
  end;
  EnableImageControls;
end;

procedure TChartEditForm.CBImageInsideClick(Sender: TObject);
begin
  TheChart.BackImageInside:=CBImageInside.Checked;
end;

procedure TChartEditForm.CBXDateTimeClick(Sender: TObject);
begin
  TheSeries.XValues.DateTime:=CBXDateTime.Checked;
end;

procedure TChartEditForm.CBYDateTimeClick(Sender: TObject);
begin
  TheSeries.YValues.DateTime:=CBYDateTime.Checked;
end;

procedure TChartEditForm.LBSeriesClick(Sender: TObject);
begin
  RefreshButtons;
end;

procedure TChartEditForm.SE3dChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.Chart3DPercent:=SE3d.Value;
end;

Function TChartEditForm.ChangeMargin(AEdit:TSpinEdit; APos,OtherSide:Integer):Integer;
begin
  result:=APos;
  if AEdit.Value+OtherSide<100 then result:=AEdit.Value
                               else AEdit.Value:=APos;
end;

procedure TChartEditForm.DestroySeriesForms;
var t       : Integer;
    tmp     : Integer;
    tmpPage : TTabPage;
begin
  for t:=1 to NumSeriesForms do
  if Assigned(TheSeriesForms[t]) then
  begin
    tmpPage:=(TheSeriesForms[t].Parent as TTabPage);
    TheSeriesForms[t].Free;
    TheSeriesForms[t]:=nil;
    tmp:=PageControlSeries.GetIndexForPage(tmpPage.Caption);
    if tmp>0 then
       if not (csDestroying in PageControlSeries.ComponentState) then
          PageControlSeries.Pages.Delete(tmp);
  end;
  NumSeriesForms:=0;
end;

Procedure TChartEditForm.AddSeriesForm(AForm:TForm; ATabIndex:Integer; ATag:Longint);
begin
  With AForm do
  begin
    Position:=poDesigned;
    BorderStyle:=bsNone;
    BorderIcons:=[];
    Tag:=ATag;
    Parent:=TTabPage(PageControlSeries.Pages.Objects[ATabIndex]);
    Font.Assign(Panel1.Font);
    Left:=((Parent.ClientWidth-ClientWidth) div 2);
    Top:=MinLong(8,Abs(Parent.ClientHeight-ClientHeight) div 2);
    Show;
  end;
end;

Function TChartEditForm.InsertSeriesForm( EditorClass:TFormClass;
                                          Position:Integer;
                                          Const EditorTabName:String;
                                          AnObject:TPersistent):TForm;
Var OldTab:Integer;
begin
  OldTab:=PageControlSeries.PageIndex;
  PageControlSeries.Pages.Insert(Position,EditorTabName);
  result:=EditorClass.Create(Self);
  Inc(NumSeriesForms);
  TheSeriesForms[NumSeriesForms]:=result;
  AddSeriesForm(result,Position,Longint(AnObject));
  if OldTab=-1 then PageControlSeries.PageIndex:=0
               else PageControlSeries.PageIndex:=OldTab;
end;

procedure TChartEditForm.SERightMaChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do MarginRight:=ChangeMargin(SERightMa,MarginRight,MarginLeft);
end;

procedure TChartEditForm.SETopMaChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do MarginTop:=ChangeMargin(SETopMa,MarginTop,MarginBottom);
end;

procedure TChartEditForm.SEBotMaChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do MarginBottom:=ChangeMargin(SEBotMa,MarginBottom,MarginTop);
end;

procedure TChartEditForm.SELeftMaChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do MarginLeft:=ChangeMargin(SELeftMa,MarginLeft,MarginRight);
end;

procedure TChartEditForm.PageControlAxisChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  Case NewTab of
    0: SetAxisScales;
    1: SetAxisTitle;
    2: SetAxisLabels;
    3: SetAxisTicks;
    4: SetAxisPosition;
  end;
end;

procedure TChartEditForm.MainPageChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  MainPageChanging(Sender,AllowChange);
  CreatingForm:=True;
  if NewTab=1 then SetTabSeries;
  CreatingForm:=False;
end;

procedure TChartEditForm.PageControlSeriesChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
var t:Integer;
begin
  PageControlSeriesChanging(Self,AllowChange);
  CreatingForm:=True;
  for t:=1 to NumSeriesForms do
      if NewTab=(t-1) then
         if TheSeriesForms[t]<>nil then TheSeriesForms[t].Show;
  if NewTab=NumSeriesForms then SetTabSeriesGeneral else
  if NewTab=NumSeriesForms+1 then SetTabSeriesMarks else
  if NewTab=NumSeriesForms+2 then SetTabSeriesDataSource;
  CreatingForm:=False;
end;

procedure TChartEditForm.TabSubWallsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  CreatingForm:=True;
  if NewTab=0 then SetTabWalls(TheChart.LeftWall) else
  if NewTab=1 then SetTabWalls(TheChart.BottomWall) else
                   SetTabWalls(TheChart.BackWall);
  CreatingForm:=False;
end;

procedure TChartEditForm.CBOrthogonalClick(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.View3DOptions.Orthogonal:=CBOrthogonal.Checked;
    Refresh3DControls;
  end;
end;

procedure TChartEditForm.SBZoomChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheChart.View3DOptions.Zoom:=SBZoom.Position;
    LZoom.Caption:=IntToStr(TheChart.View3DOptions.Zoom)+'%';
  end;
end;

procedure TChartEditForm.CBMultiLineClick(Sender: TObject);
begin
  if not CreatingForm then
     TheAxis.LabelsMultiLine:=CBMultiLine.Checked;
end;

procedure TChartEditForm.CBGridCenterClick(Sender: TObject);
begin
  if not CreatingForm then
     TheAxis.GridCentered:=CBGridCenter.Checked;
end;

procedure TChartEditForm.CBLeftWallTransClick(Sender: TObject);
begin
  if not CreatingForm then
  With SelectedWall.Brush do
  if (Sender as TCheckBox).Checked then Style:=bsClear
                                   else Style:=bsSolid;
end;

procedure TChartEditForm.SBRotChange(Sender: TObject);
begin
  TheChart.View3DOptions.Rotation:=SBRot.Position;
  LRot.Caption:=IntToStr(TheChart.View3DOptions.Rotation);
end;

procedure TChartEditForm.SBElevChange(Sender: TObject);
begin
  TheChart.View3DOptions.Elevation:=SBElev.Position;
  LElev.Caption:=IntToStr(TheChart.View3DOptions.Elevation);
end;

procedure TChartEditForm.SBHorizOfChange(Sender: TObject);
begin
  TheChart.View3DOptions.HorizOffset:=SBHorizOf.Position;
  LHorizOf.Caption:=IntToStr(TheChart.View3DOptions.HorizOffset);
end;

procedure TChartEditForm.SBVertOfChange(Sender: TObject);
begin
  TheChart.View3DOptions.VertOffset:=SBVertOf.Position;
  LVertOf.Caption:=IntToStr(TheChart.View3DOptions.VertOffset);
end;

procedure TChartEditForm.SEAxisPosChange(Sender: TObject);
begin
  if not CreatingForm then TheAxis.PositionPercent:=SEAxisPos.Value;
end;

procedure TChartEditForm.SEAxisStartChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    if SEAxisStart.Value<TheAxis.EndPosition then
       TheAxis.StartPosition:=SEAxisStart.Value
    else
       SEAxisStart.Value:=Round(TheAxis.StartPosition);
  end;
end;

procedure TChartEditForm.SEAxisEndChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    if SEAxisEnd.Value>TheAxis.StartPosition then
       TheAxis.EndPosition:=SEAxisEnd.Value
    else
       SEAxisEnd.Value:=Round(TheAxis.EndPosition);
  end;
end;

procedure TChartEditForm.CBZoomFontsClick(Sender: TObject);
begin
  if not CreatingForm then
    TheChart.View3DOptions.ZoomText:=CBZoomFonts.Checked;
end;

procedure TChartEditForm.CBWallDarkBackClick(Sender: TObject);
begin
  SelectedWall.Dark3D:=(Sender as TCheckBox).Checked
end;

procedure TChartEditForm.Button5Click(Sender: TObject);
begin
  With SelectedTitle do Color:=EditColor(Self,Color);
end;

procedure TChartEditForm.SBPerspecChange(Sender: TObject);
begin
  TheChart.View3DOptions.Perspective:=SBPerspec.Position;
  LPerspec.Caption:=IntToStr(TheChart.View3DOptions.Perspective);
end;

initialization
  RegisterClass(TChartEditForm);
end.
