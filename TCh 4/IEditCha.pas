{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEditCha;
{$S-,W-,R-,P-,H+}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Chart, Buttons, AxMaxMin, AxisIncr, TeEngine, TeCanvas,
  StdCtrls, ExtCtrls, ComCtrls, TeeFunci, TeeProcs, IEdiSeri, TeeLisB;

Const teeEditMainPage    =0;
      teeEditGeneralPage =1;
      teeEditAxisPage    =2;
      teeEditTitlePage   =3;
      teeEditLegendPage  =4;
      teeEditPanelPage   =5;
      teeEditPagingPage  =6;
      teeEditWallsPage   =7;
      teeEdit3DPage      =8;

type
  TChartEditorOption=( ceAdd,
                       ceDelete,
                       ceChange,
                       ceClone,
                       ceDataSource,
                       ceTitle,
                       ceHelp  );

Const eoAll=[ ceAdd,
              ceDelete,
              ceChange,
              ceClone,
              ceDataSource,
              ceTitle,
              ceHelp ];

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

  TChartEditorOptions=set of TChartEditorOption;
  TChartEditorHiddenTabs=set of TChartEditorTab;

  TChartEditForm = class(TForm)
    MainPage: TPageControl;
    TabChart: TTabSheet;
    Notebook1: TPageControl;
    TabSeriesList: TTabSheet;
    TabAxis: TTabSheet;
    TabGeneral: TTabSheet;
    TabTitle: TTabSheet;
    TabLegend: TTabSheet;
    TabPanel: TTabSheet;
    TabPaging: TTabSheet;
    TabWalls: TTabSheet;
    BClose: TButton;
    TabSeries: TTabSheet;
    Header1: THeaderControl;
    BMoveUP: TBitBtn;
    BMoveDown: TBitBtn;
    BAddSeries: TButton;
    BDeleteSeries: TButton;
    BRenameSeries: TButton;
    BCloneSeries: TButton;
    BChangeTypeSeries: TButton;
    Tab3D: TTabSheet;
    LBSeries: TChartListBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Notebook1Change(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure MainPageChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure BMoveUPClick(Sender: TObject);
    procedure BMoveDownClick(Sender: TObject);
    procedure BAddSeriesClick(Sender: TObject);
    procedure BDeleteSeriesClick(Sender: TObject);
    procedure BRenameSeriesClick(Sender: TObject);
    procedure BCloneSeriesClick(Sender: TObject);
    procedure MainPageChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LBSeriesEditSeries(Sender: TChartListBox; Index: Integer);
    procedure LBSeriesOtherItemsChange(Sender: TObject);
    procedure LBSeriesRefreshButtons(Sender: TObject);
    procedure BChangeTypeSeriesClick(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF TEEHELPEDITOR}
    Procedure HelpClick(Sender:TObject);
    {$ENDIF}
  protected
    {$IFDEF TEEHELPEDITOR}
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    {$ENDIF}
  public
    { Public declarations }
    TheChart           : TCustomChart;
    TheAxis            : TCustomChartAxis;
    TheTitle           : TChartTitle;
    TheWall            : TChartWall;
    TheSeries          : TChartSeries;
    TheEditSeries      : TChartSeries;
    TheActivePageIndex : Integer;
    TheHiddenTabs      : TChartEditorHiddenTabs;

    EditorOptions      : TChartEditorOptions;
    IsDssGraph         : Boolean;
    TheFormSeries      : TFormTeeSeries;
    ButtonHelp         : TButton;
    procedure SetTabSeries;
    {$IFDEF TEEHELPEDITOR}
    Procedure CheckHelpFile;
    {$ENDIF}
  end;

{$IFDEF TEEHELPEDITOR}
Function GetTeeChartHelpFile:String;
Function GetTeeChartUserHelpFile:String;
{$ENDIF}

Type TOnCreateEditSeries=Procedure(Sender:TFormTeeSeries; AChart:TCustomChart) {$IFDEF TEEOCX}of object{$ENDIF};
Var InternalOnCreateEditSeries:TOnCreateEditSeries;

implementation

{$R *.DFM}

uses Dialogs,PenDlg,TeeGally,TeeConst,Series,
     IEdiAxis,IEdiLege,IEdiPane,IEdiTitl,IEdiWall,IEdiGene,IEdiPage,IEdi3D
     {$IFDEF TEEHELPEDITOR}
     ,Registry
     {$ENDIF}
     ;

{ TChartEditForm }
{$IFDEF TEEHELPEDITOR}
Procedure TChartEditForm.CheckHelpFile;
begin
  if ceHelp in EditorOptions then
  begin
    if csDesigning in TheChart.ComponentState then
       HelpFile:=GetTeeChartHelpFile
    else
       HelpFile:=GetTeeChartUserHelpFile;
    if HelpFile<>'' then
    begin
      BorderIcons:=BorderIcons+[biHelp];
      ButtonHelp:=TButton.Create(Self);
      With ButtonHelp do
      begin
        Parent:=Self;
        Left:=31;
        Width:=BClose.Width;
        Top:=BClose.Top;
        Caption:=TeeMsg_HelpButton;
        OnClick:=HelpClick;
      end;
    end;
  end
  else HelpFile:='';
end;
{$ENDIF}

procedure TChartEditForm.FormShow(Sender: TObject);

  Function GetEditingCaption(AChart:TCustomChart):String;
  begin
    FmtStr(result,TeeMsg_Editing,[AChart.Name]);
  end;

  Procedure HideTabs;
  begin
    if cetMain in TheHiddenTabs then TabSeriesList.TabVisible:=False;
    if cetGeneral in TheHiddenTabs then TabGeneral.TabVisible:=False;
    if cetAxis in TheHiddenTabs then TabAxis.TabVisible:=False;
    if cetTitles in TheHiddenTabs then TabTitle.TabVisible:=False;
    if cetLegend in TheHiddenTabs then TabLegend.TabVisible:=False;
    if cetPanel in TheHiddenTabs then TabPanel.TabVisible:=False;
    if cetPaging in TheHiddenTabs then TabPaging.TabVisible:=False;
    if cetWalls in TheHiddenTabs then TabWalls.TabVisible:=False;
    if cet3D in TheHiddenTabs then Tab3D.TabVisible:=False;
  end;

var t:Integer;
begin
  Screen.Cursor:=crDefault;
  LBSeries.Chart:=TheChart;
  With LBSeries.Sections do
  begin
    Clear;
    for t:=0 to Header1.Sections.Count-1 do
        AddSection(Header1.Sections[t].Width);
  end;

  if Caption='' then Caption:=GetEditingCaption(TheChart);
  HideTabs;

  if TheActivePageIndex<>-1 then
  begin
    if NoteBook1.Pages[TheActivePageIndex].TabVisible then
    begin
      Notebook1.ActivePage:=NoteBook1.Pages[TheActivePageIndex];
      Notebook1Change(Self);
    end;
  end;

  LBSeries.FillSeries(TheEditSeries);
  if Assigned(TheEditSeries) then LBSeriesEditSeries(LBSeries,0);
end;

{$IFDEF TEEHELPEDITOR}
Function GetRegistryHelpPath(Const HelpFile:String):String;
begin
  result:='';
  With TRegistry.Create do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey('SOFTWARE\Microsoft\Windows\Help',False) then
       result:=ReadString(HelpFile)+'\'+HelpFile;
  finally
    Free;
  end;
end;

Function GetTeeChartHelpFile:String;
begin
  {$IFDEF TEEOCX}
  result:=GetRegistryHelpPath('TeeChartX.hlp'); // <- do not translate
  {$ELSE}
  result:=GetRegistryHelpPath('TeeChart.hlp');  // <- do not translate
  {$ENDIF}
end;

Function GetTeeChartUserHelpFile:String;
begin
  {$IFDEF TEEOCX}
  result:=GetRegistryHelpPath('TeeUserX.hlp'); // <- do not translate
  {$ELSE}
  result:=GetRegistryHelpPath('TeeUser.hlp');  // <- do not translate
  {$ENDIF}
end;
{$ENDIF}

procedure TChartEditForm.FormCreate(Sender: TObject);
begin
  LBSeries.Chart:=TheChart;
  Caption:='';
  TheActivePageIndex:=-1;
  EditorOptions:=[ceAdd,ceDelete,ceChange,ceClone,ceDataSource,ceTitle,ceHelp];
  TheSeries    :=nil;
  TheEditSeries:=nil;
  TheChart     :=nil;
  TheTitle     :=nil;

  IsDssGraph:=False;

  MainPage.ActivePage:=TabChart;
  NoteBook1.ActivePage:=TabSeriesList;
  {$IFDEF D3}
  NoteBook1.HotTrack:=True;
  {$ENDIF}
end;

procedure TChartEditForm.Notebook1Change(Sender: TObject);
var tmpForm   : TForm;
    tmpSeries : TChartSeries;
begin
  With NoteBook1.ActivePage do
  if ControlCount=0 then
  begin
    Case PageIndex of
      teeEditGeneralPage: tmpForm:=TFormTeeGeneral.CreateChart(Self,TheChart);
      teeEditAxisPage   : begin
                            if TheAxis=nil then TheAxis:=TheChart.LeftAxis;
                            tmpForm:=TFormTeeAxis.CreateAxis(Self,TheAxis);
                          end;
      teeEditTitlePage  : begin
                            if TheTitle=nil then TheTitle:=TheChart.Title;
                            tmpForm:=TFormTeeTitle.CreateTitle(Self,TheTitle);
                          end;
      teeEditLegendPage : tmpForm:=TFormTeeLegend.CreateLegend(Self,TheChart.Legend);
      teeEditPanelPage  : tmpForm:=TFormTeePanel.CreateChart(Self,TheChart);
      teeEditPagingPage : tmpForm:=TFormTeePage.CreateChart(Self,TheChart);
      teeEditWallsPage  : begin
                            if TheWall=nil then TheWall:=TheChart.LeftWall;
                            tmpForm:=TFormTeeWall.CreateWall(Self,TheWall);
                          end;
    else
       tmpForm:=TFormTee3D.CreateChart(Self,TheChart);
    end;
    tmpForm.Align:=alClient;
    tmpForm.Parent:=NoteBook1.ActivePage;
    tmpForm.Show;
  end;
  if NoteBook1.ActivePage=Tab3D then
  begin
    tmpSeries:=TheChart.GetASeries;
    With TFormTee3D(Tab3D.Controls[0]) do
    begin
      AllowRotation:=TheChart.Canvas.SupportsFullRotation or
                     (not Assigned(tmpSeries)) or
                     (not (tmpSeries is TPieSeries));
      CheckRotation;
    end;
  end;
end;

procedure TChartEditForm.BCloseClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TChartEditForm.MainPageChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if Assigned(TheFormSeries) then
     TheFormSeries.PageSeriesChanging(Sender,AllowChange);
end;

procedure TChartEditForm.BMoveUPClick(Sender: TObject);
begin
  LBSeries.MoveCurrentUp;
end;

procedure TChartEditForm.BMoveDownClick(Sender: TObject);
begin
  LBSeries.MoveCurrentDown;
end;

procedure TChartEditForm.BAddSeriesClick(Sender: TObject);
var tmpSeries : TChartSeries;
begin
  tmpSeries:=LBSeries.AddSeriesGallery;
  if Assigned(tmpSeries) then
    if tmpSeries.FunctionType<>nil then
    begin
      TheSeries:=tmpSeries;
      MainPage.ActivePage:=TabSeries;
      SetTabSeries;
      With TheFormSeries do
      begin
        PageSeries.ActivePage:=TabDataSource;
        PageSeriesChange(Self);
      end;
    end;
end;

procedure TChartEditForm.BDeleteSeriesClick(Sender: TObject);
begin
  LBSeries.DeleteSeries;
end;

procedure TChartEditForm.BRenameSeriesClick(Sender: TObject);
begin
  LBSeries.RenameSeries;
  Self.ActiveControl:=LBSeries;
end;

procedure TChartEditForm.BCloneSeriesClick(Sender: TObject);
begin
  LBSeries.CloneSeries;
end;

procedure TChartEditForm.SetTabSeries;
begin
  if Assigned(TheFormSeries) then
     TheFormSeries.TheSeries:=LBSeries.SelectedSeries
  else
  begin
    TheFormSeries:=TFormTeeSeries.Create(Self);
    With TheFormSeries do
    begin
      Self.LBSeries.OtherItems:=CBSeries.Items;
      Parent:=TabSeries;
      {$IFDEF D2C1}
      SetBounds(Left,Top,TabSeries.ClientWidth,TabSeries.ClientHeight);
      {$ENDIF}
      ShowTabDataSource:=ceDataSource in EditorOptions;
      ShowTabGeneral:=not (cetSeriesGeneral in TheHiddenTabs);
      ShowTabMarks:=not (cetSeriesMarks in TheHiddenTabs);
      IsDssGraph:=Self.IsDssGraph;
      TheSeries:=Self.LBSeries.SelectedSeries;
      LBSeries.FillSeries(TheSeries);
      if Assigned(InternalOnCreateEditSeries) then
         InternalOnCreateEditSeries(TheFormSeries,TheChart);
      Show;
    end;
  end;
  TheFormSeries.SetCBSeries;
end;

procedure TChartEditForm.MainPageChange(Sender: TObject);
begin
  if MainPage.ActivePage=TabSeries then SetTabSeries;
end;

{$IFDEF TEEHELPEDITOR}
procedure TChartEditForm.WMHelp(var Message: TWMHelp);
var Control   : TWinControl;
    ContextID : Integer;
begin
  if biHelp in BorderIcons then
  with Message.HelpInfo^ do
  begin
    if iContextType = HELPINFO_WINDOW then
    begin
      Control := FindControl(hItemHandle);
      while (Control <> nil) and (Control.HelpContext = 0) do
        Control := Control.Parent;
      if Control = nil then Exit;
      ContextID := Control.HelpContext;
      Application.HelpCommand(HELP_CONTEXT, ContextID);
    end;
  end
  else inherited;
end;

Procedure TChartEditForm.HelpClick(Sender:TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, HelpContext);
end;
{$ENDIF}

procedure TChartEditForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(TheFormSeries) then TheFormSeries.Close
end;

procedure TChartEditForm.LBSeriesEditSeries(Sender: TChartListBox;
  Index: Integer);
begin
  if LBSeries.ItemIndex<>-1 then
  begin
    MainPage.ActivePage:=TabSeries;
    SetTabSeries;
  end;
end;

procedure TChartEditForm.LBSeriesOtherItemsChange(Sender: TObject);
begin
  if Assigned(TheFormSeries) then
     TheFormSeries.CBSeries.ItemIndex:=LBSeries.ItemIndex;
end;

procedure TChartEditForm.LBSeriesRefreshButtons(Sender: TObject);
var tmp          : Boolean;
    tmpInherited : Boolean;
    tmpSeries    : TChartSeries;
begin
  tmp:=TheChart.SeriesCount>0;
  if tmp then tmpSeries:=TheChart[LBSeries.ItemIndex]
         else tmpSeries:=nil;
  tmpInherited:=tmp and (csAncestor in tmpSeries.ComponentState);
  BAddSeries.Enabled:=(ceAdd in EditorOptions);
  BDeleteSeries.Enabled:= tmp and
                          (not tmpInherited) and
                          (ceDelete in EditorOptions)
                          {$IFDEF D3}
                          and
                          (not (tssIsTemplate in tmpSeries.Style)) and
                          (not (tssDenyDelete in tmpSeries.Style))
                          {$ENDIF}
                          ;
  BRenameSeries.Enabled:=tmp and (LBSeries.SelCount<2) and (ceTitle in EditorOptions);
  BChangeTypeSeries.Enabled:= tmp and
                              (not tmpInherited) and
                              (ceChange in EditorOptions)
                              {$IFDEF D3}
                              and
                              (not (tssDenyChangeType in tmpSeries.Style))
                              {$ENDIF}
                              ;
  BCloneSeries.Enabled:= tmp and
                         (LBSeries.SelCount<2) and
                         (ceClone in EditorOptions)
                         {$IFDEF D3}
                         and
                         (not (tssIsTemplate in tmpSeries.Style)) and
                         (not (tssDenyClone in tmpSeries.Style))
                         {$ENDIF}
                         ;

  if tmp and (LBSeries.SelCount<=1) then
  begin
    BMoveDown.Enabled:=LBSeries.ItemIndex<LBSeries.Items.Count-1;
    BMoveUp.Enabled:=LBSeries.ItemIndex>0;
  end
  else
  begin
    BMoveDown.Enabled:=False;
    BMoveUp.Enabled:=False;
  end;
end;

procedure TChartEditForm.BChangeTypeSeriesClick(Sender: TObject);
begin
  LBSeries.ChangeTypeSeries(Self);
end;

initialization
  InternalOnCreateEditSeries:=nil;
  RegisterClass(TChartEditForm);
end.
