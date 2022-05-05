{*********************************************}
{  TeeChart Designer Example                  }
{  Copyright (c) 1997 by David Berneda        }
{  All rights reserved                        }
{*********************************************}
unit Main;

interface

{ The Chart designer application allows you to create, store, reload,
  edit, print, copy to clipboard, etc CHART files  ( *.TEE files ).

  It also connects Series values to a StringGrid component for easy
  point values editing.
}
uses
  SysUtils, Wintypes,WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus,
  Teengine, Chart, Series, TeeProcs, Grids,
  TeeGrid           { <-- to show points at TStringGrid }
  ;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FilePrintSetupItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditCopyItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SpeedBar: TPanel;
    SpeedButton1: TSpeedButton;  { &New }
    SpeedButton2: TSpeedButton;  { &Open... }
    SpeedButton3: TSpeedButton;  { &Save }
    SpeedButton4: TSpeedButton;  { Save &As... }
    SpeedButton5: TSpeedButton;  { P&rint Setup... }
    SpeedButton7: TSpeedButton;  { E&xit }
    SpeedButton8: TSpeedButton;  { &Arrange All }
    SpeedButton13: TSpeedButton;
    CopyasMetafile1: TMenuItem;
    SpeedButton6: TSpeedButton;
    StringGrid1: TStringGrid;
    Chart1: TChart;
    Panel1: TPanel;
    Panel2: TPanel;
    ShowColors: TCheckBox;
    ShowXValues: TCheckBox;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    ShowLabels: TCheckBox;  { &About... }
    procedure FormCreate(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure HelpContents(Sender: TObject);
    procedure HelpSearch(Sender: TObject);
    procedure HelpHowToUse(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure CopyasMetafile1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Longint; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Longint;
      const Value: String);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure ShowXValuesClick(Sender: TObject);
    procedure ShowColorsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    { current editing chart file name and path }
    CurrentName:String;
    { has been the current chart modified ? }
    Modified,
    { is the user dragging the splitter panel ? }
    Splitting:Boolean;
    { temporary variable to use the grid... }
    ColumnValues:TTeeGridColumnValues;
    { show any error when loading *.tee files... }
    Function CheckLoadingErrors(const Message: string): Boolean;
    { refill the stringgrid... }
    Procedure RefreshGrid;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
uses teestore,          { <-- to load / save Charts }
     EditChar,EditPro,  { <-- to access the Editor Dialog }
     ChartPro,          { <-- to support the Pro Series }
     TeePrevi,          { <-- to print preview Charts }
     TeeAbout;

{ add some random values for new Charts... }
Procedure FillSamples(ASeries:TChartSeries);
Const
  NumSamples=6;
  ALabel:Array[1..NumSamples] of String=('Anne','John','Mary','Mike','Dave','Charles');
Var t:Integer;
begin
  for t:=1 to NumSamples do ASeries.Add(Random(1000),ALabel[t],clTeeColor);
end;

{ show a new Chart on editor... }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  CurrentName:='';
  Modified:=False;
  Splitting:=False;
  FileNew(Self);
end;

{ ask user a question... }
Function YesNo(Const S:String):Boolean;
Var St:Array[0..255] of Char; { 16bit compatibility }
begin
  result:=Application.MessageBox(StrPCopy(St,S),'Warning!',MB_YESNO)=IDYES;
end;

{ create a new chart... }
procedure TMainForm.FileNew(Sender: TObject);
begin
  if (not Modified) or YesNo('Chart has been modified. Are you sure?') then
  begin
    Chart1.Free;
    Chart1:=TChart.Create(Self);
    With Chart1 do
    begin
      Parent:=Self;
      Align:=alClient;
      AddSeries(TBarSeries.Create(Self));
      With Series[0] do
      begin
        Name:='Series1';
        ColorEachPoint:=True;
        Marks.Style:=smsValue;
      end;
      FillSamples(Series[0]);
      OnClickSeries:=Chart1ClickSeries;
    end;
    { fill the grid with values... }
    RefreshGrid;
    Modified:=False;
  end;
end;

{ open an existing *.tee file... }
procedure TMainForm.FileOpen(Sender: TObject);
var tmpChart:TCustomChart;
begin
  if (not Modified) or YesNo('Chart has been modified. Are you sure?') then
  begin
    OpenDialog.FileName:='*.tee';
    if OpenDialog.Execute then
    begin
      { load the chart... }
      Chart1.Free;
      tmpChart:=TChart.Create(Self);
      LoadChartFromFileCheck(tmpChart,OpenDialog.FileName,CheckLoadingErrors);
      Chart1:=tmpChart as TChart;
      Chart1.OnClickSeries:=Chart1ClickSeries;
      Chart1.Align:=alClient;
      Chart1.Parent:=Self;
      { fill the grid with values... }
      RefreshGrid;
      Modified:=False;
      CurrentName:=OpenDialog.FileName;
    end;
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  if CurrentName='' then FileSaveAs(Self)
                    else
                    begin
                      SaveChartToFile(Chart1,CurrentName);
                      Modified:=False;
                    end;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    SaveChartToFile(Chart1,SaveDialog.FileName);
    CurrentName:=SaveDialog.FileName;
    Modified:=False;
  end;
end;

procedure TMainForm.FilePrint(Sender: TObject);
begin
  ChartPreview(Self,Chart1);
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
  if (not Modified) or YesNo('Chart has been modified. Are you sure?') then
     Close;
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap ;
end;

procedure TMainForm.HelpContents(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TMainForm.HelpSearch(Sender: TObject);
const EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

procedure TMainForm.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  With TTeeAboutForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.CopyasMetafile1Click(Sender: TObject);
begin
  Chart1.CopyToClipboardMetafile(True);
end;

procedure TMainForm.SpeedButton6Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
  RefreshGrid;
  Modified:=True;
end;

procedure TMainForm.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Splitting then
  begin
    StringGrid1.Height:=StringGrid1.Height-Y;
    Splitting:=False;
  end;
end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Splitting:=True;
end;

{ Select a Grid cell when the user clicks a Series point... }
procedure TMainForm.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Longint; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StringGrid1.Col:=ColumnValues.FirstColSeries+Chart1.SeriesList.IndexOf(Series);
  StringGrid1.Row:=1+ValueIndex;
end;

{ Change the Series point values when user types in the grid... }
procedure TMainForm.StringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Longint; const Value: String);
begin
  Modified:=True;
  TeeModifyGrid(Chart1,ACol,ARow,Value,ColumnValues);
end;

Function TMainForm.CheckLoadingErrors(const Message: string): Boolean;
begin
  result:=True;
{  result:=YesNo(Message); }  { <- for debug }
end;

procedure TMainForm.SpeedButton9Click(Sender: TObject);
Var t:Integer;
begin
  if StringGrid1.Col>0 then
  begin
    for t:=0 to Chart1.SeriesCount-1 do Chart1[t].Add(0,'',clTeeColor);
    RefreshGrid;
  end;
end;

procedure TMainForm.SpeedButton10Click(Sender: TObject);
Var t,ValueIndex:Integer;
begin
  if (StringGrid1.Col>0) and (StringGrid1.Row>0) then
  begin
    ValueIndex:=StringGrid1.Row-1;
    for t:=0 to Chart1.SeriesCount-1 do
    begin
      if Chart1[t].Count>ValueIndex then
      begin
        Chart1[t].Delete(ValueIndex);
        if not HasNoMandatoryValues(Chart1[t]) then
           NotMandatoryList(Chart1[t]).FillSequence;
      end;
    end;
    RefreshGrid;
  end;
end;

procedure TMainForm.ShowXValuesClick(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TMainForm.ShowColorsClick(Sender: TObject);
begin
  RefreshGrid;
end;

Procedure TMainForm.RefreshGrid;
var tmp:TChartGridOptions;
begin
  tmp:=[];
  if ShowLabels.Checked then tmp:=tmp+[cgLabels];
  if ShowColors.Checked then tmp:=tmp+[cgColors];
  if ShowXValues.Checked then tmp:=tmp+[cgXValues];
  TeeFillGrid(StringGrid1,Chart1,tmp,ColumnValues);
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=(not Modified) or YesNo('Chart has been modified. Are you sure to EXIT?');
end;

end.
