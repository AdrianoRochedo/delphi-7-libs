{********************************************}
{  TeeChart Wizard                           }
{  Copyright (c) 1995-1998 by David Berneda  }
{  All Rights Reserved                       }
{********************************************}
{$I Teedefs.inc}
unit ExpForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ToolIntf, ComCtrls, TeEngine, Chart, DbChart,
  DB, DBTables, TeeGally,  DBCtrls, TeeProcs, TeeConst, DBCombo, BDE,
  {$IFDEF D3}
  dsnConst,ActiveX,
  {$ELSE}
  LibConst,
  {$ENDIF}
  dsnDBcst, FileCtrl;

type
  TSources = (stLocal, stAlias);

  TTeeDlgWizard = class(TForm)
    CancelBtn: TButton;
    PrevButton: TButton;
    NextButton: TButton;
    PageControl: TPageControl;
    TabStyle: TTabSheet;
    Label1: TLabel;
    TableName: TTabSheet;
    Label3: TLabel;
    Buttons: TTabSheet;
    Label2: TLabel;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    TabSheet2: TTabSheet;
    Table1: TTable;
    PanelGallery: TPanel;
    Panel1: TPanel;
    LBAvailFields: TListBox;
    LBSelectedFields: TListBox;
    Button3: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel2: TPanel;
    Label6: TLabel;
    cbLabelsFields: TComboBox;
    PageControl1: TPageControl;
    TabPreviewChart: TTabSheet;
    PreviewChart: TDBChart;
    Bevel3: TBevel;
    rbChart2D: TRadioButton;
    rbChart3D: TRadioButton;
    Panel3: TPanel;
    rbDatabase: TRadioButton;
    rbNonDatabase: TRadioButton;
    Label7: TLabel;
    Label8: TLabel;
    Panel7: TPanel;
    CB3DOption: TCheckBox;
    CBShowLegendOption: TCheckBox;
    CBShowMarksOption: TCheckBox;
    Button1: TButton;
    Panel8: TPanel;
    Label27: TLabel;
    DirPath: TLabel;
    FileEdit: TEdit;
    PathText: TLabel;
    AliasFileList: TAliasFileListBox;
    AliasPathList: TAliasListBox;
    FilterList: TFilterComboBox;
    Label26: TLabel;
    Label25: TLabel;
    AliasDriveList: TAliasDrive;
    Panel4: TPanel;
    Image2: TImage;
    LabelURL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure PrevClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure LBAvailFieldsClick(Sender: TObject);
    procedure LBSelectedFieldsClick(Sender: TObject);
    procedure rbChart3DClick(Sender: TObject);
    procedure rbChart2DClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure CB3DOptionClick(Sender: TObject);
    procedure CBShowLegendOptionClick(Sender: TObject);
    procedure CBShowMarksOptionClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FileEditChange(Sender: TObject);
    procedure FileEditKeyPress(Sender: TObject; var Key: Char);
    procedure AliasFileListClick(Sender: TObject);
    procedure AliasFileListDblClick(Sender: TObject);
    procedure AliasPathListChange(Sender: TObject);
    procedure FilterListChange(Sender: TObject);
    procedure AliasDriveListChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LabelURLClick(Sender: TObject);
  private
    { Private declarations }
    tmpGallery:TTeeGallery;
    SourceBuffer: PChar;
    procedure RefreshButtons;
    procedure FmtWrite(Stream: TStream; Const Fmt: String; const Args: array of const);
    function DoFormCreation(const FormIdent: string): TForm;
  public
    { Public declarations }
    { Table Selection }
    BackupDir,
    WizardAliasName,
    WizardPath:String;
    ForceUpdate:Boolean;
    WizardTableName:String;
    WizardDriveIndex:Integer;
    WizardSource:TSources;
    { General }
    Procedure CreateSeries( AOwner:TComponent;
                            AChart:TCustomChart;
                            ATable:TTable;
                            ShowData:Boolean );
    {$IFDEF C3}
    function CreateHeader(const UnitIdent, FormIdent: string): TMemoryStream;
    function CreateCppSource(const FormIdent: string): TMemoryStream;
    {$ENDIF}
    function CreateSource(const UnitIdent, FormIdent: string): TMemoryStream;
    function CreateForm(const FormIdent: string): TMemoryStream;
    procedure SetChartOptions;
    procedure GallerySelectedChart(Sender: TObject);
    { TableSelection }
    function AliasPath(const AliasName: string): string;
    procedure SetFileEditText(const Value: string);
    procedure SetFilter(const AFilter: string);
    procedure InitFilter;
  end;

procedure TeeChartWizard(ToolServices: TIToolServices);

implementation

uses Proxies, VirtIntf, IStreams, TeeLisB, TeeAbout, Series;

{$R *.DFM}

const
  { page numbers }
  pgStyle       = 0;  { Database vs. Non-Database }
  pgTable       = 1;  { Table name }
  pgFields      = 2;  { Fields }
  pgGallery     = 3;  { Chart Type }
  pgPreviewChart= 4;  { Preview }

  TeeMsg_WizardChart1   = 'Chart1'; {  <-- dont translate }
  TeeMsg_WizardDBChart1 = 'DBChart1'; {  <-- dont translate }
  TeeMsg_WizardTable1   = 'Table1'; {  <-- dont translate }

  SourceBufferSize = 1024;

{ TeeChart Wizard }

Var LocalFilter:String='';
    RemoteFilter:String='';

procedure TTeeDlgWizard.InitFilter;
begin
  SetFilter(LocalFilter);
  with FilterList do
  begin
    SetFileEditText(Mask);
    AliasFileList.Mask := Mask;
  end;
end;

procedure TTeeDlgWizard.FileEditChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := AliasFileList.Items.IndexOf(FileEdit.Text);
  if Index<>LB_ERR then
  begin
    NextButton.Enabled := True;
    AliasFileList.ItemIndex := Index;
    WizardTableName := FileEdit.Text;
  end
  else NextButton.Enabled := False;
end;

procedure TTeeDlgWizard.FileEditKeyPress(Sender: TObject; var Key: Char);
var TempStr : String;
    Alias   : String;
    Dir     : String;
    FileName: String;
    Index   : Integer;
begin
  if Key = Chr(VK_RETURN) then
  begin
    Index := AliasFileList.Items.IndexOf(FileEdit.Text);
    if Index <> LB_ERR then
    begin
      NextButton.Enabled := True;
      AliasFileList.ItemIndex := Index;
    end
    else begin
      TempStr := FileEdit.Text;
      ForceUpdate:= False;
      try
        ProcessAlias(TempStr, Alias, Dir, FileName);
        if AliasDriveList.SetAlias(Alias) then
           AliasPathList.Directory := TempStr
        else
           TempStr := FileName;
        if TempStr <> '' then AliasFileList.ApplyFilePath(TempStr)
        else AliasFileList.Update;
      finally
        ForceUpdate := True;
      end;
      if FileName <> '' then
      begin
        SetFileEditText(FileName);
        NextButton.Enabled := True;
      end else
        SetFileEditText(AliasFileList.Mask);
    end;
    Key := Chr(0);
  end;
end;

procedure TTeeDlgWizard.AliasFileListClick(Sender: TObject);
begin
  with AliasFileList do
    SetFileEditText(Items[ItemIndex]);
end;

procedure TTeeDlgWizard.AliasFileListDblClick(Sender: TObject);
begin
  AliasFileListClick(Sender);
  NextClick(Sender);
end;

procedure TTeeDlgWizard.AliasPathListChange(Sender: TObject);
var ItemStr: TFileName;
begin
  with AliasPathList do
  begin
    if ShowAlias then
    begin
      DirPath.Enabled := False;
      WizardPath := WizardAliasName;
      with AliasFileList do
      begin
        ShowAlias := True;
        AliasName := WizardAliasName;
        System := FilterList.ItemIndex = 1;
        if ForceUpdate then Update;
      end;
      ItemStr := EmptyStr
    end else
    begin
      WizardPath := Directory;
      DirPath.Enabled := True;
{$IFDEF D3}
      ItemStr := AnsiLowerCaseFileName(GetItemPath(ItemIndex));
{$ELSE}
      ItemStr := LowerCase(GetItemPath(ItemIndex));
{$ENDIF}
      AliasFileList.ShowAlias := False;
{$IFDEF D3}
      if AnsiCompareFileName(AliasFileList.Directory,Directory) = 0 then
{$ELSE}
      if UpperCase(AliasFileList.Directory) = UpperCase(Directory) then
{$ENDIF}
        AliasFileList.Update
      else AliasFileList.ApplyFilePath(Directory);
      if Canvas.TextWidth(ItemStr) > PathText.Width then
        ItemStr := MinimizeName(ItemStr, Canvas, PathText.Width);
    end;
    PathText.Caption := ItemStr;
  end;
end;

procedure TTeeDlgWizard.SetFilter(const AFilter: string);
begin
  with FilterList do
  begin
    Filter := AFilter;
    ItemIndex := 2;
  end;
end;

procedure TTeeDlgWizard.SetFileEditText(const Value: string);
begin
  with FileEdit do
  begin
    Text := Value;
    SelectAll;
  end;
end;

procedure TTeeDlgWizard.FilterListChange(Sender: TObject);
begin
  with AliasFileList do
  begin
    Mask := FilterList.Mask;
    if (Items.IndexOf(FileEdit.Text) = LB_ERR) and
       (WizardTableName = '') then
        SetFileEditText(Mask);
    if ShowAlias then
    begin
      AliasName := WizardAliasName;
      System := FilterList.ItemIndex = 1;
      if ForceUpdate then Update;
    end;
  end;
end;

procedure TTeeDlgWizard.AliasDriveListChange(Sender: TObject);
var
  Index: Integer;
  NewPath: string;

  procedure CheckAliasDrive(CurDrive: Char);
  begin
    with AliasPathList do
    begin
      ShowAlias := False;
      if UpCase(Drive) = UpCase(CurDrive) then Update
      else Drive := CurDrive;
      SetFilter(LocalFilter);
    end;
  end;

begin
  with AliasDriveList do
  begin
    Index := ItemIndex;
    WizardDriveIndex := Index;
    if Index > AliasList.Count - 1 then
    begin
      WizardSource := stLocal;
      if AliasPathList.ShowAlias then CheckAliasDrive(Drive)
                                 else AliasPathList.Drive := Drive;
      InitFilter;
    end else
    begin
      with AliasPathList do
      begin
        WizardAliasName := AliasDriveList.Items[Index];
        NewPath := AliasPath(WizardAliasName);
        if NewPath = EmptyStr then
        begin
          WizardSource := stAlias;
          AliasName := AliasDriveList.Items[Index];
          AliasBitmap := TBitmap(AliasDriveList.Items.Objects[Index]);
          ShowAlias := True;
          SetFilter(RemoteFilter);
          Update;
        end else
        begin
          WizardSource := stLocal;
          ShowAlias := False;
          {$IFDEF D3}
          if AnsiCompareText(Directory, NewPath) = 0 then
          {$ELSE}
          if CompareText(Directory, NewPath) = 0 then
          {$ENDIF}
                Update
          else
                Directory := NewPath;
          InitFilter;
        end;
      end;
    end;
  end;
end;

function TTeeDlgWizard.AliasPath(const AliasName: string): string;
var
  Desc: DBDesc;
  SAliasName: array [0..DBIMAXNAMELEN - 1] of char;
  TempDatabase: TDatabase;
begin
  with TTable.Create(nil) do
  try
    TempDatabase := Session.OpenDatabase(AliasName);
    try
      StrPLCopy(SAliasName, AliasName, SizeOf(SAliasName) - 1);
      AnsiToOem(SAliasName, SAliasName);
      Result := '';
      if DbiGetDatabaseDesc(SAliasName, @Desc) = 0 then
        if StrPas(Desc.szDbType) = 'STANDARD' then
        begin
          OemToAnsi(Desc.szPhyName, Desc.szPhyName);
          Result := StrPas(Desc.szPhyName);
        end;
    finally
      Session.CloseDatabase(TempDatabase);
    end;
  finally
    Free;
  end;
end;

{ Paint the sample pane based on the currently selected options }
procedure TTeeDlgWizard.FormCreate(Sender: TObject);
Var AliasList:TStringList;
begin
  tmpGallery:=nil;
  GetDir(0,BackupDir);
  PageControl.ActivePage:=TabStyle;
  {$IFDEF D3}
  LocalFilter := SLocalFilter;
  RemoteFilter := SRemoteFilter;
  LabelURL.Cursor := crHandPoint;
  {$ELSE}
  LocalFilter := LoadStr(SLocalFilter);
  RemoteFilter := LoadStr(SRemoteFilter);
  {$ENDIF}
  WizardTableName:='';
  WizardAliasName:='';
  WizardPath:='';
  ForceUpdate:=True;
  AliasList := TStringList.Create;
  AliasList.Sorted := True;
  Session.GetDatabaseNames(AliasList);
  AliasDriveList.AddAliases(AliasList);
  AliasList.Free;
  AliasPathListChange(Self);
  RefreshButtons;
end;

procedure TTeeDlgWizard.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTeeDlgWizard.PrevClick(Sender: TObject);
begin
  With PageControl do
  case ActivePage.PageIndex of
    pgStyle  : Exit;
    pgTable  : ActivePage := Pages[pgStyle];
    pgFields : ActivePage := Pages[pgTable];
    pgGallery: if rbDatabase.Checked then ActivePage := Pages[pgFields]
                                     else ActivePage := Pages[pgStyle];
    pgPreviewChart: ActivePage := Pages[pgGallery];
  end;
  RefreshButtons;
end;

procedure TTeeDlgWizard.GallerySelectedChart(Sender: TObject);
begin
  PageControl.ActivePage := PageControl.Pages[pgPreviewChart];
  if rbDatabase.Checked then
     CreateSeries(Self,PreviewChart,Self.Table1,True)
  else
     CreateSeries(Self,PreviewChart,nil,True);
  CB3DOption.Checked:=PreviewChart.View3D;
  CBShowLegendOption.Checked:=PreviewChart.Legend.Visible;
  CBShowMarksOption.Visible:=PreviewChart.SeriesCount>0;
  if PreviewChart.SeriesCount>0 then
     CBShowMarksOption.Checked:=PreviewChart[0].Marks.Visible;
  PageControl1.ActivePage:=TabPreviewChart;
  RefreshButtons;
end;

procedure TTeeDlgWizard.NextClick(Sender: TObject);

  Procedure ActiveGallery;
  begin
    PageControl.ActivePage := PageControl.Pages[pgGallery];
    rbChart3D.Checked:=PreviewChart.View3D;
    if not Assigned(tmpGallery) then
    begin
      tmpGallery:=TTeeGallery.Create(Self);
      With tmpGallery do
      begin
        BorderStyle:=bsNone;
        Left:=0;
        Top:=0;
        Align:=alClient;
        Parent:=PanelGallery;
        HideButtons;
        HideFunctions;
        GalleryPanel.OnSelectedChart:=GallerySelectedChart;
        GalleryPanel.SetRowCols(2,4);
        CreateGalleryList([ TLineSeries,
                            TBarSeries,
                            THorizBarSeries,
                            TAreaSeries,
                            TPointSeries,
                            TPieSeries,
                            TFastLineSeries
                         ]);
        Show;
      end;
    end;
  end;

var t               : Integer;
    tmpDatabaseName : String;
begin
  case PageControl.ActivePage.PageIndex of
    pgStyle:  if rbDatabase.Checked then
              begin
                PageControl.ActivePage := PageControl.Pages[pgTable];
                { init table selection }
                InitFilter;
              end
              else ActiveGallery;
    pgTable:  begin
                PageControl.ActivePage := PageControl.Pages[pgFields];
                if WizardAliasName<>'' then tmpDatabaseName:=WizardAliasName
                                       else tmpDatabaseName:=WizardPath;
                if (Table1.DatabaseName<>tmpDatabaseName) or
                   (Table1.TableName<>WizardTableName) then
                begin
                  Table1.Close;
                  Table1.DatabaseName:=tmpDatabaseName;
                  Table1.TableName:=WizardTableName;
                  Table1.FieldDefs.Update;
                  LBAvailFields.Clear;
                  LBSelectedFields.Clear;
                  cbLabelsFields.Clear;
                  for t:=0 to Table1.FieldDefs.Count-1 do
                  begin
                    Case Table1.FieldDefs[t].DataType of
                      ftSmallint,
                      ftInteger,
                      ftWord,
                      ftFloat,
                      ftCurrency,
                      ftBCD,
                      ftDate,
                      ftTime,
                      ftDateTime: begin
                                    LBAvailFields.Items.Add(Table1.FieldDefs[t].Name);
                                    cbLabelsFields.Items.Add(Table1.FieldDefs[t].Name);
                                  end;
                      ftString: cbLabelsFields.Items.Add(Table1.FieldDefs[t].Name);
                    end;
                  end;
                  for t:=0 to PreviewChart.SeriesCount-1 do
                      PreviewChart[t].DataSource:=nil;
                  Table1.Open;
                end;
              end;
    pgFields: if LBSelectedFields.Items.Count > 0 then ActiveGallery;
    pgGallery:  GallerySelectedChart(Self);
    pgPreviewChart:
      begin
        ModalResult := mrOK;
        Exit;
      end;
  end;
  RefreshButtons;
end;

procedure TTeeDlgWizard.RefreshButtons;
begin
  PrevButton.Enabled := PageControl.ActivePage.PageIndex > 0;
  With NextButton do
  begin
    Case PageControl.ActivePage.PageIndex of
      pgTable:  Enabled:=WizardTableName<>'';
      pgFields: Enabled:=LBSelectedFields.Items.Count > 0;
    else
      Enabled:=True;
    end;
    if PageControl.ActivePage.PageIndex=pgPreviewChart then
       Caption := TeeMsg_ExpFinish
    else
       Caption := TeeMsg_ExpNext;
  end;
end;

Procedure TTeeDlgWizard.CreateSeries( AOwner:TComponent;
                                      AChart:TCustomChart;
                                      ATable:TTable;
                                      ShowData:Boolean );
var  t         : Integer;
     tmpSeries : TChartSeries;
     theField  : TField;
     tmpSeriesClass: TChartSeriesClass;
     NumSeries : Longint;
begin
  AChart.FreeAllSeries;
  NumSeries:=LBSelectedFields.Items.Count;
  tmpSeriesClass:= TChartSeriesClass(tmpGallery.GalleryPanel.SelectedChart[0].ClassType);
  if NumSeries>1 then
     if tmpSeriesClass=TPieSeries then NumSeries:=1;

  if ATable=nil then NumSeries:=1;
  for t:=0 to NumSeries-1 do
  begin
    tmpSeries:=tmpSeriesClass.Create(AOwner);
    With tmpSeries do
    begin
      ParentChart:=AChart;
      if ATable<>nil then
      begin
        if AChart is TDBChart then
           TDBChart(AChart).AutoRefresh:=False;
        DataSource:=ATable;
        MandatoryValueList.ValueSource:=LBSelectedFields.Items[t];
        TheField:=ATable.FieldByName(MandatoryValueList.ValueSource);
        if (t=0) and (cbLabelsFields.ItemIndex>=0) then
           XLabelsSource:=cbLabelsFields.Items[cbLabelsFields.ItemIndex];
        case TheField.DataType of
          ftDate,
          ftTime,
          ftDateTime: MandatoryValueList.DateTime:=True;
        else
          MandatoryValueList.DateTime:=False;
        end;
        GetHorizAxis.Title.Caption:='';
        GetVertAxis.Title.Caption:='';
        if YMandatory then
        begin
          XValues.DateTime:=False;
          if NumSeries=1 then  GetVertAxis.Title.Caption:=YValues.ValueSource;
          if XLabelsSource<>'' then GetHorizAxis.Title.Caption:=XLabelsSource;
        end
        else
        begin
          YValues.DateTime:=False;
          if NumSeries=1 then GetHorizAxis.Title.Caption:=XValues.ValueSource;
          if XLabelsSource<>'' then GetVertAxis.Title.Caption:=XLabelsSource;
        end;
        Title:=MandatoryValueList.ValueSource;
      end
      else FillSampleValues(8);
      Name:=TeeMsg_DefaultSeriesName+IntToStr(AChart.SeriesCount);
      ColorEachPoint:=NumSeries=1;
      Marks.Style:=smsValue;
      Marks.Visible:=CBShowMarksOption.Checked;
      if AChart is TDBChart then
        TDBChart(AChart).AutoRefresh:=True;
    end;
  end;
  AChart.View3D:=tmpGallery.GalleryPanel.SelectedChart.View3D;
  if ATable<>nil then
  begin
    AChart.Title.Text.Clear;
    AChart.Title.Text.Add(ATable.TableName);
    if ShowData then
       if AChart is TDBChart then
          TDBChart(AChart).RefreshData;
  end;
end;

{ Create the dialog defined by the user }
function TTeeDlgWizard.DoFormCreation(const FormIdent: string): TForm;
var
  tmpChart : TCustomChart;
  tmpTable : TTable;
begin
  Result := TForm.Create(nil);
  Proxies.CreateSubClass(Result, 'T' + FormIdent, TForm);  { <-- dont translate }
  with Result do
  begin
    Name := FormIdent;
    Caption := FormIdent;
    Width:=470;
    Height:=300;
    with Font do
    begin
      Name := GetDefaultFontName;
      Size := GetDefaultFontSize;
    end;

    if rbDatabase.Checked then
    begin
      tmpTable:=TTable.Create(Result);
      With tmpTable do
      begin
        Left:=12;
        Top:=8;
        Name:=TeeMsg_WizardTable1;
        if WizardAliasName<>'' then DatabaseName:=WizardAliasName
                               else DatabaseName:=WizardPath;
        TableName:=WizardTableName;
        Open; {<---- Crash }
      end;
      tmpChart:=TDBChart.Create(Result);
      With tmpChart do
      begin
        Parent:=Result;
        Name:=TeeMsg_WizardDBChart1;
        Left:=48;
        Top:=8;
      end;
      tmpChart.Assign(PreviewChart as TCustomChart);
      CreateSeries(Result,tmpChart,tmpTable,False);
    end
    else
    begin
      tmpChart:=TChart.Create(Result);
      With tmpChart do
      begin
        Parent:=Result;
        Name:=TeeMsg_WizardChart1;
        Left:=48;
        Top:=8;
      end;
      tmpChart.Assign(PreviewChart as TCustomChart);
      CreateSeries(Result,tmpChart,nil,False);
    end;
  end;
end;

procedure TTeeDlgWizard.FmtWrite(Stream: TStream; Const Fmt: String;
  const Args: array of const);
begin
  if (Stream <> nil) and (SourceBuffer <> nil) then
  begin
    StrLFmt(SourceBuffer, SourceBufferSize, @Fmt[1], Args);
    Stream.Write(SourceBuffer[0], StrLen(SourceBuffer));
  end;
end;

{$IFDEF C3}
const
  CRLF = #13#10;
  DashLine =
  '//----------------------------------------------------------------------------';

function TTeeDlgWizard.CreateHeader(const UnitIdent, FormIdent:                                         string): TMemoryStream;
var t: Integer;
begin
  SourceBuffer := StrAlloc(SourceBufferSize);
  try
    Result := TMemoryStream.Create;
    try
      FmtWrite(Result,
        DashLine + CRLF +
        '#ifndef %0:sH' + CRLF +
        '#define %0:sH' + CRLF +
        DashLine + CRLF +
        '#include <vcl\Classes.hpp>' + CRLF +
        '#include <vcl\Controls.hpp>' + CRLF +
        '#include <vcl\StdCtrls.hpp>' + CRLF +
        '#include <vcl\Forms.hpp>' + CRLF +
        '#include <vcl\TeEngine.hpp>' + CRLF +
        '#include <vcl\TeeProcs.hpp>' + CRLF +
        '#include <vcl\Chart.hpp>' + CRLF, [UnitIdent]);
      if rbDatabase.Checked then
        FmtWrite(Result,
          '#include <vcl\DBChart.hpp>' + CRLF +
          '#include <vcl\DB.hpp>' + CRLF +
          '#include <vcl\DBTables.hpp>' + CRLF, [nil]);

      FmtWrite(Result, DashLine + CRLF, [nil]);

      FmtWrite(Result,
        'class T%s : public TForm' + CRLF +
        '{' + CRLF +
        '__published:' + CRLF, [FormIdent]);

      if rbDatabase.Checked then
      begin
         FmtWrite(Result,'TTable *Table1;' + CRLF, [nil]);
         FmtWrite(Result,'TDBChart *DBChart1;' + CRLF, [nil]);
      end
      else
         FmtWrite(Result,'TChart *Chart1;' + CRLF, [nil]);

      for t:=0 to PreviewChart.SeriesCount-1 do
      begin
        FmtWrite(Result,
           '%s *%s;'+CRLF,
           [PreviewChart[t].ClassName,PreviewChart[t].Name]);
      end;

      FmtWrite(Result,'private:'+CRLF +
               'public:' +CRLF +
               '  __fastcall T%0:s::T%0:s(TComponent* Owner);' + CRLF +
               '};' + CRLF ,[FormIdent]);
      FmtWrite(Result,
        DashLine + CRLF +
        'extern  T%0:s *%0:s;' + CRLF +
        DashLine + CRLF +
        '#endif', [FormIdent]);
      Result.Position := 0;
    except
      Result.Free;
      raise;
    end;
  finally
    StrDispose(SourceBuffer);
  end;
end;

function TTeeDlgWizard.CreateCppSource(const FormIdent: string): TMemoryStream;
begin
  SourceBuffer := StrAlloc(SourceBufferSize);
  try
    Result := TMemoryStream.Create;
    try
      FmtWrite(Result,
        DashLine + CRLF +
        '#include <vcl\vcl.h>' + CRLF +
        '#pragma hdrstop' + CRLF +
        CRLF +
        '#include "%0:s.h"' + CRLF +
        DashLine + CRLF +
        '#pragma resource "*.dfm"' + CRLF +
        'T%1:s *%1:s;' + CRLF +
        DashLine + CRLF +
        '__fastcall T%1:s::T%1:s(TComponent* Owner)' + CRLF +
        '    : TForm(Owner)' + CRLF +
        '{' + CRLF +
        '}' + CRLF +
        DashLine, [FormIdent, FormIdent]);
      Result.Position := 0;
    except
      Result.Free;
      raise;
    end;
  finally
    StrDispose(SourceBuffer);
  end;
end;
{$ENDIF}

function TTeeDlgWizard.CreateSource(const UnitIdent, FormIdent: string): TMemoryStream;
const  CRLF = #13#10;
var  t: Integer;
begin
  SourceBuffer := StrAlloc(SourceBufferSize);
  try
    Result := TMemoryStream.Create;
    try
      { unit header and uses clause }
      FmtWrite(Result,
        'unit %s;' + CRLF + CRLF +
        'interface' + CRLF + CRLF +
        'uses'+CRLF +
        '  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,'+CRLF+
        '  StdCtrls, ExtCtrls, TeEngine, TeeProcs, Chart', [UnitIdent]);

      { additional units that may be needed }
      if rbDatabase.Checked then FmtWrite(Result, ',DBChart, DB, DBTables', [nil]);

      FmtWrite(Result, ';' + CRLF + CRLF, [nil]);

      { begin the class declaration }
      FmtWrite(Result,
        'type'+CRLF +
        '  T%s = class(TForm)'+CRLF, [FormIdent]);

      { add variable declarations }

      if rbDatabase.Checked then
      begin
         FmtWrite(Result,'    Table1 : TTable;' + CRLF, [nil]);
         FmtWrite(Result,'    DBChart1 : TDBChart;' + CRLF, [nil]);
      end
      else
         FmtWrite(Result,'    Chart1 : TChart;' + CRLF, [nil]);

      for t:=0 to PreviewChart.SeriesCount-1 do
      begin
        FmtWrite(Result,
           '    %s: %s;'+CRLF,
           [PreviewChart[t].Name,PreviewChart[t].ClassName]);
      end;

      FmtWrite(Result,
        '  private'+CRLF+
        '    '+TeeMsg_PrivateDeclarations+CRLF+
        '  public'+CRLF+
        '    '+TeeMsg_PublicDeclarations+CRLF+
        '  end;' + CRLF + CRLF +
        'var' + CRLF +
        '  %s: T%s;' + CRLF + CRLF +
        'implementation' + CRLF + CRLF +
        '{$R *.DFM}' + CRLF + CRLF, [FormIdent, FormIdent]);

      FmtWrite(Result, 'end.' + CRLF, [nil]);
      Result.Position := 0;
    except
      Result.Free;
      raise;
    end;
  finally
    StrDispose(SourceBuffer);
  end;
end;

function TTeeDlgWizard.CreateForm(const FormIdent: string): TMemoryStream;
var ChartForm: TForm;
begin
  ChartForm := DoFormCreation(FormIdent);
  try
    Result := TMemoryStream.Create;
    Result.WriteComponentRes(FormIdent, ChartForm);
    Result.Position := 0;
  finally
    ChartForm.Free;
  end;
end;

procedure TTeeDlgWizard.Button5Click(Sender: TObject);
begin
  MoveListAll(LBAvailFields,LBSelectedFields);
  RefreshButtons;
end;

procedure TTeeDlgWizard.Button4Click(Sender: TObject);
begin
  MoveListAll(LBSelectedFields,LBAvailFields);
  RefreshButtons;
end;

procedure TTeeDlgWizard.Button2Click(Sender: TObject);
begin
  MoveList(LBSelectedFields,LBAvailFields);
  RefreshButtons;
end;

procedure TTeeDlgWizard.Button3Click(Sender: TObject);
begin
  MoveList(LBAvailFields,LBSelectedFields);
  RefreshButtons;
end;

procedure TTeeDlgWizard.LBAvailFieldsClick(Sender: TObject);
begin
  Button3.Enabled:=LBAvailFields.SelCount>0;
end;

procedure TTeeDlgWizard.LBSelectedFieldsClick(Sender: TObject);
begin
  Button2.Enabled:=LBSelectedFields.SelCount>0;
end;

procedure TTeeDlgWizard.rbChart3DClick(Sender: TObject);
begin
  tmpGallery.CB3D.Checked:=True;
  rbChart3D.Checked:=True;
  rbChart2D.Checked:=False;
end;

procedure TTeeDlgWizard.rbChart2DClick(Sender: TObject);
begin
  tmpGallery.CB3D.Checked:=False;
  rbChart2D.Checked:=True;
  rbChart3D.Checked:=False;
end;

procedure TTeeDlgWizard.Image1Click(Sender: TObject);
begin
  TeeShowAboutBox({$IFDEF TEETRIAL}True{$ELSE}False{$ENDIF});
end;

procedure TeeChartWizard(ToolServices: TIToolServices);
var D             : TTeeDlgWizard;
    {$IFDEF C3}
    IHeaderStream : IStream;
    ISourceStream : IStream;
    IFormStream   : IStream;
    {$ELSE}
    ISourceStream : TIMemoryStream;
    IFormStream   : TIMemoryStream;
    {$ENDIF}
    UnitIdent     : String;
    FormIdent     : String;
    FileName      : String;
begin
  if ToolServices = nil then Exit;
  if ToolServices.GetNewModuleName(UnitIdent, FileName) then
  begin
    D := TTeeDlgWizard.Create(Application);
    try
      if D.ShowModal = mrOK then
      begin
        UnitIdent := LowerCase(UnitIdent);
        UnitIdent[1] := Upcase(UnitIdent[1]);
        FormIdent := 'Form' + Copy(UnitIdent, 5, 255);

        IFormStream := TIMemoryStream.Create(D.CreateForm(FormIdent));
        {$IFDEF C3}
          IHeaderStream := TIMemoryStream.Create(D.CreateHeader(UnitIdent,FormIdent), soOwned);
          ISourceStream := TIMemoryStream.Create(D.CreateCppSource(UnitIdent,FormIdent), soOwned);
          ToolServices.CreateCppModule(FileName, '', '', '', IHeaderStream,
            ISourceStream, IFormStream, [cmAddToProject, cmShowSource,
            cmShowForm, cmUnNamed, cmMarkModified]);
        {$ELSE}
          try
            {$IFNDEF D5}
            IFormStream.AddRef;
            {$ENDIF}
            ISourceStream := TIMemoryStream.Create(D.CreateSource(UnitIdent,FormIdent));
            try
              {$IFNDEF D5}
              ISourceStream.AddRef;
              {$ENDIF}
              ToolServices.CreateModule(FileName, ISourceStream, IFormStream,
                [ cmAddToProject, cmShowSource, cmShowForm, cmUnNamed,
                  cmMarkModified]);
            finally
              {$IFNDEF D5}
              ISourceStream.OwnStream := True;
              ISourceStream.Free;
              {$ENDIF}
            end;
          finally
            {$IFNDEF D5}
            IFormStream.OwnStream := True;
            IFormStream.Free;
            {$ENDIF}
          end;
        {$ENDIF}
      end;
    finally
      D.Free;
    end;
  end;
end;

procedure TTeeDlgWizard.SetChartOptions;
begin
  With PreviewChart do
  begin
    CB3DOption.Checked:=View3D;
    CBShowLegendOption.Checked:=Legend.Visible;
  end;
end;

procedure TTeeDlgWizard.CB3DOptionClick(Sender: TObject);
begin
  PreviewChart.View3D:=CB3DOption.Checked;
  if CB3DOption.Checked then rbChart3DClick(Self)
                        else rbChart2DClick(Self);
end;

procedure TTeeDlgWizard.CBShowLegendOptionClick(Sender: TObject);
begin
  PreviewChart.Legend.Visible:=CBShowLegendOption.Checked;
end;

procedure TTeeDlgWizard.CBShowMarksOptionClick(Sender: TObject);
var t:Longint;
begin
  for t:=0 to PreviewChart.SeriesCount-1 do
     PreviewChart[t].Marks.Visible:=CBShowMarksOption.Checked;
end;

procedure TTeeDlgWizard.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (PageControl.ActivePage.PageIndex=pgGallery) and
     Assigned(tmpGallery) then tmpGallery.FormKeyDown(Sender,Key,Shift);
end;

procedure TTeeDlgWizard.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=( ModalResult<>mrCancel ) or
            ( MessageDlg( TeeMsg_WizardSureToClose,
                          mtConfirmation,
                          [mbYes,mbNo],
                          0)=mrYes);
end;

procedure TTeeDlgWizard.Button1Click(Sender: TObject);
Const TeeHLPName='TEECHART.HLP';   { <-- don't translate }
begin
{$IFDEF D3}
  HelpFile:=TeeHLPName;
{$ELSE}
  Application.HelpFile:=TeeHLPName;
{$ENDIF}
  Application.HelpJump('TeeChart_Wizard');  { <-- don't translate }
end;

procedure TTeeDlgWizard.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  {$I-}
  ChDir(BackupDir);
  if IOResult<>0 then ;
end;

procedure TTeeDlgWizard.LabelURLClick(Sender: TObject);
begin
{$IFNDEF D1}
  GotoURL(Handle,LabelURL.Caption);
{$ENDIF}
end;

end.


 
