unit TeeAXImp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, TeeX_TLB, StdCtrls, ExtCtrls, TeeProcs,
  TeEngine, Chart, Buttons, Series;

type
  TTeeAxForm = class(TActiveForm, ITeeAxForm)
    Chart1: TChart;
    Panel1: TPanel;
    Panel2: TPanel;
    EditButton: TBitBtn;
    PrintButton: TBitBtn;
    ExportButton: TBitBtn;
    ComboBox1: TComboBox;
    SaveButton: TBitBtn;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    Series1: TPieSeries;
    Timer1: TTimer;
    DownloadButton: TBitBtn;
    BitBtn1: TBitBtn;
    CheckBox2: TCheckBox;
    procedure EditButtonClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Series1Click(Sender: TChartSeries; ValueIndex: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FEvents: IChartXEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
  protected
    { Protected declarations }
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure Initialize; override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: Integer; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_WindowState: TxWindowState; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: Integer); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure Set_WindowState(Value: TxWindowState); safecall;
    function Get_ChartColor: Integer; safecall;
    procedure Set_ChartColor(Value: Integer); safecall;
  public
    { Public declarations }
    Procedure DownloadChart;
  end;

implementation

uses ComServ,URLMon,TeeStore,TeePrevi,Teexport,EditChar,TeeAbout;

{$R *.DFM}

{ TCacaX }

procedure TTeeAxForm.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IChartXEvents;
end;

procedure TTeeAxForm.Initialize;
begin
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TTeeAxForm.Get_Active: WordBool;
begin
  Result := Active;
end;

function TTeeAxForm.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TTeeAxForm.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TTeeAxForm.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TTeeAxForm.Get_Color: Integer;
begin
  Result := Integer(Color);
end;

function TTeeAxForm.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TTeeAxForm.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TTeeAxForm.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TTeeAxForm.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TTeeAxForm.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TTeeAxForm.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TTeeAxForm.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TTeeAxForm.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TTeeAxForm.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TTeeAxForm.Get_WindowState: TxWindowState;
begin
  Result := Ord(WindowState);
end;

procedure TTeeAxForm.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TTeeAxForm.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TTeeAxForm.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TTeeAxForm.Set_Color(Value: Integer);
begin
  Color := TColor(Value);
end;

procedure TTeeAxForm.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TTeeAxForm.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TTeeAxForm.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TTeeAxForm.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TTeeAxForm.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TTeeAxForm.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TTeeAxForm.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TTeeAxForm.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TTeeAxForm.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TTeeAxForm.Set_WindowState(Value: TxWindowState);
begin
  WindowState := TWindowState(Value);
end;

procedure TTeeAxForm.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TTeeAxForm.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TTeeAxForm.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TTeeAxForm.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TTeeAxForm.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TTeeAxForm.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TTeeAxForm.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TTeeAxForm.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

Function URLErrorToString(ErrorCode:HResult):String;
begin
  if ErrorCode=INET_E_INVALID_URL               then  result :='INET_E_INVALID_URL' else
  if ErrorCode=INET_E_NO_SESSION                then  Result :='INET_E_INVALID_URL' else
  if ErrorCode=INET_E_CANNOT_CONNECT            then  Result :='INET_E_CANNOT_CONNECT'  else
  if ErrorCode=INET_E_RESOURCE_NOT_FOUND        then  Result :='INET_E_RESOURCE_NOT_FOUND'  else
  if ErrorCode=INET_E_OBJECT_NOT_FOUND          then  Result :='INET_E_OBJECT_NOT_FOUND'  else
  if ErrorCode=INET_E_DATA_NOT_AVAILABLE        then  Result :='INET_E_DATA_NOT_AVAILABLE'  else
  if ErrorCode=INET_E_DOWNLOAD_FAILURE          then  Result :='INET_E_DOWNLOAD_FAILURE'  else
  if ErrorCode=INET_E_AUTHENTICATION_REQUIRED   then  Result :='INET_E_AUTHENTICATION_REQUIRED'   else
  if ErrorCode=INET_E_NO_VALID_MEDIA            then  Result :='INET_E_NO_VALID_MEDIA'  else
  if ErrorCode=INET_E_CONNECTION_TIMEOUT        then  Result :='INET_E_CONNECTION_TIMEOUT'  else
  if ErrorCode=INET_E_INVALID_REQUEST           then  Result :='INET_E_INVALID_REQUEST'  else
  if ErrorCode=INET_E_UNKNOWN_PROTOCOL          then  Result :='INET_E_UNKNOWN_PROTOCOL'  else
  if ErrorCode=INET_E_SECURITY_PROBLEM          then  Result :='INET_E_SECURITY_PROBLEM'  else
  if ErrorCode=INET_E_CANNOT_LOAD_DATA          then  Result :='INET_E_CANNOT_LOAD_DATA' else
  if ErrorCode=INET_E_CANNOT_INSTANTIATE_OBJECT then  Result :='INET_E_CANNOT_INSTANTIATE_OBJECT' else
  if ErrorCode=INET_E_ERROR_FIRST               then  Result :='INET_E_ERROR_FIRST'  else
  if ErrorCode=INET_E_ERROR_LAST                then  Result :='INET_E_ERROR_LAST' ;
end;

Procedure TTeeAxForm.DownloadChart;
var tmp:HResult;
    tmpChart:TCustomChart;
    LocalFile,St:Array[0..255] of Char;
begin
  Timer1.Enabled:=False;
  CheckBox2.Enabled:=False;
  if GetTempPath(SizeOf(St),St)=0 then
     raise Exception.Create('Cannot get the Windows TEMP path');
  if GetTempFileName(St,'TMPTEE',0,LocalFile)=0 then
     raise Exception.Create('Cannot get a TEMP file name');
  tmp:=URLDownloadToFileA(ComObject,PChar(ComboBox1.Text),LocalFile,0,nil);
  if tmp=0 then
  begin
    Chart1.Free;
    tmpChart:=TChart.Create(Self);
    LoadChartFromFile(tmpChart,LocalFile);
    Chart1:=tmpChart as TChart;
    Chart1.Align:=alClient;
    Chart1.Parent:=Self;
    DownloadButton.Enabled:=False;
  end
  else
  begin
    Screen.Cursor:=crDefault;
    ShowMessage('Error code: '+URLErrorToString(tmp)+' downloading: '+ComboBox1.Text);
  end;
end;

procedure TTeeAxForm.EditButtonClick(Sender: TObject);
begin
  EditChart(nil,Chart1);
end;

procedure TTeeAxForm.PrintButtonClick(Sender: TObject);
begin
  ChartPreview(nil,Chart1);
end;

procedure TTeeAxForm.ExportButtonClick(Sender: TObject);
begin
  ChartExport(nil,Chart1);
end;

procedure TTeeAxForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then  SaveChartToFile(Chart1,SaveDialog1.FileName);
end;

procedure TTeeAxForm.CheckBox1Click(Sender: TObject);
begin
  Chart1.View3D:=CheckBox1.Checked;
end;

procedure TTeeAxForm.ComboBox1Change(Sender: TObject);
begin
  DownloadButton.Enabled:=True;
end;

function TTeeAxForm.Get_ChartColor: Integer;
begin
  result:=Chart1.Color;
end;

procedure TTeeAxForm.Set_ChartColor(Value: Integer);
begin
  Chart1.Color:=Value;
end;

procedure TTeeAxForm.ComboBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then DownloadButtonClick(Self);
end;

procedure TTeeAxForm.Timer1Timer(Sender: TObject);
begin
  Series1.Rotate(1);
end;

procedure TTeeAxForm.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(8);
end;

procedure TTeeAxForm.CheckBox2Click(Sender: TObject);
begin
  Timer1.Enabled:=CheckBox2.Checked;
end;

procedure TTeeAxForm.DownloadButtonClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    DownloadChart;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TTeeAxForm.BitBtn1Click(Sender: TObject);
begin
  With TTeeAboutForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TTeeAxForm.Series1Click(Sender: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowMessage('Visit www.'+Sender.XLabel[ValueIndex]+'.com !');
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TTeeAxForm,
    Class_ChartX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL);
end.
