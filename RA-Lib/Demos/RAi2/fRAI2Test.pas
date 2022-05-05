unit fRAI2Test;

interface

{$INCLUDE RA.INC}

uses
{$IFDEF RA_D3H} 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RARegAuto, ComCtrls,
  RAI2, RAI2Fm, RAEditor, IParser, RAHLEditor,
  Db, DBTables, Grids, DBGrids, Quickrpt, QRCtrls
 {$IFDEF RA_D6H}, Variants {$ENDIF};
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RARegAuto, ComCtrls,
  RAI2, RAI2Fm, RAEditor, IParser, RAHLEditor,
  Db, DBTables, Grids, DBGrids, Quickrep;
{$ENDIF}

type
  TTest = class(TForm)
    RegAuto1: TRegAuto;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    Table1: TTable;
    DataSource1: TDataSource;
    RAI2Program1: TRAI2Fm;
    Memo1: TRAHLEditor;
    Panel2: TPanel;
    Notebook1: TNotebook;
    bRunReport: TButton;
    bRunForm: TButton;
    Button4: TButton;
    Label1: TLabel;
    Button1: TButton;
    Button5: TButton;
    Memo2: TMemo;
    pnlTime: TPanel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    RegAuto2: TRegAuto;
    Panel3: TPanel;
    pnlResult: TPanel;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure bRunFormClick(Sender: TObject);
    procedure bRunReportClick(Sender: TObject);
    procedure RAI2Program1GetValue(Sender: TObject; Identifer: string;
      var Value: Variant; Args: TArgs; var Done: boolean);
    procedure RAI2Program1GetUnitSource(UnitName: string; var Source: string;
      var Done: Boolean);
    procedure ComboBox1Change(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure RAI2Program1Statement(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure RAI2Program1GetDfmFileName(Sender: TObject; UnitName: String;
      var FileName: String; var Done: Boolean);
  private
    { Private declarations }
    Parser : TIParser;
    InternalExamplesCount: Integer;
    CurFileName: TFileName;
  public
    { Public declarations }
    V: Variant;
  end;

var
  Test: TTest;

implementation

uses RAUtils, RACnst
  , RAI2_all
  //, RAI2_QRExpr
  ;

{$R *.DFM}

{$IFNDEF RA_D3H}
type
  TQuickRep = TQuickReport;
{$ENDIF}


{ constructor Create(Msg: string) }
procedure EZeroDivide_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(EZeroDivide.Create(Args.Values[0]));
end;

procedure TTest.FormCreate(Sender: TObject);
var
  SS: TStringList;
  i: Integer;
begin
  try
    RegAuto2.IniStrings.LoadFromFile(ExePath + 'RAI2Test.ini');
  except
    MessageBox('Can''t load file "RAI2Test.ini".'#13+
      'Please put it in same folder as RAI2Test.exe.',
      'Error loading examples', MB_ICONERROR);               
  end;
  RegAuto2.ReadSection('Demos', ComboBox1.Items);
  InternalExamplesCount := ComboBox1.Items.Count;
  SS := TStringList.Create;
  try
    ReadFolder(ExePath + 'samples', 'sample - *.pas', SS);
    if SS.Count > 0 then
      ComboBox1.Items.Add('------ custom files (samples folder) ------');
    SS.Sort;  
    for i := 0 to SS.Count - 1 do
      ComboBox1.Items.Add(SS[i]);
  finally
    SS.Free;
  end;
  RAI2Program1.Adapter.AddGet(EZeroDivide, 'Create', EZeroDivide_Create, 1, [varEmpty], varEmpty);
  DecimalSeparator := '.';
  Parser := TIParser.Create;
end;                            

procedure TTest.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TTest.Button1Click(Sender: TObject);
const
  Bool : array [boolean] of string = ('False', 'True');
var
  T1: longword;
begin
  RegAuto1AfterSave(nil);
  if (Sender = Button1) or (Sender = Button2) or (Sender = Button5) then
  begin
    RAI2Program1.Source := Memo1.Lines.Text;
    CurFileName := '';
  end
  else if Sender = Button3 then
  begin
    if not OpenDialog1.Execute then Exit;
    CurFileName := OpenDialog1.FileName;
    Memo1.Lines.Text := LoadTextFile(CurFileName);
    RAI2Program1.Source := LoadTextFile(CurFileName);
  end;                            

  pnlResult.Caption := 'Working';
  pnlResult.Color := clRed;
  pnlResult.Update;
  T1 := GetTickCount;

  try try
    if (Sender = Button1) or (Sender = Button2) or (Sender = Button3) then
      RAI2Program1.Run
    else if Sender = Button5 then
      RAI2Program1.Compile;

  pnlTime.Caption := 'ms: ' + IntToStr(GetTickCount - T1);

	case VarType(RAI2Program1.VResult) of
		varBoolean:
			pnlResult.Caption := Bool[boolean(RAI2Program1.VResult)];
		varString, varInteger, varDouble :
			pnlResult.Caption := RAI2Program1.VResult;
		varEmpty:
			pnlResult.Caption := 'Empty';
		varNull:
			pnlResult.Caption := 'Null';
    varObject:
      if V2O(RAI2Program1.VResult) = nil then
  			pnlResult.Caption := 'Object: nil'
      else
  			pnlResult.Caption := 'Object: ' + V2O(RAI2Program1.VResult).ClassName;
		varSet:
			pnlResult.Caption := 'Set: ' + IntToStr(V2S(RAI2Program1.VResult));
		else
			pnlResult.Caption := '!Unknown!';
	end;
  except
    on E : ERAI2Error do
    begin
      pnlResult.Caption := IntToStr(E.ErrCode) + ': ' + ReplaceString(E.Message, #10, ' ');
      if E.ErrPos > -1 then
      begin
        Memo1.SelStart := E.ErrPos;
        Memo1.SelLength := 0;
      end;
      Memo1.SetFocus;
    end;
    on E : Exception do
    begin
      pnlResult.Caption := IntToStr(RAI2Program1.LastError.ErrCode) + ': ' +
        ReplaceString(RAI2Program1.LastError.Message, #10, ' ');
      if RAI2Program1.LastError.ErrPos > -1 then
      begin
        Memo1.SelStart := RAI2Program1.LastError.ErrPos;
        Memo1.SelLength := 0;
      end;
      Memo1.SetFocus;
      raise;
    end
    else
    begin
      pnlResult.Caption := 'error';
      raise;
    end;
  end;
  finally
    pnlResult.Color := clBtnFace;
  end;
end;
              
procedure TTest.RegAuto1AfterSave(Sender: TObject);
begin
  RegAuto1.WriteInteger(Name, 'PrId', ComboBox1.ItemIndex);
  if ComboBox1.ItemIndex >= InternalExamplesCount then
    Memo1.Lines.SaveToFile(ExePath + 'samples\' + ComboBox1.Text);
end;

procedure TTest.RegAuto1AfterLoad(Sender: TObject);
begin
  ComboBox1.ItemIndex := RegAuto1.ReadInteger(Name, 'PrId', 0);
  ComboBox1Change(nil);
end;


procedure TTest.RAI2Program1GetValue(Sender: TObject; Identifer: string;
  var Value: Variant; Args: TArgs; var Done: boolean);
begin
  if Cmp(Identifer, 'Test') then
  begin
    Done := True;
    Value := O2V(Self);
  end
  else
  if Cmp(Identifer, 'ShowMessage') and (Args.Obj = Self) then
  begin
    Done := True;
    ShowMessage(Args.Values[0]);
    Value := Null;
  end
  else
  if Cmp(Identifer, 'MyFunction') then
  begin
    Done := True;
    Value := Args.Values[0] + 1;
  end
end;

procedure TTest.RAI2Program1GetUnitSource(UnitName: string; var Source: string;
  var Done: Boolean);
var
  FN: TFileName;
begin
  FN := FindInPath(UnitName + '.pas', ConcatSep(ExtractFilePath(CurFileName),
    ExePath + ';' + ExePath + 'samples', ';'));
  if FileExists(FN) then
  begin
    Source := LoadTextFile(FN);
    Done := True;
  end;
end;

procedure TTest.RAI2Program1GetDfmFileName(Sender: TObject; UnitName: String;
  var FileName: String; var Done: Boolean);
begin
  FileName := FindInPath(UnitName + '.dfm', ConcatSep(ExtractFilePath(CurFileName),
      ExePath + ';' + ExePath + 'samples', ';'));
  Done := FileExists(FileName);
end;

procedure TTest.bRunFormClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    RAI2Program1.RunFormModal(OpenDialog1.FileName);
end;

procedure TTest.bRunReportClick(Sender: TObject);
var
  QuickRep1: TQuickRep;
begin
  if OpenDialog1.Execute then
  begin
    with RAI2Program1.MakeForm(OpenDialog1.FileName) do
      try
       {$IFDEF RA_D3H}
        QuickRep1 := (FindComponent('QuickRep1') as TQuickRep);
       {$ELSE}
        QuickRep1 := (FindComponent('QuickReport1') as TQuickRep);
       {$ENDIF RA_D3H}
        if QuickRep1 = nil then raise Exception.Create('QuickRep1 not found on this form');
        QuickRep1.Preview;
      finally { wrap up }
        Free;
      end;    { try/finally } 
  end;
end;

procedure TTest.ComboBox1Change(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  if ComboBox1.ItemIndex < InternalExamplesCount then
  begin
    RegAuto2.ReadWholeSection(ComboBox1.Text + '\Source', Memo1.Lines);
    RegAuto2.ReadWholeSection(ComboBox1.Text + '\Description', Memo2.Lines);
    Notebook1.ActivePage := RegAuto2.ReadString(ComboBox1.Text, 'Page', 'Default');
  end else
  begin
    Memo1.Lines.LoadFromFile(ExePath + 'samples\' + ComboBox1.Text);
    Notebook1.ActivePage := 'Default';
  end;
//  Memo1.Refresh;
end;

procedure TTest.Panel1Resize(Sender: TObject);
begin
  ComboBox1.Width := Panel1.Width - ComboBox1.Left - Pixels(Self, 8);
end;

procedure TTest.RAI2Program1Statement(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TTest.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = ord('S')) and ([ssCtrl] = Shift) then
    RegAuto1AfterSave(nil);
end;

procedure TTest.ComboBox1DropDown(Sender: TObject);
begin
  RegAuto1AfterSave(nil);
end;
                                                    
initialization
  //RAI2_QRExpr.RegisterRAI2Adapter(GlobalRAI2Adapter);
end.
