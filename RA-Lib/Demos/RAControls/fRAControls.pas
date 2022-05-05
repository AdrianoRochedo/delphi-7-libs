unit fRAControls;

interface

{$INCLUDE RA.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, RAComponentPanel, StdCtrls, ComCtrls,
  RARegAuto, RAButtons, RATreeView, Buttons, RAScrollMax, Mask, DBCtrls,
  RAScrollBar, RAImage, RACt, RACombo {$IFDEF RA_D4H}, ImgList{$ENDIF RA_D4H}, RADlg,
  RAScrollText;

type
  TMainForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Item1: TMenuItem;
    Item2: TMenuItem;
    TabControl1: TTabControl;
    RAComponentPanel1: TRAComponentPanel;
    Memo1: TMemo;
    RegAuto2: TRegAuto;
    RACaptionButton1: TRACaptionButton;
    RACaptionButton2: TRACaptionButton;
    RACaptionButton3: TRACaptionButton;
    ImageList1: TImageList;
    RegAuto1: TRegAuto;
    RAStatusBar1: TRAStatusBar;
    ProgressBar1: TProgressBar;
    Notebook1: TNotebook;
    Panel2: TPanel;
    RATreeComboBox1: TRAComboBox4Tree;
    RATreeView1: TRATreeView;
    RAImage1: TRAImage;
    Label20: TLabel;
    RAScrollMax1: TRAScrollMax;
    RAScrollMaxBand1: TRAScrollMaxBand;
    Label11: TLabel;
    Label12: TLabel;
    Label16: TLabel;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit14: TEdit;
    RAScrollMaxBand2: TRAScrollMaxBand;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    RAScrollMaxBand3: TRAScrollMaxBand;
    Label17: TLabel;
    Label18: TLabel;
    Edit15: TEdit;
    Edit16: TEdit;
    RACheckBox1: TRACheckBox;
    Edit1: TEdit;
    RAColorButton1: TRAColorButton;
    RANoFrameButton1: TRANoFrameButton;
    RANoFrameButton2: TRANoFrameButton;
    RACombo1: TRACombo;
    RAScrollBar1: TRAScrollBar;
    Label1: TLabel;
    RAScrollBar951: TRAScrollBar95;
    RAScrollBar952: TRAScrollBar95;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RACombo2: TRACombo;
    Label6: TLabel;
    RANoFrameButton3: TRANoFrameButton;
    RANoFrameButton4: TRANoFrameButton;
    RANoFrameButton5: TRANoFrameButton;
    RAScrollMax2: TRAScrollMax;
    RAScrollMaxBand4: TRAScrollMaxBand;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    RAScrollMaxBand5: TRAScrollMaxBand;
    Label10: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    RAScrollMaxBand6: TRAScrollMaxBand;
    Label23: TLabel;
    Label24: TLabel;
    Edit8: TEdit;
    Edit17: TEdit;
    CheckBox1: TCheckBox;
    Label19: TLabel;
    Label25: TLabel;
    ListBox1: TListBox;
    RAhtListBox1: TRAhtListBox;
    Memo2: TMemo;
    Button1: TButton;
    CheckBox2: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    RAProgressForm1: TRAProgressForm;
    Label26: TLabel;
    RAhtComboBox1: TRAhtComboBox;
    RAScrollText1: TRAScrollText;
    Label27: TLabel;
    RAhtLabel1: TRAhtLabel;
    RAhtButton1: TRAhtButton;
    RAhtLabel2: TRAhtLabel;
    procedure RAComponentPanel1Click(Sender: TObject; Button: Integer);
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RACaptionButton1Click(Sender: TObject);
    procedure RANoFrameButton1Click(Sender: TObject);
    procedure RAColorButton1Click(Sender: TObject);
    procedure RAImage1KeyPress(Sender: TObject; var Key: Char);
    procedure RAScrollBar1Scroll(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RAProgressForm1Show(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses RAUtils;

{$R *.DFM}

{$R ractl.dcr}
{$R radb.dcr}
{$R res.res}

procedure TMainForm.TabControl1Change(Sender: TObject);
const
  sRAControls = 'TRegAuto,TRAProgressForm,TRAEditor,'+
                'TRAHint,'+
                'TRACombo,TRAComponentPanel,'+
                'TRACaptionButton,'+
                'TRAScrollMax,TRAScrollText,'+
                'TRAhtListBox,TRAhtComboBox,TRAhtButton,TRAhtLabel,'+
                'TRADoubleCombo';
  sRAAdditional = 'TRegAutoGrid,'+
                'TRATreeView,TRAComboBox4Tree,'+
                'TRAScrollBar,TRAScrollBar95,'+
                'TRANoFrameButton,TRAColorButton,TRAImage,'+
                'TRACheckBox,TRAWaitPanel';
  sRADB       = 'TRASQLScript,TRADBTreeView,TRADBLookupTreeView,'+
                'TRADBLookupTreeViewCombo,TRADBRadioGroupS,'+
                'TRADBTextS,TRADBMove';
var
  Comps : string;
  S : string;
  i : integer;
begin
  RAScrollText1.Stop;
  case TabControl1.TabIndex of
    0 :
      begin
        Comps := sRAControls;
        RAComponentPanel1.ButtonCount := 14;
      end;
    1 :
      begin
        Comps := sRAAdditional;
        RAComponentPanel1.ButtonCount := 10;
      end;
    2 :
      begin
        Comps := sRADB;
        RAComponentPanel1.ButtonCount := 7;
      end;
    else
      begin
        Comps := '';
      end;
  end;
 // RAComponentPanel1.RecreateButtons;
  RAComponentPanel1.FirstVisible := 0;
  i := 0;
  S := RAUtils.SubStr(Comps, i, ',');
  while S <> '' do
  begin
    RAComponentPanel1.Buttons[i].Hint := S;
    try
      RAComponentPanel1.Buttons[i].Glyph.LoadFromResourceName(hInstance, S);
    except
    end;
    inc(i);
    S := RAUtils.SubStr(Comps, i, ',');
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  hRgn  : Windows.HRGN;
begin
  Application.Title := Caption;
  if FileExists(ExePath + '..\..\!README.TXT') then
  begin
    RAScrollText1.Lines.LoadFromFile(ExePath + '..\..\!README.TXT');
    RAScrollText1.Lines.Insert(0, '$Font:Times New Roman;12;1');
    RAScrollText1.Lines.Insert(3, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(4, '');
    RAScrollText1.Lines.Insert(12, '$Pause 3000');
    RAScrollText1.Lines.Insert(13, '$Font:Times New Roman;9;0');
    RAScrollText1.Lines.Add('$Pause 10000');
  end
  else
  begin
    RAScrollText1.Lines.Clear;
    RAScrollText1.Lines.Add('          File "' + ExpandFileName(ExePath + '..\..\!README.TXT') + '" not found !');
  end;
  RAScrollMax1.MouseClasses([TLabel]);
  TabControl1Change(nil);
  RAComponentPanel1Click(nil, 0);
  RAhtComboBox1.ItemIndex := 0;
  with RAScrollText1 do
  begin
    hRgn := CreateRoundRectRgn(0, 0, Width, Height, Width div 6, Width div 6);
    if (hRgn = 0) or not BOOL(SetWindowRgn(Handle, hRgn, true)) then {fail};
  end;    { with }
  RAhtLabel1.Caption :=
    '<b><c:Red>TRAhtLabel</b><c:WindowText> can displays caption with few font styles.'#13+
    'But only <u>one</u> font size allowed.'#13+
    '<u>WordWrap</u> not supported, but multiline text can'#13+
    'be showed, if caption contains <b>#13</b> characters.';
 {$IFDEF RA_D3H}
  RACombo1.Flat := True;
  RACombo2.Flat := False;
 {$ENDIF RA_D3H}
end;

procedure TMainForm.RAComponentPanel1Click(Sender: TObject; Button: Integer);
begin
  Memo1.Lines.Clear;
  if Button < 0 then exit;
  RegAuto2.ReadSection(RAComponentPanel1.Buttons[Button].Hint+'\Descript', Memo1.Lines);
  RAScrollText1.Stop;
  NoteBook1.ActivePage := Trim(RegAuto2.ReadString(RAComponentPanel1.Buttons[Button].Hint, 'Page', ''));
  Memo1.Visible := RegAuto2.ReadBool(RAComponentPanel1.Buttons[Button].Hint, 'Memo', True);
  if NoteBook1.ActivePage = 'RAScrollText' then
    RAScrollText1.Scroll;
end;

procedure TMainForm.RACaptionButton1Click(Sender: TObject);
begin
  ShowMessage('RACaptionButton displays glyph');
end;

procedure TMainForm.RANoFrameButton1Click(Sender: TObject);
begin
  ShowMessage('RANoFrameButton has not border');
end;

procedure TMainForm.RAColorButton1Click(Sender: TObject);
begin
  ShowMessage('RAColorButton has color');
end;

procedure TMainForm.RAImage1KeyPress(Sender: TObject; var Key: Char);
begin
  ShowMessage('Key "'+Key+'" pressed !');
end;

procedure TMainForm.RAScrollBar1Scroll(Sender: TObject);
begin
  Label4.Caption := 'Position: ' + IntToStr(RAScrollBar1.Pos);
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  RAScrollMax1.OneExpanded := CheckBox1.Checked;
  RAScrollMax1.AutoHeight := CheckBox1.Checked;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ListBox1.Items := Memo2.Lines;
  RAhtListBox1.Items := Memo2.Lines;
  RAhtComboBox1.Items := Memo2.Lines;
end;

procedure TMainForm.CheckBox2Click(Sender: TObject);
begin
  RAhtListBox1.HideSel := CheckBox2.Checked;
  RAhtComboBox1.HideSel := CheckBox2.Checked;       
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ShowMessage(RAhtListBox1.PlainItems[0]);
end;

procedure TMainForm.RAProgressForm1Show(Sender: TObject);
begin
  RAProgressForm1.ProgressPosition := RAProgressForm1.ProgressMin; 
  while RAProgressForm1.ProgressPosition < RAProgressForm1.ProgressMax do
  begin
    Delay(50);
    RAProgressForm1.InfoLabel := 'Long operation in progress: item'+ IntToStr(Random(100));
    RAProgressForm1.ProgressPosition := RAProgressForm1.ProgressPosition + 1;
    Application.ProcessMessages;
    if RAProgressForm1.Cancel then Exit;
  end;    { while }
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  RAProgressForm1.Execute;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  RAScrollText1.Stop;
end;

end.
