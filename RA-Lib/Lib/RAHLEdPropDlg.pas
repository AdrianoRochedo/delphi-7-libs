{***********************************************************
                R&A Library
       Copyright (C) 2000 R&A

       component   : TRAHLEdPropDlg
       description : Properties dialog for TRAHLEditor component

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAHLEdPropDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, RARegAuto, StdCtrls, RAEditor, RAHLEditor, ExtCtrls;

type
  TRAHLEdPropDlg = class;

  TRAHLEditorParamsForm = class(TForm)
    Pages: TPageControl;
    bCancel: TButton;
    bOK: TButton;
    tsEditor: TTabSheet;
    lblEditorSpeedSettings: TLabel;
    cbKeyboardLayot: TComboBox;
    gbEditor: TGroupBox;
    cbUndoAfterSave: TCheckBox;
    cbDoubleClickLine: TCheckBox;
    cbKeepTrailingBlanks: TCheckBox;
    cbSytaxHighlighting: TCheckBox;
    cbAutoIndent: TCheckBox;
    cbSmartTab: TCheckBox;
    cbBackspaceUnindents: TCheckBox;
    cbGroupUndo: TCheckBox;
    cbCursorBeyondEOF: TCheckBox;
    eTabStops: TEdit;
    lblTabStops: TLabel;
    tsColors: TTabSheet;
    lblColorSpeedSettingsFor: TLabel;
    cbColorSettings: TComboBox;
    lblElement: TLabel;
    lbElements: TListBox;
    lblColor: TLabel;
    gbTextAttributes: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    gbUseDefaultsFor: TGroupBox;
    cbDefForeground: TCheckBox;
    cbDefBackground: TCheckBox;
    raColorSamples: TRegAuto;
    Label6: TLabel;
    Panel1: TPanel;
    Cell0: TPanel;
    Cell4: TPanel;
    Cell8: TPanel;
    Cell12: TPanel;
    Cell1: TPanel;
    Cell5: TPanel;
    Cell9: TPanel;
    Cell13: TPanel;
    Cell2: TPanel;
    Cell6: TPanel;
    Cell10: TPanel;
    Cell14: TPanel;
    Cell3: TPanel;
    Cell7: TPanel;
    Cell11: TPanel;
    Cell15: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure NotImplemented(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbElementsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorChanged(Sender: TObject);
    procedure cbColorSettingsChange(Sender: TObject);
    procedure DefClick(Sender: TObject);
    procedure CellMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    RAHLEditor1: TRAHLEditor;
    FHighlighter: THighlighter;
    SC: TSymbolColor;
    InChanging: boolean;
    Params: TRAHLEdPropDlg;
    procedure LoadLocale;
    function ColorToIndex(const AColor: TColor): integer;
    function GetColorIndex(const ColorName: string): Integer;
    function GetForegroundIndex: Integer;
    function GetBackgroundIndex: Integer;
    procedure SetColorIndex(const Index: Integer;
      const ColorName, OtherColorName: string);
    procedure SetForegroundIndex(const Index: Integer);
    procedure SetBackgroundIndex(const Index: Integer);
    function GetColorColor(const ColorName: string): TColor;
    function GetForegroundColor: TColor;
    function GetBackgroundColor: TColor;
    function GetCell(const Index: Integer): TPanel;
  public
    procedure ParamsToControls;
    procedure ControlsToParams;
  end;

  TRAHLEdActivePage = 0..1;
  TRAHLEdReadFrom = (rfRegAuto, rfHLEditor);
  TRAHLEdPages = set of (epEditor, epColors);
  TOnDialogPopup = procedure(Sender: TObject; Form: TForm) of object;
  TOnDialogClosed = procedure(Sender: TObject; Form: TForm; Apply: boolean) of object;

  TRAHLEdPropDlg = class(TComponent)
  private
    FRAHLEditor: TRAHLEditor;
    FRegAuto: TRegAuto;
    FColorSamples: TStrings;
    FHighlighterCombo: Boolean;
    FActivePage: TRAHLEdActivePage;
    FReadFrom: TRAHLEdReadFrom;
    FPages: TRAHLEdPages;
    FRegAutoSection: string;                 { ini section for regauto }
    FOnDialogPopup: TOnDialogPopup;
    FOnDialogClosed: TOnDialogClosed;
    procedure SetColorSamples(const Value: TStrings);
    function IsPagesStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Restore;
    procedure LoadHighlighterColors(ARAHLEditor: TRAHLEditor; AHighlighter: THighlighter);
    procedure SaveHighlighterColors(ARAHLEditor: TRAHLEditor; AHighlighter: THighlighter);
    function Execute: boolean;
    procedure LoadCurrentHighlighterColors;
    procedure SaveCurrentHighlighterColors;
  published
    property RAHLEditor: TRAHLEditor read FRAHLEditor write FRAHLEditor;
    property RegAuto: TRegAuto read FRegAuto write FRegAuto;
    property ColorSamples: TStrings read FColorSamples write SetColorSamples;
    property HighlighterCombo: Boolean read FHighlighterCombo write FHighlighterCombo default True;
    property ActivePage: TRAHLEdActivePage read FActivePage write FActivePage default 0;
    property ReadFrom: TRAHLEdReadFrom read FReadFrom write FReadFrom default rfRegAuto;
    property Pages: TRAHLEdPages read FPages write FPages stored IsPagesStored;
    property RegAutoSection: string read FRegAutoSection write FRegAutoSection;
    property OnDialogPopup: TOnDialogPopup read FOnDialogPopup write FOnDialogPopup;
    property OnDialogClosed: TOnDialogClosed read FOnDialogClosed write FOnDialogClosed;
  end;

const
  HighLighters: array[THighLighter] of string = (
    'None', 'Pascal', 'CBuilder', 'Sql', 'Python', 'Java', 'VB', 'Html',
    'Perl', 'Ini');

implementation

uses RAUtils, Consts, RACnst;

{$R *.DFM}

type
  TSampleViewer = class(TRAHLEditor)
  private
    TmpEd: TRAHLEditor;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  
{ TRAHLEdPropDlg }

constructor TRAHLEdPropDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHighlighterCombo := True;
  FColorSamples := TStringList.Create;
  with TRAHLEditorParamsForm.Create(Application) do
    try
      FColorSamples.Assign(raColorSamples.IniStrings);
    finally
      Free;
    end;
  FPages := [epEditor, epColors];
end;

destructor TRAHLEdPropDlg.Destroy;
begin
  FColorSamples.Free;
  inherited Destroy;
end;

procedure TRAHLEdPropDlg.Save;
var
  S: string;
begin
  if FRegAuto = nil then
    Error(SHLEdPropDlg_RegAutoNotAssigned);
  S := AddSlash2(FRegAutoSection);
  with FRegAuto do
  begin
    WriteBool(S + 'Params', 'DoubleClickLine'   , FRAHLEditor.DoubleClickLine   );
    WriteBool(S + 'Params', 'UndoAfterSave'     , FRAHLEditor.UndoAfterSave     );
    WriteBool(S + 'Params', 'KeepTrailingBlanks', FRAHLEditor.KeepTrailingBlanks);
    WriteBool(S + 'Params', 'AutoIndent', FRAHLEditor.AutoIndent        );
    WriteBool(S + 'Params', 'SmartTab', FRAHLEditor.SmartTab          );
    WriteBool(S + 'Params', 'BackspaceUnindents', FRAHLEditor.BackspaceUnindents);
    WriteBool(S + 'Params', 'GroupUndo', FRAHLEditor.GroupUndo         );
    WriteBool(S + 'Params', 'CursorBeyondEOF', FRAHLEditor.CursorBeyondEOF   );
    WriteBool(S + 'Params', 'SyntaxHighlighting', FRAHLEditor.SyntaxHighlighting );
    WriteString(S + 'Params', 'TabStops'        , FRAHLEditor.TabStops          );
    WriteInteger(S + 'Params', 'RightMargin', FRAHLEditor.RightMargin);
  end
end;

procedure TRAHLEdPropDlg.Restore;
var
  S: string;
begin
  if FRegAuto = nil then
    Error(SHLEdPropDlg_RegAutoNotAssigned);
  S := AddSlash2(FRegAutoSection);
  with FRegAuto do
  begin
    FRAHLEditor.DoubleClickLine    := ReadBool(S + 'Params', 'DoubleClickLine'   , FRAHLEditor.DoubleClickLine   );
    FRAHLEditor.UndoAfterSave      := ReadBool(S + 'Params', 'UndoAfterSave'     , FRAHLEditor.UndoAfterSave     );
    FRAHLEditor.KeepTrailingBlanks := ReadBool(S + 'Params', 'KeepTrailingBlanks', FRAHLEditor.KeepTrailingBlanks);
    FRAHLEditor.AutoIndent := ReadBool(S + 'Params', 'AutoIndent', FRAHLEditor.AutoIndent);
    FRAHLEditor.SmartTab := ReadBool(S + 'Params', 'SmartTab', FRAHLEditor.SmartTab);
    FRAHLEditor.BackspaceUnindents := ReadBool(S + 'Params', 'BackspaceUnindents', FRAHLEditor.BackspaceUnindents);
    FRAHLEditor.GroupUndo := ReadBool(S + 'Params', 'GroupUndo', FRAHLEditor.GroupUndo);
    FRAHLEditor.CursorBeyondEOF := ReadBool(S + 'Params', 'CursorBeyondEOF', FRAHLEditor.CursorBeyondEOF);
    FRAHLEditor.SyntaxHighlighting := ReadBool(S + 'Params', 'SyntaxHighlighting', FRAHLEditor.SyntaxHighlighting);
    FRAHLEditor.TabStops := ReadString(S + 'Params', 'TabStops'        , FRAHLEditor.TabStops          );
    FRAHLEditor.RightMargin := ReadInteger(S + 'Params', 'RightMargin', FRAHLEditor.RightMargin);
  end
end;

procedure TRAHLEdPropDlg.SaveHighlighterColors(ARAHLEditor: TRAHLEditor; AHighlighter: THighlighter);
var
  Section: string;

  procedure SaveColor(AColor: TSymbolColor; const Prefix: string);
  begin
    FRegAuto.WriteString(Section, Prefix,
      ColorToString(AColor.ForeColor) +
      ', ' + ColorToString(AColor.BackColor) +
      ', ' + IntToStr(byte(AColor.Style)));
  end;

begin
  if FRegAuto = nil then
    Error(SHLEdPropDlg_RegAutoNotAssigned);
  Section := AddSlash2(FRegAutoSection) + HighLighters[AHighLighter];
  FRegAuto.WriteString(Section, 'BackColor', ColorToString(ARAHLEditor.Color));
  FRegAuto.WriteString(Section, 'FontName', ARAHLEditor.Font.Name);
  FRegAuto.WriteString(Section, 'Charset', IntToStr(ARAHLEditor.Font.Charset));
  FRegAuto.WriteInteger(Section, 'FontSize', ARAHLEditor.Font.Size);
  FRegAuto.WriteString(Section, 'RightMarginColor', ColorToString(ARAHLEditor.RightMarginColor));
  SaveColor(ARAHLEditor.Colors.Number      , 'Number');
  SaveColor(ARAHLEditor.Colors.Strings     , 'Strings');
  SaveColor(ARAHLEditor.Colors.Symbol      , 'Symbol');
  SaveColor(ARAHLEditor.Colors.Comment     , 'Comment');
  SaveColor(ARAHLEditor.Colors.Reserved    , 'Reserved');
  SaveColor(ARAHLEditor.Colors.Identifer   , 'Identifer');
  SaveColor(ARAHLEditor.Colors.Preproc     , 'Preproc');
  SaveColor(ARAHLEditor.Colors.FunctionCall, 'FunctionCall');
  SaveColor(ARAHLEditor.Colors.Declaration , 'Declaration');
  SaveColor(ARAHLEditor.Colors.Statement   , 'Statement');
  SaveColor(ARAHLEditor.Colors.PlainText   , 'PlainText');
end;

procedure TRAHLEdPropDlg.LoadHighlighterColors(ARAHLEditor: TRAHLEditor; AHighlighter: THighlighter);
var
  Section: string;

  procedure LoadColor(AColor: TSymbolColor; DefaultForeColor,
    DefaultBackColor: TColor; DefaultStyle: TFontStyles; const Prefix: string);
  var
    S, S1: string;
  begin
    S := FRegAuto.ReadString(Section, Prefix, ColorToString(DefaultForeColor) +
      ', ' + ColorToString(DefaultBackColor) +
      ', ' + IntToStr(byte(DefaultStyle)));
    S1 := Trim(SubStr(S, 0, ','));
    if S1 <> '' then
      AColor.ForeColor := StringToColor(S1)
    else
      AColor.ForeColor := DefaultForeColor;
    S1 := Trim(SubStr(S, 1, ','));
    if S1 <> '' then
      AColor.BackColor := StringToColor(S1)
    else
      AColor.BackColor := DefaultBackColor;
    S1 := Trim(SubStr(S, 2, ','));
    if S1 <> '' then
      AColor.Style := TFontStyles(byte(StrToInt(S1)))
    else
      AColor.Style := DefaultStyle;
  end;

begin
  if FRegAuto = nil then
    Error(SHLEdPropDlg_RegAutoNotAssigned);
  Section := AddSlash2(FRegAutoSection) + HighLighters[AHighLighter];
  LoadColor(ARAHLEditor.Colors.Number      , clNavy       , clWindow, [], 'Number');
  LoadColor(ARAHLEditor.Colors.Strings     , clMaroon     , clWindow, [], 'Strings');
  LoadColor(ARAHLEditor.Colors.Symbol      , clBlue       , clWindow, [], 'Symbol');
  LoadColor(ARAHLEditor.Colors.Comment     , clOlive      , clWindow, [fsItalic], 'Comment');
  LoadColor(ARAHLEditor.Colors.Reserved    , clWindowText , clWindow, [fsBold], 'Reserved');
  LoadColor(ARAHLEditor.Colors.Identifer   , clWindowText , clWindow, [], 'Identifer');
  LoadColor(ARAHLEditor.Colors.Preproc     , clGreen      , clWindow, [], 'Preproc');
  LoadColor(ARAHLEditor.Colors.FunctionCall, clWindowText , clWindow, [], 'FunctionCall');
  LoadColor(ARAHLEditor.Colors.Declaration , clWindowText , clWindow, [], 'Declaration');
  LoadColor(ARAHLEditor.Colors.Statement   , clWindowText , clWindow, [], 'Statement');
  LoadColor(ARAHLEditor.Colors.PlainText   , clWindowText , clWindow, [], 'PlainText');
  ARAHLEditor.Color := StringToColor(FRegAuto.ReadString(Section, 'BackColor', 'clWindow'));
  ARAHLEditor.Font.Name := FRegAuto.ReadString(Section, 'FontName', 'Courier New');
  ARAHLEditor.Font.Charset := FRegAuto.ReadInteger(Section, 'Charset', DEFAULT_CHARSET);
  ARAHLEditor.Font.Size := FRegAuto.ReadInteger(Section, 'FontSize', 10);
  ARAHLEditor.RightMarginColor := StringToColor(FRegAuto.ReadString(Section, 'RightMarginColor', 'clSilver'));
end;

function TRAHLEdPropDlg.Execute: boolean;
var
  F: Integer;
  Form: TRAHLEditorParamsForm;
begin
  if FRAHLEditor = nil then
    Error(SHLEdPropDlg_RAHLEditorNotAssigned);
  Form := TRAHLEditorParamsForm.Create(Application);
  with Form do
    try
      FHighlighter := FRAHLEditor.Highlighter;
      Params := Self;
      raColorSamples.IniStrings := FColorSamples;
      ParamsToControls;
      if FReadFrom = rfHLEditor then
        RAHLEditor1.Assign(FRAHLEditor);

      tsEditor.TabVisible := epEditor in FPages;
      tsColors.TabVisible := epColors in FPages;

      F := FActivePage;

      if Assigned(FOnDialogPopup) then
        FOnDialogPopup(Self, Form);

      if FRegAuto <> nil then
        F := FRegAuto.ReadInteger(AddSlash2(FRegAutoSection) + 'Params',
          'ActivePage', F);
      F := Max(Min(F, Pages.PageCount - 1), 0);
      if not Pages.Pages[F].TabVisible then
        Pages.ActivePage := Pages.FindNextPage(Pages.Pages[F], True, True)
      else
        Pages.ActivePage := Pages.Pages[F];
        
      Result := ShowModal = mrOk;
      if Result then
      begin
        ControlsToParams;
        FRAHLEditor.Assign(RAHLEditor1);
      end;

      if (FRegAuto <> nil) and (Pages.ActivePage <> nil) then
        FRegAuto.WriteInteger(AddSlash2(FRegAutoSection) + 'Params',
          'ActivePage', Pages.ActivePage.PageIndex);

      if Assigned(FOnDialogClosed) then
        FOnDialogClosed(Self, Form, Result);
          
    finally
      Free;
    end;
end;

procedure TRAHLEdPropDlg.LoadCurrentHighlighterColors;
begin
  LoadHighlighterColors(FRAHLEditor, FRAHLEditor.Highlighter);
end;

procedure TRAHLEdPropDlg.SaveCurrentHighlighterColors;
begin
  SaveHighlighterColors(FRAHLEditor, FRAHLEditor.Highlighter);
end;

procedure TRAHLEdPropDlg.SetColorSamples(const Value: TStrings);
begin
  FColorSamples.Assign(Value);
end;

function TRAHLEdPropDlg.IsPagesStored: Boolean;
begin
  Result := FPages = [epEditor, epColors];
end;


{ TRAHLEditorParamsForm }

procedure TRAHLEditorParamsForm.ParamsToControls;
var
  i: Integer;
begin
  cbDoubleClickLine   .Checked := Params.FRAHLEditor.DoubleClickLine   ;
  cbUndoAfterSave     .Checked := Params.FRAHLEditor.UndoAfterSave     ;
  cbKeepTrailingBlanks.Checked := Params.FRAHLEditor.KeepTrailingBlanks;
  cbAutoIndent        .Checked := Params.FRAHLEditor.AutoIndent        ;
  cbSmartTab          .Checked := Params.FRAHLEditor.SmartTab          ;
  cbBackspaceUnindents.Checked := Params.FRAHLEditor.BackspaceUnindents;
  cbGroupUndo         .Checked := Params.FRAHLEditor.GroupUndo         ;
  cbCursorBeyondEOF   .Checked := Params.FRAHLEditor.CursorBeyondEOF   ;
  cbSytaxHighlighting .Checked := Params.FRAHLEditor.SyntaxHighlighting ;
  eTabStops.Text               := Params.FRAHLEditor.TabStops;
  cbColorSettings.ItemIndex := Integer(FHighlighter);
  cbColorSettingsChange(nil);
  RAHLEditor1.RightMargin := Params.FRAHLEditor.RightMargin;
  cbColorSettings.Visible := Params.FHighlighterCombo;
  lblColorSpeedSettingsFor.Visible := Params.FHighlighterCombo;
  if not Params.FHighlighterCombo then
  begin
    for i :=0 to tsColors.ControlCount - 1 do
      tsColors.Controls[i].Top := tsColors.Controls[i].Top - Pixels(tsColors, 24);
    RAHLEditor1.Height := RAHLEditor1.Height + Pixels(tsColors, 24);
  end;
end;

procedure TRAHLEditorParamsForm.ControlsToParams;
begin
  Params.FRAHLEditor.DoubleClickLine    := cbDoubleClickLine   .Checked;
  Params.FRAHLEditor.UndoAfterSave      := cbUndoAfterSave     .Checked;
  Params.FRAHLEditor.KeepTrailingBlanks := cbKeepTrailingBlanks.Checked;
  Params.FRAHLEditor.AutoIndent         := cbAutoIndent        .Checked;
  Params.FRAHLEditor.SmartTab           := cbSmartTab          .Checked;
  Params.FRAHLEditor.BackspaceUnindents := cbBackspaceUnindents.Checked;
  Params.FRAHLEditor.GroupUndo          := cbGroupUndo         .Checked;
  Params.FRAHLEditor.CursorBeyondEOF    := cbCursorBeyondEOF   .Checked;
  Params.FRAHLEditor.SyntaxHighlighting := cbSytaxHighlighting .Checked;
  Params.FRAHLEditor.TabStops           := eTabStops.Text;
  if Params.FRegAuto <> nil then
    Params.SaveHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
end;

procedure TRAHLEditorParamsForm.FormCreate(Sender: TObject);
begin
  LoadLocale;
  RAHLEditor1 := TSampleViewer.Create(Self);
  RAHLEditor1.Parent := tsColors;
  RAHLEditor1.SetBounds(8, 176, 396, 110);
  RAHLEditor1.TabStop := False;
  cbKeyboardLayot.ItemIndex := 0;
  cbColorSettings.ItemIndex := 0;
  lbElements.ItemIndex := 0;
  lbElementsClick(nil);
end;

procedure TRAHLEditorParamsForm.NotImplemented(Sender: TObject);
begin
  //(Sender as TCheckBox).Checked := true;
  //raise Exception.Create(SHLEdPropDlg_OptionCantBeChanged);
end;


{ Color tab }

{ color grid }

function TRAHLEditorParamsForm.GetCell(const Index: Integer): TPanel;
begin
  Result := FindComponent('Cell' + IntToStr(Index)) as TPanel;
  if Result = nil then
    Error('Grid cell not found');
end;

function TRAHLEditorParamsForm.ColorToIndex(const AColor: TColor): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 15 do
    if GetCell(i).Color = AColor then
    begin
      Result := i;
      Exit;
    end;
end;

function TRAHLEditorParamsForm.GetColorIndex(const ColorName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 15 do
    if (GetCell(i).Caption = 'FB') or (GetCell(i).Caption = ColorName) then
    begin
      Result := i;
      Exit;
    end;
end;

function TRAHLEditorParamsForm.GetForegroundIndex: Integer;
begin
  Result := GetColorIndex('FC');
end;

function TRAHLEditorParamsForm.GetBackgroundIndex: Integer;
begin
  Result := GetColorIndex('BC');
end;

function TRAHLEditorParamsForm.GetColorColor(const ColorName: string): TColor;
var
  Index: Integer;
begin
  Index := GetColorIndex(ColorName);
  if Index > -1 then
    Result := GetCell(Index).Color
  else
    Result := clBlack;
end;

function TRAHLEditorParamsForm.GetForegroundColor: TColor;
begin
  Result := GetColorColor('FC');
end;

function TRAHLEditorParamsForm.GetBackgroundColor: TColor;
begin
  Result := GetColorColor('BC');
end;

procedure TRAHLEditorParamsForm.SetColorIndex(const Index: Integer;
  const ColorName, OtherColorName: string);
var
  i: Integer;
begin
  for i := 0 to 15 do
    if (GetCell(i).Caption = 'FB') or (GetCell(i).Caption = ColorName) then
      GetCell(i).Caption := ColorName
    else
      GetCell(i).Caption := '';
  if Index > -1 then
    if GetCell(Index).Caption = ColorName then
      GetCell(Index).Caption := 'FB'
    else
      GetCell(Index).Caption := OtherColorName;
end;

procedure TRAHLEditorParamsForm.SetForegroundIndex(const Index: Integer);
begin
  SetColorIndex(Index, 'BC', 'FC');
end;

procedure TRAHLEditorParamsForm.SetBackgroundIndex(const Index: Integer);
begin
  SetColorIndex(Index, 'FC', 'BC');
end;

procedure TRAHLEditorParamsForm.CellMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SetForegroundIndex((Sender as TPanel).Tag)
  else if Button = mbRight then
    SetBackgroundIndex((Sender as TPanel).Tag);
  ColorChanged(Sender);
end;
 { color grid ### }

procedure TRAHLEditorParamsForm.lbElementsClick(Sender: TObject);
var
  FC, BC: TColor;
  ST: TFontStyles;
begin
  InChanging := True;
  try
  SC := nil;
  ST := [];
  FC := clWindowText;
  BC := clWindow;
  case lbElements.ItemIndex of
    0: { Whitespace }
      begin
        FC := RAHLEditor1.Font.Color;
        BC := RAHLEditor1.Color;
      end;
    1: { Comment }
      SC := RAHLEditor1.Colors.Comment;
    2: { Reserved word }
      SC := RAHLEditor1.Colors.Reserved;
    3: { Identifer }
      SC := RAHLEditor1.Colors.Identifer;
    4: { Symbol }
      SC := RAHLEditor1.Colors.Symbol;
    5: { String }
      SC := RAHLEditor1.Colors.Strings;
    6: { Number }
      SC := RAHLEditor1.Colors.Number;
    7: { Preprocessor }
      SC := RAHLEditor1.Colors.Preproc;
    8: { Declaration }
      SC := RAHLEditor1.Colors.Declaration;
    9: { Function call }
      SC := RAHLEditor1.Colors.FunctionCall;
    10: { VB Statement }
      SC := RAHLEditor1.Colors.Statement;
    11: { PlainText }
      SC := RAHLEditor1.Colors.PlainText;
    12: { Marked block }
      begin
        FC := RAHLEditor1.SelForeColor;
        BC := RAHLEditor1.SelBackColor;
      end;
    13: { Right margin }
      begin
        FC := RAHLEditor1.RightMarginColor;
        BC := -1;
      end;
  end;
  if SC <> nil then
  begin
		FC := SC.ForeColor;
		BC := SC.BackColor;
    ST := SC.Style;
  end;
  cbDefForeground.Checked := ((lbElements.ItemIndex < 12) and (FC = clWindowText)) or
    ((lbElements.ItemIndex = 12) and (FC = clHighlightText));
  cbDefBackground.Checked := ((lbElements.ItemIndex < 12) and (BC = clWindow)) or
    ((lbElements.ItemIndex = 12) and (BC = clHighlight));
  if not cbDefForeground.Checked then
    SetForegroundIndex(ColorToIndex(FC))
  else
    SetForegroundIndex(-1);
  if not cbDefBackground.Checked then
    SetBackgroundIndex(ColorToIndex(BC))
  else
    SetBackgroundIndex(-1);
  cbBold.Checked := fsBold in ST;
  cbItalic.Checked := fsItalic in ST;
  cbUnderline.Checked := fsUnderline in ST;
  finally
    InChanging := False;
  end;
end;

procedure TRAHLEditorParamsForm.ColorChanged(Sender: TObject);
var
  FC, BC: TColor;
  ST: TFontStyles;
begin
  if InChanging then Exit;
  InChanging := True;
  try
  ST := [];
  if GetForegroundIndex <> -1 then
    cbDefForeground.Checked := False;
  if GetBackgroundIndex <> -1 then
    cbDefBackground.Checked := False;
	if cbDefForeground.Checked then
	begin
		if lbElements.ItemIndex = 12 then
			FC := clHighlightText
    else
			FC := clWindowText;
    SetForegroundIndex(-1);
	end
	else
		FC := GetForegroundColor;
	if cbDefBackground.Checked then
	begin
		if lbElements.ItemIndex = 12 then { marked block }
			BC := clHighlight
    else
			BC := clWindow;
    SetBackgroundIndex(-1);
	end
	else
		BC := GetBackgroundColor;
	if cbBold.Checked then Include(ST, fsBold);
	if cbItalic.Checked then Include(ST, fsItalic);
	if cbUnderline.Checked then Include(ST, fsUnderline);
  if SC <> nil then
  begin
    SC.Style := ST;
    SC.ForeColor := FC;
    SC.BackColor := BC;
  end
  else if lbElements.ItemIndex = 12 then { marked block }
  begin
    RAHLEditor1.SelForeColor := FC;
    RAHLEditor1.SelBackColor := BC;
  end
  else if lbElements.ItemIndex = 0 then { whitespace }
    RAHLEditor1.Color := BC
  else if lbElements.ItemIndex = 13 then { right margin }
    RAHLEditor1.RightMarginColor := FC;
  RAHLEditor1.Invalidate;
  finally
    InChanging := False;
  end;
end;

procedure TRAHLEditorParamsForm.DefClick(Sender: TObject);
begin
  if InChanging then Exit;
  if cbDefForeground.Checked then
     SetForegroundIndex(-1);
  if cbDefBackground.Checked then
    SetBackgroundIndex(-1);
  ColorChanged(nil);
end;

procedure TRAHLEditorParamsForm.lbElementsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
	with (Control as TListBox).Canvas do  { draw on control canvas, not on the form }
	begin
    FillRect(Rect);       { clear the rectangle }
    TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index])  { display the text }
	end;
end;

procedure TRAHLEditorParamsForm.cbColorSettingsChange(Sender: TObject);
var
  i: Integer;
begin
  if (Sender <> nil) and (Params.FRegAuto <> nil) then
    Params.SaveHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
  raColorSamples.ReadSection(cbColorSettings.Text, RAHLEditor1.Lines);
  RAHLEditor1.Highlighter := THighlighter(cbColorSettings.ItemIndex);
  if RAHLEditor1.HighLighter = hlIni then
    for i := 0 to RAHLEditor1.Lines.Count - 1 do
      RAHLEditor1.Lines[i] := Copy(RAHLEditor1.Lines[i], 2, 10000);
  if Params.FRegAuto <> nil then
    Params.LoadHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
  lbElementsClick(nil);
end;

procedure TRAHLEditorParamsForm.LoadLocale;
begin
  Caption := SHLEdPropDlg_Caption;
  tsEditor.Caption := SHLEdPropDlg_tsEditor;
  tsColors.Caption := SHLEdPropDlg_tsColors;
  lblEditorSpeedSettings.Caption := SHLEdPropDlg_lblEditorSpeedSettings;
  cbKeyboardLayot.Items[0] := SHLEdPropDlg_cbKeyboardLayotDefault;
  gbEditor.Caption := SHLEdPropDlg_gbEditor;
  cbAutoIndent .Caption := SHLEdPropDlg_cbAutoIndent ;
  cbSmartTab.Caption := SHLEdPropDlg_cbSmartTab;
  cbBackspaceUnindents.Caption := SHLEdPropDlg_cbBackspaceUnindents;
  cbGroupUndo .Caption := SHLEdPropDlg_cbGroupUndo ;
  cbCursorBeyondEOF.Caption := SHLEdPropDlg_cbCursorBeyondEOF;
  cbUndoAfterSave .Caption := SHLEdPropDlg_cbUndoAfterSave ;
  cbKeepTrailingBlanks .Caption := SHLEdPropDlg_cbKeepTrailingBlanks ;
  cbDoubleClickLine .Caption := SHLEdPropDlg_cbDoubleClickLine ;
  cbSytaxHighlighting.Caption := SHLEdPropDlg_cbSytaxHighlighting;
  lblTabStops .Caption := SHLEdPropDlg_lblTabStops ;
  lblColorSpeedSettingsFor.Caption := SHLEdPropDlg_lblColorSpeedSettingsFor;
  lblElement .Caption := SHLEdPropDlg_lblElement ;
  lblColor .Caption := SHLEdPropDlg_lblColor ;
  gbTextAttributes.Caption := SHLEdPropDlg_gbTextAttributes;
  gbUseDefaultsFor.Caption := SHLEdPropDlg_gbUseDefaultsFor;
  cbBold .Caption := SHLEdPropDlg_cbBold ;
  cbItalic .Caption := SHLEdPropDlg_cbItalic ;
  cbUnderline .Caption := SHLEdPropDlg_cbUnderline ;
  cbDefForeground .Caption := SHLEdPropDlg_cbDefForeground ;
  cbDefBackground .Caption := SHLEdPropDlg_cbDefBackground ;
  bOK.Caption := SOK;
  bCancel.Caption := SCancel;
end;

 { TSampleViewer }
constructor TSampleViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TmpEd := TRAHLEditor.Create(Self);
  TmpEd.Visible := False;
  TmpEd.Parent := Self;
end;    { Create }

procedure TSampleViewer.WndProc(var Message: TMessage);
begin
  case Message.Msg of    { }
    {WM_LBUTTONDOWN,} WM_LBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP,
    WM_MOUSEMOVE, WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK :
      { nothing - prevent user interact };
    else
      inherited WndProc(Message);
  end;    { case }
end;

procedure TSampleViewer.WMLButtonDown(var Message: TWMLButtonDown);
var
  XX, YY: integer;
  F: integer;
begin
  { also prevent user interact }
  { detect symbol type }
	Mouse2Caret(Message.XPos, Message.YPos, XX, YY);
  if (XX = RightMargin) or (XX - 1 = RightMargin) then
    F := 13
  else
  begin
    TmpEd.Lines := Lines;
    TmpEd.HighLighter := HighLighter;
   { color values corresponds to lbElements ListBox }
    TmpEd.Font.Color := 0;
    TmpEd.Colors.Comment.ForeColor := 1;
    TmpEd.Colors.Reserved.ForeColor := 2;
    TmpEd.Colors.Identifer.ForeColor := 3;
    TmpEd.Colors.Symbol.ForeColor := 4;
    TmpEd.Colors.Strings.ForeColor := 5;
    TmpEd.Colors.Number.ForeColor := 6;
    TmpEd.Colors.Preproc.ForeColor := 7;
    TmpEd.Colors.Declaration.ForeColor := 8;
    TmpEd.Colors.FunctionCall.ForeColor := 9;
    TmpEd.Colors.Statement.ForeColor := 10;
    TmpEd.Colors.PlainText.ForeColor := 11;
    TmpEd.SelForeColor := 12;
    TSampleViewer(TmpEd).GetLineAttr(YY, 0, RAEditor.Max_X - 1);
    F := TSampleViewer(TmpEd).LineAttrs[XX].FC;
  end;
	(Owner as TRAHLEditorParamsForm).lbElements.ItemIndex :=F;
  (Owner as TRAHLEditorParamsForm).lbElementsClick((Owner as TRAHLEditorParamsForm).lbElements);
end;



end.
