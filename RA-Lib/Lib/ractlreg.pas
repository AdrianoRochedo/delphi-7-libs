{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Register custom useful controls

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm.htm
************************************************************}

{$INCLUDE RA.INC}

unit ractlreg;

interface

procedure Register;

implementation

uses
  Windows, SysUtils, Classes, Controls,
  RAUtils, RAStream, RAHook, RACnst, RADsgn, RADsgnIntf,
  RARegAuto, RARegAutoEditor, RACt, RADlg, RAHint, RAScrollBar, RATreeView,
  RAButtons, RACombo, RAComponentPanel, RAScrollMax, RAImage,
  RAEditor, RAHLEditor, RAHLEdPropDlg, RAScrollText, fRAhtHintEditor,
  RADoubleCombo,
  {$IFDEF RA_D3H}
   Zoom,
  {$ENDIF RA_D3H}
  {$IFDEF RA_D6H}
   DesignIntf, DesignEditors, VCLEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF RA_D6H}
;

{$R ractl.dcr}

type

  TRAScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount : integer; override;
    function GetVerb(Index : integer) : string; override;
    procedure ExecuteVerb(Index : integer); override;
    procedure Edit; override;
  end;

  TRAHLEdPropDlgEditor = class(TComponentEditor)
  public
    function GetVerbCount : integer; override;
    function GetVerb(Index : integer) : string; override;
    procedure ExecuteVerb(Index : integer); override;
    procedure Edit; override;
  end;

  TRAHLEditorColorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TRAScrollMaxEditor }
function TRAScrollMaxEditor.GetVerbCount : integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TRAScrollMaxEditor.GetVerb(Index : integer) : string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add Band'
  else
    Result := inherited GetVerb(Index);
end;

procedure TRAScrollMaxEditor.ExecuteVerb(Index : integer);
begin
  if Index = GetVerbCount - 1 then
    Designer.CreateComponent(TRAScrollMaxBand, Component, 0, 0, 0, 50)
  else
    inherited ExecuteVerb(Index);
end;

procedure TRAScrollMaxEditor.Edit;
begin
 // We don't need to add band on double click
end;

{ TRAHLEdPropDlgEditor }
function TRAHLEdPropDlgEditor.GetVerbCount : integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TRAHLEdPropDlgEditor.GetVerb(Index : integer) : string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Execute'
  else
    Result := inherited GetVerb(Index);
end;

procedure TRAHLEdPropDlgEditor.ExecuteVerb(Index : integer);
begin
  if Index = GetVerbCount - 1 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

procedure TRAHLEdPropDlgEditor.Edit;
var
  PakName: string;
  newRegAuto: TRegAuto;
  oldRegAuto: TRegAuto;
begin
  newRegAuto := TRegAuto.Create(nil);
  try
    newRegAuto.UseReg := False;
    newRegAuto.UseIni := True;
    newRegAuto.UseStr := False;
    SetLength(PakName, 260);
    SetLength(PakName, GetModuleFileName(hInstance, PChar(PakName), 260));
    newRegAuto.IniFile := ExtractFilePath(PakName) + 'RAHLEdPropDlg.ini';
    oldRegAuto := (Component as TRAHLEdPropDlg).RegAuto;
    (Component as TRAHLEdPropDlg).RegAuto := newRegAuto;
    if (Component as TRAHLEdPropDlg).Execute then
      Designer.Modified;
    (Component as TRAHLEdPropDlg).RegAuto := oldRegAuto;
  finally
    newRegAuto.Free;
  end;
end;

procedure TRAHLEditorColorProperty.Edit;
begin
  with TRAHLEdPropDlg.Create(nil) do
    try
      RAHLEditor := GetComponent(0) as TRAHLEditor;
      HighlighterCombo := False;
      ReadFrom := rfHLEditor;
      Pages := [epColors];
      if Execute then
        Designer.Modified;
    finally
      Free;
    end;
end;

function TRAHLEditorColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{$IFDEF RA_D5}
type
  TRAEditorCategory = class (TPropertyCategory)
  public
    class function Name: String; override;
  end;
{ TEditorCategory }


class function TRAEditorCategory.Name: String;
begin
  Result := 'Editor';
end;
{$ENDIF RA_D5H}


procedure Register;
begin
 {RARegAuto unit}
  RegisterComponents('R&&A Controls', [TRegAuto]);
  RegisterComponentEditor(TRegAuto, TRegAutoEditor);
 {RACt unit}
  RegisterComponents('R&&A Controls', [TRAhtListBox, TRAhtComboBox, TRAhtLabel]);
  RegisterPropertyEditor(TypeInfo(TCaption), TRAhtLabel, 'Caption', TRAHintProperty);
 {RAButtons unit}
  RegisterComponents ('R&&A Controls', [TRAhtButton, TRACaptionButton]);
 {RADlg unit}
  RegisterComponents('R&&A Controls', [TRAProgressForm]);
 {RAEditor unit}
  RegisterComponents('R&&A Controls', [TRAEditor]);
 {RAHLEditor unit}
  RegisterComponents('R&&A Controls', [TRAHLEditor]);
 {RAHLEdPropDlg unit}
  RegisterComponents('R&&A Controls', [TRAHLEdPropDlg]);
  RegisterComponentEditor(TRAHLEdPropDlg, TRAHLEdPropDlgEditor);
  RegisterPropertyEditor(TypeInfo(TColors), TRAHLEditor, 'Colors', TRAHLEditorColorProperty);
 {RAHint unit}
  RegisterComponents('R&&A Controls', [TRAHint]);
  RegisterPropertyEditor(TypeInfo(string), TObject, 'Hint', TRAHintProperty);
 {RACombo unit}
  RegisterComponents('R&&A Controls', [TRACombo]);
 {RAComponentPanel unit}
  RegisterComponents('R&&A Controls', [TRAComponentPanel]);
 {$IFDEF RA_150_COMPAT}
  RegisterComponents ('R&&A Controls', [TRANoFrameButton, TRAColorButton]);
 {$ENDIF RA_150_COMPAT}
 {RAScrollMax unit}
  RegisterComponents('R&&A Controls', [TRAScrollMax]);
  RegisterClass(TRAScrollMaxBand);
  RegisterComponentEditor(TRAScrollMax, TRAScrollMaxEditor);
 {RAScrollText}
  RegisterComponents('R&&A Controls', [TRAScrollText]);
 {RADoubleCombo}
  RegisterComponents('R&&A Controls', [TRADoubleCombo]);
 {Zoom unit}
 {$IFDEF RA_D3H}
  RegisterZoom;
 {$ENDIF RA_D3H}

 {RATreeView unit}
  RegisterComponents('R&&A Additional', [TRAComboBox4Tree, TRATreeView]);
 {RAScrollBar unit}
  RegisterComponents('R&&A Additional', [TRAScrollBar, TRAScrollBar95]);
 {RACt unit}
  RegisterComponents('R&&A Additional', [TRegAutoGrid, TRAStatusBar,
    TRAWaitPanel, TRACheckBox]);
 {RAImage unit}
  RegisterComponents('R&&A Additional', [TRAImage]);
 {RAButtons unit} 
  RegisterComponents ('R&&A Additional', [TRANoFrameButton, TRAColorButton]);

 {$IFDEF RA_D5}
  RegisterPropertiesInCategory(TRAEditorCategory, TRACustomEditor,
    ['InsertMode', 'DoubleClickLine', 'Completion', 'SmartTab',
    'BackSpaceUnindents', 'AutoIndent', 'KeepTrailingBlanks', 'CursorBeyondEOF',
    'GutterColor', 'GutterWidth',
    'RightMarginVisible', 'RightMargin', 'RightMarginColor',

    'OnGetLineAttr', 'OnReservedWord', 'OnCompletionIdentifer',
    'OnCompletionDrawItem', 'OnCompletionMeasureItem', 'OnCompletionTemplate',
    'OnChange', 'OnChangeStatus', 'OnSelectionChange']);

  RegisterPropertiesInCategory(TVisualCategory, TRACustomEditor,
    ['ScrollBars',
    'RightMarginVisible', 'RightMargin', 'RightMarginColor',
    'OnPaintGutter', 'OnScroll', 'OnConstrainedResize']);
 {$ENDIF}
end;

end.
