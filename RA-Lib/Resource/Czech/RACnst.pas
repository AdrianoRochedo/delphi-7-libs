{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Language specific constant for English

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = 'Smazat %s ?';
  SDeleteNode2            = 'Smazat %s (se všemi podøízenými uzly) ?';
  SMasterFieldEmpty       = 'Vlastnost "MasterField" musí být vyplnìna';
  SDetailFieldEmpty       = 'Vlastnost "DetailField" musí být vyplnìna';
  SItemFieldEmpty         = 'Vlastnost "ItemField" musí být vyplnìna';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" musí být stejného typu';
  SMasterFieldError       = 'Vlastnost "MasterField" musí být celé èíslo';
  SDetailFieldError       = 'Vlastnost "DetailField" musí být celé èíslo';
  SItemFieldError         = 'Vlastnost "ItemField" musí být øetìzec, datum nebo celé èíslo';
  SIconFieldError         = 'Vlastnost "IconField" musí být celé èíslo';
  SMoveToModeError        = 'Chybný pøesun uzlu v RADBTreeNode';
  SDataSetNotActive       = 'DataSet není aktivní';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Nejprve musíte zadat jméno vlastnosti';
  sRegAutoEditorTreeHint       = 'Dostupné vlastnosti';
  sRegAutoEditorListHint       = 'Uložené vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'Pøidat/odstranit vlastnost';
  sRegAutoEditorSort           = 'Setøídit';

 {RAEditor}
  RAEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

{IParser}
 {$IFDEF RA_D}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF RA_D}
 {$IFDEF RA_B}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF RA_B}

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z',  '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '_'];
{$ENDIF RAINTER}

{$IFDEF RA_D2}
  SScrollBarRange = 'Hodnota šoupátka je mimo hranice';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Storno';

 { Menu Designer }
  SMDMenuDesigner       = 'Návrháø &menu';
  SMDInsertItem         = '&Vložit';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvoøit &podmenu';

  SCantGetShortCut      = 'Soubor u zástupce %s není dostupný';


 { RALib 1.23 }
  SPropertyNotExists    = 'Vlastnost "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnost "%s" je špatného typu';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastností';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Barvy';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Pøednastavené mapování kláves';
  SHLEdPropDlg_gbEditor = 'Vlastnosti editoru:';
  SHLEdPropDlg_cbAutoIndent = '&Auto odsazovací mód';
  SHLEdPropDlg_cbSmartTab = 'Ch&ytrý tabulátor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Odsazení pøi &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Skupinové zpìt';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Kurzor i za &konec souboru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umožnit z&pìt po uložení';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Držet koncové mezery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na øádku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Použij z&výraznìní syntaxe';
  SHLEdPropDlg_lblTabStops = 'Stop &tabulátoru na:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Barva SpeedSettings pro';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Barva:';
  SHLEdPropDlg_gbTextAttributes = 'Textové atributy:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Použij pøednastavení pro:';
  SHLEdPropDlg_cbBold = '&Tuènì';
  SHLEdPropDlg_cbItalic = '&Kurzíva';
  SHLEdPropDlg_cbUnderline = '&Podtrženo';
  SHLEdPropDlg_cbDefForeground = 'P&opøedí';
  SHLEdPropDlg_cbDefBackground = 'Poza&dí';
  SHLEdPropDlg_OptionCantBeChanged = 'Tato možnost nemùže být zmìnìna.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnost RAHLEditor není pøiøazena';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnost RegAuto není pøiøazena';

  SNoReportProc = 'Procedura "RAI2RunReportPreview" nenalezena';
  SNoReportProc2 = 'Procedura "RAI2RunReportPreview2" nenalezena';

implementation

end.
