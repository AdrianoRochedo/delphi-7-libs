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
  SDeleteNode             = 'Delete %s ?';
  SDeleteNode2            = 'Delete %s (with all children) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = 'Invalid move mode for RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'You can type property name here';
  sRegAutoEditorTreeHint       = 'Available properties';
  sRegAutoEditorListHint       = 'Stored properties';
  sRegAutoEditorBtnAddPropHint = 'Add/Remove property';
  sRegAutoEditorSort           = 'Sort';

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
  SScrollBarRange = 'Scrollbar value out of bounds';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Cancel';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';

  SCantGetShortCut      = 'Target FileName for ShortCut %s not available';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Property "%s" does not exists';
  SInvalidPropertyType  = 'Property "%s" has invalid type';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor Properties';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colors';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Default keymapping';
  SHLEdPropDlg_gbEditor = 'Editor options:';
  SHLEdPropDlg_cbAutoIndent = '&Auto indent mode';
  SHLEdPropDlg_cbSmartTab = 'S&mart tab';
  SHLEdPropDlg_cbBackspaceUnindents = 'Backspace &unindents';
  SHLEdPropDlg_cbGroupUndo = '&Group undo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor beyond &EOF';
  SHLEdPropDlg_cbUndoAfterSave = '&Undo after sa&ve';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Keep trailing blanks';
  SHLEdPropDlg_cbDoubleClickLine = '&Double click line';
  SHLEdPropDlg_cbSytaxHighlighting = 'Use &syntax highlight';
  SHLEdPropDlg_lblTabStops = '&Tab stops:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Color SpeedSettings for';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Text attributes:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Use defaults for:';
  SHLEdPropDlg_cbBold = '&Bold';
  SHLEdPropDlg_cbItalic = '&Italic';
  SHLEdPropDlg_cbUnderline = '&Underline';
  SHLEdPropDlg_cbDefForeground = '&Foreground';
  SHLEdPropDlg_cbDefBackground = '&Background';
  SHLEdPropDlg_OptionCantBeChanged = 'This option can''t be changed. Sorry.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'RAHLEditor property is not assigned';
  SHLEdPropDlg_RegAutoNotAssigned = 'RegAuto property is not assigned';

  SNoReportProc = 'Procedure "RAI2RunReportPreview" not found';
  SNoReportProc2 = 'Procedure "RAI2RunReportPreview2" not found';

implementation

end.
