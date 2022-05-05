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
  SDeleteNode2            = 'Smazat %s (se v�emi pod��zen�mi uzly) ?';
  SMasterFieldEmpty       = 'Vlastnost "MasterField" mus� b�t vypln�na';
  SDetailFieldEmpty       = 'Vlastnost "DetailField" mus� b�t vypln�na';
  SItemFieldEmpty         = 'Vlastnost "ItemField" mus� b�t vypln�na';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" mus� b�t stejn�ho typu';
  SMasterFieldError       = 'Vlastnost "MasterField" mus� b�t cel� ��slo';
  SDetailFieldError       = 'Vlastnost "DetailField" mus� b�t cel� ��slo';
  SItemFieldError         = 'Vlastnost "ItemField" mus� b�t �et�zec, datum nebo cel� ��slo';
  SIconFieldError         = 'Vlastnost "IconField" mus� b�t cel� ��slo';
  SMoveToModeError        = 'Chybn� p�esun uzlu v RADBTreeNode';
  SDataSetNotActive       = 'DataSet nen� aktivn�';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Nejprve mus�te zadat jm�no vlastnosti';
  sRegAutoEditorTreeHint       = 'Dostupn� vlastnosti';
  sRegAutoEditorListHint       = 'Ulo�en� vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'P�idat/odstranit vlastnost';
  sRegAutoEditorSort           = 'Set��dit';

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
  SScrollBarRange = 'Hodnota �oup�tka je mimo hranice';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Storno';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh�� &menu';
  SMDInsertItem         = '&Vlo�it';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvo�it &podmenu';

  SCantGetShortCut      = 'Soubor u z�stupce %s nen� dostupn�';


 { RALib 1.23 }
  SPropertyNotExists    = 'Vlastnost "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnost "%s" je �patn�ho typu';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastnost�';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Barvy';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'P�ednastaven� mapov�n� kl�ves';
  SHLEdPropDlg_gbEditor = 'Vlastnosti editoru:';
  SHLEdPropDlg_cbAutoIndent = '&Auto odsazovac� m�d';
  SHLEdPropDlg_cbSmartTab = 'Ch&ytr� tabul�tor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Odsazen� p�i &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Skupinov� zp�t';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Kurzor i za &konec souboru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umo�nit z&p�t po ulo�en�';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Dr�et koncov� mezery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na ��dku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Pou�ij z&v�razn�n� syntaxe';
  SHLEdPropDlg_lblTabStops = 'Stop &tabul�toru na:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Barva SpeedSettings pro';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Barva:';
  SHLEdPropDlg_gbTextAttributes = 'Textov� atributy:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Pou�ij p�ednastaven� pro:';
  SHLEdPropDlg_cbBold = '&Tu�n�';
  SHLEdPropDlg_cbItalic = '&Kurz�va';
  SHLEdPropDlg_cbUnderline = '&Podtr�eno';
  SHLEdPropDlg_cbDefForeground = 'P&op�ed�';
  SHLEdPropDlg_cbDefBackground = 'Poza&d�';
  SHLEdPropDlg_OptionCantBeChanged = 'Tato mo�nost nem��e b�t zm�n�na.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnost RAHLEditor nen� p�i�azena';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnost RegAuto nen� p�i�azena';

  SNoReportProc = 'Procedura "RAI2RunReportPreview" nenalezena';
  SNoReportProc2 = 'Procedura "RAI2RunReportPreview2" nenalezena';

implementation

end.
