{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Language specific constants for Russian

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = '������� %s ?';
  SDeleteNode2            = '������� %s (������ �� ���� ����������) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = '�������� ����� ����������� RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

   {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = '��� �������� ����� ������ ����� �����';
  sRegAutoEditorTreeHint       = '��������� ��������';
  sRegAutoEditorListHint       = '������ ����������� �������';
  sRegAutoEditorBtnAddPropHint = '��������/������� ��������';
  sRegAutoEditorSort           = '�������������';

 {RAEditor}
  RAEditorCompletionChars = #8+'_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';

{IParser and RAI2Parser}
 {$IFDEF RA_D}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z', '�'..'�', '�'..'�'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z', '�'..'�', '�'..'�'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF RA_D}
 {$IFDEF RA_B}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF RA_B}

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z', '�'..'�', '�'..'�', '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '�'..'�', '�'..'�', '_'];
{$ENDIF RAINTER}

{$IFDEF RA_D2}
  SScrollBarRange = '�������� Scrollbar ����� �� ���������� �������';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = '������';

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
  SHLEdPropDlg_Caption = '��������: ��������';
  SHLEdPropDlg_tsEditor = '��������';
  SHLEdPropDlg_tsColors = '�����';
  SHLEdPropDlg_lblEditorSpeedSettings = '����� ������� ������';
  SHLEdPropDlg_cbKeyboardLayotDefault = '����������� �����';
  SHLEdPropDlg_gbEditor = '��������� ���������:';
  SHLEdPropDlg_cbAutoIndent = '&����������';
  SHLEdPropDlg_cbSmartTab = '&����� ���';
  SHLEdPropDlg_cbBackspaceUnindents = '����� &�������� �����';
  SHLEdPropDlg_cbGroupUndo = '&��������� ������';
  SHLEdPropDlg_cbCursorBeyondEOF = '������ �� ����� &�����';
  SHLEdPropDlg_cbUndoAfterSave = '������ &����� ����������';
  SHLEdPropDlg_cbKeepTrailingBlanks = '��������� &����������� �������';
  SHLEdPropDlg_cbDoubleClickLine = '&������� ���� �������� ������';
  SHLEdPropDlg_cbSytaxHighlighting = '�������� &���������';
  SHLEdPropDlg_lblTabStops = '&�����������:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = '����� ���';
  SHLEdPropDlg_lblElement = '&�������:';
  SHLEdPropDlg_lblColor = '&����:';
  SHLEdPropDlg_gbTextAttributes = '�������� ������:';
  SHLEdPropDlg_gbUseDefaultsFor = '�����������:';
  SHLEdPropDlg_cbBold = '&������';
  SHLEdPropDlg_cbItalic = '&���������';
  SHLEdPropDlg_cbUnderline = '&������������';
  SHLEdPropDlg_cbDefForeground = '&�����';
  SHLEdPropDlg_cbDefBackground = '&���';
  SHLEdPropDlg_OptionCantBeChanged = '���� �������� ������ ������. ��������.';
  SHLEdPropDlg_RAHLEditorNotAssigned = '�������� RAHLEditor �� ���������';
  SHLEdPropDlg_RegAutoNotAssigned = '�������� RegAuto �� ���������';

  SNoReportProc = '�� ������� ��������� "RAI2RunReportPreview"';
  SNoReportProc2 = '�� ������� ��������� "RAI2RunReportPreview2"';
  
implementation

end.








