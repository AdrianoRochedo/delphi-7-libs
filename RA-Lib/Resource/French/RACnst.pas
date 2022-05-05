{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov

       description : Language specific constant for French
French translation : KNIPPER John (knipjo@altavista.net)

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = 'Effacer %s ?';
  SDeleteNode2            = 'Effacer %s (avec tous les fils) ?';
  SMasterFieldEmpty       = 'La propri�t� "MasterField" doit �tre renseign�';
  SDetailFieldEmpty       = 'La propri�t� "DetailField" doit �tre renseign�';
  SItemFieldEmpty         = 'La propri�t� "ItemField" doit �tre renseign�';
  SMasterDetailFieldError = '"MasterField" et "DetailField" doivent �tre du m�me type';
  SMasterFieldError       = '"MasterField" doit �tre un entier';
  SDetailFieldError       = '"DetailField" doit �tre un entier';
  SItemFieldError         = '"ItemField" doit �tre une cha�ne, une date ou un entier';
  SIconFieldError         = '"IconField" doit �tre un entier';
  SMoveToModeError        = 'Mode de d�placement invalide pour un RADBTreeNode';
  SDataSetNotActive       = 'Ensemble de donn�es inactif';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Saisissez le type de la propri�t�';
  sRegAutoEditorTreeHint       = 'Propri�t� disponible';
  sRegAutoEditorListHint       = 'Propri�t� stock�es';
  sRegAutoEditorBtnAddPropHint = 'Ajouter/supprimer une propri�t�';
  sRegAutoEditorSort           = 'Trier';

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
  SScrollBarRange = 'Indice de barre de d�filement hors limite';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Annul�';

 { Menu Designer }
  SMDMenuDesigner       = 'Editeur &de menu';
  SMDInsertItem         = '&Ins�rer';
  SMDDeleteItem         = '&Supprimer';
  SMDCreateSubmenuItem  = 'Cr�er un &Sous-Menu';

  SCantGetShortCut      = 'Fichier cible du raccourci %s non disponible';

 { RALib 1.23 }
  SPropertyNotExists    = 'La propri�t� "%s" n''existe pas';
  SInvalidPropertyType  = 'Le type de la propri�t�"%s" est incorrect';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propri�t�s de l''�diteur';
  SHLEdPropDlg_tsEditor = 'G�n�ral';
  SHLEdPropDlg_tsColors = 'Couleurs';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Options pr�&d�finies';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Affectation par d�faut';
  SHLEdPropDlg_gbEditor = 'Options de l''�diteur';
  SHLEdPropDlg_cbAutoIndent = '&Mode auto-indentation';
  SHLEdPropDlg_cbSmartTab = '&Tabulation intelligente';
  SHLEdPropDlg_cbBackspaceUnindents = '&Retour arri�re d�sindent�';
  SHLEdPropDlg_cbGroupUndo = 'D�faire en &groupe';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Curseur apr�s la f&in de fichier';
  SHLEdPropDlg_cbUndoAfterSave = 'D�faire apr�s l''&enregistrement';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Co&nserver les espaces de fin';
  SHLEdPropDlg_cbDoubleClickLine = 'Double-cli&quer pour la ligne';
  SHLEdPropDlg_cbSytaxHighlighting = 'Mise en �&vidence de la syntaxe';
  SHLEdPropDlg_lblTabStops = 'Arr�ts tabulations :';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Co&uleurs pr�d�finies :';
  SHLEdPropDlg_lblElement = '&El�ment :';
  SHLEdPropDlg_lblColor = '&Couleur :';
  SHLEdPropDlg_gbTextAttributes = 'Attributs texte';
  SHLEdPropDlg_gbUseDefaultsFor = 'Valeurs par d�faut';
  SHLEdPropDlg_cbBold = '&Gras';
  SHLEdPropDlg_cbItalic = '&Italique';
  SHLEdPropDlg_cbUnderline = '&Soulign�';
  SHLEdPropDlg_cbDefForeground = 'Pour le &texte';
  SHLEdPropDlg_cbDefBackground = 'Pour le &fond';
  SHLEdPropDlg_OptionCantBeChanged = 'Cette option ne peut �tre modifi�e';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'propri�t� du RAHLEditor n''est pas d�finie';
  SHLEdPropDlg_RegAutoNotAssigned = 'propri�t� du RegAuto n''est pas d�finie';

  SNoReportProc = 'Proc�dure "RAI2RunReportPreview" non trouv�e';
  SNoReportProc2 = 'Proc�dure "RAI2RunReportPreview2" non trouv�e';

implementation

end.


