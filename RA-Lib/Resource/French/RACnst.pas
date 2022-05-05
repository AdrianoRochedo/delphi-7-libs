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
  SMasterFieldEmpty       = 'La propriété "MasterField" doit être renseigné';
  SDetailFieldEmpty       = 'La propriété "DetailField" doit être renseigné';
  SItemFieldEmpty         = 'La propriété "ItemField" doit être renseigné';
  SMasterDetailFieldError = '"MasterField" et "DetailField" doivent être du même type';
  SMasterFieldError       = '"MasterField" doit être un entier';
  SDetailFieldError       = '"DetailField" doit être un entier';
  SItemFieldError         = '"ItemField" doit être une chaîne, une date ou un entier';
  SIconFieldError         = '"IconField" doit être un entier';
  SMoveToModeError        = 'Mode de déplacement invalide pour un RADBTreeNode';
  SDataSetNotActive       = 'Ensemble de données inactif';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Saisissez le type de la propriété';
  sRegAutoEditorTreeHint       = 'Propriété disponible';
  sRegAutoEditorListHint       = 'Propriété stockées';
  sRegAutoEditorBtnAddPropHint = 'Ajouter/supprimer une propriété';
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
  SScrollBarRange = 'Indice de barre de défilement hors limite';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Annulé';

 { Menu Designer }
  SMDMenuDesigner       = 'Editeur &de menu';
  SMDInsertItem         = '&Insérer';
  SMDDeleteItem         = '&Supprimer';
  SMDCreateSubmenuItem  = 'Créer un &Sous-Menu';

  SCantGetShortCut      = 'Fichier cible du raccourci %s non disponible';

 { RALib 1.23 }
  SPropertyNotExists    = 'La propriété "%s" n''existe pas';
  SInvalidPropertyType  = 'Le type de la propriété "%s" est incorrect';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propriétés de l''éditeur';
  SHLEdPropDlg_tsEditor = 'Général';
  SHLEdPropDlg_tsColors = 'Couleurs';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Options pré&définies';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Affectation par défaut';
  SHLEdPropDlg_gbEditor = 'Options de l''éditeur';
  SHLEdPropDlg_cbAutoIndent = '&Mode auto-indentation';
  SHLEdPropDlg_cbSmartTab = '&Tabulation intelligente';
  SHLEdPropDlg_cbBackspaceUnindents = '&Retour arrière désindenté';
  SHLEdPropDlg_cbGroupUndo = 'Défaire en &groupe';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Curseur après la f&in de fichier';
  SHLEdPropDlg_cbUndoAfterSave = 'Défaire après l''&enregistrement';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Co&nserver les espaces de fin';
  SHLEdPropDlg_cbDoubleClickLine = 'Double-cli&quer pour la ligne';
  SHLEdPropDlg_cbSytaxHighlighting = 'Mise en é&vidence de la syntaxe';
  SHLEdPropDlg_lblTabStops = 'Arrêts tabulations :';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Co&uleurs prédéfinies :';
  SHLEdPropDlg_lblElement = '&Elément :';
  SHLEdPropDlg_lblColor = '&Couleur :';
  SHLEdPropDlg_gbTextAttributes = 'Attributs texte';
  SHLEdPropDlg_gbUseDefaultsFor = 'Valeurs par défaut';
  SHLEdPropDlg_cbBold = '&Gras';
  SHLEdPropDlg_cbItalic = '&Italique';
  SHLEdPropDlg_cbUnderline = '&Souligné';
  SHLEdPropDlg_cbDefForeground = 'Pour le &texte';
  SHLEdPropDlg_cbDefBackground = 'Pour le &fond';
  SHLEdPropDlg_OptionCantBeChanged = 'Cette option ne peut être modifiée';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'propriété du RAHLEditor n''est pas définie';
  SHLEdPropDlg_RegAutoNotAssigned = 'propriété du RegAuto n''est pas définie';

  SNoReportProc = 'Procédure "RAI2RunReportPreview" non trouvée';
  SNoReportProc2 = 'Procédure "RAI2RunReportPreview2" non trouvée';

implementation

end.


