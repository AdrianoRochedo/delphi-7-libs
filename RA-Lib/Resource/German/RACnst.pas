{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov

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
  SDeleteNode             = '%s löschen?';                                            //'Delete %s ?';
  SDeleteNode2            = '%s löschen (mit allen untergeordneten Elementen)?';      //'Delete %s (with all children) ?';
  SMasterFieldEmpty       = '"MasterField" Eigenschaft muss eingegeben werden';        //'"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" Eigenschaft muss eingegeben werden';        //'"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" Eigenschaft muss eingegeben werden';          //'"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" und "DetailField" müssen vom selben Typ sein'; //'"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" muss vom Typ Integer sein';                 //'"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" muss vom Typ Integer sein';                 //'"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" muss vom Typ String, Date oder Integer sein'; //'"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField"  muss vom Typ Integer sein';                  //'"IconField" must be integer type';
  SMoveToModeError        = 'ungültiger Verschiebe-Modus für RADBTreeNode';           //'Invalid move mode for RADBTreeNode';
  SDataSetNotActive       = 'Datensatz nicht aktiv';                                  //'DataSet not active';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Hier können Sie den Namen der Eigenschaft eingeben'; //'You can type property name here';
  sRegAutoEditorTreeHint       = 'Verfügbare Eigenschaften';                           //'Available properties';
  sRegAutoEditorListHint       = 'Gespeicherte Eigenschaften';                         //'Stored properties';
  sRegAutoEditorBtnAddPropHint = 'Eigenschaft hinzufügen/entfernen';                   //'Add/Remove property';
  sRegAutoEditorSort           = 'Sortieren';                                          //'Sort';

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
  SScrollBarRange = 'Scrollbar-Wert ausserhalb des gültigen Bereichs'; //'Scrollbar value out of bounds';
{$ENDIF}

 {RADlg}
  SCancel = 'Abbrechen'; //'Cancel';

 { Menu Designer }
  SMDMenuDesigner       = 'Menü-&Designer';       //'Menu &Designer';
  SMDInsertItem         = '&Einfügen';       //'&Insert';
  SMDDeleteItem         = '&Löschen';             //'&Delete';
  SMDCreateSubmenuItem  = '&Untermenü erstellen'; //'Create &SubMenu';

  SCantGetShortCut      = 'Das Ziel der Verknüpfung %s ist ungültig'; //'Target FileName for ShortCut %s not available';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Die Eigenschaft "%s" existiert nicht';          //'Property "%s" does not exists';
  SInvalidPropertyType  = 'Die Eigenschaft "%s" hat einen ungültigen Typ'; //'Property "%s" has invalid type';

implementation

end.
