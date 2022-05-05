{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Abbrechen';                           //'Cancel';

 { Dataset Editor }
  SDEDatasetDesigner = 'Feld-Edi&tor...';                    //'&Fields editor...';

  SDEAddItem          = '&Felder hinzufügen...';             //'&Add fields...';
  SDEDeleteItem       = '&Löschen';                          //'&Delete';
  SDESelectAllItem    = 'Alles &markieren';                  //'Se&lect All';
  SDENewItem          = '&Neues Feld...';                    //'&New Field...';

  SDEAddFieldsCaption = 'Felder hinzufügen';                 //'Add fields';
  SDEAvailableFields  = 'Verfügbare Felder';                 //'Available fields';

  SDENewFieldCaption    = 'Neues Feld';                      //'New field';
  SDEFieldProperties    = 'Feldeigenschaften';               //'Field properties';
  SDEFieldNameLabel     = '&Name:';
  SDEFieldTypeLabel     = '&Typ:';                           //'&Type:';
  SDEComponentNameLabel = 'K&omponente:';                    //'C&omponent:';
  SDEFieldSizeLabel     = '&Grösse:';                         //'&Size:';
  SDEFieldKind          = 'Feldtyp';                         //'Field type';
  SDELookupGroup        = 'Nachschlage-Definition';          //'Lookup definition';
  SDEKeyFieldsLabel     = '&Schlüsselfelder:';               //'&Key Fields:';
  SDEDatasetLabel       = 'D&atenmenge:';                    //'D&ataset:';
  SDELookupKeysLabel    = 'S&chlüssel:';                     //'Look&up Keys:';
  SDEResultFieldLabel   = 'E&rgebnisfeld:';                  //'&Result Field:';
  SDEFieldKindItems     = '&Daten'#13'&Berechnet'#13'&Nachschlagen'; //'&Data'#13'&Calculated'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Typ muss angegeben werden'; //'Field type must be specified';

  SDBGridColEditor    = 'Spa&lteneditor...';                 //'&Columns Editor...';

 { Collection Editor }
  SCEEditCollection     = '%s wird bearbeitet';              //'Editing %s';
  SCEAdd                = 'Hin&zufügen';                     //'&Add';
  SCEDelete             = '&Löschen';                        //'&Delete';
  SCEMoveUp             = 'Nach &oben';                      //'Move &Up';
  SCEMoveDown           = 'Na&ch unten';                     //'Move Dow&n';
  SCESelectAllItem      = 'Alles &markieren';                //'Se&lect All';

 { Picture Editor }
  SPELoad               = '&Laden...';                       //'&Load...';
  SPESave               = '&Speichern...';                   //'&Save...';
  SPEClear              = 'Ent&fernen';                      //'&Clear';
  SPECopy               = '&Kopieren';                       //'C&opy';
  SPEPaste              = '&Einfügen';                       //'&Paste';

 { Menu Designer }
  SMDMenuDesigner       = 'Menü-&Designer';                  //'Menu &Designer';
  SMDInsertItem         = '&Einfügen';                       //'&Insert';
  SMDDeleteItem         = '&Löschen';                        //'&Delete';
  SMDCreateSubmenuItem  = '&Untermenü erstellen';            //'Create &Submenu';

implementation

end.
