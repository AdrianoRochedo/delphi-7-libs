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
  SOK               = 'Ok';
  SCancel           = 'Storno';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor polí...';

  SDEAddItem          = '&Pøidat pole...';
  SDEDeleteItem       = '&Smazat';
  SDESelectAllItem    = 'Vybrat &vše';
  SDENewItem          = '&Nové pole...';

  SDEAddFieldsCaption = 'Pøidat pole';
  SDEAvailableFields  = 'Dostupná pole';

  SDENewFieldCaption    = 'Nové pole';
  SDEFieldProperties    = 'Vlastnosti pole';
  SDEFieldNameLabel     = '&Jméno:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = '&Komponenta:';
  SDEFieldSizeLabel     = '&Velikost:';
  SDEFieldKind          = 'Typ pole';
  SDELookupGroup        = 'Definice vyhledávání';
  SDEKeyFieldsLabel     = 'K&líèová pole:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Klíèe pro v&yhledávání:';
  SDEResultFieldLabel   = '&Výsledné pole:';
  SDEFieldKindItems     = '&Data'#13'&Poèítané'#13'&Vyhledávané';
  SDEFieldTypeMustBeSpecified = 'Musí být zadán typ pole';

  SDBGridColEditor    = 'Editor &sloupcù...';

 { Collection Editor }
  SCEEditCollection     = 'Editace %s';
  SCEAdd                = '&Pøidat';
  SCEDelete             = '&Smazat';
  SCEMoveUp             = 'Pøesunout &nahoru';
  SCEMoveDown           = 'Pøesunout &dolù';
  SCESelectAllItem      = 'Vy&brat vše';

 { Picture Editor }
  SPELoad               = '&Naèíst...';
  SPESave               = '&Uložit...';
  SPEClear              = '&Vyèistit';
  SPECopy               = '&Kopírovat';
  SPEPaste              = 'V&ložit';

 { Menu Designer }
  SMDMenuDesigner       = 'Návrháø &menu';
  SMDInsertItem         = '&Vložit';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvoøit &podmenu';

implementation

end.
