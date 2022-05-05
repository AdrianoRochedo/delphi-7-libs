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
  SDEDatasetDesigner = '&Editor pol�...';

  SDEAddItem          = '&P�idat pole...';
  SDEDeleteItem       = '&Smazat';
  SDESelectAllItem    = 'Vybrat &v�e';
  SDENewItem          = '&Nov� pole...';

  SDEAddFieldsCaption = 'P�idat pole';
  SDEAvailableFields  = 'Dostupn� pole';

  SDENewFieldCaption    = 'Nov� pole';
  SDEFieldProperties    = 'Vlastnosti pole';
  SDEFieldNameLabel     = '&Jm�no:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = '&Komponenta:';
  SDEFieldSizeLabel     = '&Velikost:';
  SDEFieldKind          = 'Typ pole';
  SDELookupGroup        = 'Definice vyhled�v�n�';
  SDEKeyFieldsLabel     = 'K&l��ov� pole:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Kl��e pro v&yhled�v�n�:';
  SDEResultFieldLabel   = '&V�sledn� pole:';
  SDEFieldKindItems     = '&Data'#13'&Po��tan�'#13'&Vyhled�van�';
  SDEFieldTypeMustBeSpecified = 'Mus� b�t zad�n typ pole';

  SDBGridColEditor    = 'Editor &sloupc�...';

 { Collection Editor }
  SCEEditCollection     = 'Editace %s';
  SCEAdd                = '&P�idat';
  SCEDelete             = '&Smazat';
  SCEMoveUp             = 'P�esunout &nahoru';
  SCEMoveDown           = 'P�esunout &dol�';
  SCESelectAllItem      = 'Vy&brat v�e';

 { Picture Editor }
  SPELoad               = '&Na��st...';
  SPESave               = '&Ulo�it...';
  SPEClear              = '&Vy�istit';
  SPECopy               = '&Kop�rovat';
  SPEPaste              = 'V&lo�it';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh�� &menu';
  SMDInsertItem         = '&Vlo�it';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvo�it &podmenu';

implementation

end.
