{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
French translation : KNIPPER John (knipjo@altavista.net)
************************************************************}

{$INCLUDE RA.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Annuler';

 { Dataset Editor }
  SDEDatasetDesigner = 'Editeur de &champs...';

  SDEAddItem          = '&Ajouter des champs...';
  SDEDeleteItem       = '&Supprimer';
  SDESelectAllItem    = 'Tous sé&lectionner';
  SDENewItem          = '&Nouveau champ...';

  SDEAddFieldsCaption = 'Ajouter des champs';
  SDEAvailableFields  = 'Champs disponible';

  SDENewFieldCaption    = 'Nouveau champ';
  SDEFieldProperties    = 'Propriétés du champs';
  SDEFieldNameLabel     = '&Nom:';
  SDEFieldTypeLabel     = '&Type:';
  SDEComponentNameLabel = 'C&omposant:';
  SDEFieldSizeLabel     = 'Ta&ille:';
  SDEFieldKind          = 'Type du champ';
  SDELookupGroup        = 'Lookup definition';
  SDEKeyFieldsLabel     = 'Champs Clés:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = 'Champ résultat:';
  SDEFieldKindItems     = '&Données'#13'&Calculé'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Le type du champs doit être spécifié';

  SDBGridColEditor    = 'Editeur de colonnes...';

 { Collection Editor }
  SCEEditCollection     = 'Edition de %s';
  SCEAdd                = '&Ajouter';
  SCEDelete             = 'Effacer';
  SCEMoveUp             = 'Monter';
  SCEMoveDown           = 'Descendre';
  SCESelectAllItem      = 'Tous sé&lectionner';

 { Picture Editor }
  SPELoad               = '&Charger...';
  SPESave               = '&Sauvegarder...';
  SPEClear              = '&Effacer';
  SPECopy               = 'C&opier';
  SPEPaste              = 'Coller';

 { Menu Designer }
  SMDMenuDesigner       = 'Editeur &de menu';
  SMDInsertItem         = '&Insérer';
  SMDDeleteItem         = '&Effacer';
  SMDCreateSubmenuItem  = 'Créer un &sous-menu';

implementation

end.
