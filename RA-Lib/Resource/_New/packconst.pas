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
  SCancel           = 'Cancel';

 { Dataset Editor }
  SDEDatasetDesigner = '&Fields editor...';

  SDEAddItem          = '&Add fields...';
  SDEDeleteItem       = '&Delete';
  SDESelectAllItem    = 'Se&lect All';
  SDENewItem          = '&New Field...';

  SDEAddFieldsCaption = 'Add fields';
  SDEAvailableFields  = 'Available fields';

  SDENewFieldCaption    = 'New field';
  SDEFieldProperties    = 'Field properties';
  SDEFieldNameLabel     = '&Name:';
  SDEFieldTypeLabel     = '&Type:';
  SDEComponentNameLabel = 'C&omponent:';
  SDEFieldSizeLabel     = '&Size:';
  SDEFieldKind          = 'Field type';
  SDELookupGroup        = 'Lookup definition';
  SDEKeyFieldsLabel     = '&Key Fields:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = '&Result Field:';
  SDEFieldKindItems     = '&Data'#13'&Calculated'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Field type must be specified';

  SDBGridColEditor    = '&Columns Editor...';

 { Collection Editor }
  SCEEditCollection     = 'Editing %s';
  SCEAdd                = '&Add';
  SCEDelete             = '&Delete';
  SCEMoveUp             = 'Move &Up';
  SCEMoveDown           = 'Move Dow&n';
  SCESelectAllItem      = 'Se&lect All';

 { Picture Editor }
  SPELoad               = '&Load...';
  SPESave               = '&Save...';
  SPEClear              = '&Clear';
  SPECopy               = 'C&opy';
  SPEPaste              = '&Paste';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &Submenu';

implementation

end.
