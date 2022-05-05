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
  SOK               = 'Aceptar';
  SCancel           = 'Cancelar';

 { Dataset Editor }
  SDEDatasetDesigner = 'Editor de &Campos...';

  SDEAddItem          = '&Agregar Campos...';
  SDEDeleteItem       = '&Eliminar';
  SDESelectAllItem    = 'Se&leccionar Tpdps';
  SDENewItem          = '&Nuevo Campo...';

  SDEAddFieldsCaption = 'Agregar campos';
  SDEAvailableFields  = 'Campos disponibles';

  SDENewFieldCaption    = 'Nuevo campo';
  SDEFieldProperties    = 'Propiedades del campo';
  SDEFieldNameLabel     = '&Nombre:';
  SDEFieldTypeLabel     = '&Tipo:';
  SDEComponentNameLabel = 'C&omponente:';
  SDEFieldSizeLabel     = '&Tamaño:';
  SDEFieldKind          = 'Tipo de Campo';
  SDELookupGroup        = 'Definición Lookup';
  SDEKeyFieldsLabel     = 'Campos &Clave:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Claves Look&up:';
  SDEResultFieldLabel   = 'Campo &Resultante:';
  SDEFieldKindItems     = '&Dato'#13'&Calculado'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'El tipo de campo debe ser especificado';

  SDBGridColEditor    = 'Editor de &Columnas...';

 { Collection Editor }
  SCEEditCollection     = 'Editando %s';
  SCEAdd                = '&Agregar';
  SCEDelete             = '&Eliminar';
  SCEMoveUp             = 'Mover &Arriba';
  SCEMoveDown           = 'Mover Aba&jo';
  SCESelectAllItem      = 'Se&leccionar Todo';

 { Picture Editor }
  SPELoad               = '&Cargar...';
  SPESave               = '&Salvar...';
  SPEClear              = '&Limpiar';
  SPECopy               = 'C&opiar';
  SPEPaste              = '&Pegar';

 { Menu Designer }
  SMDMenuDesigner       = '&Diseñador de Menú';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Eliminar';
  SMDCreateSubmenuItem  = 'Crear &Submenú';

implementation

end.
