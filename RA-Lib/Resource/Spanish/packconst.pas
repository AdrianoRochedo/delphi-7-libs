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
  SCancel           = 'Cancelar';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor de campos...';

  SDEAddItem          = '&Agregar campos...';
  SDEDeleteItem       = '&Borrar';
  SDESelectAllItem    = '&Seleccionar todo';
  SDENewItem          = '&Nuevo campo...';

  SDEAddFieldsCaption = 'Agregar campos';
  SDEAvailableFields  = 'Campos disponibles';

  SDENewFieldCaption    = 'Nuevo campo';
  SDEFieldProperties    = 'Propiedades del campo';
  SDEFieldNameLabel     = '&Nombre:';
  SDEFieldTypeLabel     = '&Tipo:';
  SDEComponentNameLabel = '&Componente:';
  SDEFieldSizeLabel     = 'Ta&maño:';
  SDEFieldKind          = 'Tipo del campo';
  SDELookupGroup        = 'Definicion de busqueda';
  SDEKeyFieldsLabel     = 'C&ampos clave:';
  SDEDatasetLabel       = 'Da&taset:';
  SDELookupKeysLabel    = 'Claves de &busqueda:';
  SDEResultFieldLabel   = 'Campo de &resultado:';
  SDEFieldKindItems     = '&Datos'#13'&Calculatdo'#13'&Busqueda';
  SDEFieldTypeMustBeSpecified = 'El tipo de campo debe ser especificado';

  SDBGridColEditor    = '&Editor de columnas...';

 { Collection Editor }
  SCEEditCollection     = 'Editando %s';
  SCEAdd                = '&Agregar';
  SCEDelete             = '&Borrar';
  SCEMoveUp             = 'Mover arr&iba';
  SCEMoveDown           = 'Mover aba&jo';
  SCESelectAllItem      = '&Seleccionar todo';

 { Picture Editor }
  SPELoad               = '&Cargar...';
  SPESave               = '&Guardar...';
  SPEClear              = '&Limpiar';
  SPECopy               = 'C&opiar';
  SPEPaste              = '&Pegar';

 { Menu Designer }
  SMDMenuDesigner       = '&Diseñador de menu';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Borrar';
  SMDCreateSubmenuItem  = 'Crear &sumbenu';

implementation

end.
