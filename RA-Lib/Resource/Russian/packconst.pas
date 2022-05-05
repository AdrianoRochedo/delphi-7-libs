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
  SCancel           = 'Отмена';

 { Dataset Editor }
  SDEDatasetDesigner = '&Редактор полей...';

  SDEAddItem          = '&Добавить поля...';
  SDEDeleteItem       = '&Удалить';
  SDESelectAllItem    = '&Выделить все';
  SDENewItem          = '&Новое поле...';

  SDEAddFieldsCaption = 'Добавление полей';
  SDEAvailableFields  = 'Доступные поля';

  SDENewFieldCaption    = 'Новое поле';
  SDEFieldProperties    = ' Свойства поля ';
  SDEFieldNameLabel     = '&Имя:';
  SDEFieldTypeLabel     = '&Тип:';
  SDEComponentNameLabel = '&Компонент:';
  SDEFieldSizeLabel     = '&Размер:';
  SDEFieldKind          = ' Тип поля ';
  SDELookupGroup        = ' Связанное поле ';
  SDEKeyFieldsLabel     = '&Key Fields:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = '&Result Field:';
  SDEFieldKindItems     = '&Данные'#13'&Вычисляемое'#13'&Связанное';
  SDEFieldTypeMustBeSpecified = 'Тип поля должен быть указан';

  SDBGridColEditor    = '&Редактор колонок...';

 { Collection Editor }
  SCEEditCollection     = 'Редактируем %s';
  SCEAdd                = '&Добавить';
  SCEDelete             = '&Удалить';
  SCEMoveUp             = 'В&верх';
  SCEMoveDown           = 'В&низ';
  SCESelectAllItem      = 'Выделить вс&е';

 { Picture Editor }
  SPELoad               = '&Загрузить...';
  SPESave               = '&Сохранить...';
  SPEClear              = '&Очистить';
  SPECopy               = '&Копировать';
  SPEPaste              = '&Вставить';

 { Menu Designer }
  SMDMenuDesigner       = '&Редактор меню';
  SMDInsertItem         = '&Вставить';
  SMDDeleteItem         = '&Удалить';
  SMDCreateSubmenuItem  = 'Создать &подменю';

implementation

end.
