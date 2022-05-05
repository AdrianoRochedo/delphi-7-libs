{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Language specific constants for Russian

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = 'Удалить %s ?';
  SDeleteNode2            = 'Удалить %s (вместе со всем содержимым) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = 'Неверный режим перемещения RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

   {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Имя свойства можно ввести прямо здесь';
  sRegAutoEditorTreeHint       = 'Доступные свойства';
  sRegAutoEditorListHint       = 'Список сохраняемых свойств';
  sRegAutoEditorBtnAddPropHint = 'Добавить/Удалить свойство';
  sRegAutoEditorSort           = 'Отсортировать';

 {RAEditor}
  RAEditorCompletionChars = #8+'_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';

{IParser and RAI2Parser}
 {$IFDEF RA_D}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF RA_D}
 {$IFDEF RA_B}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF RA_B}

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я', '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я', '_'];
{$ENDIF RAINTER}

{$IFDEF RA_D2}
  SScrollBarRange = 'значение Scrollbar вышло за допустимые пределы';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Отмена';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';

  SCantGetShortCut      = 'Target FileName for ShortCut %s not available';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Property "%s" does not exists';
  SInvalidPropertyType  = 'Property "%s" has invalid type';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Свойства: Редактор';
  SHLEdPropDlg_tsEditor = 'Редактор';
  SHLEdPropDlg_tsColors = 'Цвета';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Набор горячих клавиш';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Стандартный набор';
  SHLEdPropDlg_gbEditor = 'Параметры редактора:';
  SHLEdPropDlg_cbAutoIndent = '&Автоотступ';
  SHLEdPropDlg_cbSmartTab = '&Умный таб';
  SHLEdPropDlg_cbBackspaceUnindents = 'Забой &сдвигает назад';
  SHLEdPropDlg_cbGroupUndo = '&Групповая отмена';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Курсор за конец &файла';
  SHLEdPropDlg_cbUndoAfterSave = 'Отмена &после сохранения';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Сохранять &завершающие пробелы';
  SHLEdPropDlg_cbDoubleClickLine = '&Двойной клик выделяет строку';
  SHLEdPropDlg_cbSytaxHighlighting = 'Выделять &синтаксис';
  SHLEdPropDlg_lblTabStops = '&Табулостопы:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Цвета для';
  SHLEdPropDlg_lblElement = '&Элемент:';
  SHLEdPropDlg_lblColor = '&Цвет:';
  SHLEdPropDlg_gbTextAttributes = 'Атрибуты текста:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Стандартные:';
  SHLEdPropDlg_cbBold = '&Жирный';
  SHLEdPropDlg_cbItalic = '&Наклонный';
  SHLEdPropDlg_cbUnderline = '&Подчеркнутый';
  SHLEdPropDlg_cbDefForeground = '&Буквы';
  SHLEdPropDlg_cbDefBackground = '&Фон';
  SHLEdPropDlg_OptionCantBeChanged = 'Этот параметр менять нельзя. Извините.';
  SHLEdPropDlg_RAHLEditorNotAssigned = 'Свойство RAHLEditor не назначено';
  SHLEdPropDlg_RegAutoNotAssigned = 'Свойство RegAuto не назначено';

  SNoReportProc = 'Не найдена процедура "RAI2RunReportPreview"';
  SNoReportProc2 = 'Не найдена процедура "RAI2RunReportPreview2"';
  
implementation

end.








