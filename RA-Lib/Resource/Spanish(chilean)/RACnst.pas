{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Language specific constant for Spanish(chilean)

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = 'Borrar %s ?';
  SDeleteNode2            = 'Borrar %s (con todos los hijos) ?';
  SMasterFieldEmpty       = 'Propiedad "MasterField" debe ser llenada';
  SDetailFieldEmpty       = 'Propiedad "DetailField" debe ser llenada';
  SItemFieldEmpty         = 'Propiedad "ItemField" debe ser llenada';
  SMasterDetailFieldError = '"MasterField" y "DetailField" deben ser del mismo tipo';
  SMasterFieldError       = '"MasterField" debe ser de tipo integer';
  SDetailFieldError       = '"DetailField" debe ser de tipo integer';
  SItemFieldError         = '"ItemField" debe ser tipo string, date o integer';
  SIconFieldError         = '"IconField" debe ser de tipo integer';
  SMoveToModeError        = 'Modo de mover inválido para RADBTreeNode';
  SDataSetNotActive       = 'DataSet no activa';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Puede tipear el nombre de la propiedad aqui';
  sRegAutoEditorTreeHint       = 'Propiedades disponibles';
  sRegAutoEditorListHint       = 'Propiedades almacenadas';
  sRegAutoEditorBtnAddPropHint = 'Agregar/Eliminar propiedad';
  sRegAutoEditorSort           = 'Orden';

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
  SScrollBarRange = 'Balor de la barra de scroll fuera de límite';
{$ENDIF}

 {RADlg}
  SOk = 'Aceptar';
  SCancel = 'Cancelar';

 { Menu Designer }
  SMDMenuDesigner       = '&Diseñador de Menús';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Eliminar';
  SMDCreateSubmenuItem  = 'Crear &Sub Menú';

  SCantGetShortCut      = 'Nombre de archivo destino para ShortCut %s no disponible';


 { RALib 1.23 }
  SPropertyNotExists    = 'Propiedad "%s" no existe';
  SInvalidPropertyType  = 'Propiedad "%s" tiene un tipo inválido';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propiedades del editor';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colores';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Seteos rápidos del Editor';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Mapeo de teclas por defecto';
  SHLEdPropDlg_gbEditor = 'Opciones del Editor:';
  SHLEdPropDlg_cbAutoIndent = 'Modo de &Autoindentación';
  SHLEdPropDlg_cbSmartTab = 'Tabla I&nteligente';
  SHLEdPropDlg_cbBackspaceUnindents = 'Tecla Backspace &desindenta';
  SHLEdPropDlg_cbGroupUndo = 'Deshacer &Grupo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor más allá de &Fin de Archivo (EOF)';
  SHLEdPropDlg_cbUndoAfterSave = '&Deshacer después de sal&var';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Mantener blancos de cola';
  SHLEdPropDlg_cbDoubleClickLine = 'Linea &doble click';
  SHLEdPropDlg_cbSytaxHighlighting = 'Usar &syntaxis destacada';
  SHLEdPropDlg_lblTabStops = 'paradas de &Tabs:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Seteo rápido de color para';
  SHLEdPropDlg_lblElement = '&Elemento:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Atributos de texto:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Use valores por defecto para:';
  SHLEdPropDlg_cbBold = '&Resaltado';
  SHLEdPropDlg_cbItalic = '&Cursiva';
  SHLEdPropDlg_cbUnderline = '&Subrayado';
  SHLEdPropDlg_cbDefForeground = 'A&delante';
  SHLEdPropDlg_cbDefBackground = '&Atrás';
  SHLEdPropDlg_OptionCantBeChanged = 'Esta opción no puede ser cambiada. Lo siento.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Propiedad de RAHLEditor no está asignada';
  SHLEdPropDlg_RegAutoNotAssigned = 'Propiedad RegAuto no está asignada';

  SNoReportProc = 'Procedimiento "RAI2RunReportPreview" no encontrado';
  SNoReportProc2 = 'Procedimiento "RAI2RunReportPreview2" no encontrado';

implementation

end.
