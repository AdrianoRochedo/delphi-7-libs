{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       description : Language specific constant for English

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
  SDeleteNode2            = 'Borrar %s (con todos sus hijos) ?';
  SMasterFieldEmpty       = 'Propiedad "MasterField" debe ser ingresada';
  SDetailFieldEmpty       = 'Propiedad "DetailField" debe ser ingresada';
  SItemFieldEmpty         = 'Propiedad "ItemField" debe ser ingresada';
  SMasterDetailFieldError = 'Propiedad "MasterField" y "DetailField" deben ser del mismo tipo';
  SMasterFieldError       = 'Propiedad "MasterField" debe ser tipo entero';
  SDetailFieldError       = 'Propiedad "DetailField" debe ser tipo entero';
  SItemFieldError         = 'Propiedad "ItemField" debe ser tipo string, date o entero';
  SIconFieldError         = 'Propiedad "IconField" debe ser tipo entero';
  SMoveToModeError        = 'Movida invalida para RADBTreeNode';
  SDataSetNotActive       = 'DataSet inactivo';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Usted puede tipear el nombre de la propiedad aqui';
  sRegAutoEditorTreeHint       = 'Propiedades disponibles';
  sRegAutoEditorListHint       = 'Propiedades almacenadas';
  sRegAutoEditorBtnAddPropHint = 'Agregar/Remover propiedad';
  sRegAutoEditorSort           = 'Ordenar';

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
  SScrollBarRange = 'Valor de Scrollbar fuera de limite';
{$ENDIF}

 {RADlg}
  SOk = 'OK';
  SCancel = 'Cancelar';

 { Menu Designer }
  SMDMenuDesigner       = '&Designador de Menu';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Borrar';
  SMDCreateSubmenuItem  = 'Crear &SubMenu';

  SCantGetShortCut      = 'Nombre de archivo para acceso %s no disponible';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Propiedad "%s" no existe';
  SInvalidPropertyType  = 'Propiedad "%s" tiene tipo invalido';

 { RALib 1.55 }

 {RAHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propiedades del editor';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colores';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Configuracion del editor';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Mapeo de teclas por defecto';
  SHLEdPropDlg_gbEditor = 'Opciones del editor:';
  SHLEdPropDlg_cbAutoIndent = 'Modo &Auto indent';
  SHLEdPropDlg_cbSmartTab = 'Tab &Habil';
  SHLEdPropDlg_cbBackspaceUnindents = 'Des-Indent de &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Deshacer grupo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor mas alla de &EOF';
  SHLEdPropDlg_cbUndoAfterSave = 'D&eshacer despues de guardar';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Mantener rastros de blancos';
  SHLEdPropDlg_cbDoubleClickLine = '&Doble click la linea';
  SHLEdPropDlg_cbSytaxHighlighting = 'Usar resaltado de &sintaxis';
  SHLEdPropDlg_lblTabStops = 'Para con &Tab:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Configuracion de color para';
  SHLEdPropDlg_lblElement = '&Elemento:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Atributos de texto:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Usar por defecto para:';
  SHLEdPropDlg_cbBold = '&Negrita';
  SHLEdPropDlg_cbItalic = '&Italica';
  SHLEdPropDlg_cbUnderline = '&Subrayado';
  SHLEdPropDlg_cbDefForeground = '&Primer plano';
  SHLEdPropDlg_cbDefBackground = '&Fondo';
  SHLEdPropDlg_OptionCantBeChanged = 'Esta opcion no puede ser cambiada.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Propiedad RAHLEditor no asignada';
  SHLEdPropDlg_RegAutoNotAssigned = 'Propiedad RegAuto no asignada';

  SNoReportProc = 'Procedure "RAI2RunReportPreview" not found';
  SNoReportProc2 = 'Procedure "RAI2RunReportPreview2" not found';

implementation

end.
