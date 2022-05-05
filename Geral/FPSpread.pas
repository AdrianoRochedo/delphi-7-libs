unit FPSpread;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.130  $
// File generated on 8/3/2005 15:42:49 from Type Library described below.

// ************************************************************************  //
// Type Lib: G:\Temp\ssTrial\Bin\SPR32X60.ocx (1)
// LIBID: {FDAC2480-F4ED-4632-AA78-DCA210A74E49}
// LCID: 0
// Helpfile: G:\Temp\ssTrial\Bin\spread60.chm
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Hint: Parameter 'Var' of _DSpreadSheet.GetDataFillData changed to 'Var_'
//   Hint: Parameter 'Var' of _DSpreadSheet.CFSetResult changed to 'Var_'
//   Hint: Parameter 'Type' of _DSpreadSheet.CFGetParamInfo changed to 'Type_'
//   Hint: Parameter 'Var' of _DSpreadSheet.GetText changed to 'Var_'
//   Hint: Parameter 'Var' of _DSpreadSheet.SetDataFillData changed to 'Var_'
//   Hint: Parameter 'Type' of _DSpreadSheet.ScriptCFGetParamInfo changed to 'Type_'
//   Hint: Parameter 'Var' of _DSpreadSheet.SetText changed to 'Var_'
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

uses ActiveX, Classes, Graphics, OleCtrls, OleServer, StdVCL, Variants, 
Windows;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  FPSpreadMajorVersion = 6;
  FPSpreadMinorVersion = 0;

  LIBID_FPSpread: TGUID = '{FDAC2480-F4ED-4632-AA78-DCA210A74E49}';

  IID__DummyfpListCursor: TGUID = '{123AA700-D188-11CD-AD48-00AA003C9CB6}';
  IID__DSpreadCursor: TGUID = '{9F6AA700-D188-11CD-AD48-00AA003C9CB6}';
  IID_IfpDataObjectFiles: TGUID = '{69310C25-4993-11D1-8905-0020AF131A57}';
  CLASS_fpDataObjectFiles: TGUID = '{69310C26-4993-11D1-8905-0020AF131A57}';
  IID_IfpDataObject: TGUID = '{69310C27-4993-11D1-8905-0020AF131A57}';
  CLASS_fpDataObject: TGUID = '{69310C28-4993-11D1-8905-0020AF131A57}';
  DIID__DSpreadSheet: TGUID = '{41F841C3-AE16-11D5-8817-0050DA6EF5E5}';
  DIID__DSpreadEvents: TGUID = '{41F841C5-AE16-11D5-8817-0050DA6EF5E5}';
  CLASS_vaSpread: TGUID = '{41F841C1-AE16-11D5-8817-0050DA6EF5E5}';
  DIID__DSpreadPreview: TGUID = '{41F841C9-AE16-11D5-8817-0050DA6EF5E5}';
  DIID__DSpreadPreviewEvents: TGUID = '{41F841CB-AE16-11D5-8817-0050DA6EF5E5}';
  CLASS_vaSpreadPreview: TGUID = '{41F841C7-AE16-11D5-8817-0050DA6EF5E5}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CoordConstants
type
  CoordConstants = TOleEnum;
const
  SpreadHeader = $FFFFFC18;

// Constants for enum BorderStyleConstants
type
  BorderStyleConstants = TOleEnum;
const
  BorderStyleNone = $00000000;
  BorderStyleFixedSingle = $00000001;

// Constants for enum HeaderDisplayConstants
type
  HeaderDisplayConstants = TOleEnum;
const
  DispBlank = $00000000;
  DispNumbers = $00000001;
  DispLetters = $00000002;

// Constants for enum EditEnterActionConstants
type
  EditEnterActionConstants = TOleEnum;
const
  EditEnterActionNone = $00000000;
  EditEnterActionUp = $00000001;
  EditEnterActionDown = $00000002;
  EditEnterActionLeft = $00000003;
  EditEnterActionRight = $00000004;
  EditEnterActionNext = $00000005;
  EditEnterActionPrevious = $00000006;
  EditEnterActionSame = $00000007;
  EditEnterActionNextRow = $00000008;

// Constants for enum OperationModeConstants
type
  OperationModeConstants = TOleEnum;
const
  OperationModeNormal = $00000000;
  OperationModeRead = $00000001;
  OperationModeRow = $00000002;
  OperationModeSingle = $00000003;
  OperationModeMulti = $00000004;
  OperationModeExtended = $00000005;

// Constants for enum PositionConstants
type
  PositionConstants = TOleEnum;
const
  PositionUpperLeft = $00000000;
  PositionUpperCenter = $00000001;
  PositionUpperRight = $00000002;
  PositionCenterLeft = $00000003;
  PositionCenter = $00000004;
  PositionCenterRight = $00000005;
  PositionBottomLeft = $00000006;
  PositionBottomCenter = $00000007;
  PositionBottomRight = $00000008;

// Constants for enum ScrollBarsConstants
type
  ScrollBarsConstants = TOleEnum;
const
  ScrollBarsNone = $00000000;
  ScrollBarsHorizontal = $00000001;
  ScrollBarsVertical = $00000002;
  ScrollBarsBoth = $00000003;

// Constants for enum UnitTypeConstants
type
  UnitTypeConstants = TOleEnum;
const
  UnitTypeNormal = $00000000;
  UnitTypeVGABase = $00000001;
  UnitTypeTwips = $00000002;

// Constants for enum BackColorStyleConstants
type
  BackColorStyleConstants = TOleEnum;
const
  BackColorStyleOverGrid = $00000000;
  BackColorStyleUnderGrid = $00000001;
  BackColorStyleOverHorzGridOnly = $00000002;
  BackColorStyleOverVertGridOnly = $00000003;

// Constants for enum DAutoSizeColsConstants
type
  DAutoSizeColsConstants = TOleEnum;
const
  DAutoSizeColsNone = $00000000;
  DAutoSizeColsMax = $00000001;
  DAutoSizeColsBest = $00000002;

// Constants for enum ActionConstants
type
  ActionConstants = TOleEnum;
const
  ActionActiveCell = $00000000;
  ActionGotoCell = $00000001;
  ActionSelectBlock = $00000002;
  ActionClear = $00000003;
  ActionDeleteCol = $00000004;
  ActionDeleteRow = $00000005;
  ActionInsertCol = $00000006;
  ActionInsertRow = $00000007;
  ActionReCalc = $0000000B;
  ActionClearText = $0000000C;
  ActionPrint = $0000000D;
  ActionDeselectBlock = $0000000E;
  ActionDSave = $0000000F;
  ActionSetCellBorder = $00000010;
  ActionAddMultiSelBlock = $00000011;
  ActionGetMultiSelection = $00000012;
  ActionCopyRange = $00000013;
  ActionMoveRange = $00000014;
  ActionSwapRange = $00000015;
  ActionClipboardCopy = $00000016;
  ActionClipboardCut = $00000017;
  ActionClipboardPaste = $00000018;
  ActionSort = $00000019;
  ActionComboClear = $0000001A;
  ActionComboRemove = $0000001B;
  ActionReset = $0000001C;
  ActionSelModeClear = $0000001D;
  ActionVModeRefresh = $0000001E;
  ActionSmartPrint = $00000020;

// Constants for enum CellBorderStyleConstants
type
  CellBorderStyleConstants = TOleEnum;
const
  CellBorderStyleDefault = $00000000;
  CellBorderStyleSolid = $00000001;
  CellBorderStyleDash = $00000002;
  CellBorderStyleDot = $00000003;
  CellBorderStyleDashDot = $00000004;
  CellBorderStyleDashDotDot = $00000005;
  CellBorderStyleBlank = $00000006;
  CellBorderStyleFineSolid = $0000000B;
  CellBorderStyleFineDash = $0000000C;
  CellBorderStyleFineDot = $0000000D;
  CellBorderStyleFineDashDot = $0000000E;
  CellBorderStyleFineDashDotDot = $0000000F;

// Constants for enum CellTypeConstants
type
  CellTypeConstants = TOleEnum;
const
  CellTypeDate = $00000000;
  CellTypeEdit = $00000001;
  CellTypeFloat = $00000002;
  CellTypeInteger = $00000003;
  CellTypePic = $00000004;
  CellTypeStaticText = $00000005;
  CellTypeTime = $00000006;
  CellTypeButton = $00000007;
  CellTypeComboBox = $00000008;
  CellTypePicture = $00000009;
  CellTypeCheckBox = $0000000A;
  CellTypeOwnerDrawn = $0000000B;
  CellTypeCurrency = $0000000C;
  CellTypeNumber = $0000000D;
  CellTypePercent = $0000000E;

// Constants for enum CursorStyleConstants
type
  CursorStyleConstants = TOleEnum;
const
  CursorStyleUserDefined = $00000000;
  CursorStyleDefault = $00000001;
  CursorStyleArrow = $00000002;
  CursorStyleDefColResize = $00000003;
  CursorStyleDefRowResize = $00000004;

// Constants for enum CursorTypeConstants
type
  CursorTypeConstants = TOleEnum;
const
  CursorTypeDefault = $00000000;
  CursorTypeColResize = $00000001;
  CursorTypeRowResize = $00000002;
  CursorTypeButton = $00000003;
  CursorTypeGrayArea = $00000004;
  CursorTypeLockedCell = $00000005;
  CursorTypeColHeader = $00000006;
  CursorTypeRowHeader = $00000007;
  CursorTypeDragDropArea = $00000008;
  CursorTypeDragDrop = $00000009;

// Constants for enum PrintOrientationConstants
type
  PrintOrientationConstants = TOleEnum;
const
  PrintOrientationDefault = $00000000;
  PrintOrientationPortrait = $00000001;
  PrintOrientationLandscape = $00000002;

// Constants for enum PrintTypeConstants
type
  PrintTypeConstants = TOleEnum;
const
  PrintTypeAll = $00000000;
  PrintTypeCellRange = $00000001;
  PrintTypeCurrentPage = $00000002;
  PrintTypePageRange = $00000003;

// Constants for enum SortByConstants
type
  SortByConstants = TOleEnum;
const
  SortByRow = $00000000;
  SortByCol = $00000001;

// Constants for enum SortKeyOrderConstants
type
  SortKeyOrderConstants = TOleEnum;
const
  SortKeyOrderNone = $00000000;
  SortKeyOrderAscending = $00000001;
  SortKeyOrderDescending = $00000002;

// Constants for enum TypeButtonAlignConstants
type
  TypeButtonAlignConstants = TOleEnum;
const
  TypeButtonAlignBottom = $00000000;
  TypeButtonAlignTop = $00000001;
  TypeButtonAlignLeft = $00000002;
  TypeButtonAlignRight = $00000003;

// Constants for enum TypeButtonTypeConstants
type
  TypeButtonTypeConstants = TOleEnum;
const
  TypeButtonTypeNormal = $00000000;
  TypeButtonTypeTwoState = $00000001;

// Constants for enum TypeCheckTextAlignConstants
type
  TypeCheckTextAlignConstants = TOleEnum;
const
  TypeCheckTextAlignLeft = $00000000;
  TypeCheckTextAlignRight = $00000001;

// Constants for enum TypeCheckTypeConstants
type
  TypeCheckTypeConstants = TOleEnum;
const
  TypeCheckTypeNormal = $00000000;
  TypeCheckTypeThreeState = $00000001;

// Constants for enum TypeDateFormatConstants
type
  TypeDateFormatConstants = TOleEnum;
const
  TypeDateFormatDDMONYY = $00000000;
  TypeDateFormatDDMMYY = $00000001;
  TypeDateFormatMMDDYY = $00000002;
  TypeDateFormatYYMMDD = $00000003;
  TypeDateFormatDefault = $00000063;

// Constants for enum TypeEditCharCaseConstants
type
  TypeEditCharCaseConstants = TOleEnum;
const
  TypeEditCharCaseSetLower = $00000000;
  TypeEditCharCaseSetNone = $00000001;
  TypeEditCharCaseSetUpper = $00000002;

// Constants for enum TypeEditCharSetConstants
type
  TypeEditCharSetConstants = TOleEnum;
const
  TypeEditCharSetASCII = $00000000;
  TypeEditCharSetAlpha = $00000001;
  TypeEditCharSetAlphanumeric = $00000002;
  TypeEditCharSetNumeric = $00000003;

// Constants for enum TypeHAlignConstants
type
  TypeHAlignConstants = TOleEnum;
const
  TypeHAlignLeft = $00000000;
  TypeHAlignRight = $00000001;
  TypeHAlignCenter = $00000002;

// Constants for enum TypeTextAlignVertConstants
type
  TypeTextAlignVertConstants = TOleEnum;
const
  TypeTextAlignVertBottom = $00000000;
  TypeTextAlignVertCenter = $00000001;
  TypeTextAlignVertTop = $00000002;

// Constants for enum TypeTime24HourConstants
type
  TypeTime24HourConstants = TOleEnum;
const
  TypeTime24Hour12HourClock = $00000000;
  TypeTime24Hour24HourClock = $00000001;
  TypeTime24HourDefault = $00000002;

// Constants for enum UserResizeConstants
type
  UserResizeConstants = TOleEnum;
const
  UserResizeNone = $00000000;
  UserResizeColumns = $00000001;
  UserResizeRows = $00000002;
  UserResizeBoth = $00000003;

// Constants for enum UserResizeConstants2
type
  UserResizeConstants2 = TOleEnum;
const
  UserResizeDefault = $00000000;
  UserResizeOn = $00000001;
  UserResizeOff = $00000002;

// Constants for enum AppearanceConstants
type
  AppearanceConstants = TOleEnum;
const
  AppearanceFlat = $00000000;
  Appearance3D = $00000001;
  Appearance3DWithBorder = $00000002;

// Constants for enum TextTipConstants
type
  TextTipConstants = TOleEnum;
const
  TextTipOff = $00000000;
  TextTipFixed = $00000001;
  TextTipFloating = $00000002;
  TextTipFixedFocusOnly = $00000003;
  TextTipFloatingFocusOnly = $00000004;

// Constants for enum ScrollBarTrackConstants
type
  ScrollBarTrackConstants = TOleEnum;
const
  ScrollBarTrackOff = $00000000;
  ScrollBarTrackVertical = $00000001;
  ScrollBarTrackHorizontal = $00000002;
  ScrollBarTrackBoth = $00000003;

// Constants for enum MousePointerConstants
type
  MousePointerConstants = TOleEnum;
const
  Default = $00000000;
  Arrow = $00000001;
  Cross = $00000002;
  IBeam = $00000003;
  Icon = $00000004;
  Sizing = $00000005;
  SizeNESW = $00000006;
  SizeNS = $00000007;
  SizeNWSE = $00000008;
  SizeWE = $00000009;
  UpArrow = $0000000A;
  Hourglass = $0000000B;
  NoDrop = $0000000C;
  ArrowHourglass = $0000000D;
  ArrowQuestion = $0000000E;
  SizeAll = $0000000F;
  Custom = $00000063;

// Constants for enum PrintPageOrderConstants
type
  PrintPageOrderConstants = TOleEnum;
const
  PageOrderAuto = $00000000;
  PageOrderDownThenOver = $00000001;
  PageOrderOverThenDown = $00000002;

// Constants for enum TypeVAlignConstants
type
  TypeVAlignConstants = TOleEnum;
const
  TypeVAlignTop = $00000000;
  TypeVAlignBottom = $00000001;
  TypeVAlignCenter = $00000002;

// Constants for enum PVScrollBarConstants
type
  PVScrollBarConstants = TOleEnum;
const
  ScrollBarShow = $00000000;
  ScrollBarAuto = $00000001;
  ScrollBarHide = $00000002;

// Constants for enum PVGrayAreaMarginTypeConstants
type
  PVGrayAreaMarginTypeConstants = TOleEnum;
const
  GrayAreaMarginTypeScaled = $00000000;
  GrayAreaMarginTypeActual = $00000001;

// Constants for enum PVPageViewTypeConstants
type
  PVPageViewTypeConstants = TOleEnum;
const
  PageViewTypeWholePage = $00000000;
  PageViewTypeNormalSize = $00000001;
  PageViewTypePercentage = $00000002;
  PageViewTypePageWidth = $00000003;
  PageViewTypePageHeight = $00000004;
  PageViewTypeMultiplePages = $00000005;

// Constants for enum PVZoomStateConstants
type
  PVZoomStateConstants = TOleEnum;
const
  ZoomStateIndeterminate = $00000000;
  ZoomStateIn = $00000001;
  ZoomStateOut = $00000002;
  ZoomStateSwitch = $00000003;

// Constants for enum ColUserSortIndicatorConstants
type
  ColUserSortIndicatorConstants = TOleEnum;
const
  ColUserSortIndicatorNone = $00000000;
  ColUserSortIndicatorAscending = $00000001;
  ColUserSortIndicatorDescending = $00000002;
  ColUserSortIndicatorDisabled = $00000003;

// Constants for enum UserColActionConstants
type
  UserColActionConstants = TOleEnum;
const
  UserColActionDefault = $00000000;
  UserColActionSort = $00000001;
  UserColActionSortNoIndicator = $00000002;

// Constants for enum ShowScrollTipsConstants
type
  ShowScrollTipsConstants = TOleEnum;
const
  ShowScrollTipsOff = $00000000;
  ShowScrollTipsVertical = $00000001;
  ShowScrollTipsHorizontal = $00000002;
  ShowScrollTipsBoth = $00000003;

// Constants for enum CellNoteIndicatorConstants
type
  CellNoteIndicatorConstants = TOleEnum;
const
  CellNoteIndicatorShowAndFireEvent = $00000000;
  CellNoteIndicatorShowAndDoNotFireEvent = $00000001;
  CellNoteIndicatorDoNotShowAndFireEvent = $00000002;
  CellNoteIndicatorDoNotShowAndDoNotFireEvent = $00000003;

// Constants for enum ExportRangeToTextFileConstants
type
  ExportRangeToTextFileConstants = TOleEnum;
const
  ExportRangeToTextFileCreateNewFile = $00000001;
  ExportRangeToTextFileAppendToExistingFile = $00000002;
  ExportRangeToTextFileUnformattedData = $00000004;
  ExportRangeToTextFileColHeaders = $00000008;
  ExportRangeToTextFileRowHeaders = $00000010;
  ExportRangeToTextFileAllHeaders = $00000018;

// Constants for enum ExportToTextFileConstants
type
  ExportToTextFileConstants = TOleEnum;
const
  ExportToTextFileCreateNewFile = $00000001;
  ExportToTextFileAppendToExistingFile = $00000002;
  ExportToTextFileUnformattedData = $00000004;
  ExportToTextFileColHeaders = $00000008;
  ExportToTextFileRowHeaders = $00000010;

// Constants for enum LoadTextFileConstants
type
  LoadTextFileConstants = TOleEnum;
const
  LoadTextFileNoHeaders = $00000000;
  LoadTextFileColHeaders = $00000001;
  LoadTextFileRowHeaders = $00000002;
  LoadTextFileClearDataOnly = $00000004;

// Constants for enum ExportToXMLConstants
type
  ExportToXMLConstants = TOleEnum;
const
  ExportToXMLFormattedData = $00000000;
  ExportToXMLUnFormattedData = $00000001;

// Constants for enum PrintFlagsConstants
type
  PrintFlagsConstants = TOleEnum;
const
  PrintFlagsNone = $00000000;
  PrintFlagsShowCommonDialog = $00000001;

// Constants for enum MergeConstants
type
  MergeConstants = TOleEnum;
const
  MergeNone = $00000000;
  MergeAlways = $00000001;
  MergeRestricted = $00000002;

// Constants for enum TypeComboAutoSearchConstants
type
  TypeComboAutoSearchConstants = TOleEnum;
const
  TypeComboBoxAutoSearchNone = $00000000;
  TypeComboBoxAutoSearchSingleChar = $00000001;
  TypeComboBoxAutoSearchMultipleChar = $00000002;
  TypeComboBoxAutoSearchSingleCharGreater = $00000003;

// Constants for enum SearchFlagsConstants
type
  SearchFlagsConstants = TOleEnum;
const
  SearchFlagsNone = $00000000;
  SearchFlagsGreaterOrEqual = $00000001;
  SearchFlagsPartialMatch = $00000002;
  SearchFlagsValue = $00000004;
  SearchFlagsCaseSensitive = $00000008;
  SearchFlagsSortedAscending = $00000010;
  SearchFlagsSortedDescending = $00000020;

// Constants for enum TypeLeadingZeroConstants
type
  TypeLeadingZeroConstants = TOleEnum;
const
  TypeLeadingZeroIntl = $00000000;
  TypeLeadingZeroNo = $00000001;
  TypeLeadingZeroYes = $00000002;

// Constants for enum TypeCurrencyNegStyleConstants
type
  TypeCurrencyNegStyleConstants = TOleEnum;
const
  TypeCurrencyNegStyleIntl = $00000000;
  TypeCurrencyNegStyle1 = $00000001;
  TypeCurrencyNegStyle2 = $00000002;
  TypeCurrencyNegStyle3 = $00000003;
  TypeCurrencyNegStyle4 = $00000004;
  TypeCurrencyNegStyle5 = $00000005;
  TypeCurrencyNegStyle6 = $00000006;
  TypeCurrencyNegStyle7 = $00000007;
  TypeCurrencyNegStyle8 = $00000008;
  TypeCurrencyNegStyle9 = $00000009;
  TypeCurrencyNegStyle10 = $0000000A;
  TypeCurrencyNegStyle11 = $0000000B;
  TypeCurrencyNegStyle12 = $0000000C;
  TypeCurrencyNegStyle13 = $0000000D;
  TypeCurrencyNegStyle14 = $0000000E;
  TypeCurrencyNegStyle15 = $0000000F;
  TypeCurrencyNegStyle16 = $00000010;

// Constants for enum TypeCurrencyPosStyleConstants
type
  TypeCurrencyPosStyleConstants = TOleEnum;
const
  TypeCurrencyPosStyleIntl = $00000000;
  TypeCurrencyPosStyle1 = $00000001;
  TypeCurrencyPosStyle2 = $00000002;
  TypeCurrencyPosStyle3 = $00000003;
  TypeCurrencyPosStyle4 = $00000004;

// Constants for enum TypeNumberNegStyleConstants
type
  TypeNumberNegStyleConstants = TOleEnum;
const
  TypeNumberNegStyleIntl = $00000000;
  TypeNumberNegStyle1 = $00000001;
  TypeNumberNegStyle2 = $00000002;
  TypeNumberNegStyle3 = $00000003;
  TypeNumberNegStyle4 = $00000004;
  TypeNumberNegStyle5 = $00000005;

// Constants for enum TypePercentNegStyleConstants
type
  TypePercentNegStyleConstants = TOleEnum;
const
  TypePercentNegStyleIntl = $00000000;
  TypePercentNegStyle1 = $00000001;
  TypePercentNegStyle2 = $00000002;
  TypePercentNegStyle3 = $00000003;
  TypePercentNegStyle4 = $00000004;
  TypePercentNegStyle5 = $00000005;
  TypePercentNegStyle6 = $00000006;
  TypePercentNegStyle7 = $00000007;
  TypePercentNegStyle8 = $00000008;

// Constants for enum TypeTextOrientConstants
type
  TypeTextOrientConstants = TOleEnum;
const
  TypeTextOrientHorizontal = $00000000;
  TypeTextOrientVerticalLTR = $00000001;
  TypeTextOrientDown = $00000002;
  TypeTextOrientUp = $00000003;
  TypeTextOrientInvert = $00000004;
  TypeTextOrientVerticalRTL = $00000005;

// Constants for enum GetCellSpanConstants
type
  GetCellSpanConstants = TOleEnum;
const
  GetCellSpanNo = $00000000;
  GetCellSpanYes = $00000001;
  GetCellSpanAnchor = $00000002;

// Constants for enum EVENTENABLEDConstants
type
  EVENTENABLEDConstants = TOleEnum;
const
  EventAllEvents = $00000000;
  EventAdvance = $00000001;
  EventAfterUserSort = $00000002;
  EventBeforeUserSort = $00000003;
  EventBlockSelected = $00000004;
  EventButtonClicked = $00000005;
  EventChange = $00000006;
  EventClick = $00000007;
  EventColWidthChange = $00000008;
  EventComboCloseUp = $00000009;
  EventComboDropDown = $0000000A;
  EventComboSelChange = $0000000B;
  EventCustomFunction = $0000000C;
  EventDataColConfig = $0000000D;
  EventDataFill = $0000000E;
  EventDblClick = $0000000F;
  EventDragDropBlock = $00000010;
  EventDrawItem = $00000011;
  EventEditChange = $00000012;
  EventEditError = $00000013;
  EventEditMode = $00000014;
  EventEnterRow = $00000015;
  EventKeyDown = $00000016;
  EventKeyPress = $00000017;
  EventKeyUp = $00000018;
  EventLeaveCell = $00000019;
  EventLeaveRow = $0000001A;
  EventOLECompleteDrag = $0000001B;
  EventOLEDragDrop = $0000001C;
  EventOLEDragOver = $0000001D;
  EventOLEGiveFeedback = $0000001E;
  EventOLESetData = $0000001F;
  EventOLEStartDrag = $00000020;
  EventPrintAbort = $00000021;
  EventPrintMsgBox = $00000022;
  EventQueryAdvance = $00000023;
  EventQueryData = $00000024;
  EventRightClick = $00000025;
  EventRowHeightChange = $00000026;
  EventSelChange = $00000027;
  EventTextTipFetch = $00000028;
  EventTopLeftChange = $00000029;
  EventUserFormulaEntered = $0000002A;
  EventVirtualClearData = $0000002B;

// Constants for enum OLEDropModeConstants
type
  OLEDropModeConstants = TOleEnum;
const
  fpOLEDropNone = $00000000;
  fpOLEDropManual = $00000001;

// Constants for enum OLEDragOverConstants
type
  OLEDragOverConstants = TOleEnum;
const
  fpEnter = $00000000;
  fpLeave = $00000001;
  fpOver = $00000002;

// Constants for enum ClipFormatConstants
type
  ClipFormatConstants = TOleEnum;
const
  fpCFText = $00000001;
  fpCFBitmap = $00000002;
  fpCFMetafile = $00000003;
  fpCFDIB = $00000008;
  fpCFPalette = $00000009;
  fpCFEMetafile = $0000000E;
  fpCFFiles = $0000000F;
  fpCFRTF = $FFFFBF01;

// Constants for enum OLEDropModeEffectConstants
type
  OLEDropModeEffectConstants = TOleEnum;
const
  fpOLEDropEffectNone = $00000000;
  fpOLEDropEffectCopy = $00000001;
  fpOLEDropEffectMove = $00000002;
  fpOLEDropEffectScroll = $80000000;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _DummyfpListCursor = interface;
  _DSpreadCursor = interface;
  IfpDataObjectFiles = interface;
  IfpDataObjectFilesDisp = dispinterface;
  IfpDataObject = interface;
  IfpDataObjectDisp = dispinterface;
  _DSpreadSheet = dispinterface;
  _DSpreadEvents = dispinterface;
  _DSpreadPreview = dispinterface;
  _DSpreadPreviewEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  fpDataObjectFiles = IfpDataObjectFiles;
  fpDataObject = IfpDataObject;
  vaSpread = _DSpreadSheet;
  vaSpreadPreview = _DSpreadPreview;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PSmallint1 = ^Smallint; {*}
  PWideString1 = ^WideString; {*}
  POleVariant1 = ^OleVariant; {*}
  PDouble1 = ^Double; {*}
  PInteger1 = ^Integer; {*}
  PSingle1 = ^Single; {*}
  PWordBool1 = ^WordBool; {*}


// *********************************************************************//
// Interface: _DummyfpListCursor
// Flags:     (528) Hidden Restricted
// GUID:      {123AA700-D188-11CD-AD48-00AA003C9CB6}
// *********************************************************************//
  _DummyfpListCursor = interface(IUnknown)
    ['{123AA700-D188-11CD-AD48-00AA003C9CB6}']
  end;

// *********************************************************************//
// Interface: _DSpreadCursor
// Flags:     (0)
// GUID:      {9F6AA700-D188-11CD-AD48-00AA003C9CB6}
// *********************************************************************//
  _DSpreadCursor = interface(IUnknown)
    ['{9F6AA700-D188-11CD-AD48-00AA003C9CB6}']
  end;

// *********************************************************************//
// Interface: IfpDataObjectFiles
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {69310C25-4993-11D1-8905-0020AF131A57}
// *********************************************************************//
  IfpDataObjectFiles = interface(IDispatch)
    ['{69310C25-4993-11D1-8905-0020AF131A57}']
    function  Get_Item(lIndex: Integer): WideString; safecall;
    function  Get_Count: Integer; safecall;
    procedure Add(const bstrFilename: WideString; vIndex: OleVariant); safecall;
    procedure Clear; safecall;
    procedure Remove(vIndex: OleVariant); safecall;
    function  Get__NewEnum: IUnknown; safecall;
    property Item[lIndex: Integer]: WideString read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IfpDataObjectFilesDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {69310C25-4993-11D1-8905-0020AF131A57}
// *********************************************************************//
  IfpDataObjectFilesDisp = dispinterface
    ['{69310C25-4993-11D1-8905-0020AF131A57}']
    property Item[lIndex: Integer]: WideString readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    procedure Add(const bstrFilename: WideString; vIndex: OleVariant); dispid 2;
    procedure Clear; dispid 3;
    procedure Remove(vIndex: OleVariant); dispid 4;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IfpDataObject
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {69310C27-4993-11D1-8905-0020AF131A57}
// *********************************************************************//
  IfpDataObject = interface(IDispatch)
    ['{69310C27-4993-11D1-8905-0020AF131A57}']
    procedure Clear; safecall;
    function  GetData(nFormat: Smallint): OleVariant; safecall;
    function  GetFormat(nFormat: Smallint): WordBool; safecall;
    procedure SetData(vValue: OleVariant; vFormat: OleVariant); safecall;
    function  Get_Files: IfpDataObjectFiles; safecall;
    property Files: IfpDataObjectFiles read Get_Files;
  end;

// *********************************************************************//
// DispIntf:  IfpDataObjectDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {69310C27-4993-11D1-8905-0020AF131A57}
// *********************************************************************//
  IfpDataObjectDisp = dispinterface
    ['{69310C27-4993-11D1-8905-0020AF131A57}']
    procedure Clear; dispid 1;
    function  GetData(nFormat: Smallint): OleVariant; dispid 2;
    function  GetFormat(nFormat: Smallint): WordBool; dispid 3;
    procedure SetData(vValue: OleVariant; vFormat: OleVariant); dispid 4;
    property Files: IfpDataObjectFiles readonly dispid 5;
  end;

// *********************************************************************//
// DispIntf:  _DSpreadSheet
// Flags:     (4096) Dispatchable
// GUID:      {41F841C3-AE16-11D5-8817-0050DA6EF5E5}
// *********************************************************************//
  _DSpreadSheet = dispinterface
    ['{41F841C3-AE16-11D5-8817-0050DA6EF5E5}']
    property Col: Integer dispid 22;
    property Col2: Integer dispid 23;
    property ColHidden: WordBool dispid 25;
    property RowsFrozen: Integer dispid 107;
    property ScrollBarExtMode: WordBool dispid 108;
    property ScrollBarMaxAlign: WordBool dispid 109;
    property ChangeMade: WordBool dispid 19;
    property DataChanged: WordBool dispid 30;
    property DataColCnt: Integer dispid 31;
    property DataField: WideString dispid 32;
    property Clip: WideString dispid 20;
    property ClipValue: WideString dispid 21;
    property ColPageBreak: WordBool dispid 26;
    property SelBlockCol: Integer dispid 112;
    property ShadowColor: OLE_COLOR dispid 123;
    property Row: Integer dispid 102;
    property Row2: Integer dispid 103;
    property StartingColNumber: Integer dispid 127;
    property SelStart: Integer dispid 121;
    property SelText: WideString dispid 122;
    property RowHidden: WordBool dispid 105;
    property RowPageBreak: WordBool dispid 106;
    property ScrollBars: ScrollBarsConstants dispid 110;
    property ScrollBarShowMax: WordBool dispid 111;
    property RestrictCols: WordBool dispid 99;
    property RestrictRows: WordBool dispid 100;
    property RetainSelBlock: WordBool dispid 101;
    property GridSolid: WordBool dispid 59;
    property hDCPrinter: OLE_HANDLE dispid 60;
    property InterfaceDesigner: Smallint dispid 61;
    property AutoClipboard: WordBool dispid 10;
    property AutoSize: WordBool dispid 11;
    property DataFillEvent: WordBool dispid 33;
    property GridColor: OLE_COLOR dispid 56;
    property LockForeColor: OLE_COLOR dispid 66;
    property MaxCols: Integer dispid 67;
    property MaxRows: Integer dispid 68;
    property GridShowHoriz: WordBool dispid 57;
    property GridShowVert: WordBool dispid 58;
    property IsBlockSelected: WordBool dispid 62;
    property AutoCalc: WordBool dispid 9;
    property AllowDragDrop: WordBool dispid 5;
    property AllowMultiBlocks: WordBool dispid 6;
    property AllowUserFormulas: WordBool dispid 7;
    property ColsFrozen: Integer dispid 27;
    property CursorStyle: CursorStyleConstants dispid 28;
    property CursorType: CursorTypeConstants dispid 29;
    property ActiveCol: Integer dispid 2;
    property BlockMode: WordBool dispid 12;
    property ButtonDrawMode: Smallint dispid 13;
    property CellType: CellTypeConstants dispid 18;
    property ActiveRow: Integer dispid 3;
    property AllowCellOverflow: WordBool dispid 4;
    property ArrowsExitEditMode: WordBool dispid 8;
    property SelBackColor: OLE_COLOR dispid 289;
    property PrintMarginRight: Integer dispid 88;
    property PrintMarginTop: Integer dispid 89;
    function  OwnerPrintDraw(hDC: OLE_HANDLE; Left: Integer; Top: Integer; Right: Integer; 
                             Bottom: Integer; Page: Smallint): WordBool; dispid 328;
    function  OwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer; Right: Integer; 
                                  Bottom: Integer; var PageCount: Smallint): WordBool; dispid 329;
    function  ExportRangeToTextFile(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                    const FileName: WideString; const CellDelim: WideString; 
                                    const ColDelim: WideString; const RowDelim: WideString; 
                                    Flags: ExportRangeToTextFileConstants; const LogFile: WideString): WordBool; dispid 331;
    property PrintPageEnd: Smallint dispid 90;
    property PrintPageStart: Smallint dispid 91;
    property PrintUseDataMax: WordBool dispid 95;
    property ProcessTab: WordBool dispid 96;
    property PrintJobName: WideString dispid 85;
    property PrintMarginBottom: Integer dispid 86;
    property PrintMarginLeft: Integer dispid 87;
    function  ExportToTextFile(const FileName: WideString; const CellDelim: WideString; 
                               const ColDelim: WideString; const RowDelim: WideString; 
                               Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool; dispid 332;
    function  ExportRangeToXML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                               const FileName: WideString; const Root: WideString; 
                               const Collection: WideString; Flags: ExportToXMLConstants; 
                               const LogFile: WideString): WordBool; dispid 325;
    function  ExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                var Buff: WideString; Flags: ExportToXMLConstants; 
                                const LogFile: WideString): WordBool; dispid 326;
    function  ExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                     const Root: WideString; const Collection: WideString; 
                                     var Buff: WideString; Flags: ExportToXMLConstants; 
                                     const LogFile: WideString): WordBool; dispid 327;
    procedure ScriptGetFirstValidCell(var Col: OleVariant; var Row: OleVariant); dispid 444;
    function  ScriptGetIteration(var MaxIterations: OleVariant; var MaxChange: OleVariant): WordBool; dispid 445;
    procedure ScriptCFGetCellParam(Param: Smallint; var Col: OleVariant; var Row: OleVariant); dispid 435;
    function  ExportToXML(const FileName: WideString; const Root: WideString; 
                          const Collection: WideString; Flags: ExportToXMLConstants; 
                          const LogFile: WideString): WordBool; dispid 324;
    function  LoadTextFile(const FileName: WideString; const CellDelim: WideString; 
                           const ColDelim: WideString; const RowDelim: WideString; 
                           Flags: LoadTextFileConstants; const LogFile: WideString): WordBool; dispid 333;
    procedure SetCalTextOverride(const ShortDays: WideString; const LongDays: WideString; 
                                 const ShortMonths: WideString; const LongMonths: WideString; 
                                 const OkText: WideString; const CancelText: WideString); dispid 334;
    procedure GetCalTextOverride(var ShortDays: WideString; var LenShortDays: Smallint; 
                                 var LongDays: WideString; var LenLongDays: Smallint; 
                                 var ShortMonths: WideString; var LenShortMonths: Smallint; 
                                 var LongMonths: WideString; var LenLongMonths: Smallint; 
                                 var OkText: WideString; var LenOkText: Smallint; 
                                 var CancelText: WideString; var LenCancelText: Smallint); dispid 335;
    property ColUserSortIndicator[lCol: Integer]: ColUserSortIndicatorConstants dispid 322;
    function  IsFetchCellNote: WordBool; dispid 323;
    property ReDraw: WordBool dispid 98;
    property SelectBlockOptions: Smallint dispid 116;
    property SelLength: Integer dispid 117;
    property PrintBorder: WordBool dispid 79;
    property PrintColHeaders: WordBool dispid 80;
    property PrintColor: WordBool dispid 81;
    property SelModeIndex: Integer dispid 118;
    property SelModeSelected: WordBool dispid 120;
    property ShadowDark: OLE_COLOR dispid 124;
    property ShadowText: OLE_COLOR dispid 125;
    property SelBlockCol2: Integer dispid 113;
    property SelBlockRow: Integer dispid 114;
    property SelBlockRow2: Integer dispid 115;
    property PrintHeader: WideString dispid 84;
    property PrintType: PrintTypeConstants dispid 94;
    property NoBeep: WordBool dispid 74;
    property NoBorder: WordBool dispid 75;
    property Protect: WordBool dispid 97;
    property PrintRowHeaders: WordBool dispid 92;
    property PrintShadows: WordBool dispid 93;
    property OperationMode: OperationModeConstants dispid 76;
    property PrintAbortMsg: WideString dispid 78;
    property PrintFooter: WideString dispid 82;
    property PrintGrid: WordBool dispid 83;
    property MaxTextCellHeight: Double dispid 69;
    property MaxTextCellWidth: Double dispid 70;
    property MoveActiveOnFocus: WordBool dispid 71;
    property FormulaSync: WordBool dispid 216;
    property TypeComboBoxList: WideString dispid 148;
    property TypeComboBoxString: WideString dispid 149;
    property TypeComboBoxMaxDrop: Smallint dispid 274;
    property PrintOrientation: PrintOrientationConstants dispid 214;
    property BackColorStyle: BackColorStyleConstants dispid 215;
    property TypeDateCentury: WordBool dispid 150;
    property TypeComboBoxEditable: WordBool dispid 146;
    property TypeEditCharCase: TypeEditCharCaseConstants dispid 155;
    property TypeEditCharSet: TypeEditCharSetConstants dispid 156;
    property TypeComboBoxIndex: Smallint dispid 147;
    property TypeComboBoxCount: Smallint dispid 144;
    property TypeComboBoxCurSel: Smallint dispid 145;
    property Appearance: AppearanceConstants dispid 273;
    property PrintFirstPageNumber: Integer dispid 285;
    property TypeMaxEditLen: Smallint dispid 275;
    property FontSize: Single dispid 210;
    property TypeComboBoxhWnd: Integer dispid 288;
    property PrintPageOrder: PrintPageOrderConstants dispid 283;
    property PrintPageCount: Integer dispid 284;
    property FontStrikethru: WordBool dispid 211;
    property FontName: WideString dispid 209;
    property CursorIcon: IPictureDisp dispid 213;
    property TypeCheckType: TypeCheckTypeConstants dispid 272;
    property FontUnderline: WordBool dispid 212;
    property FontBold: WordBool dispid 207;
    property FontItalic: WordBool dispid 208;
    property TypeButtonPictureDown: IPictureDisp dispid 136;
    property TypeButtonShadowSize: Smallint dispid 137;
    property TypeButtonText: WideString dispid 138;
    property TypeCheckCenter: WordBool dispid 141;
    property TypeCheckText: WideString dispid 142;
    property TypeButtonTextColor: OLE_COLOR dispid 139;
    property TypeEditPassword: WordBool dispid 159;
    property TypePictMaintainScale: WordBool dispid 177;
    property TypePictCenter: WordBool dispid 176;
    property TypePicMask: WideString dispid 175;
    property TypePicDefaultText: WideString dispid 174;
    property TypeOwnerDrawStyle: Integer dispid 173;
    property TypeHAlign: TypeHAlignConstants dispid 168;
    property TypeButtonType: TypeButtonTypeConstants dispid 140;
    property TypeDateMax: WideString dispid 152;
    property TypeDateMin: WideString dispid 153;
    property TypeCheckTextAlign: TypeCheckTextAlignConstants dispid 143;
    property TypeEditMultiLine: WordBool dispid 158;
    property TypeDateSeparator: Smallint dispid 154;
    property TypeDateFormat: TypeDateFormatConstants dispid 151;
    property TypeButtonDarkColor: OLE_COLOR dispid 133;
    property TopRow: Integer dispid 129;
    property TypeButtonAlign: TypeButtonAlignConstants dispid 130;
    property TypeButtonBorderColor: OLE_COLOR dispid 131;
    property TypeButtonLightColor: OLE_COLOR dispid 134;
    property TypeButtonPicture: IPictureDisp dispid 135;
    property TypeButtonColor: OLE_COLOR dispid 132;
    property VirtualCurRowCount: Integer dispid 196;
    property VirtualCurTop: Integer dispid 197;
    property VirtualMaxRows: Integer dispid 198;
    property EditMode: WordBool dispid 47;
    property EditModePermanent: WordBool dispid 48;
    property StartingRowNumber: Integer dispid 128;
    property UserResizeCol: UserResizeConstants2 dispid 193;
    property VisibleCols: Integer dispid 203;
    property VisibleRows: Integer dispid 204;
    property VScrollSpecial: WordBool dispid 205;
    property UserResizeRow: UserResizeConstants2 dispid 194;
    property Value: WideString dispid 195;
    property VirtualMode: WordBool dispid 199;
    property EditEnterAction: EditEnterActionConstants dispid 46;
    property DAutoHeadings: WordBool dispid 38;
    property DAutoSave: WordBool dispid 39;
    property DAutoSizeCols: DAutoSizeColsConstants dispid 40;
    property LeftCol: Integer dispid 63;
    property Lock: WordBool dispid 64;
    property LockBackColor: OLE_COLOR dispid 65;
    property DataRowCnt: Integer dispid 34;
    property EditModeReplace: WordBool dispid 49;
    property Formula: WideString dispid 54;
    property GrayAreaBackColor: OLE_COLOR dispid 55;
    property DAutoCellTypes: WordBool dispid 36;
    property DAutoFill: WordBool dispid 37;
    property DInformActiveRowChange: WordBool dispid 43;
    property VScrollSpecialType: Smallint dispid 206;
    property ScrollBarTrack: ScrollBarTrackConstants dispid 279;
    property TypeVAlign: TypeVAlignConstants dispid 280;
    property TypeTime24Hour: TypeTime24HourConstants dispid 186;
    property TypeTimeMax: WideString dispid 187;
    property TypeTimeMin: WideString dispid 188;
    property ClipboardOptions: Smallint dispid 281;
    property PrintSmartPrint: WordBool dispid 282;
    property PrintNextPageBreakCol: Integer dispid 286;
    property PrintNextPageBreakRow: Integer dispid 287;
    property TypeComboBoxWidth: Smallint dispid 276;
    property TextTip: TextTipConstants dispid 277;
    property TextTipDelay: Integer dispid 278;
    property UnitType: UnitTypeConstants dispid 191;
    property UserResize: UserResizeConstants dispid 192;
    property TypeTextPrefix: WordBool dispid 182;
    property TypeTextShadow: WordBool dispid 183;
    property VirtualOverlap: Integer dispid 200;
    property VirtualRows: Integer dispid 201;
    property VirtualScrollBuffer: WordBool dispid 202;
    property TypeTextShadowIn: WordBool dispid 184;
    property TypeTextWordWrap: WordBool dispid 185;
    property TypeTimeSeconds: WordBool dispid 189;
    property TypeTimeSeparator: Smallint dispid 190;
    property TypePictPicture: IPictureDisp dispid 178;
    property TypePictStretch: WordBool dispid 179;
    property TypeSpin: WordBool dispid 180;
    property TypeCurrencySeparator: WideString dispid 352;
    property ColHeaderRows: Integer dispid 342;
    property UserColAction: UserColActionConstants dispid 319;
    property TypeCurrencySymbol: WideString dispid 353;
    property TypeCurrencyMax: Double dispid 350;
    property TypeCurrencyDecimal: WideString dispid 351;
    property ShowScrollTips: ShowScrollTipsConstants dispid 320;
    property TwoDigitYearMax: Smallint dispid 291;
    property OLEDropMode: OLEDropModeConstants dispid 315;
    property ColID: WideString dispid 339;
    property CellNoteIndicator: CellNoteIndicatorConstants dispid 321;
    property CellNote: WideString dispid 318;
    property SelForeColor: OLE_COLOR dispid 290;
    property TypeCurrencyNegStyle: TypeCurrencyNegStyleConstants dispid 356;
    function  GetItemData: Integer; dispid 246;
    property TypeSpinWrap: WordBool dispid 347;
    property TypeNegRed: WordBool dispid 348;
    function  GetIteration(var MaxIterations: Smallint; var MaxChange: Double): WordBool; dispid 247;
    function  GetDataFillData(var Var_: OleVariant; VarType: Smallint): WordBool; dispid 244;
    procedure GetFirstValidCell(var Col: Integer; var Row: Integer); dispid 245;
    property TypeCurrencyMin: Double dispid 349;
    property TypeComboBoxAutoSearch: TypeComboAutoSearchConstants dispid 345;
    property TypeCurrencyLeadingZero: TypeLeadingZeroConstants dispid 354;
    property TypeCurrencyDecPlaces: Smallint dispid 355;
    property TypeSpinInc: Double dispid 346;
    property ColHeadersAutoTextIndex: Integer dispid 343;
    property ColHeadersUserSortIndex: Integer dispid 344;
    property RowHeadersShow: WordBool dispid 379;
    property TypePercentLeadingZero: TypeLeadingZeroConstants dispid 380;
    property SelectionCount: Integer dispid 489;
    property Font: IFontDisp dispid -512;
    property ForeColor: OLE_COLOR dispid -513;
    property BackColor: OLE_COLOR dispid -501;
    property TypePercentDecPlaces: Smallint dispid 371;
    property TypeNumberMin: Double dispid 360;
    property TypeCurrencyPosStyle: TypeCurrencyPosStyleConstants dispid 357;
    property TypeCurrencyShowSep: WordBool dispid 358;
    property TypeNumberMax: Double dispid 361;
    property TypeNumberDecimal: WideString dispid 362;
    property TypeNumberSeparator: WideString dispid 363;
    property BorderStyle: BorderStyleConstants dispid -504;
    property TypeEllipses: WordBool dispid 330;
    property ScriptEnhanced: WordBool dispid 336;
    property ColMerge: MergeConstants dispid 337;
    property RowHeaderCols: Integer dispid 340;
    property RowHeadersAutoTextIndex: Integer dispid 341;
    property RowMerge: MergeConstants dispid 338;
    property ColHeadersAutoText: HeaderDisplayConstants dispid 376;
    property TypePercentNegStyle: TypePercentNegStyleConstants dispid 372;
    property TypeTextOrient: TypeTextOrientConstants dispid 373;
    property CellTag: WideString dispid 374;
    property ColHeadersShow: WordBool dispid 377;
    property RowHeadersAutoText: HeaderDisplayConstants dispid 378;
    property AllowEditOverflow: WordBool dispid 375;
    property MaxTextRowHeight[lRow: Integer]: Double dispid 219;
    property ColWidth[lCol: Integer]: Double dispid 217;
    property hWnd: OLE_HANDLE dispid -515;
    property TypeCheckPicture[Index: Smallint]: IPictureDisp dispid 223;
    property MaxTextColWidth[lCol: Integer]: Double dispid 218;
    property Text: WideString dispid -517;
    property SortKeyOrder[nIndex: Smallint]: SortKeyOrderConstants dispid 222;
    property SortKey[nIndex: Smallint]: Integer dispid 221;
    function  CFGetStringParam(Param: Smallint): WideString; dispid 232;
    procedure CFGetCellParam(Param: Smallint; var Col: Integer; var Row: Integer); dispid 226;
    function  AddCustomFunction(const FunctionName: WideString; ParameterCnt: Smallint): WordBool; dispid 224;
    function  CFGetDoubleParam(Param: Smallint): Double; dispid 227;
    function  CFGetDoubleParamExt(Param: Smallint; var ParamValue: Double): Double; dispid 228;
    function  CFGetLongParam(Param: Smallint): Integer; dispid 229;
    function  AddCustomFunctionExt(const FunctionName: WideString; MinParamCnt: Smallint; 
                                   MaxParamCnt: Smallint; Flags: Integer): WordBool; dispid 225;
    procedure CFSetResult(Var_: OleVariant); dispid 233;
    function  CFGetParamInfo(Param: Smallint; var Type_: Smallint; var Status: Smallint): WordBool; dispid 230;
    procedure CFGetRangeParam(Param: Smallint; var Col: Integer; var Row: Integer; 
                              var Col2: Integer; var Row2: Integer); dispid 231;
    function  ColNumberToLetter(HeaderNumber: Integer): WideString; dispid 234;
    procedure ColWidthToTwips(Width: Single; var Twips: Integer); dispid 235;
    procedure GetBottomRightCell(var Col: Integer; var Row: Integer); dispid 236;
    function  GetColItemData(Col: Integer): Integer; dispid 241;
    function  QueryCustomName(const Name: WideString): WideString; dispid 242;
    function  GetCustomName(const Name: WideString): WideString; dispid 243;
    function  SaveToFile(const FileName: WideString; DataOnly: WordBool): WordBool; dispid 260;
    function  SetCellDirtyFlag(Col: Integer; Row: Integer; Dirty: WordBool): WordBool; dispid 261;
    function  GetRowItemData(Row: Integer): Integer; dispid 251;
    procedure GetClientArea(var Width: Integer; var Height: Integer); dispid 240;
    procedure GetLastValidCell(var Col: Integer; var Row: Integer); dispid 248;
    function  GetMultiSelItem(SelPrev: Integer): Integer; dispid 249;
    function  GetRefStyle: Smallint; dispid 250;
    function  GetCellDirtyFlag(Col: Integer; Row: Integer): WordBool; dispid 237;
    procedure GetCellFromScreenCoord(var Col: Integer; var Row: Integer; x: Integer; y: Integer); dispid 238;
    function  GetCellPos(Col: Integer; Row: Integer; var x: Integer; var y: Integer; 
                         var Width: Integer; var Height: Integer): WordBool; dispid 239;
    function  SaveTabFile(const FileName: WideString): WordBool; dispid 259;
    function  LoadTabFile(const FileName: WideString): WordBool; dispid 257;
    procedure RowHeightToTwips(Row: Integer; Height: Single; var Twips: Integer); dispid 258;
    function  IsVisible(Col: Integer; Row: Integer; Partial: WordBool): WordBool; dispid 255;
    property RowHeight[lRow: Integer]: Double dispid 220;
    function  LoadFromFile(const FileName: WideString): WordBool; dispid 256;
    function  GetText(Col: Integer; Row: Integer; var Var_: OleVariant): WordBool; dispid 252;
    function  SetDataFillData(Var_: OleVariant): WordBool; dispid 264;
    procedure SetItemData(Value: Integer); dispid 265;
    procedure SetColItemData(Col: Integer; Value: Integer); dispid 262;
    function  IsCellSelected(Col: Integer; Row: Integer): WordBool; dispid 253;
    function  IsFormulaValid(const Formula: WideString): WordBool; dispid 254;
    function  SetCustomName(const Name: WideString; const Value: WideString): WordBool; dispid 263;
    function  SetTextTipAppearance(const FontName: WideString; FontSize: Smallint; 
                                   FontBold: WordBool; FontItalic: WordBool; BackColor: Integer; 
                                   ForeColor: Integer): WordBool; dispid 302;
    function  ExportToHTML(const FileName: WideString; AppendFlag: WordBool; 
                           const LogFile: WideString): WordBool; dispid 303;
    function  ExportRangeToHTML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                const FileName: WideString; AppendFlag: WordBool; 
                                const LogFile: WideString): WordBool; dispid 304;
    function  IsExcelFile(const FileName: WideString): Smallint; dispid 307;
    function  GetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                var ListCount: Smallint; const LogFileName: WideString; 
                                var WorkbookHandle: Smallint; Replace: WordBool): WordBool; dispid 308;
    function  GetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool; dispid 305;
    function  RemoveCustomFunction(const FuncName: WideString): WordBool; dispid 313;
    function  ImportExcelSheet(WorkbookHandle: Smallint; Sheet: OleVariant): WordBool; dispid 309;
    function  ExportToExcel(const FileName: WideString; const SheetName: WideString; 
                            const LogFileName: WideString): WordBool; dispid 310;
    function  EnumCustomFunction(const PrevFuncName: WideString; var FuncName: WideString): WordBool; dispid 311;
    function  ReCalcCell(Col: Integer; Row: Integer): WordBool; dispid 314;
    procedure OLEDrag; dispid 316;
    function  GetCustomFunction(const FuncName: WideString; var MinArgs: Smallint; 
                                var MaxArgs: Smallint; var Flags: Integer): WordBool; dispid 312;
    function  SetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool; dispid 306;
    procedure Reset; dispid 479;
    procedure SetActiveCell(lCol: Integer; lRow: Integer); dispid 480;
    procedure SetCellBorder(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                            nIndex: Smallint; crColor: OLE_COLOR; nStyle: CellBorderStyleConstants); dispid 481;
    function  Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                   nSortBy: SortByConstants; var SortKeys: OleVariant; var SortKeyOrders: OleVariant): WordBool; dispid 484;
    procedure SwapRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer); dispid 485;
    procedure SetSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer); dispid 482;
    function  GetCellSpan(lCol: Integer; lRow: Integer; var pvColAnchor: OleVariant; 
                          var pvRowAnchor: OleVariant; var pvNumCols: OleVariant; 
                          var pvNumRows: OleVariant): GetCellSpanConstants; dispid 491;
    procedure TypeComboBoxClear(lCol: Integer; lRow: Integer); dispid 486;
    procedure TypeComboBoxRemoveItem(lCol: Integer; lRow: Integer; nIndex: Smallint); dispid 487;
    procedure VirtualRefresh; dispid 488;
    procedure RemoveCellSpan(lCol: Integer; lRow: Integer); dispid 492;
    function  GetColFromID(const ColID: WideString): Integer; dispid 493;
    function  AddCellSpan(lCol: Integer; lRow: Integer; lNumCols: Integer; lNumRows: Integer): WordBool; dispid 490;
    function  ScriptGetCellPos(Col: Integer; Row: Integer; var x: OleVariant; var y: OleVariant; 
                               var Width: OleVariant; var Height: OleVariant): WordBool; dispid 442;
    procedure ScriptColWidthToTwips(Width: Single; var Twips: OleVariant); dispid 439;
    function  ScriptCFGetDoubleParamExt(Param: Smallint; var ParamValue: OleVariant): Double; dispid 436;
    function  GetFloat(Col: Integer; Row: Integer; var Value: Double): WordBool; dispid 296;
    procedure ScriptGetBottomRightCell(var Col: OleVariant; var Row: OleVariant); dispid 440;
    procedure ScriptGetCellFromScreenCoord(var Col: OleVariant; var Row: OleVariant; x: Integer; 
                                           y: Integer); dispid 441;
    function  ScriptCFGetParamInfo(Param: Smallint; var Type_: OleVariant; var Status: OleVariant): WordBool; dispid 437;
    procedure ScriptTwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: OleVariant); dispid 449;
    procedure ScriptGetLastValidCell(var Col: OleVariant; var Row: OleVariant); dispid 446;
    procedure ScriptGetClientArea(var Width: OleVariant; var Height: OleVariant); dispid 443;
    procedure ScriptCFGetRangeParam(Param: Smallint; var Col: OleVariant; var Row: OleVariant; 
                                    var Col2: OleVariant; var Row2: OleVariant); dispid 438;
    procedure ScriptRowHeightToTwips(Row: Integer; Height: Single; var Twips: OleVariant); dispid 447;
    procedure ScriptTwipsToColWidth(Twips: Integer; var ColWidth: OleVariant); dispid 448;
    function  SetOddEvenRowColor(clrBackOdd: Integer; clrForeOdd: Integer; clrBackEven: Integer; 
                                 clrForeEven: Integer): WordBool; dispid 295;
    function  SetActionKey(Action: Smallint; fShift: WordBool; fCtrl: WordBool; Key: Smallint): WordBool; dispid 293;
    procedure TwipsToColWidth(Twips: Integer; var ColWidth: Single); dispid 270;
    procedure SetRefStyle(RefStyle: Smallint); dispid 267;
    function  GetTextTipAppearance(var FontName: WideString; var FontSize: Smallint; 
                                   var FontBold: WordBool; var FontItalic: WordBool; 
                                   var BackColor: Integer; var ForeColor: Integer): WordBool; dispid 301;
    procedure TwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: Single); dispid 271;
    function  GetActionKey(Action: Smallint; var fShift: WordBool; var fCtrl: WordBool; 
                           var Key: Smallint): WordBool; dispid 292;
    procedure SetRowItemData(Row: Integer; Value: Integer); dispid 268;
    procedure SetCalText(const ShortDays: WideString; const LongDays: WideString; 
                         const ShortMonths: WideString; const LongMonths: WideString; 
                         const OkText: WideString; const CancelText: WideString); dispid 300;
    function  SetFloat(Col: Integer; Row: Integer; Value: Double): WordBool; dispid 297;
    function  GetOddEvenRowColor(var clrBackOdd: Integer; var clrForeOdd: Integer; 
                                 var clrBackEven: Integer; var clrForeEven: Integer): WordBool; dispid 294;
    procedure SetText(Col: Integer; Row: Integer; Var_: OleVariant); dispid 269;
    function  GetInteger(Col: Integer; Row: Integer; var Value: Integer): WordBool; dispid 298;
    function  SetInteger(Col: Integer; Row: Integer; Value: Integer): WordBool; dispid 299;
    procedure MoveRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer); dispid 477;
    procedure ReCalc; dispid 478;
    procedure InsertCols(lCol: Integer; lNumCols: Integer); dispid 475;
    procedure ClipboardCopy; dispid 466;
    procedure ClipboardCut; dispid 467;
    procedure InsertRows(lRow: Integer; lNumRows: Integer); dispid 476;
    procedure DataSave; dispid 472;
    function  ScriptGetTextTipAppearance(var FontName: OleVariant; var FontSize: OleVariant; 
                                         var FontBold: OleVariant; var FontItalic: OleVariant; 
                                         var BackColor: OleVariant; var ForeColor: OleVariant): WordBool; dispid 454;
    function  ScriptGetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                      var ListCount: OleVariant; const LogFileName: WideString; 
                                      var WorkbookHandle: OleVariant; Replace: WordBool): WordBool; dispid 455;
    function  ScriptEnumCustomFunction(const PrevFuncName: WideString; var FuncName: OleVariant): WordBool; dispid 456;
    procedure GetCellBorder(lCol: Integer; lRow: Integer; nIndex: Smallint; 
                            var pcrColor: OleVariant; var pnStyle: OleVariant); dispid 473;
    procedure GetSelection(lIndex: Integer; var plCol: OleVariant; var plRow: OleVariant; 
                           var plCol2: OleVariant; var plRow2: OleVariant); dispid 474;
    procedure ClearRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                         bDataOnly: WordBool); dispid 464;
    procedure ClearSelection; dispid 465;
    property TypePercentDecimal: WideString dispid 370;
    property TypeNumberShowSep: WordBool dispid 367;
    property TypeNumberLeadingZero: TypeLeadingZeroConstants dispid 364;
    property TypeCurrencyShowSymbol: WordBool dispid 359;
    property TypePercentMin: Double dispid 368;
    property TypePercentMax: Double dispid 369;
    property TypeNumberDecPlaces: Smallint dispid 365;
    procedure DeleteCols(lCol: Integer; lNumCols: Integer); dispid 470;
    procedure DeleteRows(lRow: Integer; lNumRows: Integer); dispid 471;
    procedure ClipboardPaste; dispid 468;
    property TypeNumberNegStyle: TypeNumberNegStyleConstants dispid 366;
    procedure SetIteration(Iteration: WordBool; MaxIterations: Smallint; MaxChange: Double); dispid 266;
    procedure CopyRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer); dispid 469;
    procedure MoveRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer); dispid 497;
    function  SaveTabFileU(const FileName: WideString): WordBool; dispid 505;
    procedure Refresh; dispid -550;
    procedure SwapColRange(lCol: Integer; lCol2: Integer; lColDest: Integer); dispid 498;
    procedure CopyRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer); dispid 495;
    procedure MoveColRange(lCol: Integer; lCol2: Integer; lColDest: Integer); dispid 496;
    procedure AboutBox; dispid -552;
    function  ExportRangeToTextFileU(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                     const FileName: WideString; const CellDelim: WideString; 
                                     const ColDelim: WideString; const RowDelim: WideString; 
                                     Flags: ExportRangeToTextFileConstants; 
                                     const LogFile: WideString): WordBool; dispid 503;
    procedure CopyColRange(lCol: Integer; lCol2: Integer; lColDest: Integer); dispid 494;
    procedure ShowCell(lCol: Integer; lRow: Integer; nPosition: PositionConstants); dispid 483;
    function  ExportToTextFileU(const FileName: WideString; const CellDelim: WideString; 
                                const ColDelim: WideString; const RowDelim: WideString; 
                                Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool; dispid 504;
    property EventEnabled[Event: EVENTENABLEDConstants]: WordBool dispid 502;
    function  SearchRow(lRow: Integer; lColStart: Integer; lColEnd: Integer; 
                        const Text: WideString; SearchFlags: SearchFlagsConstants): Integer; dispid 501;
    function  ScriptGetFloat(Col: Integer; Row: Integer; var Value: OleVariant): WordBool; dispid 452;
    procedure ScriptGetCalTextOverride(var ShortDays: OleVariant; var LenShortDays: OleVariant; 
                                       var LongDays: OleVariant; var LenLongDays: OleVariant; 
                                       var ShortMonths: OleVariant; var LenShortMonths: OleVariant; 
                                       var LongMonths: OleVariant; var LenLongMonths: OleVariant; 
                                       var OkText: OleVariant; var LenOkText: OleVariant; 
                                       var CancelText: OleVariant; var LenCancelText: OleVariant); dispid 461;
    procedure PrintSheet(var Flags: OleVariant); dispid 462;
    function  ScriptGetInteger(Col: Integer; Row: Integer; var Value: OleVariant): WordBool; dispid 453;
    function  ScriptGetActionKey(Action: Smallint; var fShift: OleVariant; var fCtrl: OleVariant; 
                                 var Key: OleVariant): WordBool; dispid 450;
    function  ScriptGetOddEvenRowColor(var clrBackOdd: OleVariant; var clrForeOdd: OleVariant; 
                                       var clrBackEven: OleVariant; var clrForeEven: OleVariant): WordBool; dispid 451;
    procedure AddSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer); dispid 463;
    function  ScriptExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; 
                                           Row2: Integer; const Root: WideString; 
                                           const Collection: WideString; var Buff: OleVariant; 
                                           Flags: ExportToXMLConstants; const LogFile: WideString): WordBool; dispid 459;
    procedure SwapRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer); dispid 499;
    function  SearchCol(lCol: Integer; lRowStart: Integer; lRowEnd: Integer; 
                        const Text: WideString; SearchFlags: SearchFlagsConstants): Integer; dispid 500;
    function  ScriptOwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer; 
                                        Right: Integer; Bottom: Integer; var PageCount: OleVariant): Integer; dispid 460;
    function  ScriptGetCustomFunction(const FuncName: WideString; var MinArgs: OleVariant; 
                                      var MaxArgs: OleVariant; var Flags: OleVariant): WordBool; dispid 457;
    function  ScriptExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                      var Buff: OleVariant; Flags: ExportToXMLConstants; 
                                      const LogFile: WideString): WordBool; dispid 458;
    property DataSource: _DSpreadCursor dispid 35;
    property Enabled: WordBool dispid -514;
    property TypeFloatDecimalChar: Smallint dispid 161;
    property TypeFloatDecimalPlaces: Smallint dispid 162;
    property TypeFloatMax: Double dispid 163;
    property FileNum: Smallint dispid 50;
    property FloatDefCurrencyChar: Smallint dispid 51;
    property RowHeaderDisplay: HeaderDisplayConstants dispid 104;
    property FloatDefDecimalChar: Smallint dispid 52;
    property FloatDefSepChar: Smallint dispid 53;
    property TypeFloatMin: Double dispid 164;
    property TypeFloatMoney: WordBool dispid 165;
    property TypeFloatSeparator: WordBool dispid 166;
    property ColHeaderDisplay: HeaderDisplayConstants dispid 24;
    property TypeFloatSepChar: Smallint dispid 167;
    property TypeFloatCurrencyChar: Smallint dispid 160;
    property TypeIntegerMax: Integer dispid 169;
    property TypeIntegerMin: Integer dispid 170;
    property TypeIntegerSpinInc: Integer dispid 171;
    property CellBorderType: Smallint dispid 17;
    property CellBorderStyle: CellBorderStyleConstants dispid 16;
    property CellBorderColor: OLE_COLOR dispid 15;
    property CalcDependencies: WordBool dispid 14;
    property TypeEditLen: Integer dispid 157;
    property DisplayRowHeaders: WordBool dispid 45;
    property DisplayColHeaders: WordBool dispid 44;
    property SelModeSelCount: Integer dispid 119;
    property SortBy: SortByConstants dispid 126;
    property DestRow: Integer dispid 42;
    property Position: PositionConstants dispid 77;
    property TypeIntegerSpinWrap: WordBool dispid 172;
    property TypeTextAlignVert: TypeTextAlignVertConstants dispid 181;
    property Action: ActionConstants dispid 1;
    property MultiSelIndex: Integer dispid 73;
    property DestCol: Integer dispid 41;
    property MultiSelCount: Integer dispid 72;
  end;

// *********************************************************************//
// DispIntf:  _DSpreadEvents
// Flags:     (4096) Dispatchable
// GUID:      {41F841C5-AE16-11D5-8817-0050DA6EF5E5}
// *********************************************************************//
  _DSpreadEvents = dispinterface
    ['{41F841C5-AE16-11D5-8817-0050DA6EF5E5}']
    procedure Advance(AdvanceNext: WordBool); dispid 1;
    procedure BlockSelected(BlockCol: Integer; BlockRow: Integer; BlockCol2: Integer; 
                            BlockRow2: Integer); dispid 2;
    procedure ButtonClicked(Col: Integer; Row: Integer; ButtonDown: Smallint); dispid 3;
    procedure Change(Col: Integer; Row: Integer); dispid 4;
    procedure Click(Col: Integer; Row: Integer); dispid 5;
    procedure ColWidthChange(Col1: Integer; Col2: Integer); dispid 6;
    procedure CustomFunction(const FunctionName: WideString; ParameterCnt: Smallint; Col: Integer; 
                             Row: Integer; var Status: Smallint); dispid 7;
    procedure DataColConfig(Col: Integer; const DataField: WideString; DataType: Smallint); dispid 9;
    procedure DataFill(Col: Integer; Row: Integer; DataType: Smallint; fGetData: Smallint; 
                       var Cancel: Smallint); dispid 10;
    procedure DblClick(Col: Integer; Row: Integer); dispid 11;
    procedure DragDropBlock(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                            NewCol: Integer; NewRow: Integer; NewCol2: Integer; NewRow2: Integer; 
                            Overwrite: WordBool; var Action: Smallint; var DataOnly: WordBool; 
                            var Cancel: WordBool); dispid 12;
    procedure DrawItem(Col: Integer; Row: Integer; hDC: OLE_HANDLE; Left: Integer; Top: Integer; 
                       Right: Integer; Bottom: Integer; Style: Integer); dispid 13;
    procedure EditError(Col: Integer; Row: Integer; EditError: Smallint); dispid 14;
    procedure EditMode(Col: Integer; Row: Integer; Mode: Smallint; ChangeMade: WordBool); dispid 15;
    procedure EnterRow(Row: Integer; RowIsLast: Integer); dispid 16;
    procedure LeaveCell(Col: Integer; Row: Integer; NewCol: Integer; NewRow: Integer; 
                        var Cancel: WordBool); dispid 17;
    procedure LeaveRow(Row: Integer; RowWasLast: WordBool; RowChanged: WordBool; 
                       AllCellsHaveData: WordBool; NewRow: Integer; NewRowIsLast: Integer; 
                       var Cancel: WordBool); dispid 18;
    procedure PrintAbort(var Abort: WordBool); dispid 19;
    procedure QueryAdvance(AdvanceNext: WordBool; var Cancel: WordBool); dispid 20;
    procedure QueryData(Row: Integer; RowsNeeded: Integer; var RowsLoaded: Integer; 
                        Direction: Smallint; var AtTop: WordBool; var AtBottom: WordBool); dispid 21;
    procedure RightClick(ClickType: Smallint; Col: Integer; Row: Integer; MouseX: Integer; 
                         MouseY: Integer); dispid 22;
    procedure RowHeightChange(Row1: Integer; Row2: Integer); dispid 23;
    procedure SelChange(BlockCol: Integer; BlockRow: Integer; BlockCol2: Integer; 
                        BlockRow2: Integer; CurCol: Integer; CurRow: Integer); dispid 24;
    procedure TopLeftChange(OldLeft: Integer; OldTop: Integer; NewLeft: Integer; NewTop: Integer); dispid 25;
    procedure UserFormulaEntered(Col: Integer; Row: Integer); dispid 26;
    procedure VirtualClearData(Row: Integer; RowsBeingCleared: Integer); dispid 27;
    procedure PrintMsgBox(const Text: WideString; PrintID: Integer; OpenMsgBox: WordBool; 
                          var Processed: WordBool); dispid 28;
    procedure ComboCloseUp(Col: Integer; Row: Integer; SelChange: Smallint); dispid 29;
    procedure ComboDropDown(Col: Integer; Row: Integer); dispid 30;
    procedure ComboSelChange(Col: Integer; Row: Integer); dispid 31;
    procedure TextTipFetch(Col: Integer; Row: Integer; var MultiLine: Smallint; 
                           var TipWidth: Integer; var TipText: WideString; var ShowTip: WordBool); dispid 32;
    procedure EditChange(Col: Integer; Row: Integer); dispid 33;
    procedure OLECompleteDrag(var Effect: Integer); dispid 34;
    procedure OLEDragDrop(var Data: fpDataObject; var Effect: Integer; var Button: Smallint; 
                          var Shift: Smallint; var x: Single; var y: Single); dispid 35;
    procedure OLEDragOver(var Data: fpDataObject; var Effect: Integer; var Button: Smallint; 
                          var Shift: Smallint; var x: Single; var y: Single; var State: Smallint); dispid 36;
    procedure OLEGiveFeedback(var Effect: Integer; var DefaultCursors: WordBool); dispid 37;
    procedure OLESetData(var Data: fpDataObject; var DataFormat: Smallint); dispid 38;
    procedure OLEStartDrag(var Data: fpDataObject; var AllowedEffects: Integer); dispid 39;
    procedure BeforeUserSort(Col: Integer; State: Integer; var DefaultAction: Integer); dispid 40;
    procedure AfterUserSort(Col: Integer); dispid 41;
    procedure KeyDown(var KeyCode: Smallint; Shift: Smallint); dispid -602;
    procedure KeyPress(var KeyAscii: Smallint); dispid -603;
    procedure KeyUp(var KeyCode: Smallint; Shift: Smallint); dispid -604;
    procedure MouseDown(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -605;
    procedure MouseMove(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -606;
    procedure MouseUp(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -607;
    procedure ScriptCustomFunction(const FunctionName: WideString; ParameterCnt: Smallint; 
                                   Col: Integer; Row: Integer; var Status: OleVariant); dispid 101;
    procedure ScriptDataFill(Col: Integer; Row: Integer; DataType: Smallint; fGetData: Smallint; 
                             var Cancel: OleVariant); dispid 102;
    procedure ScriptDragDropBlock(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                  NewCol: Integer; NewRow: Integer; NewCol2: Integer; 
                                  NewRow2: Integer; Overwrite: WordBool; var Action: OleVariant; 
                                  var DataOnly: OleVariant; var Cancel: OleVariant); dispid 103;
    procedure ScriptLeaveCell(Col: Integer; Row: Integer; NewCol: Integer; NewRow: Integer; 
                              var Cancel: OleVariant); dispid 104;
    procedure ScriptLeaveRow(Row: Integer; RowWasLast: WordBool; RowChanged: WordBool; 
                             AllCellsHaveData: WordBool; NewRow: Integer; NewRowIsLast: Integer; 
                             var Cancel: OleVariant); dispid 105;
    procedure ScriptPrintAbort(var Abort: OleVariant); dispid 106;
    procedure ScriptQueryAdvance(AdvanceNext: WordBool; var Cancel: OleVariant); dispid 107;
    procedure ScriptQueryData(Row: Integer; RowsNeeded: Integer; var RowsLoaded: OleVariant; 
                              Direction: Smallint; var AtTop: OleVariant; var AtBottom: OleVariant); dispid 108;
    procedure ScriptPrintMsgBox(const Text: WideString; PrintID: Integer; OpenMsgBox: WordBool; 
                                var Processed: OleVariant); dispid 109;
    procedure ScriptTextTipFetch(Col: Integer; Row: Integer; var MultiLine: OleVariant; 
                                 var TipWidth: OleVariant; var TipText: OleVariant; 
                                 var ShowTip: OleVariant); dispid 110;
    procedure ScriptOLECompleteDrag(var Effect: OleVariant); dispid 111;
    procedure ScriptOLEDragDrop(var Data: OleVariant; var Effect: OleVariant; 
                                var Button: OleVariant; var Shift: OleVariant; var x: OleVariant; 
                                var y: OleVariant); dispid 112;
    procedure ScriptOLEDragOver(var Data: OleVariant; var Effect: OleVariant; 
                                var Button: OleVariant; var Shift: OleVariant; var x: OleVariant; 
                                var y: OleVariant; var State: OleVariant); dispid 113;
    procedure ScriptOLEGiveFeedback(var Effect: OleVariant; var DefaultCursors: OleVariant); dispid 114;
    procedure ScriptOLEStartDrag(var Data: OleVariant; var AllowedEffects: OleVariant); dispid 115;
    procedure ScriptBeforeUserSort(Col: Integer; State: Integer; var DefaultAction: OleVariant); dispid 117;
    procedure ScriptKeyDown(var KeyCode: OleVariant; Shift: Smallint); dispid 118;
    procedure ScriptKeyPress(var KeyAscii: OleVariant); dispid 119;
    procedure ScriptKeyUp(var KeyCode: OleVariant; Shift: Smallint); dispid 120;
  end;

// *********************************************************************//
// DispIntf:  _DSpreadPreview
// Flags:     (4112) Hidden Dispatchable
// GUID:      {41F841C9-AE16-11D5-8817-0050DA6EF5E5}
// *********************************************************************//
  _DSpreadPreview = dispinterface
    ['{41F841C9-AE16-11D5-8817-0050DA6EF5E5}']
    property AllowUserZoom: WordBool dispid 1;
    property GrayAreaColor: OLE_COLOR dispid 2;
    property GrayAreaMarginH: Integer dispid 3;
    property GrayAreaMarginType: PVGrayAreaMarginTypeConstants dispid 4;
    property GrayAreaMarginV: Integer dispid 5;
    property hWndSpread: Integer dispid 6;
    property PageBorderColor: OLE_COLOR dispid 7;
    property PageBorderWidth: Smallint dispid 8;
    property PageShadowWidth: Smallint dispid 10;
    property PageShadowColor: OLE_COLOR dispid 9;
    property PageViewPercentage: Smallint dispid 11;
    property PageViewType: PVPageViewTypeConstants dispid 12;
    property ScrollBarH: PVScrollBarConstants dispid 13;
    property ScrollBarV: PVScrollBarConstants dispid 14;
    property ScrollIncH: Integer dispid 15;
    property ScrollIncV: Integer dispid 16;
    property PageMultiCntH: Smallint dispid 17;
    property PageMultiCntV: Smallint dispid 18;
    property PageGutterH: Integer dispid 19;
    property PageGutterV: Integer dispid 20;
    property ZoomState: PVZoomStateConstants dispid 21;
    property PageCurrent: Integer dispid 22;
    property PagesPerScreen: Smallint dispid 23;
    property PagePercentageActual: Smallint dispid 24;
    property MousePointer: MousePointerConstants dispid 50;
    property MouseIcon: IPictureDisp dispid 51;
    property OLEDropMode: OLEDropModeConstants dispid 52;
    property ScriptEnhanced: WordBool dispid 54;
    procedure OLEDrag; dispid 53;
    procedure AboutBox; dispid -552;
    property BorderStyle: BorderStyleConstants dispid -504;
    property Enabled: WordBool dispid -514;
  end;

// *********************************************************************//
// DispIntf:  _DSpreadPreviewEvents
// Flags:     (4096) Dispatchable
// GUID:      {41F841CB-AE16-11D5-8817-0050DA6EF5E5}
// *********************************************************************//
  _DSpreadPreviewEvents = dispinterface
    ['{41F841CB-AE16-11D5-8817-0050DA6EF5E5}']
    procedure PageChange(Page: Integer); dispid 1;
    procedure Zoom; dispid 2;
    procedure OLECompleteDrag(var Effect: Integer); dispid 3;
    procedure OLEDragDrop(var Data: fpDataObject; var Effect: Integer; var Button: Smallint; 
                          var Shift: Smallint; var x: Single; var y: Single); dispid 4;
    procedure OLEDragOver(var Data: fpDataObject; var Effect: Integer; var Button: Smallint; 
                          var Shift: Smallint; var x: Single; var y: Single; var State: Smallint); dispid 5;
    procedure OLEGiveFeedback(var Effect: Integer; var DefaultCursors: WordBool); dispid 6;
    procedure OLESetData(var Data: fpDataObject; var DataFormat: Smallint); dispid 7;
    procedure OLEStartDrag(var Data: fpDataObject; var AllowedEffects: Integer); dispid 8;
    procedure ScriptOLECompleteDrag(var Effect: OleVariant); dispid 9;
    procedure ScriptOLEDragDrop(var Data: OleVariant; var Effect: OleVariant; 
                                var Button: OleVariant; var Shift: OleVariant; var x: OleVariant; 
                                var y: OleVariant); dispid 10;
    procedure ScriptOLEDragOver(var Data: OleVariant; var Effect: OleVariant; 
                                var Button: OleVariant; var Shift: OleVariant; var x: OleVariant; 
                                var y: OleVariant; var State: OleVariant); dispid 11;
    procedure ScriptOLEGiveFeedback(var Effect: OleVariant; var DefaultCursors: OleVariant); dispid 12;
    procedure ScriptOLEStartDrag(var Data: OleVariant; var AllowedEffects: OleVariant); dispid 13;
    procedure Click; dispid -600;
    procedure DblClick; dispid -601;
    procedure KeyDown(var KeyCode: Smallint; Shift: Smallint); dispid -602;
    procedure KeyPress(var KeyAscii: Smallint); dispid -603;
    procedure KeyUp(var KeyCode: Smallint; Shift: Smallint); dispid -604;
    procedure MouseDown(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -605;
    procedure MouseMove(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -606;
    procedure MouseUp(Button: Smallint; Shift: Smallint; x: OLE_XPOS_PIXELS; y: OLE_YPOS_PIXELS); dispid -607;
  end;

// *********************************************************************//
// The Class CofpDataObjectFiles provides a Create and CreateRemote method to          
// create instances of the default interface IfpDataObjectFiles exposed by              
// the CoClass fpDataObjectFiles. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CofpDataObjectFiles = class
    class function Create: IfpDataObjectFiles;
    class function CreateRemote(const MachineName: string): IfpDataObjectFiles;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TfpDataObjectFiles
// Help String      : A collection of strings which is the type of the Files property on the fpDataObject object.
// Default Interface: IfpDataObjectFiles
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TfpDataObjectFilesProperties= class;
{$ENDIF}
  TfpDataObjectFiles = class(TOleServer)
  private
    FIntf:        IfpDataObjectFiles;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TfpDataObjectFilesProperties;
    function      GetServerProperties: TfpDataObjectFilesProperties;
{$ENDIF}
    function      GetDefaultInterface: IfpDataObjectFiles;
  protected
    procedure InitServerData; override;
    function  Get_Item(lIndex: Integer): WideString;
    function  Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IfpDataObjectFiles);
    procedure Disconnect; override;
    procedure Add(const bstrFilename: WideString); overload;
    procedure Add(const bstrFilename: WideString; vIndex: OleVariant); overload;
    procedure Clear;
    procedure Remove(vIndex: OleVariant);
    property  DefaultInterface: IfpDataObjectFiles read GetDefaultInterface;
    property Item[lIndex: Integer]: WideString read Get_Item; default;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TfpDataObjectFilesProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TfpDataObjectFiles
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TfpDataObjectFilesProperties = class(TPersistent)
  private
    FServer:    TfpDataObjectFiles;
    function    GetDefaultInterface: IfpDataObjectFiles;
    constructor Create(AServer: TfpDataObjectFiles);
  protected
    function  Get_Item(lIndex: Integer): WideString;
    function  Get_Count: Integer;
  public
    property DefaultInterface: IfpDataObjectFiles read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CofpDataObject provides a Create and CreateRemote method to          
// create instances of the default interface IfpDataObject exposed by              
// the CoClass fpDataObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CofpDataObject = class
    class function Create: IfpDataObject;
    class function CreateRemote(const MachineName: string): IfpDataObject;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TfpDataObject
// Help String      : A container for data being transferred from a source to a target in an OLE drag/drop operation.
// Default Interface: IfpDataObject
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TfpDataObjectProperties= class;
{$ENDIF}
  TfpDataObject = class(TOleServer)
  private
    FIntf:        IfpDataObject;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TfpDataObjectProperties;
    function      GetServerProperties: TfpDataObjectProperties;
{$ENDIF}
    function      GetDefaultInterface: IfpDataObject;
  protected
    procedure InitServerData; override;
    function  Get_Files: IfpDataObjectFiles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IfpDataObject);
    procedure Disconnect; override;
    procedure Clear;
    function  GetData(nFormat: Smallint): OleVariant;
    function  GetFormat(nFormat: Smallint): WordBool;
    procedure SetData; overload;
    procedure SetData(vValue: OleVariant); overload;
    procedure SetData(vValue: OleVariant; vFormat: OleVariant); overload;
    property  DefaultInterface: IfpDataObject read GetDefaultInterface;
    property Files: IfpDataObjectFiles read Get_Files;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TfpDataObjectProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TfpDataObject
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TfpDataObjectProperties = class(TPersistent)
  private
    FServer:    TfpDataObject;
    function    GetDefaultInterface: IfpDataObject;
    constructor Create(AServer: TfpDataObject);
  protected
    function  Get_Files: IfpDataObjectFiles;
  public
    property DefaultInterface: IfpDataObject read GetDefaultInterface;
  published
  end;
{$ENDIF}



// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TvaSpread
// Help String      : FarPoint Spread 6.0
// Default Interface: _DSpreadSheet
// Def. Intf. DISP? : Yes
// Event   Interface: _DSpreadEvents
// TypeFlags        : (38) CanCreate Licensed Control
// *********************************************************************//
  TvaSpreadAdvance = procedure(Sender: TObject; AdvanceNext: WordBool) of object;
  TvaSpreadBlockSelected = procedure(Sender: TObject; BlockCol: Integer; BlockRow: Integer; 
                                                      BlockCol2: Integer; BlockRow2: Integer) of object;
  TvaSpreadButtonClicked = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                      ButtonDown: Smallint) of object;
  TvaSpreadChange = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadClick = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadColWidthChange = procedure(Sender: TObject; Col1: Integer; Col2: Integer) of object;
  TvaSpreadCustomFunction = procedure(Sender: TObject; const FunctionName: WideString; 
                                                       ParameterCnt: Smallint; Col: Integer; 
                                                       Row: Integer; var Status: Smallint) of object;
  TvaSpreadDataColConfig = procedure(Sender: TObject; Col: Integer; const DataField: WideString; 
                                                      DataType: Smallint) of object;
  TvaSpreadDataFill = procedure(Sender: TObject; Col: Integer; Row: Integer; DataType: Smallint; 
                                                 fGetData: Smallint; var Cancel: Smallint) of object;
  TvaSpreadDblClick = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadDragDropBlock = procedure(Sender: TObject; Col: Integer; Row: Integer; Col2: Integer; 
                                                      Row2: Integer; NewCol: Integer; 
                                                      NewRow: Integer; NewCol2: Integer; 
                                                      NewRow2: Integer; Overwrite: WordBool; 
                                                      var Action: Smallint; var DataOnly: WordBool; 
                                                      var Cancel: WordBool) of object;
  TvaSpreadDrawItem = procedure(Sender: TObject; Col: Integer; Row: Integer; hDC: OLE_HANDLE; 
                                                 Left: Integer; Top: Integer; Right: Integer; 
                                                 Bottom: Integer; Style: Integer) of object;
  TvaSpreadEditError = procedure(Sender: TObject; Col: Integer; Row: Integer; EditError: Smallint) of object;
  TvaSpreadEditMode = procedure(Sender: TObject; Col: Integer; Row: Integer; Mode: Smallint; 
                                                 ChangeMade: WordBool) of object;
  TvaSpreadEnterRow = procedure(Sender: TObject; Row: Integer; RowIsLast: Integer) of object;
  TvaSpreadLeaveCell = procedure(Sender: TObject; Col: Integer; Row: Integer; NewCol: Integer; 
                                                  NewRow: Integer; var Cancel: WordBool) of object;
  TvaSpreadLeaveRow = procedure(Sender: TObject; Row: Integer; RowWasLast: WordBool; 
                                                 RowChanged: WordBool; AllCellsHaveData: WordBool; 
                                                 NewRow: Integer; NewRowIsLast: Integer; 
                                                 var Cancel: WordBool) of object;
  TvaSpreadPrintAbort = procedure(Sender: TObject; var Abort: WordBool) of object;
  TvaSpreadQueryAdvance = procedure(Sender: TObject; AdvanceNext: WordBool; var Cancel: WordBool) of object;
  TvaSpreadQueryData = procedure(Sender: TObject; Row: Integer; RowsNeeded: Integer; 
                                                  var RowsLoaded: Integer; Direction: Smallint; 
                                                  var AtTop: WordBool; var AtBottom: WordBool) of object;
  TvaSpreadRightClick = procedure(Sender: TObject; ClickType: Smallint; Col: Integer; Row: Integer; 
                                                   MouseX: Integer; MouseY: Integer) of object;
  TvaSpreadRowHeightChange = procedure(Sender: TObject; Row1: Integer; Row2: Integer) of object;
  TvaSpreadSelChange = procedure(Sender: TObject; BlockCol: Integer; BlockRow: Integer; 
                                                  BlockCol2: Integer; BlockRow2: Integer; 
                                                  CurCol: Integer; CurRow: Integer) of object;
  TvaSpreadTopLeftChange = procedure(Sender: TObject; OldLeft: Integer; OldTop: Integer; 
                                                      NewLeft: Integer; NewTop: Integer) of object;
  TvaSpreadUserFormulaEntered = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadVirtualClearData = procedure(Sender: TObject; Row: Integer; RowsBeingCleared: Integer) of object;
  TvaSpreadPrintMsgBox = procedure(Sender: TObject; const Text: WideString; PrintID: Integer; 
                                                    OpenMsgBox: WordBool; var Processed: WordBool) of object;
  TvaSpreadComboCloseUp = procedure(Sender: TObject; Col: Integer; Row: Integer; SelChange: Smallint) of object;
  TvaSpreadComboDropDown = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadComboSelChange = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadTextTipFetch = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                     var MultiLine: Smallint; 
                                                     var TipWidth: Integer; 
                                                     var TipText: WideString; var ShowTip: WordBool) of object;
  TvaSpreadEditChange = procedure(Sender: TObject; Col: Integer; Row: Integer) of object;
  TvaSpreadOLECompleteDrag = procedure(Sender: TObject; var Effect: Integer) of object;
  TvaSpreadOLEDragDrop = procedure(Sender: TObject; var Data: fpDataObject; var Effect: Integer; 
                                                    var Button: Smallint; var Shift: Smallint; 
                                                    var x: Single; var y: Single) of object;
  TvaSpreadOLEDragOver = procedure(Sender: TObject; var Data: fpDataObject; var Effect: Integer; 
                                                    var Button: Smallint; var Shift: Smallint; 
                                                    var x: Single; var y: Single; 
                                                    var State: Smallint) of object;
  TvaSpreadOLEGiveFeedback = procedure(Sender: TObject; var Effect: Integer; 
                                                        var DefaultCursors: WordBool) of object;
  TvaSpreadOLESetData = procedure(Sender: TObject; var Data: fpDataObject; var DataFormat: Smallint) of object;
  TvaSpreadOLEStartDrag = procedure(Sender: TObject; var Data: fpDataObject; 
                                                     var AllowedEffects: Integer) of object;
  TvaSpreadBeforeUserSort = procedure(Sender: TObject; Col: Integer; State: Integer; 
                                                       var DefaultAction: Integer) of object;
  TvaSpreadAfterUserSort = procedure(Sender: TObject; Col: Integer) of object;
  TvaSpreadScriptCustomFunction = procedure(Sender: TObject; const FunctionName: WideString; 
                                                             ParameterCnt: Smallint; Col: Integer; 
                                                             Row: Integer; var Status: OleVariant) of object;
  TvaSpreadScriptDataFill = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                       DataType: Smallint; fGetData: Smallint; 
                                                       var Cancel: OleVariant) of object;
  TvaSpreadScriptDragDropBlock = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                            Col2: Integer; Row2: Integer; 
                                                            NewCol: Integer; NewRow: Integer; 
                                                            NewCol2: Integer; NewRow2: Integer; 
                                                            Overwrite: WordBool; 
                                                            var Action: OleVariant; 
                                                            var DataOnly: OleVariant; 
                                                            var Cancel: OleVariant) of object;
  TvaSpreadScriptLeaveCell = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                        NewCol: Integer; NewRow: Integer; 
                                                        var Cancel: OleVariant) of object;
  TvaSpreadScriptLeaveRow = procedure(Sender: TObject; Row: Integer; RowWasLast: WordBool; 
                                                       RowChanged: WordBool; 
                                                       AllCellsHaveData: WordBool; NewRow: Integer; 
                                                       NewRowIsLast: Integer; var Cancel: OleVariant) of object;
  TvaSpreadScriptPrintAbort = procedure(Sender: TObject; var Abort: OleVariant) of object;
  TvaSpreadScriptQueryAdvance = procedure(Sender: TObject; AdvanceNext: WordBool; 
                                                           var Cancel: OleVariant) of object;
  TvaSpreadScriptQueryData = procedure(Sender: TObject; Row: Integer; RowsNeeded: Integer; 
                                                        var RowsLoaded: OleVariant; 
                                                        Direction: Smallint; var AtTop: OleVariant; 
                                                        var AtBottom: OleVariant) of object;
  TvaSpreadScriptPrintMsgBox = procedure(Sender: TObject; const Text: WideString; PrintID: Integer; 
                                                          OpenMsgBox: WordBool; 
                                                          var Processed: OleVariant) of object;
  TvaSpreadScriptTextTipFetch = procedure(Sender: TObject; Col: Integer; Row: Integer; 
                                                           var MultiLine: OleVariant; 
                                                           var TipWidth: OleVariant; 
                                                           var TipText: OleVariant; 
                                                           var ShowTip: OleVariant) of object;
  TvaSpreadScriptOLECompleteDrag = procedure(Sender: TObject; var Effect: OleVariant) of object;
  TvaSpreadScriptOLEDragDrop = procedure(Sender: TObject; var Data: OleVariant; 
                                                          var Effect: OleVariant; 
                                                          var Button: OleVariant; 
                                                          var Shift: OleVariant; var x: OleVariant; 
                                                          var y: OleVariant) of object;
  TvaSpreadScriptOLEDragOver = procedure(Sender: TObject; var Data: OleVariant; 
                                                          var Effect: OleVariant; 
                                                          var Button: OleVariant; 
                                                          var Shift: OleVariant; var x: OleVariant; 
                                                          var y: OleVariant; var State: OleVariant) of object;
  TvaSpreadScriptOLEGiveFeedback = procedure(Sender: TObject; var Effect: OleVariant; 
                                                              var DefaultCursors: OleVariant) of object;
  TvaSpreadScriptOLEStartDrag = procedure(Sender: TObject; var Data: OleVariant; 
                                                           var AllowedEffects: OleVariant) of object;
  TvaSpreadScriptBeforeUserSort = procedure(Sender: TObject; Col: Integer; State: Integer; 
                                                             var DefaultAction: OleVariant) of object;
  TvaSpreadScriptKeyDown = procedure(Sender: TObject; var KeyCode: OleVariant; Shift: Smallint) of object;
  TvaSpreadScriptKeyPress = procedure(Sender: TObject; var KeyAscii: OleVariant) of object;
  TvaSpreadScriptKeyUp = procedure(Sender: TObject; var KeyCode: OleVariant; Shift: Smallint) of object;

  TvaSpread = class(TOleControl)
  private
    FOnAdvance: TvaSpreadAdvance;
    FOnBlockSelected: TvaSpreadBlockSelected;
    FOnButtonClicked: TvaSpreadButtonClicked;
    FOnChange: TvaSpreadChange;
    FOnClick: TvaSpreadClick;
    FOnColWidthChange: TvaSpreadColWidthChange;
    FOnCustomFunction: TvaSpreadCustomFunction;
    FOnDataColConfig: TvaSpreadDataColConfig;
    FOnDataFill: TvaSpreadDataFill;
    FOnDblClick: TvaSpreadDblClick;
    FOnDragDropBlock: TvaSpreadDragDropBlock;
    FOnDrawItem: TvaSpreadDrawItem;
    FOnEditError: TvaSpreadEditError;
    FOnEditMode: TvaSpreadEditMode;
    FOnEnterRow: TvaSpreadEnterRow;
    FOnLeaveCell: TvaSpreadLeaveCell;
    FOnLeaveRow: TvaSpreadLeaveRow;
    FOnPrintAbort: TvaSpreadPrintAbort;
    FOnQueryAdvance: TvaSpreadQueryAdvance;
    FOnQueryData: TvaSpreadQueryData;
    FOnRightClick: TvaSpreadRightClick;
    FOnRowHeightChange: TvaSpreadRowHeightChange;
    FOnSelChange: TvaSpreadSelChange;
    FOnTopLeftChange: TvaSpreadTopLeftChange;
    FOnUserFormulaEntered: TvaSpreadUserFormulaEntered;
    FOnVirtualClearData: TvaSpreadVirtualClearData;
    FOnPrintMsgBox: TvaSpreadPrintMsgBox;
    FOnComboCloseUp: TvaSpreadComboCloseUp;
    FOnComboDropDown: TvaSpreadComboDropDown;
    FOnComboSelChange: TvaSpreadComboSelChange;
    FOnTextTipFetch: TvaSpreadTextTipFetch;
    FOnEditChange: TvaSpreadEditChange;
    FOnOLECompleteDrag: TvaSpreadOLECompleteDrag;
    FOnOLEDragDrop: TvaSpreadOLEDragDrop;
    FOnOLEDragOver: TvaSpreadOLEDragOver;
    FOnOLEGiveFeedback: TvaSpreadOLEGiveFeedback;
    FOnOLESetData: TvaSpreadOLESetData;
    FOnOLEStartDrag: TvaSpreadOLEStartDrag;
    FOnBeforeUserSort: TvaSpreadBeforeUserSort;
    FOnAfterUserSort: TvaSpreadAfterUserSort;
    FOnScriptCustomFunction: TvaSpreadScriptCustomFunction;
    FOnScriptDataFill: TvaSpreadScriptDataFill;
    FOnScriptDragDropBlock: TvaSpreadScriptDragDropBlock;
    FOnScriptLeaveCell: TvaSpreadScriptLeaveCell;
    FOnScriptLeaveRow: TvaSpreadScriptLeaveRow;
    FOnScriptPrintAbort: TvaSpreadScriptPrintAbort;
    FOnScriptQueryAdvance: TvaSpreadScriptQueryAdvance;
    FOnScriptQueryData: TvaSpreadScriptQueryData;
    FOnScriptPrintMsgBox: TvaSpreadScriptPrintMsgBox;
    FOnScriptTextTipFetch: TvaSpreadScriptTextTipFetch;
    FOnScriptOLECompleteDrag: TvaSpreadScriptOLECompleteDrag;
    FOnScriptOLEDragDrop: TvaSpreadScriptOLEDragDrop;
    FOnScriptOLEDragOver: TvaSpreadScriptOLEDragOver;
    FOnScriptOLEGiveFeedback: TvaSpreadScriptOLEGiveFeedback;
    FOnScriptOLEStartDrag: TvaSpreadScriptOLEStartDrag;
    FOnScriptBeforeUserSort: TvaSpreadScriptBeforeUserSort;
    FOnScriptKeyDown: TvaSpreadScriptKeyDown;
    FOnScriptKeyPress: TvaSpreadScriptKeyPress;
    FOnScriptKeyUp: TvaSpreadScriptKeyUp;
    FIntf: _DSpreadSheet;
    function  GetControlInterface: _DSpreadSheet;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function  Get_ColUserSortIndicator(lCol: Integer): ColUserSortIndicatorConstants;
    procedure Set_ColUserSortIndicator(lCol: Integer; Param2: ColUserSortIndicatorConstants);
    function  Get_MaxTextRowHeight(lRow: Integer): Double;
    procedure Set_ColWidth(lCol: Integer; Param2: Double);
    function  Get_TypeCheckPicture(Index: Smallint): IPictureDisp;
    function  Get_MaxTextColWidth(lCol: Integer): Double;
    procedure Set_MaxTextColWidth(lCol: Integer; Param2: Double);
    procedure Set_SortKeyOrder(nIndex: Smallint; Param2: SortKeyOrderConstants);
    function  Get_SortKey(nIndex: Smallint): Integer;
    procedure Set_MaxTextRowHeight(lRow: Integer; Param2: Double);
    function  Get_ColWidth(lCol: Integer): Double;
    procedure Set_SortKey(nIndex: Smallint; Param2: Integer);
    function  Get_SortKeyOrder(nIndex: Smallint): SortKeyOrderConstants;
    procedure Set_TypeCheckPicture(Index: Smallint; const Param2: IPictureDisp);
    function  Get_RowHeight(lRow: Integer): Double;
    procedure Set_RowHeight(lRow: Integer; Param2: Double);
    function  Get_EventEnabled(Event: EVENTENABLEDConstants): WordBool;
    procedure Set_EventEnabled(Event: EVENTENABLEDConstants; Param2: WordBool);
    function Get_DataSource: _DSpreadCursor;
    procedure Set_DataSource(const Value: _DSpreadCursor);
  public
    function  OwnerPrintDraw(hDC: OLE_HANDLE; Left: Integer; Top: Integer; Right: Integer; 
                             Bottom: Integer; Page: Smallint): WordBool;
    function  OwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer; Right: Integer; 
                                  Bottom: Integer; var PageCount: Smallint): WordBool;
    function  ExportRangeToTextFile(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                    const FileName: WideString; const CellDelim: WideString; 
                                    const ColDelim: WideString; const RowDelim: WideString; 
                                    Flags: ExportRangeToTextFileConstants; const LogFile: WideString): WordBool;
    function  ExportToTextFile(const FileName: WideString; const CellDelim: WideString; 
                               const ColDelim: WideString; const RowDelim: WideString; 
                               Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool;
    function  ExportRangeToXML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                               const FileName: WideString; const Root: WideString; 
                               const Collection: WideString; Flags: ExportToXMLConstants; 
                               const LogFile: WideString): WordBool;
    function  ExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                var Buff: WideString; Flags: ExportToXMLConstants; 
                                const LogFile: WideString): WordBool;
    function  ExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                     const Root: WideString; const Collection: WideString; 
                                     var Buff: WideString; Flags: ExportToXMLConstants; 
                                     const LogFile: WideString): WordBool;
    procedure ScriptGetFirstValidCell(var Col: OleVariant; var Row: OleVariant);
    function  ScriptGetIteration(var MaxIterations: OleVariant; var MaxChange: OleVariant): WordBool;
    procedure ScriptCFGetCellParam(Param: Smallint; var Col: OleVariant; var Row: OleVariant);
    function  ExportToXML(const FileName: WideString; const Root: WideString; 
                          const Collection: WideString; Flags: ExportToXMLConstants; 
                          const LogFile: WideString): WordBool;
    function  LoadTextFile(const FileName: WideString; const CellDelim: WideString; 
                           const ColDelim: WideString; const RowDelim: WideString; 
                           Flags: LoadTextFileConstants; const LogFile: WideString): WordBool;
    procedure SetCalTextOverride(const ShortDays: WideString; const LongDays: WideString; 
                                 const ShortMonths: WideString; const LongMonths: WideString; 
                                 const OkText: WideString; const CancelText: WideString);
    procedure GetCalTextOverride(var ShortDays: WideString; var LenShortDays: Smallint; 
                                 var LongDays: WideString; var LenLongDays: Smallint; 
                                 var ShortMonths: WideString; var LenShortMonths: Smallint; 
                                 var LongMonths: WideString; var LenLongMonths: Smallint; 
                                 var OkText: WideString; var LenOkText: Smallint; 
                                 var CancelText: WideString; var LenCancelText: Smallint);
    function  IsFetchCellNote: WordBool;
    function  GetItemData: Integer;
    function  GetIteration(var MaxIterations: Smallint; var MaxChange: Double): WordBool;
    function  GetDataFillData(var Var_: OleVariant; VarType: Smallint): WordBool;
    procedure GetFirstValidCell(var Col: Integer; var Row: Integer);
    function  CFGetStringParam(Param: Smallint): WideString;
    procedure CFGetCellParam(Param: Smallint; var Col: Integer; var Row: Integer);
    function  AddCustomFunction(const FunctionName: WideString; ParameterCnt: Smallint): WordBool;
    function  CFGetDoubleParam(Param: Smallint): Double;
    function  CFGetDoubleParamExt(Param: Smallint; var ParamValue: Double): Double;
    function  CFGetLongParam(Param: Smallint): Integer;
    function  AddCustomFunctionExt(const FunctionName: WideString; MinParamCnt: Smallint; 
                                   MaxParamCnt: Smallint; Flags: Integer): WordBool;
    procedure CFSetResult(Var_: OleVariant);
    function  CFGetParamInfo(Param: Smallint; var Type_: Smallint; var Status: Smallint): WordBool;
    procedure CFGetRangeParam(Param: Smallint; var Col: Integer; var Row: Integer; 
                              var Col2: Integer; var Row2: Integer);
    function  ColNumberToLetter(HeaderNumber: Integer): WideString;
    procedure ColWidthToTwips(Width: Single; var Twips: Integer);
    procedure GetBottomRightCell(var Col: Integer; var Row: Integer);
    function  GetColItemData(Col: Integer): Integer;
    function  QueryCustomName(const Name: WideString): WideString;
    function  GetCustomName(const Name: WideString): WideString;
    function  SaveToFile(const FileName: WideString; DataOnly: WordBool): WordBool;
    function  SetCellDirtyFlag(Col: Integer; Row: Integer; Dirty: WordBool): WordBool;
    function  GetRowItemData(Row: Integer): Integer;
    procedure GetClientArea(var Width: Integer; var Height: Integer);
    procedure GetLastValidCell(var Col: Integer; var Row: Integer);
    function  GetMultiSelItem(SelPrev: Integer): Integer;
    function  GetRefStyle: Smallint;
    function  GetCellDirtyFlag(Col: Integer; Row: Integer): WordBool;
    procedure GetCellFromScreenCoord(var Col: Integer; var Row: Integer; x: Integer; y: Integer);
    function  GetCellPos(Col: Integer; Row: Integer; var x: Integer; var y: Integer; 
                         var Width: Integer; var Height: Integer): WordBool;
    function  SaveTabFile(const FileName: WideString): WordBool;
    function  LoadTabFile(const FileName: WideString): WordBool;
    procedure RowHeightToTwips(Row: Integer; Height: Single; var Twips: Integer);
    function  IsVisible(Col: Integer; Row: Integer; Partial: WordBool): WordBool;
    function  LoadFromFile(const FileName: WideString): WordBool;
    function  GetText(Col: Integer; Row: Integer; var Var_: OleVariant): WordBool;
    function  SetDataFillData(Var_: OleVariant): WordBool;
    procedure SetItemData(Value: Integer);
    procedure SetColItemData(Col: Integer; Value: Integer);
    function  IsCellSelected(Col: Integer; Row: Integer): WordBool;
    function  IsFormulaValid(const Formula: WideString): WordBool;
    function  SetCustomName(const Name: WideString; const Value: WideString): WordBool;
    function  SetTextTipAppearance(const FontName: WideString; FontSize: Smallint; 
                                   FontBold: WordBool; FontItalic: WordBool; BackColor: Integer; 
                                   ForeColor: Integer): WordBool;
    function  ExportToHTML(const FileName: WideString; AppendFlag: WordBool; 
                           const LogFile: WideString): WordBool;
    function  ExportRangeToHTML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                const FileName: WideString; AppendFlag: WordBool; 
                                const LogFile: WideString): WordBool;
    function  IsExcelFile(const FileName: WideString): Smallint;
    function  GetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                var ListCount: Smallint; const LogFileName: WideString; 
                                var WorkbookHandle: Smallint; Replace: WordBool): WordBool;
    function  GetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool;
    function  RemoveCustomFunction(const FuncName: WideString): WordBool;
    function  ImportExcelSheet(WorkbookHandle: Smallint; Sheet: OleVariant): WordBool;
    function  ExportToExcel(const FileName: WideString; const SheetName: WideString; 
                            const LogFileName: WideString): WordBool;
    function  EnumCustomFunction(const PrevFuncName: WideString; var FuncName: WideString): WordBool;
    function  ReCalcCell(Col: Integer; Row: Integer): WordBool;
    procedure OLEDrag;
    function  GetCustomFunction(const FuncName: WideString; var MinArgs: Smallint; 
                                var MaxArgs: Smallint; var Flags: Integer): WordBool;
    function  SetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool;
    procedure Reset;
    procedure SetActiveCell(lCol: Integer; lRow: Integer);
    procedure SetCellBorder(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                            nIndex: Smallint; crColor: OLE_COLOR; nStyle: CellBorderStyleConstants);
    function  Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                   nSortBy: SortByConstants): WordBool; overload;
    function  Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                   nSortBy: SortByConstants; var SortKeys: OleVariant): WordBool; overload;
    function  Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                   nSortBy: SortByConstants; var SortKeys: OleVariant; var SortKeyOrders: OleVariant): WordBool; overload;
    procedure SwapRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer);
    procedure SetSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer);
    function  GetCellSpan(lCol: Integer; lRow: Integer; var pvColAnchor: OleVariant; 
                          var pvRowAnchor: OleVariant; var pvNumCols: OleVariant; 
                          var pvNumRows: OleVariant): GetCellSpanConstants;
    procedure TypeComboBoxClear(lCol: Integer; lRow: Integer);
    procedure TypeComboBoxRemoveItem(lCol: Integer; lRow: Integer; nIndex: Smallint);
    procedure VirtualRefresh;
    procedure RemoveCellSpan(lCol: Integer; lRow: Integer);
    function  GetColFromID(const ColID: WideString): Integer;
    function  AddCellSpan(lCol: Integer; lRow: Integer; lNumCols: Integer; lNumRows: Integer): WordBool;
    function  ScriptGetCellPos(Col: Integer; Row: Integer; var x: OleVariant; var y: OleVariant; 
                               var Width: OleVariant; var Height: OleVariant): WordBool;
    procedure ScriptColWidthToTwips(Width: Single; var Twips: OleVariant);
    function  ScriptCFGetDoubleParamExt(Param: Smallint; var ParamValue: OleVariant): Double;
    function  GetFloat(Col: Integer; Row: Integer; var Value: Double): WordBool;
    procedure ScriptGetBottomRightCell(var Col: OleVariant; var Row: OleVariant);
    procedure ScriptGetCellFromScreenCoord(var Col: OleVariant; var Row: OleVariant; x: Integer; 
                                           y: Integer);
    function  ScriptCFGetParamInfo(Param: Smallint; var Type_: OleVariant; var Status: OleVariant): WordBool;
    procedure ScriptTwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: OleVariant);
    procedure ScriptGetLastValidCell(var Col: OleVariant; var Row: OleVariant);
    procedure ScriptGetClientArea(var Width: OleVariant; var Height: OleVariant);
    procedure ScriptCFGetRangeParam(Param: Smallint; var Col: OleVariant; var Row: OleVariant; 
                                    var Col2: OleVariant; var Row2: OleVariant);
    procedure ScriptRowHeightToTwips(Row: Integer; Height: Single; var Twips: OleVariant);
    procedure ScriptTwipsToColWidth(Twips: Integer; var ColWidth: OleVariant);
    function  SetOddEvenRowColor(clrBackOdd: Integer; clrForeOdd: Integer; clrBackEven: Integer; 
                                 clrForeEven: Integer): WordBool;
    function  SetActionKey(Action: Smallint; fShift: WordBool; fCtrl: WordBool; Key: Smallint): WordBool;
    procedure TwipsToColWidth(Twips: Integer; var ColWidth: Single);
    procedure SetRefStyle(RefStyle: Smallint);
    function  GetTextTipAppearance(var FontName: WideString; var FontSize: Smallint; 
                                   var FontBold: WordBool; var FontItalic: WordBool; 
                                   var BackColor: Integer; var ForeColor: Integer): WordBool;
    procedure TwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: Single);
    function  GetActionKey(Action: Smallint; var fShift: WordBool; var fCtrl: WordBool; 
                           var Key: Smallint): WordBool;
    procedure SetRowItemData(Row: Integer; Value: Integer);
    procedure SetCalText(const ShortDays: WideString; const LongDays: WideString; 
                         const ShortMonths: WideString; const LongMonths: WideString; 
                         const OkText: WideString; const CancelText: WideString);
    function  SetFloat(Col: Integer; Row: Integer; Value: Double): WordBool;
    function  GetOddEvenRowColor(var clrBackOdd: Integer; var clrForeOdd: Integer; 
                                 var clrBackEven: Integer; var clrForeEven: Integer): WordBool;
    procedure SetText(Col: Integer; Row: Integer; Var_: OleVariant);
    function  GetInteger(Col: Integer; Row: Integer; var Value: Integer): WordBool;
    function  SetInteger(Col: Integer; Row: Integer; Value: Integer): WordBool;
    procedure MoveRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer);
    procedure ReCalc;
    procedure InsertCols(lCol: Integer; lNumCols: Integer);
    procedure ClipboardCopy;
    procedure ClipboardCut;
    procedure InsertRows(lRow: Integer; lNumRows: Integer);
    procedure DataSave;
    function  ScriptGetTextTipAppearance(var FontName: OleVariant; var FontSize: OleVariant; 
                                         var FontBold: OleVariant; var FontItalic: OleVariant; 
                                         var BackColor: OleVariant; var ForeColor: OleVariant): WordBool;
    function  ScriptGetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                      var ListCount: OleVariant; const LogFileName: WideString; 
                                      var WorkbookHandle: OleVariant; Replace: WordBool): WordBool;
    function  ScriptEnumCustomFunction(const PrevFuncName: WideString; var FuncName: OleVariant): WordBool;
    procedure GetCellBorder(lCol: Integer; lRow: Integer; nIndex: Smallint; 
                            var pcrColor: OleVariant; var pnStyle: OleVariant);
    procedure GetSelection(lIndex: Integer; var plCol: OleVariant; var plRow: OleVariant; 
                           var plCol2: OleVariant; var plRow2: OleVariant);
    procedure ClearRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                         bDataOnly: WordBool);
    procedure ClearSelection;
    procedure DeleteCols(lCol: Integer; lNumCols: Integer);
    procedure DeleteRows(lRow: Integer; lNumRows: Integer);
    procedure ClipboardPaste;
    procedure SetIteration(Iteration: WordBool; MaxIterations: Smallint; MaxChange: Double);
    procedure CopyRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                        lColDest: Integer; lRowDest: Integer);
    procedure MoveRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
    function  SaveTabFileU(const FileName: WideString): WordBool;
    procedure Refresh;
    procedure SwapColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
    procedure CopyRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
    procedure MoveColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
    procedure AboutBox;
    function  ExportRangeToTextFileU(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                     const FileName: WideString; const CellDelim: WideString; 
                                     const ColDelim: WideString; const RowDelim: WideString; 
                                     Flags: ExportRangeToTextFileConstants; 
                                     const LogFile: WideString): WordBool;
    procedure CopyColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
    procedure ShowCell(lCol: Integer; lRow: Integer; nPosition: PositionConstants);
    function  ExportToTextFileU(const FileName: WideString; const CellDelim: WideString; 
                                const ColDelim: WideString; const RowDelim: WideString; 
                                Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool;
    function  SearchRow(lRow: Integer; lColStart: Integer; lColEnd: Integer; 
                        const Text: WideString; SearchFlags: SearchFlagsConstants): Integer;
    function  ScriptGetFloat(Col: Integer; Row: Integer; var Value: OleVariant): WordBool;
    procedure ScriptGetCalTextOverride(var ShortDays: OleVariant; var LenShortDays: OleVariant; 
                                       var LongDays: OleVariant; var LenLongDays: OleVariant; 
                                       var ShortMonths: OleVariant; var LenShortMonths: OleVariant; 
                                       var LongMonths: OleVariant; var LenLongMonths: OleVariant; 
                                       var OkText: OleVariant; var LenOkText: OleVariant; 
                                       var CancelText: OleVariant; var LenCancelText: OleVariant);
    procedure PrintSheet; overload;
    procedure PrintSheet(var Flags: OleVariant); overload;
    function  ScriptGetInteger(Col: Integer; Row: Integer; var Value: OleVariant): WordBool;
    function  ScriptGetActionKey(Action: Smallint; var fShift: OleVariant; var fCtrl: OleVariant; 
                                 var Key: OleVariant): WordBool;
    function  ScriptGetOddEvenRowColor(var clrBackOdd: OleVariant; var clrForeOdd: OleVariant; 
                                       var clrBackEven: OleVariant; var clrForeEven: OleVariant): WordBool;
    procedure AddSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer);
    function  ScriptExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; 
                                           Row2: Integer; const Root: WideString; 
                                           const Collection: WideString; var Buff: OleVariant; 
                                           Flags: ExportToXMLConstants; const LogFile: WideString): WordBool;
    procedure SwapRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
    function  SearchCol(lCol: Integer; lRowStart: Integer; lRowEnd: Integer; 
                        const Text: WideString; SearchFlags: SearchFlagsConstants): Integer;
    function  ScriptOwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer; 
                                        Right: Integer; Bottom: Integer; var PageCount: OleVariant): Integer;
    function  ScriptGetCustomFunction(const FuncName: WideString; var MinArgs: OleVariant; 
                                      var MaxArgs: OleVariant; var Flags: OleVariant): WordBool;
    function  ScriptExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                      var Buff: OleVariant; Flags: ExportToXMLConstants; 
                                      const LogFile: WideString): WordBool;
    property  ControlInterface: _DSpreadSheet read GetControlInterface;
    property  DefaultInterface: _DSpreadSheet read GetControlInterface;
    property ColUserSortIndicator[lCol: Integer]: ColUserSortIndicatorConstants read Get_ColUserSortIndicator write Set_ColUserSortIndicator;
    property MaxTextRowHeight[lRow: Integer]: Double read Get_MaxTextRowHeight write Set_MaxTextRowHeight;
    property ColWidth[lCol: Integer]: Double read Get_ColWidth write Set_ColWidth;
    property TypeCheckPicture[Index: Smallint]: IPictureDisp read Get_TypeCheckPicture write Set_TypeCheckPicture;
    property MaxTextColWidth[lCol: Integer]: Double read Get_MaxTextColWidth write Set_MaxTextColWidth;
    property SortKeyOrder[nIndex: Smallint]: SortKeyOrderConstants read Get_SortKeyOrder write Set_SortKeyOrder;
    property SortKey[nIndex: Smallint]: Integer read Get_SortKey write Set_SortKey;
    property RowHeight[lRow: Integer]: Double read Get_RowHeight write Set_RowHeight;
    property EventEnabled[Event: EVENTENABLEDConstants]: WordBool read Get_EventEnabled write Set_EventEnabled;
    property TypeFloatDecimalChar: Smallint index 161 read GetSmallintProp write SetSmallintProp;
    property TypeFloatDecimalPlaces: Smallint index 162 read GetSmallintProp write SetSmallintProp;
    property TypeFloatMax: Double index 163 read GetDoubleProp write SetDoubleProp;
    property FileNum: Smallint index 50 read GetSmallintProp write SetSmallintProp;
    property FloatDefCurrencyChar: Smallint index 51 read GetSmallintProp write SetSmallintProp;
    property RowHeaderDisplay: TOleEnum index 104 read GetTOleEnumProp write SetTOleEnumProp;
    property FloatDefDecimalChar: Smallint index 52 read GetSmallintProp write SetSmallintProp;
    property FloatDefSepChar: Smallint index 53 read GetSmallintProp write SetSmallintProp;
    property TypeFloatMin: Double index 164 read GetDoubleProp write SetDoubleProp;
    property TypeFloatMoney: WordBool index 165 read GetWordBoolProp write SetWordBoolProp;
    property TypeFloatSeparator: WordBool index 166 read GetWordBoolProp write SetWordBoolProp;
    property ColHeaderDisplay: TOleEnum index 24 read GetTOleEnumProp write SetTOleEnumProp;
    property TypeFloatSepChar: Smallint index 167 read GetSmallintProp write SetSmallintProp;
    property TypeFloatCurrencyChar: Smallint index 160 read GetSmallintProp write SetSmallintProp;
    property TypeIntegerMax: Integer index 169 read GetIntegerProp write SetIntegerProp;
    property TypeIntegerMin: Integer index 170 read GetIntegerProp write SetIntegerProp;
    property TypeIntegerSpinInc: Integer index 171 read GetIntegerProp write SetIntegerProp;
    property CellBorderType: Smallint index 17 read GetSmallintProp write SetSmallintProp;
    property CellBorderStyle: TOleEnum index 16 read GetTOleEnumProp write SetTOleEnumProp;
    property CellBorderColor: TColor index 15 read GetTColorProp write SetTColorProp;
    property CalcDependencies: WordBool index 14 read GetWordBoolProp write SetWordBoolProp;
    property TypeEditLen: Integer index 157 read GetIntegerProp write SetIntegerProp;
    property DisplayRowHeaders: WordBool index 45 read GetWordBoolProp write SetWordBoolProp;
    property DisplayColHeaders: WordBool index 44 read GetWordBoolProp write SetWordBoolProp;
    property SelModeSelCount: Integer index 119 read GetIntegerProp write SetIntegerProp;
    property SortBy: TOleEnum index 126 read GetTOleEnumProp write SetTOleEnumProp;
    property DestRow: Integer index 42 read GetIntegerProp write SetIntegerProp;
    property Position: TOleEnum index 77 read GetTOleEnumProp write SetTOleEnumProp;
    property TypeIntegerSpinWrap: WordBool index 172 read GetWordBoolProp write SetWordBoolProp;
    property TypeTextAlignVert: TOleEnum index 181 read GetTOleEnumProp write SetTOleEnumProp;
    property Action: TOleEnum index 1 read GetTOleEnumProp write SetTOleEnumProp;
    property MultiSelIndex: Integer index 73 read GetIntegerProp write SetIntegerProp;
    property DestCol: Integer index 41 read GetIntegerProp write SetIntegerProp;
    property MultiSelCount: Integer index 72 read GetIntegerProp write SetIntegerProp;
  published
    property  ParentColor;
    property  ParentFont;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property  OnMouseUp;
    property  OnMouseMove;
    property  OnMouseDown;
    property  OnKeyUp;
    property  OnKeyPress;
    property  OnKeyDown;
    property Col: Integer index 22 read GetIntegerProp write SetIntegerProp stored False;
    property Col2: Integer index 23 read GetIntegerProp write SetIntegerProp stored False;
    property ColHidden: WordBool index 25 read GetWordBoolProp write SetWordBoolProp stored False;
    property RowsFrozen: Integer index 107 read GetIntegerProp write SetIntegerProp stored False;
    property ScrollBarExtMode: WordBool index 108 read GetWordBoolProp write SetWordBoolProp stored False;
    property ScrollBarMaxAlign: WordBool index 109 read GetWordBoolProp write SetWordBoolProp stored False;
    property ChangeMade: WordBool index 19 read GetWordBoolProp write SetWordBoolProp stored False;
    property DataChanged: WordBool index 30 read GetWordBoolProp write SetWordBoolProp stored False;
    property DataColCnt: Integer index 31 read GetIntegerProp write SetIntegerProp stored False;
    property DataField: WideString index 32 read GetWideStringProp write SetWideStringProp stored False;
    property Clip: WideString index 20 read GetWideStringProp write SetWideStringProp stored False;
    property ClipValue: WideString index 21 read GetWideStringProp write SetWideStringProp stored False;
    property ColPageBreak: WordBool index 26 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelBlockCol: Integer index 112 read GetIntegerProp write SetIntegerProp stored False;
    property ShadowColor: TColor index 123 read GetTColorProp write SetTColorProp stored False;
    property Row: Integer index 102 read GetIntegerProp write SetIntegerProp stored False;
    property Row2: Integer index 103 read GetIntegerProp write SetIntegerProp stored False;
    property StartingColNumber: Integer index 127 read GetIntegerProp write SetIntegerProp stored False;
    property SelStart: Integer index 121 read GetIntegerProp write SetIntegerProp stored False;
    property SelText: WideString index 122 read GetWideStringProp write SetWideStringProp stored False;
    property RowHidden: WordBool index 105 read GetWordBoolProp write SetWordBoolProp stored False;
    property RowPageBreak: WordBool index 106 read GetWordBoolProp write SetWordBoolProp stored False;
    property ScrollBars: TOleEnum index 110 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScrollBarShowMax: WordBool index 111 read GetWordBoolProp write SetWordBoolProp stored False;
    property RestrictCols: WordBool index 99 read GetWordBoolProp write SetWordBoolProp stored False;
    property RestrictRows: WordBool index 100 read GetWordBoolProp write SetWordBoolProp stored False;
    property RetainSelBlock: WordBool index 101 read GetWordBoolProp write SetWordBoolProp stored False;
    property GridSolid: WordBool index 59 read GetWordBoolProp write SetWordBoolProp stored False;
    property hDCPrinter: Integer index 60 read GetIntegerProp write SetIntegerProp stored False;
    property InterfaceDesigner: Smallint index 61 read GetSmallintProp write SetSmallintProp stored False;
    property AutoClipboard: WordBool index 10 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 11 read GetWordBoolProp write SetWordBoolProp stored False;
    property DataFillEvent: WordBool index 33 read GetWordBoolProp write SetWordBoolProp stored False;
    property GridColor: TColor index 56 read GetTColorProp write SetTColorProp stored False;
    property LockForeColor: TColor index 66 read GetTColorProp write SetTColorProp stored False;
    property MaxCols: Integer index 67 read GetIntegerProp write SetIntegerProp stored False;
    property MaxRows: Integer index 68 read GetIntegerProp write SetIntegerProp stored False;
    property GridShowHoriz: WordBool index 57 read GetWordBoolProp write SetWordBoolProp stored False;
    property GridShowVert: WordBool index 58 read GetWordBoolProp write SetWordBoolProp stored False;
    property IsBlockSelected: WordBool index 62 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoCalc: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property AllowDragDrop: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property AllowMultiBlocks: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property AllowUserFormulas: WordBool index 7 read GetWordBoolProp write SetWordBoolProp stored False;
    property ColsFrozen: Integer index 27 read GetIntegerProp write SetIntegerProp stored False;
    property CursorStyle: TOleEnum index 28 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property CursorType: TOleEnum index 29 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ActiveCol: Integer index 2 read GetIntegerProp write SetIntegerProp stored False;
    property BlockMode: WordBool index 12 read GetWordBoolProp write SetWordBoolProp stored False;
    property ButtonDrawMode: Smallint index 13 read GetSmallintProp write SetSmallintProp stored False;
    property CellType: TOleEnum index 18 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ActiveRow: Integer index 3 read GetIntegerProp write SetIntegerProp stored False;
    property AllowCellOverflow: WordBool index 4 read GetWordBoolProp write SetWordBoolProp stored False;
    property ArrowsExitEditMode: WordBool index 8 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelBackColor: TColor index 289 read GetTColorProp write SetTColorProp stored False;
    property PrintMarginRight: Integer index 88 read GetIntegerProp write SetIntegerProp stored False;
    property PrintMarginTop: Integer index 89 read GetIntegerProp write SetIntegerProp stored False;
    property PrintPageEnd: Smallint index 90 read GetSmallintProp write SetSmallintProp stored False;
    property PrintPageStart: Smallint index 91 read GetSmallintProp write SetSmallintProp stored False;
    property PrintUseDataMax: WordBool index 95 read GetWordBoolProp write SetWordBoolProp stored False;
    property ProcessTab: WordBool index 96 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintJobName: WideString index 85 read GetWideStringProp write SetWideStringProp stored False;
    property PrintMarginBottom: Integer index 86 read GetIntegerProp write SetIntegerProp stored False;
    property PrintMarginLeft: Integer index 87 read GetIntegerProp write SetIntegerProp stored False;
    property ReDraw: WordBool index 98 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelectBlockOptions: Smallint index 116 read GetSmallintProp write SetSmallintProp stored False;
    property SelLength: Integer index 117 read GetIntegerProp write SetIntegerProp stored False;
    property PrintBorder: WordBool index 79 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintColHeaders: WordBool index 80 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintColor: WordBool index 81 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelModeIndex: Integer index 118 read GetIntegerProp write SetIntegerProp stored False;
    property SelModeSelected: WordBool index 120 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShadowDark: TColor index 124 read GetTColorProp write SetTColorProp stored False;
    property ShadowText: TColor index 125 read GetTColorProp write SetTColorProp stored False;
    property SelBlockCol2: Integer index 113 read GetIntegerProp write SetIntegerProp stored False;
    property SelBlockRow: Integer index 114 read GetIntegerProp write SetIntegerProp stored False;
    property SelBlockRow2: Integer index 115 read GetIntegerProp write SetIntegerProp stored False;
    property PrintHeader: WideString index 84 read GetWideStringProp write SetWideStringProp stored False;
    property PrintType: TOleEnum index 94 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property NoBeep: WordBool index 74 read GetWordBoolProp write SetWordBoolProp stored False;
    property NoBorder: WordBool index 75 read GetWordBoolProp write SetWordBoolProp stored False;
    property Protect: WordBool index 97 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintRowHeaders: WordBool index 92 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintShadows: WordBool index 93 read GetWordBoolProp write SetWordBoolProp stored False;
    property OperationMode: TOleEnum index 76 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property PrintAbortMsg: WideString index 78 read GetWideStringProp write SetWideStringProp stored False;
    property PrintFooter: WideString index 82 read GetWideStringProp write SetWideStringProp stored False;
    property PrintGrid: WordBool index 83 read GetWordBoolProp write SetWordBoolProp stored False;
    property MaxTextCellHeight: Double index 69 read GetDoubleProp write SetDoubleProp stored False;
    property MaxTextCellWidth: Double index 70 read GetDoubleProp write SetDoubleProp stored False;
    property MoveActiveOnFocus: WordBool index 71 read GetWordBoolProp write SetWordBoolProp stored False;
    property FormulaSync: WordBool index 216 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeComboBoxList: WideString index 148 read GetWideStringProp write SetWideStringProp stored False;
    property TypeComboBoxString: WideString index 149 read GetWideStringProp write SetWideStringProp stored False;
    property TypeComboBoxMaxDrop: Smallint index 274 read GetSmallintProp write SetSmallintProp stored False;
    property PrintOrientation: TOleEnum index 214 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property BackColorStyle: TOleEnum index 215 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeDateCentury: WordBool index 150 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeComboBoxEditable: WordBool index 146 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeEditCharCase: TOleEnum index 155 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeEditCharSet: TOleEnum index 156 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeComboBoxIndex: Smallint index 147 read GetSmallintProp write SetSmallintProp stored False;
    property TypeComboBoxCount: Smallint index 144 read GetSmallintProp write SetSmallintProp stored False;
    property TypeComboBoxCurSel: Smallint index 145 read GetSmallintProp write SetSmallintProp stored False;
    property Appearance: TOleEnum index 273 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property PrintFirstPageNumber: Integer index 285 read GetIntegerProp write SetIntegerProp stored False;
    property TypeMaxEditLen: Smallint index 275 read GetSmallintProp write SetSmallintProp stored False;
    property FontSize: Single index 210 read GetSingleProp write SetSingleProp stored False;
    property TypeComboBoxhWnd: Integer index 288 read GetIntegerProp write SetIntegerProp stored False;
    property PrintPageOrder: TOleEnum index 283 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property PrintPageCount: Integer index 284 read GetIntegerProp write SetIntegerProp stored False;
    property FontStrikethru: WordBool index 211 read GetWordBoolProp write SetWordBoolProp stored False;
    property FontName: WideString index 209 read GetWideStringProp write SetWideStringProp stored False;
    property CursorIcon: TPicture index 213 read GetTPictureProp write SetTPictureProp stored False;
    property TypeCheckType: TOleEnum index 272 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property FontUnderline: WordBool index 212 read GetWordBoolProp write SetWordBoolProp stored False;
    property FontBold: WordBool index 207 read GetWordBoolProp write SetWordBoolProp stored False;
    property FontItalic: WordBool index 208 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeButtonPictureDown: TPicture index 136 read GetTPictureProp write SetTPictureProp stored False;
    property TypeButtonShadowSize: Smallint index 137 read GetSmallintProp write SetSmallintProp stored False;
    property TypeButtonText: WideString index 138 read GetWideStringProp write SetWideStringProp stored False;
    property TypeCheckCenter: WordBool index 141 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeCheckText: WideString index 142 read GetWideStringProp write SetWideStringProp stored False;
    property TypeButtonTextColor: TColor index 139 read GetTColorProp write SetTColorProp stored False;
    property TypeEditPassword: WordBool index 159 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypePictMaintainScale: WordBool index 177 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypePictCenter: WordBool index 176 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypePicMask: WideString index 175 read GetWideStringProp write SetWideStringProp stored False;
    property TypePicDefaultText: WideString index 174 read GetWideStringProp write SetWideStringProp stored False;
    property TypeOwnerDrawStyle: Integer index 173 read GetIntegerProp write SetIntegerProp stored False;
    property TypeHAlign: TOleEnum index 168 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeButtonType: TOleEnum index 140 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeDateMax: WideString index 152 read GetWideStringProp write SetWideStringProp stored False;
    property TypeDateMin: WideString index 153 read GetWideStringProp write SetWideStringProp stored False;
    property TypeCheckTextAlign: TOleEnum index 143 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeEditMultiLine: WordBool index 158 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeDateSeparator: Smallint index 154 read GetSmallintProp write SetSmallintProp stored False;
    property TypeDateFormat: TOleEnum index 151 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeButtonDarkColor: TColor index 133 read GetTColorProp write SetTColorProp stored False;
    property TopRow: Integer index 129 read GetIntegerProp write SetIntegerProp stored False;
    property TypeButtonAlign: TOleEnum index 130 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeButtonBorderColor: TColor index 131 read GetTColorProp write SetTColorProp stored False;
    property TypeButtonLightColor: TColor index 134 read GetTColorProp write SetTColorProp stored False;
    property TypeButtonPicture: TPicture index 135 read GetTPictureProp write SetTPictureProp stored False;
    property TypeButtonColor: TColor index 132 read GetTColorProp write SetTColorProp stored False;
    property VirtualCurRowCount: Integer index 196 read GetIntegerProp write SetIntegerProp stored False;
    property VirtualCurTop: Integer index 197 read GetIntegerProp write SetIntegerProp stored False;
    property VirtualMaxRows: Integer index 198 read GetIntegerProp write SetIntegerProp stored False;
    property EditMode: WordBool index 47 read GetWordBoolProp write SetWordBoolProp stored False;
    property EditModePermanent: WordBool index 48 read GetWordBoolProp write SetWordBoolProp stored False;
    property StartingRowNumber: Integer index 128 read GetIntegerProp write SetIntegerProp stored False;
    property UserResizeCol: TOleEnum index 193 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property VisibleCols: Integer index 203 read GetIntegerProp write SetIntegerProp stored False;
    property VisibleRows: Integer index 204 read GetIntegerProp write SetIntegerProp stored False;
    property VScrollSpecial: WordBool index 205 read GetWordBoolProp write SetWordBoolProp stored False;
    property UserResizeRow: TOleEnum index 194 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Value: WideString index 195 read GetWideStringProp write SetWideStringProp stored False;
    property VirtualMode: WordBool index 199 read GetWordBoolProp write SetWordBoolProp stored False;
    property EditEnterAction: TOleEnum index 46 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property DAutoHeadings: WordBool index 38 read GetWordBoolProp write SetWordBoolProp stored False;
    property DAutoSave: WordBool index 39 read GetWordBoolProp write SetWordBoolProp stored False;
    property DAutoSizeCols: TOleEnum index 40 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property LeftCol: Integer index 63 read GetIntegerProp write SetIntegerProp stored False;
    property Lock: WordBool index 64 read GetWordBoolProp write SetWordBoolProp stored False;
    property LockBackColor: TColor index 65 read GetTColorProp write SetTColorProp stored False;
    property DataRowCnt: Integer index 34 read GetIntegerProp write SetIntegerProp stored False;
    property EditModeReplace: WordBool index 49 read GetWordBoolProp write SetWordBoolProp stored False;
    property Formula: WideString index 54 read GetWideStringProp write SetWideStringProp stored False;
    property GrayAreaBackColor: TColor index 55 read GetTColorProp write SetTColorProp stored False;
    property DAutoCellTypes: WordBool index 36 read GetWordBoolProp write SetWordBoolProp stored False;
    property DAutoFill: WordBool index 37 read GetWordBoolProp write SetWordBoolProp stored False;
    property DInformActiveRowChange: WordBool index 43 read GetWordBoolProp write SetWordBoolProp stored False;
    property VScrollSpecialType: Smallint index 206 read GetSmallintProp write SetSmallintProp stored False;
    property ScrollBarTrack: TOleEnum index 279 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeVAlign: TOleEnum index 280 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeTime24Hour: TOleEnum index 186 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeTimeMax: WideString index 187 read GetWideStringProp write SetWideStringProp stored False;
    property TypeTimeMin: WideString index 188 read GetWideStringProp write SetWideStringProp stored False;
    property ClipboardOptions: Smallint index 281 read GetSmallintProp write SetSmallintProp stored False;
    property PrintSmartPrint: WordBool index 282 read GetWordBoolProp write SetWordBoolProp stored False;
    property PrintNextPageBreakCol: Integer index 286 read GetIntegerProp write SetIntegerProp stored False;
    property PrintNextPageBreakRow: Integer index 287 read GetIntegerProp write SetIntegerProp stored False;
    property TypeComboBoxWidth: Smallint index 276 read GetSmallintProp write SetSmallintProp stored False;
    property TextTip: TOleEnum index 277 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TextTipDelay: Integer index 278 read GetIntegerProp write SetIntegerProp stored False;
    property UnitType: TOleEnum index 191 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property UserResize: TOleEnum index 192 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeTextPrefix: WordBool index 182 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeTextShadow: WordBool index 183 read GetWordBoolProp write SetWordBoolProp stored False;
    property VirtualOverlap: Integer index 200 read GetIntegerProp write SetIntegerProp stored False;
    property VirtualRows: Integer index 201 read GetIntegerProp write SetIntegerProp stored False;
    property VirtualScrollBuffer: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeTextShadowIn: WordBool index 184 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeTextWordWrap: WordBool index 185 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeTimeSeconds: WordBool index 189 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeTimeSeparator: Smallint index 190 read GetSmallintProp write SetSmallintProp stored False;
    property TypePictPicture: TPicture index 178 read GetTPictureProp write SetTPictureProp stored False;
    property TypePictStretch: WordBool index 179 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeSpin: WordBool index 180 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeCurrencySeparator: WideString index 352 read GetWideStringProp write SetWideStringProp stored False;
    property ColHeaderRows: Integer index 342 read GetIntegerProp write SetIntegerProp stored False;
    property UserColAction: TOleEnum index 319 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeCurrencySymbol: WideString index 353 read GetWideStringProp write SetWideStringProp stored False;
    property TypeCurrencyMax: Double index 350 read GetDoubleProp write SetDoubleProp stored False;
    property TypeCurrencyDecimal: WideString index 351 read GetWideStringProp write SetWideStringProp stored False;
    property ShowScrollTips: TOleEnum index 320 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TwoDigitYearMax: Smallint index 291 read GetSmallintProp write SetSmallintProp stored False;
    property OLEDropMode: TOleEnum index 315 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ColID: WideString index 339 read GetWideStringProp write SetWideStringProp stored False;
    property CellNoteIndicator: TOleEnum index 321 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property CellNote: WideString index 318 read GetWideStringProp write SetWideStringProp stored False;
    property SelForeColor: TColor index 290 read GetTColorProp write SetTColorProp stored False;
    property TypeCurrencyNegStyle: TOleEnum index 356 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeSpinWrap: WordBool index 347 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeNegRed: WordBool index 348 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeCurrencyMin: Double index 349 read GetDoubleProp write SetDoubleProp stored False;
    property TypeComboBoxAutoSearch: TOleEnum index 345 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeCurrencyLeadingZero: TOleEnum index 354 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeCurrencyDecPlaces: Smallint index 355 read GetSmallintProp write SetSmallintProp stored False;
    property TypeSpinInc: Double index 346 read GetDoubleProp write SetDoubleProp stored False;
    property ColHeadersAutoTextIndex: Integer index 343 read GetIntegerProp write SetIntegerProp stored False;
    property ColHeadersUserSortIndex: Integer index 344 read GetIntegerProp write SetIntegerProp stored False;
    property RowHeadersShow: WordBool index 379 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypePercentLeadingZero: TOleEnum index 380 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property SelectionCount: Integer index 489 read GetIntegerProp write SetIntegerProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property ForeColor: TColor index -513 read GetTColorProp write SetTColorProp stored False;
    property BackColor: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property TypePercentDecPlaces: Smallint index 371 read GetSmallintProp write SetSmallintProp stored False;
    property TypeNumberMin: Double index 360 read GetDoubleProp write SetDoubleProp stored False;
    property TypeCurrencyPosStyle: TOleEnum index 357 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeCurrencyShowSep: WordBool index 358 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeNumberMax: Double index 361 read GetDoubleProp write SetDoubleProp stored False;
    property TypeNumberDecimal: WideString index 362 read GetWideStringProp write SetWideStringProp stored False;
    property TypeNumberSeparator: WideString index 363 read GetWideStringProp write SetWideStringProp stored False;
    property BorderStyle: TOleEnum index -504 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeEllipses: WordBool index 330 read GetWordBoolProp write SetWordBoolProp stored False;
    property ScriptEnhanced: WordBool index 336 read GetWordBoolProp write SetWordBoolProp stored False;
    property ColMerge: TOleEnum index 337 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property RowHeaderCols: Integer index 340 read GetIntegerProp write SetIntegerProp stored False;
    property RowHeadersAutoTextIndex: Integer index 341 read GetIntegerProp write SetIntegerProp stored False;
    property RowMerge: TOleEnum index 338 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ColHeadersAutoText: TOleEnum index 376 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypePercentNegStyle: TOleEnum index 372 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeTextOrient: TOleEnum index 373 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property CellTag: WideString index 374 read GetWideStringProp write SetWideStringProp stored False;
    property ColHeadersShow: WordBool index 377 read GetWordBoolProp write SetWordBoolProp stored False;
    property RowHeadersAutoText: TOleEnum index 378 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property AllowEditOverflow: WordBool index 375 read GetWordBoolProp write SetWordBoolProp stored False;
    property hWnd: Integer index -515 read GetIntegerProp write SetIntegerProp stored False;
    property Text: WideString index -517 read GetWideStringProp write SetWideStringProp stored False;
    property TypePercentDecimal: WideString index 370 read GetWideStringProp write SetWideStringProp stored False;
    property TypeNumberShowSep: WordBool index 367 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypeNumberLeadingZero: TOleEnum index 364 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property TypeCurrencyShowSymbol: WordBool index 359 read GetWordBoolProp write SetWordBoolProp stored False;
    property TypePercentMin: Double index 368 read GetDoubleProp write SetDoubleProp stored False;
    property TypePercentMax: Double index 369 read GetDoubleProp write SetDoubleProp stored False;
    property TypeNumberDecPlaces: Smallint index 365 read GetSmallintProp write SetSmallintProp stored False;
    property TypeNumberNegStyle: TOleEnum index 366 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property DataSource: _DSpreadCursor read Get_DataSource write Set_DataSource stored False;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnAdvance: TvaSpreadAdvance read FOnAdvance write FOnAdvance;
    property OnBlockSelected: TvaSpreadBlockSelected read FOnBlockSelected write FOnBlockSelected;
    property OnButtonClicked: TvaSpreadButtonClicked read FOnButtonClicked write FOnButtonClicked;
    property OnChange: TvaSpreadChange read FOnChange write FOnChange;
    property OnClick: TvaSpreadClick read FOnClick write FOnClick;
    property OnColWidthChange: TvaSpreadColWidthChange read FOnColWidthChange write FOnColWidthChange;
    property OnCustomFunction: TvaSpreadCustomFunction read FOnCustomFunction write FOnCustomFunction;
    property OnDataColConfig: TvaSpreadDataColConfig read FOnDataColConfig write FOnDataColConfig;
    property OnDataFill: TvaSpreadDataFill read FOnDataFill write FOnDataFill;
    property OnDblClick: TvaSpreadDblClick read FOnDblClick write FOnDblClick;
    property OnDragDropBlock: TvaSpreadDragDropBlock read FOnDragDropBlock write FOnDragDropBlock;
    property OnDrawItem: TvaSpreadDrawItem read FOnDrawItem write FOnDrawItem;
    property OnEditError: TvaSpreadEditError read FOnEditError write FOnEditError;
    property OnEditMode: TvaSpreadEditMode read FOnEditMode write FOnEditMode;
    property OnEnterRow: TvaSpreadEnterRow read FOnEnterRow write FOnEnterRow;
    property OnLeaveCell: TvaSpreadLeaveCell read FOnLeaveCell write FOnLeaveCell;
    property OnLeaveRow: TvaSpreadLeaveRow read FOnLeaveRow write FOnLeaveRow;
    property OnPrintAbort: TvaSpreadPrintAbort read FOnPrintAbort write FOnPrintAbort;
    property OnQueryAdvance: TvaSpreadQueryAdvance read FOnQueryAdvance write FOnQueryAdvance;
    property OnQueryData: TvaSpreadQueryData read FOnQueryData write FOnQueryData;
    property OnRightClick: TvaSpreadRightClick read FOnRightClick write FOnRightClick;
    property OnRowHeightChange: TvaSpreadRowHeightChange read FOnRowHeightChange write FOnRowHeightChange;
    property OnSelChange: TvaSpreadSelChange read FOnSelChange write FOnSelChange;
    property OnTopLeftChange: TvaSpreadTopLeftChange read FOnTopLeftChange write FOnTopLeftChange;
    property OnUserFormulaEntered: TvaSpreadUserFormulaEntered read FOnUserFormulaEntered write FOnUserFormulaEntered;
    property OnVirtualClearData: TvaSpreadVirtualClearData read FOnVirtualClearData write FOnVirtualClearData;
    property OnPrintMsgBox: TvaSpreadPrintMsgBox read FOnPrintMsgBox write FOnPrintMsgBox;
    property OnComboCloseUp: TvaSpreadComboCloseUp read FOnComboCloseUp write FOnComboCloseUp;
    property OnComboDropDown: TvaSpreadComboDropDown read FOnComboDropDown write FOnComboDropDown;
    property OnComboSelChange: TvaSpreadComboSelChange read FOnComboSelChange write FOnComboSelChange;
    property OnTextTipFetch: TvaSpreadTextTipFetch read FOnTextTipFetch write FOnTextTipFetch;
    property OnEditChange: TvaSpreadEditChange read FOnEditChange write FOnEditChange;
    property OnOLECompleteDrag: TvaSpreadOLECompleteDrag read FOnOLECompleteDrag write FOnOLECompleteDrag;
    property OnOLEDragDrop: TvaSpreadOLEDragDrop read FOnOLEDragDrop write FOnOLEDragDrop;
    property OnOLEDragOver: TvaSpreadOLEDragOver read FOnOLEDragOver write FOnOLEDragOver;
    property OnOLEGiveFeedback: TvaSpreadOLEGiveFeedback read FOnOLEGiveFeedback write FOnOLEGiveFeedback;
    property OnOLESetData: TvaSpreadOLESetData read FOnOLESetData write FOnOLESetData;
    property OnOLEStartDrag: TvaSpreadOLEStartDrag read FOnOLEStartDrag write FOnOLEStartDrag;
    property OnBeforeUserSort: TvaSpreadBeforeUserSort read FOnBeforeUserSort write FOnBeforeUserSort;
    property OnAfterUserSort: TvaSpreadAfterUserSort read FOnAfterUserSort write FOnAfterUserSort;
    property OnScriptCustomFunction: TvaSpreadScriptCustomFunction read FOnScriptCustomFunction write FOnScriptCustomFunction;
    property OnScriptDataFill: TvaSpreadScriptDataFill read FOnScriptDataFill write FOnScriptDataFill;
    property OnScriptDragDropBlock: TvaSpreadScriptDragDropBlock read FOnScriptDragDropBlock write FOnScriptDragDropBlock;
    property OnScriptLeaveCell: TvaSpreadScriptLeaveCell read FOnScriptLeaveCell write FOnScriptLeaveCell;
    property OnScriptLeaveRow: TvaSpreadScriptLeaveRow read FOnScriptLeaveRow write FOnScriptLeaveRow;
    property OnScriptPrintAbort: TvaSpreadScriptPrintAbort read FOnScriptPrintAbort write FOnScriptPrintAbort;
    property OnScriptQueryAdvance: TvaSpreadScriptQueryAdvance read FOnScriptQueryAdvance write FOnScriptQueryAdvance;
    property OnScriptQueryData: TvaSpreadScriptQueryData read FOnScriptQueryData write FOnScriptQueryData;
    property OnScriptPrintMsgBox: TvaSpreadScriptPrintMsgBox read FOnScriptPrintMsgBox write FOnScriptPrintMsgBox;
    property OnScriptTextTipFetch: TvaSpreadScriptTextTipFetch read FOnScriptTextTipFetch write FOnScriptTextTipFetch;
    property OnScriptOLECompleteDrag: TvaSpreadScriptOLECompleteDrag read FOnScriptOLECompleteDrag write FOnScriptOLECompleteDrag;
    property OnScriptOLEDragDrop: TvaSpreadScriptOLEDragDrop read FOnScriptOLEDragDrop write FOnScriptOLEDragDrop;
    property OnScriptOLEDragOver: TvaSpreadScriptOLEDragOver read FOnScriptOLEDragOver write FOnScriptOLEDragOver;
    property OnScriptOLEGiveFeedback: TvaSpreadScriptOLEGiveFeedback read FOnScriptOLEGiveFeedback write FOnScriptOLEGiveFeedback;
    property OnScriptOLEStartDrag: TvaSpreadScriptOLEStartDrag read FOnScriptOLEStartDrag write FOnScriptOLEStartDrag;
    property OnScriptBeforeUserSort: TvaSpreadScriptBeforeUserSort read FOnScriptBeforeUserSort write FOnScriptBeforeUserSort;
    property OnScriptKeyDown: TvaSpreadScriptKeyDown read FOnScriptKeyDown write FOnScriptKeyDown;
    property OnScriptKeyPress: TvaSpreadScriptKeyPress read FOnScriptKeyPress write FOnScriptKeyPress;
    property OnScriptKeyUp: TvaSpreadScriptKeyUp read FOnScriptKeyUp write FOnScriptKeyUp;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TvaSpreadPreview
// Help String      : FarPoint Spread Preview 6.0
// Default Interface: _DSpreadPreview
// Def. Intf. DISP? : Yes
// Event   Interface: _DSpreadPreviewEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TvaSpreadPreviewPageChange = procedure(Sender: TObject; Page: Integer) of object;
  TvaSpreadPreviewOLECompleteDrag = procedure(Sender: TObject; var Effect: Integer) of object;
  TvaSpreadPreviewOLEDragDrop = procedure(Sender: TObject; var Data: fpDataObject; 
                                                           var Effect: Integer; 
                                                           var Button: Smallint; 
                                                           var Shift: Smallint; var x: Single; 
                                                           var y: Single) of object;
  TvaSpreadPreviewOLEDragOver = procedure(Sender: TObject; var Data: fpDataObject; 
                                                           var Effect: Integer; 
                                                           var Button: Smallint; 
                                                           var Shift: Smallint; var x: Single; 
                                                           var y: Single; var State: Smallint) of object;
  TvaSpreadPreviewOLEGiveFeedback = procedure(Sender: TObject; var Effect: Integer; 
                                                               var DefaultCursors: WordBool) of object;
  TvaSpreadPreviewOLESetData = procedure(Sender: TObject; var Data: fpDataObject; 
                                                          var DataFormat: Smallint) of object;
  TvaSpreadPreviewOLEStartDrag = procedure(Sender: TObject; var Data: fpDataObject; 
                                                            var AllowedEffects: Integer) of object;
  TvaSpreadPreviewScriptOLECompleteDrag = procedure(Sender: TObject; var Effect: OleVariant) of object;
  TvaSpreadPreviewScriptOLEDragDrop = procedure(Sender: TObject; var Data: OleVariant; 
                                                                 var Effect: OleVariant; 
                                                                 var Button: OleVariant; 
                                                                 var Shift: OleVariant; 
                                                                 var x: OleVariant; 
                                                                 var y: OleVariant) of object;
  TvaSpreadPreviewScriptOLEDragOver = procedure(Sender: TObject; var Data: OleVariant; 
                                                                 var Effect: OleVariant; 
                                                                 var Button: OleVariant; 
                                                                 var Shift: OleVariant; 
                                                                 var x: OleVariant; 
                                                                 var y: OleVariant; 
                                                                 var State: OleVariant) of object;
  TvaSpreadPreviewScriptOLEGiveFeedback = procedure(Sender: TObject; var Effect: OleVariant; 
                                                                     var DefaultCursors: OleVariant) of object;
  TvaSpreadPreviewScriptOLEStartDrag = procedure(Sender: TObject; var Data: OleVariant; 
                                                                  var AllowedEffects: OleVariant) of object;

  TvaSpreadPreview = class(TOleControl)
  private
    FOnPageChange: TvaSpreadPreviewPageChange;
    FOnZoom: TNotifyEvent;
    FOnOLECompleteDrag: TvaSpreadPreviewOLECompleteDrag;
    FOnOLEDragDrop: TvaSpreadPreviewOLEDragDrop;
    FOnOLEDragOver: TvaSpreadPreviewOLEDragOver;
    FOnOLEGiveFeedback: TvaSpreadPreviewOLEGiveFeedback;
    FOnOLESetData: TvaSpreadPreviewOLESetData;
    FOnOLEStartDrag: TvaSpreadPreviewOLEStartDrag;
    FOnScriptOLECompleteDrag: TvaSpreadPreviewScriptOLECompleteDrag;
    FOnScriptOLEDragDrop: TvaSpreadPreviewScriptOLEDragDrop;
    FOnScriptOLEDragOver: TvaSpreadPreviewScriptOLEDragOver;
    FOnScriptOLEGiveFeedback: TvaSpreadPreviewScriptOLEGiveFeedback;
    FOnScriptOLEStartDrag: TvaSpreadPreviewScriptOLEStartDrag;
    FIntf: _DSpreadPreview;
    function  GetControlInterface: _DSpreadPreview;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure OLEDrag;
    procedure AboutBox;
    property  ControlInterface: _DSpreadPreview read GetControlInterface;
    property  DefaultInterface: _DSpreadPreview read GetControlInterface;
  published
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property  OnMouseUp;
    property  OnMouseMove;
    property  OnMouseDown;
    property  OnKeyUp;
    property  OnKeyPress;
    property  OnKeyDown;
    property  OnDblClick;
    property  OnClick;
    property AllowUserZoom: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property GrayAreaColor: TColor index 2 read GetTColorProp write SetTColorProp stored False;
    property GrayAreaMarginH: Integer index 3 read GetIntegerProp write SetIntegerProp stored False;
    property GrayAreaMarginType: TOleEnum index 4 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property GrayAreaMarginV: Integer index 5 read GetIntegerProp write SetIntegerProp stored False;
    property hWndSpread: Integer index 6 read GetIntegerProp write SetIntegerProp stored False;
    property PageBorderColor: TColor index 7 read GetTColorProp write SetTColorProp stored False;
    property PageBorderWidth: Smallint index 8 read GetSmallintProp write SetSmallintProp stored False;
    property PageShadowWidth: Smallint index 10 read GetSmallintProp write SetSmallintProp stored False;
    property PageShadowColor: TColor index 9 read GetTColorProp write SetTColorProp stored False;
    property PageViewPercentage: Smallint index 11 read GetSmallintProp write SetSmallintProp stored False;
    property PageViewType: TOleEnum index 12 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScrollBarH: TOleEnum index 13 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScrollBarV: TOleEnum index 14 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScrollIncH: Integer index 15 read GetIntegerProp write SetIntegerProp stored False;
    property ScrollIncV: Integer index 16 read GetIntegerProp write SetIntegerProp stored False;
    property PageMultiCntH: Smallint index 17 read GetSmallintProp write SetSmallintProp stored False;
    property PageMultiCntV: Smallint index 18 read GetSmallintProp write SetSmallintProp stored False;
    property PageGutterH: Integer index 19 read GetIntegerProp write SetIntegerProp stored False;
    property PageGutterV: Integer index 20 read GetIntegerProp write SetIntegerProp stored False;
    property ZoomState: TOleEnum index 21 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property PageCurrent: Integer index 22 read GetIntegerProp write SetIntegerProp stored False;
    property PagesPerScreen: Smallint index 23 read GetSmallintProp write SetSmallintProp stored False;
    property PagePercentageActual: Smallint index 24 read GetSmallintProp write SetSmallintProp stored False;
    property MousePointer: TOleEnum index 50 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property MouseIcon: TPicture index 51 read GetTPictureProp write SetTPictureProp stored False;
    property OLEDropMode: TOleEnum index 52 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ScriptEnhanced: WordBool index 54 read GetWordBoolProp write SetWordBoolProp stored False;
    property BorderStyle: TOleEnum index -504 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnPageChange: TvaSpreadPreviewPageChange read FOnPageChange write FOnPageChange;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnOLECompleteDrag: TvaSpreadPreviewOLECompleteDrag read FOnOLECompleteDrag write FOnOLECompleteDrag;
    property OnOLEDragDrop: TvaSpreadPreviewOLEDragDrop read FOnOLEDragDrop write FOnOLEDragDrop;
    property OnOLEDragOver: TvaSpreadPreviewOLEDragOver read FOnOLEDragOver write FOnOLEDragOver;
    property OnOLEGiveFeedback: TvaSpreadPreviewOLEGiveFeedback read FOnOLEGiveFeedback write FOnOLEGiveFeedback;
    property OnOLESetData: TvaSpreadPreviewOLESetData read FOnOLESetData write FOnOLESetData;
    property OnOLEStartDrag: TvaSpreadPreviewOLEStartDrag read FOnOLEStartDrag write FOnOLEStartDrag;
    property OnScriptOLECompleteDrag: TvaSpreadPreviewScriptOLECompleteDrag read FOnScriptOLECompleteDrag write FOnScriptOLECompleteDrag;
    property OnScriptOLEDragDrop: TvaSpreadPreviewScriptOLEDragDrop read FOnScriptOLEDragDrop write FOnScriptOLEDragDrop;
    property OnScriptOLEDragOver: TvaSpreadPreviewScriptOLEDragOver read FOnScriptOLEDragOver write FOnScriptOLEDragOver;
    property OnScriptOLEGiveFeedback: TvaSpreadPreviewScriptOLEGiveFeedback read FOnScriptOLEGiveFeedback write FOnScriptOLEGiveFeedback;
    property OnScriptOLEStartDrag: TvaSpreadPreviewScriptOLEStartDrag read FOnScriptOLEStartDrag write FOnScriptOLEStartDrag;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

implementation

uses ComObj;

class function CofpDataObjectFiles.Create: IfpDataObjectFiles;
begin
  Result := CreateComObject(CLASS_fpDataObjectFiles) as IfpDataObjectFiles;
end;

class function CofpDataObjectFiles.CreateRemote(const MachineName: string): IfpDataObjectFiles;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_fpDataObjectFiles) as IfpDataObjectFiles;
end;

procedure TfpDataObjectFiles.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{69310C26-4993-11D1-8905-0020AF131A57}';
    IntfIID:   '{69310C25-4993-11D1-8905-0020AF131A57}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TfpDataObjectFiles.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IfpDataObjectFiles;
  end;
end;

procedure TfpDataObjectFiles.ConnectTo(svrIntf: IfpDataObjectFiles);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TfpDataObjectFiles.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TfpDataObjectFiles.GetDefaultInterface: IfpDataObjectFiles;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TfpDataObjectFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TfpDataObjectFilesProperties.Create(Self);
{$ENDIF}
end;

destructor TfpDataObjectFiles.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TfpDataObjectFiles.GetServerProperties: TfpDataObjectFilesProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TfpDataObjectFiles.Get_Item(lIndex: Integer): WideString;
begin
  Result := DefaultInterface.Item[lIndex];
end;

function  TfpDataObjectFiles.Get_Count: Integer;
begin
  Result := DefaultInterface.Count;
end;

procedure TfpDataObjectFiles.Add(const bstrFilename: WideString);
begin
  DefaultInterface.Add(bstrFilename, EmptyParam);
end;

procedure TfpDataObjectFiles.Add(const bstrFilename: WideString; vIndex: OleVariant);
begin
  DefaultInterface.Add(bstrFilename, vIndex);
end;

procedure TfpDataObjectFiles.Clear;
begin
  DefaultInterface.Clear;
end;

procedure TfpDataObjectFiles.Remove(vIndex: OleVariant);
begin
  DefaultInterface.Remove(vIndex);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TfpDataObjectFilesProperties.Create(AServer: TfpDataObjectFiles);
begin
  inherited Create;
  FServer := AServer;
end;

function TfpDataObjectFilesProperties.GetDefaultInterface: IfpDataObjectFiles;
begin
  Result := FServer.DefaultInterface;
end;

function  TfpDataObjectFilesProperties.Get_Item(lIndex: Integer): WideString;
begin
  Result := DefaultInterface.Item[lIndex];
end;

function  TfpDataObjectFilesProperties.Get_Count: Integer;
begin
  Result := DefaultInterface.Count;
end;

{$ENDIF}

class function CofpDataObject.Create: IfpDataObject;
begin
  Result := CreateComObject(CLASS_fpDataObject) as IfpDataObject;
end;

class function CofpDataObject.CreateRemote(const MachineName: string): IfpDataObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_fpDataObject) as IfpDataObject;
end;

procedure TfpDataObject.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{69310C28-4993-11D1-8905-0020AF131A57}';
    IntfIID:   '{69310C27-4993-11D1-8905-0020AF131A57}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TfpDataObject.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IfpDataObject;
  end;
end;

procedure TfpDataObject.ConnectTo(svrIntf: IfpDataObject);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TfpDataObject.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TfpDataObject.GetDefaultInterface: IfpDataObject;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TfpDataObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TfpDataObjectProperties.Create(Self);
{$ENDIF}
end;

destructor TfpDataObject.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TfpDataObject.GetServerProperties: TfpDataObjectProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TfpDataObject.Get_Files: IfpDataObjectFiles;
begin
  Result := DefaultInterface.Files;
end;

procedure TfpDataObject.Clear;
begin
  DefaultInterface.Clear;
end;

function  TfpDataObject.GetData(nFormat: Smallint): OleVariant;
begin
  Result := DefaultInterface.GetData(nFormat);
end;

function  TfpDataObject.GetFormat(nFormat: Smallint): WordBool;
begin
  Result := DefaultInterface.GetFormat(nFormat);
end;

procedure TfpDataObject.SetData;
begin
  DefaultInterface.SetData(EmptyParam, EmptyParam);
end;

procedure TfpDataObject.SetData(vValue: OleVariant);
begin
  DefaultInterface.SetData(vValue, EmptyParam);
end;

procedure TfpDataObject.SetData(vValue: OleVariant; vFormat: OleVariant);
begin
  DefaultInterface.SetData(vValue, vFormat);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TfpDataObjectProperties.Create(AServer: TfpDataObject);
begin
  inherited Create;
  FServer := AServer;
end;

function TfpDataObjectProperties.GetDefaultInterface: IfpDataObject;
begin
  Result := FServer.DefaultInterface;
end;

function  TfpDataObjectProperties.Get_Files: IfpDataObjectFiles;
begin
  Result := DefaultInterface.Files;
end;

{$ENDIF}

procedure TvaSpread.InitControlData;
const
  CEventDispIDs: array [0..58] of DWORD = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $00000007, $00000009, $0000000A, $0000000B, $0000000C, $0000000D,
    $0000000E, $0000000F, $00000010, $00000011, $00000012, $00000013,
    $00000014, $00000015, $00000016, $00000017, $00000018, $00000019,
    $0000001A, $0000001B, $0000001C, $0000001D, $0000001E, $0000001F,
    $00000020, $00000021, $00000022, $00000023, $00000024, $00000025,
    $00000026, $00000027, $00000028, $00000029, $00000065, $00000066,
    $00000067, $00000068, $00000069, $0000006A, $0000006B, $0000006C,
    $0000006D, $0000006E, $0000006F, $00000070, $00000071, $00000072,
    $00000073, $00000075, $00000076, $00000077, $00000078);
  CLicenseKey: array[0..7] of Word = ( $7254, $6169, $206C, $6576, $7372, $6F69, $006E, $0000);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CTPictureIDs: array [0..3] of DWORD = (
    $000000D5, $00000088, $00000087, $000000B2);
  CControlData: TControlData2 = (
    ClassID: '{41F841C1-AE16-11D5-8817-0050DA6EF5E5}';
    EventIID: '{41F841C5-AE16-11D5-8817-0050DA6EF5E5}';
    EventCount: 59;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: @CLicenseKey;
    Flags: $0000002F;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs;
    PictureCount: 4;
    PictureIDs: @CTPictureIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnAdvance) - Cardinal(Self);
end;

procedure TvaSpread.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DSpreadSheet;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TvaSpread.GetControlInterface: _DSpreadSheet;
begin
  CreateControl;
  Result := FIntf;
end;

function  TvaSpread.Get_ColUserSortIndicator(lCol: Integer): ColUserSortIndicatorConstants;
begin
  Result := DefaultInterface.ColUserSortIndicator[lCol];
end;

procedure TvaSpread.Set_ColUserSortIndicator(lCol: Integer; Param2: ColUserSortIndicatorConstants);
begin
  DefaultInterface.ColUserSortIndicator[lCol] := Param2;
end;

function  TvaSpread.Get_MaxTextRowHeight(lRow: Integer): Double;
begin
  Result := DefaultInterface.MaxTextRowHeight[lRow];
end;

procedure TvaSpread.Set_ColWidth(lCol: Integer; Param2: Double);
begin
  DefaultInterface.ColWidth[lCol] := Param2;
end;

function  TvaSpread.Get_TypeCheckPicture(Index: Smallint): IPictureDisp;
begin
  Result := DefaultInterface.TypeCheckPicture[Index];
end;

function  TvaSpread.Get_MaxTextColWidth(lCol: Integer): Double;
begin
  Result := DefaultInterface.MaxTextColWidth[lCol];
end;

procedure TvaSpread.Set_MaxTextColWidth(lCol: Integer; Param2: Double);
begin
  DefaultInterface.MaxTextColWidth[lCol] := Param2;
end;

procedure TvaSpread.Set_SortKeyOrder(nIndex: Smallint; Param2: SortKeyOrderConstants);
begin
  DefaultInterface.SortKeyOrder[nIndex] := Param2;
end;

function  TvaSpread.Get_SortKey(nIndex: Smallint): Integer;
begin
  Result := DefaultInterface.SortKey[nIndex];
end;

procedure TvaSpread.Set_MaxTextRowHeight(lRow: Integer; Param2: Double);
begin
  DefaultInterface.MaxTextRowHeight[lRow] := Param2;
end;

function  TvaSpread.Get_ColWidth(lCol: Integer): Double;
begin
  Result := DefaultInterface.ColWidth[lCol];
end;

procedure TvaSpread.Set_SortKey(nIndex: Smallint; Param2: Integer);
begin
  DefaultInterface.SortKey[nIndex] := Param2;
end;

function  TvaSpread.Get_SortKeyOrder(nIndex: Smallint): SortKeyOrderConstants;
begin
  Result := DefaultInterface.SortKeyOrder[nIndex];
end;

procedure TvaSpread.Set_TypeCheckPicture(Index: Smallint; const Param2: IPictureDisp);
begin
  DefaultInterface.TypeCheckPicture[Index] := Param2;
end;

function  TvaSpread.Get_RowHeight(lRow: Integer): Double;
begin
  Result := DefaultInterface.RowHeight[lRow];
end;

procedure TvaSpread.Set_RowHeight(lRow: Integer; Param2: Double);
begin
  DefaultInterface.RowHeight[lRow] := Param2;
end;

function  TvaSpread.Get_EventEnabled(Event: EVENTENABLEDConstants): WordBool;
begin
  Result := DefaultInterface.EventEnabled[Event];
end;

procedure TvaSpread.Set_EventEnabled(Event: EVENTENABLEDConstants; Param2: WordBool);
begin
  DefaultInterface.EventEnabled[Event] := Param2;
end;

function TvaSpread.Get_DataSource: _DSpreadCursor;
begin
  Result := DefaultInterface.DataSource;
end;

procedure TvaSpread.Set_DataSource(const Value: _DSpreadCursor);
begin
  DefaultInterface.DataSource := Value;
end;

function  TvaSpread.OwnerPrintDraw(hDC: OLE_HANDLE; Left: Integer; Top: Integer; Right: Integer; 
                                   Bottom: Integer; Page: Smallint): WordBool;
begin
  Result := DefaultInterface.OwnerPrintDraw(hDC, Left, Top, Right, Bottom, Page); {<<<<<}
end;

function  TvaSpread.OwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer;
                                        Right: Integer; Bottom: Integer; var PageCount: Smallint): WordBool;
begin
  Result := DefaultInterface.OwnerPrintPageCount(hDC, Left, Top, Right, Bottom, PageCount); {<<<<<}
end;

function  TvaSpread.ExportRangeToTextFile(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                          const FileName: WideString; const CellDelim: WideString; 
                                          const ColDelim: WideString; const RowDelim: WideString; 
                                          Flags: ExportRangeToTextFileConstants; 
                                          const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportRangeToTextFile(Col, Row, Col2, Row2, FileName, CellDelim, ColDelim, {<<<<<}
                                         RowDelim, Flags, LogFile);
end;

function  TvaSpread.ExportToTextFile(const FileName: WideString; const CellDelim: WideString; 
                                     const ColDelim: WideString; const RowDelim: WideString; 
                                     Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToTextFile(FileName, CellDelim, ColDelim, RowDelim, Flags, LogFile); {<<<<<}
end;

function  TvaSpread.ExportRangeToXML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                     const FileName: WideString; const Root: WideString; 
                                     const Collection: WideString; Flags: ExportToXMLConstants; 
                                     const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportRangeToXML(Col, Row, Col2, Row2, FileName, Root, Collection, Flags, LogFile); {<<<<<}
end;

function  TvaSpread.ExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                      var Buff: WideString; Flags: ExportToXMLConstants; 
                                      const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToXMLBuffer(Root, Collection, Buff, Flags, LogFile); {<<<<<}
end;

function  TvaSpread.ExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; 
                                           Row2: Integer; const Root: WideString; 
                                           const Collection: WideString; var Buff: WideString; 
                                           Flags: ExportToXMLConstants; const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportRangeToXMLBuffer(Col, Row, Col2, Row2, Root, Collection, Buff, Flags, {<<<<<}
                                          LogFile);
end;

procedure TvaSpread.ScriptGetFirstValidCell(var Col: OleVariant; var Row: OleVariant);
begin
  DefaultInterface.ScriptGetFirstValidCell(Col, Row);
end;

function  TvaSpread.ScriptGetIteration(var MaxIterations: OleVariant; var MaxChange: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetIteration(MaxIterations, MaxChange); {<<<<<}
end;

procedure TvaSpread.ScriptCFGetCellParam(Param: Smallint; var Col: OleVariant; var Row: OleVariant);
begin
  DefaultInterface.ScriptCFGetCellParam(Param, Col, Row);
end;

function  TvaSpread.ExportToXML(const FileName: WideString; const Root: WideString; 
                                const Collection: WideString; Flags: ExportToXMLConstants; 
                                const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToXML(FileName, Root, Collection, Flags, LogFile); {<<<<<}
end;

function  TvaSpread.LoadTextFile(const FileName: WideString; const CellDelim: WideString; 
                                 const ColDelim: WideString; const RowDelim: WideString; 
                                 Flags: LoadTextFileConstants; const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.LoadTextFile(FileName, CellDelim, ColDelim, RowDelim, Flags, LogFile); {<<<<<}
end;

procedure TvaSpread.SetCalTextOverride(const ShortDays: WideString; const LongDays: WideString; 
                                       const ShortMonths: WideString; const LongMonths: WideString; 
                                       const OkText: WideString; const CancelText: WideString);
begin
  DefaultInterface.SetCalTextOverride(ShortDays, LongDays, ShortMonths, LongMonths, OkText, 
                                      CancelText);
end;

procedure TvaSpread.GetCalTextOverride(var ShortDays: WideString; var LenShortDays: Smallint; 
                                       var LongDays: WideString; var LenLongDays: Smallint; 
                                       var ShortMonths: WideString; var LenShortMonths: Smallint; 
                                       var LongMonths: WideString; var LenLongMonths: Smallint; 
                                       var OkText: WideString; var LenOkText: Smallint; 
                                       var CancelText: WideString; var LenCancelText: Smallint);
begin
  DefaultInterface.GetCalTextOverride(ShortDays, LenShortDays, LongDays, LenLongDays, ShortMonths, 
                                      LenShortMonths, LongMonths, LenLongMonths, OkText, LenOkText, 
                                      CancelText, LenCancelText);
end;

function  TvaSpread.IsFetchCellNote: WordBool;
begin
  Result := DefaultInterface.IsFetchCellNote; {<<<<<}
end;

function  TvaSpread.GetItemData: Integer;
begin
  Result := DefaultInterface.GetItemData; {<<<<<}
end;

function  TvaSpread.GetIteration(var MaxIterations: Smallint; var MaxChange: Double): WordBool;
begin
  Result := DefaultInterface.GetIteration(MaxIterations, MaxChange); {<<<<<}
end;

function  TvaSpread.GetDataFillData(var Var_: OleVariant; VarType: Smallint): WordBool;
begin
  Result := DefaultInterface.GetDataFillData(Var_, VarType); {<<<<<}
end;

procedure TvaSpread.GetFirstValidCell(var Col: Integer; var Row: Integer);
begin
  DefaultInterface.GetFirstValidCell(Col, Row);
end;

function  TvaSpread.CFGetStringParam(Param: Smallint): WideString;
begin
  Result := DefaultInterface.CFGetStringParam(Param); {<<<<<}
end;

procedure TvaSpread.CFGetCellParam(Param: Smallint; var Col: Integer; var Row: Integer);
begin
  DefaultInterface.CFGetCellParam(Param, Col, Row);
end;

function  TvaSpread.AddCustomFunction(const FunctionName: WideString; ParameterCnt: Smallint): WordBool;
begin
  Result := DefaultInterface.AddCustomFunction(FunctionName, ParameterCnt); {<<<<<}
end;

function  TvaSpread.CFGetDoubleParam(Param: Smallint): Double;
begin
  Result := DefaultInterface.CFGetDoubleParam(Param); {<<<<<}
end;

function  TvaSpread.CFGetDoubleParamExt(Param: Smallint; var ParamValue: Double): Double;
begin
  Result := DefaultInterface.CFGetDoubleParamExt(Param, ParamValue); {<<<<<}
end;

function  TvaSpread.CFGetLongParam(Param: Smallint): Integer;
begin
  Result := DefaultInterface.CFGetLongParam(Param); {<<<<<}
end;

function  TvaSpread.AddCustomFunctionExt(const FunctionName: WideString; MinParamCnt: Smallint; 
                                         MaxParamCnt: Smallint; Flags: Integer): WordBool;
begin
  Result := DefaultInterface.AddCustomFunctionExt(FunctionName, MinParamCnt, MaxParamCnt, Flags); {<<<<<}
end;

procedure TvaSpread.CFSetResult(Var_: OleVariant);
begin
  DefaultInterface.CFSetResult(Var_);
end;

function  TvaSpread.CFGetParamInfo(Param: Smallint; var Type_: Smallint; var Status: Smallint): WordBool;
begin
  Result := DefaultInterface.CFGetParamInfo(Param, Type_, Status); {<<<<<}
end;

procedure TvaSpread.CFGetRangeParam(Param: Smallint; var Col: Integer; var Row: Integer; 
                                    var Col2: Integer; var Row2: Integer);
begin
  DefaultInterface.CFGetRangeParam(Param, Col, Row, Col2, Row2);
end;

function  TvaSpread.ColNumberToLetter(HeaderNumber: Integer): WideString;
begin
  Result := DefaultInterface.ColNumberToLetter(HeaderNumber); {<<<<<}
end;

procedure TvaSpread.ColWidthToTwips(Width: Single; var Twips: Integer);
begin
  DefaultInterface.ColWidthToTwips(Width, Twips);
end;

procedure TvaSpread.GetBottomRightCell(var Col: Integer; var Row: Integer);
begin
  DefaultInterface.GetBottomRightCell(Col, Row);
end;

function  TvaSpread.GetColItemData(Col: Integer): Integer;
begin
  Result := DefaultInterface.GetColItemData(Col); {<<<<<}
end;

function  TvaSpread.QueryCustomName(const Name: WideString): WideString;
begin
  Result := DefaultInterface.QueryCustomName(Name); {<<<<<}
end;

function  TvaSpread.GetCustomName(const Name: WideString): WideString;
begin
  Result := DefaultInterface.GetCustomName(Name); {<<<<<}
end;

function  TvaSpread.SaveToFile(const FileName: WideString; DataOnly: WordBool): WordBool;
begin
  Result := DefaultInterface.SaveToFile(FileName, DataOnly); {<<<<<}
end;

function  TvaSpread.SetCellDirtyFlag(Col: Integer; Row: Integer; Dirty: WordBool): WordBool;
begin
  Result := DefaultInterface.SetCellDirtyFlag(Col, Row, Dirty); {<<<<<}
end;

function  TvaSpread.GetRowItemData(Row: Integer): Integer;
begin
  Result := DefaultInterface.GetRowItemData(Row); {<<<<<}
end;

procedure TvaSpread.GetClientArea(var Width: Integer; var Height: Integer);
begin
  DefaultInterface.GetClientArea(Width, Height);
end;

procedure TvaSpread.GetLastValidCell(var Col: Integer; var Row: Integer);
begin
  DefaultInterface.GetLastValidCell(Col, Row);
end;

function  TvaSpread.GetMultiSelItem(SelPrev: Integer): Integer;
begin
  Result := DefaultInterface.GetMultiSelItem(SelPrev); {<<<<<}
end;

function  TvaSpread.GetRefStyle: Smallint;
begin
  Result := DefaultInterface.GetRefStyle; {<<<<<}
end;

function  TvaSpread.GetCellDirtyFlag(Col: Integer; Row: Integer): WordBool;
begin
  Result := DefaultInterface.GetCellDirtyFlag(Col, Row); {<<<<<}
end;

procedure TvaSpread.GetCellFromScreenCoord(var Col: Integer; var Row: Integer; x: Integer; 
                                           y: Integer);
begin
  DefaultInterface.GetCellFromScreenCoord(Col, Row, x, y);
end;

function  TvaSpread.GetCellPos(Col: Integer; Row: Integer; var x: Integer; var y: Integer; 
                               var Width: Integer; var Height: Integer): WordBool;
begin
  Result := DefaultInterface.GetCellPos(Col, Row, x, y, Width, Height); {<<<<<}
end;

function  TvaSpread.SaveTabFile(const FileName: WideString): WordBool;
begin
  Result := DefaultInterface.SaveTabFile(FileName); {<<<<<}
end;

function  TvaSpread.LoadTabFile(const FileName: WideString): WordBool;
begin
  Result := DefaultInterface.LoadTabFile(FileName); {<<<<<}
end;

procedure TvaSpread.RowHeightToTwips(Row: Integer; Height: Single; var Twips: Integer);
begin
  DefaultInterface.RowHeightToTwips(Row, Height, Twips);
end;

function  TvaSpread.IsVisible(Col: Integer; Row: Integer; Partial: WordBool): WordBool;
begin
  Result := DefaultInterface.IsVisible(Col, Row, Partial); {<<<<<}
end;

function  TvaSpread.LoadFromFile(const FileName: WideString): WordBool;
begin
  Result := DefaultInterface.LoadFromFile(FileName); {<<<<<}
end;

function  TvaSpread.GetText(Col: Integer; Row: Integer; var Var_: OleVariant): WordBool;
begin
  Result := DefaultInterface.GetText(Col, Row, Var_); {<<<<<}
end;

function  TvaSpread.SetDataFillData(Var_: OleVariant): WordBool;
begin
  Result := DefaultInterface.SetDataFillData(Var_); {<<<<<}
end;

procedure TvaSpread.SetItemData(Value: Integer);
begin
  DefaultInterface.SetItemData(Value);
end;

procedure TvaSpread.SetColItemData(Col: Integer; Value: Integer);
begin
  DefaultInterface.SetColItemData(Col, Value);
end;

function  TvaSpread.IsCellSelected(Col: Integer; Row: Integer): WordBool;
begin
  Result := DefaultInterface.IsCellSelected(Col, Row); {<<<<<}
end;

function  TvaSpread.IsFormulaValid(const Formula: WideString): WordBool;
begin
  Result := DefaultInterface.IsFormulaValid(Formula); {<<<<<}
end;

function  TvaSpread.SetCustomName(const Name: WideString; const Value: WideString): WordBool;
begin
  Result := DefaultInterface.SetCustomName(Name, Value); {<<<<<}
end;

function  TvaSpread.SetTextTipAppearance(const FontName: WideString; FontSize: Smallint; 
                                         FontBold: WordBool; FontItalic: WordBool; 
                                         BackColor: Integer; ForeColor: Integer): WordBool;
begin
  Result := DefaultInterface.SetTextTipAppearance(FontName, FontSize, FontBold, FontItalic, BackColor, {<<<<<}
                                        ForeColor);
end;

function  TvaSpread.ExportToHTML(const FileName: WideString; AppendFlag: WordBool; 
                                 const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToHTML(FileName, AppendFlag, LogFile); {<<<<<}
end;

function  TvaSpread.ExportRangeToHTML(Col: Integer; Row: Integer; Col2: Integer; Row2: Integer; 
                                      const FileName: WideString; AppendFlag: WordBool; 
                                      const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportRangeToHTML(Col, Row, Col2, Row2, FileName, AppendFlag, LogFile); {<<<<<}
end;

function  TvaSpread.IsExcelFile(const FileName: WideString): Smallint;
begin
  Result := DefaultInterface.IsExcelFile(FileName); {<<<<<}
end;

function  TvaSpread.GetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                      var ListCount: Smallint; const LogFileName: WideString; 
                                      var WorkbookHandle: Smallint; Replace: WordBool): WordBool;
begin
  Result := DefaultInterface.GetExcelSheetList(FileName, VarArray, ListCount, LogFileName, WorkbookHandle, {<<<<<}
                                     Replace);
end;

function  TvaSpread.GetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool;
begin
  Result := DefaultInterface.GetArray(ColLeft, RowTop, VarArray); {<<<<<}
end;

function  TvaSpread.RemoveCustomFunction(const FuncName: WideString): WordBool;
begin
  Result := DefaultInterface.RemoveCustomFunction(FuncName); {<<<<<}
end;

function  TvaSpread.ImportExcelSheet(WorkbookHandle: Smallint; Sheet: OleVariant): WordBool;
begin
  Result := DefaultInterface.ImportExcelSheet(WorkbookHandle, Sheet); {<<<<<}
end;

function  TvaSpread.ExportToExcel(const FileName: WideString; const SheetName: WideString; 
                                  const LogFileName: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToExcel(FileName, SheetName, LogFileName); {<<<<<}
end;

function  TvaSpread.EnumCustomFunction(const PrevFuncName: WideString; var FuncName: WideString): WordBool;
begin
  Result := DefaultInterface.EnumCustomFunction(PrevFuncName, FuncName); {<<<<<}
end;

function  TvaSpread.ReCalcCell(Col: Integer; Row: Integer): WordBool;
begin
  Result := DefaultInterface.ReCalcCell(Col, Row); {<<<<<}
end;

procedure TvaSpread.OLEDrag;
begin
  DefaultInterface.OLEDrag;
end;

function  TvaSpread.GetCustomFunction(const FuncName: WideString; var MinArgs: Smallint; 
                                      var MaxArgs: Smallint; var Flags: Integer): WordBool;
begin
  Result := DefaultInterface.GetCustomFunction(FuncName, MinArgs, MaxArgs, Flags); {<<<<<}
end;

function  TvaSpread.SetArray(ColLeft: Integer; RowTop: Integer; VarArray: OleVariant): WordBool;
begin
  Result := DefaultInterface.SetArray(ColLeft, RowTop, VarArray); {<<<<<}
end;

procedure TvaSpread.Reset;
begin
  DefaultInterface.Reset;
end;

procedure TvaSpread.SetActiveCell(lCol: Integer; lRow: Integer);
begin
  DefaultInterface.SetActiveCell(lCol, lRow);
end;

procedure TvaSpread.SetCellBorder(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                                  nIndex: Smallint; crColor: OLE_COLOR; 
                                  nStyle: CellBorderStyleConstants);
begin
  DefaultInterface.SetCellBorder(lCol, lRow, lCol2, lRow2, nIndex, crColor, nStyle);
end;

function  TvaSpread.Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                         nSortBy: SortByConstants): WordBool;
begin
  Result := DefaultInterface.Sort(lCol, lRow, lCol2, lRow2, nSortBy, EmptyParam, EmptyParam); {<<<<<}
end;

function  TvaSpread.Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                         nSortBy: SortByConstants; var SortKeys: OleVariant): WordBool;
begin
  Result := DefaultInterface.Sort(lCol, lRow, lCol2, lRow2, nSortBy, SortKeys, EmptyParam); {<<<<<}
end;

function  TvaSpread.Sort(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                         nSortBy: SortByConstants; var SortKeys: OleVariant; 
                         var SortKeyOrders: OleVariant): WordBool;
begin
  Result := DefaultInterface.Sort(lCol, lRow, lCol2, lRow2, nSortBy, SortKeys, SortKeyOrders); {<<<<<}
end;

procedure TvaSpread.SwapRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                              lColDest: Integer; lRowDest: Integer);
begin
  DefaultInterface.SwapRange(lCol, lRow, lCol2, lRow2, lColDest, lRowDest);
end;

procedure TvaSpread.SetSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer);
begin
  DefaultInterface.SetSelection(lCol, lRow, lCol2, lRow2);
end;

function  TvaSpread.GetCellSpan(lCol: Integer; lRow: Integer; var pvColAnchor: OleVariant; 
                                var pvRowAnchor: OleVariant; var pvNumCols: OleVariant; 
                                var pvNumRows: OleVariant): GetCellSpanConstants;
begin
  Result := DefaultInterface.GetCellSpan(lCol, lRow, pvColAnchor, pvRowAnchor, pvNumCols, pvNumRows); {<<<<<}
end;

procedure TvaSpread.TypeComboBoxClear(lCol: Integer; lRow: Integer);
begin
  DefaultInterface.TypeComboBoxClear(lCol, lRow);
end;

procedure TvaSpread.TypeComboBoxRemoveItem(lCol: Integer; lRow: Integer; nIndex: Smallint);
begin
  DefaultInterface.TypeComboBoxRemoveItem(lCol, lRow, nIndex);
end;

procedure TvaSpread.VirtualRefresh;
begin
  DefaultInterface.VirtualRefresh;
end;

procedure TvaSpread.RemoveCellSpan(lCol: Integer; lRow: Integer);
begin
  DefaultInterface.RemoveCellSpan(lCol, lRow);
end;

function  TvaSpread.GetColFromID(const ColID: WideString): Integer;
begin
  Result := DefaultInterface.GetColFromID(ColID); {<<<<<}
end;

function  TvaSpread.AddCellSpan(lCol: Integer; lRow: Integer; lNumCols: Integer; lNumRows: Integer): WordBool;
begin
  Result := DefaultInterface.AddCellSpan(lCol, lRow, lNumCols, lNumRows); {<<<<<}
end;

function  TvaSpread.ScriptGetCellPos(Col: Integer; Row: Integer; var x: OleVariant; 
                                     var y: OleVariant; var Width: OleVariant; 
                                     var Height: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetCellPos(Col, Row, x, y, Width, Height); {<<<<<}
end;

procedure TvaSpread.ScriptColWidthToTwips(Width: Single; var Twips: OleVariant);
begin
  DefaultInterface.ScriptColWidthToTwips(Width, Twips);
end;

function  TvaSpread.ScriptCFGetDoubleParamExt(Param: Smallint; var ParamValue: OleVariant): Double;
begin
  Result := DefaultInterface.ScriptCFGetDoubleParamExt(Param, ParamValue); {<<<<<}
end;

function  TvaSpread.GetFloat(Col: Integer; Row: Integer; var Value: Double): WordBool;
begin
  Result := DefaultInterface.GetFloat(Col, Row, Value); {<<<<<}
end;

procedure TvaSpread.ScriptGetBottomRightCell(var Col: OleVariant; var Row: OleVariant);
begin
  DefaultInterface.ScriptGetBottomRightCell(Col, Row);
end;

procedure TvaSpread.ScriptGetCellFromScreenCoord(var Col: OleVariant; var Row: OleVariant; 
                                                 x: Integer; y: Integer);
begin
  DefaultInterface.ScriptGetCellFromScreenCoord(Col, Row, x, y);
end;

function  TvaSpread.ScriptCFGetParamInfo(Param: Smallint; var Type_: OleVariant; 
                                         var Status: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptCFGetParamInfo(Param, Type_, Status); {<<<<<}
end;

procedure TvaSpread.ScriptTwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: OleVariant);
begin
  DefaultInterface.ScriptTwipsToRowHeight(Row, Twips, RowHeight);
end;

procedure TvaSpread.ScriptGetLastValidCell(var Col: OleVariant; var Row: OleVariant);
begin
  DefaultInterface.ScriptGetLastValidCell(Col, Row);
end;

procedure TvaSpread.ScriptGetClientArea(var Width: OleVariant; var Height: OleVariant);
begin
  DefaultInterface.ScriptGetClientArea(Width, Height);
end;

procedure TvaSpread.ScriptCFGetRangeParam(Param: Smallint; var Col: OleVariant; 
                                          var Row: OleVariant; var Col2: OleVariant; 
                                          var Row2: OleVariant);
begin
  DefaultInterface.ScriptCFGetRangeParam(Param, Col, Row, Col2, Row2);
end;

procedure TvaSpread.ScriptRowHeightToTwips(Row: Integer; Height: Single; var Twips: OleVariant);
begin
  DefaultInterface.ScriptRowHeightToTwips(Row, Height, Twips);
end;

procedure TvaSpread.ScriptTwipsToColWidth(Twips: Integer; var ColWidth: OleVariant);
begin
  DefaultInterface.ScriptTwipsToColWidth(Twips, ColWidth);
end;

function  TvaSpread.SetOddEvenRowColor(clrBackOdd: Integer; clrForeOdd: Integer; 
                                       clrBackEven: Integer; clrForeEven: Integer): WordBool;
begin
  Result := DefaultInterface.SetOddEvenRowColor(clrBackOdd, clrForeOdd, clrBackEven, clrForeEven); {<<<<<}
end;

function  TvaSpread.SetActionKey(Action: Smallint; fShift: WordBool; fCtrl: WordBool; Key: Smallint): WordBool;
begin
  Result := DefaultInterface.SetActionKey(Action, fShift, fCtrl, Key); {<<<<<}
end;

procedure TvaSpread.TwipsToColWidth(Twips: Integer; var ColWidth: Single);
begin
  DefaultInterface.TwipsToColWidth(Twips, ColWidth);
end;

procedure TvaSpread.SetRefStyle(RefStyle: Smallint);
begin
  DefaultInterface.SetRefStyle(RefStyle);
end;

function  TvaSpread.GetTextTipAppearance(var FontName: WideString; var FontSize: Smallint; 
                                         var FontBold: WordBool; var FontItalic: WordBool; 
                                         var BackColor: Integer; var ForeColor: Integer): WordBool;
begin
  Result := DefaultInterface.GetTextTipAppearance(FontName, FontSize, FontBold, FontItalic, BackColor, {<<<<<}
                                        ForeColor);
end;

procedure TvaSpread.TwipsToRowHeight(Row: Integer; Twips: Integer; var RowHeight: Single);
begin
  DefaultInterface.TwipsToRowHeight(Row, Twips, RowHeight);
end;

function  TvaSpread.GetActionKey(Action: Smallint; var fShift: WordBool; var fCtrl: WordBool; 
                                 var Key: Smallint): WordBool;
begin
  Result := DefaultInterface.GetActionKey(Action, fShift, fCtrl, Key); {<<<<<}
end;

procedure TvaSpread.SetRowItemData(Row: Integer; Value: Integer);
begin
  DefaultInterface.SetRowItemData(Row, Value);
end;

procedure TvaSpread.SetCalText(const ShortDays: WideString; const LongDays: WideString; 
                               const ShortMonths: WideString; const LongMonths: WideString; 
                               const OkText: WideString; const CancelText: WideString);
begin
  DefaultInterface.SetCalText(ShortDays, LongDays, ShortMonths, LongMonths, OkText, CancelText);
end;

function  TvaSpread.SetFloat(Col: Integer; Row: Integer; Value: Double): WordBool;
begin
  Result := DefaultInterface.SetFloat(Col, Row, Value); {<<<<<}
end;

function  TvaSpread.GetOddEvenRowColor(var clrBackOdd: Integer; var clrForeOdd: Integer; 
                                       var clrBackEven: Integer; var clrForeEven: Integer): WordBool;
begin
  Result := DefaultInterface.GetOddEvenRowColor(clrBackOdd, clrForeOdd, clrBackEven, clrForeEven); {<<<<<}
end;

procedure TvaSpread.SetText(Col: Integer; Row: Integer; Var_: OleVariant);
begin
  DefaultInterface.SetText(Col, Row, Var_);
end;

function  TvaSpread.GetInteger(Col: Integer; Row: Integer; var Value: Integer): WordBool;
begin
  Result := DefaultInterface.GetInteger(Col, Row, Value); {<<<<<}
end;

function  TvaSpread.SetInteger(Col: Integer; Row: Integer; Value: Integer): WordBool;
begin
  Result := DefaultInterface.SetInteger(Col, Row, Value); {<<<<<}
end;

procedure TvaSpread.MoveRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                              lColDest: Integer; lRowDest: Integer);
begin
  DefaultInterface.MoveRange(lCol, lRow, lCol2, lRow2, lColDest, lRowDest);
end;

procedure TvaSpread.ReCalc;
begin
  DefaultInterface.ReCalc;
end;

procedure TvaSpread.InsertCols(lCol: Integer; lNumCols: Integer);
begin
  DefaultInterface.InsertCols(lCol, lNumCols);
end;

procedure TvaSpread.ClipboardCopy;
begin
  DefaultInterface.ClipboardCopy;
end;

procedure TvaSpread.ClipboardCut;
begin
  DefaultInterface.ClipboardCut;
end;

procedure TvaSpread.InsertRows(lRow: Integer; lNumRows: Integer);
begin
  DefaultInterface.InsertRows(lRow, lNumRows);
end;

procedure TvaSpread.DataSave;
begin
  DefaultInterface.DataSave;
end;

function  TvaSpread.ScriptGetTextTipAppearance(var FontName: OleVariant; var FontSize: OleVariant; 
                                               var FontBold: OleVariant; 
                                               var FontItalic: OleVariant; 
                                               var BackColor: OleVariant; var ForeColor: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetTextTipAppearance(FontName, FontSize, FontBold, FontItalic, BackColor, {<<<<<}
                                              ForeColor);
end;

function  TvaSpread.ScriptGetExcelSheetList(const FileName: WideString; VarArray: OleVariant; 
                                            var ListCount: OleVariant; 
                                            const LogFileName: WideString; 
                                            var WorkbookHandle: OleVariant; Replace: WordBool): WordBool;
begin
  Result := DefaultInterface.ScriptGetExcelSheetList(FileName, VarArray, ListCount, LogFileName, {<<<<<}
                                           WorkbookHandle, Replace);
end;

function  TvaSpread.ScriptEnumCustomFunction(const PrevFuncName: WideString; 
                                             var FuncName: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptEnumCustomFunction(PrevFuncName, FuncName); {<<<<<}
end;

procedure TvaSpread.GetCellBorder(lCol: Integer; lRow: Integer; nIndex: Smallint; 
                                  var pcrColor: OleVariant; var pnStyle: OleVariant);
begin
  DefaultInterface.GetCellBorder(lCol, lRow, nIndex, pcrColor, pnStyle);
end;

procedure TvaSpread.GetSelection(lIndex: Integer; var plCol: OleVariant; var plRow: OleVariant; 
                                 var plCol2: OleVariant; var plRow2: OleVariant);
begin
  DefaultInterface.GetSelection(lIndex, plCol, plRow, plCol2, plRow2);
end;

procedure TvaSpread.ClearRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                               bDataOnly: WordBool);
begin
  DefaultInterface.ClearRange(lCol, lRow, lCol2, lRow2, bDataOnly);
end;

procedure TvaSpread.ClearSelection;
begin
  DefaultInterface.ClearSelection;
end;

procedure TvaSpread.DeleteCols(lCol: Integer; lNumCols: Integer);
begin
  DefaultInterface.DeleteCols(lCol, lNumCols);
end;

procedure TvaSpread.DeleteRows(lRow: Integer; lNumRows: Integer);
begin
  DefaultInterface.DeleteRows(lRow, lNumRows);
end;

procedure TvaSpread.ClipboardPaste;
begin
  DefaultInterface.ClipboardPaste;
end;

procedure TvaSpread.SetIteration(Iteration: WordBool; MaxIterations: Smallint; MaxChange: Double);
begin
  DefaultInterface.SetIteration(Iteration, MaxIterations, MaxChange);
end;

procedure TvaSpread.CopyRange(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer; 
                              lColDest: Integer; lRowDest: Integer);
begin
  DefaultInterface.CopyRange(lCol, lRow, lCol2, lRow2, lColDest, lRowDest);
end;

procedure TvaSpread.MoveRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
begin
  DefaultInterface.MoveRowRange(lRow, lRow2, lRowDest);
end;

function  TvaSpread.SaveTabFileU(const FileName: WideString): WordBool;
begin
  Result := DefaultInterface.SaveTabFileU(FileName); {<<<<<}
end;

procedure TvaSpread.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TvaSpread.SwapColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
begin
  DefaultInterface.SwapColRange(lCol, lCol2, lColDest);
end;

procedure TvaSpread.CopyRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
begin
  DefaultInterface.CopyRowRange(lRow, lRow2, lRowDest);
end;

procedure TvaSpread.MoveColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
begin
  DefaultInterface.MoveColRange(lCol, lCol2, lColDest);
end;

procedure TvaSpread.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

function  TvaSpread.ExportRangeToTextFileU(Col: Integer; Row: Integer; Col2: Integer; 
                                           Row2: Integer; const FileName: WideString; 
                                           const CellDelim: WideString; const ColDelim: WideString; 
                                           const RowDelim: WideString; 
                                           Flags: ExportRangeToTextFileConstants; 
                                           const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportRangeToTextFileU(Col, Row, Col2, Row2, FileName, CellDelim, ColDelim, {<<<<<}
                                          RowDelim, Flags, LogFile);
end;

procedure TvaSpread.CopyColRange(lCol: Integer; lCol2: Integer; lColDest: Integer);
begin
  DefaultInterface.CopyColRange(lCol, lCol2, lColDest);
end;

procedure TvaSpread.ShowCell(lCol: Integer; lRow: Integer; nPosition: PositionConstants);
begin
  DefaultInterface.ShowCell(lCol, lRow, nPosition);
end;

function  TvaSpread.ExportToTextFileU(const FileName: WideString; const CellDelim: WideString; 
                                      const ColDelim: WideString; const RowDelim: WideString; 
                                      Flags: ExportToTextFileConstants; const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ExportToTextFileU(FileName, CellDelim, ColDelim, RowDelim, Flags, LogFile); {<<<<<}
end;

function  TvaSpread.SearchRow(lRow: Integer; lColStart: Integer; lColEnd: Integer; 
                              const Text: WideString; SearchFlags: SearchFlagsConstants): Integer;
begin
  Result := DefaultInterface.SearchRow(lRow, lColStart, lColEnd, Text, SearchFlags); {<<<<<}
end;

function  TvaSpread.ScriptGetFloat(Col: Integer; Row: Integer; var Value: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetFloat(Col, Row, Value); {<<<<<}
end;

procedure TvaSpread.ScriptGetCalTextOverride(var ShortDays: OleVariant; 
                                             var LenShortDays: OleVariant; 
                                             var LongDays: OleVariant; var LenLongDays: OleVariant; 
                                             var ShortMonths: OleVariant; 
                                             var LenShortMonths: OleVariant; 
                                             var LongMonths: OleVariant; 
                                             var LenLongMonths: OleVariant; var OkText: OleVariant; 
                                             var LenOkText: OleVariant; var CancelText: OleVariant; 
                                             var LenCancelText: OleVariant);
begin
  DefaultInterface.ScriptGetCalTextOverride(ShortDays, LenShortDays, LongDays, LenLongDays, 
                                            ShortMonths, LenShortMonths, LongMonths, LenLongMonths, 
                                            OkText, LenOkText, CancelText, LenCancelText);
end;

procedure TvaSpread.PrintSheet;
begin
  DefaultInterface.PrintSheet(EmptyParam);
end;

procedure TvaSpread.PrintSheet(var Flags: OleVariant);
begin
  DefaultInterface.PrintSheet(Flags);
end;

function  TvaSpread.ScriptGetInteger(Col: Integer; Row: Integer; var Value: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetInteger(Col, Row, Value); {<<<<<}
end;

function  TvaSpread.ScriptGetActionKey(Action: Smallint; var fShift: OleVariant; 
                                       var fCtrl: OleVariant; var Key: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetActionKey(Action, fShift, fCtrl, Key); {<<<<<}
end;

function  TvaSpread.ScriptGetOddEvenRowColor(var clrBackOdd: OleVariant; 
                                             var clrForeOdd: OleVariant; 
                                             var clrBackEven: OleVariant; 
                                             var clrForeEven: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetOddEvenRowColor(clrBackOdd, clrForeOdd, clrBackEven, clrForeEven); {<<<<<}
end;

procedure TvaSpread.AddSelection(lCol: Integer; lRow: Integer; lCol2: Integer; lRow2: Integer);
begin
  DefaultInterface.AddSelection(lCol, lRow, lCol2, lRow2);
end;

function  TvaSpread.ScriptExportRangeToXMLBuffer(Col: Integer; Row: Integer; Col2: Integer; 
                                                 Row2: Integer; const Root: WideString; 
                                                 const Collection: WideString; 
                                                 var Buff: OleVariant; Flags: ExportToXMLConstants; 
                                                 const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ScriptExportRangeToXMLBuffer(Col, Row, Col2, Row2, Root, Collection, Buff, {<<<<<}
                                                Flags, LogFile);
end;

procedure TvaSpread.SwapRowRange(lRow: Integer; lRow2: Integer; lRowDest: Integer);
begin
  DefaultInterface.SwapRowRange(lRow, lRow2, lRowDest);
end;

function  TvaSpread.SearchCol(lCol: Integer; lRowStart: Integer; lRowEnd: Integer; 
                              const Text: WideString; SearchFlags: SearchFlagsConstants): Integer;
begin
  Result := DefaultInterface.SearchCol(lCol, lRowStart, lRowEnd, Text, SearchFlags); {<<<<<}
end;

function  TvaSpread.ScriptOwnerPrintPageCount(hDC: OLE_HANDLE; Left: Integer; Top: Integer; 
                                              Right: Integer; Bottom: Integer; 
                                              var PageCount: OleVariant): Integer;
begin
  Result := DefaultInterface.ScriptOwnerPrintPageCount(hDC, Left, Top, Right, Bottom, PageCount); {<<<<<}
end;

function  TvaSpread.ScriptGetCustomFunction(const FuncName: WideString; var MinArgs: OleVariant; 
                                            var MaxArgs: OleVariant; var Flags: OleVariant): WordBool;
begin
  Result := DefaultInterface.ScriptGetCustomFunction(FuncName, MinArgs, MaxArgs, Flags); {<<<<<}
end;

function  TvaSpread.ScriptExportToXMLBuffer(const Root: WideString; const Collection: WideString; 
                                            var Buff: OleVariant; Flags: ExportToXMLConstants; 
                                            const LogFile: WideString): WordBool;
begin
  Result := DefaultInterface.ScriptExportToXMLBuffer(Root, Collection, Buff, Flags, LogFile); {<<<<<}
end;

procedure TvaSpreadPreview.InitControlData;
const
  CEventDispIDs: array [0..12] of DWORD = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $00000007, $00000008, $00000009, $0000000A, $0000000B, $0000000C,
    $0000000D);
  CLicenseKey: array[0..7] of Word = ( $7254, $6169, $206C, $6576, $7372, $6F69, $006E, $0000);
  CTPictureIDs: array [0..0] of DWORD = (
    $00000033);
  CControlData: TControlData2 = (
    ClassID: '{41F841C7-AE16-11D5-8817-0050DA6EF5E5}';
    EventIID: '{41F841CB-AE16-11D5-8817-0050DA6EF5E5}';
    EventCount: 13;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: @CLicenseKey;
    Flags: $00000008;
    Version: 401;
    FontCount: 0;
    FontIDs: nil;
    PictureCount: 1;
    PictureIDs: @CTPictureIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnPageChange) - Cardinal(Self);
end;

procedure TvaSpreadPreview.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DSpreadPreview;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TvaSpreadPreview.GetControlInterface: _DSpreadPreview;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TvaSpreadPreview.OLEDrag;
begin
  DefaultInterface.OLEDrag;
end;

procedure TvaSpreadPreview.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

procedure Register;
begin
  RegisterComponents('ActiveX',[TvaSpread, TvaSpreadPreview]);
  //RegisterComponents(dtlServerPage, [TfpDataObjectFiles, TfpDataObject]);
end;

end.
