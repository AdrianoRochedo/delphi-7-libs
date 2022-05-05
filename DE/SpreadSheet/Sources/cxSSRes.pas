{*******************************************************************}
{                                                                   }
{       Developer Express Cross platform Visual Component Library   }
{       ExpressSpreadSheet				            }
{                                                                   }
{       Copyright (c) 2001-2003 Developer Express Inc.              }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by U.S. and       }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES           }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE    }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS   }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET AND ALL           }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE      }
{   PROGRAM ONLY.                                                   }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                      }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}

unit cxSSRes;

interface

resourcestring
  scxSheetName = 'Sheet';

  // categories constants
  scxSpreadSheet = 'SpreadSheet';
  scxSpreadSheetDesc = 'SpreadSheet properties and/or events';

  // SpreadSheetBook errors
  scxSpreadSheetErrorReadSST        = 'Reading SST record error';
  scxSpreadSheetInvalidSheetNumber  = 'Invalid page number';
  scxSpreadSheetInvalidFileName     = 'Invalid file %s name';
  scxSpreadSheetInvalidFileFormat   = 'Invalid %s file format';
  scxSpreadSheetInvalidStreamFormat = 'Invalid stream format';
  scxSpreadSheetInvalidSheetCaption = 'Sheet caption invalid or already exist';
  scxSpreadSheetSheetPageExist      = 'Page already exists';
  scxSpreadSheetMergeCellError      = 'Merge cell error, certain cells already merged';
  scxChangePartOfMergeCells         = 'Cannot change part of merged cells';
  scxSpreadSheetMergeCellError2     = 'Some modify cells is merged';
  scxSpreadSheetDefineNameError     = 'Error define name, name already exist';
  scxSpreadSheetDefineNameError2    = 'Error define name, name contains extra characters';

  // sheet popup menu constants
  scxPopupMenuMergeCells  = 'Merge cells';
  scxPopupMenuSplitCells  = 'Split cells';
  scxPopupMenuFormatCells = 'Format cells';
  scxPopupMenuHideRows    = 'Hide rows';
  scxPopupMenuUnhideRows  = 'Unhide rows';
  scxPopupMenuHideCols    = 'Hide column';
  scxPopupMenuUnhideCols  = 'Unhide column';

  // insert and deliting cells constants
  scxSpreadSheetShiftCellLeft   = 'Shift cells left';
  scxSpreadSheetShiftCellTop    = 'Shift cells top';
  scxSpreadSheetShiftCellRight  = 'Shift cells right';
  scxSpreadSheetShiftCellBottom = 'Shift cells bottom';
  scxSpreadSheetAllRow          = 'Entire row';
  scxSpreadSheetAllColumn       = 'Entire column';
  scxSpreadSheetDeleteCells     = 'Delete';
  scxSpreadSheetInsertCells     = 'Insert';

  // datastorage
  scxDataStorageErrorReadCellRecord = 'Error reading cell record';
  scxDataStorageErrorSetCellRecord  = 'Error setting cell record';
  // style
  scxStyleInvalidColorIndex = 'Invalid color index';
  scxStyleInvalidCellStyle  = 'Invalid cell style';

  // calculation error
  scxCaclulatorCyclingError      = 'Calculation error. Formula cycled indexes present';
  scxCaclulatorConstructFormula  = 'Error constructing formula. Parsed expression error offset at';
  scxCaclulatorParseFormula      = 'Error parsing formula at string position';
  scxCaclulatorStringExpression  = 'Error syntax in string expression';
  scxCaclulatorFuncInvalidIndex  = 'Invalid function index';
  scxCaclulatorFuncInvalidName   = 'Invalid function name';
  scxCaclulatorErrorSymbol       = 'Error - symbol '')'' expected!';
  scxCaclulatorErrorString       = 'Error - unterminated string!';
  scxCaclulatorUnknownExpression = 'Unknown string expression';
  scxCaclulatorUnknownToken      = 'Unknown token in expression';
  scxCaclulatorDivByZero         = 'Division by zero!';
  scxCaclulatorTypeErr           = 'Incompatible operands type!';
  scxCaclulatorMissingTokens     = 'Missing tokens';
  scxCaclulatorMissingParamters  = 'Missing stack parameters';
  scxCaclulatorFuncNeedResult    = 'Function need result';

  // error codes such as excel program
  scxNullError    = '#NULL!';
  scxDivZeroError = '#DIV/0!';
  scxValueError   = '#VALUE!';
  scxNameError    = '#NAME!';
  scxNumError     = '#NUM!';
  scxRefError     = '#REF!';
  scxNAError      = '#N/A!';

  // forms resources
  scxFormApply  = 'Apply';
  scxFormCancel = 'Cancel';
  scxFormOk     = 'Ok';

  // format form strings
  scxFormatDialogFormatCaption = 'Format cells';
  scxFormatCellStyle           = 'Cell style';
  scxFormatCellStyleType       = 'Style type';
  scxFormatStyleStyleSettings  = 'Style settings';

  scxFormatStyleGeneral  = '&General';
  scxFormatStyleCurrency = '&Currency';
  scxFormatStyleNumber   = '&Number';
  scxFormatStyleDateTime = '&Date/Time';
  scxFormatStyleText     = '&Text';

  scxGeneralStyleDescription  = 'General format cells have no specific format.';
  scxNumberStyleDescription   = 'Number is used for general display of numbers.';
  scxCurrencyStyleDescription = 'Currency formats are used for general monetary amounts.';
  scxDateTimeStyleDescription = 'Date/Time formats are used for date and time serial numbers as Date/Time values.';
  scxTextStyleDescription     = 'Text formatted cells are treated as text even when a number is in the cell.  The cell is displayed exactly as entered.';


  scxFormatDialogText          = '&Alignment';
  scxFormatDialogBorder        = '&Border';
  scxFormatDialogPatterns      = '&Patterns';
  scxFormatDialogHorzAlign     = '&Horizontal';
  scxFormatDialogVertAlign     = '&Vertical';
  scxFormatDialogTextAlignment = 'Text alignment';
  scxFormatDialogTextControl   = 'Text control';
  scxFormatDialogGeneral       = 'General';
  scxFormatDialogLeft          = 'Left';
  scxFormatDialogCenter        = 'Center';
  scxFormatDialogRight         = 'Right';
  scxFormatDialogFill          = 'Fill';
  scxFormatDialogJustify       = 'Justify';
  scxFormatDialogTop           = 'Top';
  scxFormatDialogBottom        = 'Bottom';
  scxFormatDialogWrap          = '&Word wrap';
  scxFormatDialogFont          = '&Font';
  scxFormatDialogNone          = '&None';
  scxFormatDialogNone2         = 'None';
  scxFormatDialogOutline       = '&Ou&tl&ine';
  scxFormatDialogInside        = '&Inside';
  scxFormatDialogItems         = 'Items';
  scxFormatDialogAllBorder     = 'All borders';
  scxFormatDialogLine          = 'Line';
  scxFormatDialogStyle         = '&Style:';
  scxFormatDialogColor         = '&Color:';
  scxFormatDialogTextStr       = 'Text';
  scxFormatDialogCellShading   = 'Cell shading';
  scxFormatDialogSample        = 'Sample';
  scxFormatDialogPattern       = '&Pattern:';
  scxFormatDialogSampleText    = 'The quick brown fox jumps over the lazy dog';
  scxUseDefaultColor           = 'Use default color';
  scxColorEditorCaption        = 'Color property editor'; 

  // history constants
  scxChangeCellsStyle  = 'Format cells';
  scxChangeCellsData   = 'Change cells';
  scxChangeInsertCells = 'Insert cells';
  scxChangeDeleteCells = 'Delete cells';
  scxPasteCommand      = 'Paste cells';
  scxCutCommand        = 'Cut cells';
  scxMergeCells        = 'Merge cells';
  scxSplitCells        = 'Split cells';
  scxSortCellsAction   = 'Sort cells';
  scxClearCells        = 'Clear cells';
  scxClearAllAction    = 'Clear all';

  // color box dialog strings
  scxColorBoxAutomatic = 'Automatic';
  scxColorBoxNone      = 'None';

  // import/export strings
  scxExcelImportUndefinedString = 'Undefined string in shared string table!';
  scxXLSFuncCustructErr = 'Formula not construct';
  scxXLSUnknownFunc     = 'Unknown function';
  scxXLSSharedFunc      = 'Shared';
  scxXLSNameRef         = 'Invalid name reference: '; 

  scxWorkbookWrite = 'Error store workbook stream';
  scxWorkbookRead  = 'Error read workbook stream';


// function names definition
  sxlfAbs         = 'ABS';
  sxlfAcos        = 'ACOS';
  sxlfAcosh       = 'ACOSH';
  sxlfAnd         = 'AND';
  sxlfAsin        = 'ASIN';
  sxlfAsinh       = 'ASINH';
  sxlfAtan        = 'ATAN';
  sxlfAtan2       = 'ATAN2';
  sxlfAtanh       = 'ATANH';
  sxlfAverage     = 'AVERAGE';
  sxlfAverageA    = 'AVERAGEA';
  sxlfCos         = 'COS';
  sxlfCosh        = 'COSH';
  sxlfCount       = 'COUNT';
  sxlfCounta      = 'COUNTA';
  sxlfCountblank  = 'COUNTBLANK';
  sxlfCountif     = 'COUNTIF';
  sxlfDate        = 'DATE';
  sxlfDay         = 'DAY';
  sxlfDollar      = 'DOLLAR';
  sxlfExp         = 'EXP';
  sxlfFact        = 'FACT';
  sxlfInt         = 'INT';
  sxlfIF          = 'IF';
  sxlfLn          = 'LN';
  sxlfLog         = 'LOG';
  sxlfLog10       = 'LOG10';
  sxlfMax         = 'MAX';
  sxlfMin         = 'MIN';
  sxlfMod         = 'MOD';
  sxlfMonth       = 'MONTH';
  sxlfNot         = 'NOT';
  sxlfNow         = 'NOW';
  sxlfOdd         = 'ODD';
  sxlfOr          = 'OR';
  sxlfPi          = 'PI';
  sxlfPower       = 'POWER';
  sxlfRadians     = 'RADIANS';
  sxlfRand        = 'RAND';
  sxlfRound       = 'ROUND';
  sxlfRounddown   = 'ROUNDDOWN';
  sxlfRoundup     = 'ROUNDUP';
  sxlfSign        = 'SIGN';
  sxlfSin         = 'SIN';
  sxlfSinh        = 'SINH';
  sxlfSqrt        = 'SQRT';
  sxlfSum         = 'SUM';
  sxlfSumsq       = 'SUMSQ';
  sxlfTan         = 'TAN';
  sxlfTanh        = 'TANH';
  sxlfTime        = 'TIME';
  sxlfToday       = 'TODAY';
  sxlfTrunc       = 'TRUNC';
  sxlfYear        = 'YEAR';
  sxlfWeekDay     = 'WEEKDAY';
  sxlfFalse       = 'FALSE';
  sxlfTrue        = 'TRUE';

  sxlfEven        = 'EVEN';
  sxlfTrim        = 'TRIM';
  sxlfCeiling     = 'CEILING';
  sxlfFloor       = 'FLOOR';
  sxlfConcatenate = 'CONCATENATE';
  sxlfDegrees     = 'DEGREES';
  sxlfFixed       = 'FIXED';
  sxlfHour        = 'HOUR';
  sxlfMinute      = 'MINUTE';
  sxlfSecond      = 'SECOND';
  sxlfIsEven      = 'ISEVEN';
  sxlfIsOdd       = 'ISODD';
  sxlfIsBlank     = 'ISBLANK';
  sxlfIsNa        = 'ISNA';
  sxlfIsLogical   = 'ISLOGICAL';
  sxlfIsErr       = 'ISERR';
  sxlfIsError     = 'ISERROR';
  sxlfIsNonText   = 'ISNONTEXT';
  sxlfIsNumber    = 'ISNUMBER';
  sxlfIsText      = 'ISTEXT';
  sxlfLen         = 'LEN';
  sxlfLeft        = 'LEFT';
  sxlfRight       = 'RIGHT';
  sxlfMid         = 'MID';
  sxlfLower       = 'LOWER';
  sxlfUpper       = 'UPPER';

implementation

end.


