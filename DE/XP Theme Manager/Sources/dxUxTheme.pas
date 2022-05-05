
{********************************************************************}
{                                                                    }
{       Developer Express Visual Component Library                   }
{       ExpressXPThemeManager                                        }
{                                                                    }
{       Copyright (c) 1998-2003 Developer Express Inc.               }
{       ALL RIGHTS RESERVED                                          }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSXPTHEMEMANAGER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxUxTheme;

interface

uses Windows, CommCtrl;

const
// DrawThemeText() flags
  DTT_GRAYED = $1;

// HitTestThemeBackground(), HitTestThemeBackgroundRegion() flags
  HTTB_BACKGROUNDSEG = $0;

// Fixed border hit test option
  HTTB_FIXEDBORDER = $2;

// Caption hit test option
  HTTB_CAPTION = $4;

// Resizing border hit test flags
  HTTB_RESIZINGBORDER_LEFT = $10;
  HTTB_RESIZINGBORDER_TOP = $20;
  HTTB_RESIZINGBORDER_RIGHT = $40;
  HTTB_RESIZINGBORDER_BOTTOM = $80;
  HTTB_RESIZINGBORDER = HTTB_RESIZINGBORDER_LEFT or HTTB_RESIZINGBORDER_TOP or
    HTTB_RESIZINGBORDER_RIGHT or HTTB_RESIZINGBORDER_BOTTOM;
  HTTB_SIZINGTEMPLATE = $100;
  HTTB_SYSTEMSIZINGMARGINS = $200;

(*// DrawThemeLine() flags
  DTL_LEFT = $1;
  DTL_TOP = $2;
  DTL_RIGHT = $4;
  DTL_BOTTOM = $8;*)

  MAX_INTLIST_COUNT = 10;

  ETDT_DISABLE = $1;
  ETDT_ENABLE = $2;
  ETDT_USETABTEXTURE = $4;
  ETDT_ENABLETAB = ETDT_ENABLE or ETDT_USETABTEXTURE;

// flags to control theming within an app
  STAP_ALLOW_NONCLIENT = $1;
  STAP_ALLOW_CONTROLS = $2;
  STAP_ALLOW_WEBCONTENT = $4;

  SZ_THDOCPROP_DISPLAYNAME = 'DisplayName';
  SZ_THDOCPROP_CANONICALNAME = 'ThemeName';
  SZ_THDOCPROP_TOOLTIP = 'ToolTip';
  SZ_THDOCPROP_AUTHOR = 'author';

  TS_MIN = 0;
  TS_TRUE = 1;
  TS_DRAW = 2;

type
  HTHEME = THandle;
  TTheme = HTHEME;

  THEMESIZE = Integer;
  TThemeSize = THEMESIZE;

  _MARGINS = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;
  MARGINS = _MARGINS;
  PMARGINS = ^MARGINS;
  TMargins = MARGINS;

  _INTLIST = record
    iValueCount: Integer;
    iValues: array[0..MAX_INTLIST_COUNT - 1] of Integer;
  end;
  INTLIST = _INTLIST;
  PINTLIST = ^INTLIST;
  TIntList = INTLIST;

  PROPERTYORIGIN = (PO_STATE, PO_PART, PO_CLASS, PO_GLOBAL, PO_NOTFOUND);
  TPropertyOrigin = PROPERTYORIGIN;

function OpenThemeData(hWnd: HWND; pszClassList: PWideChar): HTHEME; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
// TODO: HRESULT -> THEMEAPI ?
function CloseThemeData(hTheme: HTHEME): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

// functions for basic drawing support
function DrawThemeBackground(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; pClipRect: PRect = nil): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO
function DrawThemeText(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pszText: PWideChar; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  pRect: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeBackgroundContentRect(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pBoundingRect: PRect; out ContentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO
function GetThemeBackgroundExtent(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pContentRect: PRect; out ExtentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemePartSize(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  prc: PRect; eSize: THEMESIZE; psz: PSize): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeTextExtent(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
  pBoundingRect: PRect; out ExtentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeTextMetrics(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  out tm: TEXTMETRIC): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeBackgroundRegion(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; out Region: HRGN): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function HitTestThemeBackground(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  dwOptions: DWORD; pRect : PRect; hRgn: HRGN; ptTest: TPoint;
  out wHitTestCode: WORD): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function DrawThemeEdge(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pDestRect: PRect; uEdge, uFlags: UINT; pContentRect: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function DrawThemeIcon(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function IsThemePartDefined(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function IsThemeBackgroundPartiallyTransparent(hTheme: HTHEME;
  iPartId, iStateId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function DrawThemeParentBackground(hWnd: HWND; DC: HDC; prc: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO prc: PRect = nil

// lower-level theme information services
function GetThemeColor(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Color: TColorRef): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeMetric(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeString(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  pszBuff: PWideChar{???}; cchMaxBuffChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO
function GetThemeBool(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out fVal{???}: BOOL): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO
function GetThemeInt(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeEnumValue(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemePosition(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Point: TPoint): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeFont(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out Font: LOGFONT): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeRect(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Rect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeMargins(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  prc: PRect; out Margins: MARGINS): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeIntList(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out IntList: INTLIST): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemePropertyOrigin(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Origin: PROPERTYORIGIN): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function SetWindowTheme(hWnd: HWND; pszSubAppName, pszSubIdList: PWideChar): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeFilename(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysColorBrush(hTheme: HTHEME; iColorId: Integer): HBRUSH; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysBool(hTheme: HTHEME; iBoolId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysSize(hTheme: HTHEME; iSizeId: Integer): Integer; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysFont(hTheme: HTHEME; iFontId: Integer; out lf: LOGFONT): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysString(hTheme: HTHEME; iStringId: Integer; pszStringBuff: PWideChar;
  cchMaxStringChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeSysInt(hTheme: HTHEME; iIntId: Integer; var iValue: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF}; // TODO var -> out ???

function IsThemeActive: BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function IsAppThemed: BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetWindowTheme(hWnd: HWND): HTHEME;cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function EnableThemeDialogTexture(hWnd: HWND; dwFlags: DWORD): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function IsThemeDialogTextureEnabled(hWnd: HWND): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeAppProperties: DWORD; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
procedure SetThemeAppProperties(dwFlags: DWORD); cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetCurrentThemeName(pszThemeFileName: PWideChar; cchMaxNameChars: Integer;
  pszColorBuff: PWideChar; cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
  cchMaxSizeChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
function GetThemeDocumentationProperty(pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
  cchMaxValChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

(*// TODO ???
// Theme API Error Handling
type
  THEME_ERROR_CONTEXT = record
    dwSize: DWORD;
    hr: HRESULT;
    szMsgParam1: Array [0..MAX_PATH - 1] of WCHAR;
    szMsgParam2: Array [0..MAX_PATH - 1] of WCHAR;
    szFileName: Array [0..MAX_PATH - 1] of WCHAR;
    szSourceLine: Array [0..MAX_PATH - 1] of WCHAR;
    iLineNum: Integer;
  end;
PTHEME_ERROR_CONTEXT = ^THEME_ERROR_CONTEXT;

//---------------------------------------------------------------------------
//  GetThemeLastErrorContext()
//                      - fill in the THEME_ERROR_CONTEXT structure
//                        with needed information about the last theme API
//                        error.
//
//  pContext            - ptr to the THEME_ERROR_CONTEXT to be filled
//---------------------------------------------------------------------------
function GetThemeLastErrorContext(pContext: THEME_ERROR_CONTEXT): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

//---------------------------------------------------------------------------
//  FormatThemeMessage()
//                      - use the supplied theme error context
//                        to format an error message in the specified
//                        language.
//
//  dwLanguageId        - the language to format the error msg into
//
//  pContext            - ptr to theme context structure (see above)
//                        that supplies the HRESULT and string params
//
//  pszMessageBuff      - where to write the formatted msg.  If the
//                        message is too long, it is truncated.
//
//  cchMaxMessageChars  - maximum number of chars to write to buff
//---------------------------------------------------------------------------
function FormatThemeMessage(dwLanguageId: DWORD; pContext: PTHEME_ERROR_CONTEXT;
     var pszMessageBuff: WideString; cchMaxMessageChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

//---------------------------------------------------------------------------
// GetThemeImageFromWindow
//---------------------------------------------------------------------------

function GetThemeImageFromParent(hWnd: HWND; DC: HDC; prc: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

//------------------------------------------------------------------------
//  DrawThemeLine()     - draws the theme-defined line within the
//                        specified "pRect".  "iStateId" selects the line
//                        type (flat, raised, lowered).  "dwDtlFlags"
//                        selects which line(s) to draw around the rect
//                        (left, top, right, bottom).
//
//  hTheme              - theme data handle
//  DC                 - HDC to draw into
//  iStateId            - style of line to draw (LVS_XXX or LHS_XXX)
//  pRect               - the RECT used to draw the line(s)
//  dwDtlFlags          - which parts of rect to draw (DTL_XXX flags)
//------------------------------------------------------------------------
function DrawThemeLine(hTheme: HTHEME; DC: HDC; iStateId: Integer;
    const pRect: TRect; dwDtlFlags: DWORD): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

//------------------------------------------------------------------------
//  DrawThemeBorder()   - draws the theme-defined border within the
//                        specified "pRect".  "iStateId" selects the
//                        border type (flat, raised, lowered).
//
//  hTheme              - theme data handle
//  DC                 - HDC to draw into
//  iStateId            - style of border to draw (BSS_XXX enums)
//  pRect               - the RECT used to draw the border
//------------------------------------------------------------------------
function DrawThemeBorder(hTheme: HTHEME; DC: HDC; iStateId: Integer;
    const pRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};*)

function IsThemeLibraryLoaded: Boolean;
function IsWindowsXPOrLater: Boolean;

implementation

uses
  ComCtrls;

const
  ThemeLibraryName = 'UxTheme.dll';

type
  TCloseThemeData = function(hTheme: HTHEME): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TDrawThemeBackground = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; pClipRect: PRect = nil): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TDrawThemeEdge = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pDestRect: PRect; uEdge, uFlags: UINT; pContentRect: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TDrawThemeIcon = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TDrawThemeParentBackground = function(hWnd: HWND; DC: HDC; prc: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TDrawThemeText = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pszText: PWideChar; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
    pRect: PRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TEnableThemeDialogTexture = function(hWnd: HWND; dwFlags: DWORD): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetCurrentThemeName = function(pszThemeFileName: PWideChar; cchMaxNameChars: Integer;
    pszColorBuff: PWideChar; cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeAppProperties = function: DWORD; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeBackgroundContentRect = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pBoundingRect: PRect; out ContentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeBackgroundRegion = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pRect: PRect; out Region: HRGN): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeBackgroundExtent = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pContentRect: PRect; out ExtentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeBool = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out fVal: BOOL): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeColor = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out Color: TColorRef): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeDocumentationProperty = function(pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
    cchMaxValChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeEnumValue = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeFilename = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeFont = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
    out pFont: LOGFONT): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeInt = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeIntList = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out IntList: INTLIST): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeMargins = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
    prc: PRect; out Margins: MARGINS): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeMetric = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
    out iVal: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemePartSize = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    prc: PRect; eSize: THEMESIZE; psz: PSize): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemePosition = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out Point: TPoint): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemePropertyOrigin = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out Origin: PROPERTYORIGIN): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeRect = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    out Rect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeString = function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    pszBuff: PWideChar; cchMaxBuffChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysBool = function(hTheme: HTHEME; iBoolId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysColor = function(hTheme: HTHEME; iColorId: Integer): COLORREF; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysColorBrush = function(hTheme: HTHEME; iColorId: Integer): HBRUSH; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysFont = function(hTheme: HTHEME; iFontId: Integer; out lf: LOGFONT): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysInt = function(hTheme: HTHEME; iIntId: Integer; var iValue: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysSize = function(hTheme: HTHEME; iSizeId: Integer): Integer; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeSysString = function(hTheme: HTHEME; iStringId: Integer; pszStringBuff: PWideChar;
    cchMaxStringChars: Integer): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeTextExtent = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
    pBoundingRect: PRect; out ExtentRect: TRect): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetThemeTextMetrics = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    out tm: TEXTMETRIC): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TGetWindowTheme = function(hWnd: HWND): HTHEME;cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  THitTestThemeBackground = function(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
    dwOptions: DWORD; pRect : PRect; hRgn: HRGN; ptTest: TPoint;
    out wHitTestCode: WORD): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TIsAppThemed = function: BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TIsThemeActive = function: BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TIsThemePartDefined = function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TIsThemeBackgroundPartiallyTransparent = function(hTheme: HTHEME;
    iPartId, iStateId: Integer): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TIsThemeDialogTextureEnabled = function(hWnd: HWND): BOOL; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TOpenThemeData = function(hWnd: HWND; pszClassList: PWideChar): HTHEME; cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TSetThemeAppProperties = procedure(dwFlags: DWORD); cdecl{$IFDEF WIN32} stdcall{$ENDIF};
  TSetWindowTheme = function(hWnd: HWND; pszSubAppName, pszSubIdList: PWideChar): HRESULT; cdecl{$IFDEF WIN32} stdcall{$ENDIF};

  TThemeLibraryMethodPointersKeeper = record
    FCloseThemeData: TCloseThemeData;
    FDrawThemeBackground: TDrawThemeBackground;
    FDrawThemeEdge: TDrawThemeEdge;
    FDrawThemeIcon: TDrawThemeIcon;
    FDrawThemeParentBackground: TDrawThemeParentBackground;
    FDrawThemeText: TDrawThemeText;
    FEnableThemeDialogTexture: TEnableThemeDialogTexture;
    FGetCurrentThemeName: TGetCurrentThemeName;
    FGetThemeAppProperties: TGetThemeAppProperties;
    FGetThemeBackgroundContentRect: TGetThemeBackgroundContentRect;
    FGetThemeBackgroundRegion: TGetThemeBackgroundRegion;
    FGetThemeBackgroundExtent: TGetThemeBackgroundExtent;
    FGetThemeBool: TGetThemeBool;
    FGetThemeColor: TGetThemeColor;
    FGetThemeDocumentationProperty: TGetThemeDocumentationProperty;
    FGetThemeEnumValue: TGetThemeEnumValue;
    FGetThemeFilename: TGetThemeFilename;
    FGetThemeFont: TGetThemeFont;
    FGetThemeInt: TGetThemeInt;
    FGetThemeIntList: TGetThemeIntList;
    FGetThemeMargins: TGetThemeMargins;
    FGetThemeMetric: TGetThemeMetric;
    FGetThemePartSize: TGetThemePartSize;
    FGetThemePosition: TGetThemePosition;
    FGetThemePropertyOrigin: TGetThemePropertyOrigin;
    FGetThemeRect: TGetThemeRect;
    FGetThemeString: TGetThemeString;
    FGetThemeSysBool: TGetThemeSysBool;
    FGetThemeSysColor: TGetThemeSysColor;
    FGetThemeSysColorBrush: TGetThemeSysColorBrush;
    FGetThemeSysFont: TGetThemeSysFont;
    FGetThemeSysInt: TGetThemeSysInt;
    FGetThemeSysSize: TGetThemeSysSize;
    FGetThemeSysString: TGetThemeSysString;
    FGetThemeTextExtent: TGetThemeTextExtent;
    FGetThemeTextMetrics: TGetThemeTextMetrics;
    FGetWindowTheme: TGetWindowTheme;
    FHitTestThemeBackground: THitTestThemeBackground;
    FIsAppThemed: TIsAppThemed;
    FIsThemeActive: TIsThemeActive;
    FIsThemePartDefined: TIsThemePartDefined;
    FIsThemeBackgroundPartiallyTransparent: TIsThemeBackgroundPartiallyTransparent;
    FIsThemeDialogTextureEnabled: TIsThemeDialogTextureEnabled;
    FOpenThemeData: TOpenThemeData;
    FSetThemeAppProperties: TSetThemeAppProperties;
    FSetWindowTheme: TSetWindowTheme;
  end;

var
  FIsWindowsXPOrLater: Boolean;
  FThemeLibrary: HMODULE = 0;
  FThemeLibraryMethodPointersKeeper: TThemeLibraryMethodPointersKeeper;

function CloseThemeData(hTheme: HTHEME): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FCloseThemeData(hTheme);
end;

function DrawThemeBackground(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; pClipRect: PRect = nil): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeBackground(hTheme, DC,
    iPartId, iStateId, pRect, pClipRect);
end;

function DrawThemeEdge(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pDestRect: PRect; uEdge, uFlags: UINT; pContentRect: PRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeEdge(hTheme, DC, iPartId, iStateId,
    pDestRect, uEdge, uFlags, pContentRect);
end;

function DrawThemeIcon(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; himl: HIMAGELIST; iImageIndex: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeIcon(hTheme, DC, iPartId, iStateId,
    pRect, himl, iImageIndex);
end;

function DrawThemeParentBackground(hWnd: HWND; DC: HDC; prc: PRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeParentBackground(hWnd, DC, prc);
end;

function DrawThemeText(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pszText: PWideChar; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD;
  pRect: PRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FDrawThemeText(hTheme, DC, iPartId, iStateId,
    pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect);
end;

function EnableThemeDialogTexture(hWnd: HWND; dwFlags: DWORD): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FEnableThemeDialogTexture(hWnd, dwFlags);
end;

function GetCurrentThemeName(pszThemeFileName: PWideChar; cchMaxNameChars: Integer;
  pszColorBuff: PWideChar; cchMaxColorChars: Integer; pszSizeBuff: PWideChar;
  cchMaxSizeChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetCurrentThemeName(pszThemeFileName,
    cchMaxNameChars, pszColorBuff, cchMaxColorChars, pszSizeBuff, cchMaxSizeChars);
end;

function GetThemeAppProperties: DWORD;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeAppProperties;
end;

function GetThemeBackgroundContentRect(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pBoundingRect: PRect; out ContentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundContentRect(hTheme,
    DC, iPartId, iStateId, pBoundingRect, ContentRect);
end;

function GetThemeBackgroundExtent(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pContentRect: PRect; out ExtentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundExtent(hTheme, DC,
    iPartId, iStateId, pContentRect, ExtentRect);
end;

function GetThemeBackgroundRegion(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pRect: PRect; out Region: HRGN): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBackgroundRegion(hTheme, DC, iPartId, iStateId, pRect, Region);
end;

function GetThemeBool(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out fVal: BOOL): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeBool(hTheme, iPartId, iStateId,
    iPropId, fVal);
end;

function GetThemeColor(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Color: TColorRef): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeColor(hTheme, iPartId, iStateId, iPropId, Color);
end;

function GetThemeDocumentationProperty(pszThemeName, pszPropertyName, pszValueBuff: PWideChar;
  cchMaxValChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeDocumentationProperty(pszThemeName,
    pszPropertyName, pszValueBuff, cchMaxValChars);
end;

function GetThemeEnumValue(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeEnumValue(hTheme, iPartId,
    iStateId, iPropId, iVal);
end;

function GetThemeFilename(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  pszThemeFileName: PWideChar; cchMaxBuffChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeFilename(hTheme, iPartId,
    iStateId, iPropId, pszThemeFileName, cchMaxBuffChars);
end;

function GetThemeFont(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out Font: LOGFONT): HRESULT;
var
  S: array [0 .. 1000] of Char;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeFont(hTheme, DC, iPartId, iStateId,
    iPropId, Font);
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Result, LOCALE_USER_DEFAULT, S, 1001, nil);
end;

function GetThemeInt(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeInt(hTheme, iPartId, iStateId,
    iPropId, iVal);
end;

function GetThemeIntList(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out IntList: INTLIST): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeIntList(hTheme, iPartId, iStateId,
    iPropId, IntList);
end;

function GetThemeMargins(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  prc: PRect; out Margins: MARGINS): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeMargins(hTheme, DC, iPartId,
    iStateId, iPropId, prc, Margins);
end;

function GetThemeMetric(hTheme: HTHEME; DC: HDC; iPartId, iStateId, iPropId: Integer;
  out iVal: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeMetric(hTheme, DC, iPartId,
    iStateId, iPropId, iVal);
end;

function GetThemePartSize(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  prc: PRect; eSize: THEMESIZE; psz: PSize): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePartSize(hTheme, DC, iPartId,
    iStateId, prc, eSize, psz);
end;

function GetThemePosition(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Point: TPoint): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePosition(hTheme, iPartId, iStateId,
    iPropId, Point);
end;

function GetThemePropertyOrigin(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Origin: PROPERTYORIGIN): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemePropertyOrigin(hTheme, iPartId,
    iStateId, iPropId, Origin);
end;

function GetThemeRect(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  out Rect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeRect(hTheme, iPartId, iStateId,
    iPropId, Rect);
end;

function GetThemeString(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
  pszBuff: PWideChar; cchMaxBuffChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeString(hTheme, iPartId, iStateId,
    iPropId, pszBuff, cchMaxBuffChars);
end;

function GetThemeSysBool(hTheme: HTHEME; iBoolId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysBool(hTheme, iBoolId);
end;

function GetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysColor(hTheme, iColorId);
end;

function GetThemeSysColorBrush(hTheme: HTHEME; iColorId: Integer): HBRUSH;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysColorBrush(hTheme, iColorId);
end;

function GetThemeSysFont(hTheme: HTHEME; iFontId: Integer; out lf: LOGFONT): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysFont(hTheme, iFontId, lf);
end;

function GetThemeSysInt(hTheme: HTHEME; iIntId: Integer; var iValue: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysInt(hTheme, iIntId, iValue);
end;

function GetThemeSysSize(hTheme: HTHEME; iSizeId: Integer): Integer;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysSize(hTheme, iSizeId);
end;

function GetThemeSysString(hTheme: HTHEME; iStringId: Integer; pszStringBuff: PWideChar;
  cchMaxStringChars: Integer): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeSysString(hTheme, iStringId,
    pszStringBuff, cchMaxStringChars);
end;

function GetThemeTextExtent(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  pszTextL: PWideChar; iCharCount: Integer; dwTextFlags: DWORD;
  pBoundingRect: PRect; out ExtentRect: TRect): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeTextExtent(hTheme, DC, iPartId,
    iStateId, pszTextL, iCharCount, dwTextFlags, pBoundingRect, ExtentRect);
end;

function GetThemeTextMetrics(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  out tm: TEXTMETRIC): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetThemeTextMetrics(hTheme, DC, iPartId,
    iStateId, tm);
end;

function GetWindowTheme(hWnd: HWND): HTHEME;
begin
  Result := FThemeLibraryMethodPointersKeeper.FGetWindowTheme(hWnd);
end;

function HitTestThemeBackground(hTheme: HTHEME; DC: HDC; iPartId, iStateId: Integer;
  dwOptions: DWORD; pRect : PRect; hRgn: HRGN; ptTest: TPoint;
  out wHitTestCode: WORD): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FHitTestThemeBackground(hTheme, DC,
    iPartId, iStateId, dwOptions, pRect, hRgn, ptTest, wHitTestCode);
end;

function IsAppThemed: BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsAppThemed;
end;

function IsThemeActive: BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemeActive;
end;

function IsThemeBackgroundPartiallyTransparent(hTheme: HTHEME;
  iPartId: Integer; iStateId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemeBackgroundPartiallyTransparent(hTheme,
    iPartId, iStateId);
end;

function IsThemeDialogTextureEnabled(hWnd: HWND): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemeDialogTextureEnabled(hWnd);
end;

function IsThemePartDefined(hTheme: HTHEME; iPartId: Integer; iStateId: Integer): BOOL;
begin
  Result := FThemeLibraryMethodPointersKeeper.FIsThemePartDefined(hTheme, iPartId, iStateId);
end;

function OpenThemeData(hWnd: HWND; pszClassList: LPCWSTR): HTHEME;
begin
  Result := FThemeLibraryMethodPointersKeeper.FOpenThemeData(hWnd, pszClassList);
end;

procedure SetThemeAppProperties(dwFlags: DWORD);
begin
  FThemeLibraryMethodPointersKeeper.FSetThemeAppProperties(dwFlags);
end;

function SetWindowTheme(hWnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT;
begin
  Result := FThemeLibraryMethodPointersKeeper.FSetWindowTheme(hWnd, pszSubAppName, pszSubIdList);
end;

function IsThemeLibraryLoaded: Boolean;
begin
  Result := FThemeLibrary <> 0;
end;

function IsWindowsXPOrLater: Boolean;
begin
  Result := FIsWindowsXPOrLater;
end;

procedure CheckOSVersion;
var
  Info: TOSVersionInfo;
begin
  Info.dwOSVersionInfoSize := SizeOf(Info);
  GetVersionEx(Info);
  FIsWindowsXPOrLater := (Info.dwMajorVersion > 5) or
    ((Info.dwMajorVersion = 5) and (Info.dwMinorVersion >= 1));
end;

procedure SetMethodPointers;
begin
  with FThemeLibraryMethodPointersKeeper do
  begin
    @FCloseThemeData := GetProcAddress(FThemeLibrary, 'CloseThemeData');
    @FDrawThemeBackground := GetProcAddress(FThemeLibrary, 'DrawThemeBackground');
    @FDrawThemeEdge := GetProcAddress(FThemeLibrary, 'DrawThemeEdge');
    @FDrawThemeIcon := GetProcAddress(FThemeLibrary, 'DrawThemeIcon');
    @FDrawThemeParentBackground := GetProcAddress(FThemeLibrary, 'DrawThemeParentBackground');
    @FDrawThemeText := GetProcAddress(FThemeLibrary, 'DrawThemeText');
    @FEnableThemeDialogTexture := GetProcAddress(FThemeLibrary, 'EnableThemeDialogTexture');
    @FGetCurrentThemeName := GetProcAddress(FThemeLibrary, 'GetCurrentThemeName');
    @FGetThemeAppProperties := GetProcAddress(FThemeLibrary, 'GetThemeAppProperties');
    @FGetThemeBackgroundContentRect := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundContentRect');
    @FGetThemeBackgroundRegion := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundRegion');
    @FGetThemeBackgroundExtent := GetProcAddress(FThemeLibrary, 'GetThemeBackgroundExtent');
    @FGetThemeBool := GetProcAddress(FThemeLibrary, 'GetThemeBool');
    @FGetThemeColor := GetProcAddress(FThemeLibrary, 'GetThemeColor');
    @FGetThemeDocumentationProperty := GetProcAddress(FThemeLibrary, 'GetThemeDocumentationProperty');
    @FGetThemeEnumValue := GetProcAddress(FThemeLibrary, 'GetThemeEnumValue');
    @FGetThemeFilename := GetProcAddress(FThemeLibrary, 'GetThemeFilename');
    @FGetThemeFont := GetProcAddress(FThemeLibrary, 'GetThemeFont');
    @FGetThemeInt := GetProcAddress(FThemeLibrary, 'GetThemeInt');
    @FGetThemeIntList := GetProcAddress(FThemeLibrary, 'GetThemeIntList');
    @FGetThemeMargins := GetProcAddress(FThemeLibrary, 'GetThemeMargins');
    @FGetThemeMetric := GetProcAddress(FThemeLibrary, 'GetThemeMetric');
    @FGetThemePartSize := GetProcAddress(FThemeLibrary, 'GetThemePartSize');
    @FGetThemePosition := GetProcAddress(FThemeLibrary, 'GetThemePosition');
    @FGetThemePropertyOrigin := GetProcAddress(FThemeLibrary, 'GetThemePropertyOrigin');
    @FGetThemeRect := GetProcAddress(FThemeLibrary, 'GetThemeRect');
    @FGetThemeString := GetProcAddress(FThemeLibrary, 'GetThemeString');
    @FGetThemeSysBool := GetProcAddress(FThemeLibrary, 'GetThemeSysBool');
    @FGetThemeSysColor := GetProcAddress(FThemeLibrary, 'GetThemeSysColor');
    @FGetThemeSysColorBrush := GetProcAddress(FThemeLibrary, 'GetThemeSysColorBrush');
    @FGetThemeSysFont := GetProcAddress(FThemeLibrary, 'GetThemeSysFont');
    @FGetThemeSysInt := GetProcAddress(FThemeLibrary, 'GetThemeSysInt');
    @FGetThemeSysSize := GetProcAddress(FThemeLibrary, 'GetThemeSysSize');
    @FGetThemeSysString := GetProcAddress(FThemeLibrary, 'GetThemeSysString');
    @FGetThemeTextExtent := GetProcAddress(FThemeLibrary, 'GetThemeTextExtent');
    @FGetThemeTextMetrics := GetProcAddress(FThemeLibrary, 'GetThemeTextMetrics');
    @FGetWindowTheme := GetProcAddress(FThemeLibrary, 'GetWindowTheme');
    @FHitTestThemeBackground := GetProcAddress(FThemeLibrary, 'HitTestThemeBackground');
    @FIsAppThemed := GetProcAddress(FThemeLibrary, 'IsAppThemed');
    @FIsThemeActive := GetProcAddress(FThemeLibrary, 'IsThemeActive');
    @FIsThemePartDefined := GetProcAddress(FThemeLibrary, 'IsThemePartDefined');
    @FIsThemeBackgroundPartiallyTransparent := GetProcAddress(FThemeLibrary, 'IsThemeBackgroundPartiallyTransparent');
    @FIsThemeDialogTextureEnabled := GetProcAddress(FThemeLibrary, 'IsThemeDialogTextureEnabled');
    @FOpenThemeData := GetProcAddress(FThemeLibrary, 'OpenThemeData');
    @FSetThemeAppProperties := GetProcAddress(FThemeLibrary, 'SetThemeAppProperties');
    @FSetWindowTheme := GetProcAddress(FThemeLibrary, 'SetWindowTheme');
  end;
end;

initialization
  CheckOSVersion;
  InitCommonControls;
  if FIsWindowsXPOrLater (*and (GetComCtlVersion >= $60000) *)then
  begin
    FThemeLibrary := LoadLibrary(ThemeLibraryName);
    if FThemeLibrary <> 0 then
      SetMethodPointers;
  end;

finalization
  if FThemeLibrary <> 0 then
    FreeLibrary(FThemeLibrary);

end.
