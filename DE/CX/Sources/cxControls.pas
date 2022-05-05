
{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
{                                                                    }
{           Copyright (c) 2000-2003 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

{$DEFINE USETCXSCROLLBAR}

unit cxControls;

{$I cxVer.inc}

interface

uses
{$IFDEF VCL}
  Windows, Messages,
{$ELSE}
  Qt, Types,
{$ENDIF}
  Classes, Controls, Graphics, Forms, StdCtrls, ExtCtrls, cxGraphics,
{$IFDEF USETCXSCROLLBAR}
  cxScrollBar,
{$ENDIF}
  cxClasses,
  cxLookAndFeels,
  cxLookAndFeelPainters;

const
{$IFNDEF VCL}
  VK_ESCAPE = Key_Escape;
  VK_RETURN = Key_Return;
  VK_INSERT = Key_Insert;
  VK_BACK = Key_Backspace;
  VK_DELETE = Key_Delete;
  VK_TAB = Key_Tab;
  VK_LEFT = Key_Left;
  VK_RIGHT = Key_Right;
  VK_UP = Key_Up;
  VK_DOWN = Key_Down;
  VK_HOME = Key_Home;
  VK_END = Key_End;
  VK_PRIOR = Key_Prior;
  VK_NEXT = Key_Next;
  VK_ADD = Key_Plus;
  VK_SUBTRACT = Key_Minus;
  VK_MULTIPLY = Key_Asterisk;
  VK_SPACE = Key_Space;
  VK_F2 = Key_F2;
{$ELSE}
  CM_NCSIZECHANGED = WM_APP + 305;
{$ENDIF}

type
  TcxHandle = {$IFDEF VCL}HWND{$ELSE}QWidgetH{$ENDIF};

{$IFDEF VCL}
  TcxDragDetect = (ddNone, ddDrag, ddCancel);
{$ELSE}
  TWinControlClass = class of TWinControl;
{$ENDIF}

  TcxControl = class;

  TDragControlObjectClass = class of TDragControlObject;

  IcxMouseCaptureObject = interface
    ['{ACB73657-6066-4564-9A3D-D4D0273AA82F}']
    procedure DoCancelMode;
  end;

  IcxMouseTrackingCaller = interface
    ['{84A4BCBE-E001-4D60-B7A6-75E2B0DCD3E9}']
    procedure MouseLeave;
  end;

  { IcxCompoundControl }

  IcxCompoundControl = interface
  ['{A4A77F28-1D03-425B-9A83-0B853B5D8DEF}']
    function GetActiveControl: TWinControl;
    property ActiveControl: TWinControl read GetActiveControl;
  end;

  { control child component }

  TcxControlChildComponent = class(TcxComponent)
  private
    FControl: TcxControl;
    function GetIsLoading: Boolean;
  protected
    function GetIsDestroying: Boolean; virtual;
    procedure SetControl(Value: TcxControl); virtual;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True);
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property Control: TcxControl read FControl write SetControl;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
  end;

  { scrollbar and size grip }

  TcxScrollBarData = record
    Min: Integer;
    Max: Integer;
    Position: Integer;
    PageSize: Integer;
    SmallChange: TScrollBarInc;
    LargeChange: TScrollBarInc;
    Enabled: Boolean;
    Visible: Boolean;
    AllowShow: Boolean;
    AllowHide: Boolean;
  end;

  TcxControlScrollBar = class({$IFDEF USETCXSCROLLBAR}TcxScrollBar{$ELSE}TScrollBar{$ENDIF})
  private
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
  {$IFDEF VCL}
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  {$ENDIF}
  protected
  {$IFDEF VCL}
    procedure WndProc(var Message: TMessage); override;
  {$ELSE}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    procedure InitWidget; override;
    procedure WidgetDestroyed; override;
  {$ENDIF}
    procedure FocusParent; virtual;
  public
    Data: TcxScrollBarData;
    constructor Create(AOwner: TComponent); override;
    procedure ApplyData;
  {$IFDEF VCL}
  {$ELSE}
    function CanFocus: Boolean; override;
  {$ENDIF}
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TcxSizeGrip = class(TCustomControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { drag & drop objects }

  TcxDragAndDropObjectClass = class of TcxDragAndDropObject;

  TcxDragAndDropObject = class
  private
    FCanvas: TcxCanvas;
    FControl: TcxControl;
    FDirty: Boolean;
    procedure SetDirty(Value: Boolean);
  protected
    procedure ChangeMousePos(const P: TPoint);
    procedure DirtyChanged; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; virtual;
    function GetImmediateStart: Boolean; virtual;

    procedure AfterDragAndDrop(Accepted: Boolean); virtual;
    procedure BeginDragAndDrop; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;

    property Canvas: TcxCanvas read FCanvas write FCanvas;
    property Control: TcxControl read FControl;
    property Dirty: Boolean read FDirty write SetDirty;
  public
    CurMousePos: TPoint;
    PrevMousePos: TPoint;
    constructor Create(AControl: TcxControl); virtual;

    procedure DoBeginDragAndDrop;
    procedure DoDragAndDrop(const P: TPoint; var Accepted: Boolean);
    procedure DoEndDragAndDrop(Accepted: Boolean);

    property ImmediateStart: Boolean read GetImmediateStart;
  end;

  TcxDragControlObject = class(TDragControlObject)
  protected
  {$IFDEF VCL}
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  {$ENDIF}
  end;

  { control }

  TcxControlBorderStyle = (cxcbsNone, cxcbsDefault);
  TcxDragAndDropState = (ddsNone, ddsStarting, ddsInProcess);

  TcxKey = (kAll, kArrows, kChars, kTab);
  TcxKeys = set of TcxKey;

  TcxMouseWheelScrollingKind = (mwskNone, mwskHorizontal, mwskVertical);

  TcxControl = class(TCustomControl {$IFNDEF DELPHI6}, IUnknown{$ENDIF})
  private
    FActiveCanvas: TcxCanvas;
    FBorderStyle: TcxControlBorderStyle;
    FCalculatingScrollBarsParams: Boolean;
    FCanvas: TcxCanvas;
    FDefaultCursor: TCursor;
    FDragAndDropObject: TcxDragAndDropObject;
    FDragAndDropObjectClass: TcxDragAndDropObjectClass;
    FDragAndDropPrevCursor: TCursor;
    FDragAndDropState: TcxDragAndDropState;
    FFinishingDragAndDrop: Boolean;
    FFontListenerList: IInterfaceList;
    FHScrollBar: TcxControlScrollBar;
    FInitialHScrollBarVisible: Boolean;
    FInitialVScrollBarVisible: Boolean;
    FIsInitialScrollBarsParams: Boolean;
    FKeys: TcxKeys;
    FListenerLinks: TList;
    FLookAndFeel: TcxLookAndFeel;
    FMouseCaptureObject: TObject;
    FMouseDownPos: TPoint;
    FScrollBars: TScrollStyle;
    FSizeGrip: TcxSizeGrip;
    FUpdatingScrollBars: Boolean;
    FVScrollBar: TcxControlScrollBar;
    FOnFocusChanged: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    function GetActiveCanvas: TcxCanvas;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetHScrollBarVisible: Boolean;
    function GetIsCopyDragDrop: Boolean;
    function GetIsDesigning: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetVScrollBarVisible: Boolean;
    procedure SetBorderStyle(Value: TcxControlBorderStyle);
    procedure SetDragAndDropState(Value: TcxDragAndDropState);
    procedure SetKeys(Value: TcxKeys);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetMouseCaptureObject(Value: TObject);
    procedure SetScrollBars(Value: TScrollStyle);
  {$IFDEF VCL}
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCCalcSize(var Message: TMessage); message WM_NCCALCSIZE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMNCSizeChanged(var Message: TMessage); message CM_NCSIZECHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  {$ENDIF}
    procedure ClearListenerLinks;
    procedure CreateScrollBars;
    procedure DestroyScrollBars;
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  protected
  {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
  {$ELSE}
    procedure InitWidget; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    procedure SetParent(const AParent: TWidgetControl); override;
  {$ENDIF}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {$IFNDEF VCL}const{$ENDIF} MousePos: TPoint): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

  {$IFDEF VCL}
    procedure AfterMouseDown(AButton: TMouseButton; X, Y: Integer); virtual;
    procedure ColorChanged; dynamic;
    procedure DoScrolling;
    procedure Modified;
    procedure VisibleChanged; dynamic;
  {$ENDIF}
    procedure AddChildComponent(AComponent: TcxControlChildComponent); dynamic;
    procedure RemoveChildComponent(AComponent: TcxControlChildComponent); dynamic;

    procedure AddFontListener(AListener: IcxFontListener);
    procedure RemoveFontListener(AListener: IcxFontListener);
    property FontListenerList: IInterfaceList read FFontListenerList;

    function AllowAutoDragAndDropAtDesignTime(X, Y: Integer;
      Shift: TShiftState): Boolean; virtual;
    function AllowDragAndDropWithoutFocus: Boolean; dynamic;
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure BoundsChanged; {$IFDEF VCL}dynamic{$ELSE}override{$ENDIF};
    function CanDrag(X, Y: Integer): Boolean; dynamic;
    function CanFocusOnClick: Boolean; virtual;
    procedure CursorChanged; {$IFDEF VCL}dynamic{$ELSE}override{$ENDIF};
    procedure DoCancelMode; dynamic;
    procedure FocusChanged; dynamic;
    function FocusWhenChildIsClicked(AChild: TControl): Boolean; virtual;
    procedure FontChanged; {$IFDEF VCL}dynamic;{$ELSE}override;{$ENDIF}
    function GetBorderSize: Integer; virtual;
    function GetBounds: TRect; virtual;
    function GetClientBounds: TRect; virtual;
    function GetCursor(X, Y: Integer): TCursor; virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; dynamic;
    function GetDragObjectClass: TDragControlObjectClass; dynamic;
    function GetIsFocused: Boolean; virtual;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; virtual;
    function HasBackground: Boolean; virtual;
    procedure InitControl; virtual;
    function IsInternalControl(AControl: TControl): Boolean; virtual;
    function MayFocus: Boolean; dynamic;
    procedure MouseEnter(AControl: TControl); {$IFDEF VCL}dynamic{$ELSE}override{$ENDIF};
    procedure MouseLeave(AControl: TControl); {$IFDEF VCL}dynamic{$ELSE}override{$ENDIF};

    // look&feel
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainterClass; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); virtual;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainterClass read GetLookAndFeelPainter;

    // scrollbars
    function GetHScrollBarBounds: TRect; virtual;
    function GetSizeGripBounds: TRect; virtual;
    function GetVScrollBarBounds: TRect; virtual;
    procedure InitScrollBarsParameters; virtual;
    function IsPixelScrollBar(AKind: TScrollBarKind): Boolean; virtual;
    function NeedsScrollBars: Boolean; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); virtual;
    procedure SetInternalControlsBounds; virtual;
    procedure UpdateScrollBars; virtual;

    property CalculatingScrollBarsParams: Boolean read FCalculatingScrollBarsParams;
    property HScrollBar: TcxControlScrollBar read FHScrollBar;
    property HScrollBarVisible: Boolean read GetHScrollBarVisible;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property SizeGrip: TcxSizeGrip read FSizeGrip;
    property UpdatingScrollBars: Boolean read FUpdatingScrollBars;
    property VScrollBar: TcxControlScrollBar read FVScrollBar;
    property VScrollBarVisible: Boolean read GetVScrollBarVisible;

    // internal drag and drop (columns moving, ...)
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); dynamic;
    procedure EndDragAndDrop(Accepted: Boolean); dynamic;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; dynamic;
    property DragAndDropState: TcxDragAndDropState read FDragAndDropState
      write SetDragAndDropState;

    procedure AddListenerLink(ALink: Pointer);
    procedure RemoveListenerLink(ALink: Pointer);

    property BorderSize: Integer read GetBorderSize;
    property BorderStyle: TcxControlBorderStyle read FBorderStyle write SetBorderStyle;
    property IsCopyDragDrop: Boolean read GetIsCopyDragDrop;
    property Keys: TcxKeys read FKeys write SetKeys;
    property MouseWheelScrollingKind: TcxMouseWheelScrollingKind read GetMouseWheelScrollingKind;

    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF DELPHI4}
    procedure BeforeDestruction; override;
  {$ENDIF}
    function CanFocusEx: Boolean; virtual;
    function AcceptMousePosForClick(X, Y: Integer): Boolean; virtual;
  {$IFDEF VCL}
    procedure InvalidateRect(const R: TRect; EraseBackground: Boolean);
  {$ENDIF}
    function IsMouseInPressedArea(X, Y: Integer): Boolean;
    procedure ScrollContent(ADirection: TcxDirection);
    procedure SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
      AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
    function StartDrag({var }DragObject: TDragObject): Boolean; dynamic;
    procedure PostMouseMove(AMousePos: PPoint = nil);

    // internal drag and drop (columns moving, ...)
    procedure BeginDragAndDrop; dynamic;
    procedure FinishDragAndDrop(Accepted: Boolean);
    property DragAndDropObject: TcxDragAndDropObject read GetDragAndDropObject;
    property DragAndDropObjectClass: TcxDragAndDropObjectClass read GetDragAndDropObjectClass
      write FDragAndDropObjectClass;

    property ActiveCanvas: TcxCanvas read GetActiveCanvas;
    property Bounds: TRect read GetBounds;
    property Canvas: TcxCanvas read FCanvas;
    property ClientBounds: TRect read GetClientBounds;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsFocused: Boolean read GetIsFocused;
    property MouseCaptureObject: TObject read FMouseCaptureObject write SetMouseCaptureObject;
    property MouseDownPos: TPoint read FMouseDownPos write FMouseDownPos;
  end;

  { customize listbox }

  TcxCustomizeListBox = class(TListBox)
  private
    FDragAndDropItemIndex: Integer;
    FMouseDownPos: TPoint;
    function GetDragAndDropItemObject: TObject;
    function GetItemObject: TObject;
    procedure SetItemObject(Value: TObject);
  {$IFDEF VCL}
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  {$ENDIF}
  protected
  {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
  {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  {$IFNDEF VCL}
    procedure Resize; override;
  {$ENDIF}

    procedure BeginDragAndDrop; dynamic;

    property DragAndDropItemIndex: Integer read FDragAndDropItemIndex;
    property DragAndDropItemObject: TObject read GetDragAndDropItemObject;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemObject: TObject read GetItemObject write SetItemObject;
  end;

  { popup }

  TcxPopupAlignHorz = (pahLeft, pahCenter, pahRight);
  TcxPopupAlignVert = (pavTop, pavCenter, pavBottom);
  TcxPopupDirection = (pdHorizontal, pdVertical);

  TcxPopupWindow = class(TForm)
  private
    FAdjustable: Boolean;
    FAlignHorz: TcxPopupAlignHorz;
    FAlignVert: TcxPopupAlignVert;
    FBorderSpace: Integer;
    FBorderStyle: TcxPopupBorderStyle;
    FCanvas: TcxCanvas;
    FDirection: TcxPopupDirection;
    FFrameColor: TColor;
    FOwnerBounds: TRect;
    FOwnerParent: TControl;
  {$IFDEF VCL}
    FPrevActiveWindow: HWND;
  {$ELSE}
    FAppDeactivationTimer: TTimer;
  {$ENDIF}
    function GetNCHeight: Integer;
    function GetNCWidth: Integer;
    procedure SetBorderSpace(Value: Integer);
    procedure SetBorderStyle(Value: TcxPopupBorderStyle);
  {$IFDEF VCL}
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  {$ELSE}
    procedure AppDeactivationTimerHandler(Sender: TObject);
  {$ENDIF}
  protected
    procedure Deactivate; override;
    procedure Paint; override;
  {$IFDEF VCL}
    procedure VisibleChanged; dynamic;
  {$ELSE}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
  {$ENDIF}

    function CalculatePosition: TPoint; virtual;
    procedure CalculateSize; virtual;
    function GetBorderWidth(ABorder: TcxBorder): Integer; virtual;
    function GetFrameWidth(ABorder: TcxBorder): Integer; virtual;
    procedure InitPopup; virtual;
    procedure RestoreControlsBounds;

    procedure DrawFrame; virtual;

    property BorderWidths[ABorder: TcxBorder]: Integer read GetBorderWidth;
    property Canvas: TcxCanvas read FCanvas;
    property FrameWidths[ABorder: TcxBorder]: Integer read GetFrameWidth;
    property NCHeight: Integer read GetNCHeight;
    property NCWidth: Integer read GetNCWidth;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure CloseUp; virtual;
    procedure Popup; virtual;

    property Adjustable: Boolean read FAdjustable write FAdjustable;
    property AlignHorz: TcxPopupAlignHorz read FAlignHorz write FAlignHorz;
    property AlignVert: TcxPopupAlignVert read FAlignVert write FAlignVert;
    property BorderSpace: Integer read FBorderSpace write SetBorderSpace;
    property BorderStyle: TcxPopupBorderStyle read FBorderStyle write SetBorderStyle;
    property Direction: TcxPopupDirection read FDirection write FDirection;
    property FrameColor: TColor read FFrameColor write FFrameColor;
    property OwnerBounds: TRect read FOwnerBounds write FOwnerBounds;
    property OwnerParent: TControl read FOwnerParent write FOwnerParent;
  end;

{$IFDEF VCL}
function DragDetect(Wnd: HWND): TcxDragDetect;
function GetMouseKeys: WPARAM;
{$ENDIF}
function GetDblClickInterval: Integer;
function GetDesktopWorkArea(const P: TPoint): TRect;
function IsCtrlPressed: Boolean;
function IsPointInDragDetectArea(const AMouseDownPos: TPoint; X, Y: Integer): Boolean;
procedure MapWindowPoint(AHandleFrom, AHandleTo: TcxHandle; var P: TPoint);
procedure MapWindowRect(AHandleFrom, AHandleTo: TcxHandle; var R: TRect);
function TranslateKey(Key: Word): Word;

// mouse tracking
procedure BeginMouseTracking(AControl: TcxControl; const ABounds: TRect;
  ACaller: IcxMouseTrackingCaller);
procedure EndMouseTracking(ACaller: IcxMouseTrackingCaller);
function IsMouseTracking(ACaller: IcxMouseTrackingCaller): Boolean;

// hourglass cursor showing
procedure HideHourglassCursor;
procedure ShowHourglassCursor;

implementation

{$IFDEF VCL}
{$R cxControls.res}
{$ENDIF}

uses
{$IFDEF VCL}
  dxuxTheme,
  dxThemeConsts,
{$ENDIF}
  SysUtils;

const
{$IFDEF VCL}
  crBase = 2100;
  crDragCopy = crBase;
  crFullScroll = crBase + 1;
  crHorScroll = crBase + 2;
  crVerScroll = crBase + 3;
  crUpScroll = crBase + 4;
  crRightScroll = crBase + 5;
  crDownScroll = crBase + 6;
  crLeftScroll = crBase + 7;
{$ELSE}
  CYHSCROLL = 16;
  CXVSCROLL = 16;
{$ENDIF}

  ScreenHandle = {$IFDEF VCL}0{$ELSE}nil{$ENDIF};

type
  TControlAccess = class(TControl);

{$IFDEF VCL}

var
  FUser32DLL: HMODULE;

function DragDetect(Wnd: HWND): TcxDragDetect;
var
  NoDragZone: TRect;
  P: TPoint;
  Msg: TMsg;
begin
  Result := ddCancel;

  GetCursorPos(P);
  with NoDragZone, P do
  begin
    Right := 2 * Mouse.DragThreshold;//GetSystemMetrics(SM_CXDRAG);
    Bottom := 2 * Mouse.DragThreshold;//GetSystemMetrics(SM_CYDRAG);
    Left := X - Right div 2;
    Top := Y - Bottom div 2;
    Inc(Right, Left);
    Inc(Bottom, Top);
  end;

  SetCapture(Wnd);
  try
    while GetCapture = Wnd do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
            PostQuitMessage(Msg.wParam);
            Break;
          end;
      end;
      try
        case Msg.message of
          WM_KEYDOWN, WM_KEYUP:
            if Msg.wParam = VK_ESCAPE then Break;
          WM_MOUSEMOVE:
            begin
              P := SmallPointToPoint(TSmallPoint(Msg.lParam));
              ClientToScreen(Msg.hwnd, P);
              if not PtInRect(NoDragZone, P) then
              begin
                Result := ddDrag;
                Break;
              end;
            end;
          WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
            begin
              Result := ddNone;
              Break;
            end;
        end;
      finally
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = Wnd then ReleaseCapture;
  end;
end;

function GetMouseKeys: WPARAM;
begin
  Result := 0;
  if GetAsyncKeyState(VK_LBUTTON) < 0 then Inc(Result, MK_LBUTTON);
  if GetAsyncKeyState(VK_MBUTTON) < 0 then Inc(Result, MK_MBUTTON);
  if GetAsyncKeyState(VK_RBUTTON) < 0 then Inc(Result, MK_RBUTTON);
  if GetAsyncKeyState(VK_CONTROL) < 0 then Inc(Result, MK_CONTROL);
  if GetAsyncKeyState(VK_SHIFT) < 0 then Inc(Result, MK_SHIFT);
end;

{$ENDIF}

function GetDblClickInterval: Integer;
begin
{$IFDEF VCL}
  Result := GetDoubleClickTime;
{$ELSE}
  Result := QApplication_doubleClickInterval;
{$ENDIF}
end;

function GetDesktopWorkArea(const P: TPoint): TRect;
{$IFDEF VCL}
const
  MONITOR_DEFAULTTONEAREST = $2;
type
  HMONITOR = type Integer;
  PMonitorInfo = ^TMonitorInfo;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFalgs: DWORD;
  end;
var
  Info: TMonitorInfo;
  GetMonitorInfo: function(hMonitor: HMONITOR; lpMonitorInfo: PMonitorInfo): Boolean; stdcall;
  MonitorFromPoint: function(ptScreenCoords: TPoint; dwFlags: DWORD): HMONITOR; stdcall;
begin
  if FUser32DLL > 32 then
  begin
    GetMonitorInfo := GetProcAddress(FUser32DLL, 'GetMonitorInfoA');
    MonitorFromPoint := GetProcAddress(FUser32DLL, 'MonitorFromPoint');
  end
  else
  begin
    GetMonitorInfo := nil;
    MonitorFromPoint := nil;
  end;

  if @GetMonitorInfo = nil then
    SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0)
  else
  begin
    Info.cbSize := SizeOf(Info);
    GetMonitorInfo(MonitorFromPoint(P, MONITOR_DEFAULTTONEAREST), @Info);
    Result := Info.rcWork;
  end;
end;
{$ELSE}
begin
  QWidget_rect(QApplication_desktop, @Result);
end;
{$ENDIF}

function IsCtrlPressed: Boolean;
begin
{$IFDEF VCL}
  Result := GetAsyncKeyState(VK_CONTROL) < 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsPointInDragDetectArea(const AMouseDownPos: TPoint; X, Y: Integer): Boolean;
begin
  Result :=
    (Abs(X - AMouseDownPos.X) < Mouse.DragThreshold) and
    (Abs(Y - AMouseDownPos.Y) < Mouse.DragThreshold);
end;

procedure MapWindowPoint(AHandleFrom, AHandleTo: TcxHandle; var P: TPoint);
begin            {10}
{$IFDEF VCL}
  MapWindowPoints(AHandleFrom, AHandleTo, P, 1);
{$ELSE}
  if AHandleFrom = ScreenHandle then
    QWidget_mapFromGlobal(AHandleTo, @P, @P)
  else
    if AHandleTo = ScreenHandle then
      QWidget_mapToGlobal(AHandleFrom, @P, @P)
    else
      QWidget_mapFrom(AHandleFrom, @P, AHandleTo, @P);
{$ENDIF}
end;

procedure MapWindowRect(AHandleFrom, AHandleTo: TcxHandle; var R: TRect);
begin
  MapWindowPoint(AHandleFrom, AHandleTo, R.TopLeft);
  MapWindowPoint(AHandleFrom, AHandleTo, R.BottomRight);
end;

function TranslateKey(Key: Word): Word;
begin
{$IFDEF VCL}
  Result := Key;
{$ELSE}
  if (Key = Key_Return) or (Key = Key_Enter) then
    Result := VK_RETURN
  else
    Result := Key;
{$ENDIF}
end;

{$IFDEF VCL}
  {$IFDEF USETCXSCROLLBAR}

{ TcxSettingsController }

type
  TcxSettingsController = class
  private
    FList: TList;
  protected
    FWindow: HWND;
    procedure MainWndProc(var Message: TMessage);
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddListener(AListener: TcxControl);
    procedure NotifyListeners;
    procedure RemoveListener(AListener: TcxControl);
  end;

constructor TcxSettingsController.Create;
begin
  FList := TList.Create;
end;

destructor TcxSettingsController.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TcxSettingsController.AddListener(AListener: TcxControl);
begin
  with FList do
    if IndexOf(Pointer(AListener)) = -1 then
    begin
    {$IFDEF VCL}
      if Count = 0 then
      {$IFDEF DELPHI6}
        FWindow := Classes.AllocateHWnd(MainWndProc);
      {$ELSE}
        FWindow := Forms.AllocateHWnd(MainWndProc);
      {$ENDIF}
    {$ENDIF}
      Add(Pointer(AListener));
    end;
end;

procedure TcxSettingsController.RemoveListener(AListener: TcxControl);
begin
  FList.Remove(Pointer(AListener));
{$IFDEF VCL}
  if FList.Count = 0 then
  {$IFDEF DELPHI6}
    Classes.DeallocateHWnd(FWindow);
  {$ELSE}
    Forms.DeallocateHWnd(FWindow);
  {$ENDIF}
{$ENDIF}
end;

procedure TcxSettingsController.NotifyListeners;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if TCustomControl(FList[I]).HandleAllocated then
      SendNotifyMessage(TCustomControl(FList[I]).Handle, CM_NCSIZECHANGED, 0, 0);
end;

procedure TcxSettingsController.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    Application.HandleException(Self);
  end;
end;

procedure TcxSettingsController.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_SETTINGCHANGE) and (Message.WParam = SPI_SETNONCLIENTMETRICS) then
  begin
    NotifyListeners;
    Message.Result := 0;
    Exit;
  end;
  with Message do Result := DefWindowProc(FWindow, Msg, wParam, lParam);
end;

var
  FSettingsController: TcxSettingsController;

function cxSettingsController: TcxSettingsController;
begin
  if FSettingsController = nil then
    FSettingsController := TcxSettingsController.Create;
  Result := FSettingsController;
end;

  {$ENDIF}
{$ENDIF}


{ mouse tracking }

var
  FMouseTrackingTimerList: TList;

function MouseTrackingTimerList: TList;
begin
  if FMouseTrackingTimerList = nil then
    FMouseTrackingTimerList := TList.Create;
  Result := FMouseTrackingTimerList;
end;

type
  TMouseTrackingTimer = class(TTimer)
  protected
    procedure TimerHandler(Sender: TObject);
  public
    Caller: IcxMouseTrackingCaller;
    Control: TcxControl;
    Bounds: TRect;
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMouseTrackingTimer.Create(AOwner: TComponent);
begin
  inherited;
  Interval := 10;
  OnTimer := TimerHandler;
end;

procedure TMouseTrackingTimer.TimerHandler(Sender: TObject);
var
  ACaller: IcxMouseTrackingCaller;
begin
  if not PtInRect(Bounds, Mouse.CursorPos) then
  begin
    ACaller := Caller;
    if Control <> nil then Control.MouseLeave(nil);
    if ACaller <> nil then ACaller.MouseLeave;
    EndMouseTracking(ACaller);
  end;
end;

procedure BeginMouseTracking(AControl: TcxControl; const ABounds: TRect;
  ACaller: IcxMouseTrackingCaller);
var
  ATimer: TMouseTrackingTimer;
begin
  if IsMouseTracking(ACaller) then Exit;
  ATimer := TMouseTrackingTimer.Create(nil);
  with ATimer do
  begin
    Control := AControl;
    Bounds := ABounds;
    if Control <> nil then
      MapWindowRect(Control.Handle, ScreenHandle, Bounds);
    Caller := ACaller;
  end;
  MouseTrackingTimerList.Add(ATimer);
end;

function GetMouseTrackingTimer(ACaller: IcxMouseTrackingCaller): TMouseTrackingTimer;
var
  I: Integer;
begin
  for I := 0 to MouseTrackingTimerList.Count - 1 do
  begin
    Result := TMouseTrackingTimer(MouseTrackingTimerList[I]);
    if Result.Caller = ACaller then Exit;
  end;
  Result := nil;
end;

procedure EndMouseTracking(ACaller: IcxMouseTrackingCaller);
var
  ATimer: TMouseTrackingTimer;
begin
  ATimer := GetMouseTrackingTimer(ACaller);
  if ATimer <> nil then
  begin
    MouseTrackingTimerList.Remove(ATimer);
    ATimer.Free;
  end;
end;

{ hourglass cursor showing }

var
  FPrevScreenCursor: TCursor;
  FHourglassCursorUseCount: Integer;

function IsMouseTracking(ACaller: IcxMouseTrackingCaller): Boolean;
begin
  Result := GetMouseTrackingTimer(ACaller) <> nil;
end;

procedure HideHourglassCursor;
begin
  if FHourglassCursorUseCount <> 0 then
  begin
    Dec(FHourglassCursorUseCount);
    if FHourglassCursorUseCount = 0 then
      Screen.Cursor := FPrevScreenCursor;
  end;
end;

procedure ShowHourglassCursor;
begin
  if FHourglassCursorUseCount = 0 then
  begin
    FPrevScreenCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
  end;
  Inc(FHourglassCursorUseCount);
end;

{ TcxControlChildComponent }

constructor TcxControlChildComponent.CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True);
begin
  //FControl := AControl;
  if AAssignOwner then
    Create(AControl.Owner)
  else
    Create(nil);
  Control := AControl;
end;

destructor TcxControlChildComponent.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveChildComponent(Self);
  inherited;
end;

function TcxControlChildComponent.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxControlChildComponent.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

procedure TcxControlChildComponent.SetControl(Value: TcxControl);
begin
  FControl := Value;
end;

procedure TcxControlChildComponent.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value is TcxControl then
    TcxControl(Value).AddChildComponent(Self);
end;

function TcxControlChildComponent.GetParentComponent: TComponent;
begin
  Result := FControl;
end;

function TcxControlChildComponent.HasParent: Boolean;
begin
  Result := FControl <> nil;
end;

{ TcxControlScrollBar }

constructor TcxControlScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  Color := clBtnFace;
  ControlStyle := ControlStyle - [csFramed] + [csNoDesignVisible];
  TabStop := False;
  Visible := False;
{$IFNDEF VCL}  //!!!
  Height := CYHSCROLL;
  Width := CXVSCROLL;
{$ENDIF}
{$IFDEF USETCXSCROLLBAR}
  LookAndFeel.MasterLookAndFeel := TcxControl(AOwner).LookAndFeel;
{$ENDIF}
end;

function TcxControlScrollBar.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TcxControlScrollBar.SetVisible(Value: boolean);
begin
  inherited Visible := Value;
{$IFNDEF VCL}
  if csDesigning in ComponentState then
    if Value then
      QWidget_show(Handle)
    else
      QWidget_hide(Handle);
{$ENDIF}
end;

{$IFDEF VCL}

procedure TcxControlScrollBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

{$ENDIF}

{$IFDEF VCL}

procedure TcxControlScrollBar.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONDOWN then FocusParent;
  inherited;
end;

{$ELSE}

function TcxControlScrollBar.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  if (QEvent_type(Event) = QEventType_MouseButtonPress) and
    (QMouseEvent_button(QMouseEventH(Event)) = ButtonState_LeftButton) then
    FocusParent;
  Result := inherited EventFilter(Sender, Event);
end;

procedure TcxControlScrollBar.InitWidget;
begin
  inherited InitWidget;
  QWidget_setFocusPolicy(Handle, QWidgetFocusPolicy_NoFocus);
  //QClxObjectMap_add(FHandle, Integer(FControl));
end;

procedure TcxControlScrollBar.WidgetDestroyed;
begin
  //QClxObjectMap_remove(FHandle);
  inherited WidgetDestroyed;
end;

function TcxControlScrollBar.CanFocus: Boolean;
begin
  Result := False;
end;

{$ENDIF}

procedure TcxControlScrollBar.FocusParent;
begin
  if Parent = nil then Exit;
  if (Parent is TcxControl) and TcxControl(Parent).FocusWhenChildIsClicked(Self) or
    not (Parent is TcxControl) and Parent.CanFocus then
    Parent.SetFocus;
end;

procedure TcxControlScrollBar.ApplyData;
begin
  if Data.Visible then
  begin
{$IFNDEF USETCXSCROLLBAR}
  {$IFDEF VCL}
    PageSize := Data.PageSize;
  {$ENDIF}
    SetParams(Data.Position, Data.Min, Data.Max);
  {$IFDEF VCL}
    PageSize := Data.PageSize;  // to resetup
  {$ENDIF}
{$ELSE}
    SetScrollParams(Data.Min, Data.Max, Data.Position, Data.PageSize, True);
{$ENDIF}
    SmallChange := Data.SmallChange;
    LargeChange := Data.LargeChange;
  end;
  Enabled := Data.Enabled;
  Visible := Data.Visible;
end;

{ TcxSizeGrip }

constructor TcxSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  Color := {$IFDEF VCL}clBtnFace{$ELSE}clButton{$ENDIF};
  ControlStyle := ControlStyle + [csNoDesignVisible];
end;

procedure TcxSizeGrip.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

{ TcxDragAndDropObject }

constructor TcxDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create;
  FControl := AControl;
  FCanvas := Control.Canvas;
  CurMousePos := Point(-1, -1);
  PrevMousePos := Point(-1, -1);
end;

procedure TcxDragAndDropObject.SetDirty(Value: Boolean);
begin
  if FDirty <> Value then
  begin
    FDirty := Value;
    DirtyChanged;
  end;
end;

procedure TcxDragAndDropObject.ChangeMousePos(const P: TPoint);
begin
  PrevMousePos := CurMousePos;
  CurMousePos := P;
end;

procedure TcxDragAndDropObject.DirtyChanged;
begin
end;

function TcxDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crNoDrop, crDrag);
begin
  Result := Cursors[Accepted];
end;

function TcxDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := False;
end;

procedure TcxDragAndDropObject.AfterDragAndDrop(Accepted: Boolean);
begin
end;

procedure TcxDragAndDropObject.BeginDragAndDrop;
begin
  DirtyChanged;
end;

procedure TcxDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Dirty := False;
  Screen.Cursor := GetDragAndDropCursor(Accepted);
end;

procedure TcxDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  Dirty := True;
end;

procedure TcxDragAndDropObject.DoBeginDragAndDrop;
begin
  GetCursorPos(CurMousePos);
  CurMousePos := Control.ScreenToClient(CurMousePos);
  PrevMousePos := CurMousePos;
  BeginDragAndDrop;
end;

procedure TcxDragAndDropObject.DoDragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  ChangeMousePos(P);
  DragAndDrop(P, Accepted);
end;

procedure TcxDragAndDropObject.DoEndDragAndDrop(Accepted: Boolean);
begin
  EndDragAndDrop(Accepted);
  AfterDragAndDrop(Accepted);
end;

{ TcxDragControlObject }

{$IFDEF VCL}

function TcxDragControlObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted and (Control as TcxControl).IsCopyDragDrop then
    Result := crDragCopy
  else
    Result := inherited GetDragCursor(Accepted, X, Y);
end;

{$ENDIF}

{ TcxControl }

constructor TcxControl.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TcxCanvas.Create(inherited Canvas);
  FDefaultCursor := Cursor;
  FFontListenerList := TInterfaceList.Create;
  FListenerLinks := TList.Create;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FScrollBars := ssBoth;
  CreateScrollBars;
  TabStop := MayFocus;
end;

destructor TcxControl.Destroy;
begin
  DestroyScrollBars;
  FFontListenerList := nil;
  FreeAndNil(FActiveCanvas);
  FCanvas.Free;
  FreeAndNil(FLookAndFeel);
  ClearListenerLinks;
  FreeAndNil(FListenerLinks);
  inherited;
end;

{$IFDEF DELPHI4}

procedure TcxControl.BeforeDestruction;
begin
  if not (csDestroying in ComponentState) then
    Destroying;
end;

{$ENDIF}
function TcxControl.GetActiveCanvas: TcxCanvas;
begin
  if HandleAllocated then
  begin
    if FActiveCanvas <> nil then FreeAndNil(FActiveCanvas);
    Result := Canvas;
  end
  else
  begin
    if FActiveCanvas = nil then
      FActiveCanvas := TcxScreenCanvas.Create;
    Result := FActiveCanvas;
  end;
end;

function TcxControl.GetDragAndDropObject: TcxDragAndDropObject;
begin
  if FDragAndDropObject = nil then
    FDragAndDropObject := GetDragAndDropObjectClass.Create(Self);
  Result := FDragAndDropObject;
end;

function TcxControl.GetHScrollBarVisible: Boolean;
begin
  if NeedsScrollBars then
    if FCalculatingScrollBarsParams then
      Result := FHScrollBar.Data.Visible
    else
      Result := FHScrollBar.Visible
  else
    Result := False;  
end;

function TcxControl.GetIsCopyDragDrop: Boolean;
begin
  Result := IsCtrlPressed;
end;

function TcxControl.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TcxControl.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxControl.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxControl.GetVScrollBarVisible: Boolean;
begin
  if NeedsScrollBars then
    if FCalculatingScrollBarsParams then
      Result := FVScrollBar.Data.Visible
    else
      Result := FVScrollBar.Visible
  else
    Result := False;  
end;

procedure TcxControl.SetBorderStyle(Value: TcxControlBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    BoundsChanged;
  end;
end;

procedure TcxControl.SetDragAndDropState(Value: TcxDragAndDropState);
begin
  if FDragAndDropState <> Value then
  begin
    FDragAndDropState := Value;
    if (Value = ddsNone) and not FFinishingDragAndDrop then DoCancelMode;
  end;
end;

procedure TcxControl.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxControl.SetKeys(Value: TcxKeys);
{$IFNDEF VCL}
const
  InputKey: array[TcxKey] of TInputKey = (ikAll, ikArrows, ikChars, ikTabs);
var
  I: TcxKey;
{$ENDIF}
begin
  if FKeys <> Value then
  begin
    FKeys := Value;
  {$IFNDEF VCL}
    InputKeys := [];
    for I := Low(TcxKey) to High(TcxKey) do
      if I in FKeys then
        InputKeys := InputKeys + [InputKey[I]];
  {$ENDIF}
  end;
end;

procedure TcxControl.SetMouseCaptureObject(Value: TObject);
var
  AIMouseCaptureObject: IcxMouseCaptureObject;
begin
  if FMouseCaptureObject <> Value then
  begin
    if (FMouseCaptureObject <> nil) and
      FMouseCaptureObject.GetInterface(IcxMouseCaptureObject, AIMouseCaptureObject) then
      AIMouseCaptureObject.DoCancelMode;
    FMouseCaptureObject := Value;
    MouseCapture := FMouseCaptureObject <> nil;
  end;
end;

procedure TcxControl.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    BoundsChanged;
  end;
end;

{$IFDEF VCL}

procedure TcxControl.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FinishDragAndDrop(False);
  DoCancelMode;
end;

procedure TcxControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if HasBackground then
    inherited
  else
    Message.Result := 1;
end;

procedure TcxControl.WMGetDlgCode(var Message: TWMGetDlgCode);
const
  DlgCodes: array[TcxKey] of Integer =
    (DLGC_WANTALLKEYS, DLGC_WANTARROWS, DLGC_WANTCHARS, DLGC_WANTTAB);
var
  I: TcxKey;
begin
  Message.Result := 0;
  for I := Low(TcxKey) to High(TcxKey) do
    if I in FKeys then
      Inc(Message.Result, DlgCodes[I]);
end;

procedure TcxControl.WMNCCalcSize(var Message: TMessage);
begin
  inherited;
  PostMessage(Handle, CM_NCSIZECHANGED, 0, 0);
end;

procedure TcxControl.CMColorChanged(var Message: TMessage);
begin
  ColorChanged;
  inherited;
end;

procedure TcxControl.CMCursorChanged(var Message: TMessage);
begin
  CursorChanged;
  inherited;
end;

procedure TcxControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  with Message do
    if Result = 0 then
      Result := Integer(GetDesignHitTest(XPos, YPos, KeysToShiftState(Keys)));
end;

procedure TcxControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FontChanged;
end;

procedure TcxControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxControl.CMNCSizeChanged(var Message: TMessage);
var
  ANewHeight, ANewWidth: Integer;
begin
  if not NeedsScrollBars then Exit;
  ANewHeight := GetSystemMetrics(SM_CYHSCROLL);
  ANewWidth  := GetSystemMetrics(SM_CXVSCROLL);
  if (FHScrollBar.Height <> ANewHeight) or (FVScrollBar.Width <> ANewWidth) then
  begin
    FHScrollBar.Height := ANewHeight;;
    FVScrollBar.Width := ANewWidth;
    BoundsChanged;
  end;
end;

procedure TcxControl.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  VisibleChanged;
end;

{$ENDIF}

{$IFNDEF DELPHI6}
type
  PPointer = ^Pointer;
{$ENDIF}

procedure TcxControl.ClearListenerLinks;
var
  I: Integer;
begin
  for I := FListenerLinks.Count - 1 downto 0 do
  begin
    PPointer(FListenerLinks[I])^ := nil;
    RemoveListenerLink(FListenerLinks[I]);
  end;
end;

procedure TcxControl.CreateScrollBars;
begin
  if not NeedsScrollBars then Exit;
  FHScrollBar := TcxControlScrollBar.Create(Self);
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.OnScroll := ScrollEvent;
  FVScrollBar := TcxControlScrollBar.Create(Self);
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.OnScroll := ScrollEvent;
  FSizeGrip := TcxSizeGrip.Create(Self);
{$IFNDEF VCL}
  FHScrollBar.Height := CYHSCROLL;
  FVScrollBar.Width := CXVSCROLL;
{$ENDIF}
{$IFDEF VCL}
  {$IFDEF USETCXSCROLLBAR}
    cxSettingsController.AddListener(Self);
  {$ENDIF}
{$ENDIF}
end;

procedure TcxControl.DestroyScrollBars;
begin
  if not NeedsScrollBars then Exit;
{$IFDEF VCL}
  {$IFDEF USETCXSCROLLBAR}
    cxSettingsController.RemoveListener(Self);
  {$ENDIF}
{$ENDIF}
  FSizeGrip.Free;
  FVScrollBar.Free;
  FHScrollBar.Free;
end;

procedure TcxControl.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Scroll(TcxControlScrollBar(Sender).Kind, ScrollCode, ScrollPos);
end;

{$IFDEF VCL}

procedure TcxControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    Style := Style or WS_CLIPCHILDREN;
end;

procedure TcxControl.CreateWnd;
begin
  inherited;
  InitControl;
end;

procedure TcxControl.Resize;
begin
  inherited;
  BoundsChanged;
end;

procedure TcxControl.WndProc(var Message: TMessage);
var
  ASelf: TcxControl;

  function GetMousePos: TPoint;
  begin
    if (Width > 32768) or (Height > 32768) then
    begin
      GetCursorPos(Result);
      Result := ScreenToClient(Result);
    end
    else
      Result := SmallPointToPoint(TWMMouse(Message).Pos);
  end;

  function GetMouseButton: TMouseButton;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN:
        Result := mbLeft;
      WM_RBUTTONDOWN:
        Result := mbRight;
    else
      Result := mbMiddle;
    end;
  end;

  procedure DoAfterMouseDown;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
        with GetMousePos do
          AfterMouseDown(GetMouseButton, X, Y);
    end;
  end;

begin
  ASelf := Self;
  AddListenerLink(@ASelf);
  try
    with TWMMouse(Message), GetMousePos do
      if (DragMode = dmAutomatic) and not Dragging and
        ((Msg = WM_LBUTTONDOWN) or (Msg = WM_LBUTTONDBLCLK)) and
        (not IsDesigning or GetDesignHitTest(X, Y, [ssLeft])) then
      begin
        if not IsControlMouseMsg(TWMMouse(Message)) then
        begin
          ControlState := ControlState + [csLButtonDown];
          Dispatch(Message);
          ControlState := ControlState - [csLButtonDown];
        end;
        Exit;
      end;
    inherited;
  finally
    if ASelf <> nil then
    begin
      RemoveListenerLink(@ASelf);
      case Message.Msg of
    {    WM_KEYDOWN:
          if Message.wParam = VK_ESCAPE then FinishDragAndDrop(False);//!!!}
        WM_SETFOCUS, WM_KILLFOCUS:
          FocusChanged;
      end;
      DoAfterMouseDown;
    end;
  end;
end;

{$ELSE}

procedure TcxControl.InitWidget;
begin
  inherited;
  if not MayFocus then
    QWidget_setFocusPolicy(Handle, QWidgetFocusPolicy_NoFocus);
  if not HasBackground then
    QWidget_setBackgroundMode(Handle, QWidgetBackgroundMode_NoBackground);
  InitControl;
end;

function TcxControl.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := inherited EventFilter(Sender, Event);
  case QEvent_type(Event) of
    QEventType_FocusIn, QEventType_FocusOut:
      FocusChanged;
  end;
end;

procedure TcxControl.SetParent(const AParent: TWidgetControl);
begin                             {4}
  // no check for <> in inherited method - to avoid bugs from ReSubmitFlags
  if (csLoading in ComponentState) or (Parent <> AParent) then
    inherited;
end;

{$ENDIF}

function TcxControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  {$IFNDEF VCL}const{$ENDIF} MousePos: TPoint): Boolean;
const
  ADirections: array[Boolean, Boolean] of TcxDirection = ((dirLeft, dirRight), (dirUp, dirDown));
var
  I: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and (MouseWheelScrollingKind <> mwskNone) then
  begin
    for I := 0 to {$IFDEF VCL}Mouse.WheelScrollLines{$ELSE}QApplication_wheelScrollLines{$ENDIF} - 1 do
      ScrollContent(ADirections[MouseWheelScrollingKind = mwskVertical, WheelDelta < 0]);
    Result := True;
  end;
end;

procedure TcxControl.DoStartDrag(var DragObject: TDragObject);
{$IFNDEF VCL}
var
  ADragObject: TDragObject;
{$ENDIF}
begin
  inherited;
  if DragObject = nil then
    DragObject := GetDragObjectClass.Create(Self);
  if not StartDrag(DragObject) then
  begin
  {$IFDEF VCL}
    DragObject.Free;
    DragObject := nil;
    CancelDrag;
  {$ELSE}
    ADragObject := DragObject;
    EndDrag(False);
    ADragObject.Free;
  {$ENDIF}
  end;
end;

procedure TcxControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then FinishDragAndDrop(False);
  inherited;
end;

procedure TcxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ASelf: TcxControl;

  procedure ProcessDragAndDrop;
  begin
    if (Button = mbLeft) and not (ssDouble in Shift) and StartDragAndDrop(Point(X, Y)) then
      if DragAndDropObject.ImmediateStart then
        BeginDragAndDrop
      else
        DragAndDropState := ddsStarting
    else
      FinishDragAndDrop(False);
  end;

begin
  FMouseDownPos := Point(X, Y);
  ASelf := Self;
  AddListenerLink(@ASelf);
  try
    if CanFocusOnClick then
    begin
      if not (ssDouble in Shift) then SetFocus;  // to allow form showing on dbl click
      if ASelf = nil then Exit;
      if not IsFocused and not AllowDragAndDropWithoutFocus then
      begin
        MouseCapture := False;
        Exit;
      end;
    end;
    ProcessDragAndDrop;
    if ASelf = nil then Exit;
    BeforeMouseDown(Button, Shift, X, Y);
    if ASelf = nil then Exit;
    inherited;
  finally
    if ASelf <> nil then
      RemoveListenerLink(@ASelf);
  end;
end;

procedure TcxControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AAccepted: Boolean;

  procedure SetCursor;
  var
    ACursor: TCursor;
  begin
    ACursor := FDefaultCursor;
    Cursor := GetCursor(X, Y);
    FDefaultCursor := ACursor;
  end;

begin
  SetCursor;
  inherited;
  if (DragAndDropState = ddsStarting) and not IsMouseInPressedArea(X, Y) then
    BeginDragAndDrop;
  if DragAndDropState = ddsInProcess then
  begin
    AAccepted := False;
    DragAndDrop(Point(X, Y), AAccepted);
  end;
end;

procedure TcxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FinishDragAndDrop(True);
  MouseCaptureObject := nil;
end;

procedure TcxControl.Paint;
begin
  if FBorderStyle = cxcbsDefault then
  begin
    LookAndFeelPainter.DrawBorder(Canvas, Bounds);
    Canvas.IntersectClipRect(ClientBounds);
  end;
  inherited;
end;

{$IFDEF VCL}

procedure TcxControl.AfterMouseDown(AButton: TMouseButton; X, Y: Integer);
begin
  if (DragMode = dmAutomatic) and
    (not IsDesigning or AllowAutoDragAndDropAtDesignTime(X, Y, [])) and
    CanDrag(X, Y) and (DragDetect(Handle) = ddDrag) then
    BeginDrag(False);
  if AButton = mbMiddle then DoScrolling;
end;

procedure TcxControl.ColorChanged;
begin
end;

procedure TcxControl.DoScrolling;
const
  ScrollTimeStep = 20;
  ScrollValueStep = 5;
  MaxSpeed = 12;
var
  BreakOnMouseUp: Boolean;
  AllowHorScrolling, AllowVerScrolling: Boolean;
  P, PrevP, AnchorPos: TPoint;
  AnchorSize, Speed, TimerHits: Integer;
  AnchorWnd, CaptureWnd: HWND;
  Direction: TcxDirection;
  Timer: UINT;
  Msg: TMsg;

  function CreateScrollingAnchorWnd: HWND;
  var
    B: TBitmap;
    W, H: Integer;
    Rgn: HRGN;
    DC: HDC;

    function GetResourceBitmapName: string;
    begin
      if AllowHorScrolling and AllowVerScrolling then
        Result := 'CX_FULLSCROLLBITMAP'
      else
        if AllowHorScrolling then
          Result := 'CX_HORSCROLLBITMAP'
        else
          Result := 'CX_VERSCROLLBITMAP';
    end;

  begin
    B := TBitmap.Create;
    B.LoadFromResourceName(HInstance, GetResourceBitmapName);

    W := B.Width;
    H := B.Height;
    AnchorSize := W;
    with AnchorPos do
      Result := CreateWindowEx(WS_EX_TOPMOST, 'STATIC', nil, WS_POPUP,
        X - W div 2, Y - H div 2, W, H, Handle, 0, HInstance, nil);
    Rgn := CreateEllipticRgn(0, 0, W + 1, H + 1);
    SetWindowRgn(Result, Rgn, True);
    SetWindowPos(Result, 0, 0, 0, 0, 0,
      SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);

    DC := GetWindowDC(Result);
    BitBlt(DC, 0, 0, W, H, B.Canvas.Handle, 0, 0, SRCCOPY);
    Rgn := CreateEllipticRgn(0, 0, W + 1, H + 1);
    FrameRgn(DC, Rgn, GetSysColorBrush(COLOR_WINDOWTEXT), 1, 1);
    DeleteObject(Rgn);
    ReleaseDC(Result, DC);

    B.Free;
  end;

  procedure CalcDirectionAndSpeed(const P: TPoint);
  var
    DeltaX, DeltaY, SpeedValue: Integer;

    function GetNeutralZone: TRect;
    begin
      with AnchorPos do
        Result := Classes.Bounds(X - AnchorSize div 2, Y - AnchorSize div 2, AnchorSize, AnchorSize);
      if not AllowHorScrolling then
      begin
        Result.Left := 0;
        Result.Right := Screen.Width;
      end;
      if not AllowVerScrolling then
      begin
        Result.Top := 0;
        Result.Bottom := Screen.Height;
      end;
    end;

  begin
    if PtInRect(GetNeutralZone, P) then
    begin
      Direction := dirNone;
      Speed := 0;
      Exit;
    end
    else
    begin
      BreakOnMouseUp := True;
      DeltaX := P.X - AnchorPos.X;
      DeltaY := P.Y - AnchorPos.Y;
      if AllowHorScrolling and (not AllowVerScrolling or (Abs(DeltaX) > Abs(DeltaY))) then
      begin
        if DeltaX < 0 then
          Direction := dirLeft
        else
          Direction := dirRight;
        SpeedValue := Abs(DeltaX);
      end
      else
      begin
        if DeltaY < 0 then
          Direction := dirUp
        else
          Direction := dirDown;
        SpeedValue := Abs(DeltaY);
      end;
    end;
    Dec(SpeedValue, AnchorSize div 2);
    Speed := 1 + SpeedValue div ScrollValueStep;
    if Speed > MaxSpeed then Speed := MaxSpeed;
  end;

  procedure SetMouseCursor;
  var
    Cursor: TCursor;
  begin
    case Direction of
      dirLeft:
        Cursor := crLeftScroll;
      dirUp:
        Cursor := crUpScroll;
      dirRight:
        Cursor := crRightScroll;
      dirDown:
        Cursor := crDownScroll;
    else
      if AllowHorScrolling and AllowVerScrolling then
        Cursor := crFullScroll
      else
        if AllowHorScrolling then
          Cursor := crHorScroll
        else
          Cursor := crVerScroll;
    end;
    SetCursor(Screen.Cursors[Cursor]);
  end;

begin
  AllowHorScrolling := HScrollBarVisible;
  AllowVerScrolling := VScrollBarVisible;
  if not (AllowHorScrolling or AllowVerScrolling) then Exit;
  BreakOnMouseUp := False;
  GetCursorPos(PrevP);
  AnchorPos := PrevP;
  AnchorWnd := CreateScrollingAnchorWnd;
  Direction := dirNone;
  SetMouseCursor;
  Speed := 1;
  TimerHits := 0;
  Timer := SetTimer(0, 0, ScrollTimeStep, nil);

  CaptureWnd := Handle;
  SetCapture(CaptureWnd);
  try
    while GetCapture = CaptureWnd do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
            PostQuitMessage(Msg.wParam);
            Break;
          end;
      end;
      if (Msg.message = WM_PAINT) and (Msg.hwnd = AnchorWnd) then
      begin
        ValidateRect(AnchorWnd, nil);
        Continue;
      end;
      if (Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST) and
        (Msg.message <> WM_MOUSEMOVE) and (Msg.message <> WM_MBUTTONUP) then
        Break;
      try
        case Msg.message of
          WM_KEYDOWN, WM_KEYUP:
            if Msg.wParam = VK_ESCAPE then Break;
          WM_MOUSEMOVE:
            begin
              P := SmallPointToPoint(TSmallPoint(Msg.lParam));
              Windows.ClientToScreen(Msg.hwnd, P);
              if (P.X <> PrevP.X) or (P.Y <> PrevP.Y) then
              begin
                CalcDirectionAndSpeed(P);
                SetMouseCursor;
                PrevP := P;
              end;
            end;
          WM_LBUTTONDOWN, WM_RBUTTONDOWN:
            Break;
          WM_MBUTTONUP:
            if BreakOnMouseUp then Break;
          WM_TIMER:
            if UINT(Msg.wParam) = Timer then
            begin
              Inc(TimerHits);
              if TimerHits mod (MaxSpeed - Speed + 1) = 0 then
                ScrollContent(Direction);
            end;
        end;
      finally
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = CaptureWnd then ReleaseCapture;

    KillTimer(0, Timer);
    DestroyWindow(AnchorWnd);
  end;
end;

procedure TcxControl.Modified;
var
  AForm: TCustomForm;
begin
  AForm := GetParentForm(Self);
  if (AForm <> nil) and (AForm.Designer <> nil) then
    AForm.Designer.Modified;
end;

procedure TcxControl.VisibleChanged;
begin
end;

{$ENDIF}

procedure TcxControl.AddChildComponent(AComponent: TcxControlChildComponent);
begin
  AComponent.Control := Self;
end;

procedure TcxControl.RemoveChildComponent(AComponent: TcxControlChildComponent);
begin
  AComponent.Control := nil;
end;

procedure TcxControl.AddFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Add(AListener);
end;

procedure TcxControl.RemoveFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Remove(AListener);
end;

function TcxControl.AllowAutoDragAndDropAtDesignTime(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TcxControl.AllowDragAndDropWithoutFocus: Boolean;
begin
  Result := False;
end;

procedure TcxControl.BeforeMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
end;

procedure TcxControl.BoundsChanged;
begin
  {$IFNDEF VCL}inherited;{$ENDIF}
  UpdateScrollBars;
end;

function TcxControl.CanDrag(X, Y: Integer): Boolean;
begin
  Result := DragAndDropState = ddsNone;
end;

function TcxControl.CanFocusOnClick: Boolean;
begin
  Result := not IsDesigning and MayFocus and (CanFocus {or (GetParentForm(Self) = nil)!!!});
end;

procedure TcxControl.CursorChanged;
begin
  FDefaultCursor := Cursor;
  {$IFNDEF VCL}inherited;{$ENDIF}
end;

procedure TcxControl.DoCancelMode;
begin
  MouseCaptureObject := nil;
end;

procedure TcxControl.FocusChanged;
begin
  if Assigned(FOnFocusChanged) then FOnFocusChanged(Self);
end;

function TcxControl.FocusWhenChildIsClicked(AChild: TControl): Boolean;
begin
  Result := CanFocusOnClick;
end;

procedure TcxControl.FontChanged;
var
  I: Integer;
begin
  for I := 0 to FFontListenerList.Count - 1 do
    IcxFontListener(FFontListenerList[I]).Changed(Self, Font);
{$IFDEF VCL}
  Invalidate;
{$ELSE}
  inherited;
{$ENDIF}
end;

function TcxControl.GetBorderSize: Integer;
begin
  if FBorderStyle = cxcbsNone then
    Result := 0
  else
    Result := LookAndFeelPainter.BorderSize;
end;

function TcxControl.GetBounds: TRect;
begin
  if HandleAllocated then
    Result := ClientRect
  else
    Result := Rect(0, 0, Width, Height);
end;

function TcxControl.GetClientBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, -BorderSize, -BorderSize);
  if HScrollBarVisible then
    Dec(Result.Bottom, FHScrollBar.Height);
  if VScrollBarVisible then
    Dec(Result.Right, FVScrollBar.Width);
end;

function TcxControl.GetCursor(X, Y: Integer): TCursor;
begin
  Result := FDefaultCursor;
end;

function TcxControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := DragAndDropState <> ddsNone;
end;

function TcxControl.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TcxDragControlObject;
end;

function TcxControl.GetIsFocused: Boolean;
begin                                {7}
{$IFDEF VCL}
  Result := Focused;
{$ELSE}
  // Focused is wrong when form/app is inactive
  Result := HandleAllocated and QWidget_hasFocus(Handle);
{$ENDIF}
end;

function TcxControl.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  if VScrollBarVisible then
    Result := mwskVertical
  else
    Result := mwskNone;
end;

function TcxControl.HasBackground: Boolean;
begin
  Result := False;
end;

procedure TcxControl.InitControl;
begin
  if NeedsScrollBars then
  begin
    FHScrollBar.Parent := Self;
    FVScrollBar.Parent := Self;
    FSizeGrip.Parent := Self;
  end;  
end;

function TcxControl.IsInternalControl(AControl: TControl): Boolean;
begin
  Result := (AControl = FHScrollBar) or (AControl = FVScrollBar) or
    (AControl = FSizeGrip);
end;

function TcxControl.MayFocus: Boolean;
begin
  Result := True;
end;

procedure TcxControl.MouseEnter(AControl: TControl);
begin
  {$IFNDEF VCL}inherited;{$ENDIF}
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcxControl.MouseLeave(AControl: TControl);
begin
  {$IFNDEF VCL}inherited;{$ENDIF}
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

function TcxControl.GetLookAndFeelPainter: TcxCustomLookAndFeelPainterClass;
begin
  Result := FLookAndFeel.Painter;
end;

procedure TcxControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
end;

procedure TcxControl.InitScrollBarsParameters;
begin
end;

function TcxControl.IsPixelScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

function TcxControl.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxControl.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);
begin
end;

function TcxControl.GetHScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  Result.Top := Result.Bottom;
  Result.Bottom := Result.Top + FHScrollBar.Height;
end;

function TcxControl.GetSizeGripBounds: TRect;
begin
  with Result do
  begin
    Left := FVScrollBar.Left;
    Right := Left + FVScrollBar.Width;
    Top := FHScrollBar.Top;
    Bottom := Top + FHScrollBar.Height;
  end;
end;

function TcxControl.GetVScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  Result.Left := Result.Right;
  Result.Right := Result.Left + FVScrollBar.Width;
end;

procedure TcxControl.SetInternalControlsBounds;
begin
  if FHScrollBar.Visible then
    FHScrollBar.BoundsRect := GetHScrollBarBounds;
  if FVScrollBar.Visible then
    FVScrollBar.BoundsRect := GetVScrollBarBounds;
  if FSizeGrip.Visible then
    FSizeGrip.BoundsRect := GetSizeGripBounds;
end;

procedure TcxControl.UpdateScrollBars;
var
  APrevUpdatingScrollBars, APrevHScrollBarVisible, APrevVScrollBarVisible: Boolean;

  procedure CalculateScrollBarsParams;
  var
    APrevHScrollBarVisible, APrevVScrollBarVisible: Boolean;

    procedure CheckPixelScrollBars;
    var
      AHideScrollBar: array[TScrollBarKind] of Boolean;
      I: TScrollBarKind;

      function GetScrollBar(AScrollBarKind: TScrollBarKind): TcxControlScrollBar;
      begin
        if AScrollBarKind = sbHorizontal then
          Result := FHScrollBar
        else
          Result := FVScrollBar;
      end;

      function CanHide(AScrollBarKind: TScrollBarKind): Boolean;
      begin
        with GetScrollBar(AScrollBarKind).Data do
          Result := Visible and Enabled and AllowHide;
      end;

      function GetOppositeScrollBarSize(AScrollBarKind: TScrollBarKind): Integer;
      begin
        if AScrollBarKind = sbHorizontal then
          Result := FVScrollBar.Width
        else
          Result := FHScrollBar.Height;
      end;

      procedure CheckPixelScrollBar(AScrollBarKind: TScrollBarKind);
      begin
        if IsPixelScrollBar(AScrollBarKind) then
          with GetScrollBar(AScrollBarKind).Data do
            if PageSize + GetOppositeScrollBarSize(AScrollBarKind) >= Max - Min + 1 then
              AHideScrollBar[AScrollBarKind] := True;
      end;

    begin
      if not CanHide(sbHorizontal) or not CanHide(sbVertical) then Exit;
      AHideScrollBar[sbHorizontal] := False;
      AHideScrollBar[sbVertical] := False;
      CheckPixelScrollBar(sbHorizontal);
      CheckPixelScrollBar(sbVertical);
      if AHideScrollBar[sbHorizontal] and AHideScrollBar[sbVertical] then
        for I := Low(TScrollBarKind) to High(TScrollBarKind) do
          with GetScrollBar(I).Data do
            SetScrollBarInfo(I, Min, Max, SmallChange, PageSize + GetOppositeScrollBarSize(I),
              Position, AllowShow, AllowHide);
    end;

  begin
    if FCalculatingScrollBarsParams then Exit;
    FCalculatingScrollBarsParams := True;
    try
      FHScrollBar.Data.Visible := False;
      FVScrollBar.Data.Visible := False;
      repeat
        APrevHScrollBarVisible := HScrollBarVisible;
        APrevVScrollBarVisible := VScrollBarVisible;
        //BoundsChanged; - causes things like Left/TopPos to be recalculated during scrolling
        InitScrollBarsParameters;
      until (HScrollBarVisible = APrevHScrollBarVisible) and
        (VScrollBarVisible = APrevVScrollBarVisible);
      CheckPixelScrollBars;
    finally
      FCalculatingScrollBarsParams := False;
    end;
  end;

  function GetIsInitialScrollBarsParams: Boolean;
  begin
    Result := APrevUpdatingScrollBars and
      (FHScrollBar.Data.Visible = FInitialHScrollBarVisible) and
      (FVScrollBar.Data.Visible = FInitialVScrollBarVisible);
  end;

  procedure CheckScrollBarParams(AScrollBar: TcxControlScrollBar; AInitialVisible: Boolean);
  begin
    with AScrollBar do
      if Visible <> AInitialVisible then
      begin
        Enabled := False;
        Visible := True;
      end;
  end;

  function IsBoundsChangeNeeded: Boolean;
  begin
    Result :=
      (FHScrollBar.Visible <> APrevHScrollBarVisible) or
      (FVScrollBar.Visible <> APrevVScrollBarVisible);
  end;

  procedure BringInternalControlsToFront;
  begin
    FHScrollBar.BringToFront;
    FVScrollBar.BringToFront;
    FSizeGrip.BringToFront;
  end;

begin
  if not NeedsScrollBars then Exit;
  if not FUpdatingScrollBars then
  begin
    FInitialHScrollBarVisible := FHScrollBar.Visible;
    FInitialVScrollBarVisible := FVScrollBar.Visible;
  end;
  APrevUpdatingScrollBars := FUpdatingScrollBars;
  FUpdatingScrollBars := True;
  try
    APrevHScrollBarVisible := FHScrollBar.Visible;
    APrevVScrollBarVisible := FVScrollBar.Visible;
    if not FIsInitialScrollBarsParams then
    begin
      CalculateScrollBarsParams;
      FIsInitialScrollBarsParams := GetIsInitialScrollBarsParams;
      if FIsInitialScrollBarsParams then
      begin
        CheckScrollBarParams(FHScrollBar, FInitialHScrollBarVisible);
        CheckScrollBarParams(FVScrollBar, FInitialVScrollBarVisible);
      end
      else
      begin
        FHScrollBar.ApplyData;
        FVScrollBar.ApplyData;
      end;
    end;
    if IsBoundsChangeNeeded then
      BoundsChanged
    else
    begin
      FSizeGrip.Visible := FHScrollBar.Visible and FVScrollBar.Visible;
      SetInternalControlsBounds;
      BringInternalControlsToFront;
    end;
  finally
    if not APrevUpdatingScrollBars then
      FIsInitialScrollBarsParams := False;
    FUpdatingScrollBars := APrevUpdatingScrollBars;
  end;
end;

procedure TcxControl.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  DragAndDropObject.DoDragAndDrop(P, Accepted);
end;

procedure TcxControl.EndDragAndDrop(Accepted: Boolean);
begin
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;
  MouseCapture := False;
  MouseCaptureObject := nil;
  DragAndDropObject.DoEndDragAndDrop(Accepted);
  {DragAndDropObject.DoEndDragAndDrop(Accepted);
  SetCaptureControl(nil);
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;}
end;

function TcxControl.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := FDragAndDropObjectClass;
  if Result = nil then
    Result := TcxDragAndDropObject;
end;

function TcxControl.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxControl.AddListenerLink(ALink: Pointer);
begin
  if FListenerLinks <> nil then
    FListenerLinks.Add(ALink);
end;

procedure TcxControl.RemoveListenerLink(ALink: Pointer);
begin
  if FListenerLinks <> nil then
    FListenerLinks.Remove(ALink);
end;

function TcxControl.CanFocusEx: Boolean;
var
  AParentForm: TCustomForm;
begin
  AParentForm := GetParentForm(Self);
  Result := CanFocus and ((AParentForm = nil) or
    AParentForm.CanFocus and AParentForm.Enabled and AParentForm.Visible);
end;

function TcxControl.AcceptMousePosForClick(X, Y: Integer): Boolean;
begin
  Result := (DragMode = dmManual) or IsMouseInPressedArea(X, Y);
end;

{$IFDEF VCL}

procedure TcxControl.InvalidateRect(const R: TRect; EraseBackground: Boolean);
begin
  if HandleAllocated then
    Windows.InvalidateRect(Handle, @R, EraseBackground)
end;

{$ENDIF}

function TcxControl.IsMouseInPressedArea(X, Y: Integer): Boolean;
begin
  Result := IsPointInDragDetectArea(MouseDownPos, X, Y);
end;

procedure TcxControl.ScrollContent(ADirection: TcxDirection);
var
  AScrollPos: Integer;

  function GetScrollBarKind: TScrollBarKind;
  begin
    if ADirection in [dirLeft, dirRight] then
      Result := sbHorizontal
    else
      Result := sbVertical;
  end;

  function GetScrollCode: TScrollCode;
  begin
    if ADirection in [dirLeft, dirUp] then
      Result := scLineUp
    else
      Result := scLineDown;
  end;

  function GetScrollPos: Integer;
  begin
    if GetScrollBarKind = sbHorizontal then
      Result := FHScrollBar.Position
    else
      Result := FVScrollBar.Position;
  end;

begin
  if (ADirection = dirNone) or not NeedsScrollBars then Exit;
  AScrollPos := GetScrollPos;
  Scroll(GetScrollBarKind, GetScrollCode, AScrollPos);
end;

procedure TcxControl.SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
  AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);

  function GetScrollBar: TcxControlScrollBar;
  begin
    if AScrollBarKind = sbHorizontal then
      Result := FHScrollBar
    else
      Result := FVScrollBar;
  end;

begin
  with GetScrollBar.Data do
  begin
    if AScrollBarKind = sbHorizontal then
      AllowShow := AAllowShow and (FScrollBars in [ssHorizontal, ssBoth])
    else
      AllowShow := AAllowShow and (FScrollBars in [ssVertical, ssBoth]);
    AllowHide := AAllowHide;
    if AllowShow then
      if (AMax < AMin) or (AMax - AMin + 1 <= APage) or (APos < AMin) then
        if AllowHide then
          Visible := False
        else
        begin
          Enabled := False;
          Visible := True;
        end
      else
      begin
      {$IFNDEF VCL}
        Dec(AMax, APage - 1);
      {$ENDIF}
        Min := AMin;
      {$IFDEF VCL}
        if PageSize > AMax then
          PageSize := AMax;
      {$ENDIF}
        Max := AMax;
        SmallChange := AStep;
        LargeChange := APage;
      {$IFDEF VCL}
        PageSize := APage;
      {$ENDIF}
        Position := APos;

        Enabled := True;
        Visible := True;
      end
    else
      Visible := False;
  end;    
end;

function TcxControl.StartDrag({var }DragObject: TDragObject): Boolean;
begin
{$IFDEF VCL}
  Result := True;
{$ELSE}
  with MouseDownPos do
    Result := CanDrag(X, Y);
{$ENDIF}
end;

procedure TcxControl.PostMouseMove(AMousePos: PPoint = nil);
{$IFNDEF VCL}
var
  P: TPoint;
{$ENDIF}  

  function GetMousePos: TPoint;
  begin
    if AMousePos = nil then
      Result := ScreenToClient(Mouse.CursorPos)
    else
      Result := AMousePos^;
  end;

begin
  if not HandleAllocated then Exit;
{$IFDEF VCL}
  with GetMousePos do
    PostMessage(Handle, WM_MOUSEMOVE, 0, MakeLParam(X, Y));
{$ELSE}
  P := GetMousePos;
  QApplication_postEvent(Handle, QMouseEvent_create(QEventType_MouseMove, @P,
    Integer(ButtonState_NoButton), Integer(ButtonState_NoButton)));
{$ENDIF}
end;

procedure TcxControl.BeginDragAndDrop;
begin
  DragAndDropObject.DoBeginDragAndDrop;
  MouseCapture := True;
  FDragAndDropPrevCursor := Screen.Cursor;
  DragAndDropState := ddsInProcess;
end;

procedure TcxControl.FinishDragAndDrop(Accepted: Boolean);
begin
  if FFinishingDragAndDrop then Exit;
  FFinishingDragAndDrop := True;
  try
    if DragAndDropState = ddsInProcess then
      EndDragAndDrop(Accepted)
    else
      DragAndDropState := ddsNone;
    FreeAndNil(FDragAndDropObject);
  finally
    FFinishingDragAndDrop := False;
  end;
end;

{ TcxCustomizeListBox }

constructor TcxCustomizeListBox.Create(AOwner: TComponent);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

function TcxCustomizeListBox.GetDragAndDropItemObject: TObject;
begin
  Result := Items.Objects[FDragAndDropItemIndex];
end;

function TcxCustomizeListBox.GetItemObject: TObject;
begin
  if ItemIndex = -1 then
    Result := nil
  else
    Result := Items.Objects[ItemIndex];
end;

procedure TcxCustomizeListBox.SetItemObject(Value: TObject);
begin
  if ItemObject <> Value then
  begin
    ItemIndex := Items.IndexOfObject(Value);
    Click;
  end;
end;

{$IFDEF VCL}

procedure TcxCustomizeListBox.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

procedure TcxCustomizeListBox.WMMouseMove(var Message: TWMMouseMove);
begin
  if FDragAndDropItemIndex = -1 then
    inherited
  else
    with Message do
      MouseMove(KeysToShiftState(Keys), XPos, YPos);
end;

procedure TcxCustomizeListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do
    style := style or CS_HREDRAW;
end;

{$ENDIF}

procedure TcxCustomizeListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (ItemAtPos(Point(X, Y), True) <> -1) and
    (ItemObject <> nil) then
  begin
    FDragAndDropItemIndex := ItemIndex;
    FMouseDownPos := Point(X, Y);
  end;
end;

procedure TcxCustomizeListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FDragAndDropItemIndex <> -1) and
    (not IsPointInDragDetectArea(FMouseDownPos, X, Y) or
     (ItemAtPos(Point(X, Y), True) <> FDragAndDropItemIndex)) then
  begin
    ItemIndex := FDragAndDropItemIndex;
    BeginDragAndDrop;
    FDragAndDropItemIndex := -1;
  end;
end;

procedure TcxCustomizeListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

{$IFNDEF VCL}

procedure TcxCustomizeListBox.Resize;
begin
  inherited;
  Invalidate;
end;

{$ENDIF}

procedure TcxCustomizeListBox.BeginDragAndDrop;
var
  I: Integer;
begin
  if MultiSelect then
    with Items do
    begin
      BeginUpdate;
      try
        for I := 0 to Count - 1 do
          Selected[I] := I = ItemIndex;
      finally
        EndUpdate;
      end;
    end;
end;

{ TcxPopupWindow }

constructor TcxPopupWindow.Create;
begin
  CreateNew(nil);
  inherited BorderStyle := {$IFDEF VCL}bsNone{$ELSE}fbsNone{$ENDIF};
{$IFDEF VCL}
  DefaultMonitor := dmDesktop;
{$ENDIF}
  FormStyle := fsStayOnTop;
  FAdjustable := True;
  FAlignVert := pavBottom;
  FCanvas := TcxCanvas.Create(inherited Canvas);
  FDirection := pdVertical;
  FFrameColor := clWindowText;
end;

destructor TcxPopupWindow.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TcxPopupWindow.GetNCHeight: Integer;
begin
  Result := BorderWidths[bTop] + BorderWidths[bBottom];
end;

function TcxPopupWindow.GetNCWidth: Integer;
begin
  Result := BorderWidths[bLeft] + BorderWidths[bRight];
end;

procedure TcxPopupWindow.SetBorderSpace(Value: Integer);
begin
  RestoreControlsBounds;
  FBorderSpace := Value;
end;

procedure TcxPopupWindow.SetBorderStyle(Value: TcxPopupBorderStyle);
begin
  RestoreControlsBounds;
  FBorderStyle := Value;
end;

{$IFDEF VCL}

procedure TcxPopupWindow.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active <> WA_INACTIVE then
  begin
    FPrevActiveWindow := Message.ActiveWindow;
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(True), 0);
  end;
end;

procedure TcxPopupWindow.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
  begin
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(False), 0);
    CloseUp;
  end;
end;

procedure TcxPopupWindow.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  VisibleChanged;
end;

{$ELSE}

procedure TcxPopupWindow.AppDeactivationTimerHandler(Sender: TObject);
begin
  if FindControl(QApplication_focusWidget(Application.Handle)) = nil then
    CloseUp;
end;

{$ENDIF}

procedure TcxPopupWindow.Deactivate;
begin
  inherited;
  CloseUp;
end;

procedure TcxPopupWindow.Paint;
begin
  inherited;
  DrawFrame;
end;

{$IFDEF VCL}

procedure TcxPopupWindow.VisibleChanged;
begin
end;

{$ELSE}

function TcxPopupWindow.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := inherited EventFilter(Sender, Event);
  {case QEvent_type(Event) of
    QEventType_WindowDeactivate:
      begin
        CloseUp;
      end;
  end;}
end;

{$ENDIF}

function TcxPopupWindow.CalculatePosition: TPoint;
var
  AAlignHorz: TcxPopupAlignHorz;
  AAlignVert: TcxPopupAlignVert;
  AOwnerScreenBounds: TRect;
  AOrigin: TPoint;

  procedure CalculateOwnerScreenBounds;
  begin
    AOwnerScreenBounds := FOwnerBounds;
    with FOwnerParent, AOwnerScreenBounds do
    begin
      TopLeft := ClientToScreen(TopLeft);
      BottomRight := ClientToScreen(BottomRight);
    end;
  end;

  procedure CalculateCommonPosition;
  begin
    with AOwnerScreenBounds, Result do
    begin
      if AAlignHorz = pahCenter then
      begin
        X := (Left + Right - Width) div 2;
        AOrigin.X := X;
      end;
      if AAlignVert = pavCenter then
      begin
        Y := (Top + Bottom - Height) div 2;
        AOrigin.Y := Y;
      end;  
    end;
  end;

  procedure CalculateHorizontalPosition;
  begin
    with AOwnerScreenBounds, Result do
    begin
      case AAlignHorz of
        pahLeft:
          begin
            X := Left - Width;
            AOrigin.X := Left;
          end;
        pahRight:
          begin
            X := Right;
            AOrigin.X := Right;
          end;
      end;
      case AAlignVert of
        pavTop:
          begin
            Y := Top;
            AOrigin.Y := Top;
          end;
        pavBottom:
          begin
            Y := Bottom - Height;
            AOrigin.Y := Bottom;
          end;
      end;
    end;
  end;

  procedure CalculateVerticalPosition;
  begin
    with AOwnerScreenBounds, Result do
    begin
      case AAlignHorz of
        pahLeft:
          begin
            X := Left;
            AOrigin.X := Left;
          end;
        pahRight:
          begin
            X := Right - Width;
            AOrigin.X := Right;
          end;
      end;
      case AAlignVert of
        pavTop:
          begin
            Y := Top - Height;
            AOrigin.Y := Top;
          end;
        pavBottom:
          begin
            Y := Bottom;
            AOrigin.Y := Bottom;
          end;
      end;
    end;
  end;

  procedure CheckPosition;
  var
    ADesktopWorkArea: TRect;

    procedure CheckCommonPosition;
    begin
      with ADesktopWorkArea, Result do
      begin
        if (FDirection = pdVertical) or (AAlignHorz = pahCenter) then
        begin
          if X + Width > Right then X := Right - Width;
          if X < Left then X := Left;
        end;
        if (FDirection = pdHorizontal) or (AAlignVert = pavCenter) then
        begin
          if Y + Height > Bottom then Y := Bottom - Height;
          if Y < Top then Y := Top;
        end;
      end;
    end;

    procedure CheckHorizontalPosition;

      function MoreSpaceOnLeft: Boolean;
      begin
        with ADesktopWorkArea do
          Result := AOwnerScreenBounds.Left - Left > Right - AOwnerScreenBounds.Right;
      end;

    begin
      with ADesktopWorkArea, Result do
        case AAlignHorz of
          pahLeft:
            if (X < Left) and not MoreSpaceOnLeft then
              AAlignHorz := pahRight;
          pahRight:
            if (X + Width > Right) and MoreSpaceOnLeft then
              AAlignHorz := pahLeft;
        end;
      CalculateHorizontalPosition;
    end;

    procedure CheckVerticalPosition;

      function MoreSpaceOnTop: Boolean;
      begin
        with ADesktopWorkArea do
          Result := AOwnerScreenBounds.Top - Top > Bottom - AOwnerScreenBounds.Bottom;
      end;

    begin
      with ADesktopWorkArea, Result do
        case AAlignVert of
          pavTop:
            if (Y < Top) and not MoreSpaceOnTop then
              AAlignVert := pavBottom;
          pavBottom:
            if (Y + Height > Bottom) and MoreSpaceOnTop then
              AAlignVert := pavTop;
        end;
      CalculateVerticalPosition;
    end;

  begin
    ADesktopWorkArea := GetDesktopWorkArea(AOrigin);
    if FDirection = pdHorizontal then
      CheckHorizontalPosition
    else
      CheckVerticalPosition;
    CheckCommonPosition;
  end;

begin
  AAlignHorz := FAlignHorz;
  AAlignVert := FAlignVert;
  CalculateOwnerScreenBounds;
  CalculateCommonPosition;
  if FDirection = pdHorizontal then
    CalculateHorizontalPosition
  else
    CalculateVerticalPosition;
  CheckPosition;
end;

procedure TcxPopupWindow.CalculateSize;
var
  AClientWidth, AClientHeight, I: Integer;
  ABounds: TRect;

  procedure CheckClientSize;
  begin
    with ABounds do
    begin
      if Right > AClientWidth then AClientWidth := Right;
      if Bottom > AClientHeight then AClientHeight := Bottom;
    end;
  end;

begin
  if not FAdjustable then Exit;

  AClientWidth := 0;
  AClientHeight := 0;
  for I := 0 to ControlCount - 1 do
  begin
    ABounds := Controls[I].BoundsRect;
    CheckClientSize;
    OffsetRect(ABounds, BorderWidths[bLeft], BorderWidths[bTop]);
    Controls[I].BoundsRect := ABounds;
  end;

  if AClientWidth <> 0 then
    Width := BorderWidths[bLeft] + AClientWidth + BorderWidths[bRight];
  if AClientHeight <> 0 then
    Height := BorderWidths[bTop] + AClientHeight + BorderWidths[bBottom];
end;

function TcxPopupWindow.GetBorderWidth(ABorder: TcxBorder): Integer;
begin
  Result := FBorderSpace + FrameWidths[ABorder];
end;

function TcxPopupWindow.GetFrameWidth(ABorder: TcxBorder): Integer;
begin
  case FBorderStyle of
    pbsUltraFlat:
      Result := 1;
    pbsFlat:
      Result := 1;
    pbs3D:
      Result := 2;
  else
    Result := 0;
  end;
end;

procedure TcxPopupWindow.InitPopup;
begin
  if FOwnerParent <> nil then
    Font := TControlAccess(FOwnerParent).Font;
end;

procedure TcxPopupWindow.RestoreControlsBounds;
var
  I: Integer;
  ABounds: TRect;
begin
  for I := 0 to ControlCount - 1 do
  begin
    ABounds := Controls[I].BoundsRect;
    OffsetRect(ABounds, -BorderWidths[bLeft], -BorderWidths[bTop]);
    Controls[I].BoundsRect := ABounds;
  end;
end;

procedure TcxPopupWindow.DrawFrame;
var
  R: TRect;

  procedure DrawUltraFlatBorder;
  begin
    Canvas.FrameRect(R, FrameColor);
  end;

  procedure DrawFlatBorder;
  begin
    Canvas.DrawEdge(R, False, False);
  end;

  procedure Draw3DBorder;
  begin
    Canvas.DrawEdge(R, False, True);
    InflateRect(R, -1, -1);
    Canvas.DrawEdge(R, False, False);
  end;

begin
  R := Bounds(0, 0, Width, Height);
  case FBorderStyle of
    pbsUltraFlat:
      DrawUltraFlatBorder;
    pbsFlat:
      DrawFlatBorder;
    pbs3D:
      Draw3DBorder;
  end;
end;

procedure TcxPopupWindow.CloseUp;
begin
{$IFNDEF VCL}
  FreeAndNil(FAppDeactivationTimer);
{$ENDIF}
  Hide;
end;

procedure TcxPopupWindow.Popup;

{$IFNDEF VCL}

  procedure StartAppDeactivationTimer;
  begin
    FAppDeactivationTimer := TTimer.Create(nil);
    with FAppDeactivationTimer do
    begin
      Interval := 50;
      OnTimer := AppDeactivationTimerHandler;
    end;
  end;

  procedure FocusFirstControl;
  var
    AControl: TWinControl;
  begin
    AControl := FindNextControl(nil, True, True, True);
    if AControl <> nil then AControl.SetFocus;
  end;

{$ENDIF}

begin
  InitPopup;
  CalculateSize;
  with CalculatePosition do
  begin
    Left := X;
    Top := Y;
  end;
{$IFNDEF VCL}
  StartAppDeactivationTimer;
{$ENDIF}
  Show;
{$IFNDEF VCL}
  FocusFirstControl;
{$ENDIF}
end;

initialization
{$IFDEF VCL}
  Screen.Cursors[crDragCopy] := LoadCursor(HInstance, 'CX_DRAGCOPYCURSOR');
  Screen.Cursors[crFullScroll] := LoadCursor(HInstance, 'CX_FULLSCROLLCURSOR');
  Screen.Cursors[crHorScroll] := LoadCursor(HInstance, 'CX_HORSCROLLCURSOR');
  Screen.Cursors[crVerScroll] := LoadCursor(HInstance, 'CX_VERSCROLLCURSOR');
  Screen.Cursors[crUpScroll] := LoadCursor(HInstance, 'CX_UPSCROLLCURSOR');
  Screen.Cursors[crRightScroll] := LoadCursor(HInstance, 'CX_RIGHTSCROLLCURSOR');
  Screen.Cursors[crDownScroll] := LoadCursor(HInstance, 'CX_DOWNSCROLLCURSOR');
  Screen.Cursors[crLeftScroll] := LoadCursor(HInstance, 'CX_LEFTSCROLLCURSOR');
  FUser32DLL := LoadLibrary('USER32');
{$ENDIF}
{$IFDEF DELPHI6}
  //StartClassGroup(TControl);
  GroupDescendentsWith(TcxControlChildComponent, TControl);
{$ENDIF}

finalization
  if FMouseTrackingTimerList <> nil then
  begin
    if FMouseTrackingTimerList.Count <> 0 then
      raise Exception.Create('MouseTrackingTimerList.Count <> 0');
    FreeAndNil(FMouseTrackingTimerList);
  end;
{$IFDEF VCL}
  if FUser32DLL > 32 then FreeLibrary(FUser32DLL);
{$ENDIF}

{$IFDEF VCL}
  {$IFDEF USETCXSCROLLBAR}
    FreeAndNil(FSettingsController);
  {$ENDIF}
{$ENDIF}

end.


