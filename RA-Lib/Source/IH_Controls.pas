
{
  public and published declarations ready to use with pas2rai2.exe :
  - constructor Create added to all classes;
  - property published without types completly redeclared
}

unit IH_Controls;

interface

type

  THackControl = class
    property Caption: TCaption read GetText write SetText;
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow;
    property DesktopFont: Boolean read FDesktopFont write SetDesktopFont default False;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crDrag;
    property DragMode: TDragMode read FDragMode write SetDragMode default dmManual;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ScalingFlags: TScalingFlags read FScalingFlags write FScalingFlags;
    property Text: TCaption read GetText write SetText;
    property WindowText: PChar read FText write FText;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
  end;

  THackWinControl = class
    property Ctl3D: Boolean read FCtl3D write SetCtl3D stored IsCtl3DStored;
    property DefWndProc: Pointer read FDefWndProc write FDefWndProc;
    property ImeMode: TImeMode read FImeMode write FImeMode default imDontCare;
    property ImeName: TImeName read FImeName write FImeName;
    property ParentCtl3D: Boolean read FParentCtl3D write SetParentCtl3D default True;
    property WindowHandle: HWnd read FHandle write FHandle;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

  THackCustomControl = class(TWinControl)
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;
  end;

implementation

end.
