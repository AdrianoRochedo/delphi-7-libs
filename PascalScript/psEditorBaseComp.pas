unit psEditorBaseComp;

{$DEFINE DEBUG}
{$IFNDEF RAEDITOR_NOEDITOR}
{$DEFINE RAEDITOR_EDITOR} {if not RAEDITOR_EDITOR then mode = Viewer}
{$ENDIF}
{$DEFINE RAEDITOR_DEFLAYOT} {set default keyboard layot}
{$IFNDEF RAEDITOR_NOUNDO}
{$DEFINE RAEDITOR_UNDO} {enable undo}
{$ENDIF}
{$IFNDEF RAEDITOR_NOCOMPLETION}
{$DEFINE RAEDITOR_COMPLETION} {enable code completion}
{$ENDIF}

{$IFNDEF RAEDITOR_EDITOR}
{$UNDEF RAEDITOR_DEFLAYOT}
{$UNDEF RAEDITOR_UNDO}
{$UNDEF RAEDITOR_COMPLETION}
{$ENDIF RAEDITOR_EDITOR}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, ClipBrd,
  psScrollBar;

const
  Max_X = 1024; {max symbols per row}
  Max_X_Scroll = 256;
  {max symbols per row for scrollbar - max кол-во символов в строке для прокрутки}
  GutterRightMargin = 2;

  WM_EDITCOMMAND = WM_USER + $101;

type

  TCellRect = record
    Width: integer;
    Height: integer;
  end;

  TLineAttr = record
    FC, BC: TColor;
    Style: TFontStyles;
  end;

  TCustomEditor = class;

  TLineAttrs = array[0..Max_X] of TLineAttr;
  TOnGetLineAttr = procedure(Sender: TObject; var Line: string; index: integer;
    var Attrs: TLineAttrs) of object;
  TOnChangeStatus = TNotifyEvent;

  TEditorStrings = class(TStringList)
  private
    FRAEditor: TCustomEditor;
    procedure StringsChanged(Sender: TObject);
    procedure SetInternal(index: integer; value: string);
    procedure ReLine;
    procedure SetLockText(Text: string);
  protected
    procedure SetTextStr(const Value: string); override;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    property Internal[index: integer]: string write SetInternal;
  end;

  TModifiedAction = (maInsert, maDelete);

  TBookMark = record
    X, Y: integer;
    Valid: boolean;
  end;
  TBookMarkNum = 0..9;
  TBookMarks = array[TBookMarkNum] of TBookMark;

  TEditorClient = class
  private
    FRAEditor: TCustomEditor;
    Top: integer;
    function Left: integer;
    function Height: integer;
    function Width: integer;
    function ClientWidth: integer;
    function ClientHeight: integer;
    function ClientRect: TRect;
    function BoundsRect: TRect;
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  TGutter = class
  private
    FRAEditor: TCustomEditor;
  public
    procedure Paint;
    procedure Invalidate;
  end;

  TOnPaintGutter = procedure(Sender: TObject; Canvas: TCanvas) of object;

  TEditCommand = word;
  TMacro = string; { uses as buffer }

  TEditKey = class
  public
    Key1, Key2: Word;
    Shift1, Shift2: TShiftState;
    Command: TEditCommand;
    constructor Create(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    constructor Create2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
  end;

  TKeyboard = class
  private
    List: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
    procedure Clear;
    function Command(const AKey: word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: word; const AShift1: TShiftState;
      const AKey2: word; const AShift2: TShiftState): TEditCommand;
    {$IFDEF RAEDITOR_DEFLAYOT}
    procedure SetDefLayot;
    {$ENDIF RAEDITOR_DEFLAYOT}
  end;

  EEditorError = class(Exception);

  {$IFDEF RAEDITOR_UNDO}
  TUndoBuffer = class;

  TUndo = class
  private
    FRAEditor: TCustomEditor;
    function UndoBuffer: TUndoBuffer;
  public
    constructor Create(ARAEditor: TCustomEditor);
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; abstract;
  end;

  TUndoBuffer = class(TList)
  private
    FRAEditor: TCustomEditor;
    FPtr: integer;
    InUndo: boolean;
    function LastUndo: TUndo;
    function IsNewGroup(AUndo: TUndo): boolean;
  public
    procedure Add(AUndo: TUndo);
    procedure Undo;
    procedure Redo;
    procedure Clear; {$IFDEF RA_D35H}override; {$ENDIF}
    procedure Delete;
  end;
  {$ENDIF RAEDITOR_UNDO}

  {$IFDEF RAEDITOR_COMPLETION}
  TCompletion = class;
  TOnCompletion = procedure(Sender: TObject; var Cancel: boolean) of object;
  {$ENDIF RAEDITOR_COMPLETION}

  TTabStop = (tsTabStop, tsAutoIndent);

  {*** TCustomEditor }

  TCustomEditor = class(TCustomControl)
  private
    { internal objects }
    FLines: TEditorStrings;
    scbHorz: TControlScrollBar95;
    scbVert: TControlScrollBar95;
    EditorClient: TEditorClient;
    FGutter: TGutter;
    FKeyboard: TKeyboard;
    FUpdateLock: integer;
    {$IFDEF RAEDITOR_UNDO}
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: boolean;
    {$ENDIF RAEDITOR_UNDO}
    {$IFDEF RAEDITOR_COMPLETION}
    FCompletion: TCompletion;
    {$ENDIF RAEDITOR_COMPLETION}

    { internal - Columns and rows attributes }
    FCols, FRows: integer;
    FLeftCol, FTopRow: integer;
    // FLeftColMax, FTopRowMax : integer;
    FLastVisibleCol, FLastVisibleRow: integer;
    FCaretX, FCaretY: integer;
    FVisibleColCount: integer;
    FVisibleRowCount: integer;

    { internal - other flags and attributes }
    FAllRepaint: boolean;
    FCellRect: TCellRect;
    {$IFDEF RAEDITOR_EDITOR}
    IgnoreKeyPress: boolean;
    {$ENDIF RAEDITOR_EDITOR}
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelected: boolean;
    FSelBlock: boolean; {режим выделения прямоугольных блоков}
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FUpdateSelBegY, FUpdateSelEndY: integer;
    FSelStartX, FSelStartY: integer;
    FclSelectBC, FclSelectFC: TColor;

    { mouse support }
    timerScroll: TTimer;
    MouseMoveY, MouseMoveXX, MouseMoveYY: integer;
    FDbl: boolean;

    { internal }
    FTabPos: array[0..Max_X] of boolean;
    FTabStops: string;

    { internal - primary for TIReader support }
    FEditBuffer: string;
    FPEditBuffer: PChar;
    FEditBufferSize: Integer;

    FCompound: Integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: integer;
    FRightMarginVisible: boolean;
    FRightMargin: integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: boolean;
    FSmartTab: Boolean;
    FBackSpaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    { FInclusive - Inclusive mode = последний выделенный символ на самом деле не выделенный}
    FInclusive: Boolean;


    { non-visual attributes - properties }
    FInsertMode: boolean;
    FReadOnly: boolean;
    FModified: boolean;
    FRecording: boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    {$IFDEF RAEDITOR_COMPLETION}
    FOnCompletionIdentifer: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    {$ENDIF RAEDITOR_COMPLETION}

    { internal message processing }
    {$IFNDEF RA_D4H}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ENDIF RA_D4H}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEditCommand(var Message: TMessage); message WM_EDITCOMMAND;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

    procedure UpdateEditorSize;
    {$IFDEF RAEDITOR_COMPLETION}
    procedure DoCompletionIdentifer(var Cancel: boolean);
    procedure DoCompletionTemplate(var Cancel: boolean);
    {$ENDIF RAEDITOR_COMPLETION}
    procedure ScrollTimer(Sender: TObject);

    procedure ReLine;
    function GetDefTabStop(const X: integer; const Next: Boolean): Integer;
    function GetTabStop(const X, Y: Integer; const What: TTabStop;
      const Next: Boolean): Integer;
    function GetBackStop(const X, Y: Integer): Integer;

    procedure TextAllChangedInternal(const Unselect: Boolean);

    { property }
    procedure SetGutterWidth(AWidth: integer);
    procedure SetGutterColor(AColor: TColor);
    function GetLines: TStrings;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetLines(ALines: TStrings);
    function GetSelStart: integer;
    procedure SetSelStart(const ASelStart: integer);
    procedure SetSelLength(const ASelLength: integer);
    function GetSelLength: integer;
    procedure SetMode(index: integer; Value: boolean);
    procedure SetCaretPosition(const index, Pos: integer);
    procedure SetCols(ACols: integer);
    procedure SetRows(ARows: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: boolean);
    procedure SetRightMargin(Value: integer);
    procedure SetRightMarginColor(Value: TColor);
  protected
    LineAttrs: TLineAttrs;
    procedure Resize; {$IFDEF RA_D4H}override; {$ELSE}dynamic; {$ENDIF RA_D4H}
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: Integer);
    procedure Scroll(const Vert: boolean; const ScrollPos: integer);
    procedure PaintLine(const Line: integer; ColBeg, ColEnd: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFDEF RAEDITOR_EDITOR}
    procedure KeyPress(var Key: Char); override;
    procedure InsertChar(const Key: Char);
    {$ENDIF RAEDITOR_EDITOR}
    procedure SetSel(const SelX, SelY: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DblClick; override;
    procedure DrawRightMargin;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure Mouse2Cell(const X, Y: integer; var CX, CY: integer);
    procedure Mouse2Caret(const X, Y: integer; var CX, CY: integer);
    procedure CaretCoord(const X, Y: integer; var CX, CY: integer);
    function PosFromMouse(const X, Y: integer): integer;
    procedure SetLockText(const Text: string);
    function ExpandTabs(const S: string): string;

    {$IFDEF RAEDITOR_UNDO}
    procedure CantUndo;
    {$ENDIF RAEDITOR_UNDO}
    procedure SetCaretInternal(X, Y: integer);
    procedure ValidateEditBuffer;

    {$IFDEF RAEDITOR_EDITOR}
    procedure ChangeBookMark(const BookMark: TBookMarkNum; const Valid:
      boolean);
    {$ENDIF RAEDITOR_EDITOR}
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure GetLineAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure GetAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure ChangeAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure GutterPaint(Canvas: TCanvas); dynamic;
    procedure BookmarkCnanged(BookMark: integer); dynamic;
    {$IFDEF RAEDITOR_COMPLETION}
    procedure CompletionIdentifer(var Cancel: boolean); dynamic;
    procedure CompletionTemplate(var Cancel: boolean); dynamic;
    {$ENDIF RAEDITOR_COMPLETION}
    { don't use method TextModified: see comment at method body }
    procedure TextModified(Pos: integer; Action: TModifiedAction; Text: string);
      dynamic;
    property Gutter: TGutter read FGutter;
  public
    BookMarks: TBookMarks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLeftTop(ALeftCol, ATopRow: integer);
    procedure ClipBoardCopy;
    procedure ClipBoardPaste;
    procedure ClipBoardCut;
    procedure DeleteSelected;
    function CalcCellRect(const X, Y: integer): TRect;
    procedure SetCaret(X, Y: integer);
    procedure CaretFromPos(const Pos: integer; var X, Y: integer);
    function PosFromCaret(const X, Y: integer): integer;
    procedure PaintCaret(const bShow: boolean);
    function GetTextLen: Integer;
    function GetSelText: string;
    procedure SetSelText(const AValue: string);
    function GetWordOnCaret: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: Integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);
    {$IFDEF RAEDITOR_EDITOR}
    procedure InsertText(const Text: string);
    procedure ReplaceWord(const NewString: string);
    procedure ReplaceWord2(const NewString: string);
    {$ENDIF}
    procedure BeginCompound;
    procedure EndCompound;

    procedure Print;

    function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint;
    property LeftCol: integer read FLeftCol;
    property TopRow: integer read FTopRow;
    property VisibleColCount: integer read FVisibleColCount;
    property VisibleRowCount: integer read FVisibleRowCount;
    property LastVisibleCol: integer read FLastVisibleCol;
    property LastVisibleRow: integer read FLastVisibleRow;
    property Cols: integer read FCols write SetCols;
    property Rows: integer read FRows write SetRows;
    property CaretX: integer index 0 read FCaretX write SetCaretPosition;
    property CaretY: integer index 1 read FCaretY write SetCaretPosition;
    property Modified: boolean read FModified write FModified;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property Keyboard: TKeyboard read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    {$IFDEF RAEDITOR_UNDO}
    property UndoBuffer: TUndoBuffer read FUndoBuffer;
    {$ENDIF RAEDITOR_UNDO}
    property Recording: boolean read FRecording;
  public { published in descendants }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Lines: TStrings read GetLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Cursor default crIBeam;
    property Color default clWindow;

    property GutterWidth: integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clBtnFace;
    property RightMarginVisible: boolean read FRightMarginVisible write SetRightMarginVisible default true;
    property RightMargin: integer read FRightMargin write SetRightMargin default 80;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor default clBtnFace;
    property InsertMode: boolean index 0 read FInsertMode write SetMode default true;
    property ReadOnly: boolean index 1 read FReadOnly write SetMode default false;
    property DoubleClickLine: boolean read FDoubleClickLine write FDoubleClickLine default false;
    {$IFDEF RAEDITOR_COMPLETION}
    property Completion: TCompletion read FCompletion write FCompletion;
    {$ENDIF RAEDITOR_COMPLETION}
    property TabStops: string read FTabStops write FTabStops;
    property SmartTab: Boolean read FSmartTab write FSmartTab default true;
    property BackSpaceUnindents: Boolean read FBackSpaceUnindents write FBackSpaceUnindents default true;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default true;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default false;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default false;
    property SelForeColor: TColor read FclSelectFC write FclSelectFC;
    property SelBackColor: TColor read FclSelectBC write FclSelectBC;

    property OnGetLineAttr: TOnGetLineAttr read FOnGetLineAttr write FOnGetLineAttr;
    property OnChangeStatus: TOnChangeStatus read FOnChangeStatus write FOnChangeStatus;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaintGutter: TOnPaintGutter read FOnPaintGutter write FOnPaintGutter;
    {$IFDEF RAEDITOR_COMPLETION}
    property OnCompletionIdentifer: TOnCompletion read FOnCompletionIdentifer write FOnCompletionIdentifer;
    property OnCompletionTemplate: TOnCompletion read FOnCompletionTemplate write FOnCompletionTemplate;
    property OnCompletionDrawItem: TDrawItemEvent read FOnCompletionDrawItem write FOnCompletionDrawItem;
    property OnCompletionMeasureItem: TMeasureItemEvent read FOnCompletionMeasureItem write FOnCompletionMeasureItem;
    {$ENDIF RAEDITOR_COMPLETION}
    {$IFDEF RA_D4H}
    property DockManager;
    {$ENDIF RA_D4H}
  end;

  TBaseEditor = class(TCustomEditor)
  published
    property BorderStyle;
    property Lines;
    property ScrollBars;
    property GutterWidth;
    property GutterColor;
    property RightMarginVisible;
    property RightMargin;
    property RightMarginColor;
    property InsertMode;
    property ReadOnly;
    property DoubleClickLine;
    {$IFDEF RAEDITOR_COMPLETION}
    property Completion;
    {$ENDIF RAEDITOR_COMPLETION}
    property TabStops;
    property SmartTab;
    property BackSpaceUnindents;
    property AutoIndent;
    property KeepTrailingBlanks;
    property CursorBeyondEOF;
    property SelForeColor;
    property SelBackColor;

    property OnGetLineAttr;
    property OnChangeStatus;
    property OnScroll;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange;
    property OnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnPaintGutter;
    {$IFDEF RAEDITOR_COMPLETION}
    property OnCompletionIdentifer;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    {$ENDIF RAEDITOR_COMPLETION}

    { TCustomControl }
    property Align;
    property Enabled;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    {$IFDEF RA_D4H}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF RA_D4H}
  end;

  {$IFDEF RAEDITOR_COMPLETION}
  TCompletionList = (cmIdentifers, cmTemplates);

  TCompletion = class(TPersistent)
  private
    FRAEditor: TCustomEditor;
    FPopupList: TListBox;
    FIdentifers: TStrings;
    FTemplates: TStrings;
    FItems: TStringList;
    FItemIndex: integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: integer;
    FTimer: TTimer;
    FEnabled: boolean;
    FVisible: boolean;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: char;
    FCRLF: string;
    FSeparator: string;
    function DoKeyDown(Key: Word; Shift: TShiftState): boolean;
    procedure DoKeyPress(Key: Char);
    procedure OnTimer(Sender: TObject);
    procedure FindSelItem(var Eq: boolean);
    procedure ReplaceWord(const NewString: string);

    procedure SetStrings(index: integer; AValue: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(AValue: integer);
    function GetInterval: cardinal;
    procedure SetInterval(AValue: cardinal);
    procedure MakeItems;
    function GetItems: TStrings;
  public
    constructor Create2(ARAEditor: TCustomEditor);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: boolean);
    procedure SelectItem;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Visible: boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
    property Items: TStringList read FItems;
  published
    property DropDownCount: integer read FDropDownCount write FDropDownCount
      default 6;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth
      default 300;
    property Enabled: boolean read FEnabled write FEnabled default false;
    property Identifers: TStrings index 0 read FIdentifers write SetStrings;
    property Templates: TStrings index 1 read FTemplates write SetStrings;
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property Interval: cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
    property CaretChar: char read FCaretChar write FCaretChar;
    property CRLF: string read FCRLF write FCRLF;
    property Separator: string read FSeparator write FSeparator;
  end;
  {$ENDIF RAEDITOR_COMPLETION}

const

 { Editor commands }
 { When add new commands, please add them into RAI2_RAEditor.pas unit also ! }

  ecCharFirst = $00;
  ecCharLast = $FF;
  ecCommandFirst = $100;
  ecUser = $8000; { use this for descendants }

  {Cursor}
  ecLeft = ecCommandFirst + 1;
  ecUp = ecLeft + 1;
  ecRight = ecLeft + 2;
  ecDown = ecLeft + 3;
  {Cursor with select}
  ecSelLeft = ecCommandFirst + 9;
  ecSelUp = ecSelLeft + 1;
  ecSelRight = ecSelLeft + 2;
  ecSelDown = ecSelLeft + 3;
  {Cursor по словам}
  ecPrevWord = ecSelDown + 1;
  ecNextWord = ecPrevWord + 1;
  ecSelPrevWord = ecPrevWord + 2;
  ecSelNextWord = ecPrevWord + 3;
  ecSelWord = ecPrevWord + 4;

  ecWindowTop = ecSelWord + 1;
  ecWindowBottom = ecWindowTop + 1;
  ecPrevPage = ecWindowTop + 2;
  ecNextPage = ecWindowTop + 3;
  ecSelPrevPage = ecWindowTop + 4;
  ecSelNextPage = ecWindowTop + 5;

  ecBeginLine = ecSelNextPage + 1;
  ecEndLine = ecBeginLine + 1;
  ecBeginDoc = ecBeginLine + 2;
  ecEndDoc = ecBeginLine + 3;
  ecSelBeginLine = ecBeginLine + 4;
  ecSelEndLine = ecBeginLine + 5;
  ecSelBeginDoc = ecBeginLine + 6;
  ecSelEndDoc = ecBeginLine + 7;
  ecSelAll = ecBeginLine + 8;

  ecScrollLineUp = ecSelAll + 1;
  ecScrollLineDown = ecScrollLineUp + 1;

  ecInsertPara = ecCommandFirst + 101;
  ecBackspace = ecInsertPara + 1;
  ecDelete = ecInsertPara + 2;
  ecChangeInsertMode = ecInsertPara + 3;
  ecTab = ecInsertPara + 4;
  ecBackTab = ecInsertPara + 5;
  ecIndent = ecInsertPara + 6;
  ecUnindent = ecInsertPara + 7;

  ecDeleteSelected = ecInsertPara + 10;
  ecClipboardCopy = ecInsertPara + 11;
  ecClipboardCut = ecClipboardCopy + 1;
  ecClipBoardPaste = ecClipboardCopy + 2;

  ecDeleteLine = ecClipBoardPaste + 1;
  ecDeleteWord = ecDeleteLine + 1;

  ecToUpperCase = ecDeleteLine + 2;
  ecToLowerCase = ecToUpperCase + 1;
  ecChangeCase = ecToUpperCase + 2;

  ecUndo = ecChangeCase + 1;
  ecRedo = ecUndo + 1;
  ecBeginCompound = ecUndo + 2; { not implemented }
  ecEndCompound = ecUndo + 3; { not implemented }

  ecBeginUpdate = ecUndo + 4;
  ecEndUpdate = ecUndo + 5;

  ecSetBookmark0 = ecEndUpdate + 1;
  ecSetBookmark1 = ecSetBookmark0 + 1;
  ecSetBookmark2 = ecSetBookmark0 + 2;
  ecSetBookmark3 = ecSetBookmark0 + 3;
  ecSetBookmark4 = ecSetBookmark0 + 4;
  ecSetBookmark5 = ecSetBookmark0 + 5;
  ecSetBookmark6 = ecSetBookmark0 + 6;
  ecSetBookmark7 = ecSetBookmark0 + 7;
  ecSetBookmark8 = ecSetBookmark0 + 8;
  ecSetBookmark9 = ecSetBookmark0 + 9;

  ecGotoBookmark0 = ecSetBookmark9 + 1;
  ecGotoBookmark1 = ecGotoBookmark0 + 1;
  ecGotoBookmark2 = ecGotoBookmark0 + 2;
  ecGotoBookmark3 = ecGotoBookmark0 + 3;
  ecGotoBookmark4 = ecGotoBookmark0 + 4;
  ecGotoBookmark5 = ecGotoBookmark0 + 5;
  ecGotoBookmark6 = ecGotoBookmark0 + 6;
  ecGotoBookmark7 = ecGotoBookmark0 + 7;
  ecGotoBookmark8 = ecGotoBookmark0 + 8;
  ecGotoBookmark9 = ecGotoBookmark0 + 9;

  ecCompletionIdentifers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifers + 1;

  ecRecordMacro = ecCompletionTemplates + 1;
  ecPlayMacro = ecRecordMacro + 1;
  ecBeginRecord = ecRecordMacro + 2;
  ecEndRecord = ecRecordMacro + 3;

  twoKeyCommand = High(word);

implementation
uses Consts,
     psConst,
     SysUtilsEx,
     Math,
     Printers;

{$IFDEF RAEDITOR_UNDO}

type

  TCaretUndo = class(TUndo)
  private
    FCaretX, FCaretY: integer;
  public
    constructor Create(ARAEditor: TCustomEditor; ACaretX, ACaretY: integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TInsertUndo = class(TCaretUndo)
  private
    FText: string;
  public
    constructor Create(ARAEditor: TCustomEditor; ACaretX, ACaretY: integer;
      AText: string);
    procedure Undo; override;
  end;

  TReLineUndo = class(TInsertUndo);

  TInsertTabUndo = class(TInsertUndo);

  TDeleteUndo = class(TInsertUndo)
  public
    procedure Undo; override;
  end;

  TDeleteTrailUndo = class(TDeleteUndo);

  TBackspaceUndo = class(TDeleteUndo)
  public
    procedure Undo; override;
  end;

  TReplaceUndo = class(TCaretUndo)
  private
    FBeg, FEnd: integer;
    FText, FNewText: string;
  public
    constructor Create(ARAEditor: TCustomEditor; ACaretX, ACaretY: integer;
      ABeg, AEnd: integer; AText, ANewText: string);
    procedure Undo; override;
  end;

  TDeleteSelectedUndo = class(TDeleteUndo)
  private
    FSelBlock: boolean; { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  public
    constructor Create(ARAEditor: TCustomEditor; ACaretX, ACaretY: integer;
      AText: string; ASelBlock: boolean; ASelBegX, ASelBegY, ASelEndX,
      ASelEndY: integer);
    procedure Undo; override;
  end;

  TSelectUndo = class(TCaretUndo)
  private
    FSelBlock: boolean; { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  public
    constructor Create(ARAEditor: TCustomEditor; ACaretX, ACaretY: integer;
      ASelBlock: boolean; ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
    procedure Undo; override;
  end;

  TUnselectUndo = class(TSelectUndo);

  TBeginCompoundUndo = class(TUndo)
  public
    procedure Undo; override;
  end;

  TEndCompoundUndo = class(TBeginCompoundUndo);

  {$ENDIF RAEDITOR_UNDO}

procedure Err;
begin
  MessageBeep(0);
end;

function FindNotBlankCharPos(const S: string): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 1 to Length(S) do
    if S[i] <> ' ' then Exit;
end;

function ANSIChangeCase(const S: string): string;
var
  i: Integer;
  Up: ANSIChar;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    Up := ANSIUpperCase(Result[i])[1];
    if Result[i] = Up then
      Result[i] := ANSILowerCase(Result[i])[1]
    else
      Result[i] := Up;
  end;
end;

constructor TEditorStrings.Create;
begin
  inherited Create;
  OnChange := StringsChanged;
end;

procedure TEditorStrings.SetTextStr(const Value: string);
begin
  inherited SetTextStr(FRAEditor.ExpandTabs(Value));
 {$IFDEF RAEDITOR_UNDO}
  if FRAEditor.FUpdateLock = 0 then FRAEditor.CantUndo;
 {$ENDIF RAEDITOR_UNDO}
  FRAEditor.TextAllChanged;
end;

procedure TEditorStrings.StringsChanged(Sender: TObject);
begin
  if FRAEditor.FUpdateLock = 0 then 
    FRAEditor.TextAllChanged;
end;

procedure TEditorStrings.SetLockText(Text: string);
begin
  inc(FRAEditor.FUpdateLock);
  try
    inherited SetTextStr(Text)
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TEditorStrings.SetInternal(index: integer; value: string);
begin
  inc(FRAEditor.FUpdateLock);
  try
    inherited Strings[index] := Value;
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

function TEditorStrings.Add(const S: string): Integer;
begin
  inc(FRAEditor.FUpdateLock);
  try
    Result := inherited Add(FRAEditor.ExpandTabs(S));
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TEditorStrings.Insert(Index: Integer; const S: string);
begin
  inc(FRAEditor.FUpdateLock);
  try
    inherited Insert(Index, FRAEditor.ExpandTabs(S));
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TEditorStrings.Put(Index: Integer; const S: string);
{$IFDEF RAEDITOR_UNDO}
var
  L: integer;
  {$ENDIF RAEDITOR_UNDO}
begin
  if FRAEditor.FKeepTrailingBlanks then
    inherited Put(Index, S)
  else
  begin
    {$IFDEF RAEDITOR_UNDO}
    L := Length(S) - Length(TrimRight(S));
    if L > 0 then
      TDeleteTrailUndo.Create(FRAEditor, Length(S), Index, Spaces(L));
    {$ENDIF RAEDITOR_UNDO}
    inherited Put(Index, TrimRight(S));
  end;
end;

procedure TEditorStrings.ReLine;
var
  L: Integer;
begin
  inc(FRAEditor.FUpdateLock);
  try
    {$IFDEF RAEDITOR_UNDO}
    if Count = 0 then
      L := FRAEditor.FCaretX
    else
      L := Length(Strings[Count - 1]);
    while FRAEditor.FCaretY > Count - 1 do
    begin
      TReLineUndo.Create(FRAEditor, L, FRAEditor.FCaretY, #13#10);
      L := 0;
      Add('');
    end;
    {$ENDIF RAEDITOR_UNDO}
    if FRAEditor.FCaretX > Length(Strings[FRAEditor.FCaretY]) then
    begin
      L := FRAEditor.FCaretX - Length(Strings[FRAEditor.FCaretY]);
      {$IFDEF RAEDITOR_UNDO}
      TReLineUndo.Create(FRAEditor, Length(Strings[FRAEditor.FCaretY]),
        FRAEditor.FCaretY, Spaces(L));
      {$ENDIF RAEDITOR_UNDO}
      inherited Put(FRAEditor.FCaretY, Strings[FRAEditor.FCaretY] + Spaces(L));
    end;
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end; { ReLine }

function TEditorClient.GetCanvas: TCanvas;
begin
  Result := FRAEditor.Canvas;
end;

function TEditorClient.Left: integer;
begin
  Result := FRAEditor.GutterWidth + 2;
end;

function TEditorClient.Height: integer;
begin
  Result := FRAEditor.ClientHeight;
end;

function TEditorClient.Width: integer;
begin
  Result := Math.Max(FRAEditor.ClientWidth - Left, 0);
end;

function TEditorClient.ClientWidth: integer;
begin
  Result := Width;
end;

function TEditorClient.ClientHeight: integer;
begin
  Result := Height;
end;

function TEditorClient.ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TEditorClient.BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

procedure TGutter.Invalidate;
{var
  R : TRect;}
begin
  //  Owner.Invalidate;
  //  R := Bounds(0, 0, FRAEditor.GutterWidth, FRAEditor.Height);
  //  InvalidateRect(FRAEditor.Handle, @R, false);
  Paint;
end;

procedure TGutter.Paint;
begin
  with FRAEditor, Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FGutterColor;
    FillRect(Bounds(0, EditorClient.Top, GutterWidth, EditorClient.Height));
    Pen.Width := 1;
    Pen.Color := Color;
    MoveTo(GutterWidth - 2, EditorClient.Top);
    LineTo(GutterWidth - 2, EditorClient.Top + EditorClient.Height);
    Pen.Width := 2;
    MoveTo(GutterWidth + 1, EditorClient.Top);
    LineTo(GutterWidth + 1, EditorClient.Top + EditorClient.Height);
    Pen.Width := 1;
    Pen.Color := clGray;
    MoveTo(GutterWidth - 1, EditorClient.Top);
    LineTo(GutterWidth - 1, EditorClient.Top + EditorClient.Height);
  end;
  with FRAEditor do
    GutterPaint(Canvas);
end;



{*********************** TCustomEditor ***********************}

constructor TCustomEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks,
  csReplicatable];
  FInsertMode := true;
  FLines := TEditorStrings.Create;
  FLines.FRAEditor := self;
  FKeyboard := TKeyboard.Create;
  FRows := 1;
  FCols := 1;
  {$IFDEF RAEDITOR_UNDO}
  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FRAEditor := Self;
  FGroupUndo := true;
  {$ENDIF RAEDITOR_UNDO}

  { Font.Name := 'Courier New';
   Font.Size := 10; }
  FRightMarginVisible := true;
  FRightMargin := 80;
  FBorderStyle := bsSingle;
  Ctl3d := true;
  Height := 40;
  Width := 150;
  ParentColor := false;
  Cursor := crIBeam;
  TabStop := true;
  FTabStops := '3 5';
  FSmartTab := true;
  FBackSpaceUnindents := true;
  FAutoIndent := true;
  FKeepTrailingBlanks := false;
  FCursorBeyondEOF := false;

  FScrollBars := ssBoth;
  scbHorz := TControlScrollBar95.Create;
  scbVert := TControlScrollBar95.Create;
  scbVert.Kind := sbVertical;
  scbHorz.OnScroll := ScrollBarScroll;
  scbVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FclSelectBC := clHighLight;
  FclSelectFC := clHighLightText;
  FRightMarginColor := clSilver;

  EditorClient := TEditorClient.Create;
  EditorClient.FRAEditor := Self;
  FGutter := TGutter.Create;
  FGutter.FRAEditor := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelected := false;
  FCaretX := 0;
  FCaretY := 0;

  timerScroll := TTimer.Create(Self);
  timerScroll.Enabled := false;
  timerScroll.Interval := 100;
  timerScroll.OnTimer := ScrollTimer;

  {$IFDEF RAEDITOR_EDITOR}

  {$IFDEF RAEDITOR_DEFLAYOT}
  FKeyboard.SetDefLayot;
  {$ENDIF RAEDITOR_DEFLAYOT}

  {$IFDEF RAEDITOR_COMPLETION}
  FCompletion := TCompletion.Create2(Self);
  {$ENDIF RAEDITOR_COMPLETION}

  {$ENDIF RAEDITOR_EDITOR}
end;

destructor TCustomEditor.Destroy;
begin
  FLines.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  {$IFDEF RAEDITOR_EDITOR}
  {$IFDEF RAEDITOR_UNDO}
  FUndoBuffer.Free;
  {$ENDIF RAEDITOR_UNDO}
  {$IFDEF RAEDITOR_COMPLETION}
  FCompletion.Free;
  {$ENDIF RAEDITOR_COMPLETION}
  {$ENDIF RAEDITOR_EDITOR}
  FGutter.Free;
  inherited Destroy;
end;

procedure TCustomEditor.Loaded;
begin
  inherited Loaded;
  UpdateEditorSize;
  {  Rows := FLines.Count;
    Cols := Max_X; }
end;

{************** Управление отрисовкой ***************}

procedure TCustomEditor.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of cardinal = (0, WS_BORDER);
const
  ScrollStyles: array[TScrollStyle] of cardinal = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or ScrollStyles[FScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{$IFNDEF RA_D4H}

procedure TCustomEditor.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csLoading in ComponentState) then Resize;
end;
{$ENDIF RA_D4H}

procedure TCustomEditor.Resize;
begin
  UpdateEditorSize;
end;

procedure TCustomEditor.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then
    scbHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then
    scbVert.Handle := Handle;
  FAllRepaint := true;
end;

procedure TCustomEditor.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateEditorSize;
end;

procedure TCustomEditor.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  {$IFDEF RAEDITOR_NOOPTIMIZE}
  inherited;
  Message.Result := 1;
  {$ELSE}
  Message.Result := 0;
  {$ENDIF}
end;

procedure TCustomEditor.PaintSelection;
var
  iR: integer;
begin
  for iR := FUpdateSelBegY to FUpdateSelEndY do
    PaintLine(iR, -1, -1);
end;

procedure TCustomEditor.SetUnSelected;
begin
  if FSelected then
  begin
    FSelected := false;
    {$IFDEF RAEDITOR_UNDO}
    TUnselectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    {$ENDIF RAEDITOR_UNDO}
    PaintSelection;
  end;
end;

procedure TCustomEditor.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X < GutterWidth) and (Cursor = crIBeam) then
  begin
    Message.Result := 1;
    Windows.SetCursor(Screen.Cursors[crArrow])
  end
  else
    inherited;
end;
{############## Управление отрисовкой ###############}

{************** Отрисовка ***************}

{
function IsRectEmpty(R: TRect): boolean;
begin
  Result := (R.Top = R.Bottom) and (R.Left = R.Right);
end;
}

function TCustomEditor.CalcCellRect(const X, Y: integer): TRect;
begin
  Result := Bounds(
    EditorClient.Left + X * FCellRect.Width + 1,
    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TCustomEditor.Paint;
var
  iR: integer;
  ECR: TRect;
  BX, EX, BY, EY: integer;
begin
  if FUpdateLock > 0 then exit;
  {$IFDEF RAEDITOR_NOOPTIMIZE}
  FAllRepaint := true;
  {$ENDIF}
  {оптимизировано - отрисовывается только необходимая часть}
  PaintCaret(false);

  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then ECR := EditorClient.BoundsRect;
  BX := ECR.Left div FCellRect.Width - 1;
  EX := ECR.Right div FCellRect.Width + 1;
  BY := ECR.Top div FCellRect.Height;
  EY := ECR.Bottom div FCellRect.Height + 1;
  for iR := BY to EY do
    PaintLine(FTopRow + iR, FLeftCol + BX, FLeftCol + EX);

  PaintCaret(true);
  FGutter.Paint;
  FAllRepaint := false;
end;

procedure TCustomEditor.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TCustomEditor.EndUpdate;
begin
  if FUpdateLock = 0 then Exit; { Error ? }
  dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    FAllRepaint := True;
    UpdateEditorSize;
    StatusChanged;
    Invalidate;
  end;
end;

procedure TCustomEditor.UpdateEditorSize;
const
  BiggestSymbol = 'W';
begin
  if (csLoading in ComponentState) then exit;
  EditorClient.Canvas.Font := Font;
  FCellRect.Width := EditorClient.Canvas.TextWidth(BiggestSymbol);
  FCellRect.Height := EditorClient.Canvas.TextHeight(BiggestSymbol) + 1;

  FVisibleColCount := Trunc(EditorClient.ClientWidth / FCellRect.Width);
  FVisibleRowCount := Trunc(EditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  Rows := FLines.Count;
  Cols := Max_X_Scroll;
  scbHorz.Page := FVisibleColCount;
  scbVert.Page := FVisibleRowCount;
  scbHorz.LargeChange := Math.Max(FVisibleColCount, 1);
  scbVert.LargeChange := Math.Max(FVisibleRowCount, 1);
  scbVert.Max := Math.Max(1, FRows - 1 + FVisibleRowCount -1);
  FGutter.Invalidate;
end;

procedure TCustomEditor.PaintLine(const Line: integer; ColBeg, ColEnd:
  integer);
var
  Ch: string;
  R: TRect;
  i, iC, jC, SL, MX: integer;
  S: string;
  LA: TLineAttr;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) then exit;
  // Debug('PaintLine '+IntToStr(Line));
  if ColBeg < FLeftCol then
    ColBeg := FLeftCol;
  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount) then
    ColEnd := FLeftCol + FVisibleColCount;
  ColEnd := Math.Min(ColEnd, Max_X - 1);
  i := ColBeg;
  if (Line > -1) and (Line < FLines.Count) {and (Length(FLines[Line]) > 0)} then
    with EditorClient do
    begin
      S := FLines[Line];
      GetLineAttr(Line, ColBeg, ColEnd);

      {левая кромка}
      EditorClient.Canvas.Brush.Color := LineAttrs[1].BC;
      EditorClient.Canvas.FillRect(Bounds(EditorClient.Left, (Line - FTopRow) *
        FCellRect.Height, 1, FCellRect.Height));

      {с оптимизацией - вывод по словам(с одинаковыми атрибутами)}
      SL := Length(S);
     { if SL > ColEnd then
        MX := ColEnd
      else
        MX := SL; }
      MX := ColEnd;

      i := ColBeg;
      while i < MX do
        with Canvas do
        begin
          iC := i + 1;
          LA := LineAttrs[iC];
          jC := iC + 1;
          if iC <= SL then
            Ch := S[iC]
          else
            Ch := ' ';
          while (jC <= MX + 1) and
            CompareMem(@LA, @LineAttrs[jC], sizeof(LineAttrs[1])) do
          begin
            if jC <= SL then
              Ch := Ch + S[jC]
            else
              Ch := Ch + ' ';
            inc(jC);
          end;
          Brush.Color := LA.BC;
          Font.Color := LA.FC;
          Font.Style := LA.Style;
          R := CalcCellRect(i - FLeftCol, Line - FTopRow);
          TextOut(R.Left, R.Top, Ch);

          {нижняя кромка}
          FillRect(Bounds(R.Left, R.Bottom - 1, FCellRect.Width * Length(Ch),
            2));

          i := jC - 1;
        end;
    end
  else
  begin
    EditorClient.Canvas.Brush.Color := Color;
    EditorClient.Canvas.FillRect(Bounds(EditorClient.Left, (Line - FTopRow) *
      FCellRect.Height, 1, FCellRect.Height));
  end;
  {дорисовать правую часть}
  R := Bounds(CalcCellRect(i - FLeftCol, Line - FTopRow).Left,
    (Line - FTopRow) * FCellRect.Height,
    (FLeftCol + FVisibleColCount - i + 2) * FCellRect.Width,
    FCellRect.Height);
  {если строка выделена то выделить всю правую часть}
  if FSelected and not FSelBlock and (Line >= FSelBegY) and (Line < FSelEndY)
    then
    EditorClient.Canvas.Brush.Color := FclSelectBC
  else
    EditorClient.Canvas.Brush.Color := Color;
  EditorClient.Canvas.FillRect(R);
  DrawRightMargin;
end;

procedure TCustomEditor.GetLineAttr(Line, ColBeg, ColEnd: integer);

  procedure ChangeSelectedAttr;

    procedure DoChange(const iBeg, iEnd: integer);
    var
      i: integer;
    begin
      for i := iBeg to iEnd do
      begin
        LineAttrs[i + 1].FC := FclSelectFC;
        LineAttrs[i + 1].BC := FclSelectBC;
      end;
    end;

  begin
    if (Line = FSelBegY) and (Line = FSelEndY) then
      DoChange(FSelBegX, FSelEndX - 1 + Integer(FInclusive))
    else
    begin
      if Line = FSelBegY then DoChange(FSelBegX, FSelBegX + FVisibleColCount);
      if (Line > FSelBegY) and (Line < FSelEndY) then DoChange(ColBeg, ColEnd);
      if Line = FSelEndY then
        DoChange(ColBeg, FSelEndX - 1 + Integer(FInclusive));
    end;
  end;
var
  i: integer;
  S: string;
begin
	LineAttrs[ColBeg].FC := Font.Color;
	LineAttrs[ColBeg].Style := Font.Style;
	LineAttrs[ColBeg].BC := Color;
	for i := ColBeg to ColEnd + 1 do
		Move(LineAttrs[ColBeg], LineAttrs[i], sizeof(LineAttrs[1]));
	S := FLines[Line];
	GetAttr(Line, ColBeg, ColEnd);
	if Assigned(FOnGetLineAttr) then FOnGetLineAttr(Self, S, Line, LineAttrs);
	if FSelected then ChangeSelectedAttr; {изменяем атрибуты выделенного блока}
	ChangeAttr(Line, ColBeg, ColEnd);
end;

procedure TCustomEditor.GetAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TCustomEditor.ChangeAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TCustomEditor.DrawRightMargin;
var
  F: integer;
begin
  if FRightMarginVisible and (FRightMargin > FLeftCol)
    and (FRightMargin < FLastVisibleCol + 3) then
    with EditorClient.Canvas do
    begin
      {рисуем RightMargin Line}
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRightMargin - FLeftCol, 0).Left;
      MoveTo(F, EditorClient.Top);
      LineTo(F, EditorClient.Top + EditorClient.Height);
    end;
end;

{************************** Scroll **************************}

procedure TCustomEditor.WMHScroll(var Message: TWMHScroll);
begin
  scbHorz.DoScroll(Message);
end;

procedure TCustomEditor.WMVScroll(var Message: TWMVScroll);
begin
  scbVert.DoScroll(Message);
end;

procedure TCustomEditor.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, {scPosition,} scTrack {, scEndScroll}:
      begin
        if Sender = scbVert then
          Scroll(true, ScrollPos)
        else if Sender = scbHorz then
          Scroll(false, ScrollPos);
      end;
  end;
  //  Tracer.Writeln(IntToStr((Sender as TRAControlScrollBar95).Position));
end;

procedure TCustomEditor.Scroll(const Vert: boolean; const ScrollPos: integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: integer;
  //  OldFLeftCol: integer;
begin
  //  BeginTick;
  if FUpdateLock = 0 then
  begin
    PaintCaret(false);
    if Vert then
    begin {Vertical Scroll}
      {оптимизировано}
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      if Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < EditorClient.Height
        then
      begin
        R := EditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * (FVisibleRowCount + 1); {??}
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle, // handle of device context
          0, // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height, // vertical scroll units
          R, // address of structure for scrolling rectangle
          RClip, // address of structure for clipping rectangle
          0, // handle of scrolling region
          @RUpdate // address of structure for update rectangle
          );
        InvalidateRect(Handle, @RUpdate, False);
      end
      else
        Invalidate;
      Update;
    end
    else {Horizontal Scroll}
    begin
      {неоптимизировано}
      FLeftCol := ScrollPos;
      (*  OldFLeftCol := FLeftCol;
        FLeftCol := ScrollPos;
        if Abs((OldFLeftCol - ScrollPos) * FCellRect.Width) < EditorClient.Width then
        begin
          R := EditorClient.ClientRect;
          R.Right := R.Left + CellRect.Width * (FVisibleColCount + 1); {??}
          RClip := R;
          ScrollDC(
            EditorClient.Canvas.Handle, // handle of device context
            (OldFLeftCol - ScrollPos) * FCellRect.Width, // horizontal scroll units
            0, // vertical scroll units
            R, // address of structure for scrolling rectangle
            RClip, // address of structure for clipping rectangle
            0, // handle of scrolling region
            @RUpdate // address of structure for update rectangle
            );
          InvalidateRect(Handle, @RUpdate, False);
        end
        else
          Invalidate;
        Update;  *)
      Invalidate;
    end;
  end
  else { FUpdateLock > 0 }
  begin
    if Vert then
      FTopRow := ScrollPos
    else
      FLeftCol := ScrollPos;
  end;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  if FUpdateLock = 0 then
  begin
    DrawRightMargin;
    PaintCaret(true);
  end;
  if Assigned(FOnScroll) then FOnScroll(Self);
  //  EndTick;
end;
{####################### Scroll #########################}
{####################### Painting - Отрисовка #######################}

{************** Caret ***************}

procedure TCustomEditor.PaintCaret(const bShow: boolean);
var
  R: TRect;
begin
  // Debug('PaintCaret: ' + IntToStr(integer(bShow)));
  if not bShow then
    HideCaret(Handle)
  else if Focused then
  begin
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow);
    SetCaretPos(R.Left - 1, R.Top + 1);
    ShowCaret(Handle)
  end
end;

procedure TCustomEditor.SetCaretInternal(X, Y: integer);
var
  R: TRect;
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  //прокручивать изображение
  if not FCursorBeyondEOF then
    Y := Math.Min(Y, FLines.Count - 1);
  Y := Math.Max(Y, 0);
  X := Math.Min(X, Max_X);
  X := Math.Max(X, 0);
  if Y < FTopRow then
    SetLeftTop(FLeftCol, Y)
  else if Y > Math.Max(FLastVisibleRow, 0) then
    SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);
  if X < 0 then X := 0;
  if X < FLeftCol then
    SetLeftTop(X, FTopRow)
  else if X > FLastVisibleCol then
    SetLeftTop(X - FVisibleColCount + 1, FTopRow);

  R := CalcCellRect(X - FLeftCol, Y - FTopRow);
  SetCaretPos(R.Left - 1, R.Top + 1);
  if Assigned(FOnChangeStatus) and ((FCaretX <> X) or (FCaretY <> Y)) then
  begin
    FCaretX := X;
    FCaretY := Y;
    FOnChangeStatus(Self);
  end;
  FCaretX := X;
  FCaretY := Y;
end;

procedure TCustomEditor.SetCaret(X, Y: integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  {$IFDEF RAEDITOR_UNDO}
  TCaretUndo.Create(Self, FCaretX, FCaretY);
  {$ENDIF RAEDITOR_UNDO}
  SetCaretInternal(X, Y);
  if FUpdateLock = 0 then StatusChanged;
end;

procedure TCustomEditor.SetCaretPosition(const index, Pos: integer);
begin
  if index = 0 then
    SetCaret(Pos, FCaretY)
  else
    SetCaret(FCaretX, Pos)
end;

procedure TCustomEditor.WMSetFocus(var Message: TWMSetFocus);
begin
  CreateCaret(Handle, 0, 2, CellRect.Height - 2);
  PaintCaret(true);
end;

procedure TCustomEditor.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  {$ENDIF RAEDITOR_COMPLETION}
  DestroyCaret;
end;

{############### Caret ###############}

{************** Keyboard ***************}

procedure TCustomEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TCustomEditor.KeyDown(var Key: Word; Shift: TShiftState);
{$IFDEF RAEDITOR_EDITOR}
var
  Com: word;
  {$ENDIF RAEDITOR_EDITOR}
begin
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then
    if FCompletion.DoKeyDown(Key, Shift) then
      exit
    else
  else
    FCompletion.FTimer.Enabled := false;
  {$ENDIF RAEDITOR_COMPLETION}
  {$IFDEF RAEDITOR_EDITOR}
  if WaitSecondKey then
  begin
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := false;
    IgnoreKeyPress := true;
  end
  else
  begin
    inherited KeyDown(Key, Shift);
    Key1 := Key;
    Shift1 := Shift;
    Com := FKeyboard.Command(Key, Shift);
    if Com = twoKeyCommand then
    begin
      IgnoreKeyPress := true;
      WaitSecondKey := true;
    end
    else
      IgnoreKeyPress := Com > 0;
  end;
  if (Com > 0) and (Com <> twoKeyCommand) then
  begin
    Command(Com);
    Key := 0;
  end;
  {$IFDEF RAEDITOR_COMPLETION}
  if (Com = ecBackSpace) then
    FCompletion.DoKeyPress(#8);
  {$ENDIF RAEDITOR_COMPLETION}
  {$ENDIF RAEDITOR_EDITOR}
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TCustomEditor.ReLine;
begin
  FLines.ReLine;
end; { ReLine }

procedure TCustomEditor.KeyPress(var Key: Char);
begin
  if IgnoreKeyPress then
  begin
    IgnoreKeyPress := false;
    exit
  end;
  if FReadOnly then exit;
  PaintCaret(false);
  inherited KeyPress(Key);

  Command(ord(Key));

  PaintCaret(true);
end;

procedure TCustomEditor.InsertChar(const Key: Char);
var
  S: string;
begin
  ReLine;
  case Key of
    #32..#255:
      begin
        {$IFDEF RAEDITOR_COMPLETION}
        if not HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF RAEDITOR_COMPLETION}
        begin
          DeleteSelected;
          S := FLines[FCaretY];
          if FInsertMode then
          begin
            {$IFDEF RAEDITOR_UNDO}
            TInsertUndo.Create(Self, FCaretX, FCaretY, Key);
            {$ENDIF RAEDITOR_UNDO}
            Insert(Key, S, FCaretX + 1);
          end
          else
          begin
            {$IFDEF RAEDITOR_UNDO}
            CantUndo;
            {$ENDIF RAEDITOR_UNDO}
            if FCaretX + 1 <= Length(S) then
              S[FCaretX + 1] := Key
            else
              S := S + Key
          end;
          FLines.Internal[FCaretY] := S;
          SetCaretInternal(FCaretX + 1, FCaretY);
          TextModified(SelStart, maInsert, Key);
          PaintLine(FCaretY, -1, -1);
          Changed;
        end;
        {$IFDEF RAEDITOR_COMPLETION}
        if HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF RAEDITOR_COMPLETION}
      end;
  end;
end;

{$ENDIF RAEDITOR_EDITOR}

{$IFDEF RAEDITOR_EDITOR}
type
  EComplete = class(EAbort);

procedure TCustomEditor.Command(ACommand: TEditCommand);
var
  X, Y: integer;
  {$IFDEF RAEDITOR_UNDO}
  CaretUndo: boolean;
  {$ENDIF RAEDITOR_UNDO}

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    {$IFDEF RAEDITOR_COMPLETION}
    CaretUndo := false;
    {$ENDIF RAEDITOR_COMPLETION}
  end;

  function Com(const Args: array of TEditCommand): boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to High(Args) do
      if Args[i] = ACommand then exit;
    Result := false;
  end;

  procedure SetSel1(X, Y: integer);
  begin
    SetSel(X, Y);
    {$IFDEF RAEDITOR_UNDO}
    CaretUndo := False;
    {$ENDIF RAEDITOR_UNDO}
  end;

  procedure SetSelText1(S: string);
  begin
    SelText := S;
   {$IFDEF RAEDITOR_UNDO}
    CaretUndo := False;
   {$ENDIF RAEDITOR_UNDO}
  end;

  procedure Complete;
  begin
    raise EComplete.Create('');
  end;

var
  F: integer;
  S, S2: string;
  B: boolean;
  iBeg, iEnd: integer;
begin
  X := FCaretX;
  Y := FCaretY;
  {$IFDEF RAEDITOR_UNDO}
  CaretUndo := true;
  {$ENDIF RAEDITOR_UNDO}
  PaintCaret(false);
  // inc(FUpdateLock);
 { macro recording }
  if FRecording and not Com([ecRecordMacro, ecBeginCompound]) and
     (FCompound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));
  try
    try
      case ACommand of

        { caret movements }

        ecLeft, ecRight, ecSelLeft, ecSelRight:
          begin
            if Com([ecSelLeft, ecSelRight]) and not FSelected then
              SetSel1(X, Y);
            if Com([ecLeft, ecSelLeft]) then
              dec(X)
            else
              inc(X);
            if Com([ecSelLeft, ecSelRight]) then
              SetSel1(X, Y)
            else
              SetUnSelected;
          end;
        ecUp, ecDown, ecSelUp, ecSelDown:
          if Com([ecUp, ecSelUp]) or (Y < FRows - 1) or FCursorBeyondEOF then
          begin
            if Com([ecSelUp, ecSelDown]) and not FSelected then SetSel1(X, Y);
            if Com([ecUp, ecSelUp]) then
              dec(Y)
            else
              inc(Y);
            if Com([ecSelUp, ecSelDown]) then
              SetSel1(X, Y)
            else
              SetUnSelected;
          end;
        ecPrevWord, ecSelPrevWord:
          begin
            if (ACommand = ecSelPrevWord) and not FSelected then
              SetSel1(FCaretX, FCaretY);
            S := FLines[Y];
            B := false;
            if FCaretX > Length(s) then
            begin
              X:=Length(s);
              SetSel1(X, Y);
            end
            else
            begin
              for F := X - 1 downto 0 do
                if B then
                  if (S[F + 1] in Separators) then
                  begin
                    X := F + 1;
                    break;
                  end
                  else
                else if not (S[F + 1] in Separators) then
                  B := true;
              if X = FCaretX then
                X := 0;
              if ACommand = ecSelPrevWord then
                SetSel1(X, Y)
              else
                SetUnselected;
            end;    
          end;
        ecNextWord, ecSelNextWord:
          begin
            if (ACommand = ecSelNextWord) and not FSelected then
              SetSel1(FCaretX, FCaretY);
            S := FLines[Y];
            B := false;
            if FCaretX >= Length(S) then
            begin
              Y := FCaretY + 1;
              X:=0;
              SetSel1(X, Y);
            end
            else
            begin
              for F := X to Length(S) - 1 do
                if B then
                  if not (S[F + 1] in Separators) then
                  begin
                    X := F;
                    break;
                  end
                  else
                else if (S[F + 1] in Separators) then
                  B := true;
              if X = FCaretX then
                X := Length(S);
              if ACommand = ecSelNextWord then
                SetSel1(X, Y)
              else
                SetUnselected;
            end;    
          end;
        ecScrollLineUp, ecScrollLineDown:
          begin
            if not ((ACommand = ecScrollLineDown) and
              (Y >= FLines.Count - 1) and (Y = FTopRow)) then
            begin
              if ACommand = ecScrollLineUp then
                F := -1
              else
                F := 1;
              scbVert.Position := scbVert.Position + F;
              Scroll(true, scbVert.Position);
            end;
            if Y < FTopRow then
              Y := FTopRow
            else if Y > FLastVisibleRow then
              Y := FLastVisibleRow;
          end;
        ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
          ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
          begin
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
              and not FSelected then
              SetSel1(FCaretX, Y);
            if Com([ecBeginLine, ecSelBeginLine]) then
              X := 0
            else if Com([ecBeginDoc, ecSelBeginDoc]) then
            begin
              X := 0;
              Y := 0;
              SetLeftTop(0, 0);
            end
            else if Com([ecEndLine, ecSelEndLine]) then
              if Y < FLines.Count then
                X := Length(FLines[Y])
               else
                X := 0
            else if Com([ecEndDoc, ecSelEndDoc]) then
            begin
              Y := FLines.Count - 1;
              X := Length(FLines[Y]);
              SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount div 2);
            end;
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
              then
              SetSel1(X, Y)
            else
              SetUnSelected;
          end;
        ecPrevPage:
          begin
            scbVert.Position := scbVert.Position - scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y - FVisibleRowCount;
            SetUnSelected;
          end;
        ecNextPage:
          begin
            scbVert.Position := scbVert.Position + scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y + FVisibleRowCount;
            SetUnSelected;
          end;
        ecSelPrevPage:
          begin
            BeginUpdate;
            SetSel1(X, Y);
            scbVert.Position := scbVert.Position - scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y - FVisibleRowCount;
            SetSel1(X, Y);
            EndUpdate;
          end;
        ecSelNextPage:
          begin
            BeginUpdate;
            SetSel1(X, Y);
            scbVert.Position := scbVert.Position + scbVert.LargeChange;
            Scroll(true, scbVert.Position);
            Y := Y + FVisibleRowCount;
            if Y <= FLines.Count - 1 then
              SetSel1(X, Y)
            else
              SetSel1(X, FLines.Count - 1);
            EndUpdate;
          end;
        ecSelWord:
          if not FSelected and (GetWordOnPosEx(FLines[Y] + ' ', X + 1, iBeg,
            iEnd) <> '') then
          begin
            SetSel1(iBeg - 1, Y);
            SetSel1(iEnd - 1, Y);
            X := iEnd - 1;
          end;

        ecWindowTop:
          Y := FTopRow;
        ecWindowBottom:
          Y := FTopRow + FVisibleRowCount - 1;

        { editing }

        {$IFDEF RAEDITOR_EDITOR}
        ecCharFirst..ecCharLast:
          if not FReadOnly then
          begin
            InsertChar(Char(ACommand - ecCharFirst));
            Complete;
          end;
        ecInsertPara:
          if not FReadOnly then
          begin
            DeleteSelected;
            {$IFDEF RAEDITOR_UNDO}
            TInsertUndo.Create(Self, FCaretX, FCaretY, #13#10);
            CaretUndo := false;
            {$ENDIF RAEDITOR_UNDO}
            if FLines.Count = 0 then FLines.Add('');
            F := SelStart - 1;
            FLines.Insert(Y + 1, System.Copy(FLines[Y], X + 1, Length(FLines[Y])));
            FLines.Internal[Y] := System.Copy(FLines[Y], 1, X);
            inc(Y);
            { smart tab }
            if FAutoIndent and
              (((Length(FLines[FCaretY]) > 0) and
               (FLines[FCaretY][1] = ' ')) or
              ((Trim(FLines[FCaretY]) = '') and (X > 0))) then
            begin
              X := GetTabStop(0, Y, tsAutoIndent, True);
              {$IFDEF RAEDITOR_UNDO}
              TInsertUndo.Create(Self, 0, Y, Spaces(X));
              {$ENDIF RAEDITOR_UNDO}
              FLines.Internal[Y] := Spaces(X) + FLines[Y];
            end
            else
              X := 0;
            UpdateEditorSize;
            TextModified(F, maInsert, #13#10);
            Invalidate;
            Changed;
          end;
        ecBackspace:
          if not FReadOnly then
            if X > 0 then
            begin
              { into line - в середине строки }
              if FSelected then
                DoAndCorrectXY(DeleteSelected)
              else
              begin
                ReLine;
                if FBackSpaceUnindents then
                  X := GetBackStop(FCaretX, FCaretY)
                else
                  X := FCaretX - 1;
                S := System.Copy(FLines[FCaretY], X + 1, FCaretX - X);
                {$IFDEF RAEDITOR_UNDO}
                TBackspaceUndo.Create(Self, FCaretX, FCaretY, S);
                CaretUndo := false;
                {$ENDIF RAEDITOR_UNDO}
                F := SelStart - 1;
                FLines.Internal[Y] := System.Copy(FLines[Y], 1, X) +
                  System.Copy(FLines[Y], FCaretX + 1, Length(FLines[Y]));
                TextModified(F, maDelete, S);
                PaintLine(Y, -1, -1);
              end;
              Changed;
            end
            else if Y > 0 then
            begin
              { on begin of line - в начале строки}
              DeleteSelected;
              F := SelStart - 2;

              if SelStart > 1 then
                 S := FLines.Text[SelStart - 2] + FLines.Text[SelStart - 1];

              if (Y > -1) and (Y < FLines.Count) then
                 begin
                 X := Length(FLines[Y - 1]);
                 {$IFDEF RAEDITOR_UNDO}
                 TBackspaceUndo.Create(Self, FCaretX, FCaretY, #13);
                 CaretUndo := false;
                 {$ENDIF RAEDITOR_UNDO}
                 FLines.Internal[Y - 1] := FLines[Y - 1] + FLines[Y];
                 FLines.Delete(Y);
                 end;

              dec(Y);
              UpdateEditorSize;
              TextModified(F, maDelete, S);
              Invalidate;
              Changed;
            end;
        ecDelete:
          if not FReadOnly then
          begin
            if FLines.Count = 0 then FLines.Add('');
            if FSelected then
              DoAndCorrectXY(DeleteSelected)
            else if X < Length(FLines[Y]) then
            begin
              { into line - в середине строки}
              {$IFDEF RAEDITOR_UNDO}
              TDeleteUndo.Create(Self, FCaretX, FCaretY, FLines[Y] [X + 1]);
              CaretUndo := false;
              {$ENDIF RAEDITOR_UNDO}
              S := FLines[Y] [X + 1];
              FLines.Internal[Y] := System.Copy(FLines[Y], 1, X) +
                System.Copy(FLines[Y], X + 2, Length(FLines[Y]));
              TextModified(SelStart, maDelete, S);
              PaintLine(FCaretY, -1, -1);
              Changed;
            end
            else if (Y >= 0) and (Y <= FLines.Count - 2) then
            begin
              { on end of line - в конце строки}
              {$IFDEF RAEDITOR_UNDO}
              TDeleteUndo.Create(Self, FCaretX, FCaretY, #13#10);
              CaretUndo := false;
              {$ENDIF RAEDITOR_UNDO}
              S := FLines.Text[SelStart + 1] + FLines.Text[SelStart + 2];
              FLines.Internal[Y] := FLines[Y] + FLines[Y + 1];
              FLines.Delete(Y + 1);
              UpdateEditorSize;
              TextModified(SelStart, maDelete, S);
              Invalidate;
              Changed;
            end;
          end;
        ecTab, ecBackTab:
          if not FReadOnly then
          begin
            if FSelected then
            begin
              if ACommand = ecTab then
                PostCommand(ecIndent)
              else
                PostCommand(ecUnindent);
            end
            else
            begin
              ReLine;
              X := GetTabStop(FCaretX, FCaretY, tsTabStop, ACommand = ecTab);
              if (ACommand = ecTab) and FInsertMode then
              begin
                S := FLines[FCaretY];
                S2 := Spaces(X - FCaretX);
                {$IFDEF RAEDITOR_UNDO}
                TInsertTabUndo.Create(Self, FCaretX, FCaretY, S2);
                CaretUndo := false;
                {$ENDIF RAEDITOR_UNDO}
                Insert(S2, S, FCaretX + 1);
                FLines.Internal[FCaretY] := S;
                TextModified(SelStart, maInsert, S2);
                PaintLine(FCaretY, -1, -1);
                Changed;
              end
              else
                { move cursor - oh yes!, it's allready moved: X := GetTabStop(..); }
            end;
          end;
        ecIndent:
          if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
            (FSelBegX = 0) and (FSelEndX = 0) then
          begin
            F := FindNotBlankCharPos(FLines[FCaretY]);
            S2 := Spaces(GetDefTabStop(F, True) - FCaretX);
            S := SelText;
            S := ReplaceString(S, #13#10, #13#10 + S2);
            Delete(S, Length(S) - Length(S2) + 1, Length(S2));
            SetSelText1(S2 + S)
          end;
        ecUnIndent:
          if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
            (FSelBegX = 0) and (FSelEndX = 0) then
          begin
            F := FindNotBlankCharPos(FLines[FCaretY]);
            S2 := Spaces(GetDefTabStop(F, True) - FCaretX);
            S := SelText;
            S := ReplaceString(S, #13#10 + S2, #13#10);
            for iBeg := 1 to Length(S2) do
              if S[1] = ' ' then
                Delete(S, 1, 1)
              else
                Break;
            SetSelText1(S);
          end;
        ecChangeInsertMode:
          begin
            FInsertMode := not FInsertMode;
            StatusChanged;
          end;
        ecClipBoardCut:
          if not FReadOnly then
            DoAndCorrectXY(ClipBoardCut);
        {$ENDIF RAEDITOR_EDITOR}
        ecClipBoardCopy:
          ClipBoardCopy;
        {$IFDEF RAEDITOR_EDITOR}
        ecClipBoardPaste:
          if not FReadOnly then
            DoAndCorrectXY(ClipBoardPaste);
        ecDeleteSelected:
          if not FReadOnly and FSelected then
            DoAndCorrectXY(DeleteSelected);

        ecDeleteWord:
          if not FReadOnly then
          begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecSelWord);
            // Command(ecSelNextWord); //???? it should work as in Delphi editor...
            Command(ecDeleteSelected);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecDeleteLine:
          if not FReadOnly then
          begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginLine);
            Command(ecSelEndLine);
            Command(ecDelete);
            Command(ecDelete);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecSelAll:
          begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginDoc);
            Command(ecSelEndDoc);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecToUpperCase:
          if not FReadOnly then
            SelText := ANSIUpperCase(SelText);
        ecToLowerCase:
          if not FReadOnly then
            SelText := ANSILowerCase(SelText);
        ecChangeCase:
          if not FReadOnly then
            SelText := ANSIChangeCase(SelText);
        {$ENDIF RAEDITOR_EDITOR}
        {$IFDEF RAEDITOR_UNDO}
        ecUndo:
          if not FReadOnly then
          begin
            FUndoBuffer.Undo;
            PaintCaret(true);
            Complete;
          end;
        ecRedo:
          if not FReadOnly then
          begin
            FUndoBuffer.Redo;
            PaintCaret(true);
            Complete;
          end;
        ecBeginCompound:
          BeginCompound;
        ecEndCompound:
          EndCompound;
        {$ENDIF RAEDITOR_UNDO}

        ecSetBookmark0..ecSetBookmark9:
          ChangeBookMark(ACommand - ecSetBookmark0, true);
        ecGotoBookmark0..ecGotoBookmark9:
          begin
            ChangeBookMark(ACommand - ecGotoBookmark0, false);
            X := FCaretX;
            Y := FCaretY;
          end;
        {$IFDEF RAEDITOR_COMPLETION}
        ecCompletionIdentifers:
          if not FReadOnly then
          begin
            FCompletion.DoCompletion(cmIdentifers);
            PaintCaret(true);
            Complete;
          end;
        ecCompletionTemplates:
          if not FReadOnly then
          begin
            FCompletion.DoCompletion(cmTemplates);
            PaintCaret(true);
            Complete;
          end;
        {$ENDIF RAEDITOR_COMPLETION}
        ecBeginUpdate:
          BeginUpdate;
        ecEndUpdate:
          EndUpdate;

        ecRecordMacro:
          if FRecording then
            EndRecord(FDefMacro)
          else
            BeginRecord;
        ecPlayMacro:
          begin
            PlayMacro(FDefMacro);
            Complete;
          end;
      end;

      {$IFDEF RAEDITOR_UNDO}
      if CaretUndo then
        SetCaret(X, Y)
      else
        SetCaretInternal(X, Y);
      {$ELSE}
      SetCaret(X, Y);
      {$ENDIF RAEDITOR_UNDO}
    except
      on E: EComplete do { OK };
    end;
  finally
    // dec(FUpdateLock);
    PaintCaret(true);
  end;
end;
{$ENDIF}

procedure TCustomEditor.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end; { PostCommand }

procedure TCustomEditor.WMEditCommand(var Message: TMessage);
begin
  Command(Message.WParam);
  Message.Result := ord(True);
end;

procedure TCustomEditor.WMCopy(var Message: TMessage);
begin
  PostCommand(ecClipboardCopy);
  Message.Result := ord(True);
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TCustomEditor.WMCut(var Message: TMessage);
begin
  if not FReadOnly then
    PostCommand(ecClipboardCut);
  Message.Result := ord(True);
end;

procedure TCustomEditor.WMPaste(var Message: TMessage);
begin
  if not FReadOnly then
    PostCommand(ecClipBoardPaste);
  Message.Result := ord(True);
end;
{$ENDIF}

{$IFDEF RAEDITOR_EDITOR}

procedure TCustomEditor.ChangeBookMark(const BookMark: TBookMarkNum; const
  Valid: boolean);

  procedure SetXY(X, Y: integer);
  var
    X1, Y1: integer;
  begin
    X1 := FLeftCol;
    Y1 := FTopRow;
    if (Y < FTopRow) or (Y > FLastVisibleRow) then
      Y1 := Y - (FVisibleRowCount div 2);
    if (X < FLeftCol) or (X > FVisibleColCount) then
      X1 := X - (FVisibleColCount div 2);
    SetLeftTop(X1, Y1);
    SetCaret(X, Y);
  end;

begin
  if Valid then
    if BookMarks[Bookmark].Valid and (BookMarks[Bookmark].Y = FCaretY) then
      BookMarks[Bookmark].Valid := false
    else
    begin
      BookMarks[Bookmark].X := FCaretX;
      BookMarks[Bookmark].Y := FCaretY;
      BookMarks[Bookmark].Valid := true;
    end
  else if BookMarks[Bookmark].Valid then
    SetXY(BookMarks[Bookmark].X, BookMarks[Bookmark].Y);
  BookmarkCnanged(BookMark);
end;
{$ENDIF}

procedure TCustomEditor.BookmarkCnanged(BookMark: integer);
begin
  FGutter.Invalidate;
end;


{############### Клавиатура ###############}

procedure TCustomEditor.SelectionChanged;
begin
  {abstract}
end;

procedure TCustomEditor.SetSel(const SelX, SelY: integer);

  procedure UpdateSelected;
  var
    iR: integer;
  begin
    if FUpdateSelBegY < FSelBegY then
      for iR := FUpdateSelBegY to FSelBegY do
        PaintLine(iR, -1, -1)
    else
      for iR := FSelBegY to FUpdateSelBegY do
        PaintLine(iR, -1, -1);
    if FUpdateSelEndY < FSelEndY then
      for iR := FUpdateSelEndY to FSelEndY do
        PaintLine(iR, -1, -1)
    else
      for iR := FSelEndY to FUpdateSelEndY do
        PaintLine(iR, -1, -1);
    SelectionChanged;
    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  end;

begin
  if not FSelected then
  begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelEndX := SelX;
    FSelEndY := SelY;
    FSelBegX := SelX;
    FSelBegY := SelY;
    FSelected := true;
  end
  else
  begin
    {$IFDEF RAEDITOR_UNDO}
    TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    {$ENDIF RAEDITOR_UNDO}
    FUpdateSelBegY := FSelBegY;
    FUpdateSelEndY := FSelEndY;
    if SelY <= FSelStartY then FSelBegY := SelY;
    if SelY >= FSelStartY then FSelEndY := SelY;
    if (SelY < FSelStartY) or ((SelY = FSelStartY) and (SelX <= FSelStartX))
      then
      FSelBegX := SelX;
    if (SelY > FSelStartY) or ((SelY = FSelStartY) and (SelX >= FSelStartX))
      then
      FSelEndX := SelX;
    if FSelBegY < 0 then FSelBegY := 0;
    FSelected := true;
  end;
  if FCompound = 0 then
    UpdateSelected;
  if FUpdateSelBegY > FSelBegY then FUpdateSelBegY := FSelBegY;
  if FUpdateSelEndY < FSelEndY then FUpdateSelEndY := FSelEndY;
end;

{************** Мышь ***************}

procedure TCustomEditor.Mouse2Cell(const X, Y: integer; var CX, CY: integer);
begin
  CX := Round((X - EditorClient.Left) / FCellRect.Width);
  CY := (Y - EditorClient.Top) div FCellRect.Height;
end;

procedure TCustomEditor.Mouse2Caret(const X, Y: integer; var CX, CY: integer);
begin
  Mouse2Cell(X, Y, CX, CY);
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := CX + FLeftCol;
  CY := CY + FTopRow;
  if CX > FLastVisibleCol then CX := FLastVisibleCol;
  if CY > FLines.Count - 1 then CY := FLines.Count - 1;
end;

procedure TCustomEditor.CaretCoord(const X, Y: integer; var CX, CY: integer);
begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := FCellRect.Width * CX;
  CY := FCellRect.Height * CY;
end;

procedure TCustomEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  XX, YY: integer;
begin
  if FDbl then
  begin
    FDbl := false;
    exit
  end;
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  {$ENDIF RAEDITOR_COMPLETION}
  Mouse2Caret(X, Y, XX, YY);
  // if (XX = FCaretX) and (YY = FCaretY) then exit;
  PaintCaret(false);
  if Button = mbLeft then
    SetUnSelected;
  SetFocus;
  if Button = mbLeft then
    SetCaret(XX, YY);
  PaintCaret(true);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomEditor.DblClick;
var
  iBeg, iEnd: integer;
begin
  FDbl := true;
  if Assigned(FOnDblClick) then FOnDblClick(Self);
  if FDoubleClickLine then
  begin
    PaintCaret(false);
    SetSel(0, FCaretY);
    if FCaretY = FLines.Count - 1 then
    begin
      SetSel(Length(FLines[FCaretY]), FCaretY);
      SetCaret(Length(FLines[FCaretY]), FCaretY);
    end
    else
    begin
      SetSel(0, FCaretY + 1);
      SetCaret(0, FCaretY + 1);
    end;
    PaintCaret(true);
  end
  else if GetWordOnPosEx(FLines[FCaretY] + ' ', FCaretX + 1, iBeg, iEnd) <> ''
    then
  begin
    PaintCaret(false);
    SetSel(iBeg - 1, FCaretY);
    SetSel(iEnd - 1, FCaretY);
    SetCaret(iEnd - 1, FCaretY);
    PaintCaret(true);
  end;
end;

procedure TCustomEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  timerScroll.Enabled := false;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    PaintCaret(false);
    MouseMoveY := Y;
    Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);
    if MouseMoveYY <= FLastVisibleRow then
    begin
      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    end;
    timerScroll.Enabled := (Y < 0) or (Y > ClientHeight);
    PaintCaret(true);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomEditor.ScrollTimer(Sender: TObject);
begin
  if (MouseMoveY < 0) or (MouseMoveY > ClientHeight) then
  begin
    if (MouseMoveY < -20) then
      dec(MouseMoveYY, FVisibleRowCount)
    else if (MouseMoveY < 0) then
      dec(MouseMoveYY)
    else if (MouseMoveY > ClientHeight + 20) then
      inc(MouseMoveYY, FVisibleRowCount)
    else if (MouseMoveY > ClientHeight) then
      inc(MouseMoveYY);
    PaintCaret(false);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(true);
  end;
end;

{############## Мышь ###############}

{************** ClipBoard ***************}

function TCustomEditor.GetSelText: string;
var
  S: string;
  i: integer;
begin
  Result := '';
  if not FSelected then exit;
  if not FSelBlock then
  begin
    if (FSelBegY < 0) or (FSelBegY > FLines.Count - 1) or (FSelEndY < 0) or
      (FSelEndY > FLines.Count - 1) then
    begin
      Err;
      Exit;
    end;
    if FSelBegY = FSelEndY then
      S := System.Copy(FLines[FSelEndY], FSelBegX + 1, FSelEndX - FSelBegX +
        Integer(FInclusive))
    else
    begin
      S := System.Copy(FLines[FSelBegY], FSelBegX + 1, Length(FLines[FSelBegY]));
      for i := FSelBegY + 1 to FSelEndY - 1 do
        S := S + #13#10 + FLines[i];
      S := S + #13#10 + System.Copy(FLines[FSelEndY], 1, FSelEndX +
        Integer(FInclusive));
    end;
    Result := S;
  end;
end;

procedure TCustomEditor.SetSelText(const AValue: string);
begin
  BeginUpdate;
  try
    BeginCompound;
    DeleteSelected;
    InsertText(AValue);
    FSelected := True;
    SelStart := PosFromCaret(FSelBegX, FSelBegY);
    SelLength := Length(AValue);
    EndCompound;
  finally
    EndUpdate;
  end;
end;

procedure TCustomEditor.ClipBoardCopy;
begin
  if not FSelBlock then
    ClipBoard.SetTextBuf(PChar(GetSelText));
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TCustomEditor.InsertText(const Text: string);
var
  S: string;
  P: integer;
  X, Y: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  S := FLines.Text;
  P := PosFromCaret(FCaretX, FCaretY);
  {$IFDEF RAEDITOR_UNDO}
  TInsertUndo.Create(Self, FCaretX, FCaretY, Text);
  //FUndoBuffer.EndGroup;
  {$ENDIF RAEDITOR_UNDO}
  Insert(Text, S, P + 1);
  TextModified(P, maInsert, Text);
  FLines.Text := S; {!!! Вызывает перерисовку всего}
  CaretFromPos(P + Length(Text), X, Y);
  SetCaretInternal(X, Y);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

{заменяет слово в позиции курсора на NewString}
{ строка NewString не должна содержать #13, #10}

procedure TCustomEditor.ReplaceWord(const NewString: string);
var
  iBeg, iEnd: integer;

  function GetWordOnPos2(S: string; P: integer): string;
  begin
    Result := '';
    if P < 1 then exit;
    if (S[P] in Separators) and ((P < 1) or (S[P - 1] in Separators)) then
      inc(P);
    iBeg := P;
    while iBeg >= 1 do
      if S[iBeg] in Separators then
        break
      else
        dec(iBeg);
    inc(iBeg);
    iEnd := P;
    while iEnd <= Length(S) do
      if S[iEnd] in Separators then
        break
      else
        inc(iEnd);
    if iEnd > iBeg then
      Result := System.Copy(S, iBeg, iEnd - iBeg)
    else
      Result := S[P];
  end;

var
  S, W: string;
  X: integer;
  F: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  F := PosFromCaret(FCaretX, FCaretY);
  S := FLines[FCaretY];
  while FCaretX > Length(S) do
    S := S + ' ';
  W := Trim(GetWordOnPos2(S, FCaretX));
  if W = '' then
  begin
    iBeg := FCaretX + 1;
    iEnd := FCaretX
  end;
  {$IFDEF RAEDITOR_UNDO}
  CantUndo;
  //TReplaceUndo.Create(Self, FCaretX - Length(W), FCaretY, iBeg, iEnd, W, NewString);
  {$ENDIF RAEDITOR_UNDO}
  //  LW := Length(W);
  if FSelected then
  begin
    if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
      // скорректировать LW ..
  end;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(NewString, S, iBeg);
  FLines.Internal[FCaretY] := S;
  X := iBeg + Length(NewString) - 1;
  TextModified(F, maInsert, NewString);
  PaintLine(FCaretY, -1, -1);
  SetCaretInternal(X, FCaretY);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

{заменяет слово в позиции курсора на NewString}

procedure TCustomEditor.ReplaceWord2(const NewString: string);
var
  S, S1, W: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret: integer;
begin
  PaintCaret(false);
  if FCaretX > Length(FLines[FCaretY]) then
    FLines.Internal[FCaretY] := FLines[FCaretY] +
      Spaces(FCaretX - Length(FLines[FCaretY]));
  S := FLines.Text;
  P := PosFromCaret(FCaretX, FCaretY);
  W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
  if W = '' then
  begin
    iBeg := P + 1;
    iEnd := P
  end;
  S1 := NewString;
  NewCaret := Length(NewString);
  {$IFDEF RAEDITOR_UNDO}
  TReplaceUndo.Create(Self, FCaretX, FCaretY, iBeg, iEnd, W, S1);
  {$ENDIF RAEDITOR_UNDO}
  //  LW := Length(W);
  if FSelected then
  begin
    if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
      // скорректировать LW ..
  end;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(S1, S, iBeg);
  FLines.Text := S; {!!! Вызывает перерисовку всего}
  CaretFromPos(iBeg + NewCaret - 1, X, Y);
  SetCaretInternal(X, Y);
  Changed;
  PaintCaret(true);
end;
{$ENDIF RAEDITOR_EDITOR}

procedure TCustomEditor.ClipBoardPaste;
var
  S, ClipS: string;
  Len, P: integer;
  H: THandle;
  X, Y: integer;
begin
  if (FCaretY > FLines.Count - 1) and (FLines.Count > 0) then Err;
  BeginUpdate;
  H := ClipBoard.GetAsHandle(CF_TEXT);
  Len := GlobalSize(H);
  if Len = 0 then exit;
  PaintCaret(false);
  BeginCompound;
  try
    DeleteSelected;
    SetLength(ClipS, Len);
    SetLength(ClipS, ClipBoard.GetTextBuf(PChar(ClipS), Len));
    ClipS := ExpandTabs(AdjustLineBreaks(ClipS));
    if FLines.Count > 0 then
      ReLine;
    S := FLines.Text;
    if FLines.Count > 0 then
      P := PosFromCaret(FCaretX, FCaretY)
    else
      P := 0;
   {$IFDEF RAEDITOR_UNDO}
    TInsertUndo.Create(Self, FCaretX, FCaretY, ClipS);
   {$ENDIF RAEDITOR_UNDO}
  finally
    EndCompound;
  end;
  Insert(ClipS, S, P + 1);
  TextModified(P, maInsert, ClipS);
  FLines.SetLockText(S);
  CaretFromPos(P + Length(ClipS), X, Y);
  SetCaretInternal(X, Y);
  Changed;
  // Invalidate;
  EndUpdate; {!!! Вызывает перерисовку всего}
  PaintCaret(true);
end;

procedure TCustomEditor.ClipBoardCut;
begin
  ClipBoardCopy;
  DeleteSelected;
end;

procedure TCustomEditor.DeleteSelected;
var
  S, S1: string;
  iBeg, iEnd, X, Y: integer;
begin
  if FSelected then
  begin
    PaintCaret(false);
    S := FLines.Text;
    iBeg := PosFromCaret(FSelBegX, FSelBegY);
    iEnd := PosFromCaret(FSelEndX, FSelEndY);
    {$IFDEF RAEDITOR_UNDO}
    TDeleteSelectedUndo.Create(Self, FCaretX, FCaretY, GetSelText, FSelBlock,
      FSelBegX, FSelBegY, FSelEndX, FSelEndY);
    {$ENDIF RAEDITOR_UNDO}
    Delete(S, iBeg + 1, iEnd - iBeg);
    S1 := GetSelText;
    FSelected := false;
    FLines.SetLockText(S);
    TextModified(iBeg, maDelete, S1);
    CaretFromPos(iBeg, X, Y);
    SetCaretInternal(X, Y);
    Changed;
    Invalidate; {!!!}
    PaintCaret(true);
  end;
end;
{############### ClipBoard ###############}

procedure TCustomEditor.SetGutterWidth(AWidth: integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TCustomEditor.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  end;
end;

function TCustomEditor.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TCustomEditor.SetLines(ALines: TStrings);
begin
  if ALines <> nil then FLines.Assign(ALines);
 {$IFDEF RAEDITOR_UNDO}
  CantUndo;
 {$ENDIF RAEDITOR_UNDO}
end;

procedure TCustomEditor.TextAllChanged;
begin
  TextAllChangedInternal(True);
end;

procedure TCustomEditor.TextAllChangedInternal(const Unselect: Boolean);
begin
  if Unselect then
    FSelected := false;
  TextModified(0, maInsert, FLines.Text);
  UpdateEditorSize;
  if Showing and (FUpdateLock = 0) then Invalidate;
end;

procedure TCustomEditor.SetCols(ACols: integer);
begin
  if FCols <> ACols then
  begin
    FCols := Math.Max(ACols, 1);
    scbHorz.Max := FCols - 1;
  end;
end;

procedure TCustomEditor.SetRows(ARows: integer);
begin
  if FRows <> ARows then
  begin
    FRows := Math.Max(ARows, 1);
    scbVert.Max := Math.Max(1, FRows - 1 + FVisibleRowCount - 1);
  end;
end;

procedure TCustomEditor.SetLeftTop(ALeftCol, ATopRow: integer);
begin
  if ALeftCol < 0 then ALeftCol := 0;
  if (FLeftCol <> ALeftCol) then
  begin
    scbHorz.Position := ALeftCol;
    Scroll(false, ALeftCol);
  end;
  if ATopRow < 0 then ATopRow := 0;
  if (FTopRow <> ATopRow) then
  begin
    scbVert.Position := ATopRow;
    Scroll(true, ATopRow);
  end;
end;

procedure TCustomEditor.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TCustomEditor.SetRightMarginVisible(Value: boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TCustomEditor.SetRightMargin(Value: integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    Invalidate;
  end;
end;

procedure TCustomEditor.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;

function TCustomEditor.ExpandTabs(const S: string): string;
var
  i: integer;
  Sp: string;
begin
  { very slow and not complete implementation - NEED TO OPTIMIZE ! }
  if System.Pos(#9, S) > 0 then
  begin
    Sp := Spaces(GetDefTabStop(0 {!!}, True));
    Result := '';
    for i := 1 to Length(S) do
      if S[i] = #9 then
        Result := Result + Sp
      else
        Result := Result + S[i];
  end
  else
    Result := S;
end;

{ ************************ triggers ************************ }

procedure TCustomEditor.TextModified(Pos: integer; Action: TModifiedAction;
  Text: string);
begin
  { This method don't called at all cases, when text is modified,
    so don't use it. Later I may be complete it. }
end;

procedure TCustomEditor.Changed;
begin
  FModified := true;
  FPEditBuffer := nil;
  if Assigned(FOnChange) then FOnChange(self);
  StatusChanged;
end;

procedure TCustomEditor.StatusChanged;
begin
  if Assigned(FOnChangeStatus) then FOnChangeStatus(self);
end;

procedure TCustomEditor.CaretFromPos(const Pos: integer; var X, Y: integer);
begin
  GetXYByPos(FLines.Text, Pos, X, Y);
end;

function TCustomEditor.PosFromCaret(const X, Y: integer): integer;
{наоборот}
var
  i: integer;
begin
  if (Y > FLines.Count - 1) or (Y < 0) then
    Result := -1
  else
  begin
    Result := 0;
    for i := 0 to Y - 1 do
      inc(Result, Length(FLines[i]) + 2 {CR/LF});
    if X < Length(FLines[Y]) then
      inc(Result, X)
    else
      inc(Result, Length(FLines[Y]))
  end;
end;

function TCustomEditor.PosFromMouse(const X, Y: integer): integer;
var
  X1, Y1: integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then
    Result := -1
  else
    Result := PosFromCaret(X1, Y1);
end;

function TCustomEditor.GetTextLen: Integer;
begin
  Result := Length(FLines.Text);
end;

function TCustomEditor.GetSelStart: integer;
begin
  Result := PosFromCaret(FCaretX, FCaretY);
end;

procedure TCustomEditor.SetSelStart(const ASelStart: integer);
begin
  FSelected := true;
  CaretFromPos(ASelStart, FSelBegX, FSelBegY);
 { FCaretX := FSelBegX;
  FCaretY := FSelBegY; }
  SetCaretInternal(FSelBegX, FSelBegY);
  SetSelLength(0);
  MakeRowVisible(FSelBegY);
  //  PaintSelection;
  //  EditorPaint;
end;

procedure TCustomEditor.MakeRowVisible(ARow: Integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := FCaretY - Trunc(VisibleRowCount / 2);
    if ARow < 0 then ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

function TCustomEditor.GetSelLength: integer;
begin
  Result := Length(GetSelText);
end;

procedure TCustomEditor.SetSelLength(const ASelLength: integer);
begin
  FSelected := ASelLength > 0;
  CaretFromPos(SelStart + ASelLength, FSelEndX, FSelEndY);
	FUpdateSelBegY := FSelBegY;
	FUpdateSelEndY := FSelEndY;
  SetCaretInternal(FSelEndX, FSelEndY);
  //PaintSelection;
  Invalidate;
end;

procedure TCustomEditor.SetLockText(const Text: string);
begin
  FLines.SetLockText(Text);
end;

procedure TCustomEditor.GutterPaint(Canvas: TCanvas);
begin
  if Assigned(FOnPaintGutter) then FOnPaintGutter(Self, Canvas);
end;

procedure TCustomEditor.SetMode(index: integer; Value: boolean);
var
  PB: ^boolean;
begin
  case index of
    0: PB := @FInsertMode;
  else {1 :}
    PB := @FReadOnly;
  end;
  if PB^ <> Value then
  begin
    PB^ := Value;
    StatusChanged;
  end;
end;

function TCustomEditor.GetWordOnCaret: string;
begin
  Result := GetWordOnPos(FLines[CaretY], CaretX);
end;

function TCustomEditor.GetTabStop(const X, Y: Integer; const What: TTabStop;
  const Next: Boolean): Integer;

  procedure UpdateTabStops;
  var
    S: string;
    j, i: Integer;
  begin
    FillChar(FTabPos, sizeof(FTabPos), False);
    if (What = tsAutoIndent) or FSmartTab then
    begin
      j := 1;
      i := 1;
      while Y - j >= 0 do
      begin
        S := TrimRight(FLines[Y - j]);
        if Length(S) > i then
          FTabPos[Length(S)] := True;
        while i <= Length(S) do { Iterate }
        begin
          if CharInSet(S[i], StIdSymbols) then
          begin
            FTabPos[i - 1] := True;
            while (i <= Length(S)) and CharInSet(S[i], StIdSymbols) do
              inc(i);
          end;
          inc(i);
        end; { for }
        if i >= Max_X_Scroll then Break;
        if j >= FVisibleRowCount * 2 then Break;
        inc(j);
      end;
    end;
  end;

var
  i: integer;
begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for i := X + 1 to High(FTabPos) do { Iterate }
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
    if Result = X then
      Result := GetDefTabStop(X, True);
  end
  else
  begin
    if Result = X then
      Result := GetDefTabStop(X, False);
  end;
end;

function TCustomEditor.GetDefTabStop(const X: integer; const Next: Boolean)
  : Integer;
var
  i: Integer;
  S: string;
  A, B: Integer;
begin
  i := 0;
  S := Trim(SubStr(FTabStops, i, ' '));
  A := 0; B := 1;
  while S <> '' do
  begin
    A := B;
    B := StrToInt(S) - 1;
    if B > X then
    begin
      Result := B;
      Exit;
    end;
    inc(i);
    S := Trim(SubStr(FTabStops, i, ' '));
  end;
  { after last tab pos }
  Result := X + ((B - A) - ((X - B) mod (B - A)));
end;

function TCustomEditor.GetBackStop(const X, Y: Integer): Integer;

  procedure UpdateBackStops;
  var
    S: string;
    j, i, k: Integer;
  begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos, sizeof(FTabPos), False);
    FTabPos[0] := True;
    while Y - j >= 0 do
    begin
      S := FLines[Y - j];
      for k := 1 to Math.Min(Length(S), i) do { Iterate }
        if S[k] <> ' ' then
        begin
          i := k;
          FTabPos[i - 1] := True;
          Break;
        end;
      if i = 1 then Break;
      if j >= FVisibleRowCount * 2 then Break;
      inc(j);
    end;
  end;

var
  i: integer;
  S: string;
begin
  Result := X - 1;
  S := TrimRight(FLines[Y]);
  if (Trim(System.Copy(S, 1, X)) = '') and
    ((X + 1 > Length(S)) or (S[X + 1] <> ' ')) then
  begin
    UpdateBackStops;
    for i := X downto 0 do
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

procedure TCustomEditor.BeginCompound;
begin
  inc(FCompound);
 {$IFDEF RAEDITOR_UNDO}
  TBeginCompoundUndo.Create(Self);
 {$ENDIF RAEDITOR_UNDO}
end;

procedure TCustomEditor.EndCompound;
begin
 {$IFDEF RAEDITOR_UNDO}
  TEndCompoundUndo.Create(Self);
 {$ENDIF RAEDITOR_UNDO}
  dec(FCompound);
end;

procedure TCustomEditor.BeginRecord;
begin
  FMacro := '';
  FRecording := True;
  StatusChanged;
end;

procedure TCustomEditor.EndRecord(var AMacro: TMacro);
begin
  FRecording := False;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TCustomEditor.PlayMacro(const AMacro: TMacro);
var
  i: Integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    i := 1;
    while i < Length(AMacro) do
    begin
      Command(byte(AMacro[i]) + byte(AMacro[i + 1]) shl 8);
      inc(i, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;


{************************** TEditKey ***************************}

constructor TEditKey.Create(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
end;

constructor TEditKey.Create2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
end;

constructor TKeyboard.Create;
begin
  List := TList.Create;
end;

destructor TKeyboard.Destroy;
begin
  Clear;
  List.Free;
end;

procedure TKeyboard.Add(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  List.Add(TEditKey.Create(ACommand, AKey1, AShift1));
end;

procedure TKeyboard.Add2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  List.Add(TEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TKeyboard.Clear;
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function TKeyboard.Command(const AKey: word; const AShift: TShiftState):
  TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey) and (Shift1 = AShift) then
      begin
        if Key2 = 0 then
          Result := Command
        else
          Result := twoKeyCommand;
        Exit;
      end;
end;

function TKeyboard.Command2(const AKey1: word; const AShift1: TShiftState;
  const AKey2: word; const AShift2: TShiftState): TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey1) and (Shift1 = AShift1) and
        (Key2 = AKey2) and (Shift2 = AShift2) then
      begin
        Result := Command;
        Exit;
      end;
end;

{$IFDEF RAEDITOR_EDITOR}
{$IFDEF RAEDITOR_DEFLAYOT}

procedure TKeyboard.SetDefLayot;
begin
  Clear;
  Add(ecLeft, VK_LEFT, []);
  Add(ecRight, VK_RIGHT, []);
  Add(ecUp, VK_UP, []);
  Add(ecDown, VK_DOWN, []);
  Add(ecSelLeft, VK_LEFT, [ssShift]);
  Add(ecSelRight, VK_RIGHT, [ssShift]);
  Add(ecSelUp, VK_UP, [ssShift]);
  Add(ecSelDown, VK_DOWN, [ssShift]);
  Add(ecBeginLine, VK_HOME, []);
  Add(ecSelBeginLine, VK_HOME, [ssShift]);
  Add(ecBeginDoc, VK_HOME, [ssCtrl]);
  Add(ecSelBeginDoc, VK_HOME, [ssCtrl, ssShift]);
  Add(ecEndLine, VK_END, []);
  Add(ecSelEndLine, VK_END, [ssShift]);
  Add(ecEndDoc, VK_END, [ssCtrl]);
  Add(ecSelEndDoc, VK_END, [ssCtrl, ssShift]);
  Add(ecPrevWord, VK_LEFT, [ssCtrl]);
  Add(ecNextWord, VK_RIGHT, [ssCtrl]);
  Add(ecSelPrevWord, VK_LEFT, [ssCtrl, ssShift]);
  Add(ecSelNextWord, VK_RIGHT, [ssCtrl, ssShift]);
  Add(ecSelAll, ord('A'), [ssCtrl]);

  Add(ecWindowTop, VK_PRIOR, [ssCtrl]);
  Add(ecWindowBottom, VK_NEXT, [ssCtrl]);
  Add(ecPrevPage, VK_PRIOR, []);
  Add(ecNextPage, VK_NEXT, []);
  Add(ecSelPrevPage, VK_PRIOR, [ssShift]);
  Add(ecSelNextPage, VK_NEXT, [ssShift]);
  Add(ecScrollLineUp, VK_UP, [ssCtrl]);
  Add(ecScrollLineDown, VK_DOWN, [ssCtrl]);

  Add(ecChangeInsertMode, VK_INSERT, []);

  Add(ecInsertPara, VK_RETURN, []);
  Add(ecBackspace, VK_BACK, []);
  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);
  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipBoardPaste, VK_INSERT, [ssShift]);

  Add(ecClipboardCopy, ord('C'), [ssCtrl]);
  Add(ecClipboardCut, ord('X'), [ssCtrl]);
  Add(ecClipBoardPaste, ord('V'), [ssCtrl]);


  Add(ecSetBookmark0, ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, ord('9'), [ssCtrl]);

  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), []);
  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), []);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), []);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), []);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), []);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), []);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), []);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), []);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), []);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), []);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), []);
  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), []);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), []);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), []);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), []);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), []);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), []);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), []);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), []);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), []);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), [ssCtrl]);

  {$IFDEF RAEDITOR_UNDO}
  Add(ecUndo, ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);
  {$ENDIF RAEDITOR_UNDO}

  {$IFDEF RAEDITOR_COMPLETION}
  Add(ecCompletionIdentifers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, ord('J'), [ssCtrl]);
  {$ENDIF RAEDITOR_COMPLETION}

  { cursor movement - default and classic }
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), []);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), []);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), []);
  Add2(ecLeft, ord('Q'), [ssCtrl], ord('P'), []);
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), []);
  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), []);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), []);
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecLeft, ord('Q'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), [ssCtrl]);

  Add(ecDeleteWord, ord('T'), [ssCtrl]);
  Add(ecInsertPara, ord('N'), [ssCtrl]);
  Add(ecDeleteLine, ord('Y'), [ssCtrl]);

  Add2(ecSelWord, ord('K'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecToUpperCase, ord('K'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecToLowerCase, ord('K'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecChangeCase, ord('O'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecIndent, ord('K'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecUnindent, ord('K'), [ssCtrl], ord('U'), [ssCtrl]);

  Add(ecRecordMacro, ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, ord('P'), [ssCtrl, ssShift]);
end;
{$ENDIF RAEDITOR_DEFLAYOT}


{$IFDEF RAEDITOR_UNDO}

procedure RedoNotImplemented;
begin
  raise EEditorError.Create('Redo not yet implemented');
end;

procedure TCustomEditor.CantUndo;
begin
  FUndoBuffer.Clear;
end;

procedure TUndoBuffer.Add(AUndo: TUndo);
begin
  if InUndo then exit;
  while (Count > 0) and (FPtr < Count - 1) do
  begin
    TUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: Integer;
begin
  InUndo := true;
  try
    if LastUndo <> nil then
    begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      while (LastUndo <> nil) and
        ((UndoClass = LastUndo.ClassType) or
        (LastUndo is TDeleteTrailUndo) or
        (LastUndo is TReLineUndo) or
        (Compound > 0)) do
      begin
        if LastUndo.ClassType = TBeginCompoundUndo then
        begin
          dec(Compound);
          UndoClass := nil;
        end
        else if LastUndo.ClassType = TEndCompoundUndo then
          inc(Compound);
        LastUndo.Undo;
        dec(FPtr);
        if (UndoClass = TDeleteTrailUndo) or
          (UndoClass = TReLineUndo) then
          UndoClass := LastUndo.ClassType;
        if not FRAEditor.FGroupUndo then break;
        // FRAEditor.Paint; {DEBUG !!!!!!!!!}
      end;
      if FRAEditor.FUpdateLock = 0 then
      begin
        FRAEditor.TextAllChangedInternal(False);
        FRAEditor.Changed;
      end;
    end;
  finally
    InUndo := false;
  end;
end;

procedure TUndoBuffer.Redo;
begin
  { DEBUG !!!! }
  inc(FPtr);
  LastUndo.Redo;
end;

procedure TUndoBuffer.Clear;
begin
  while Count > 0 do
  begin
    TUndo(Items[0]).Free;
    inherited Delete(0);
  end;
end;

procedure TUndoBuffer.Delete;
begin
  if Count > 0 then
  begin
    TUndo(Items[Count - 1]).Free;
    inherited Delete(Count - 1);
  end;
end;

function TUndoBuffer.LastUndo: TUndo;
begin
  if (FPtr >= 0) and (Count > 0) then
    Result := TUndo(Items[FPtr])
  else
    Result := nil;
end;

function TUndoBuffer.IsNewGroup(AUndo: TUndo): boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;


{* TUndo}

constructor TUndo.Create(ARAEditor: TCustomEditor);
begin
  FRAEditor := ARAEditor;
  UndoBuffer.Add(Self);
end;

function TUndo.UndoBuffer: TUndoBuffer;
begin
  if FRAEditor <> nil then
    Result := FRAEditor.FUndoBuffer
  else
    Result := nil;
end;
{* TUndo}

{* TCaretUndo}

constructor TCaretUndo.Create(ARAEditor: TCustomEditor; ACaretX, ACaretY:
  integer);
begin
  inherited Create(ARAEditor);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
end;

procedure TCaretUndo.Undo;
begin
  with UndoBuffer do
  begin
    dec(FPtr);
    while FRAEditor.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      dec(FPtr);
    inc(FPtr);
    with TCaretUndo(Items[FPtr]) do
      FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TCaretUndo.Redo;
begin
  RedoNotImplemented;
end;
{# TCaretUndo}

{* TInsertUndo}

constructor TInsertUndo.Create(ARAEditor: TCustomEditor; ACaretX, ACaretY:
  integer; AText: string);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FText := AText;
end;

procedure TInsertUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TInsertUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TInsertUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret(FCaretX, FCaretY);
    Delete(S, iBeg + 1, Length(Text));
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TInsertUndo}

{* TDeleteUndo}

procedure TDeleteUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret(FRAEditor.FCaretX, FRAEditor.FCaretY);
    Insert(Text, S, iBeg + 1);
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TDeleteUndo}

{* TBackspaceUndo}

procedure TBackspaceUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TDeleteUndo(LastUndo).FText;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret(FRAEditor.FCaretX, FRAEditor.FCaretY);
    Insert(Text, S, iBeg + 1);
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TBackspaceUndo}

{* TReplaceUndo}

constructor TReplaceUndo.Create(ARAEditor: TCustomEditor; ACaretX, ACaretY:
  integer; ABeg, AEnd: integer; AText, ANewText: string);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FBeg := ABeg;
  FEnd := AEnd;
  FText := AText;
  FNewText := ANewText;
end;

procedure TReplaceUndo.Undo;
var
  S: string;
begin
  S := FRAEditor.FLines.Text;
  Delete(S, FBeg, Length(FNewText));
  Insert(FText, S, FBeg);
  FRAEditor.FLines.SetLockText(S);
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;
{# TReplaceUndo}

{* TDeleteSelectedUndo}

constructor TDeleteSelectedUndo.Create(ARAEditor: TCustomEditor; ACaretX,
  ACaretY: integer; AText: string; ASelBlock: boolean; ASelBegX, ASelBegY,
  ASelEndX, ASelEndY: integer);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY, AText);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TDeleteSelectedUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret(FSelBegX, FSelBegY);
    Insert(Text, S, iBeg + 1);
    {выделить FSelBegX, FSelBegY}
   //!!!! ..
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.FSelBlock := FSelBlock;
    FRAEditor.FSelBegX := FSelBegX;
    FRAEditor.FSelBegY := FSelBegY;
    FRAEditor.FSelEndX := FSelEndX;
    FRAEditor.FSelEndY := FSelEndY;
    FRAEditor.FSelected := Length(FText) > 0;
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TDeleteSelectedUndo}

{* TSelectUndo}

constructor TSelectUndo.Create(ARAEditor: TCustomEditor; ACaretX,
  ACaretY: integer; ASelBlock: boolean; ASelBegX, ASelBegY, ASelEndX,
  ASelEndY: integer);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TSelectUndo.Undo;
begin
  FRAEditor.FSelected := True;
  FRAEditor.FSelBegX := FSelBegX;
  FRAEditor.FSelBegY := FSelBegY;
  FRAEditor.FSelEndX := FSelEndX;
  FRAEditor.FSelEndY := FSelEndY;
  FRAEditor.FSelBlock := FSelBlock;
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;
{# TSelectUndo}

procedure TBeginCompoundUndo.Undo;
begin
  { nothing }
end;

{$ENDIF RAEDITOR_UNDO}

{#####################  Undo  #####################}


{*********************  Code Completion  *********************}
{$IFDEF RAEDITOR_COMPLETION}

procedure TCustomEditor.CompletionIdentifer(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomEditor.CompletionTemplate(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomEditor.DoCompletionIdentifer(var Cancel: boolean);
begin
  CompletionIdentifer(Cancel);
  if Assigned(FOnCompletionIdentifer) then FOnCompletionIdentifer(Self, Cancel);
end;

procedure TCustomEditor.DoCompletionTemplate(var Cancel: boolean);
begin
  CompletionTemplate(Cancel);
  if Assigned(FOnCompletionTemplate) then FOnCompletionTemplate(Self, Cancel);
end;

type
  TBaseEditorCompletionList = class(TListBox)
  private
    FTimer: TTimer;
    YY: integer;
    // HintWindow : THintWindow;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure OnTimer(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCompletion.Create2(ARAEditor: TCustomEditor);
begin
  inherited Create;
  FRAEditor := ARAEditor;
  FPopupList := TBaseEditorCompletionList.Create(FRAEditor);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifers := TStringList.Create;
  FTemplates := TStringList.Create;
  FItems := TStringList.Create;
  FDefMode := cmIdentifers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TCompletion.Destroy;
begin
  inherited Destroy;
  FPopupList.Free;
  FIdentifers.Free;
  FTemplates.Free;
  FItems.Free;
  FTimer.Free;
end;

function TCompletion.GetItems: TStrings;
begin
  case FMode of
    cmIdentifers: Result := FIdentifers;
    cmTemplates: Result := FTemplates;
  else
    Result := nil;
  end;
end;

{заменяет слово в позиции курсора на NewString}

procedure TCompletion.ReplaceWord(const NewString: string);
var
  S, S1, W: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret, LNum, CX, CY, i: integer;
begin
  with FRAEditor do
  begin
    PaintCaret(false);
    BeginUpdate;
    ReLine;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);
    if S[P] in Separators then
      W := ''
    else
      W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
    LNum := 0;
    CaretFromPos(iBeg, CX, CY);
    if W = '' then
    begin
      iBeg := P + 1;
      iEnd := P
    end;
    case FMode of
      cmIdentifers:
        begin
          S1 := NewString;
          NewCaret := Length(NewString);
        end;
      cmTemplates:
        begin
          S1 := ReplaceString(NewString, FCRLF, #13#10 + Spaces(FCaretX -
            Length(W)));
          S1 := ReplaceString(S1, FCaretChar, '');
          NewCaret := System.Pos(FCaretChar, NewString) - 1;
          if NewCaret = -1 then NewCaret := Length(S1);
          for i := 1 to NewCaret do
            if S1[i] = #13 then inc(LNum);
        end
    else
      raise EEditorError.Create('Invalid RAEditor Completion Mode');
    end;
    {$IFDEF RAEDITOR_UNDO}
    TReplaceUndo.Create(FRAEditor, FCaretX, FCaretY, iBeg, iEnd, W, S1);
    {$ENDIF RAEDITOR_UNDO}
    //  LW := Length(W);
    if FSelected then
    begin
      if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
        // скорректировать LW ..
    end;
    Delete(S, iBeg, iEnd - iBeg);
    Insert(S1, S, iBeg);
    FLines.SetLockText(S);
    CaretFromPos(iBeg - 1 + (CX - 1) * LNum + NewCaret, X, Y);
    SetCaretInternal(X, Y);
    FRAEditor.TextAllChanged; // Invalidate; {!!!}
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TCompletion.DoKeyPress(Key: Char);
begin
  if FVisible then
    if HasChar(Key, RAEditorCompletionChars) then
      SelectItem
    else
      CloseUp(true)
  else if FEnabled then
    FTimer.Enabled := true;
end;

function TCompletion.DoKeyDown(Key: Word; Shift: TShiftState): boolean;
begin
  Result := true;
  case Key of
    VK_ESCAPE: CloseUp(false);
    VK_RETURN: CloseUp(true);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  else
    Result := false;
  end;
end;

procedure TCompletion.DoCompletion(const AMode: TCompletionList);
var
  Eq: boolean;
  Cancel: boolean;
begin
  if FRAEditor.FReadOnly then exit;
  if FPopupList.Visible then CloseUp(False);
  FMode := AMode;
  case FMode of
    cmIdentifers:
      DropDown(AMode, true);
    cmTemplates:
      begin
        Cancel := false;
        // FRAEditor.DoCompletionIdentifer(Cancel);
        FRAEditor.DoCompletionTemplate(Cancel);
        if Cancel or (FTemplates.Count = 0) then exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then
          ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator))
        else
          DropDown(AMode, true);
      end;
  end;
end;

procedure TCompletion.DropDown(const AMode: TCompletionList; const ShowAlways:
  boolean);
var
  ItemCount: Integer;
  P: TPoint;
  Y: Integer;
  PopupWidth, PopupHeight: Integer;
  SysBorderWidth, SysBorderHeight: Integer;
  R: TRect;
  Cancel: boolean;
  Eq: boolean;
begin
  CloseUp(false);
  FMode := AMode;
  with FRAEditor do
  begin
    Cancel := false;
    case FMode of
      cmIdentifers: FRAEditor.DoCompletionIdentifer(Cancel);
      cmTemplates:
        FRAEditor.DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    // Cancel := not Visible and (ItemIndex = -1);
    if Cancel or (FItems.Count = 0) or (((ItemIndex = -1) or Eq) and not
      ShowAlways) then exit;
    FPopupList.Items := FItems;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := true;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then
      FPopupList.Style := lbOwnerDrawFixed
    else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FRAEditor.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FRAEditor.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow + 1);
    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    if ItemCount > FDropDownCount then ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    if (Y + PopupHeight) > Screen.Height then
    begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      if Y < 0 then Y := P.Y;
    end;
    PopupWidth := FDropDownWidth;
    if PopupWidth = 0 then PopupWidth := Width + 2 * SysBorderWidth;
  end;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FPopupList.Visible := true;
end;

procedure TCompletion.MakeItems;
var
  i: integer;
  S: string;
begin
  FItems.Clear;
  case FMode of
    cmIdentifers:
      for i := 0 to FIdentifers.Count - 1 do
        FItems.Add(FIdentifers[i]);
    cmTemplates:
      begin
        with FRAEditor do
          if FLines.Count > CaretY then
            S := GetWordOnPos(FLines[CaretY], CaretX)
          else
            S := '';
        for i := 0 to FTemplates.Count - 1 do
          if ANSIStrLIComp(PChar(FTemplates[i]), PChar(S), Length(S)) = 0 then
            FItems.Add(FTemplates[i]);
        if FItems.Count = 0 then FItems.Assign(FTemplates);
      end;
  end;
end;

procedure TCompletion.FindSelItem(var Eq: boolean);

  function FindFirst(Ss: TSTrings; S: string): integer;
  var
    i: integer;
  begin
    for i := 0 to Ss.Count - 1 do
      if ANSIStrLIComp(PChar(Ss[i]), PChar(S), Length(S)) = 0 then
      begin
        Result := i;
        exit;
      end;
    Result := -1;
  end;

var
  S: string;
begin
  with FRAEditor do
    if FLines.Count > 0 then
      S := GetWordOnPos(FLines[CaretY], CaretX) else
      S := '';
  if Trim(S) = '' then
    ItemIndex := -1
  else
    ItemIndex := FindFirst(FItems, S);
  Eq := (ItemIndex > -1) and Cmp(Trim(SubStr(FItems[ItemIndex], 0, FSeparator)),
    S);
end;

procedure TCompletion.SelectItem;
var
  Cancel: boolean;
  Param: boolean;
begin
  FindSelItem(Param);
  Cancel := not Visible and (ItemIndex = -1);
  case FMode of
    cmIdentifers: FRAEditor.DoCompletionIdentifer(Cancel);
    cmTemplates: FRAEditor.DoCompletionTemplate(Cancel);
  end;
  if Cancel or (GetItems.Count = 0) then CloseUp(false);
end;

procedure TCompletion.CloseUp(const Apply: boolean);
begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := false;
  //  (FPopupList as TBaseEditorCompletionList). HintWindow.ReleaseHandle;
  FVisible := false;
  FTimer.Enabled := false;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifers: ReplaceWord(SubStr(FItems[ItemIndex], 0, FSeparator));
      cmTemplates: ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator));
    end;
end;

procedure TCompletion.OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, false);
end;

procedure TCompletion.SetStrings(index: integer; AValue: TStrings);
begin
  case index of
    0: FIdentifers.Assign(AValue);
    1: FTemplates.Assign(AValue);
  end;
end;

function TCompletion.GetItemIndex: integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TCompletion.SetItemIndex(AValue: integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TCompletion.GetInterval: cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TCompletion.SetInterval(AValue: cardinal);
begin
  FTimer.Interval := AValue;
end;



{**************  TBaseEditorCompletionList  *************}

constructor TBaseEditorCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := false;
  TabStop := False;
  ParentFont := false;
  Parent := Owner as TCustomEditor;
  Ctl3D := false;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;
  //  HintWindow := THintWindow.Create(Self);
end;

destructor TBaseEditorCompletionList.Destroy;
begin
  FTimer.Free;
  //  HintWindow.Free;
  inherited Destroy;
end;

procedure TBaseEditorCompletionList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style {or WS_POPUP} or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TBaseEditorCompletionList.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    Windows.SetParent(Handle, 0);
  //  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0); {??}
end;

procedure TBaseEditorCompletionList.DestroyWnd;
begin
  inherited DestroyWnd;
  //  HintWindow.ReleaseHandle;
end;

procedure TBaseEditorCompletionList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: integer;
begin
  YY := Y;
  F := ItemAtPos(Point(X, Y), true);
  if KeyPressed(VK_LBUTTON) then
  begin
    F := ItemAtPos(Point(X, Y), true);
    if F > -1 then ItemIndex := F;
    FTimer.Enabled := (Y < 0) or (Y > ClientHeight);
    if (Y < -ItemHeight) or (Y > ClientHeight + ItemHeight) then
      FTimer.Interval := 50
    else
      FTimer.Interval := 200;
  end;
  if (F > -1) and not FTimer.Enabled then
  begin
    //Application.CancelHint;
   // Hint := Items[F];
  //  HintWindow.ActivateHint(Bounds(ClientOrigin.X + X, ClientOrigin.Y + Y, 300, ItemHeight), Items[F]);
  end;
end;

procedure TBaseEditorCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  F: integer;
begin
  MouseCapture := true;
  F := ItemAtPos(Point(X, Y), true);
  if F > -1 then ItemIndex := F;
end;

procedure TBaseEditorCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  MouseCapture := false;
  (Owner as TCustomEditor).FCompletion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TBaseEditorCompletionList.OnTimer(Sender: TObject);
begin
  if (YY < 0) then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else if (YY > ClientHeight) then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TBaseEditorCompletionList.WMCancelMode(var Message: TMessage);
begin
  (Owner as TCustomEditor).FCompletion.CloseUp(false);
end;

procedure TBaseEditorCompletionList.CMHintShow(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TBaseEditorCompletionList.DrawItem(Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset, W: Integer;
  S: string;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    Offset := 3;
    with (Owner as TCustomEditor).FCompletion do
      case FMode of
        cmIdentifers:
          Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
            Separator));
        cmTemplates:
          begin
            Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              Separator));
            Canvas.Font.Style := [fsBold];
            S := SubStr(Items[Index], 0, Separator);
            W := Canvas.TextWidth(S);
            Canvas.TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
          end;
      end;
  end;
end;

{$ENDIF RAEDITOR_COMPLETION}

{$ENDIF RAEDITOR_EDITOR}


{ TIEditReader support }

procedure TCustomEditor.ValidateEditBuffer;
begin
  if FPEditBuffer = nil then
  begin
    FEditBuffer := Lines.Text;
    FPEditBuffer := PChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  end;
end; { ValidateEditBuffer }

function TCustomEditor.GetText(Position: Longint; Buffer: PChar;
  Count: Longint): Longint;
begin
  ValidateEditBuffer;
  if Position <= FEditBufferSize then
  begin
    Result := Math.Min(FEditBufferSize - Position, Count);
    Move(FPEditBuffer[Position], Buffer[0], Result);
  end
  else
    Result := 0;
end;

procedure TCustomEditor.Print;
var Line: Cardinal;
    PrintText: System.Text;
begin
  AssignPrn(PrintText);
  try
    Rewrite(PrintText);
    Printer.Canvas.Font.Name := 'Courie New';
    Printer.Canvas.Font.Size := 9;
    for Line := 0 to FLines.Count-1 do
      Writeln(PrintText, FLines[Line]);
  finally
    System.Close(PrintText);
  end;
end;

end.

