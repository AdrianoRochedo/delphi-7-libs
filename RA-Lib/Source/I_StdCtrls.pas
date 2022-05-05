
{
  public and published declarations ready to use with pas2rai2.exe :
  - constructor Create added to all classes;
  - property publics without types completly redeclared
  - destructor Destroy removed
}

unit I_StdCtrls;

interface

uses Messages, Windows, SysUtils, Classes, Controls, Forms, Menus, Graphics;

type

  TGroupBox = class(TCustomGroupBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTextLayout = (tlTop, tlCenter, tlBottom);

  TCustomLabel = class(TGraphicControl)
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property Canvas: TCanvas read FCanvas;
  end;

  TLabel = class(TCustomLabel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);

  TCustomEdit = class(TWinControl)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; virtual;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; virtual;
    procedure SelectAll;
    procedure SetSelTextBuf(Buffer: PChar);
    property Modified: Boolean read GetModified write SetModified;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
  end;

  TEdit = class(TCustomEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth);

  TCustomMemo = class(TCustomEdit)
  public
    constructor Create(AOwner: TComponent); override;
    property Lines: TStrings read FLines write SetLines;
  end;

  TMemo = class(TCustomMemo)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed,
    csOwnerDrawVariable);
  TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked,
    odFocused);

  TDrawItemEvent = procedure(Control: TWinControl; Index: Integer;
    Rect: TRect; State: TOwnerDrawState) of object;

  TMeasureItemEvent = procedure(Control: TWinControl; Index: Integer;
    var Height: Integer) of object;

  TCustomComboBox = class(TWinControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SelectAll;
    property Canvas: TCanvas read FCanvas;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
  end;

  TComboBox = class(TCustomComboBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TButton = class(TButtonControl)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCustomCheckBox = class(TButtonControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCheckBox = class(TCustomCheckBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TRadioButton = class(TButtonControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable);

  TCustomListBox = class(TWinControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;

    property Canvas: TCanvas read FCanvas;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelCount: Integer read GetSelCount;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  end;

  TListBox = class(TCustomListBox)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
    scTrack, scTop, scBottom, scEndScroll);

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
    var ScrollPos: Integer) of object;

  TScrollBar = class(TWinControl)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(APosition, AMin, AMax: Integer);
  end;

  TStaticBorderStyle = (sbsNone, sbsSingle, sbsSunken);

  TCustomStaticText = class(TWinControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TStaticText = class(TCustomStaticText)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

end.
