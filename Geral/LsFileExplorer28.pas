
{Unit LsFileExplorer28.pas -- v2.84  }
{=============================================================================}
{
Archive       :- FileExpl.zip

Components    :- TLsDirTree21, TLsDirTreeCombo28 and
                 TLsFilelistView28

Version       :- 2.84 For Delphi versions 3, 4 and 5

Author        :- Leo D. Shih <ldshih@ecn.ab.ca>

Copyright     :- (C)1998/2000 Leo D. Shih, All rights reserved

Last Modified :- November 19, 2000
------------------------------------------------------------------------------
DISCLAIMER
==========
  These components are currently distributed as Freeware,  and may be
  freely used in Delphi applications, provided that
    (a) the source code is unmodified and
    (b) this notice remains intact.

  There is absolutely no warranty of any kind, either expressed or implied.
  In no event shall the author be liable for any problems or damages that
  may result from the use of these components.

  Suggestions for improvement or bug-fixing will be welcome.
------------------------------------------------------------------------------
ACKNOWLEDGMENTS
===============
  I'm most grateful to those people in the Delphi community who have
  reported bugs, suggested fixings or recommanded improvements. In
  particular, I would like to thank Ales Trtnik, Andreas Roth, Brad Huggins,
  Claude Hunter, Detlef Scheil, Harrie Roaymans, Marcelo Rodrigues,
  Helmut Knaack, Peter Caliban, Steve Pinneo, and Tom Lisjac for their
  professional suggestions.

------------------------------------------------------------------------------
Brief Description
=================

  1. TLsDirTree21
  ---------------
  TLsDirTree21 is a simple but fully functional Directory Treeview with a
  popup FileListDlg (i.e. a simplified File ListView), that
  (a) displays drives/directories in the DirectoryTree and files of the
      selected directory in a popup FileListDlg.  Thus directories can be
      selected from the DirectoryTree, and files can be selected or opened
      in the FileListDlg;
  (b) supports creating, renaming and deleting operation on directories;
  (c) calculates the size of a selected portion of the DirectoryTree.
  (d) connects and disconnects network drives.

  2. TLsDirTreeCombo28
  --------------------
  TLsDirTreeCombo28 is a specialized ComboBox with a dropdown Directory
  TreeView, that displays a hierarchical tree of drives and directories of
  the File System.

  3. TLsFilelistView28 w/TLsFilelistView28PopUp
  ---------------------------------------------
  TLsFilelistView28 is a File ListView component. In addition to its normal
  functions, it can perform various file management tasks, such as cut, copy,
  paste, rename, delete, open, view and send_to operations on files.  It also
  provides statistics on Disk-FreeSpace, Selected-Numbers and Selected-Size as
  those available in the Windows Explorer.

  TLsFilelistView28PopUp is LsFilelistView28's Context Menu, containing
  commands that perform operations on files.

  -> For further information please refer to LsFileExplorer27.txt
                                                                              }
{=============================================================================}


{$IFNDEF VER80}  // If not Delphi 1     //53
  {$DEFINE D2_OR_HIGHER}
  {$IFNDEF VER90}  // If not Delphi 2
    {$DEFINE D3_OR_HIGHER}
    {$IFNDEF VER100} // If not Delphi 3
      {$DEFINE D4_OR_HIGHER}
      {$IFNDEF VER120} // If not Delphi 4
        {$DEFINE D5_OR_HIGHER}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


unit LsFileExplorer28;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CommCtrl, FileCtrl, ShellAPI,
  ClipBrd, Menus,
{$IFNDEF D3_OR_HIGHER}
  ShellObj, OLE2
{$ELSE}
  ShlObj, ActiveX, ComObj
{$ENDIF}
{$IFDEF D4_OR_HIGHER},
  ImgList
{$ENDIF};


type

  TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftArchive, ftNormal);
  TFileType = set of TFileAttr;

  TDTFormat = (df_MMddyyyy, df_MMddyy, df_ddMMyyyy, //70
    df_ddMMyy_GB, df_ddMMyy_DE, df_ddMMyy_IT,
    df_yyyyMMdd, df_yyMMdd, df_Customize);

  // for LsDirTreeCombo28
  TPathChangeEvent =
    procedure(Sender: TObject; SelectedPath: string) of object;  //83
  // for LsDirTree21
  TDirChangeEvent =
    procedure(Sender: TObject; SelectedPath: string) of object;  //83
  TFileChangeEvent =
    procedure(Sender: TObject; SelectedFile: string) of Object;  //83
  // for LsFileListView28
  TSelItemChangeEvent =
    procedure(Sender: TObject; SelectedItem: string) of Object;  //83

  TLsFilelistView28 = class;
  TLsFilelistView28PopUp = class;
  TLsDirTreeCombo28 = class;
  TLsDirTree21PopUp = class;

  {*******************************************}
  {             TLsDirTreeView                }
  {  Drop-down TreeView of TLsDirTreeCombo28  }
  {*******************************************}

  TLsDirTreeView = class(TCustomTreeView)
  private
    TreeViewPath: string;
    FSelectedPath: string;
    FExpandRoot: Boolean;
    FMouseInControl: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message
      WM_LBUTTONDOWN;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Expand(Node: TTreeNode); override;
    procedure LoadDrives;
    procedure MakePath(Node: TTreeNode);
    procedure AddSubs(Path: string; Node: TTreeNode);
    procedure Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  end;

  {*******************************************}
  {            TLsDirTreeCombo28              }
  {*******************************************}

  TLsDirTreeCombo28 = class(TCustomEdit)
    Btn: TSpeedButton;
    Tree: TLsDirTreeView;

  private
    FFileList: TLsFilelistView28;
    FWinDir: string;
    ImageIndex: integer;
    ImagLst: TImageList;
    FCanvas: TControlCanvas;
    FOnPathChange: TPathChangeEvent;  //83

  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEditRect;
    procedure BtnClick(Sender: TObject);
    function GetFileList: TLsFilelistView28;
    procedure SetFileList(Value: TLsFilelistView28);
    function GetBtnGlyph: TBitmap;
    procedure SetBtnGlyph(NewValue: TBitmap);
    function GetTreeHeight: Integer;
    procedure SetTreeHeight(newValue: Integer);
    function GetSelectedPath: string;
    procedure SetSelectedPath(Value: string);
    function GetExpandRoot: Boolean;
    procedure SetExpandRoot(Value: Boolean);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(Value: Boolean);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Keydown(var Key: Word; Shift: TShiftState); override; //52

  public
  { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure SetBounds(Left, Top, Width, Height: Integer); override;
    procedure OpenPath(dPath: string);
    procedure ResetTreeView;

  published
    property SelectedPath: string read GetSelectedPath write SetSelectedPath;
    property FileList: TLsFilelistView28 read GetFileList write SetFileList;
    property ExpandRoot: Boolean read GetExpandRoot write SetExpandRoot
      default True;
    property HideSelection: Boolean read GetHideSelection
      write SetHideSelection default True;
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property Name;
    property Glyph: TBitmap read GetBtnGlyph write SetBtnGlyph;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly; //53+
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property TreeHeight: Integer read GetTreeHeight write SetTreeHeight;
    property Visible;
    property Width;
    property OnPathChange: TPathChangeEvent read FOnPathChange
      write FOnPathChange;  //83
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property BiDiMode;
    property Constraints;
{$ENDIF}
  end;

 {*******************************************}
 {             TLsDirTree21              //60}
 {*******************************************}

  TLsDirTree21 = class(TCustomTreeView)
  private
    FFileList: TLsFilelistView28;
    FSelectedPath: string;
    TreeViewPath: string;
    FPopUpMenu: TLsDirTree21PopUp;
    FPopUpMenuEnabled: Boolean;
    FIsNewFolder: Boolean;
    FSelectedFile: string;  //80^
    DlgForm: TForm;  //82
    FileView: TListView;  //82
    BtnOK: TBitBtn;   //82
    SImgList: TImageList;  //82
    FOnDirChange: TDirChangeEvent;  //83
    FOnFileChange: TFileChangeEvent;  //83

  protected
    procedure CreateWnd; override;
    procedure Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure LoadRoot;
    procedure LoadDrives;
    procedure Loaded; override;
    procedure AddSubs(Path: string; Node: TTreeNode);
    procedure MakePath(Node: TTreeNode);
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;  //84
    procedure SetSelectedPath(Value: string);
    procedure SetSelectedFile(Value: string); //80^

    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;
    procedure SetPopUpMenuEnabled(Value: Boolean);
    procedure SetFileList(Value: TLsFilelistView28);
    function GetPathFromNode(Node: TTreeNode): string;

    procedure OpenFileListDlg(Sender: TObject);  //82
    procedure DlgFormResize(Sender: TObject);  //82
    procedure FileViewDblClick(Sender: TObject);  //80^ //82
    procedure OKBtnClick(Sender: TObject);  //82

  public
  { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure ReLoad;
    procedure OpenPath(dPath: string);
    procedure SHowFolderContents; //80^
    function AddNewNode(ParentNode: TTreeNode; NodeName: string):
      Boolean;
    function DeleteNode(Node: TTreeNode): Boolean;
    function GetTreeSize: integer;
    procedure ConnectNetResource(Sender: TObject);  //82
    procedure DisConnectNetResource(Sender: TObject);  //82

  published
    property FileList: TLsFilelistView28 read FFileList write SetFileList;
    property PopUpMenuEnabled: Boolean read FPopUpMenuEnabled
      write SetPopUpMenuEnabled default True;
    property SelectedPath: string read FSelectedPath write SetSelectedPath;
    property SelectedFile: string read FSelectedFile write SetSelectedFile;  //80^
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Images;
    property Indent;
    property Items;
    property Left;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnDirChange: TDirChangeEvent read FOnDirChange write FOnDirChange;  //83
    property OnFileChange: TFileChangeEvent read FOnFileChange write FOnFileChange;  //83
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property BiDiMode;
    property Constraints;
{$ENDIF}
  end;

  {========== TLsDirTree21PopUp =============}

  TLsDirTree21PopUp = class(TPopupMenu)
  private
    FDirTree: TLsDirTree21; //70
    Bmp1,
      Bmp2,
      Bmp3,
      Bmp4,
      Bmp5,           //70
      Bmp6,           //82
      Bmp7: HBitmap;  //82
  protected
    function AddNewItem(const aCaption: string; aShortCut: TShortCut;
      aChecked, aEnabled: Boolean; aGroup: integer; aOnClick: TNotifyEvent;
      hCtx: word; const aName: string; aBitMap: HBitMap): TMenuItem;

    procedure SetDirTree(Value: TLsDirTree21);
    procedure ItemOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildItems;
    procedure Popup(X, Y: integer); override;
    property DirTree: TLsDirTree21 read FDirTree;
  end;


 {*******************************************}
 {           TLsFilelistView28               }
 {*******************************************}

  TLsFilelistView28 = class(TCustomListView)
  private
  { Private declarations }
    //80 >>
    FColWidth_Name: Integer;
    FColWidth_Size: Integer;
    FColWidth_Type: Integer;
    FColWidth_Mod: Integer;
    FColWidth_Attr: Integer;
    //80 <<
    FDirTreeCombo: TLsDirTreeCombo28;
    FDirTree: TLsDirTree21;
    FDirectory: string;
    FDirectorySize: integer;
    FSelectedItem: string;
    FFileType: TFileType;
    FOpMode: Integer;
    FMask: string;
    FPopupMenu: TLsFilelistView28PopUp;
    FPopUpMenuEnabled: Boolean;
    FSelectedFiles: TStrings;
    FShowFolders: Boolean;
    FSortColumn: integer;
    FSortForward: boolean;
    OldFName: string;
    FDblClickToOpen: Boolean; //70
    FDateFormat: TDTFormat; //70
    FDFormatStr: string; //70
    FTFormatStr: string; //70
    FHideFileExt: Boolean; //70

    Bmp_Up28: HBitMap; //70
    Bmp_Down28: HBitMap; //70
    FBitMap: TBitmap; //70
    FColumnClickEnabled : Boolean;  //70
    FOnItemChange: TSelItemChangeEvent;  //83

    function GetFreeSpace: Integer;
    function GetSelectedNum: Integer;
    function GetSelectedSize: Integer;

    procedure Createimages;
    procedure CompareFiles(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ColumnClick(Sender: TObject; Column: TListColumn);
    procedure SetPopUpMenuEnabled(Value: Boolean);
    procedure SetDblClickToOpen(Value: Boolean);

  protected
  { Protected declarations }
    function AddFiles(FileMask: string; Attr: DWORD): Boolean;
    function CanEdit(Item: TListItem): Boolean; override;
    function GetSelectedItem: string;
    function GetDirectory: string;
    function GetWkgMask(var MaskStr: string): string;

    procedure AddDrives;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure CreateDriveColumns;
    procedure CreateFileColumns;
    procedure DblClick; override;
    procedure Edit(const Item: TLVItem); override;
    procedure Loaded; override;
    procedure Keydown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;  //84

    procedure SetColWidth_Name(Value: Integer);  //80
    procedure SetColWidth_Size(Value: Integer);  //80
    procedure SetColWidth_Type(Value: Integer);  //80
    procedure SetColWidth_Mod(Value: Integer);   //80
    procedure SetColWidth_Attr(Value: Integer);  //80

    procedure SetDirectory(NewDir: string);
    procedure SetDirTreeCombo(Val: TLsDirTreeCombo28);
    procedure SetDirTree(VaL: TLsDirTree21);
    procedure SetSelectedItem(NewItem: string);
    procedure SetFileType(NewFileType: TFileType);
    procedure SetMask(const NewMasks: string);
    procedure SetShowFolders(Value: Boolean);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SendTo(SubItems: integer);
    procedure SendTo2(Path: string);
    procedure SendToPath(DestPath: string);
    procedure SendToDrive(DriveID: string);
    procedure SetHideFileExt(Value: Boolean); //70
    procedure SetDaTeFormat(Value: TDTFormat); //70
    procedure SetTFormatStr(Value: string); //70

  public
  { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CutCopy(Mode: integer);
    procedure DeleteItems;
    procedure FindFile;
    procedure FileAttr;
    procedure NewFolder;
    procedure OneLevelUp;
    procedure OpenItem;
    procedure Paste;
    procedure RenameFile;
    procedure UpdateFileList;
    procedure ViewFile;
    property DirectorySize: integer read FDirectorySize;
    property SelCount;
    property Selected;

  published
  { Published declarations }
    property ColWidth_Name: Integer read FColWidth_Name
      write SetColWidth_Name Default 165;  //80
    property ColWidth_Size: Integer read FColWidth_Size
      write SetColWidth_Size Default 75;  //80
    property ColWidth_Type: Integer read FColWidth_Type
      write SetColWidth_Type Default 95;  //80
    property ColWidth_Mod:  Integer read FColWidth_Mod
      write SetColWidth_Mod Default 115;   //80
    property ColWidth_Attr: Integer read FColWidth_Attr
      write SetColWidth_Attr Default 40;  //80

    property DateFormat: TDTFormat read FDaTeFormat write SetDaTeFormat
      default df_Customize;   //df_MMddyyyy;   //82
    property TimeFormatStr: string read FTFormatStr write SetTFormatStr; //70
    property HideFileExt: Boolean read FHideFileExt write SetHideFileExt
      default False; //70
    property Directory: string read GetDirectory write SetDirectory;
    property DirTreeCombo: TLsDirTreeCombo28 read FDirTreeCombo
      write SetDirTreeCombo;
    property DirTree: TLsDirTree21 read FDirTree
      write SetDirTree; //60
    property DriveFreeSpace: Integer read GetFreeSpace;
    property DblClickToOpen: Boolean read FDblClickToOpen
      write SetDblClickToOpen default True;
    property FileType: TFileType read FFileType write SetFileType default
      [ftNormal];
    property Mask: string read FMask write SetMask;
    property PopUpMenuEnabled: Boolean read FPopUpMenuEnabled
      write SetPopUpMenuEnabled default True;
    property SelectedItem: string read GetSelectedItem write SetSelectedItem;
    property SelectedNumber: Integer read GetSelectedNum;
    property SelectedSize: Integer read GetSelectedSize;
    property ShowFolders: Boolean read FShowFolders write SetShowFolders
      default True;
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Cursor;
    property Dragmode;
    property DragCursor;
    property Enabled;
    property Font;
    property Height;
    property HideSelection default False;
    property Hint;
    property IconOptions;
    property Items;
    property Left;
    property MultiSelect default True;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property ShowColumnHeaders default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property ViewStyle default vsReport;
    property Visible;
    property Width;
    property OnItemChange: TSelItemChangeEvent read FOnItemChange
      write FOnItemChange;  //83
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property HotTrack;
    property RowSelect default False;
{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property HotTrackStyles;
{$ENDIF}
  end;

 { ====== TLsFilelistView28PopUp ====== }

  TLsFilelistView28PopUp = class(TPopupMenu)
  private
    SendToList: TStrings;
    FFileListView: TLsFilelistView28;
    Bmp1, Bmp2,
      Bmp3, Bmp4,
      Bmp5, Bmp6,
      Bmp7, Bmp8,
      Bmp9, Bmp10,
      Bmp11, Bmp12: HBitmap; //70

  protected
    function AddNewItem(const aCaption: string; aShortCut: TShortCut;
      aChecked, aEnabled: Boolean; aGroup: integer; aOnClick: TNotifyEvent;
      hCtx: word; const aName: string; aBitMap: HBitMap): TMenuItem;
    procedure SetFileListView(Value: TLsFilelistView28);
    procedure GetSendToSubMenu;
    procedure ItemOnClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildItems;
    procedure Popup(X, Y: integer); override;
    property FileListView: TLsFilelistView28 read FFileListView;
  end;



{*******************************************}
{           Global Functions                }
{*******************************************}

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: string;
  Dest: string; var Aborted: Boolean): Boolean;
function AddNullToStr(Path: string): string;
function StrContains(Str1, Str2: string): Boolean;
function BrowseForDir(const FormHandle: HWND; var DirPath: string):
  Boolean;
function numpos(a: char; b: string; c: integer): integer;
function getcount(a: char; b: string): integer;
function GetDiskSize(Root: string): LongInt;
function GetFreeDiskSize(Root: string): LongInt;
function DiskinDrive(Drive: Char; ShowMsg: word): Boolean;
function SlashSep(Path, FName: string): string;
function AddSlash(Path: string): string;
function DelSlash(Path: string): string;
function FileTimeToDateTimeStr(FTime: TFileTime; DFormat: string; //70
  TFormat: string): string;
function FileDirExist(FDName: string): Boolean;
function GetNormalIcon(Path: string): integer;
function GetSelectedIcon(Path: string): Integer;
function ConvertSize(FSize: integer; FAttr: string): String;


procedure Register;


implementation

{$R LsFileExplorer28.Res}

const

  InvalidDOSChars = '\*?/="<>|:,;+^';
  DefaultMask = '*.*';
  FileOpMode: array[0..3] of UInt =
    (FO_COPY, FO_DELETE, FO_MOVE, FO_RENAME);

  FileExt: array[0..13] of string = ('.C', '.CPP', '.DPK', '.DPR', '.H',
    '.INF', '.INI', '.PAS', '.PRG', '.TXT', '.DOC', '.RTF', '.WRI', '.BMP');

var
  SelectedDir: String;  //80^
  Drives     : Set of 0..25; //80^


{***********************************************************************}
{                        Global Functions                               }
{***********************************************************************}

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zOperation: array[0..79] of Char;
  zFileName: array[0..79] of Char;
  zParams: array[0..79] of Char;
  zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.Handle,
    StrPCopy(zOperation, Operation),
    StrPCopy(zFileName, FileName),
    StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
  if Result <= 32 then
    MessageDlg('ERROR - Can''t ' + Operation + ' file  ' +
      FileName, mtError, [mbOK], 0);
end; {ExecuteFile}

function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: string;
  Dest: string; var Aborted: Boolean): Boolean;
var
  ipFileOp: TSHFileOpStruct;
begin
  Src := AddNullToStr(Src);
  Dest := AddNullToStr(Dest);
  FillChar(ipFileOp, SizeOf(ipFileOp), 0);
  with ipFileOp do
  begin
    wnd := Handle;
    wFunc := OpMode;
    pFrom := pChar(Src);
    pTo := pChar(Dest);
    fFlags := FOF_ALLOWUNDO; //or FOF_NOCONFIRMATION;
    fAnyOperationsAborted := Aborted;
    hNameMappings := nil;
    lpszProgressTitle := '';
  end;
  Result := SHFileOperation(ipFileOp) = 0;
  if ipFileOp.fAnyOperationsAborted = True then
    Result := False;
end; {DoSHFileOp}

function AddNullToStr(Path: string): string; //70
begin
  if Path = '' then exit;
  if Path[Length(Path)] <> #0 then
    Result := Path + #0
  else
    Result := Path;
end; {AddnullToStr}

function StrContains(Str1, Str2: string): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(Str1) do
    if Pos(Str1[i], Str2) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end; {StringCountains}

function BrowseForDir(const FormHandle: HWND; var DirPath: string):
  Boolean;
var
  pidl: PItemIDList;
  FBrowseInfo: TBrowseInfo;
  Success: Boolean;
  TitleName: string;
  Buffer: array[0..Max_Path] of Char;
begin
  Result := False;
  ZeroMemory(@FBrowseInfo, SizeOf(FBrowseInfo));
  try
    GetMem(FBrowseInfo.pszDisplayName, MAX_PATH);
    FBrowseInfo.HWndOwner := FormHandle;
    TitleName := 'Please specify a directory';
    FBrowseInfo.lpszTitle := PChar(TitleName);
    pidl := SHBrowseForFolder(FBrowseInfo);
    if pidl <> nil then
    begin
      Success := SHGetPathFromIDList(pidl, Buffer);
      if Success then
      begin
        DirPath := Buffer;
        if DirPath[Length(DirPath)] <> '\' then
          DirPath := DirPath + '\';
        result := True;
      end;
      GlobalFreePtr(pidl);
    end;
  finally
    if Assigned(FBrowseInfo.pszDisplayName) then
      FreeMem(FBrowseInfo.pszDisplayName, Max_Path);
  end;
end; {BrowseForDir}

function numpos(a: char; b: string; c: integer): integer;
var
  it: integer;
  az: integer;
begin
  result := maxint;
  if length(b) > 0 then
  begin
    az := 0;
    for it := 1 to length(b) do
      if b[it] = a then
      begin
        inc(az);
        if az = c then
        begin
          result := it;
          exit;
        end;
      end;
  end;
end; {numpos}

function getcount(a: char; b: string): integer;
var
  it: integer;
begin
  result := 0;
  if length(b) > 0 then
    for it := 1 to length(b) do
      if b[it] = a then inc(result);
end; {getcount}

function GetDiskSize(Root: string): LongInt; //81
var
  DrvID: Byte;
begin
  Root := UpperCase(Root);
  DrvID := Ord(Root[1]) - 64;
  Result := DiskSize(DrvID) div 1024;  //in KB
end; {GetDiskSize}

function GetFreeDiskSize(Root: string): LongInt;  //81
var
  DrvID: Byte;
begin
  Root := UpperCase(Root);
  DrvID := Ord(Root[1]) - 64;
  Result := DiskFree(DrvID) div 1024;  //in KB
end; {GetFreeDiskSize}

function DiskinDrive(Drive: Char; ShowMsg: word): Boolean;
var
  ErrorMode: word;
begin
  if Drive in ['a'..'z'] then
    Dec(Drive, $20);
  if not (Drive in ['A'..'Z']) then
    MessageDlg('Not a valid Drive ID', mtError, [mbOK], 0);

  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try
    if DiskSize(Ord(Drive) - $40) = -1 then
    begin
      if ShowMsg > 0 then
      begin
        MessageBeep(MB_IconHand);
        MessageDlg('There is no disk in Drive ' + Drive + #13 +
                   'or Drive ' + Drive + ': is not ready',
                   mtWarning, [mbOK], 0);
      end;
      Result := False
    end
    else
      Result := True;
  finally
    SetErrorMode(ErrorMode);
  end;
end; {DiskinDrive}

function SlashSep(Path, FName: string): string;
begin
  if (Path = '') or (FName = '') then exit;
  Result := AddSlash(Path) + FName;
end; {SlashSep}

function AddSlash(Path: string): string;
begin
  if Path = '' then exit;
  if Path[Length(Path)] <> '\' then
    Result := Path + '\'
  else
    Result := Path;
end; {AddSlash}

function DelSlash(Path: string): string;
begin
  Result := Path;
  if Path <> '' then
    if Path[Length(Path)] = '\' then
      Delete(Result, Length(Path), 1);
end; {DelSlash}

function FileTimeToDateTimeStr(FTime: TFileTime; DFormat,
  TFormat: string): string;  //70
var
  SysTime       : TSystemTime;
  DateTime      : TDateTime;
  LocalFileTime : TFileTime;
begin
  FileTimeToLocalFileTime(Ftime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SysTime);
  DateTime := SystemTimeToDateTime(SysTime);
  Result := FormatDateTime(DFormat + ' ' + TFormat, DateTime);
end; {FileTimeToDateTimeStr}

function FileDirExist(FDName: string): Boolean;
var
  SRec: TSearchRec;
  FName: string;
begin
  FillChar(SRec, SizeOf(TSearchRec), 0);
  Result := FindFirst(AddNullToStr(FDName), faAnyFile or faDirectory, SRec) = 0;
  if Result then
  begin
    FName := ExtractFileName(DelSlash(FDName));
    if (FName[Length(FName)] = #0) then
      FName := Copy(FName, 1, Length(FName) - 1);
    MessageDlg('"' + FName + '" already exists !', mtError, [mbOK], 0);
  end;
  SysUtils.FindClose(SRec);
end; {FileDirExist}

function GetNormalIcon(Path: string): integer;
var
  sfi: TShFileInfo;
begin
  SHGetFileInfo(Pchar(Path), 0, sfi, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Result := sfi.iIcon;
end; {GetNormalIcon}

function GetSelectedIcon(Path: string): Integer;
var
  sfi: TShFileInfo;
begin
  SHGetFileInfo(Pchar(Path), 0, sfi, sizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON);
  Result := sfi.iIcon;
end; {GetSelectedIcon}

function ConvertSize(FSize: integer; FAttr: String): String;
begin
  if (FSize < 1024) and (FSize > 0) then
    FSize := 1024;
  Result := IntToStr(FSize div 1024) + ' KB';  //80
  if (UpperCase(FAttr) = 'DIR') then
    Result := ' ';  //'0';   //80
end; {ConvertSize}

///// End of Global Functions /////


{*************************************************************************}
{                           TLsDirTreeCombo28                             }
{*************************************************************************}

{ ===== LsDirTreeView is the Drop-down TreeView of TLsDirTreeCombo28 ==== }

procedure TLsDirTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    //  ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;  //60
    WindowClass.Style := CS_SAVEBITS;
  end;
end; {CreateParams}

procedure TLsDirTreeView.CreateWnd;
begin
  inherited CreateWnd;
//  Font.Size := 8;                 //82
//  Font.Name := 'MS Sans Serif';   //82
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end; {CreateWnd}

procedure TLsDirTreeView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  ReleaseCapture;
end; {CMMouseEnter}

procedure TLsDirTreeView.CMMouseLeave(var Message: TMessage);
var
  x, y: integer;
  PtPos: TPoint;
begin
  inherited;
  FMouseInControl := False;
  if not visible then
  begin
    x := 0;
    y := 0;
    PtPos := Point(x, y);
    PtPos := ClientToScreen(PtPos);
    SetCaptureControl(ControlAtPos(PtPos, False));
  end
  else
    SetCaptureControl(Self);
end; {CMMouseLeave}

procedure TLsDirTreeView.WMMouseMove(var Message: TWMMouseMove);
var
  TreeHitTest: THitTests;
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    TreeHitTest := GetHitTestInfoAt(Message.XPos, Message.YPos);
    if htOnLabel in TreeHitTest then
      Selected := GetNodeAt(Message.XPos, Message.YPos);
  end;
end; {WMMouseMove}

procedure TLsDirTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  HitTest: THitTests;
  DrvCh: Char;
begin
  inherited;
  case FMouseInControl of
    False:
      begin
        ReleaseCapture;
        Enabled := False;
        Visible := False;
      end;
    True:
      begin
        SendMessage((Owner as TLsDirTreeCombo28).Handle, WM_LBUTTONDOWN, 0, 0);
        HitTest := GetHitTestInfoAt(Message.XPos, Message.YPos);
        if (htOnItem in HitTest) then
        begin
          MakePath(Selected);

          if (Selected = Items[0]) then
            FSelectedPath := 'Drives'
          else
            FSelectedPath := TreeViewPath;

          with (Owner as TLsDirTreeCombo28) do
          begin
            SetSelectedPath(FSelectedPath);
            ImageIndex := Tree.Selected.ImageIndex;
            Text := Selected.Text;
            if Assigned(FOnPathChange) then
              FOnPathChange(Self, FSelectedPath);  //83
          end;

          Enabled := False;
          Visible := False;

          if Selected.Level = 1 then
            if GetDriveType(PChar(FSelectedPath)) = DRIVE_REMOVABLE then
            begin
              DrvCh := FSelectedPath[1];
              if not DiskInDrive(DrvCh, 1) then
                exit;
            end;
        end; {Hittest}
        ReleaseCapture;
      end;
  end; {Case}
end; {WMLButtonDown}

procedure TLsDirTreeView.Expand(Node: TTreeNode);
begin
  Items.BeginUpdate;
  Node.AlphaSort;
  Items.EndUpdate;
  inherited Expand(Node);
end; {Expand}

procedure TLsDirTreeView.LoadDrives;
var
  ADrive: integer;
  DriveLetter: char;
  DriveString: string;
  DrvName: string;
  Sfi: TSHFileInfo;
  Root: TTreenode;
  idRoot: PItemIDList;
begin
  Root := nil;
  Items.BeginUpdate;
  Items.Clear;
  if SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, idRoot) = NOERROR then
    if SHGetFileInfo(PChar(idRoot), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_PIDL
      or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME) <> 0 then
    begin
      Root := items.AddFirst(nil, Sfi.szDisplayName);
      Root.ImageIndex := Sfi.iIcon;
      Root.SelectedIndex := Sfi.iIcon;
    end;
  Integer(Drives) := GetLogicalDrives;
  for ADrive := 0 to 25 do
  begin
    if (ADrive in Drives) then
    begin
      DriveLetter := Chr(ADrive + ord('A'));
      DriveString := DriveLetter + ':\';
      SHGetFileInfo(PChar(DriveString), 0, Sfi, SizeOf(Sfi),
        SHGFI_DISPLAYNAME);
      DrvName := Copy(Sfi.szDisplayName, 1, (Pos('(', Sfi.szDisplayName) - 1));
      with Items do
      begin
        AddChild(Root, ' (' + DriveLetter + ':)  ' + DrvName);
        Items[Count - 1].HasChildren := true;
        Items[Count - 1].ImageIndex := GetNormalIcon(DriveString);
        Items[Count - 1].SelectedIndex := GetSelectedIcon(DriveString);
      end;
    end;
  end;
  Items.EndUpdate;
end; {LoadDrives}

procedure TLsDirTreeView.MakePath(Node: TTreeNode);

  procedure MakeSubPath;
  begin
    if Node.Level = 1 then
      TreeViewPath := Copy(Node.Text, 3, 2) + '\' + TreeViewPath
    else if Node.Level > 1 then
      if TreeViewPath = '' then
        TreeViewPath := Node.Text
      else
        TreeViewPath := Node.Text + '\' + TreeViewPath;
  end; {MakeSubPath}

begin
  TreeViewPath := '';
  MakeSubPath;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    MakeSubPath;
  end;
end; {MakePath}

procedure TLsDirTreeView.AddSubs(Path: string; Node: TTreeNode);
var
  ANode: TTreeNode;
  APath: string;
  hFindFile: THandle;
  Win32FD: TWin32FindData;

  function IsDirectory(dWin32FD: TWin32FindData): Boolean;
  var
    FName: string;
  begin
    FName := StrPas(dWin32FD.cFileName);
    with dWin32FD do
      Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
        FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
  end; {IsDirectory}

  function HasSubs(sPath: string): Boolean;
  var
    sAPath: string;
    shFindFile: THandle;
    sWin32FD: TWin32FindData;
  begin
    Result := False;
    sAPath := sPath;
    sAPath := AddSlash(sAPath);
    shFindFile := FindFirstFile(PChar(sAPath + '*.*'), sWin32FD);
    if shFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        if IsDirectory(sWin32FD) then
        begin
          Result := True;
          Break;
        end;
      until not FindNextFile(shFindFile, sWin32FD);
    finally
      Windows.FindClose(shFindFile);
    end;
  end; {HasSubs}

begin
  APath := Path;
  APath := AddSlash(APath);
  hFindFile := FindFirstFile(PChar(APath + '*.*'), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if IsDirectory(Win32FD) then
      begin
        ANode := Items.AddChild(Node, Win32FD.cFileName);
        ANode.HasChildren := HasSubs(APath + Win32FD.cFileName);
        ANode.ImageIndex := GetNormalIcon(APath + Win32FD.cFileName);
        ANode.SelectedIndex := GetSelectedIcon(APath + Win32FD.cFileName);
      end;
    until not FindNextFile(hFindFile, Win32FD);
  finally
    Windows.FindClose(hFindFile);
  end;
end; {AddSubs}

procedure TLsDirTreeView.Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.GetFirstChild = nil then
  begin
    MakePath(Node);
    Node.HasChildren := false;
    AddSubs(TreeViewPath, Node);
    Node.AlphaSort;
  end;
end; {Expending}

///// End of TLsDirTreeView /////


{ ========================= TLsDirTreeCombo28 ========================== }

procedure TLsDirTreeCombo28.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end; {CreateWnd}

procedure TLsDirTreeCombo28.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.style := Params.style or ES_MULTILINE or WS_CLIPCHILDREN;
end; {CrateParams}

procedure TLsDirTreeCombo28.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

//  Font.Name := 'MS Sans Serif';  //82
//  Font.Size := 8;                //82
  with Btn do
  begin
    Top := 0;
    width := 17;
    Left := 174;
    Height := 17;
    Cursor := crArrow;
    Down := False;
    OnClick := BtnClick;
    Glyph.Handle := LoadBitmap(0, pChar(OBM_COMBO));
    NumGlyphs := 1;
  end; {Btn}

  with Tree do
  begin
    Left := 0;
    Top := 0;
    Width := 0;
    BorderStyle := bsSingle;
    Ctl3D := False;
    LoadDrives;
    OnExpanding := Expanding;
    ExpandRoot := True;
    ReadOnly := True;
    Enabled := False;
    Visible := False;
  end; {Tree}

  if Tree.Selected = nil then
  begin
    ImageIndex := Tree.Items[0].ImageIndex;
    Text := Tree.Items[0].Text;
  end;
end; {CreateWindowHandle}

constructor TLsDirTreeCombo28.Create;
var
  sfi: TShFileInfo;
begin
  inherited Create(AOwner);
  Width := 216;
  Height := 21;
  TabOrder := 0;
  ReadOnly := True; //53+

  FCanvas := TControlCanvas.create;
  FCanvas.Control := self;

  Btn := TSpeedButton.Create(Self);
  Btn.Parent := Self;

  Tree := TLsDirTreeView.Create(Self);
  with Tree do
  begin
    Parent := Self;
    Top := Self.Top + Self.Height;
    Height := 240;
  end;

  ImagLst := TImageList.Create(Self);
  try
    SetLength(FWinDir, MAX_PATH);
    Setlength(FWinDir, GetWindowsDirectory(PChar(FWinDir), MAX_PATH));
    FWinDir := AddSlash(FWinDir);
    ImagLst.Handle := SHGetFileInfo(PChar(FWinDir), 0, sfi, sizeOf(sfi),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    ImagLst.ShareImages := True;
    ImagLst.BlendColor := clHighLight;
  finally
    Tree.Images := ImagLst;
  end;
end; {Create}

destructor TLsDirTreeCombo28.destroy;
var
  i: integer;
begin
  for i := Tree.Items.Count - 1 downto 0 do //70
    Tree.Items[i].Free;
  ImagLst.Free;

  inherited Destroy;
end; {destroy}

procedure TLsDirTreeCombo28.SetEditRect;
var
  Loc: TRect;
begin
  if (ImageIndex >= 0) and (ImageIndex < Tree.Images.Count) then
  begin
    SetRect(Loc, 23, 0, ClientWidth - Btn.Width - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  end
  else
  begin
    SetRect(Loc, 0, 0, ClientWidth - Btn.Width - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  end;
end; {SetEditRect}

procedure TLsDirTreeCombo28.BtnClick(Sender: TObject);
var
  CP, SP: TPoint;
begin
  CP.X := Left;
  CP.Y := Top + Height;
  SP := Parent.ClientToScreen(CP);
  with Tree do
  begin
    Left := SP.X;
    Top := SP.Y;
    Width := Self.Width;
    if Enabled = False then
    begin
      Enabled := True;
      Visible := True;
      BringToFront; //60
      SetCaptureControl(Tree);
    end
    else
    begin
      SendToBack; //60
      Enabled := False;
      Visible := False;
      ReleaseCapture;
    end;
  end;
end; {BtnClick}

procedure TLsDirTreeCombo28.SetBounds(Left, Top, Width, Height: Integer);
begin
  case Parent <> nil of
    True:
      begin
        inherited SetBounds(Left, Top, Width, Height);
        with Btn do
        begin
          Left := Self.Width - Btn.Width - 4;
          Height := Self.Height - 4;
        end;
        SetEditRect;
      end;
    False: inherited SetBounds(Left, Top, Width, Height);
  end;
end; {SetBounds}

procedure TLsDirTreeCombo28.OpenPath(dPath: string);
var
  CurItem: TTreeNode;
  count: Integer;
  TempPath: string;
  CurPath: string;
  FullPath: string; //51
begin
  if (dPath = '') or (Length(dPath) = 1) then exit; //53
  if not DirectoryExists(dPath) then exit; //53
  dPath := AddSlash(dPath); //51
  FullPath := dPath;
  with Tree do
  begin
    Items.BeginUpdate;
    CurItem := Items.GetFirstNode;
    if CurItem.Expanded then
      CurItem.Collapse(True);
    while Pos('\', dPath) > 0 do
    begin
      count := Pos('\', dPath);
      tempPath := Copy(dPath, 1, count);
      dPath := Copy(dPath, count + 1, Length(dPath));
      CurItem := CurItem.getFirstChild;
      while CurItem <> nil do
      begin
        if CurItem.Level = 1 then
          CurPath := Copy(CurItem.Text, 3, 2) + '\'
        else if CurItem.Level > 1 then
          CurPath := CurItem.Text + '\';
        if AnsiCompareText(CurPath, TempPath) = 0 then //51
        begin
          CurItem.Selected := True;
          CurItem.Expand(False);
          Break;
        end;
        CurItem := CurItem.GetNext;
        if CurItem = nil then exit;
      end;
    end;
    Items.EndUpdate;
  end;
  ImageIndex := Tree.Selected.ImageIndex;
  Text := CurItem.Text;
  if AnsiCompareText(Tree.FSelectedPath, FullPath) <> 0 then //51
    Tree.FSelectedPath := FullPath; //51
  if Assigned(FOnPathChange) then
    FOnPathChange(Self, Tree.FSelectedPath);  //84
end; {OpenPath}

procedure TLsDirTreeCombo28.ResetTreeView;
begin
  with Tree do
  begin
    Items.BeginUpdate;
    try
      Selected := nil;
      Items.Clear;
      LoadDrives;
      OnExpanding := Expanding;
    finally
      Items.EndUpdate;
    end;
  end;
end; {ResetTreeView}

procedure TLsDirTreeCombo28.WMPaint(var Message: TWMPaint);
var
  Img: TCustomImageList;
  YPos: integer;
begin
  inherited PaintHandler(Message);
  Img := Tree.Images;
  if (ImageIndex >= 0) and (ImageIndex < Tree.Images.Count) then
  begin
    YPos := ClientHeight div 2 - 8;
    Img.Draw(FCanvas, 5, YPos, ImageIndex);
  end;
end; {WMPaint}

function TLsDirTreeCombo28.GetSelectedPath: string;
begin
  GetSelectedPath := Tree.FSelectedPath;
end; {GetSelectedPath}

procedure TLsDirTreeCombo28.SetSelectedPath(Value: string);
begin
  if AnsiCompareText(Tree.FSelectedPath, Value) <> 0 then
    Tree.FSelectedPath := Value;
  if FFileList <> nil then
    FFileList.Directory := Tree.FSelectedPath;
end; {SetSelectedPath}

function TLsDirTreeCombo28.GetFileList: TLsFilelistView28;
begin
  GetFileList := FFileList;
end; {GetFileList}

procedure TLsDirTreeCombo28.SetFileList(Value: TLsFilelistView28);
begin
  if FFileList <> Value then
    FFileList := Value;
  if (FFileList <> nil) and (Tree.FSelectedPath <> '') then
  begin
    FFileList.Directory := Tree.FSelectedPath;
    FFileList.UpdateFileList;
  end;
end; {SetFileList}

function TLsDirTreeCombo28.GetExpandRoot: Boolean;
begin
  GetExpandRoot := Tree.FExpandRoot;
end; {GetExpandRoot}

procedure TLsDirTreeCombo28.SetExpandRoot(Value: Boolean);
begin
  Tree.FExpandRoot := Value;
  with Tree do
    if FExpandRoot and Assigned(Items[0]) then
      Items[0].Expand(False);
end; {SetExpandRoot}

procedure TLsDirTreeCombo28.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFileList) then
    FFileList := nil;
end; {Notification}

function TLsDirTreeCombo28.GetBtnGlyph: TBitmap;
begin
  GetBtnGlyph := Btn.Glyph;
end; {GetBtnGlyph}

procedure TLsDirTreeCombo28.SetBtnGlyph(NewValue: TBitmap);
begin
  if NewValue <> nil then
    Btn.Glyph.Assign(NewValue);
end; {SetBtnGlyph}

function TLsDirTreeCombo28.GetTreeHeight: integer;
begin
  GetTreeHeight := Tree.Height;
end; {GetTreeHeight}

procedure TLsDirTreeCombo28.SetTreeHeight(newValue: Integer);
begin
  Tree.Height := newValue;
end; {SetTreeHeight}

function TLsDirTreeCombo28.GetHideSelection: Boolean;
begin
  GetHideSelection := Tree.HideSelection;
end; {GetHideSelection}

procedure TLsDirTreeCombo28.SetHideSelection(Value: Boolean);
begin
  Tree.HideSelection := Value;
end; {SetHideSelection}


procedure TLsDirTreeCombo28.Keydown(var Key: Word; Shift: TShiftState);
var
  CurItem: TTreeNode;
begin
  case Key of
    VK_RIGHT: Tree.Selected.Expand(False); //52 Expand Selected Node
    VK_LEFT: //52 Collapse Selected Node
      begin
        with Tree do
        begin
          if Selected.Expanded then
            Selected.Collapse(True);
          if (Selected.getPrevSibling = Items[0]) or
            (Selected.GetPrevVisible = Items[0]) or
            (Selected = Items[0]) then exit;
          CurItem := Selected.GetPrevVisible;
          CurItem.Selected := True;
        end;
      end;
    VK_F4: //52 Activate LsDirTreeView
      begin
        BtnClick(Self);
        if Tree.Visible then
          Tree.SetFocus;
      end;
    VK_ESCAPE: //52 Close LsDirTreeView
      begin
        if Tree.Enabled = True then
        begin
          Tree.Enabled := False;
          Tree.Visible := False;
        end;
      end;
    VK_DOWN: //52 Move to NextChild or nextVisible
      begin
        with Tree do
        begin
          if (Selected.GetNext = nil) or
            (Selected.getNextVisible = nil) then exit; //53
          if Selected.Expanded then
            CurItem := Selected.GetNext
          else
            CurItem := Selected.getNextVisible;
          CurItem.Selected := True;
        end;
      end;
    VK_UP: //52 Move to prevChild or prevVisible
      begin
        with Tree do
        begin
          if Selected = Items[0] then exit;
          CurItem := Selected.GetPrevVisible;
          CurItem.Selected := True;
        end;
      end;
    VK_RETURN: //52 set the SelectedPath
      begin
        with Tree do
        begin
          MakePath(selected);
          if (Selected = Items[0]) then
            FSelectedPath := 'Drives'
          else
            FSelectedPath := TreeViewPath;
          SetSelectedPath(FSelectedPath);

          if Assigned(FOnPathChange) then
            FOnPathChange(Self, FSelectedPath);  //83

          SendToBack;
          Enabled := False;
          Visible := False;
          if Selected.Level = 1 then
            if GetDriveType(PChar(FSelectedPath)) = DRIVE_REMOVABLE then
            begin
              if not DiskInDrive(FSelectedPath[1], 1) then
                exit;
            end;
        end;
        Text := Tree.selected.Text;
      end; //VK_Return
  end;
  inherited KeyDown(Key, Shift);
end; {Keydown}

///// End of TLsDirTreeCombo28 /////


{************************************************************************}
{                         TLsDirTree21                              //70 }
{************************************************************************}

procedure TLsDirTree21.CreateWnd;
begin
  inherited CreateWnd;

  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  if not (csLoading in ComponentState) then
    LoadRoot;
end; {CreateWnd}


constructor TLsDirTree21.Create;
var
  sfi: TShFileInfo;
  hImgLst: Uint;
begin
  inherited Create(AOwner);
//  Font.size := 8;                //80
//  Font.name := 'MS Sans Serif';  //80

  Width := 180;
  Height := 120;

  Images := TImageList.Create(Self);
  hImgLst := SHGetFileInfo('', 0,
    sfi, SizeOf(sfi),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if hImgLst <> 0 then
  begin
    Images.Handle := hImgLst;
    Images.ShareImages := True;
  end;

  FPopupMenu := TLsDirTree21PopUp.Create(Self);
  FPopupMenu.BuildItems;
  FPopupMenu.AutoPopup := True;
  FPopupMenu.FDirTree := Self;
  PopupMenu := FPopupMenu;
  FPopUpMenuEnabled := True;

  OnExpanding := Expanding;
  ReadOnly := False;
  SortType := stNone;
  HideSelection := False;
  FIsNewFolder := False;
end; {Create}

destructor TLsDirTree21.destroy;
var
  i: integer;
begin
  for i := Items.Count - 1 downto 0 do
    Items[i].Free;
  Images.Free;
  inherited Destroy;
end; {Destroy}

procedure TLsDirTree21.LoadRoot;
var
  Sfi: TSHFileInfo;
  Root: TTreenode;
  idRoot: PItemIDList;
begin
  Items.BeginUpdate;
  Items.Clear;
  if SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, idRoot) = NOERROR then
    if SHGetFileInfo(PChar(idRoot), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_PIDL or
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME) <> 0 then
    begin
      Root := items.AddFirst(nil, Sfi.szDisplayName);
      Root.ImageIndex := Sfi.iIcon;
      Root.SelectedIndex := Sfi.iIcon;
    end;
  Items.EndUpdate;
end; {LoadRoot}

procedure TLsDirTree21.LoadDrives;
var
  ADrive: integer;
  DriveLetter: char;
  DriveString: string;
  DrvName: string;
  Sfi: TSHFileInfo;
begin
  Items.BeginUpdate;
  Integer(Drives) := GetLogicalDrives;
  for ADrive := 0 to 25 do
  begin
    if ADrive in Drives then
    begin
      DriveLetter := Chr(ADrive + ord('A'));
      DriveString := DriveLetter + ':\';
      SHGetFileInfo(PChar(DriveString), 0, Sfi, SizeOf(Sfi),
        SHGFI_DISPLAYNAME);
      DrvName := Copy(Sfi.szDisplayName, 1, (Pos('(', Sfi.szDisplayName) - 1));
      with Items do
      begin
        AddChild(Items[0], ' (' + DriveLetter + ':)  ' + DrvName);
        ShowButtons := True;
        Items[Count - 1].HasChildren := true;
        Items[Count - 1].ImageIndex := GetNormalIcon(DriveString);
        Items[Count - 1].SelectedIndex := GetSelectedIcon(DriveString);
      end;
    end;
  end;
  Items.EndUpdate;
end; {LoadDrives}

procedure TLsDirTree21.MakePath(Node: TTreeNode);

  procedure MakeSubPath;
  begin
    if Node.Level = 1 then
      TreeViewPath := Copy(Node.Text, 3, 2) + '\' + TreeViewPath
    else if Node.Level > 1 then
      if TreeViewPath = '' then
        TreeViewPath := Node.Text
      else
        TreeViewPath := Node.Text + '\' + TreeViewPath;
  end; {MakeSubPath}

begin
  TreeViewPath := '';
  MakeSubPath;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    MakeSubPath;
  end;
end; {MakePath}

procedure TLsDirTree21.AddSubs(Path: string; Node: TTreeNode);
var
  ANode: TTreeNode;
  APath: string;
  hFindFile: THandle;
  Win32FD: TWin32FindData;

  function IsDirectory(dWin32FD: TWin32FindData): Boolean;
  var
    FName: string;
  begin
    FName := StrPas(dWin32FD.cFileName);
    with dWin32FD do
      Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
        FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
  end; {IsDirectory}

  function HasSubs(sPath: string): Boolean;
  var
    sAPath: string;
    shFindFile: THandle;
    sWin32FD: TWin32FindData;
  begin
    Result := False;
    sAPath := sPath;
    sAPath := AddSlash(sAPath);
    shFindFile := FindFirstFile(PChar(sAPath + '*.*'), sWin32FD);
    if shFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        if IsDirectory(sWin32FD) then
        begin
          Result := True;
          Break;
        end;
      until not FindNextFile(shFindFile, sWin32FD);
    finally
      Windows.FindClose(shFindFile);
    end;
  end; {HasSubs}

begin
  APath := Path;
  APath := AddSlash(APath);
  hFindFile := FindFirstFile(PChar(APath + '*.*'), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if IsDirectory(Win32FD) then
      begin
        ANode := Items.AddChild(Node, Win32FD.cFileName);
        ANode.HasChildren := HasSubs(APath + Win32FD.cFileName);
        ANode.ImageIndex := GetNormalIcon(APath + Win32FD.cFileName);
        ANode.SelectedIndex := GetSelectedIcon(APath + Win32FD.cFileName);
      end;
    until not FindNextFile(hFindFile, Win32FD);
  finally
    Windows.FindClose(hFindFile);
  end;
end; {addSubs}

procedure TLsDirTree21.ReLoad;
begin
  Items.BeginUpdate;
  Items.Clear;
  LoadRoot;
  LoadDrives;
  Items.EndUpdate;
end; {Reload}

procedure TLsDirTree21.Loaded;  //70
begin
  inherited Loaded;
  Reload;
  if Items.GetFirstNode <> nil then
    Items.GetFirstNode.Expand(False);
end; {Loaded}

procedure TLsDirTree21.Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.GetFirstChild = nil then
  begin
    MakePath(Node);
    Node.HasChildren := false;
    AddSubs(TreeViewPath, Node);
    Node.AlphaSort;
  end;
end; {Expanding}

procedure TLsDirTree21.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vHitTest: THitTests;
  DrvChar: Char;
begin
  TreeViewPath := '';
  if Button = mbRight then exit;  //81
  if Button = mbLeft then
  begin
    vHitTest := GetHitTestInfoAt(X, Y);
    if (htOnItem in vHitTest) or (htOnIcon in vHitTest) then
    begin
      if (Selected.Level = 0) and (Items[0].getFirstChild = nil) then
        LoadDrives
      else
        MakePath(Selected);

      if Selected = Items[0] then
        FSelectedPath := 'Drives'
      else
        FSelectedPath := TreeViewPath;

      if Selected.Level = 1 then
        if GetDriveType(PChar(FSelectedPath)) in
          [DRIVE_REMOVABLE, DRIVE_CDROM] then
        begin
          DrvChar := FSelectedPath[1];
          if not DiskInDrive(DrvChar, 1) then
            exit;
        end;

      if Assigned(FOnDirChange) then        //83
        FOnDirChange(Self, FSelectedPath);
      if Assigned(FFileList) and (FSelectedPath <> '') then
        FFileList.Directory := FSelectedPath;
    end;
  end;
end; {MouseDown}

procedure TLsDirTree21.SetSelectedPath(Value: string);
begin
  if AnsiCompareText(Value, FSelectedPath) = 0 then
    exit;
  FSelectedPath := Value;
end; {SetSelectedPath}

procedure TLsDirTree21.SetSelectedFile(Value: string); //80^
begin
  if AnsiCompareText(Value, FSelectedFile) = 0 then exit;
    FSelectedFile := Value;
end;  {SetSelectedFile}


procedure TLsDirTree21.OpenPath(dPath: string);
var
  CurNode: TTreeNode;
  count: Integer;
  TempPath: string;
  CurPath: string;
  FullPath: string;
begin
  if (dPath = '') or (Length(dPath) = 1) then exit;
  if not DirectoryExists(dPath) then exit;
  dPath := AddSlash(dPath);
  FullPath := dPath;
  Items.BeginUpdate;
  CurNode := Items.GetFirstNode;  //70
  if CurNode.getFirstChild = nil then
    LoadDrives;
  if CurNode.Expanded then
    CurNode.Collapse(True);
  CurNode := Items.GetFirstNode;
  while Pos('\', dPath) > 0 do
  begin
    count := Pos('\', dPath);
    tempPath := Copy(dPath, 1, count);
    dPath := Copy(dPath, count + 1, Length(dPath));
    CurNode := CurNode.getFirstChild;

    while CurNode <> nil do
    begin
      if CurNode.Level = 1 then
        CurPath := Copy(CurNode.Text, 3, 2) + '\'
      else if CurNode.Level > 1 then
        CurPath := CurNode.Text + '\';
      if AnsiCompareText(CurPath, tempPath) = 0 then
      begin
        CurNode.Selected := True;
        CurNode.Expand(False);
        Break;
      end;
      CurNode := CurNode.GetNext;
      if CurNode = nil then exit;
    end;
  end;
  Items.EndUpdate;
  if AnsiCompareText(FSelectedPath, FullPath) <> 0 then
  begin
    FullPath := AddSlash(FullPath);
    FSelectedPath := FullPath;
  end;
  if Assigned(FOnDirChange) then
    FOnDirChange(Self, FSelectedPath);  //83
end; {OpenPath}

procedure TLsDirTree21.KeyUp(var Key: Word; Shift: TShiftState);  //84
var
  DrvChar: Char;
begin
  if (Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_LEFT) or (Key = VK_RIGHT) then
  begin
    inherited KeyUp(Key, Shift);

    if selected = nil then exit;
    if (Selected.Level = 0) and (Items[0].getFirstChild = nil) then
      LoadDrives
    else
      MakePath(Selected);

    if (Selected.Level = 0) then
      FSelectedPath := 'Drives'
    else
      FSelectedPath := TreeViewPath;

    if Selected.Level = 1 then
      if GetDriveType(PChar(FSelectedPath)) in
        [DRIVE_REMOVABLE, DRIVE_CDROM] then
      begin
        DrvChar := FSelectedPath[1];
        if not DiskInDrive(DrvChar, 1) then
          exit;
      end;

    if Assigned(FOnDirChange) then
      FOnDirChange(Self, FSelectedPath);  //83
    if Assigned(FFileList) and (FSelectedPath <> '') then
      FFileList.Directory := FSelectedPath;
  end;
end; {KeyUp}  //84

function TLsDirTree21.GetPathFromNode(Node: TTreeNode): string;
begin
  Result := '';
  if Node = nil then exit;
  if Assigned(Node) then
  begin
    MakePath(Node);
    Result := TreeViewPath;
  end;
end; {GetPathFromNode}

function TLsDirTree21.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := False;
  if (Assigned(Node.Parent)) and (Node.Level > 1) and
    (not ReadOnly) then
    Result := inherited CanEdit(Node);
end; {CanEdit}

procedure TLsDirTree21.Edit(const Item: TTVItem);
var
  OldDirName: string;
  NewDirName: string;
  Aborted: Boolean;
  OldCur: TCursor;
  Rslt: Boolean;
  SelNode: TTreeNode;
  PrevNode: TTreeNode;

  function GetNodeFromItem(Item: TTVItem): TTreeNode;
  begin
    with Item do
      if (State and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
  end; {GetNodeFromItem}

begin
  SelNode := GetNodeFromItem(Item);
  PrevNode := SelNode.Parent;
  if not Assigned(SelNode) then exit;
  if (SelNode = Items[0]) or (SelNode.Level = 1) then
    exit;

  if (Length(Item.pszText) = 0)
    or (StrContains(InvalidDosChars, Item.pszText)) then
  begin
    MessageBeep(MB_ICONHAND);
    if (Length(Item.pszText) > 0) then
      MessageDlg('Error - Invalid Directory Name' + #13 +
        Item.pszText, mtError, [mbOK], 0);
    Exit;
  end;
  if SelNode <> nil then
    OldDirName := GetPathFromNode(SelNode);
  if OldDirName = '' then exit;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Aborted := False;
    OldDirName := DelSlash(OldDirName);
    NewDirName := ExtractFilePath(OldDirName) + Item.pszText;

    if not FIsNewFolder then
    begin
      if MessageDlg('Do you want to rename the selected folder' + #13 + #13 +
        '    From  : ' + ExtractFileName(OldDirName) + #13 +
        '    To     : ' + ExtractFileName(NewDirName), mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then
        exit;
    end;

    Rslt := DoSHFileOp(Parent.Handle, FO_RENAME, OldDirName,
      NewDirName, Aborted);

    if Rslt then
    begin
      inherited Edit(Item);
      Selected := PrevNode;
      if Assigned(FFileList) then
      begin
        FFileList.Directory := GetPathFromNode(Selected);
      end;
    end;
  finally
    Screen.Cursor := OldCur;
    FIsNewFolder := False;
  end;
end; {Edit}

function TLsDirTree21.AddNewNode(ParentNode: TTreeNode;
  NodeName: string): Boolean;
var
  Path: string;
  Dir: string;
  NewNode: TTreeNode;
begin
  Result := False;
  if ParentNode = nil then
    ParentNode := Selected;
  if ParentNode.Level = 0 then
  begin
    MessageDlg('Can''t add drives', mtError, [mbOK], 0);
    exit;
  end;

  if NodeName = '' then
  begin
    NodeName := 'New Folder';
    FIsNewFolder := True;
  end;
  try
    Path := GetPathFromNode(ParentNode);
    if Path = '' then exit;
    Path := AddSlash(Path);
    Dir := Path + NodeName;
//    Dir := AddNullToStr(Dir);  //80

    if StrContains(InvalidDosChars, NodeName) then
    begin
      MessageBeep(MB_ICONHAND);
      MessageDlg('Folder Name contains invalid characters', mtError, [mbOK], 0);
      exit;
    end;
    Items.BeginUpdate;

    Result := CreateDirectory(PChar(Dir), nil);

    if Result then
    begin
      ReLoad;
      Dir := AddSlash(Dir);
      OpenPath(Dir);
      NewNode := Selected;
      if (NewNode <> nil) and (NodeName = 'New Folder') then
        NewNode.EditText;
    end;
  finally
    Items.EndUpdate;
  end;
end; {AddNewNode}


function TLsDirTree21.DeleteNode(Node: TTreeNode): Boolean;
var
  DelDir: string;
  DelPath: string;
  PrevNode: TTreeNode;
  oldCur: TCursor;
  Aborted: Boolean;
begin
  Result := False;
  Aborted := True;
  PrevNode := Node.Parent; //.GetPrevVisible;
  if (Assigned(Node)) and (Node.Level > 1) then
  begin
    oldCur := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    if Selected <> nil then
      DelDir := GetPathFromNode(Selected);
    if DelDir = '' then exit;
    if not DirectoryExists(Deldir) then
    begin
      MessageBeep(MB_ICONHAND);
      MessageDlg(DelDir + 'not found', mtError, [mbOK], 0);
      Screen.Cursor := oldCur;
      exit;
    end;
    DelDir := DelSlash(Deldir);
    DelPath := ExtractFilePath(DelDir);
 //   MessageBeep(MB_ICONHAND);
    Result := DoSHFileOp(Parent.Handle, FO_DELETE, DelDir, '', Aborted);
    if Result then
    begin
      if Assigned(PrevNode) then
        Selected := PrevNode;
      Node.Delete;
      if Assigned(FFileList) then
        FFileList.Directory := GetPathFromNode(Selected);
    end;
    Screen.Cursor := oldCur;
  end;
end; {DeleteNode}

procedure TLsDirTree21.SetFileList(Value: TLsFilelistView28);
begin
  if FFileList <> Value then
    FFileList := Value;
  if (Assigned(FFileList)) and (FSelectedPath <> '') then
    FFileList.Directory := FSelectedPath;
end; {SetFileList}

procedure TLsDirTree21.SetPopUpMenuEnabled(Value: Boolean);
begin
  if Value <> FPopUpMenuEnabled then
  begin
    FPopUpMenuEnabled := Value;
    if FPopUpMenuEnabled then
      PopupMenu := FPopupMenu
    else
      PopupMenu := nil;
  end;
end; {SetPopUpMenuEnabled}

function TLsDirTree21.GetTreeSize: integer;
var
  CurNode,
   SubNode  : TTreeNode;
  CurPath,
   SubPath,
   CurDir,
   DispSize,
   DispName : string;
  FTreeSize : Integer;
  TreeSize  : double;
  OldCur    : TCurSor;

  function GetDirSize(Path: String): integer;
  var
    SRec: TSearchRec;
    FSize: integer;
  begin
    FSize := 0;
    FillChar(SRec, SizeOf(TSearchRec), 0);
    Path := AddSlash(Path);
    if FindFirst(Path + '*.*', faAnyFile, SRec) = 0 then
    begin
      Repeat
        if (SRec.Attr and faDirectory) = 0 then
          FSize := FSize + SRec.Size;
      Until FindNext(SRec) <> 0;
      FindClose(SRec);
    end;
    Result := FSize;
  end;

  function IsSubDir(RefPath, SubPath: string): Boolean;
  var
    RPLen : integer;
    TempPath: String;
  begin
    RPLen := Length(RefPath);
    TempPath := Copy(SubPath, 1, RPLen);
    if AnsiCompareText(RefPath, TempPath) = 0 then
      Result := True
    else
      Result := False;
  end;

begin
  CurPath := '';
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    CurNode := Selected;
    CurPath := GetPathFromNode(CurNode);
    CurDir := ExtractFileName(CurPath);
    FTreeSize := GetDirSize(CurPath);
    if (CurNode.Level = 1) then
      DispName := 'Drive '+CurPath
    else if (CurNode.Level > 1) then
      DispName := 'Folder "'+CurDir + '"';

    CurNode.Expand(False);
    SubNode := CurNode.GetFirstChild;

    while (SubNode <> nil) do
    begin
      SubNode.Selected := True;
      SubNode.Expand(False);
      SubPath := GetPathFromNode(SubNode);
//      ShowMessage(SubPath);  //for debugging
      if not IsSubDir(CurPath, SubPath) then
        Break;
      FTreeSize := FTreeSize + GetDirSize(SubPath);
      SubNode := SubNode.getNext;
    end;
  finally
    OpenPath(CurPath);
    Items.EndUpdate;
    Screen.CurSor := OldCur;
  end;

  Result := FTreeSize;

  if (FTreeSize > 0) and (FTreeSize < 1024) then
    TreeSize := 1.0
  else
    TreeSize := FTreeSize / 1024;
  if TreeSize <= 99999 then
    DispSize := Format('%.2n KB', [TreeSize])
  else
    DispSize := Format('%.2n MB', [TreeSize/1024]);

  MessageDlg('=== TREE SIZE ===' + #13 + #13 +
             DispName + #13 +
             'and all its Sub-Folders  =  ' + DispSize,
             mtInformation, [mbOK], 0);
end;  {GetTreeSize}


procedure TLsDirTree21.ShowFolderContents; //80^
var
  CurFolder: string;
begin
  SelectedDir := '';
  SelectedDir := Self.FSelectedPath;
  if Selected.Level = 1 then
    CurFolder := '" ' + SelectedDir[1] + ':\ "'
  else if Selected.Level > 1 then
    CurFolder := '" \' + ExtractFileName(DelSlash(SelectedDir)) + ' "';
  if Items.GetFirstNode <> nil then
    Items.GetFirstNode.Collapse(True);
  OpenFileListDlg(Self);
  with DlgForm do
  begin
    Caption := 'Files in ' + CurFolder;
    Show;
  end;
end;  {ShowFolderContents}

procedure TLsDirTree21.ConnectNetResource(Sender: TObject);  //82
var
  ADrv: integer;
  DrvLtr: Char;
  CurNode: TTreeNode;
begin
  DrvLtr := ' ';
  Integer(Drives) := GetLogicalDrives;
  for ADrv := 0 to 25 do
    if ADrv in Drives then
      DrvLtr := Chr(ADrv + Ord('B'));
  if WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR then
  begin
    Reload;
    Items[0].Expand(False);
    CurNode := Items[0].GetLastChild;
    CurNode.Selected := True;
    FSelectedPath := DrvLtr + ':\';
    if Assigned(FFileList) then
      FFilelist.Directory := FSelectedPath;
  end;
end; //ConnecttoNetResource   //82


procedure TLsDirTree21.DisConnectNetResource(Sender: TObject);  //82
begin
  WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK);
  Reload;
  Items[0].Expand(False);
  Items[0].Selected := True;
  SelectedPath := 'Drives';
  if Assigned(FFileList) then
  begin
    FFileList.Directory := SelectedPath;
    FFileList.UpdateFileList;
  end;
end;  //DisConnectNetResource   //82

//82# >>
procedure TLsDirTree21.OpenFileListDlg(Sender: TObject);  //82#
var
  FFont: TFont;
  Panel1,
    Panel2: TPanel;

  procedure CreateImages;  //80^
  var
    sfi: TSHFileInfo;
    hImgList: Uint;
  begin
    SImgList := TImageList.Create(Self);
    hImgList := SHGetFileInfo('', 0, sfi, SizeOf(sfi), SHGFI_SYSICONINDEX or
                SHGFI_SMALLICON);
    if hImgList <> 0 then
    begin
      SImgList.Handle := hImgList;
      SImgList.ShareImages := True;
    end;
    FileView.SmallImages := SImgList;
  end;  //CreateImages

  procedure BuildFileList; //80 //82
  var
    CurDir,
    FName,
      FileName: string;
    sfi: TSHFileInfo;
    hFindFile: THandle;
    Win32FD: TWin32FindData;
    OldCur: TCursor;
  begin
    OldCur := Screen.Cursor;
    CurDir := AddSlash(SelectedDir);
    hFindFile := FindFirstFile(PChar(CurDir + '*.*'), Win32FD);
    if hFindFile <> INVALID_HANDLE_VALUE then
    try
      Screen.Cursor := crHourGlass;
      FileView.Items.BeginUpdate;
      repeat
        with Win32FD do
        begin
          if (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0) then
            Continue;
          FName := StrPas(Win32FD.cFileName);
          FileName := CurDir + FName;
          SHGetFileInfo(PChar(FileName), 0, sfi, SizeOf(sfi),
            SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME);

          with FileView.Items.Add do
          begin
            Caption := FName;
            ImageIndex := sfi.iIcon;
            SubItems.Add(FileName);
          end;
        end;
      until not FindNextFile(hFindFile, Win32FD);
    finally
      Windows.FindClose(hFindFile);
      FileView.Items.EndUpdate;
      Screen.Cursor := OldCur;
    end;
  end;  {BuildList}

begin
  DlgForm := TForm.Create(Self);
  with DlgForm do
  begin
    Parent := self;
    Align := alClient;
    BorderStyle := bsSizeToolWin; //bsSizeable;
    BorderIcons := BorderIcons - [biMaximize] - [biMinimize] -
      [biSystemMenu];
    FFont := TFont.Create;
    if owner is TForm then
      FFont.Assign(TForm(Owner).Font);
    Enabled := True;
    TabStop := False;
    FormStyle := fsStayOnTop;
    OnResize := DlgFormResize;  //82

    Panel1 := TPanel.Create(Self);
    with Panel1 do
    begin
      Parent := DlgForm;
      Align := alBottom;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Caption := '';
      Height := 22;
      TabStop := False;  //84
    end;

    Panel2 := TPanel.Create(Self);
    with Panel2 do
    begin
      Parent := Panel1;
      Height := 22;
      Width := 80;
      Align := alRight;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Caption := '';
      TabStop := False;  //84
    end;

    BtnOK := TBitBtn.Create(Self);  //84
    with BtnOk do
    begin
      Parent := Panel2;
      Height := 22;
      Width := 60;
      Top := 1;
      Left := 1;
      Caption := '&OK';
      Kind := bkOK;  //84
      Layout := blGlyphLeft;
      TabStop := True;
//      Glyph.Handle := LoadBitmap(hInstance, 'OK28');  //84
      OnClick := OKBtnClick;
      ModalResult := mrOK;
    end;

    FileView := TListView.Create(Self);
    with FileView do
    begin
      ParentWindow := DlgForm.Handle;
      parent := DlgForm;
      Align := alClient;
      Enabled := True;
      ViewStyle := vsList;
      HideSelection := False;  //84
      Hint := 'LeftClick - Select' +#13 +
              'DblClick  - Open';
      ShowHint := True;
      SortType := stText; //stBoth;
      TabStop := True;  //84
      Visible := True;
      OnDblClick := FileViewDblClick;
      CreateImages;
      BuildFileList;
      AlphaSort;
      if (Visible = True) and (Items.Count <> 0) then  //84
        Items[0].Focused := True;  //84
    end;
  end; //DlgForm
end;  {OpenFileListDlg}

procedure TLsDirTree21.DlgFormResize(Sender: TObject);   //82#
begin
  with DlgForm do
  begin
    FileView.Width := DlgForm.ClientWidth;
    FileView.Height := DlgForm.ClientHeight;
  end;
end;  //DlgFormResize


procedure TLsDirTree21.FileViewDblClick(Sender: TObject);  //82#
var
  sFile,
  sDir: string;
begin
  with DlgForm do
  begin
    if FileView.Selected <> nil then
    begin
      hide;
      sFile := ExtractFileName(FileView.Selected.SubItems[0]);
      sDir := ExtractFilePath(FileView.Selected.SubItems[0]);
      ExecuteFile('Open', sFile, '', sDir, SW_SHOW);
    end;
    visible := True;
  end;
end;  {FileViewDblClick}


procedure TLsDirTree21.OKBtnClick(Sender: TObject);  //82#
begin
  FSelectedFile := '';
  with DlgForm do
  try
    if FileView.Selected <> nil then   //82#
    begin
      FSelectedFile := FileView.Selected.SubItems[0];
//      ShowMessage(SelectedFile);  //for debugging
      if Assigned(FOnFileChange) then      //83
        FOnFileChange(Self, FSelectedFile);
    end;
  finally
    SImgList.Free;
    FileView.Free;
    DlgForm.Close;
  end;
  OpenPath(SelectedDir);  //82#
end;  {OKBtnClick}

///// End of TLsDirTree21 /////


{ ======================== TLsDirTree21PopUp =========================== }

constructor TLsDirTree21PopUp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Bmp1 := LoadBitmap(hInstance, 'NewFolder28'); //70
  Bmp2 := LoadBitmap(hInstance, 'EditFolder28'); //70
  Bmp3 := LoadBitmap(hInstance, 'DelFolder28'); //70
  Bmp4 := LoadBitmap(hInstance, 'TreeSize28');  //70
  Bmp5 := LoadBitmap(hInstance, 'DirContents28'); //80
  Bmp6 := LoadBitmap(hInstance, 'NetConnect28');  //82
  Bmp7 := LoadBitmap(hInstance, 'NetDisConnect28');  //82
end; {Create}

destructor TLsDirTree21PopUp.Destroy;
begin
  DeleteObject(Bmp1); //70
  DeleteObject(Bmp2); //70
  DeleteObject(Bmp3); //70
  DeleteObject(Bmp4); //70
  DeleteObject(Bmp5); //80
  DeleteObject(Bmp6); //82
  DeleteObject(Bmp7); //82
  inherited Destroy;
end; {Destroy}

function TLsDirTree21PopUp.AddNewItem(const aCaption: string;
  aShortCut: TShortCut; aChecked, aEnabled: Boolean; aGroup: integer;
  aOnClick: TNotifyEvent; hCtx: word; const aName: string;
  aBitMap: HBitmap): TMenuItem; //70
begin
  Result := TMenuItem.Create(nil);
  with result do
  begin
    Caption := aCaption;
    ShortCut := aShortCut;
    Checked := aChecked;
    Enabled := aEnabled;
    GroupIndex := aGroup;
    OnClick := aOnClick;
    Name := aName;
{$IFDEF D4_OR_HIGHER}
    BITMAP.Handle := aBitmap;
{$ENDIF}
  end;
end; {AddNewItem}

procedure TLsDirTree21PopUp.SetDirTree(Value: TLsDirTree21);
begin
  FDirTree := Value;
end; {SetDirTree}

procedure TLsDirTree21PopUp.BuildItems; //70
begin
  Items.Add(AddNewItem('&New Folder', 0, False, True, 0,
    ItemOnClick, 0, 'AddNode', Bmp1));
  Items.Add(AddNewItem('&Rename Folder', 0, False, True, 0,
    ItemOnClick, 0, 'EditNode', Bmp2));
  Items.Add(AddNewItem('&Delete Folder', 0, False, True, 0,
    ItemOnClick, 0, 'DelNode', Bmp3));
  Items.Add(NewLine);
  Items.Add(AddNewItem('&Tree Size', 0, False, True, 0,
    ItemOnClick, 0, 'GetTreeSize', Bmp4));
  Items.Add(AddNewItem('&Folder Contents', 0, False, True, 0,
    ItemOnClick, 0, 'ShowFolderContents', Bmp5));   //80
  Items.Add(NewLine);  //82
  Items.Add(AddNewItem('&Map Network Drive', 0, False, True, 0,
    ItemOnClick, 0, 'ConnectNetResource', Bmp6));   //82
  Items.Add(AddNewItem('Di&sConnect Network Drive', 0, False, True, 0,
    ItemOnClick, 0, 'DisConnectNetResource', Bmp7));   //82
end; {BuildItems}

procedure TLsDirTree21PopUp.ItemOnClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if TMenuItem(Sender).Name = ''  then exit;
  if TMenuItem(Sender).Name = 'AddNode' then
    FDirTree.AddNewNode(FDirTree.Selected, '')
  else if TMenuItem(Sender).Name = 'EditNode' then
    FDirTree.Selected.EditText
  else if TMenuItem(Sender).Name = 'DelNode' then
  begin
    Node := FDirTree.Selected;
    if Assigned(Node) then
      FDirTree.DeleteNode(Node);
  end
  else if TMenuItem(Sender).Name = 'GetTreeSize' then
    FDirTree.GetTreeSize
  else if TMenuItem(Sender).Name = 'ShowFolderContents' then
    FDirTree.SHowFolderContents  //80
  else if TMenuItem(Sender).Name = 'ConnectNetResource' then
    FDirTree.ConnectNetResource(Sender)            //82
  else if TMenuItem(Sender).Name = 'DisConnectNetResource' then
    FDirTree.DisConnectNetResource(Sender);        //82
end; {ItemOnClick}

procedure TLsDirTree21PopUp.PopUp(X, Y: integer);
begin
  with FDirTree do
  begin
    Self.Items[0].Enabled := (Selected.Level > 0) and (Selected <> nil);
    Self.Items[1].Enabled := (Selected.Level > 1) and (Selected <> nil);
    Self.Items[2].Enabled := (Selected.Level > 1) and (Selected <> nil);
    Self.ITems[4].Enabled := (Selected.Level > 0) and (Selected <> nil);
    Self.ITems[5].Enabled := (Selected.Level > 0) and (Selected <> nil);  //80^
  end;
  inherited Popup(X + 10, Y + 10);
end; {PopUp}

///// End of TLsDirTree21PopUp /////


{************************************************************************}
{                         LsFilelistView28                               }
{************************************************************************}

constructor TLsFilelistView28.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Createimages;
  FSelectedFiles := TStringList.Create;
  MultiSelect := True;
  ReadOnly := False;
  SetDaTeFormat(df_Customize);  //(df_MMddyyyy); //81
  SetTFormatStr('HH:mm:ss');  //70
  HideSelection := False;

  ViewStyle := vsReport;
  FMask := DefaultMask;
  FSortForward := True;
  FSortColumn := 0;
  OnCompare := CompareFiles;
  OnColumnClick := ColumnClick;

//  Bmp_Up28 := LoadBitMap(hInstance, 'Bmp_Up28'); //70
//  Bmp_Down28 := LoadBitMap(hInstance, 'Bmp_Down28'); //70
  Bmp_Up28 := LoadImage(hInstance, 'Bmp_Up28', IMAGE_BITMAP, 0, 0,
              LR_LOADMAP3DCOLORS);  //80
  Bmp_Down28 := LoadImage(hInstance, 'Bmp_Down28', IMAGE_BITMAP, 0, 0,
              LR_LOADMAP3DCOLORS); //80
  FBitmap := TBitmap.Create; //70

  FColWidth_Name := 165;  //80
  FColWidth_Size := 75;   //80
  FColWidth_Type := 95;   //80
  FColWidth_Mod  := 115;  //80
  FColWidth_Attr := 40;   //80

  ShowColumnHeaders := True;
  FShowFolders := True;
  FDblClickToOpen := True;
  FColumnClickEnabled := True;  //70

  FPopupMenu := TLsFilelistView28PopUp.Create(Self);
  FPopupMenu.BuildItems;
  FPopupMenu.AutoPopup := True;
  FPopupMenu.FFileListView := Self;
  PopupMenu := FPopupMenu;
  FPopUpMenuEnabled := True;
  FFileType := [ftReadOnly, ftHidden, ftSystem, ftArchive];

  FDirectory := 'Drives';
end; {Create}

destructor TLsFilelistView28.Destroy;
begin
  LargeImages.Free;
  SmallImages.Free;
  FPopupMenu.Free;
  FSelectedFiles.Free;

  FBitMap.Free;
  DeleteObject(Bmp_Up28);
  DeleteObject(Bmp_Down28);

  inherited Destroy;
end; {Destroy}

procedure TLsFilelistView28.CreateWnd;
begin
  inherited CreateWnd;
//  Font.Size := 8;  //80
//  Font.Name := 'MS Sans Serif';  //80
  if not (csLoading in ComponentState) then
    CreateFileColumns;
end; {CreateWnd}

procedure TLsFilelistView28.Loaded;
begin
  inherited Loaded;
  CreateFileColumns;
end; {Loaded}

function TLsFilelistView28.GetFreeSpace: Integer;
begin
  Result := GetFreeDiskSize(Copy(FDirectory, 1, 1) + ':\');
end; {GetFreeSpace}

function TLsFilelistView28.GetSelectedNum: Integer;
begin
  Result := SelCount;
  if Result = 0 then Result := Items.Count;
end; {GetSelectedNum}

function TLsFilelistView28.GetSelectedSize: Integer;
var
  i, FSize: UInt;
  FName: string;
  win32FD: TWin32FindData;
  hFindFile: THandle;
begin
  Result := 0;
  FSize := 0;
  hFindFile := 0;
  if SelCount = 0 then exit;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].selected then
    begin
      FName := ExtractFileName(Items[i].SubItems[4] + #0);
      hFindFile := FindFirstFile(pChar(FName), win32FD);
      if hFindFile <> INVALID_HANDLE_VALUE then
        FSize := FSize + ((win32FD.nFileSizeHigh * MAXDWORD) +
          win32FD.nFileSizeLow);
    end;
  end;
  Windows.FindClose(hFindFile);
  Result := FSize;
end; {GetSelectedSize}

function TLsFilelistView28.GetDirectory: string;
begin
  Result := FDirectory;
end; {GetDirectory}

procedure TLsFilelistView28.SetColWidth_Name(Value: Integer);  //80+
begin
  if Value <> FColWidth_Name then
    FColWidth_Name := Value;
end;  {SetColWidth_Name}

procedure TLsFilelistView28.SetColWidth_Size(Value: Integer);  //80+
begin
  if Value <> FColWidth_Size then
    FColWidth_Size := Value;
end;  {SetColWidth_Size}

procedure TLsFilelistView28.SetColWidth_Type(Value: Integer);  //80+
begin
  if Value <> FColWidth_Type then
    FColWidth_Type := Value;
end;  {SetColWidth_Type}

procedure TLsFilelistView28.SetColWidth_Mod(Value: Integer);  //80+
begin
  if Value <> FColWidth_Mod then
    FColWidth_Mod := Value;
end;  {SetColWidth_Mod}

procedure TLsFilelistView28.SetColWidth_Attr(Value: Integer);  //80+
begin
  if Value <> FColWidth_Attr then
    FColWidth_Attr := Value;
end;  {SetColWidth_Attr}

procedure TLsFilelistView28.SetDirectory(NewDir: string);
begin
  if AnsiCompareText(NewDir, FDirectory) = 0 then exit;
  if (UpperCase(NewDir) = 'DRIVES') then
  begin
    FDirectory := NewDir;
    UpdateFileList;
  end
  else
  begin
    if not DirectoryExists(NewDir) then exit;
    NewDir := AddSlash(NewDir);
    SetCurrentDir(NewDir);
    FDirectory := NewDir;
    UpdateFileList;
  end;
end; {SetDirectory}

procedure TLsFilelistView28.SetDirTreeCombo(val: TLsDirTreeCombo28);
begin
  if FDirTreeCombo = Val then
    exit
  else
  begin
    if Assigned(FDirTreeCombo) then
      FDirTreeCombo.FileList := nil;
    FDirTreeCombo := Val;
  end;
  if Assigned(FDirTreeCombo) then
    FDirTreeCombo.FileList := self;
end; {SetDirTreeCombo}

procedure TLsFilelistView28.SetDirTree(VaL: TLsDirTree21);
begin
  if FDirTree = Val then
    exit
  else
  begin
    if Assigned(FDirTree) then
      FDirTree.FileList := nil;
    FDirTree := Val;
  end;
  if Assigned(FDirTree) then
    FDirTree.FileList := self;
end; {SetDirTree}

procedure TLsFilelistView28.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDirTree) then
    FDirTree := nil;
  if (Operation = opRemove) and (AComponent = FDirTreeCombo) then
    FDirTreeCombo := nil;
end; {Notification}

procedure TLsFilelistView28.SetMask(const NewMasks: string);
begin
  if FMask <> NewMasks then
  begin
    FMask := NewMasks;
    UpdateFileList;
  end;
end; {SetMask}

function TLsFilelistView28.GetSelectedItem: string;
begin
  Result := FSelectedItem;
end; {GetSelectedItem}

procedure TLsFilelistView28.SetSelectedItem(NewItem: string);
begin
  if FSelectedItem = NewItem then exit;
  FSelectedItem := NewItem;
end; {SetSelectedItem}

procedure TLsFilelistView28.SetFileType(NewFileType: TFileType);
begin
  if NewFileType <> FFileType then
  begin
    FFileType := NewFileType;
    UpdateFileList;
  end;
end; {SetFileType}

procedure TLsFilelistView28.SetDaTeFormat(Value: TDTFormat); //70
var
  DefaultLCID: LCID;
begin
  if Value <> FDateFormat then
    FDateFormat := Value;
  DefaultLCID := GetThreadLocale;
  case FDateFormat of
    df_MMddyyyy: FDFormatStr := 'MM/dd/yyyy';
    df_MMddyy: FDFormatStr := 'MM/dd/yy';
    df_ddMMyyyy: FDFormatStr := 'dd/MM/yyyy';
    df_ddMMyy_GB: FDFormatStr := 'dd/MM/yy';
    df_ddMMyy_DE: FDFormatStr := 'dd.MM.yy';
    df_ddMMyy_IT: FDFormatStr := 'dd-MM-yy';
    df_yyyyMMdd: FDFormatStr := 'yyyy-MM-dd';
    df_yyMMdd: FDFormatStr := 'yy-MM-dd';
    df_Customize: FDFormatStr :=
      GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, '');
  end;
end; {SetDaTeFormat}

procedure TLsFilelistView28.SetTFormatStr(Value: string); //70
var
  DeFaultLCID: LCID;
begin
  if Value <> FTFormatStr then
    FTFormatStr := Value;
  if FTFormatStr = '' then
  begin
    DefaultLCID := GetThreadLocale;
    FTFormatStr := GetLocaleStr(DefaultLCID, LOCALE_STIMEFORMAT, '');
  end;
end; {SetTFormatStr}

procedure TLsFilelistView28.SetShowFolders(Value: Boolean);
begin
  if FShowFolders = Value then
    exit;
  FShowFolders := Value;
  UpdateFileList;
end; {SetShowFolders}

procedure TLsFilelistView28.SetHideFileExt(Value: Boolean);
begin
  if Value <> FHideFileExt then
    FHideFileExt := Value;
end; {SetHideFileExt}

procedure TLsFilelistView28.Createimages;
var
  sfi: TSHFileInfo;
begin
  if not Assigned(LargeImages) then
  begin
    Largeimages := TImageList.Create(self);
    LargeImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_SHELLICONSIZE);
    LargeImages.ShareImages := TRUE;
  end;

  if not Assigned(SmallImages) then
  begin
    Smallimages := TImageList.Create(Self);
    Smallimages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    Smallimages.ShareImages := TRUE;
  end;
end; {CreateImages}

procedure TLsFilelistView28.CreateDriveColumns;
begin
  Columns.Clear;
  with Columns.Add do
  begin
    Caption := 'Name';
    Width := 160;
    Alignment := taLeftJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Type';
    Width := 110;
    Alignment := taLeftJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Disk Size';
    Width := 90;
    Alignment := taRightJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Free Space';
    Width := 90;
    Alignment := taRightJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Attributes';
    Width := 40;
    Alignment := taLeftJustify;
  end;
end; {CreateDriveColumns}

procedure TLsFilelistView28.CreateFileColumns;
begin
  Columns.Clear;
  with Columns.Add do
  begin
    Caption := 'Name';
    Width := FColWidth_Name;   //80+
    Alignment := taLeftJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Size';
    Width := FColWidth_Size;  //80+
    Alignment := taRightJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Type';
    Width := FColWidth_Type;  //80+
    Alignment := taLeftJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Modified';
    Width := FColWidth_Mod;  //80+
    Alignment := taLeftJustify;
  end;
  with Columns.Add do
  begin
    Caption := 'Attr';
    Width := FColWidth_Attr;  //80+
    Alignment := taLeftJustify;
  end;
end; {CreateFileColumns}

procedure TLsFilelistView28.ColumnClick(Sender: TObject;
  Column: TListColumn);
var
  required_column: integer;

  procedure SetColumnBitmap; //70
  var
    Hditem: THdItem;
    i: Integer;
  begin
    for i := 0 to Columns.Count - 1 do
    begin
      FBitmap.Releasehandle;
      Hditem.Mask := HDI_FORMAT;
      Header_GetItem(GetDlgItem(Self.Handle, 0), i, Hditem);
      Hditem.Mask := HDI_BITMAP or HDI_FORMAT;

      if i = Required_Column then
      begin
        if FSortForward then
          FBitmap.Handle := Bmp_Up28
        else
          FBitmap.Handle := Bmp_Down28;
        Hditem.fmt := Hditem.fmt or HDF_BITMAP
          {$IFDEF D4_OR_HIGHER} or HDF_BITMAP_ON_RIGHT{$ENDIF};
      end
      else begin
        FBitmap.Handle := Bmp_Up28;
        Hditem.fmt := Hditem.fmt and not (HDF_BITMAP
          {$IFDEF D4_OR_HIGHER} or HDF_BITMAP_ON_RIGHT{$ENDIF});
      end;

      Hditem.hbm := FBitmap.Handle;
      Header_SetItem(GetDlgItem(Self.Handle, 0), i, hditem);
    end;
  end; {SetColumnBitmap}

begin
  if not FColumnClickEnabled then exit;  //70
  required_column := Column.Index;
  if required_column = FSortColumn then
    FSortForward := not FSortForward
  else
  begin
    FSortColumn := required_column;
    FSortForward := True;
  end;
  SortType := stData;
  SortType := stNone;
  SetColumnBitmap; //70
end; {ColumnClick}

procedure TLsFilelistView28.CompareFiles(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Caption1,
    Caption2,
    str1,
    str2: string;   //80
  size1,
    size2,
    i,              //80
    result: integer;
  date1,
    date2: TDateTime;
  TopItem: Boolean;

begin
  Result := 0;
  // To force Directories to be displayed before Files and to keep
  // "Parent Folder" at the top regardless sorting directions.
  if (Item1.Caption = ' ^ Parent') or (Item2.Caption = ' ^ Parent') then  //81
    TopItem := True    //80
  else
    TopItem := False;  //80

  if (Item1.SubItems[5] = 'dir') and (Item2.SubItems[5] <> 'dir') then
    Result := -1
  else if (Item1.SubItems[5] <> 'dir') and (Item2.SubItems[5] = 'dir')
    then
    Result := 1
  // Compare files
  else
  begin
    // sort on file name
    if FSortColumn = 0 then
    begin
      Caption1 := AnsiUpperCase(Item1.Caption);
      Caption2 := AnsiUpperCase(Item2.Caption);
      if Caption1 > Caption2 then
        Result := 1
      else if Caption1 < Caption2 then
        Result := -1;
    end
    // sort on file size
    else if FSortColumn = 1 then
    begin
      // Convert space to integer and eleminate word 'KB'    //80
      //80 >
      str1 := Item1.SubItems.Strings[0];
      if str1 = ' ' then
         size1 := 0
      else
      begin
        i := Pos('K', str1);
        str1 := copy(str1, 1, i-2);
        size1 := StrToInt(Trim(str1));
      end;
      str2 := Item2.SubItems.Strings[0];
      if str2 = ' ' then
        size2 := 0
      else
      begin
        i := Pos('K', str2);
        str2 := copy(str2, 1, i-2);
        size2 := StrToInt(Trim(str2));
      end;
      //80 <

      if Size1 > Size2 then
        Result := 1
      else if Size1 < Size2 then
        Result := -1;
    end
    // sort on files' modified date
    else if FSortColumn = 3 then
    begin
      date1 := StrToDateTime(Item1.SubItems.Strings[2]);
      date2 := StrToDateTime(Item2.SubItems.Strings[2]);
      result := Round(1E5 * (date1 - date2));
    end
    else
    begin
      result := CompareText(Item1.SubItems.Strings[FSortColumn - 1],
        Item2.SubItems.Strings[FSortColumn - 1]);
    end;
  end; // else
  if FSortForward then
    if TopItem then       //80+
      Compare := result   //80+
    else                  //80+
      Compare := -result
  else begin
    Compare := result;
  end;
end; {CompareFiles}

procedure TLsFilelistView28.Keydown(var Key: Word; Shift: TShiftState);
var
  i: integer;  //84
begin
  if Shift = [ssCtrl] then
  begin
    case Key of
      $43:
        if Selected <> nil then CutCopy(0);      //(VK_C)Copy
      $58:
        if Selected <> nil then CutCopy(2);      //(VK_X)Cut
      $56:
        if FSelectedFiles.Count <> 0 then Paste; //(VK_V)Paste
      $4F:
        if Selected <> nil then OpenItem;        //(VK_O)OpenItem
      $46:
        if Selected <> nil then ViewFile;        //(VK_F)View file
      $4E:
        if AnsiCompareText(FDirectory, 'DRIVES') <> 0 then
          NewFolder;                             //(VK_N)New Folder
      $41:                                       //(VK_A)Select All  //84
        begin
          if Items.Count <> 0 then
          begin
            for i := 0 to Items.Count -1 do
              Items[i].Selected := True;
          end;
        end;
    end;
  end
  else if (Shift = []) and (not isEditing) then
  begin
    case Key of
//      VK_UP: Click;                            //UpArrow    //84
//      VK_DOWN: Click;                          //DownArrow  //84
      VK_DELETE:
        if Selected <> nil then DeleteItems;     //Delete File
      VK_RETURN:
        if Selected <> nil then OpenItem;        //Open
      VK_BACK: OneLevelUp;                       //Previous Directory
      VK_F2:
        if Selected <> nil then RenameFile;      //ReName File
      VK_F3: FindFile;
      VK_F5: UpdateFileList; //52

      VK_F4:                                     //52 Display TreeView
        begin
          if not Assigned(DirTreeCombo) then exit;
          DirTreeCombo.BtnClick(Self);
          if DirTreeCombo.Tree.Visible then
            DirTreeCombo.Tree.SetFocus;
        end;
      VK_ESCAPE:                                 //52 Close TreeView
        begin
          if not Assigned(DirTreeCombo) then exit;
          if DirTreeCombo.Tree.Enabled = True then
          begin
            DirTreeCombo.Tree.Enabled := False;
            DirTreeCombo.Tree.Visible := False;
            DirTreeCombo.SendToBack;
          end;
        end;
    end; // case
  end; // else
  inherited KeyDown(Key, Shift);
end; {KeyDown}

procedure TLsFilelistView28.KeyUp(var Key: Word; Shift: TShiftState);  //84
begin
  Inherited KeyUp(Key, Shift);
  if (Key = VK_UP) or (Key = VK_DOWN) then  Click;
end;  {KeyUp}

function TLsFileListView28.GetWkgMask(var MaskStr: string): string; //84
var
  SepPos: integer;
begin
  SepPos := Pos(';', MaskStr);
  if SepPos = 0 then
    Result := MaskStr
  else
    Result := Copy(MaskStr, 1, SepPos - 1);
  System.Delete(MaskStr, 1, (Length(Result) + 1));
end;  {GetWkgMask}

procedure TLsFilelistView28.UpdateFileList;
var
  oldCur: TCursor;
//  MaskPtr: PChar;  //84
  TmpMask,
    WkgMask: string;  //84
  AttrIndex: TFileAttr;
//  Ptr: PChar;  //84
  DirAttr,
    FileAttr: DWORD;
  FName,
    CurPath: string;
const
  dwFileAttr: array[TFileAttr] of DWord = (FILE_ATTRIBUTE_READONLY,
    FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_SYSTEM,
    FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_NORMAL);
begin
  Items.beginUpdate;
  Items.Clear;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FDirectorySize := 0;
  try
  // Add Drives to LsFilelistView28
    if UpperCase(FDirectory) = 'DRIVES' then
    begin
      CreateDriveColumns;
      AddDrives
    end
    else begin
    // Add directories to LsFilelistView28
      CreateFileColumns;
      FileAttr := 0;
      for AttrIndex := ftReadOnly to ftNormal do
        if AttrIndex in FileType then
          FileAttr := FileAttr or dwFileAttr[AttrIndex];
      DirAttr := FileAttr or FILE_ATTRIBUTE_DIRECTORY;
      CurPath := AddSlash(FDirectory);
      FName := CurPath + '*.*';

      if ShowFolders then
        AddFiles(FName, DirAttr);

      ////84 >>
      TmpMask := FMask;
      While (Length(TmpMask) > 0) and (TmpMask[1] = ' ') do
        System.Delete(TmpMask, 1, 1);
      While (Length(TmpMask) > 0) and (TmpMask[Length(TmpMask)] = ' ') do
        System.Delete(TmpMask, Length(TmpMask), 1);
      if Length(TmpMask) =  0 then
        TmpMask := '*.*';

      While Length(TmpMask) > 0 do
      begin
        WkgMask := GetWkgMask(TmpMask);
        AddFiles((CurPath + WkgMask), FileAttr);
      end;
      {**
      // Add files to LsFilelistView28
      MaskPtr := PChar(FMask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan(MaskPtr, ';');
        if Ptr <> nil then
          Ptr^ := #0;
        AddFiles((CurPath + StrPas(MaskPtr)), FileAttr);
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          inc(Ptr);
        end;
        MaskPtr := Ptr;
      end; // while MaskPtr ...
      **}
      ////84 <<
    end; // else  FDirectory <> 'Drives'
  finally
    FSortForward := True;
    ColumnClick(Self, Columns[0]);
  end; // try
  Items.EndUpdate;
  Screen.Cursor := oldCur;
  if (Items.Count > 0) and (SelCount = 0) then
    ItemFocused := Items[0];
  Application.ProcessMessages;
end; {UpdateFileList}

procedure TLsFilelistView28.AddDrives;
var
  sfi: TSHFileInfo;
  NewItem: TListItem;
  i: Integer;
  DiskType: Integer;
  DiskSize,
    FreeSize: Integer;
  Drv: Char;
  DrvName: string;

  function GetDriveTypeStr(Root: string): string;
  var
    DrvType: Integer;
  begin
    DrvType := GetDriveType(PChar(Root));
    case DrvType of
      0: Result := 'Unknown';
      1: Result := 'Not exist';
      DRIVE_REMOVABLE: Result := 'Removable Disk';
      DRIVE_FIXED: Result := 'Fixed Disk';
      DRIVE_REMOTE: Result := 'Network Disk';
      DRIVE_CDROM: Result := 'CD-ROM Disk';
      DRIVE_RAMDISK: Result := 'RAM Disk';
    end;
  end; {GetDriveTypeStr}

begin
  FColumnClickEnabled := False; //70
  Integer(Drives) := GetLogicalDrives;
  for i := 0 to 25 do
    if (i in Drives) then
    begin
      Drv := Char(i + Ord('A'));
      NewItem := Items.Add;
      try
        SHGetFileInfo(PChar(Drv + ':\'), 0, sfi, SizeOf(sfi),
          SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME or SHGFI_TYPENAME);
        if SmallImages <> nil then
          NewItem.ImageIndex := sfi.Iicon;
        DrvName := copy(sfi.szDisplayName, 1,
          (Pos('(', sfi.szDisplayName) - 1));
        NewItem.Caption := ' (' + Drv + ':)  ' + DrvName;
        DiskType := GetDriveType(PChar(Drv + ':\'));
        NewItem.SubItems.Add(GetDriveTypeStr(Drv + ':\'));

        if (DiskType <> DRIVE_REMOVABLE) and (DiskType <> DRIVE_CDROM) then
        begin
          DiskSize := GetDiskSize(Drv + ':\');
          FreeSize := GetFreeDiskSize(Drv + ':\');
        end
        else
        begin
          DiskSize := 0;
          FreeSize := 0;
        end;

        if DiskSize > 0 then
          NewItem.SubItems.Add(FormatFloat('###,###,### Mb',
            DiskSize div 1000))
        else
          NewItem.SubItems.Add('0 Mb');

        if FreeSize > 0 then
          NewItem.SubItems.Add(FormatFloat('###,###,### Mb',
            FreeSize div 1000))
        else
          NewItem.SubItems.Add('0 Mb');
        NewItem.SubItems.Add('');
        NewItem.SubItems.Add(Drv + ':\');
        NewItem.SubItems.Add('drv');
      except
        Items.Delete(NewItem.Index);
      end;
    end;
end; {AddDrives}

function TLsFilelistView28.AddFiles(FileMask: string; Attr: DWord):
  Boolean;
var
  sfi: TSHFileInfo;
  attributes,
    CurPath,
    FDate,
    FName,
    FileName,
    FileDir: string;
  Count,
    FSize: Integer;
  hFindFile: THandle;
  Win32FD: TWin32FindData;

  function AttrStr(Attr: integer): string;
  begin
    Result := '';
    if (FILE_ATTRIBUTE_ARCHIVE and Attr) > 0 then Result := Result + 'A';
    if (FILE_ATTRIBUTE_READONLY and Attr) > 0 then Result := Result + 'R';
    if (FILE_ATTRIBUTE_HIDDEN and Attr) > 0 then Result := Result + 'H';
    if (FILE_ATTRIBUTE_SYSTEM and Attr) > 0 then Result := Result + 'S';
  end;

begin
  Result := False;
  FColumnClickEnabled := True; //70
  CurPath := AddSlash(FDirectory);
  hFindFile := FindFirstFile(PChar(FileMask), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      with Win32FD do
      begin
        if ((Attr and FILE_ATTRIBUTE_DIRECTORY) =
          (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)) and
          ((Attr and FILE_ATTRIBUTE_READONLY) >=
          (dwFileAttributes and FILE_ATTRIBUTE_READONLY)) and
          ((Attr and FILE_ATTRIBUTE_HIDDEN) >=
          (dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)) and
          ((Attr and FILE_ATTRIBUTE_SYSTEM) >=
          (dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)) then
        begin
          if (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then
            FileDir := 'dir'
          else
            FileDir := 'file';

          FName := StrPas(Win32FD.cFilename);
          FileName := CurPath + FName;

//          if ( FName = '.' )  or  ( FName = '..' )  then continue;  //70
          if (FName = '.') then continue; //70

          SHGetFileInfo(PChar(FileName), 0, sfi, SizeOf(sfi),
            SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_ICON or
            SHGFI_DISPLAYNAME);

          FSize := (nFileSizeHigh * MAXDWORD) + nFileSizeLow;
          FDate := FileTimeToDateTimeStr(ftLastWriteTime, FDFormatStr,
            FTFormatStr);
          Attributes := AttrStr(dwFileAttributes);

          with Items.Add do
          begin
            if FName = '..' then //70
              Caption := ' ^ Parent'  //81
            else begin
              if FHideFileExt then //70
              begin
                Count := Pos('.', FName);
                if Count > 0 then
                  Caption := Copy(FName, 1, Count - 1)
                else Caption := FName;
              end
              else
                Caption := FName;            //add caption
            end;

            ImageIndex := sfi.iIcon;         //add icon

            SubItems.Add(ConvertSize(FSize, FileDir)); //SubItems[0]-Size
            if FName = '..' then  //80
              SubItems.Add(' ')   //80
            else
              SubItems.Add(sfi.szTypeName);  //SubItems[1]-TypeName
            SubItems.Add(FDate);             //SubItems[2]-DateTime
            SubItems.Add(Attributes);        //SubItems[3]-Attributes
            SubItems.Add(FileName);          //SubItems[4]-Path+FileName
            SubItems.Add(FileDir);           //SubItems[5]-dir or file
          end; // with Items ...

          FDirectorySize := FDirectorySize + FSize;
          Result := True;
        end; // Attr ...
      end; // with Win32FD
    until not FindNextFile(hFindFile, Win32FD);
  finally
    windows.FindClose(hFindFile);
  end; // try
end; {AddFiles}

procedure TLsFilelistView28.OneLevelUp;
var
  NewDir: string;
begin
  if UpperCase(Directory) = 'DRIVES' then
    exit;
  FDirectory := AddSlash(FDirectory);
  if (FDirectory[Length(FDirectory) - 1] = ':') then
  begin
    if (FDirTreeCombo <> nil) then
      FDirTreeCombo.OpenPath(FDirTreeCombo.Tree.Items[0].Text);
    if (FDirTree <> nil) then
      FDirTree.OpenPath(FDirTree.Items[0].Text);
    SetDirectory('Drives');
  end
  else begin
    FDirectory := Copy(FDirectory, 1, Length(FDirectory) - 1);
    NewDir := ExtractFilePath(FDirectory);
    SetDirectory(NewDir);
    if FDirTree <> nil then
      FDirTree.OpenPath(NewDir);
    if FDirTreeCombo <> nil then
      FDirTreeCombo.OpenPath(NewDir);
  end;
end; {OneLevelUp}

procedure TLsFilelistView28.Click;
begin
  if Selected <> nil then
    FSelectedItem := Selected.SubItems[4];
  if Assigned(FOnItemChange) then  //83
    FOnItemChange(Self, FSelectedItem);  //83
  inherited Click;
end; {Click}

procedure TLsFilelistView28.SetDblClickToOpen(Value: Boolean);
begin
  if Value <> FDblClickToOpen then
    FDblClickToOpen := Value;
end; {SetDblClickToOpen}

procedure TLsFilelistView28.DblClick;
begin
  if (Selected = nil) or (Selected.Caption = '') then  
    exit;                                              //81
  if FDblClickToOpen then //70
    OpenItem
  else begin
    if (Selected.Caption = ' ^ Parent') then //70  //81
      OneLevelUp
    else
      inherited DblClick;
  end;
end; {DblClick}

procedure TLsFilelistView28.SetPopUpMenuEnabled(Value: Boolean);
begin
  if Value <> FPopUpMenuEnabled then
  begin
    FPopUpMenuEnabled := Value;
    if FPopUpMenuEnabled then
      PopupMenu := FPopupMenu
    else
    begin
      PopupMenu := nil;
      MouseCapture := False; //80
    end;
  end;
end; {SetPopUpMenuEnabled}


///// FileOperations /////

function TLsFilelistView28.CanEdit(Item: TListItem): Boolean;
begin
  Result := False;
  if not ReadOnly then
  begin
    OldFName := Item.SubItems[4];
    Result := inherited CanEdit(Item);
  end;
end; {CanEdit}

procedure TLsFilelistView28.Edit(const Item: TLVItem);
var
  Path,
    newFName,
    DirOrFile: string;
  Abort: Boolean;
  OldCur: TCursor;
begin
  inherited Edit(Item);
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    if Selected <> nil then
    begin
      Path := ExtractFilePath(OldFName);
      DirOrFile := Selected.SubItems[5];
      newFName := AddNullToStr(Path + Selected.Caption);
      if AnsiCompareText(OldFName, newFName) = 0 then exit;

      if not FileDirExist(newFName) then
        DoSHFileOp(Parent.Handle, FO_RENAME, OldFName, newFName, Abort);

      UpdateFileList;

      if AnsiCompareText(DirOrFile, 'DIR') = 0 then
      begin
        if Assigned(FDirTree) then
        begin
          FDirTree.Reload;
          FDirTree.OpenPath(Path);
        end
        else if Assigned(FDirTreeCombo) then
        begin
          FDirTreeCombo.ResetTreeView;
          FDirTreeCombo.OpenPath(Path);
        end;
      end;
    end;
  finally
    Items.EndUpdate;
    Screen.Cursor := oldCur;
  end;
end; {Edit}

procedure TLsFilelistView28.NewFolder;
var
  NewDir: string;
  i: integer;
begin
  //81 >>
  NewDir := SlashSep(FDirectory, 'New Folder');
  if DirectoryExists(NewDir) then
  begin
    i := 1;
    Repeat
      inc(i);
    until not DirectoryExists(NewDir + '(' + IntToStr(i) + ')');
    NewDir := NewDir + '(' + IntToStr(i) + ')';
  end;  //81 <<
  CreateDir(NewDir);
  UpdateFileList;
  //81 >>
  Selected := nil;
  For i := 0 to Items.Count - 1 do
  begin
    if Items[i].SubItems[4] = NewDir then
      Selected := Items[i];
  end;
  CanEdit(Selected);
  Selected.EditCaption;
  //81 <<
  if Assigned(DirTreeCombo) then
  begin
    FDirTreeCombo.ResetTreeView;
    FDirTreeCombo.OpenPath(NewDir);
  end
  else if Assigned(FDirTree) then
  begin
    FDirTree.Reload;
    FDirTree.OpenPath(NewDir);
  end;
end; {NewFolder}

procedure TLsFilelistView28.RenameFile;
var
  SelItem: TListItem;
begin
  if Selected = nil then exit;
  if ReadOnly or (UpperCase(Selected.SubItems[3]) = 'R') then
  begin
    MessageDlg('It''s ReadOnly', mtWarning, [mbOK], 0);
    exit;
  end;
  OldFName := AddNullToStr(Selected.SubItems[4]);
  SelItem := Selected;
  CanEdit(SelItem);
  Selected.EditCaption;
end; {ReNameFile}

procedure TLsFilelistView28.DeleteItems;
var
  i: integer;
  Abort: Boolean;
  DelFName: string;
  oldCur: TCursor;
begin
  Abort := False;
  oldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if not SetCurrentDir(FDirectory) then exit;
  try
    FSelectedFiles.Clear;
    DelFName := '';
    for i := 0 to Items.Count - 1 do
    begin
      if Items[i].Selected then
//        FSelectedFiles.Add(AddNullToStr(Items[i].SubItems[4])); //60  //81
        DelFName := DelFName + AddNullToStr(Items[i].SubItems[4]); //81
    end;
    DoSHFileOp(Parent.Handle, FileOpMode[1], DelFName, '', Abort);

  finally
    UpdateFileList;
    FSelectedFiles.Clear;
    if Assigned(FDirTreeCombo) then
    begin
      FDirTreeCombo.ResetTreeView;
      FDirTreeCombo.OpenPath(FDirectory);
    end
    else if Assigned(FDirTree) then
    begin
      FDirTree.Reload;
      FDirTree.OpenPath(FDirectory);
    end;
  end;
  Screen.Cursor := oldCur;
end; {DeleteItems}

procedure TLsFilelistView28.CutCopy(Mode: integer);
var
  i: integer;
begin
  FOpMode := -1;
  FSelectedFiles.Clear;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].selected then
      FSelectedFiles.Add(AddNullToStr(Items[i].SubItems[4]));
  end;
  FOpMode := Mode;
end; {CutCopy}

procedure TLsFilelistView28.Paste;
var
  i: integer;
  FSrc,
    FDes,
    DFName: string;
  Abort: Boolean;
  oldCur: TCursor;
begin
  Abort := False;
  oldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if not SetCurrentDir(FDirectory) then
    exit;
  if FSelectedFiles.Count = 0 then
  begin
    MessageDlg('No file(s) selected', mtWarning, [mbOK], 0);
    exit;
  end;
  try
    for i := 0 to FSelectedFiles.Count - 1 do
    begin
      FSrc := '';
      FDes := '';
      FSrc := FSelectedFiles.Strings[i];
      DFName := ExtractFileName(FSrc);
      FDes := SlashSep(FDirectory, DFName);
      FDes := AddNullToStr(FDes);
      DoSHFileOp(Parent.Handle, FileOpMode[FOpMode], FSrc, FDes, Abort);
    end;
  finally
    UpdateFileList;
    FSelectedFiles.Clear;
    //81 >>
    if Assigned(FDirTreeCombo) then
    begin
      FDirTreeCombo.ResetTreeView;
      FDirTreeCombo.OpenPath(FDirectory);
    end
    else if Assigned(FDirTree) then
    begin
      FDirTree.Reload;
      FDirTree.OpenPath(FDirectory);
    end;
    //81 <<
  end;
  Screen.Cursor := oldCur;
end; {Paste}

procedure TLsFilelistView28.OpenItem;
var
  sFile,
    sDir: string;
  cDrv: Char;
  oldCur: TCursor;
begin
  if Selected = nil then exit;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if (Selected.Caption = ' ^ Parent') then //70  //81++
  begin
    OneLevelUp;
    Screen.Cursor := oldCur;
    exit;
  end;
  if (Selected.SubItems[5] = 'dir') or
    (Selected.SubItems[5] = 'drv') then
  begin
    sDir := Selected.SubItems[4];
    sDir := AddSlash(sDir);
    if Selected.SubItems[5] = 'drv' then
      if GetDriveType(PChar(sDir)) = DRIVE_REMOVABLE then
      begin
        cDrv := Selected.SubItems[4][1];
        if not DiskInDrive(cDrv, 1) then
        begin
          Screen.Cursor := OldCur;
          exit;
        end;
      end;
    SetDirectory(sDir);
    if FDirTree <> nil then
      FDirTree.OpenPath(sDir);
    if FDirTreeCombo <> nil then
      FDirTreeCombo.OpenPath(sDir);
  end
  else if Selected.SubItems[5] = 'file' then
  begin
    sFile := ExtractFileName(Selected.SubItems[4]);
    sDir := ExtractFilePath(Selected.SubItems[4]);
    ExecuteFile('Open', sFile, '', sDir, SW_SHOW);
  end;
  Screen.Cursor := OldCur;
end; {OpenItem}

procedure TLsFilelistView28.ViewFile;
var
  i: integer;
  sExt,
    sPath,
    sFName,
    WinDir: string;
begin
  if Selected <> nil then
  begin
    sPath := ExtractFilePath(Selected.SubItems[4]);
    sPath := AddSlash(sPath);
    sFName := ExtractFileName(Selected.SubItems[4]);
    sExt := UpperCase(ExtractFileExt(Selected.SubItems[4]));
    SetLength(WinDir, MAX_PATH); //52
    SetLength(WinDir, GetWindowsDirectory(PChar(WinDir), MAX_PATH)); //52
    WinDir := AddSlash(WinDir);
    for i := Low(FileExt) to High(FileExt) do
      if (i <= 9) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'NotePad.exe', sFName, sPath, SW_SHOW)
      else if (i > 9) and (i <= 12) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'Write.exe', sFName, sPath, SW_SHOW)
      else if (i = 13) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'PBrush.exe', sFName, sPath, SW_SHOW);
  end;
end; {ViewFile}

procedure TLsFilelistView28.FindFile;
begin
  if Focused then
    ExecuteFile('Find', Directory, '', '', SW_SHOWNORMAL);
end; {FindFile}

procedure TLsFilelistView28.FileAttr;
var
  NewAttr,
    OldAttr: word;
  AttrForm: TForm;
  Panel1: TPanel;
  CbxA,
    CbxR,
    CbxS,
    CbxH: TCheckBox;
  BtnOK,
    BtnCancel: TBitBtn;
  LblFName: TLabel;
begin
  AttrForm := TForm.Create(Application);
  with AttrForm do
  try
    ClientHeight := 172;
    ClientWidth := 266;
    Canvas.Font.Name := 'MS Sens Serif';
    Canvas.Font.Size := 8;
    BorderStyle := bsDialog;
    Caption := 'Set Attributes';
    Position := poScreenCenter;
    LblFName := TLabel.Create(Self);
    with LblFName do
    begin
      Parent := AttrForm;
      Left := 20;
      Top := 12;
      Font.Size := 10;
      Caption := ExtractFileName(Selected.SubItems[4]);
    end;
    Panel1 := TPanel.Create(Self);
    with Panel1 do
    begin
      Parent := AttrForm;
      Left := 20;
      Top := 34;
      Height := 77;
      Width := 225;
      BevelOuter := bvNone;
      BevelInner := bvLowered;
      CbxR := TCheckBox.Create(Self);
      with CbxR do
      begin
        Parent := Panel1;
        Left := 20;
        Top := 16;
        Caption := 'ReadOnly';
        Checked := False;
      end;
      CbxA := TCheckBox.create(Self);
      with CbxA do
      begin
        Parent := Panel1;
        Left := 20;
        Top := 44;
        Caption := 'Archive';
        Checked := False;
      end;
      CbxH := TCheckBox.Create(Self);
      with CbxH do
      begin
        Parent := Panel1;
        Left := 120;
        Top := 16;
        Caption := 'Hidden';
        Caption := Trim(Caption);
        Checked := False;
      end;
      CbxS := TCheckBox.Create(Self);
      with CbxS do
      begin
        Parent := Panel1;
        Left := 120;
        Top := 44;
        Caption := 'System';
        Caption := Trim(Caption);
        Checked := False;
      end;
    end; // Panel1
    BtnOK := TBitBtn.Create(Self);
    with BtnOK do
    begin
      Parent := AttrForm;
      Top := 128;
      Left := 44;
      Height := 25;
      Width := 75;
      Kind := bkOK;
      ModalResult := mrOK;
    end;
    BtnCancel := TBitBtn.Create(Self);
    with BtnCancel do
    begin
      Parent := AttrForm;
      Top := 128;
      Left := 140;
      Height := 25;
      Width := 75;
      Kind := bkCancel;
      ModalResult := mrCancel;
    end;
    OldAttr := FileGetAttr(Selected.SubItems[4]);
    CbxR.Checked := OldAttr and faReadOnly = faReadOnly;
    CbxA.Checked := OldAttr and faArchive = faArchive;
    CbxH.Checked := OldAttr and faHidden = faHidden;
    CbxS.Checked := OldAttr and faSysFile = faSysFile;
    ShowModal;
    if ModalResult = mrOK then
    begin
      NewAttr := OldAttr;
      if CbxR.Checked then
        NewAttr := NewAttr or faReadOnly
      else
        NewAttr := NewAttr and not faReadOnly;
      if CbxA.Checked then
        NewAttr := NewAttr or faArchive
      else
        NewAttr := NewAttr and not faArchive;
      if CbxS.Checked then
        NewAttr := NewAttr or faSysFile
      else
        NewAttr := NewAttr and not faSysFile;
      if CbxH.Checked then
        NewAttr := NewAttr or faHidden
      else
        NewAttr := NewAttr and not faHidden;
      if NewAttr <> OldAttr then
        FileSetAttr(Selected.SubItems[4], NewAttr);
      UpdateFileList;
    end; // mrOK
  finally
    AttrForm.Free;
  end;
end; {FileAttr}

procedure TLsFilelistView28.SendTo(SubItems: integer);
var
  ADirPath,
    SelPath: string;
  CopyBrd: TRichEdit;
  FNMemo: TMemo;
  AImage: TImage;
  FPath,
    FExt: string;
  i: integer;
  oldCur: TCursor;

  function InputDlg(AValue: string): string;
  var
    TDlgForm: TForm;
    Prompt: TLabel;
    Edit: TEdit;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    BtnBrowse: TBitBtn;
    ButtonTop: Integer;
    ButtonWidth: Integer;
    ButtonHeight: Integer;
    mrBrowse: TModalResult;
  begin
    Result := '';
    mrBrowse := mrNo + 3;
    TDlgForm := TForm.Create(Application);
    with TDlgForm do
    try
      Canvas.Font.Name := 'MS Sans Serif';
      Canvas.Font.Size := 8;
      BorderStyle := bsDialog;
      Caption := 'Send to any folder';
      ClientWidth := 333;
      ClientHeight := 135;
      Position := poScreenCenter;
      Prompt := TLabel.Create(TDlgForm);
      with Prompt do
      begin
        Parent := TDlgForm;
        AutoSize := True;
        Left := 20;
        Top := 12;
        Caption := 'Please enter the send_to path';
      end;
      Edit := TEdit.Create(TDlgForm);
      with Edit do
      begin
        Parent := TDlgForm;
        Left := Prompt.Left;
        Top := 32;
        Width := 293;
        MaxLength := 285;
        Text := AValue;
        SelectAll;
      end;
      ButtonTop := 80;
      ButtonWidth := 75;
      ButtonHeight := 25;
      BtnOK := TBitBtn.Create(TDlgForm);
      with BtnOK do
      begin
        Parent := TDlgForm;
        Kind := bkOK;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        Cancel := True;
        SetBounds(37, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      BtnCancel := TBitBtn.Create(TDlgForm);
      with BtnCancel do
      begin
        Parent := TDlgForm;
        Kind := bkCancel;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(129, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      BtnBrowse := TBitBtn.Create(TDlgForm);
      with BtnBrowse do
      begin
        Parent := TDlgForm;
        Kind := bkHelp;
        Caption := 'Browse';
        Cancel := False;
        ModalResult := mrBrowse;
        SetBounds(221, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      ShowModal;
      if ModalResult = mrBrowse then
      begin
        if BrowseForDir(Application.Handle, ADirPath) then
          SelPath := InputDlg(ADirPath);
      end;
      if ModalResult = mrOK then
      begin
        AValue := Edit.Text;
        if AValue <> '' then
          Result := AValue;
        TDlgForm.Close;
        exit;
      end;
      if ModalResult = mrCancel then
      begin
        Result := '';
      end;
    finally
      TDlgForm.Free;
    end;
  end; {InputDlg}

begin
  AImage := nil;
  case SubItems of
    0:
      begin
        ADirPath := '';
        InputDlg(ADirPath);
        if SelPath <> '' then
          SendToPath(SelPath);
      end;
    1:
      begin
        FPath := Selected.SubItems[4];
        FExt := UpperCase(ExtractFileExt(FPath));
        if (FExt = '.BMP') or (FExt = '.WMF') then
        begin
          try
            AImage := TImage.Create(Self);
            AImage.Parent := Self;
            AImage.Picture.LoadFromFile(FPath);
            ClipBoard.Assign(AImage.Picture);
          finally
            AImage.Free;
          end;
        end
        else
        begin
          OldCur := Screen.Cursor;
          Screen.Cursor := crHourGlass;
          CopyBrd := TRichEdit.Create(Self);
          with CopyBrd do
          begin
            Parent := Self;
            PlainText := False;
            Visible := False;
          end;
          if Selected <> nil then
          try
            CopyBrd.Lines.LoadFromFile(FPath);
            CopyBrd.SelectAll;
            CopyBrd.CopyToClipboard;
          finally
            CopyBrd.Free;
            Screen.Cursor := OldCur;
          end;
        end;
      end;
    2:
      begin
        FNMemo := TMemo.Create(Self);
        FNMemo.Parent := Self;
        FNMemo.Lines.Clear;
        try
          for i := 0 to Items.Count - 1 do
          begin
            if Items[i].Selected then
              FNMemo.Lines.Add(Selected.SubItems[4] + #0);
          end;
          FNMemo.SelectAll;
          FNMemo.CopyToClipboard;
        finally
          FNMemo.Free;
        end;
      end;
  end;
end; {SendTo}


procedure TLsFilelistView28.SendTo2(Path: string);
var
  FName: string;
  PName: string;
  DPath: string;
  Drv: string;

 // Resolving Shortcuts
  function GetShellLinkPath(Handle: THandle; LinkFileName: string):
      string;
  var
    pShlLnk: IShellLink;
    pszPath: array[0..MAX_PATH - 1] of Char;
    win32FD: TWin32FindData;
    ppF: IPersistFile;
    hRes: hResult;
//  {$IFNDEF D3_OR_HIGHER}   //82
//    pSource: array[0..MAX_PATH - 1] of wideChar;  //82
//  {$ELSE}   //82
    IUnk: IUnknown;
    pSource: WideString;
//  {$ENDIF}  //82
  begin
    Result := '';

//{$IFNDEF D3_OR_HIGHER}  //82 >>
//    CoInitialize(nil);
//    if CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
//      IID_IShellLink, pShlLnk) = S_OK then
//      if pShlLnk.QueryInterface(IID_IPersistFile, ppF) = S_OK then
//      try
//        MultiByteToWideChar(CP_ACP, 0, @LinkFileName[1], -1, pSource,
//          MAX_PATH);
//        //  ShowMessage(pSource);    // for debug
//        if ppF.Load(pSource, STGM_READ) = S_OK then
//        begin
//          hRes := pShlLnk.GetPath(pszPath, MAX_PATH, win32FD, SLGP_SHORTPATH
//            );
//          if hRes = S_OK then
//            Result := StrPas(pszPath)
//          else
//            exit;
//        end;
//      finally
//        ppF.Release;
//        CoUnInitialize;
//      end;
//{$ELSE}  //82 <<

    IUnk := CreateComObject(CLSID_ShellLink);
    pShlLnk := IUnk as IShellLink;
    ppF := IUnk as IPersistFile;
    pSource := LinkFileName;

    hRes := ppF.Load(pWideChar(pSource), STGM_READ);
    if Succeeded(hRes) then
    begin
      hRes := pShlLnk.Resolve(Application.Handle, SLR_ANY_MATCH);
      if Succeeded(hRes) then
      begin
        hRes := pShlLnk.GetPath(@pszPath, MAX_PATH, Win32FD, 0);
        if Succeeded(hRes) then
          Result := string(pChar(@pszPath));
      end;
    end;
//{$ENDIF}  //82
  end; {GetShellLinkPath}

begin
  if Selected = nil then exit;
  if UpperCase(ExtractFileExt(Path)) = '.LNK' then
  begin
    FName := GetShellLinkPath(Handle, Path);
    PName := ExtractFileName(Selected.SubItems[4]);
    DPath := ExtractFilePath(Selected.SubItems[4]);
    // ShowMessage(FName); // for debug
    if FName = '' then exit;
    Drv := Copy(FName, 1, 3);
    if (GetDriveType(PChar(Drv)) = DRIVE_REMOVABLE) then
      SendToDrive(FName)
    else
    begin
      if DirectoryExists(FName) then
        SendToPath(FName)
      else
        ExecuteFile('Open', FName, PName, DPath, SW_SHOW);
    end;
  end
  else if DirectoryExists(Path) then
    SendToPath(Path);
end; {SendTo2}

procedure TLsFilelistView28.SendToPath(DestPath: string);
var
  i: integer;
  FSrc: string;
  FDes: string;
  DFName: string;
  Abort: Boolean;
  OldCur: TCursor;
begin
  if not SetCurrentDir(FDirectory) then exit;
  Abort := False;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FSelectedFiles.Clear;
  try
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        FSelectedFiles.Add(Items[i].SubItems[4] + #0);
    for i := 0 to FSelectedFiles.Count - 1 do
    begin
      FSrc := '';
      FDes := '';
      FSrc := FSelectedFiles.Strings[i];
      DFName := ExtractFileName(FSrc);
      FDes := DestPath + DFName + #0;
      DoSHFileOp(Parent.Handle, FileOpMode[0], FSrc, FDes, Abort);
    end;
  finally
    FSelectedFiles.Clear;
    Screen.Cursor := OldCur;
  end;
end; {SendToPath}

procedure TLsFilelistView28.SendToDrive(DriveID: string);
var
  Drv: Char;
  OldCur: TCursor;
begin
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Drv := DriveID[1];
  if not DiskInDrive(Drv, 1) then
  begin
    Screen.Cursor := oldCur;
    exit
  end
  else
    SendToPath(DriveID);
  Screen.Cursor := oldCur;
end; {SendToDrive}

//// End of TLsFilelistView28 /////


{***********************************************************************}
{                       LsFilelistView28PopUp                           }
{***********************************************************************}

constructor TLsFilelistView28PopUp.Create(AOwner: TComponent);
var
  sfi: TSHFileInfo;
  hImgLst: Uint;
begin
  inherited Create(AOwner);

  SendToList := TStringList.Create;

{$IFDEF D4_OR_HIGHER}      //70
  Images := TImageList.Create(self);
  hImgLst := SHGetFileInfo('', 0,
                           sfi, SizeOf(sfi),
                           SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if hImgLst <> 0 then
  begin
    Images.Handle := hImgLst;
    Images.BkColor := clNone;  //80+
    Images.ShareImages := True;
  end;
{$ENDIF}

 // Load Bitmaps from Resource            //70
  Bmp1 := LoadBitMap(hInstance, 'Open28');
  Bmp2 := LoadBitmap(hInstance, 'View28');
  Bmp3 := LoadBitmap(hInstance, 'SendTo28');
  Bmp4 := LoadBitmap(hInstance, 'Cut28');
  Bmp5 := LoadBitmap(hInstance, 'Copy28');
  Bmp6 := LoadBitmap(hInstance, 'Paste28');
  Bmp7 := LoadBitmap(hInstance, 'ReName28');
  Bmp8 := LoadBitmap(hInstance, 'Delete28');
  Bmp9 := LoadBitmap(hInstance, 'Attributes28');
  Bmp10 := LoadBitmap(hInstance, 'NewFolder28');
  Bmp11 := LoadBitmap(hInstance, 'AnyFolder28');
  Bmp12 := LoadBitmap(hInstance, 'ClipBoard28');
end; {Create}

destructor TLsFilelistView28PopUp.Destroy;
begin
  SendToList.Free;
{$IFDEF D4_OR_HIGHER}
  Images.Free;  //70
{$ENDIF}
  DeleteObject(Bmp1); //70
  DeleteObject(Bmp2);
  DeleteObject(Bmp3);
  DeleteObject(Bmp4);
  DeleteObject(Bmp5);
  DeleteObject(Bmp6);
  DeleteObject(Bmp7);
  DeleteObject(Bmp8);
  DeleteObject(Bmp9);
  DeleteObject(Bmp10);
  DeleteObject(Bmp11);
  DeleteObject(Bmp12); //70

  inherited Destroy;
end; {Destroy}

function TLsFilelistView28PopUp.AddNewItem(const aCaption: string;
  aShortCut: TShortCut; aChecked, aEnabled: Boolean;
  aGroup: integer; aOnClick: TNotifyEvent;
  hCtx: word; const aName: string; aBitMap: HBitmap): TMenuItem; //70
begin
  Result := TMenuItem.Create(nil);
  with result do
  begin
    Caption := aCaption;
    ShortCut := aShortCut;
    Checked := aChecked;
    Enabled := aEnabled;
    GroupIndex := aGroup;
    OnClick := aOnClick;
    Name := aName;
{$IFDEF D4_OR_HIGHER}
    BITMAP.Handle := aBitmap;
{$ENDIF}
  end;
end; {AddNewItem}


procedure TLsFilelistView28PopUp.SetFileListView(Value: TLsFilelistView28);
begin
  FFileListView := Value;
end; {SetFileListView}


procedure TLsFilelistView28PopUp.GetSendToSubMenu;
var
  SendToDir,
    FName,
    FullName: string;
  win32FD: TWin32FindData;
  hFindFile: THandle;
  sfi: TSHFileInfo;
  i: integer;
  NewItem: TMenuItem;

  function GetShellPath(Handle: THandle; var DestPath: string;
    nFldr: integer): Boolean;
  var
    ShellMalloc: IMALLOC;
    shBuff: pChar;
    idRoot: PItemIDList;
  begin
    Result := False;
    SetLength(DestPath, MAX_PATH);
    if CoGetMalloc(1, ShellMalloc) = NOERROR then
    try
      shBuff := PChar(ShellMalloc.Alloc(MAX_PATH));
      if assigned(shBuff) then
      begin
        SHGetSpecialFolderLocation(Handle, nFldr, idRoot);
        // Convert idRoot to a file system path and pass to shBuff.
        if SHGetPathFromIDList(idRoot, shBuff) then
        begin
          DestPath := shBuff;
          Result := True;
        end;
      end;
    finally
      ShellMalloc.Free(idRoot);
      ShellMalloc._Release; //53
      //  ShellMalloc.Free(shBuff);   //53
    end;
  end; {GetShellPath}

begin
  SendToList.Clear;
  i := 0;
  if GetShellPath(Handle, SendToDir, CSIDL_SENDTO) then
  begin
    hFindFile := FindFirstFile(PChar(SlashSep(SendToDir, '*.LNK')), win32FD);
    if hFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        with win32FD do
        begin
          FName := StrPas(cFileName);
          if (FName = '.') or (FName = '..') then continue;
          FullName := SlashSep(SendToDir, FName);
          SHGetFileInfo(PChar(FullName), 0, sfi, SizeOf(sfi),
            SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME); //60

          NewItem := AddNewItem(StrPas(sfi.szDisplayName), 0, False,
            True, 1, ItemOnClick, 0, 'Send_To' + IntToStr(i + 4), 0);
{$IFDEF D4_OR_HIGHER}
          NewItem.ImageIndex := sfi.iIcon; //60
{$ENDIF}
          Items.Items[3].Add(NewItem);
          SendToList.Add(FullName);
          inc(i);
        end;
      until not FindNextFile(hFindFile, win32FD);
    finally
      Windows.FindClose(hFindFile);
    end;
  end;
end; {GetSendToSubMenu}


procedure TLsFilelistView28PopUp.BuildItems; //70
begin
  Items.Add(AddNewItem('&Open', 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpen', Bmp1)); //Items[0]
  Items.Add(AddNewItem('&View', 0, False, True, 0,
    ItemOnClick, 0, 'ItemView', Bmp2)); //Items[1]
  Items.Add(NewLine);
  Items.Add(AddNewItem('Se&nd To', 0, False, True, 0,
    ItemOnClick, 0, 'SubMenuSend', Bmp3)); //Items[3]
  Items.Add(NewLine);
  Items.Add(AddNewItem('Cu&t', 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpCut', Bmp4)); //Items[5]
  Items.Add(AddNewItem('&Copy', 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpCopy', Bmp5)); //Items[6]
  Items.Add(AddNewItem('&Paste', 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpPaste', Bmp6)); //Items[7]
  Items.Add(NewLine);
  Items.Add(AddNewItem('&Rename', 0, False, True, 0,
    ItemOnClick, 0, 'ItemRename', Bmp7)); //Items[9]
  Items.Add(AddNewItem('&Delete', 0, False, True, 0,
    ItemOnClick, 0, 'ItemDelete', Bmp8)); //Items[10]
  Items.Add(NewLine);
  Items.Add(AddNewItem('&Attributes ...', 0, False, True, 0,
    ITemOnClick, 0, 'ItemFileAttr', Bmp9)); //Items[12]
  Items.Add(NewLine);
  Items.Add(AddNewItem('&New Folder ...', 0, False, True, 0,
    ItemOnClick, 0, 'ItemFolder', Bmp10)); //Items[14]
  // Items[3] SendTo SubItems
  Items.Items[3].Add(AddNewItem('Any Folder ...', 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(0), Bmp11));
  Items.Items[3].Add(AddNewItem('Clipboard as Contents', 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(1), Bmp12));
  Items.Items[3].Add(AddNewItem('Clipboard as FileName', 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(2), Bmp12));
  Items.Items[3].Add(NewLine);
  GetSendToSubMenu;
end; {BuildItems}

procedure TLsFilelistView28PopUp.ItemOnClick(Sender: TObject);
var
  i: integer;
begin
  if TMenuItem(Sender).Name = 'ItemOpen' then
    FFileListView.OpenItem
  else if TMenuItem(Sender).Name = 'ItemView' then
    FFileListView.ViewFile
  else if TMenuItem(Sender).Name = 'ItemOpCut' then
    FFileListView.CutCopy(2)
  else if TMenuItem(Sender).Name = 'ItemOpCopy' then
    FFileListView.CutCopy(0)
  else if TMenuItem(Sender).Name = 'ItemOpPaste' then
    FFileListView.Paste
  else if TMenuItem(Sender).Name = 'ItemRename' then
    FFileListView.RenameFile
  else if TMenuItem(Sender).Name = 'ItemDelete' then
    FFileListView.DeleteItems
  else if TMenuItem(Sender).Name = 'ItemFileAttr' then
    FFileListView.FileAttr
  else if TMenuItem(Sender).Name = 'ItemFolder' then
    FFileListView.NewFolder
  else if Pos('Send_To', TMenuItem(Sender).Name) = 1 then
  begin
    i := StrToIntDef(Copy(TMenuItem(Sender).Name, 8, 2), -1);
    if (i > -1) and (i < 3) then
      FFileListView.SendTo(i)
    else if (i > 3) then
      FFileListView.SendTo2(SendToList[i - 4]);
  end
end; {ItemOnClick}

procedure TLsFilelistView28PopUp.PopUp(X, Y: integer);
var
  i: integer;
  Ext: string;
begin
  Ext := '';
  if (FFileListView.Selected <> nil) then
    Ext := (Uppercase(ExtractFileExt(FFileListView.Selected.SubItems[4]))
      );
  Items[0].Enabled := FFileListView.Selected <> nil;
  Items[1].Enabled := False;
  if (FFileListView.Selected <> nil) then
  begin
    for i := Low(FileExt) to High(FileExt) do
      if (Ext = FileExt[i]) then
        Items[1].Enabled := True;
  end;
  Items[3].Enabled := FFileListView.Selected <> nil;
  Items[5].Enabled := FFileListView.Selected <> nil;
  Items[6].Enabled := FFileListView.Selected <> nil;
  Items[7].Enabled := FFileListView.FSelectedFiles.Count <> 0;
  Items[9].Enabled := (FFileListView.Selected <> nil) and
    (FFileListView.SelCount = 1);
  Items[10].Enabled := FFileListView.Selected <> nil;
  Items[12].Enabled := FFileListView.Selected <> nil;
  Items[14].Enabled := UpperCase(FFileListView.Directory) <> 'DRIVES';

  Items[3][1].Enabled := FFileListView.Selected <> nil;
  Items[3][2].Enabled := FFileListView.Selected <> nil;
  Items[3][3].Enabled := False;
  if (FFileListView.Selected <> nil) and (FFileListView.SelCount = 1) then
  begin
    for i := Low(FileExt) to High(FileExt) do
      if (Ext = FileExt[i]) then
        Items[3][3].Enabled := True;
    if (Ext = '.BMP') or (Ext = '.WMF') then
      Items[3][3].Enabled := True;
  end;
  Items[3][4].Enabled := FFileListView.Selected <> nil;
  inherited Popup(X + 10, Y + 10);
end; {PopUp}

///// End of TLsFilelistView28PopUp /////

                                          
{ ============================= Register =============================== }

procedure Register;
begin
  RegisterComponents('AD-2', [TLsDirTree21]);
  RegisterComponents('AD-2', [TLsDirTreeCombo28]);
  RegisterComponents('AD-2', [TLsFilelistView28]);
end;

end.

