
{
  public and published declarations ready to use with pas2rai2.exe :
  - constructor Create added to all classes;
  - property published without types completly redeclared
}


unit IH_ComCtrls;

interface

uses Messages, Windows, SysUtils, CommCtrl, Classes, Controls, Forms,
  Menus, Graphics, StdCtrls, RichEdit, ToolWin;

type

  THackCustomTabControl = class(TWinControl)
  public
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property ScrollOpposite: Boolean read FScrollOpposite
      write SetScrollOpposite default False;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
  end;

  THackCustomTreeView = class(TWinControl)
  public
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnCompare: TTVCompareEvent read FOnCompare write FOnCompare;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property ShowButtons: Boolean read FShowButtons write SetButtonStyle default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ShowLines: Boolean read FShowLines write SetLineStyle default True;
    property ShowRoot: Boolean read FShowRoot write SetRootStyle default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default False;
    property Indent: Integer read GetIndent write SetIndent;
    property Items: TTreeNodes read FTreeNodes write SetTreeNodes;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property Images: TImageList read FImages write SetImages;
    property StateImages: TImageList read FStateImages write SetStateImages;
  end;

  THackCustomRichEdit = class(TCustomMemo)
  public
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HideScrollBars: Boolean read FHideScrollBars
      write SetHideScrollBars default True;
    property Lines: TStrings read FRichEditStrings write SetRichEditStrings;
    property OnSaveClipboard: TRichEditSaveClipboard read FOnSaveClipboard
      write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnProtectChange: TRichEditProtectChange read FOnProtectChange
      write FOnProtectChange;
    property OnResizeRequest: TRichEditResizeEvent read FOnResizeRequest
      write FOnResizeRequest;
    property PlainText: Boolean read GetPlainText write SetPlainText default False;
  end;

  THackCustomUpDown = class(TWinControl)
  public
    property AlignButton: TUDAlignButton read FAlignButton write SetAlignButton default udRight;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property Min: SmallInt read FMin write SetMin;
    property Max: SmallInt read FMax write SetMax default 100;
    property Increment: Integer read FIncrement write SetIncrement default 1;
    property Orientation: TUDOrientation read FOrientation write SetOrientation default udVertical;
    property Position: SmallInt read GetPosition write SetPosition;
    property Thousands: Boolean read FThousands write SetThousands default True;
    property Wrap: Boolean read FWrap write SetWrap;
    property OnChanging: TUDChangingEvent read FOnChanging write FOnChanging;
    property OnClick: TUDClickEvent read FOnClick write FOnClick;
  end;

  THackCustomHotKey = class(TWinControl)
  public
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys;
    property Modifiers: THKModifiers read FModifiers write SetModifiers;
    property HotKey: TShortCut read GetHotKey write SetHotKey;
    property TabStop default True;
  end;


  THackCustomListView = class(TWinControl)
  public
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: TListColumns read FListColumns write SetListColumns;
    property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property IconOptions: TIconOptions read FIconOptions write SetIconOptions;
    property Items: TListItems read FListItems write SetItems;
    property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
    property LargeImages: TImageList read FLargeImages write SetLargeImages;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnChanging: TLVChangingEvent read FOnChanging write FOnChanging;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick
      write FOnColumnClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnEdited: TLVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    property OnInsert: TLVDeletedEvent read FOnInsert write FOnInsert;
    property ShowColumnHeaders: Boolean read FShowColumnHeaders write
      SetColumnHeaders default True;
    property SmallImages: TImageList read FSmallImages write SetSmallImages;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TImageList read FStateImages write SetStateImages;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsIcon;
  end;


end.
