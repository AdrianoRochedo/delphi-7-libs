unit TeeX_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ TeeX Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_TeeX: TGUID = '{6EFF2420-BF09-11D0-B4DF-00A0C901D681}';

const

{ TxActiveFormBorderStyle }

  afbNone = 0;
  afbSingle = 1;
  afbSunken = 2;
  afbRaised = 3;

{ TxPrintScale }

  poNone = 0;
  poProportional = 1;
  poPrintToFit = 2;

{ TxMouseButton }

  mbLeft = 0;
  mbRight = 1;
  mbMiddle = 2;

{ TxWindowState }

  wsNormal = 0;
  wsMinimized = 1;
  wsMaximized = 2;

const

{ Component class GUIDs }
  Class_ChartX: TGUID = '{6EFF2423-BF09-11D0-B4DF-00A0C901D681}';

type

{ Forward declarations: Interfaces }
  ITeeAxForm = interface;
  ITeeAxFormDisp = dispinterface;
  IChartXEvents = dispinterface;

{ Forward declarations: CoClasses }
  ChartX = ITeeAxForm;

{ Forward declarations: Enums }
  TxActiveFormBorderStyle = TOleEnum;
  TxPrintScale = TOleEnum;
  TxMouseButton = TOleEnum;
  TxWindowState = TOleEnum;

{ Dispatch interface for TeeChartX Control }

  ITeeAxForm = interface(IDispatch)
    ['{6EFF2421-BF09-11D0-B4DF-00A0C901D681}']
    function Get_AutoScroll: WordBool; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: Integer; safecall;
    procedure Set_Color(Value: Integer); safecall;
    function Get_KeyPreview: WordBool; safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    function Get_PixelsPerInch: Integer; safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    function Get_Scaled: WordBool; safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    function Get_Active: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    function Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    function Get_WindowState: TxWindowState; safecall;
    procedure Set_WindowState(Value: TxWindowState); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    function Get_ChartColor: Integer; safecall;
    procedure Set_ChartColor(Value: Integer); safecall;
    property AutoScroll: WordBool read Get_AutoScroll write Set_AutoScroll;
    property AxBorderStyle: TxActiveFormBorderStyle read Get_AxBorderStyle write Set_AxBorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: Integer read Get_Color write Set_Color;
    property KeyPreview: WordBool read Get_KeyPreview write Set_KeyPreview;
    property PixelsPerInch: Integer read Get_PixelsPerInch write Set_PixelsPerInch;
    property PrintScale: TxPrintScale read Get_PrintScale write Set_PrintScale;
    property Scaled: WordBool read Get_Scaled write Set_Scaled;
    property Active: WordBool read Get_Active;
    property DropTarget: WordBool read Get_DropTarget write Set_DropTarget;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property WindowState: TxWindowState read Get_WindowState write Set_WindowState;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
    property ChartColor: Integer read Get_ChartColor write Set_ChartColor;
  end;

{ DispInterface declaration for Dual Interface ITeeAxForm }

  ITeeAxFormDisp = dispinterface
    ['{6EFF2421-BF09-11D0-B4DF-00A0C901D681}']
    property AutoScroll: WordBool dispid 1;
    property AxBorderStyle: TxActiveFormBorderStyle dispid 2;
    property Caption: WideString dispid 3;
    property Color: Integer dispid 4;
    property KeyPreview: WordBool dispid 5;
    property PixelsPerInch: Integer dispid 6;
    property PrintScale: TxPrintScale dispid 7;
    property Scaled: WordBool dispid 8;
    property Active: WordBool readonly dispid 9;
    property DropTarget: WordBool dispid 10;
    property HelpFile: WideString dispid 11;
    property WindowState: TxWindowState dispid 12;
    property Visible: WordBool dispid 13;
    property Enabled: WordBool dispid 14;
    property Cursor: Smallint dispid 15;
    property ChartColor: Integer dispid 16;
  end;

{ Events interface for TeeChartX Control }

  IChartXEvents = dispinterface
    ['{6EFF2422-BF09-11D0-B4DF-00A0C901D681}']
    procedure OnActivate; dispid 1;
    procedure OnClick; dispid 2;
    procedure OnCreate; dispid 3;
    procedure OnDblClick; dispid 4;
    procedure OnDestroy; dispid 5;
    procedure OnDeactivate; dispid 6;
    procedure OnKeyPress(var Key: Smallint); dispid 7;
    procedure OnPaint; dispid 8;
  end;

{ TeeChartX }

  TChartXOnKeyPress = procedure(Sender: TObject; var Key: Smallint) of object;

  TChartX = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TChartXOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: ITeeAxForm;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    property ControlInterface: ITeeAxForm read FIntf;
    property Active: WordBool index 9 read GetWordBoolProp;
  published
    property Align;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property AutoScroll: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property AxBorderStyle: TxActiveFormBorderStyle index 2 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property Color: Integer index 4 read GetIntegerProp write SetIntegerProp stored False;
    property KeyPreview: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property PixelsPerInch: Integer index 6 read GetIntegerProp write SetIntegerProp stored False;
    property PrintScale: TxPrintScale index 7 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Scaled: WordBool index 8 read GetWordBoolProp write SetWordBoolProp stored False;
    property DropTarget: WordBool index 10 read GetWordBoolProp write SetWordBoolProp stored False;
    property HelpFile: WideString index 11 read GetWideStringProp write SetWideStringProp stored False;
    property WindowState: TxWindowState index 12 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Visible: WordBool index 13 read GetWordBoolProp write SetWordBoolProp stored False;
    property Enabled: WordBool index 14 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 15 read GetSmallintProp write SetSmallintProp stored False;
    property ChartColor: Integer index 16 read GetIntegerProp write SetIntegerProp stored False;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TChartXOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

implementation

uses ComObj;

procedure TChartX.InitControlData;
const
  CEventDispIDs: array[0..7] of Integer = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $00000007, $00000008);
  CControlData: TControlData = (
    ClassID: '{6EFF2423-BF09-11D0-B4DF-00A0C901D681}';
    EventIID: '{6EFF2422-BF09-11D0-B4DF-00A0C901D681}';
    EventCount: 8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TChartX.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as ITeeAxForm;
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TChartX]);
end;

end.
