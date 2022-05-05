unit TeeXTLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ TeeX Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_TeeChartXControl: TGUID = '{008BBE7B-C096-11D0-B4E3-00A0C901D681}';

const

{ Component class GUIDs }
  Class_TeeChartX: TGUID = '{008BBE7E-C096-11D0-B4E3-00A0C901D681}';

type

{ Forward declarations: Interfaces }
  ITeeChartX = interface;
  ITeeChartXDisp = dispinterface;
  ITeeChartXEvents = dispinterface;

{ Forward declarations: CoClasses }
  TeeChartX = ITeeChartX;

{ Dispatch interface for TeeChartX Control }

  ITeeChartX = interface(IDispatch)
    ['{008BBE7C-C096-11D0-B4E3-00A0C901D681}']
    function Get_ChartFile: WideString; safecall;
    procedure Set_ChartFile(const Value: WideString); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure AboutBox; safecall;
    function Get_ToolBarVisible: WordBool; safecall;
    procedure Set_ToolBarVisible(Value: WordBool); safecall;
    property ChartFile: WideString read Get_ChartFile write Set_ChartFile;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
    property ToolBarVisible: WordBool read Get_ToolBarVisible write Set_ToolBarVisible;
  end;

{ DispInterface declaration for Dual Interface ITeeChartX }

  ITeeChartXDisp = dispinterface
    ['{008BBE7C-C096-11D0-B4E3-00A0C901D681}']
    property ChartFile: WideString dispid 1;
    property Visible: WordBool dispid 2;
    property Enabled: WordBool dispid 3;
    property Cursor: Smallint dispid 4;
    procedure AboutBox; dispid -552;
    property ToolBarVisible: WordBool dispid 5;
  end;

{ Events interface for TeeChartX Control }

  ITeeChartXEvents = dispinterface
    ['{008BBE7D-C096-11D0-B4E3-00A0C901D681}']
  end;

{ TeeChartXControl }

  TTeeChartX = class(TOleControl)
  private
    FIntf: ITeeChartX;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    procedure AboutBox;
    property ControlInterface: ITeeChartX read FIntf;
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
    property ChartFile: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property Visible: WordBool index 2 read GetWordBoolProp write SetWordBoolProp stored False;
    property Enabled: WordBool index 3 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 4 read GetSmallintProp write SetSmallintProp stored False;
    property ToolBarVisible: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
  end;

procedure Register;

implementation

uses ComObj;

procedure TTeeChartX.InitControlData;
const
  CControlData: TControlData = (
    ClassID: '{008BBE7E-C096-11D0-B4E3-00A0C901D681}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TTeeChartX.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as ITeeChartX;
end;

procedure TTeeChartX.AboutBox;
begin
  ControlInterface.AboutBox;
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TTeeChartX]);
end;

end.
