unit TeeChaIm;

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, TeeXTLB, ExtCtrls, TeeAx;

type
  TTeeChartX = class(TActiveXControl, ITeeChartX)
  private
    { Private declarations }
    FDelphiControl: TTeeChart;
    FEvents: ITeeChartXEvents;
  protected
    { Protected declarations }
    procedure InitializeControl; override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    function Get_ChartFile: WideString; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    procedure AboutBox; safecall;
    procedure Set_ChartFile(const Value: WideString); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_ToolBarVisible: WordBool; safecall;
    procedure Set_ToolBarVisible(Value: WordBool); safecall;
  end;

implementation

uses About1,Dialogs;

{ TTeeChartX }

procedure TTeeChartX.InitializeControl;
begin
  FDelphiControl := Control as TTeeChart;
end;

procedure TTeeChartX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ITeeChartXEvents;
end;

procedure TTeeChartX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
  DefinePropertyPage(Class_PropertyPage1);
    }
end;

function TTeeChartX.Get_ChartFile: WideString;
begin
  Result := WideString(FDelphiControl.ChartFile);
end;

function TTeeChartX.Get_Cursor: Smallint;
begin
  Result := Smallint(FDelphiControl.Cursor);
end;

function TTeeChartX.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TTeeChartX.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

procedure TTeeChartX.AboutBox;
begin
  ShowTeeChartXAbout;
end;

procedure TTeeChartX.Set_ChartFile(const Value: WideString);
begin
  FDelphiControl.ChartFile := String(Value);
end;

procedure TTeeChartX.Set_Cursor(Value: Smallint);
begin
  FDelphiControl.Cursor := TCursor(Value);
end;

procedure TTeeChartX.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TTeeChartX.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

function TTeeChartX.Get_ToolBarVisible: WordBool;
begin
  Result := FDelphiControl.ToolBarVisible;
end;

procedure TTeeChartX.Set_ToolBarVisible(Value: WordBool);
begin
  FDelphiControl.ToolBarVisible := Value;
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TTeeChartX,
    TTeeChart,
    Class_TeeChartX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL);
end.
