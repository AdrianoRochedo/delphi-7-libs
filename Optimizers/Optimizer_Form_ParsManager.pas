unit Optimizer_Form_ParsManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Optimizer_Frame_ParManager, ExtCtrls;

type
  TfoParsManager = class(TForm)
    Panel1: TPanel;
    paSim: TPanel;
    Panel5: TPanel;
    paFrames: TScrollBox;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    procedure FormShow(Sender: TObject);
  private
    procedure SetSimulationCount(const Value: Integer);
  public
    function CreateFrame(i: Integer): TfrParManager;
    procedure Clear;

    property SimulationCount : Integer write SetSimulationCount;
  end;

implementation

{$R *.dfm}

{ TrbFormParMan }

procedure TfoParsManager.Clear;
var i: Integer;
    c: TComponent;
begin
  for i := paFrames.ComponentCount - 1 downto 0 do
    begin
    c := paFrames.Components[i];
    if c is TFrame then
       begin
       paFrames.RemoveComponent(c);
       c.Free;
       end;
    end;
end;

function TfoParsManager.CreateFrame(i: Integer): TfrParManager;
begin
  Result := TfrParManager.Create(nil);
  Result.Align := alTop;
  Result.Parent := paFrames;
  Result.Name := 'P' + IntToStr(i);
  paFrames.InsertComponent(Result);
  if i < 18 then ClientHeight := Result.Height * i + paFrames.Top + 1;
end;

procedure TfoParsManager.SetSimulationCount(const Value: Integer);
begin
  paSim.Caption := ' ' + IntToStr(Value);
  paSim.Refresh();
end;

procedure TfoParsManager.FormShow(Sender: TObject);
begin
  Invalidate();
end;

end.
