unit Optimizer_Form_ParsManager_Rosen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Optimizer_Form_ParsManager, ExtCtrls;

type
  TfoParsManager_Rosen = class(TfoParsManager)
    Panel3: TPanel;
    paRot: TPanel;
  private
    procedure SetRotationCount(const Value: Integer);
  public
    property RotationCount: Integer write SetRotationCount;
  end;

implementation

{$R *.dfm}

{ TfoParsManager_Rosen }

procedure TfoParsManager_Rosen.SetRotationCount(const Value: Integer);
begin
  paRot.Caption := ' ' + IntToStr(Value);
  paRot.Refresh();
end;

end.
