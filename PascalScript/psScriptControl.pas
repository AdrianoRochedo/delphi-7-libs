unit psScriptControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, psBASE, psCORE, ComCtrls, ExtCtrls, StdCtrls, Buttons, SysUtilsEx;

type
  TfoScriptControl = class(TForm, IProgressBar)
    Label1: TLabel;
    laName: TLabel;
    btnExec: TBitBtn;
    Bevel: TBevel;
    btnStop: TBitBtn;
    Label2: TLabel;
    PB: TProgressBar;
    laPBMes: TLabel;
    procedure btnStopClick(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
  private
    FScript: TPascalScript;
    procedure setScript(const Value: TPascalScript);
    procedure setMin(value: integer);
    procedure setMax(value: integer);
    procedure setValue(value: integer);
    procedure setMessage(const s: string);
  public
    constructor Create();
    property Script: TPascalScript read FScript write setScript;
  end;

implementation

{$R *.dfm}

{ TfoScriptControl }

constructor TfoScriptControl.Create();
begin
  inherited Create(nil);
  laPBMes.Caption := '';
  laName.Caption := '';
end;

procedure TfoScriptControl.setScript(const Value: TPascalScript);
begin
  FScript := Value;
  if FScript <> nil then
     begin
     btnStop.Enabled := true;
     //btnExec.Enabled := (FScript.Status = ssInactive);
     laName.Caption := ExtractFilename(FScript.Filename);
     end
  else
     begin
     btnStop.Enabled := false;
     btnExec.Enabled := false;
     laName.Caption := '';
     end;
end;

procedure TfoScriptControl.btnStopClick(Sender: TObject);
begin
  if FScript <> nil then
     FScript.Stop();
end;

procedure TfoScriptControl.btnExecClick(Sender: TObject);
begin
  if FScript <> nil then
     FScript.Execute();
end;

procedure TfoScriptControl.setMax(value: integer);
begin
  PB.Max := value;
end;

procedure TfoScriptControl.setMessage(const s: string);
begin
  laPBMes.Caption := s;
  Application.ProcessMessages();
end;

procedure TfoScriptControl.setMin(value: integer);
begin
  PB.Min := value;
end;

procedure TfoScriptControl.setValue(value: integer);
begin
  PB.Position := value;
end;

end.
