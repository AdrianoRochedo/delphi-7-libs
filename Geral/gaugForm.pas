unit Gaugform;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, OvcBase, OvcMeter, ExtCtrls;

type
  TGaugeForm = class(TForm)
    OvcController1: TOvcController;
    OvcMeter1: TOvcMeter;
  private
    FMessage : String;
    FPercent : Integer;
    Procedure SetPercent(Value: Longint);
    Procedure SetMessage(Value: String);
  public
    Property Percent: Longint Write SetPercent;
    Property Message: String Write SetMessage;
  end;

implementation
{$R *.DFM}

Procedure TGaugeForm.SetPercent(Value: Longint);
Begin
  If Value <> FPercent Then
     Begin
     FPercent := Value;
     OvcMeter1.Percent := FPercent;
     Caption := Format(' %s - (%d%%)',[FMessage, FPercent]);
     End;
End;

Procedure TGaugeForm.SetMessage(Value: String);
Begin
  If Value <> FMessage Then Begin
     FMessage := Value;
     Caption := Format(' %s - (%d%%)',[FMessage, FPercent]);
  End;
End;

end.
