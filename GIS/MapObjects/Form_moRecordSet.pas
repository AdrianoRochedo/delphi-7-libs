unit Form_moRecordSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MapObjects;

type
  TForm_moRecordSet = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRecordSet: IMoRecordset;
    procedure SetRecordSet(const Value: IMoRecordset);
    { Private declarations }
  public
    class procedure ShowRecordSet(const Caption: String;
                                  RS: ImoRecordSet;
                                  FormStyle: TFormStyle = fsNormal);

    property RecordSet: IMoRecordset read FRecordSet write SetRecordSet;
  end;

implementation
uses Frame_moRecordSet, Frame_moRecord;

{$R *.dfm}

{ TForm_moRecordSet }

class procedure TForm_moRecordSet.ShowRecordSet(
                                     const Caption: String;
                                     RS: ImoRecordSet;
                                     FormStyle: TFormStyle = fsNormal);
var d: TForm_moRecordSet;
begin
  d := TForm_moRecordSet.Create(nil);
  d.Caption := Caption;
  d.RecordSet := RS;
  d.FormStyle := FormStyle;
  d.Show;
end;

procedure TForm_moRecordSet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TForm_moRecordSet.SetRecordSet(const Value: IMoRecordset);
var f: TFrame;
begin
  FRecordSet := Value;
  if FRecordSet.Count > 1 then
     begin
     f := TFrame_moRecordSet.Create(self);
     SetBounds(Left, Top, 480, 240);
     TFrame_moRecordSet(f).RecordSet := FRecordSet;
     end
  else
     begin
     f := TFrame_moRecord.Create(self);
     SetBounds(Left, Top, 260, 350);
     TFrame_moRecord(f).RecordSet := FRecordSet;
     end;

  f.Parent := self;
  f.Align := alClient;
end;

end.
