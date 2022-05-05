{**********************************************}
{   TAxisMaxMin Dialog Editor                  }
{   Copyright (c) 1996-98 David Berneda        }
{**********************************************}
{$I teedefs.inc}
unit AxMaxMin;

interface

uses
  WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, SysUtils;

type
  TAxisMaxMin = class(TForm)
    BitBtn1: TButton;
    BitBtn2: TButton;
    EMaximum: TEdit;
    EMinimum: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsDateTime : Boolean;
    MaxMin     : Double;
  end;

implementation

{$R *.DFM}
Uses TeeProcs,TeeConst;

procedure TAxisMaxMin.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  if IsDateTime then
  Begin
    if MaxMin>=1 then EMaximum.Text:=DateToStr(MaxMin)
                 else
                 Begin
                   Label1.Visible:=False;
                   EMaximum.Visible:=False;
                 end;
    EMinimum.Text:=TimeToStr(MaxMin);
  end
  else
  begin
    EMaximum.Hint:='';
    EMinimum.Hint:='';
    Label1.Caption:=TeeMsg_AxisDlgValue;
    EMaximum.Text:=FloatToStr(MaxMin);
    Label2.Visible:=False;
    EMinimum.Visible:=False;
  end;
end;

procedure TAxisMaxMin.BitBtn1Click(Sender: TObject);
begin
  try
    if IsDateTime then
    begin
      if EMaximum.Visible then
         MaxMin:=StrToDateTime(EMaximum.Text+' '+EMinimum.Text)
      else
         MaxMin:=StrToTime(EMinimum.Text);
    end
    else MaxMin:=StrToFloat(EMaximum.Text);
    ModalResult:=mrOk;
  except
    on E:Exception do
       ShowMessage(Format(TeeMsg_IncorrectMaxMinValue,[E.Message]));
  end;
end;

procedure TAxisMaxMin.BitBtn2Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.

