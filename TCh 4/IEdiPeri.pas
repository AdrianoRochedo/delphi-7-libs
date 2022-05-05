{****************************************}
{     TeeChart Pro Charting Library      }
{  For Delphi 1,2,3,4 & C++ Builder 1&3  }
{ Copyright (c) 1995-98 by David Berneda }
{         All Rights Reserved            }
{****************************************}
unit IEdiPeri;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Teengine;

type
  TFormPeriod = class(TForm)
    ENum: TEdit;
    CBAll: TCheckBox;
    RBNum: TRadioButton;
    RBRange: TRadioButton;
    BLabel: TLabel;
    BChange: TButton;
    Bevel1: TBevel;
    Button2: TButton;
    Button3: TButton;
    LCalc: TLabel;
    RGAlign: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure RBRangeClick(Sender: TObject);
    procedure RBNumClick(Sender: TObject);
    procedure BChangeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CBAllClick(Sender: TObject);
    procedure ENumChange(Sender: TObject);
    procedure RGAlignClick(Sender: TObject);
  private
    { Private declarations }
    Procedure EnableControls(Value:Boolean);
  public
    { Public declarations }
    ThePeriod:Double;
    ThePeriodStyle:TFunctionPeriodStyle;
    TheCanRange:Boolean;
    TheAlignment:TFunctionPeriodAlign;
    TheIsDateTime:Boolean;
  end;

implementation

{$R *.DFM}
Uses AxisIncr,TeeConst,TeeProcs;

procedure TFormPeriod.FormShow(Sender: TObject);
begin
  if (ThePeriodStyle=psRange) and TheCanRange then RBRange.Checked:=True
                                              else RBNum.Checked:=True;
  RGAlign.ItemIndex:=Ord(TheAlignment);
  CBAll.Checked:=ThePeriod=0;
  if RBNum.Checked then
  begin
    if ThePeriod<0 then ThePeriod:=0;
    ThePeriod:=Round(ThePeriod);
  end;
  ENum.Text:=FloatToStr(ThePeriod);
  BLabel.Caption:=GetIncrementText(Self,ThePeriod,TheIsDateTime,True,'');
  RBRange.Enabled:=TheCanRange;
  BChange.Enabled:=TheCanRange;
  EnableControls(RBNum.Checked)
end;

Procedure TFormPeriod.EnableControls(Value:Boolean);
begin
  CBAll.Enabled:=Value;
  ENum.Enabled:=Value;
  BChange.Enabled:=not Value;
end;

procedure TFormPeriod.RBRangeClick(Sender: TObject);
begin
  if RBRange.Checked then
  begin
    ThePeriodStyle:=psRange;
    EnableControls(False);
    BChange.SetFocus;
  end;
end;

procedure TFormPeriod.RBNumClick(Sender: TObject);
begin
  if RBNum.Checked then
  begin
    ThePeriodStyle:=psNumPoints;
    EnableControls(True);
    ENum.SetFocus;
  end;
end;

procedure TFormPeriod.BChangeClick(Sender: TObject);
begin
  RBRange.Checked:=True;
  With TAxisIncrement.Create(Self) do
  try
    IsDateTime :=TheIsDateTime;
    IsExact    :=True;
    Increment  :=ThePeriod;
    IStep      :=FindDateTimeStep(ThePeriod);
    Caption    :=TeeMsg_PeriodRange;
    if ShowModal=mrOk then
    begin
      ThePeriod:=Increment;
      TheIsDateTime:=IsDateTime;
      Self.BLabel.Caption:=GetIncrementText(Self,ThePeriod,TheIsDateTime,True,'');
      Self.CBAll.Checked:=ThePeriod=0;
    end;
  finally
    Free;
  end;
end;

procedure TFormPeriod.Button2Click(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TFormPeriod.Button3Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFormPeriod.CBAllClick(Sender: TObject);
begin
  if CBAll.Checked then
  begin
    ENum.Text:='0';
    ThePeriod:=0;
  end;
end;

procedure TFormPeriod.ENumChange(Sender: TObject);
begin
  ThePeriod:=StrToFloat(ENum.Text);
  CBAll.Checked:=ThePeriod=0;
end;

procedure TFormPeriod.RGAlignClick(Sender: TObject);
begin
  TheAlignment:=TFunctionPeriodAlign(RGAlign.ItemIndex);
end;

end.
