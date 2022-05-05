{*********************************************************}
{*                  RIEDITU2.PAS 1.05                    *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit RIEditU2;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Mask;

type
  EditingStateType = (etAll, etName, etValue);

  TDataDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DataTypeRG: TRadioGroup;
    Label1: TLabel;
    ValueName: TEdit;
    Label2: TLabel;
    IData: TMaskEdit;
    procedure DataTypeRGClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EditingState   : EditingStateType;
    RGIdx : Integer;
  end;

var
  DataDlg: TDataDlg;

implementation

{$R *.DFM}


procedure TDataDlg.DataTypeRGClick(Sender: TObject);
begin
  case DataTypeRG.ItemIndex of
    0,
    1,
    5 : begin
          IData.MaxLength := 256;
          IData.EditMask := '';
        end;
    2,
    3 : begin
          IData.MaxLength := 10;
          IData.EditMask := '';
        end;
    4 : begin
          IData.MaxLength := 15;
          IData.EditMask := '';
        end;
    6 : begin
          IData.MaxLength := 12;
          IData.EditMask := '';
        end;
  end;
  IData.SetFocus;
end;

procedure TDataDlg.FormActivate(Sender: TObject);
begin
  DataTypeRG.ItemIndex := RGIdx;
  case EditingState of
    etAll   : begin
                DataTypeRG.Enabled := True;
                IData.Enabled := True;
                ValueName.Enabled := True;
                DataTypeRG.ItemIndex := 5;
                ValueName.SetFocus;
              end;

    etName  : begin
                DataTypeRG.Enabled := False;
                ValueName.Enabled := True;
                IData.Enabled := False;
                DataTypeRG.ItemIndex := RGIdx;
                ValueName.SetFocus;
              end;

    etValue : begin
                DataTypeRG.Enabled := False;
                ValueName.Enabled := False;
                IData.Enabled := True;
                DataTypeRG.ItemIndex := RGIdx;
                IData.SetFocus;
              end;
  end;
end;

end.
