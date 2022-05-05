{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiTitl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,Chart;

type
  TFormTeeTitle = class(TForm)
    CBTitles: TComboBox;
    CBVisible: TCheckBox;
    CBAdjust: TCheckBox;
    BFont: TButton;
    BFrame: TButton;
    BBrush: TButton;
    RGAlign: TRadioGroup;
    MText: TMemo;
    BColor: TButton;
    procedure BFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure MTextChange(Sender: TObject);
    procedure BFrameClick(Sender: TObject);
    procedure CBAdjustClick(Sender: TObject);
    procedure BBrushClick(Sender: TObject);
    procedure RGAlignClick(Sender: TObject);
    procedure CBTitlesChange(Sender: TObject);
    procedure BColorClick(Sender: TObject);
  private
    { Private declarations }
    AssigningText:Boolean;
    Procedure SetMemoFormat;
    procedure SetTitleControls;
  public
    { Public declarations }
    TheTitle:TChartTitle;
    Constructor CreateTitle(Owner:TComponent; ATitle:TChartTitle);
  end;

implementation

{$R *.DFM}
Uses TeEngine, PenDlg, BrushDlg;

Constructor TFormTeeTitle.CreateTitle(Owner:TComponent; ATitle:TChartTitle);
begin
  inherited Create(Owner);
  TheTitle:=ATitle;
  AssigningText:=False;
end;

Procedure TFormTeeTitle.SetMemoFormat;
begin
  MText.Font:=TheTitle.Font;
  if TheTitle.Color=clTeeColor then
  begin
    if MText.Font.Color=clWhite then MText.Color:=clDkGray
                                else MText.Color:=clWhite;
  end
  else MText.Color:=TheTitle.Color;
end;

procedure TFormTeeTitle.BFontClick(Sender: TObject);
begin
  With TheTitle do Font:=InternalEditFont(Self,Font);
  SetMemoFormat;
end;

procedure TFormTeeTitle.SetTitleControls;
begin
  With TheTitle do
  begin
    Case Alignment of
      taLeftJustify:  RGAlign.ItemIndex:=0;
      taCenter:       RGAlign.ItemIndex:=1;
      taRightJustify: RGAlign.ItemIndex:=2;
    end;
    CBVisible.Checked :=Visible;
    CBAdjust.Checked  :=AdjustFrame;
    SetMemoFormat;
    AssigningText:=True;
    MText.Lines:=Text;
    AssigningText:=False;
  end;
end;

procedure TFormTeeTitle.FormShow(Sender: TObject);
begin
  if TheTitle=TCustomChart(TheTitle.ParentChart).Foot then
     CBTitles.ItemIndex:=1
  else
     CBTitles.ItemIndex:=0;
  SetTitleControls;
end;

procedure TFormTeeTitle.CBVisibleClick(Sender: TObject);
begin
  TheTitle.Visible:=CBVisible.Checked;
end;

procedure TFormTeeTitle.MTextChange(Sender: TObject);

  Function EqualStrings(A,B:TStrings):Boolean;
  var t:Integer;
  begin
    result:=A.Count=B.Count;
    if result then
      for t:=0 to A.Count-1 do
      begin
        result:=A[t]=B[t];
        if not result then break;
      end;
  end;

begin
  if not AssigningText then
  With TheTitle do
  if not EqualStrings(Text,MText.Lines) then Text:=MText.Lines;
end;

procedure TFormTeeTitle.BFrameClick(Sender: TObject);
begin
  EditChartPen(Self,TheTitle.Frame);
end;

procedure TFormTeeTitle.CBAdjustClick(Sender: TObject);
begin
  TheTitle.AdjustFrame:=CBAdjust.Checked;
end;

procedure TFormTeeTitle.BBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,TheTitle.Brush);
end;

procedure TFormTeeTitle.RGAlignClick(Sender: TObject);
begin
  With TheTitle do
  Case RGAlign.ItemIndex of
    0: Alignment:=taLeftJustify;
    1: Alignment:=taCenter;
    2: Alignment:=taRightJustify;
  end;
end;

procedure TFormTeeTitle.CBTitlesChange(Sender: TObject);
begin
  With TCustomChart(TheTitle.ParentChart) do
  if CBTitles.ItemIndex=0 then TheTitle:=Title
                          else TheTitle:=Foot;
  SetTitleControls;
end;

procedure TFormTeeTitle.BColorClick(Sender: TObject);
begin
  With TheTitle do Color:=EditColor(Self,Color);
  SetMemoFormat;
end;

end.
