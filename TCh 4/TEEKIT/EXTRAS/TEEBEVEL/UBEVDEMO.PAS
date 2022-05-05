(****************************************
   TeeChart TeeBevel component example.
   Copyright 1995-1997 by David Berneda.
*****************************************)
unit ubevdemo;

interface

{
  WARNING:   Please install first the TeeBevel.pas unit at Delphi
             component palette.

}

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, StdCtrls, teebevel, Spin;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    TeeBevel1: TTeeBevel;
    Panel1: TPanel;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    ColorDialog1: TColorDialog;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Longint; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Chart1ClickBackground(Sender: TCustomChart;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses EditChar;

{ Prepare the Chart properties.... }

procedure TForm1.FormCreate(Sender: TObject);
begin
  { some random values.... }
  Series1.FillSampleValues(10);
  Series2.FillSampleValues(10);
  Series3.FillSampleValues(10);

  { remove all pens to not conflict with the bevels... }
  Chart1.Frame.Visible:=False;
  Chart1.BottomAxis.Axis.Visible:=False;
  Chart1.LeftAxis.Axis.Visible:=False;
  Chart1.Legend.Frame.Visible:=False;

  { set default Bevel style... }
  ComboBox1.ItemIndex:=2;
  ComboBox2.ItemIndex:=2;
  ComboBox3.ItemIndex:=2;
  ComboBox4.ItemIndex:=2;

  { get bevel colors... }
  Shape1.Brush.Color:=TeeBevel1.FirstColor;
  Shape2.Brush.Color:=TeeBevel1.SecondColor;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  TeeBevel1.Foot:=TTeeBevelStyle(ComboBox4.ItemIndex); { change Foot bevel }
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  TeeBevel1.Title:=TTeeBevelStyle(ComboBox3.ItemIndex);  { change Title bevel }
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  TeeBevel1.Legend:=TTeeBevelStyle(ComboBox2.ItemIndex);  { change Legend bevel }
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  TeeBevel1.Axis:=TTeeBevelStyle(ComboBox1.ItemIndex);  { change Axis bevel }
end;

procedure TForm1.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { Choose a new Color for bevels... }
  With ColorDialog1 do
  begin
    Color:=Shape1.Brush.Color;
    if Execute then
    begin
      Shape1.Brush.Color:=Color;
      TeeBevel1.FirstColor:=Color;
    end;
  end;
end;

procedure TForm1.Shape2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { Choose a new Color for bevels... }
  With ColorDialog1 do
  begin
    Color:=Shape2.Brush.Color;
    if Execute then
    begin
      Shape2.Brush.Color:=Color;
      TeeBevel1.SecondColor:=Color;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);  { <-- edit the Chart1 }
end;

procedure TForm1.Chart1ClickLegend(Sender: TCustomChart;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { change Legend Bevel style when clicking inside the Legend... }
  With TeeBevel1 do
  if Legend=tbLoweredBox then Legend:=tbRaisedBox
                         else Legend:=tbLoweredBox;
end;

procedure TForm1.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Longint; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { change Axis Bevel style when clicking any Point of any Series... }
  With TeeBevel1 do
  if Axis=tbLoweredBox then Axis:=tbRaisedBox
                       else Axis:=tbLoweredBox;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  TeeBevel1.Size:=RadioGroup1.ItemIndex+1;  { change Bevel sizes... }
end;

procedure TForm1.Chart1ClickBackground(Sender: TCustomChart;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if PtInRect(Chart1.Title.TitleRect,Point(x,y)) then
  With TeeBevel1 do
  if Title=tbLoweredBox then Title:=tbRaisedBox
                        else Title:=tbLoweredBox;
end;

end.
