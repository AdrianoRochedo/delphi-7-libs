unit NPTee;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls,NPForm, NPPlugin, TeeProcs, TeEngine, Chart, ExtCtrls;

type
  TTeeChartPluginForm = class(TPluginForm)
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TheType:TChartSeriesClass;
    Procedure LoadChart(FileName:String);
    procedure LoadURL(URL:String);
  end;

var
  TeeChartPluginForm: TTeeChartPluginForm;

implementation

{$R *.DFM}
Uses Series,EditChar,TeeStore,Dialogs,TeePrevi;

procedure TTeeChartPluginForm.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

type TChartType=Record
        Name:String;
        AType:TChartSeriesClass;
     end;

Const NumTeeTypes=7;
      TeeTypes:Array[1..NumTeeTypes] of TChartType=(
        ( Name: 'Bar';      AType: TBarSeries ),
        ( Name: 'Line';     AType: TLineSeries ),
        ( Name: 'Point';    AType: TPointSeries ),
        ( Name: 'Pie';      AType: TPieSeries ),
        ( Name: 'HorizBar'; AType: THorizBarSeries ),
        ( Name: 'Area';     AType: TAreaSeries ),
        ( Name: 'FastLine'; AType: TFastLineSeries )
          );

Function WhatChartType(AParam:String; Var Index:Integer):TChartSeriesClass;
var t:Integer;
begin
  AParam:=UpperCase(AParam);
  for t:=1 to NumTeeTypes do
  if UpperCase(TeeTypes[t].Name)=AParam then
  begin
    Index:=t;
    result:=TeeTypes[t].AType;
    exit;
  end;
  Index:=-1;
  result:=TBarSeries;
end;

type TPluginColor=Record
          Name:String;
          Color:TColor;
     end;

Const NumColors=16;
      Colors:Array[1..NumColors] of TPluginColor=(
          ( Name:'Red'; Color:clRed),
          ( Name:'Green'; Color:clGreen),
          ( Name:'Yellow'; Color:clYellow),
          ( Name:'Blue'; Color:clBlue),
          ( Name:'White'; Color:clWhite),
          ( Name:'Gray'; Color:clGray),
          ( Name:'Fuchsia'; Color:clFuchsia),
          ( Name:'Teal'; Color:clTeal),
          ( Name:'Navy'; Color:clNavy),
          ( Name:'Maroon'; Color:clMaroon),
          ( Name:'Lime'; Color:clLime),
          ( Name:'Olive'; Color:clOlive),
          ( Name:'Purple'; Color:clPurple),
          ( Name:'Silver'; Color:clSilver),
          ( Name:'Aqua'; Color:clAqua),
          ( Name:'Black'; Color:clBlack)
        );

  Function WhatColor(Const AColor:String):TColor;
  var t:Integer;
      tmp:String;
  begin
    tmp:=Uppercase(AColor);
    for t:=1 to NumColors do
    begin
      if Uppercase(Colors[t].Name)=tmp then
      begin
        result:=Colors[t].Color;
        exit;
      end;
    end;
    result:=clWhite;
  end;

Const NumTeeParams=23;
      TeeParams:Array[1..NumTeeParams] of String=(
{1}         'View3D',
{2}         'TitleText',
{3}         'ChartType',
{4}         'Color',
{5}         'Chart3DPercent',
{6}         'Animate',
{7}         'LegendVisible',
{8}         'URLChart',
{9}         'ToolbarVisible',
{10}         'Labels',
{11}         'Series1',
{12}         'Series1Title',
{13}         'Series2',
{14}         'Series2Title',
{15}         'Series3',
{16}         'Series3Title',
{17}         'Series4',
{18}         'Series4Title',
{19}         'Series5',
{20}         'Series5Title',
{21}         'LabelsTitle',
{22}         'ValuesTitle',
{23}         'GradientColor'
          );

Function WhatParam(AParam:String):Integer;
var t:Integer;
begin
  AParam:=UpperCase(AParam);
  for t:=1 to NumTeeParams do
  if UpperCase(TeeParams[t])=AParam then
  begin
    result:=t;
    exit;
  end;
  result:=0;
end;

Function ParamBool(Const AParam:String):Boolean;
begin
  result:=UpperCase(AParam)='TRUE';
end;

Function ParamLong(AParam:String):Longint;
begin
  result:=StrToInt(AParam);
end;

Procedure TTeeChartPluginForm.LoadChart(FileName:String);
var tmpChart:TCustomChart;
begin
  Chart1.Free;
  tmpChart:=TChart.Create(Self);
  LoadChartFromFile(tmpChart,FileName);
  Chart1:=tmpChart as TChart;
  Chart1.Align:=alClient;
  Chart1.Parent:=Self;
end;

procedure TTeeChartPluginForm.LoadURL(URL:String);
var tmp:SmallInt;
    St,LocalFile:Array[0..255] of char;
begin
  if GetTempPath(SizeOf(St),St)=0 then
     raise Exception.Create('Cannot get the Windows TEMP path');
  if GetTempFileName(St,'TMPTEE',0,LocalFile)=0 then
     raise Exception.Create('Cannot get a TEMP file name');
  tmp:=NPN_GetURL( Plugin.Instance, PChar(URL), LocalFile );
  showmessage(localfile);
  if tmp <> NPERR_NO_ERROR then
    ShowMessage('Error loading: '+URL+' ('+IntToStr(tmp)+')')
  else
    LoadChart(LocalFile);
end;

procedure TTeeChartPluginForm.FormCreate(Sender: TObject);

   Procedure CheckNumSeries(SeriesNum:Integer);
   var tmpType:TChartSeriesClass;
       t:Integer;
       tmp:TChartSeries;
   begin
     if Chart1.SeriesCount<=SeriesNum then
     begin
       if Chart1.SeriesCount>0 then tmpType:=TChartSeriesClass(Chart1[0].ClassType)
                               else tmpType:=TheType;
       for t:=Chart1.SeriesCount to SeriesNum-1 do
       begin
         tmp:=tmpType.Create(Self);
         tmp.Marks.Style:=smsValue;
         Chart1.AddSeries(tmp);
       end;
     end;
   end;

   Procedure ProcessLabels(Const AText:String);
   var tmp:TStringList;
       t:Integer;
   begin
     CheckNumSeries(1);
     With Chart1[Chart1.SeriesCount-1] do
     begin
       Clear;
       tmp:=TStringList.Create;
       try
         tmp.CommaText:=AText;
         if tmp.Count>0 then
         begin
           for t:=0 to tmp.Count-1 do
           begin
             if Count<=t then
                Add(0,tmp[t],clTeeColor)
             else
                XLabel[t]:=tmp[t];
           end;
         end;
       finally
         tmp.Free;
       end;
     end;
   end;

   Procedure AddValues(SeriesNum:Integer; Const AText:String);
   var t:Integer;
       tmp:TStringList;
   begin
     CheckNumSeries(SeriesNum);
     With Chart1[SeriesNum-1] do
     begin
       tmp:=TStringList.Create;
       try
         tmp.CommaText:=AText;
         if tmp.Count>0 then
         begin
           for t:=0 to tmp.Count-1 do
           begin
             if Count<=t then
                Add(StrToFloat(tmp[t]),'',clTeeColor)
             else
                MandatoryValueList[t]:=StrToFloat(tmp[t]);
           end;
         end;
       finally
         tmp.Free;
       end;
     end;
   end;

   Procedure SeriesTitle(SeriesNum:Integer; Const AText:String);
   begin
     CheckNumSeries(SeriesNum);
     Chart1[SeriesNum-1].Title:=AText;
   end;

Var i,t:Integer;
    tmp1,tmp2:TColor;
    tmpType:TChartSeriesClass;
begin
  TheType:=TBarSeries;
  ComboBox1.ItemIndex:=0;
  with Plugin do
  for t:=0 to ParamNames.Count-1 do
  Case WhatParam(ParamNames[t]) of
    1: Chart1.View3D:=ParamBool(ParamValues[t]);
    2: Begin
         Chart1.Title.Text.Clear;
         Chart1.Title.Text.Add(ParamValues[t]);
       end;
    3: begin
         tmpType:=WhatChartType(ParamValues[t],i);
         ChangeAllSeriesType(Chart1,tmpType);
         ComboBox1.ItemIndex:=i-1;
       end;
    4: Chart1.Color:=ParamLong(ParamValues[t]);
    5: Chart1.Chart3DPercent:=ParamLong(ParamValues[t]);
    6: Timer1.Enabled:=ParamBool(ParamValues[t]);
    7: Chart1.Legend.Visible:=ParamBool(ParamValues[t]);
    8: begin
         Timer1.Enabled:=False;
         LoadURL(ParamValues[t]);
       end;
    9: Panel1.Visible:=ParamBool(ParamValues[t]);
   10: ProcessLabels(ParamValues[t]);
   11: AddValues(1,ParamValues[t]);
   12: SeriesTitle(1,ParamValues[t]);
   13: AddValues(2,ParamValues[t]);
   14: SeriesTitle(2,ParamValues[t]);
   15: AddValues(3,ParamValues[t]);
   16: SeriesTitle(3,ParamValues[t]);
   17: AddValues(4,ParamValues[t]);
   18: SeriesTitle(4,ParamValues[t]);
   19: AddValues(5,ParamValues[t]);
   20: SeriesTitle(5,ParamValues[t]);
   21: Chart1.BottomAxis.Title.Caption:=ParamValues[t];
   22: Chart1.LeftAxis.Title.Caption:=ParamValues[t];
   23: begin
         i:=Pos(',',ParamValues[t]);
         if i>0 then
         begin
           tmp1:=WhatColor(Copy(ParamValues[t],1,i-1));
           tmp2:=WhatColor(Copy(ParamValues[t],i+1,255));
         end
         else
         begin
           tmp1:=WhatColor(ParamValues[t]);
           tmp2:=tmp1;
         end;
         With Chart1.Gradient do
         begin
           Visible:=tmp1<>tmp2;
           StartColor:=tmp1;
           EndColor:=tmp2;
           if not Visible then Chart1.Color:=tmp1;
         end;
       end;
  end;
end;

procedure TTeeChartPluginForm.Timer1Timer(Sender: TObject);
begin
  if Chart1.SeriesCount>0 then
  begin
    if Chart1[0] is TPieSeries then
    With TPieSeries(Chart1[0]) do RotationAngle:=RotationAngle+5;
  end;
end;


procedure TTeeChartPluginForm.Button2Click(Sender: TObject);
begin
  ChartPreview(Self,Chart1);
end;

procedure TTeeChartPluginForm.ComboBox1Change(Sender: TObject);
var i:Integer;
begin
  ChangeAllSeriesType(Chart1,WhatChartType(ComboBox1.Items[ComboBox1.ItemIndex],i));
end;

initialization
  RegisterPluginForm( '', TTeeChartPluginForm ) ;
end.
