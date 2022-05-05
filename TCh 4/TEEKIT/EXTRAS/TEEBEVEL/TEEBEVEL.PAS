(****************************************
   TeeChart TeeBevel Component.
   Copyright 1995-1997 by David Berneda.
*****************************************)
unit TeeBevel;

interface

{ This component draws Bevels (3D frames) around the different
  parts of a Chart, DBChart or QRChart component.
}
Uses ExtCtrls,Chart,Classes,Graphics,WinTypes,WinProcs;

type TTeeBevelStyle=(tbNone, tbLoweredBox, tbRaisedBox,
                             tbLoweredFrame,tbRaisedFrame);

     TTeeBevel=class(TComponent)
     private
       FChart:TCustomChart;
       FAxis,
       FLegend,
       FTitle,
       FFoot:TTeeBevelStyle;
       FOldAfter:TNotifyEvent;
       FSize:Integer;
       FFirstColor,
       FSecondColor:TColor;
       Procedure SetChart(AChart:TCustomChart);
       Procedure SetAxis(Value:TTeeBevelStyle);
       Procedure SetLegend(Value:TTeeBevelStyle);
       Procedure SetTitle(Value:TTeeBevelStyle);
       Procedure SetFoot(Value:TTeeBevelStyle);
       procedure SetSize(Value: Integer);
       Procedure SetFirstColor(Value:TColor);
       procedure SetSecondColor(Value:TColor);
     protected
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
     public
       constructor Create(AOwner: TComponent); override;
       Procedure DrawBevels;
       procedure DrawRect(Const ARect:TRect; FStyle:TTeeBevelStyle);
       Procedure AfterDraw(Sender:TObject);
       Procedure Repaint;
     published
       property Chart:TCustomChart read FChart write SetChart;
       property Axis:TTeeBevelStyle read FAxis write SetAxis default tbRaisedBox;
       property Legend:TTeeBevelStyle read FLegend write SetLegend default tbRaisedBox;
       property Title:TTeeBevelStyle read FTitle write SetTitle default tbRaisedBox;
       property Foot:TTeeBevelStyle read FFoot write SetFoot default tbRaisedBox;
       property Size:Integer read FSize write SetSize default 1;
       property FirstColor:TColor read FFirstColor write SetFirstColor default clBtnShadow;
       property SecondColor:TColor read FSecondColor write SetSecondColor default clBtnHighlight;
     end;


Procedure Register;

implementation

Uses TeeProco;

constructor TTeeBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAxis:=tbRaisedBox;
  FLegend:=tbRaisedBox;
  FTitle:=tbRaisedBox;
  FFoot:=tbRaisedBox;
  FSize:=1;
  FFirstColor:=clBtnShadow;
  FSecondColor:=clBtnHighlight;
end;

Procedure TTeeBevel.SetFirstColor(Value:TColor);
begin
  if Value<>FFirstColor then
  begin
    FFirstColor:=Value;
    Repaint;
  end;
end;

procedure TTeeBevel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent=FChart) and (Operation=opRemove) then
  begin
    FChart:=nil;
    FOldAfter:=nil;
  end;
end;

procedure TTeeBevel.SetSecondColor(Value:TColor);
begin
  if Value<>FSecondColor then
  begin
    FSecondColor:=Value;
    Repaint;
  end;
end;

procedure TTeeBevel.DrawRect(Const ARect:TRect; FStyle:TTeeBevelStyle);
var Color1,Color2,temp: TColor;

  procedure BevelRect(const R: TRect);
  begin
    with FChart.Canvas do
    begin
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), R.TopLeft,Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), R.BottomRight,Point(R.Left, R.Bottom)]);
    end;
  end;

begin
  FChart.Canvas.Pen.Width := FSize;
  if (FStyle = tbLoweredBox) or
     (FStyle = tbLoweredFrame) then
  begin
    Color1 := FFirstColor;
    Color2 := FSecondColor;
  end
  else
  begin
    Color1 := FSecondColor;
    Color2 := FFirstColor;
  end;
  With ARect do
  if (FStyle=tbLoweredBox) or (FStyle=tbRaisedBox) then
     BevelRect(Classes.Rect( Left-FSize,Top-FSize,
                             Right+FSize,Bottom+FSize))
  else
  if (FStyle=tbLoweredFrame) or (FStyle=tbRaisedFrame) then
  begin
    Temp := Color1;
    Color1 := Color2;
    BevelRect(Classes.Rect(Left-FSize-1, Top-FSize-1, Right+FSize, Bottom+FSize));
    Color2 := Temp;
    Color1 := Temp;
    BevelRect(Classes.Rect(Left-FSize, Top-FSize, Right+FSize+1, Bottom+FSize+1));
  end;
end;

Procedure TTeeBevel.Repaint;
begin
  if Assigned(FChart) then FChart.Repaint;
end;

procedure TTeeBevel.SetSize(Value: Integer);
begin
  if Value<>FSize then
  begin
    FSize:=Value;
    Repaint;
  end;
end;

Procedure TTeeBevel.SetAxis(Value:TTeeBevelStyle);
begin
  if FAxis<>Value then
  begin
    FAxis:=Value;
    Repaint;
  end;
end;

Procedure TTeeBevel.SetLegend(Value:TTeeBevelStyle);
begin
  if FLegend<>Value then
  begin
    FLegend:=Value;
    Repaint;
  end;
end;

Procedure TTeeBevel.SetTitle(Value:TTeeBevelStyle);
begin
  if FTitle<>Value then
  begin
    FTitle:=Value;
    Repaint;
  end;
end;

Procedure TTeeBevel.SetFoot(Value:TTeeBevelStyle);
begin
  if FFoot<>Value then
  begin
    FFoot:=Value;
    Repaint;
  end;
end;

Procedure TTeeBevel.AfterDraw(Sender:TObject);
begin
  DrawBevels;
  if Assigned(FChart) and Assigned(FOldAfter) then FOldAfter(FChart);
end;

Procedure TTeeBevel.SetChart(AChart:TCustomChart);
begin
  if FChart<>AChart then
  begin
    if Assigned(FChart) then
    begin
      FChart.OnAfterDraw:=FOldAfter;
      FChart.Repaint;
    end;
    FChart:=AChart;
    if Assigned(FChart) then
    begin
      FOldAfter:=FChart.OnAfterDraw;
      FChart.OnAfterDraw:=AfterDraw;
      With FChart do
      begin
        With (Legend as TChartLegend) do
        begin
          ShadowSize:=0;
          Color:=clBtnFace;
          Frame.Visible:=False;
        end;
        Frame.Visible:=False;
        LeftAxis.Axis.Visible:=False;
        TopAxis.Axis.Visible:=False;
        RightAxis.Axis.Visible:=False;
        BottomAxis.Axis.Visible:=False;
        View3D:=False;
        Repaint;
      end;
    end
    else FOldAfter:=nil;
  end;
end;

Procedure TTeeBevel.DrawBevels;
begin
  if Assigned(FChart) then
  begin
    if FAxis<>tbNone then DrawRect(FChart.BackWallRect,FAxis);
    With FChart.Legend do
      if Visible then DrawRect(RectLegend,FLegend);
    With FChart.Title do
      if Visible and (Text.Count>0) then DrawRect(TitleRect,FTitle);
    With FChart.Foot do
      if Visible and (Text.Count>0) then DrawRect(TitleRect,FFoot);
  end;
end;

Procedure Register;
begin
  RegisterComponents(TeeMsg_TeeChartPalette,[TTeeBevel]);
end;

end.
