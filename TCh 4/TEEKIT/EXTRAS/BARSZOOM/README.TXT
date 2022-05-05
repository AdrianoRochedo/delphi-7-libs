==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

  Auto-resizing of Bar points when zooming
=============================================

The included BarZoom.dpr contains a new BarSeries
component that resizes Bars when zooming.

It also implements "Animated Scroll".

Please run this project and see the included UBarZoom.pas 
unit source code.


The animated scroll feature can be used generically by
reusing this code:

procedure TForm1.AnimatedScroll( Const Speed:Double );
var t:Integer;
begin
  for t:=1 to 5 do   { <-- 5 animation scroll steps... }
  begin
    TheAxis.Scroll(Speed,False);
    Chart1.Refresh;
  end;
end;

Where "TheAxis" should be chosen from Chart1.LeftAxis, 
TopAxis, RightAxis and/or BottomAxis.

-------------------------------------------------------------

