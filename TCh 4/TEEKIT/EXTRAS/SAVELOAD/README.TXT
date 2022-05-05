==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


 Saving and Loading Charts
=============================================

1 -- Introduction
2 -- How it works ?
3 -- Routines
4 -- Testing

---------------------------------------------

1 -- Introduction

The example project TEESAVIN.DPR shows you how to
store and reload Chart components, with all Chart and
Series properties and will all Series point values.

This demo is a small "Chart Designer" application.


2 -- How it works ?

The TEESTORE.PAS unit (included in TeeChart-Pro), 
contains the necessary routines to save and load Charts
to files and to standard Delphi streams.

This unit basically calls the Delphi "WriteComponent" 
method to store all Chart and Series properties, and then,
for each Series in the Chart, it writes all Series point
values.


3 -- Routines
--------------------

The included methods are:

{ Read a Chart from a file ( Chart1,'c:\demo.tee' ) }
Procedure LoadChartFromFile(Var AChart:TCustomChart; Const AName:String);

{ Write a Chart to a file ( Chart1,'c:\demo.tee' ) }
Procedure SaveChartToFile(AChart:TCustomChart; Const AName:String);

{ The same using TStream components (good for BLOB fields, etc)  }
Procedure LoadChartFromStream(Var AChart:TCustomChart; AStream:TStream);
Procedure SaveChartToStream(AChart:TCustomChart; AStream:TStream);



4 -- Testing
------------------

When you install TeeChart-Pro, you should see a new menu
item on the right-click component menu at design-time.

This new menu item is "Save Chart..." and allows you to 
create a *.TEE file and write the Chart into it.

Note: For DBCharts this option is disabled, as the DBChart
will reload all points again from the databases.


-- Saving 
----------------

The TEESAVIN.DPR project includes code to save and load charts.
You can refer to this sources.


Saving is very easy. You should only do the following:


   SaveChartToFile( MyChart, 'c:\mychart.tee' );


-- Loading
-----------------

Loading Charts it's a little bit more difficult than saving
them.

The following code does it:


Var tmpChart : TCustomChart;
Begin
    Chart1.Free;
    tmpChart:=TChart.Create(Self);
    LoadChartFromfile(tmpChart,'c:\mychart.tee');
    Chart1:=tmpChart as TChart;
    Chart1.Parent:=Self;
End;


---------------------------------------------------------------

