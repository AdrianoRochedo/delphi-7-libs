==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


 TeeBevel Component
=============================================

1 -- Introduction
2 -- How it works ?
3 -- Properties
4 -- Limitations

------------------------------

1 -- Introduction

This small component is a "plug-in" that, when
attached to a TeeChart component, draws "bevels" 
around the Chart Legend, Titles and Axis.

The TTeeBevel component is located at TeeBevel.pas unit.

Under Delphi, install the TeeBevel component by selecting
the TeeBevel.pas unit.
This will install a new component at "TeeChart" component
palette.

2 -- How it works ?

When connecting a TeeBevel to an existing Chart, the TeeBevel
component will automatically draw the bevels whenever the Chart
component is redrawn.

Internally, it uses the Chart "OnAfterDraw" event to be notified
on Chart redraws.

3 -- Properties

The most important property is the "Chart" property.
You should set this property to an existing Chart component.
(It works also with DBChart, QRChart, DecisionGraph, etc)

You can selectively choose where to draw bevels and in which
bevel style :

Property   Default       Description
========   ============  =====================================

Axis       tbRaisedBox   Draws a bevel around the chart Axes.
Foot       tbRaisedBox   Draws a bevel around the chart Foot text.
Legend     tbRaisedBox   Draws a bevel around the chart Legend.
Title      tbRaisedBox   Draws a bevel around the chart Title text.

Size       1             Determines the width of the bevels.

FirstColor DarkGrey      Determines one color used to draw the
                         bevels.

SecondColor  White       Determines the other color used to draw
                         the bevels.

------------------------------------------------------------------


Notes:

After connecting a TeeBevel component to a Chart component,
you should set the following properties to not conflict with
the bevels drawing:

For axis:
---------------
Chart1.Frame.Visible := False;

Chart1.LeftAxis.Axis.Visible := False;
Chart1.BottomAxis.Axis.Visible := False;
Chart1.TopAxis.Axis.Visible := False;
Chart1.RightAxis.Axis.Visible := False;

Chart1.View3D := False ;

For Legend:
--------------
Chart1.Legend.Frame.Visible := False;
Chart1.Legend.Color := clSilver;


See the included BeveDemo.dpr project for an example of use.


4 -- Limitations
------------------

** The TeeBevel component has is little useful for printing
   purposes unless you are printing the Chart background in 
   grey color.

 
** You can export the Chart to bitmap or metafile, but you
   cannot save the Chart *and* the Bevels together in a 
   TEE file.

--------------------------------------------------------------------

