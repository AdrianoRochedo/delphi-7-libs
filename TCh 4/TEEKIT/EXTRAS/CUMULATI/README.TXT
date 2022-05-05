==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


Cumulative Function Component
=============================================

1 --- Explanation
2 --- How to Install
3 --- Demo

=============================================


1 --- Explanation
=================

The TCumulative function is a new Function type for
TeeChart family of calculations.

It performs a SUM of the source Series point values in
an accumulative mode.
Every point is the SUM of all previous points.

Example:

If we apply a Cumulative function to a Series with these
values:

1, 4, 5, 2

We obtain the following results:

1, 5, 10, 12

every point is the sum of all previous points:


1  = 1
5  = 1+4
10 = 1+4+5
12 = 1+4+5+2


2 --- How to Install
====================

The TCumulative function is implemented at TeeCumu.PAS unit.

To install this new function, simply rebuild the component library. This will recompile the TEELOPER unit, which now "uses" this new function.

Warning: DO NOT add TeeCumu to the component library. This
unit does not contain a Delphi Register procedure.

3 --- Demo
============

To test this new function, do the following:

1) Create a new Form
2) Drop a TChart component
3) Double-click the TChart to show the Editor
4) Click "Add" and add a new Bar Series
5) Click "Add" again and click on "Functions"
6) The Cumulative function will appear on the Gallery
7) Select the Cumulative function and press Ok
8) Add Series1 as a Source Series for the function
9) Close the Chart Editor

You should see a Chart with a Bar Series and a Line Series.
The Line should draw the cumulative SUM of the Bar point
values.

You might want to assign the Cumulative Series to the Right Axis,
to use 2 vertical Axis. 
One for the Bar, and one for the Line with different scales.

Series2.VertAxis := aRightAxis;

Remember you can always "Change" the type of the function.
Just go to the Chart editor, select Series2 and click on "Change".


-------------------------------------------------------------------

