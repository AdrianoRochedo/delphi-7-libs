==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

     TeeChart virtual DataSet
=============================================

This example shows the new TSeriesDataSet component in action !

1 -- Introduction
2 -- Explanation
3 -- Testing
4 -- Additional information

---------------------------------------------------------

1 -- Introduction
-----------------

Now with Delphi 3.0 the TDataSet component is completely abstract.
That means everything important is VIRTUAL, and, more important,
the TDataSet component does not need the BDE (Borland Database
Engine).

With some work it's possible to create derived TDataSet components
to suite your needs or to extend the Delphi database architecture.

The TSeriesDataSet component is an example of reusing the
TDataSet for an specific case.

2 -- Explanation
------------------

What's it for ?

The TSeriesDataSet will provide you all data values on a Series
component.
You can connect a standard TDataSource, TDBGrid and any other
suitable DB component to it.

This is very useful to show or edit the Series data values as
if the Series was connected to a Table.


3 -- Testing
------------------

When you install TeeChart-Pro, you'll find the TSeriesDataSet
component located at the "TeeChart" tab on Delphi's component
palette.

Drop it on a Form, set the Series property to any existing 
Series in your Charts, and set the Active property to True.

That's it !

Now you can drop a TDataSource and a TDBGrid components and
connect them as usually.

As soon you add point values to the Chart Series, the SeriesDataSet
will refresh the DBGrid, and vice-versa.

4 -- Additional information
----------------------------

See the included DEMO1 and DEMO2 example projects for 
an example of TSeriesDataSet.

Delphi 3.0 comes with an example of a virtual DataSet 
for component writers.

You can find this example at:

\Delphi 3.0\Demos\DB\TEXTDATA\Textdata.pas

--------------------------------------------------------------------