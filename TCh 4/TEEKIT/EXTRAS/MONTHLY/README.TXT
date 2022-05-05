==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


 Charting Monthly and Weekly data
=============================================

1 -- Introduction
2 -- How it works ?
3 -- Testing 


1 -- Introduction
--------------------

The included TEEMONTH.PAS unit contains one useful 
routine to create Series showing data grouped by
Month, or by Week in the year.

This routine is called:

DBMonthlySeries

And the declaration and parameters are:


procedure DBMonthlySeries( ASeries:TChartSeries;
                           ATable:TDataSet;
                           Const DateFieldName,
                                 ValueFieldName:String;
                           DatePeriod:TDateTimeStep;
                           Count:Boolean);


Parameter       Description
============    ============================================
ASeries         The Series you want to fill with points.
ATable          The Table (or Query) where you have the data.
DateFieldName   The field name in the Table with Dates.
ValueFieldName  The field name in the Table with Values.
DatePeriod      possible values:   dtMonth or dtWeek
Count           Set to False if you want to Sum, or True to Count.

============    ============================================


2 -- How it works ?
--------------------

When you call this routine, the following things happen:

a) The current Series points are deleted ( Series1.Clear )

b) The Table or Query is traversed completely.
   ( All records are loaded in a loop )

c) For each record, a small algorithm determines to which
   MONTH, or to which WEEK the record belongs.

   This is done using the "DateFieldName" field.

d) The "Value" field is used to accumulate the value 
   corresponding to the MONTH or WEEK.

   ( The value is summed or incremented depending on "Count" 
     parameter )


The final result is a Series containing one point for each
MONTH or WEEK existing in the Table.

The value of every point corresponds to the sum or count of
records for that MONTH or WEEK.


3 -- Testing 
-----------------

Open and run the included MONTHLY.DPR example to see how
it works.

Follow the example unit source code comments to understand
how it is done.

--------------------------------------------------------------
