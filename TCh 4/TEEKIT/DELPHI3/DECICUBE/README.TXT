==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

Delphi 3.0 Decision Cube and TeeChart 
=============================================


1 -- Introduction
2 -- Drilling a DecisionGraph



1 -- Introduction
==============================

Delphi 3 ( Client / Server version ) includes a new
powerful database component: the Decision Cube.

The "cube" maintains data grouped by several Fields and
calculates Totals and averages automatically.
The "cube" results can be displayed in a special "DBGrid"
which allows multiple levels of displaying the diferent Fields.

The TDecisionGraph component is a TChart component that connects 
with the "cube" datasource and displays the same grid information 
using visual Chart Series.

TDecisionGraph was developed by Borland Intl.


2 -- Drilling a DecisionGraph
==============================

** How to drill the Cube using Chart mouse events.

   The TeeCube1.PAS unit shows an example of drill-down
   with the mouse.

   Steps:

   - At design-time open a Cube dimension to view data in the Chart.

   - Go to the Chart Editor and select the Series corresponding to
     the open dimension in the Cube.
     On the Series editor tab, check the "Persistent" checkbox to
     make the Series properties to be stored with the Form.

     Note: The "Persistent" property shows only in Series used by
           TDecisionGraph.

   - Using the Chart Editor, change the Series Cursor to 
     "crTeeHand".

   - Use the Chart OnClickSeries event to detect when user
     has clicked a Series point.

   - Call the Cube "DrillDimIndex" method to "drill" on the
     clicked point in the Series.

   - Open a new dimension to Chart subvalues for the "drilled" value.


   For example, if you are seeing a Bar Chart with 3 points:

   Name            Cost
   -----   ----------------
   Development     1234
   Management      2233
   Marketing       4321


   If you then click on the "Management" Bar, then
   calling "DrillDimIndex" will make the Cube to filter its data to
   "Management" only.
   
   Then you can open another dimension like, for example, 
   "Employees".

   This will show you the "Employees" of the "Management" department,
   with a "Cost" for each "Employee" :

   Department: Management
   Name            Cost
   -----   ----------------
   John            1000
   Peter           1233
   -----   ----------------
        TOTAL:     2233

   (Note the TOTAL (2233) corresponds to the "Management" 
    value in the first Chart)
    

   So, combining together the Chart "OnClickSeries" event and
   the Cube "DrillDimIndex" method, allows visual drill-down of
   cube dimensions.
  
   See the TeeCube1.pas unit for detailed code.

====================================================================
