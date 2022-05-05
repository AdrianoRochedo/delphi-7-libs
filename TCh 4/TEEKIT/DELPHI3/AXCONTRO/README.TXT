==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

TeeChart as ActiveX Control ( OCX )
=============================================

Included is an example of Delphi 3 ActiveX Control creation.

The TeeChartX ActiveX Control is a TPanel containing a Chart
component and several buttons.

Through these buttons, the Chart can be Edited, Printed, Exported,
Loaded and Saved to disk during design-time and run-time.

The ActiveX Control exposes two useful properties:

ChartFile       :String   ( a *.tee file name and path )
ToolbarVisible  :Boolean  ( to show / hide the ToolBar buttons }

Setting the ChartFile property will load the *.tee file and
show the Chart:

Chart1.ChartFile = "c:\myfiles\demo.tee"

Now you can scroll, zoom and do everything with the Chart
as if it was a standard Delphi Chart component !

The TeeChartX control has a ToolBar.
Setting ToolBarVisible = False will hide the ToolBar.

The "ToolBar" shows 3 buttons:

  "Edit..."  to access the Chart Editor Dialog

  "Save..." to save the Chart to a *.tee file

  "Load..." to load a Chart from a *.tee file


Testing the OCX
=====================

The best way to test the TeeChartX OCX control might be to install
it in Delphi 3.0 or Visual Basic.

  Testing in Delphi 3
  ---------------------

Open Delphi 3 and select "Component -> Import ActiveX Control"

From the list of controls, choose "TeeChartXControl Library" and
press "Install...".

The default users package will be recompiled and the new
TeeChartX component icon will appear in Delphi "ActiveX" component
palette tab.

If the installation went fine, you can test the ActiveX:

- Create a new Form and drop a TeeChartX component to it.
- Now you will see an empty gray Panel with three buttons.
- Set the ChartFile property to the path of a *.tee file.

  ( There are several *.tee files included in this kit )

- If the path and file name are corrects, the Panel will
  load and show the *.tee Chart file.

- At run-time is necessary to have access the *.tee file.
 
---------------------------------------------------------------------

