==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

   Netscape TeeChart Plugin 
=============================================

This Netscape plugin is a DLL that allows creating Charts
in HTML language and displaying them inside Netscape.

The NPTeeDem.htm page contains an example of use.

You should run Netscape version 3.0 or higher.

Installation:
==============

1) Close Netscape
2) Copy the NPTeeCha.DLL to the \Netscape\Program\Plugin folder.
3) Open Netscape

Testing:
==============

** Open the "NPTeeDem.HTM" file under Netscape to test the plugin.

You should see a Chart.

If not, then recheck your installation.
You can select "About --> Plugins" in Netscape menu to show all
installed plugins.  You should see "TeeChart plugin".


Description:
==============

The plugin is a DLL used by Netscape to show TeeChart controls
in HTM pages.

To create a new TeeChart, edit an HTM file and insert this
special code:

<EMBED TYPE="application/x-TeeChart-plugin"
 ALIGN=CENTER WIDTH=500 HEIGHT=300
 name="teedemo"
>

This will show an empty Chart on Netscape.

To show data on the Chart, you can specify a "*.tee"
file URL in the HTM page:

<EMBED TYPE="application/x-TeeChart-plugin"
 ALIGN=CENTER WIDTH=500 HEIGHT=300
 name="teedemo"
 URLChart="http://ourworld.compuserve.com/homepages/dberneda/demo1.tee" 
>

The plugin will start downloading this URL file and will
load it into the Chart component, showing both the properties
and point values contained in the *.tee URL file.

There are other several parameters you can use in the HTM
page. More will be added in future releases.

Parameter name:
-----------------
View3D         True / False
TitleText      String
ChartType      String 
  "Bar"
  "Line"
  "Point"
  "Pie"
  "HorizBar"
  "Area"
  "FastLine"

Color           Integer
Chart3DPercent  Integer
LegendVisible   True / False


Copyrights:
===========

This TeeChart Netscape Plugin has been created with
Mr. Mike Scott's Netscape Plugin components.
You can contact Mr. Mike Scott at this Compuserve
email address: 

   100140.2420@compuserve.com

Netscape is copyright of Netscape Communications:
www.netscape.com
==========================================================
