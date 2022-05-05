=======================================
      TeeChart-Pro 3.0 
        Developer Kit
=======================================

Creating your own new Chart, Series and Function types is 
now easier and more documented.

Please read the enclosed TEELOPER.DOC guide to know how to
create new Series, Function and Chart components.

See TeeLoper.pas unit to find the list of sample Series 
units. Check these units to see how are they made.

This "Kit" will be progressively updated, with new samples
and documentations.

Check TeeChart web site (www.teemach.com coming soon) for
more information.

See below a "Quick & Dirty" small guide for creating Series.

-------------
David Berneda
teeMach, SL
March 1997

===============================
Rapid Guide to creating Series:
===============================

New Series types and Functions can be "Registered"
in a similar manner to Delphi components so they
will appear on TeeChart gallery exactly as standard
included Series and Functions.

New Series and Functions can be mixed and linked to 
DataSources exactly as standard Series types.

There has been a major effort to implement "pluggable"
custom Series and Functions types, hoping the whole
TeeChart users community will benefit from new Series
and calculations functions from 3rd party developers.

Creating new Series and Functions:
------------------------------------

Creating new Series and Functions consist on the following
4 steps:

1) Create a new unit and add a new Series component definition,
   adding the necessary units to the "Uses" clause:

   Uses TeeProcs,Teengine,Chart,Series,...;
  
   type
      TMySeries=class( TPointSeries )
      end;

2) Add new properties and override existing functionality:

   Check the protected and public sections of the ancestor
   component on TeeChart source code.
   Always override the Abstract methods if the ancestor does
   not do so.
   Apply your desired functionality to the Series.

3) Add Series and Function registration code at unit's
   initialization section:

   initialization
      RegisterTeeSeries( TMySeries, 'MyPoint', 'John', 1 );
 
   Where "MyPoint" is the text the Series will show at 
   TeeChart gallery, and "John" is the desired gallery tab.
   "1" means only one random Series will be filled at the gallery.

   (Note: See included sample units for complete "initialization" section)   

4) Add an empty "Register" procedure and install this unit
   as usually under Delphi.
   Note: "Register" should be empty by default. No new components are
   installed. Not necessary to call RegisterComponents.

   Procedure Register;
   begin  { empty is ok }
   end;

Now drop a Chart, doubleclick and click on "Add..."
Check your new Series appears on the selected gallery tab.
Use it and check it works both at design-time and runtime as
expected.

-----------------------------------------------------------
