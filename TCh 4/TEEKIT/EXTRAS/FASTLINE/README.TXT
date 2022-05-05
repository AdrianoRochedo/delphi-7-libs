==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


More Speed when adding new Points
=============================================

The TEEFAST.PAS unit shows how to dramatically accelerate point 
drawing speed when adding new points to a Fast Line Series.

NOTE:
This is to accelerate when adding NEW points. Not when adding
points in a loop.


A BIG speed gain is to draw only the last inserted point and not
the whole Chart every time a new point is added to the Series.


The following conditions apply:

***  The new inserted point coordinates are INSIDE axis scales.
***  The new inserted point is the LAST point in the Series.


Also remember leaving "time" to Windows to send Draw messages
after adding a point:


   Series1.Add( 1234, '', clTeeColor );
   Application.ProcessMessages;


If you are using a loop to add many points, it is not necessary
to call "ProcessMessages".   The Chart will redraw one time only
after adding the last point.

-----------------------------------------------------------------