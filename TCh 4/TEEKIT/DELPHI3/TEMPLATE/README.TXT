==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

Delphi 3.0 Component Templates and TeeChart 
=============================================

1 -- Instructions
2 -- Explanation
3 -- TeeChart and Templates
4 -- Difference between a Template and a TEE file.

1 -- Instructions
====================

Templates are a very handy way to create complex 
Chart components, and to store them for reusing on 
new project Forms.

The steps are very simple:

Create the Template
-------------------
1) Run Delphi 3.
2) Select an existing Chart component on any project Form.
3) Click on "Component" --> "Create Component Template"
4) Type a new name for the Template and press Ok.

Test and reuse the Template
---------------------------
1) Create a new Form
2) Go to the "Templates" component palette
3) Drop the new created Template component into the Form.


2 -- Explanation
======================

You can think on Templates as "copied to clipboard" 
components, permanently stored and installed on Delphi's palette.

This means that you can only modify the Template by dropping
it into a Form, changing it and reinstalling it again as a 
Template.

Modified Templates DO NOT change Forms containing them.
Using a Template is the same as "Pasting" a component from the
Clipboard.

A very important thing is Delphi stores the Component Events
source code.
This is a very smart and powerful Delphi 3 feature.
Delphi saves CODE as well as properties. This means that when you
drop a Template component on a Form, Delphi "pastes" both the
Components and the optional Event source code accompaning the 
template components.


3 -- TeeChart and Templates
=============================

There are several reasons to use Templates with Chart
components.

 Chart "Looks"
===================

One is to create a palette of different Chart "looks" with
different drawing properties like Axis formatting, colors, 
fonts, Titles, etc.

The ULOOKS.PAS unit in the example "Looks" project can be 
used to test this.
Open this unit and select a Chart, then create a Component Template
of it.

Create a new Application and drop this new Template into the Form.
You will obtain a customized Chart. This saves time when all of
your Charts share a common format.

 Pre-Created Series
======================

Another use of Templates with Charts is to create Charts containing
pre-defined Series and Functions.

You can open the UDEMOCUM.PAS unit in the "Cummulative Function"
directory to see a sample of a "Histogram" Chart with a Bar Series 
and a "Cummulative" function of the Bar.  

Creating a Template of this Chart and reusing it in another Form
will give you a fast new "Histogram" Chart, ready to draw your
values.
Just fill the Bar Series with your own values to finish.


  Multiple-Charts
=======================

Templates can consist of a GROUP of components. 
The MULTIAXI.PAS unit includes 3 Charts inside a Panel component.
Each Chart has its Left and Right vertical Axis and the 3 Charts
are configured to synchronize scales when scrolling them horizontally,
as if it was a single Chart with multiple axis.

Creating a Template of the Panel1 component will save the 3 Charts
and the synchronizing source code for future reuse.


4 -- Difference between a Template and a TEE file.
====================================================

-- A Delphi "Template" contains Chart and Series properties
   but DO NOT contains any Series DATA.

   Delphi stores all templates in a propietary binary file.

-- A TEE file contains BOTH properties AND Series data.

   You can load / save *.Tee files using the TEESTORE.PAS unit.

-- At run-time, you can load a TEE file, but not a Delphi Template.

-- At design-time, you can use a Template, but not a TEE file.

----------------------------------------------------------------------
