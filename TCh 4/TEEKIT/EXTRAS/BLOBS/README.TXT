==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================


  TeeChart in BLOB fields
=============================================

The TEEBLOB.DPR example project shows how to save and
load Charts from BLOB fields in a table.

BLOB fields are a special database field type to store binary data.
The included "teeblob.db" table (in Paradox 5.0 format), has
a BLOB field.

The demo opens the Table and shows a ListBox of the different
Chart descriptions saved in the table.

Clicking on a description will extract the Chart from the BLOB
field to display it.

Editing the Chart or the description will put the table in edit
mode.  Posting the record changes will store the Chart again
into the BLOB field to save the record.


---------------------------------------------------------------

