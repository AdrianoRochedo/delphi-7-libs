==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

JPEG support for TeeChart in Delphi 3.0
=============================================


Delphi 3.0 provides an additional unit supporting JPG and 
JPEG graphic format images.

This format is widely used in the Internet due to its
small size compared to Bitmap BMP files.

Several options like Monochrome and color depth are also
supported by this new TJPEGImage component.

Running the sample project:
=============================

Included you'll see a sample project showing the
different export to graphic format options in TeeChart-Pro.

BEFORE running this project, you should do the following in Delphi 3.0:

1) Go to "Component --> Install Component"

2) At "Unit file Name" type this:

  \Delphi 3.0\Lib\JPEG.DCU

3) Click Ok to recompile and install the JPEG unit.


By doing this, you now have the possibility of loading
JPG or JPEG files to use in Delphi Forms.

To test this, just create a new Form, drop a TImage 
component, double-click on it, and load an existing JPG
file.


How it works ?
=====================

The "trick" is to create a temporary Bitmap object and
draw a Chart component on it.
Then, this bitmap can be Assigned to a TJPEGImage, and
then the JPEG image can be saved to disk using the standard
SaveToFile method.

==============================================================
