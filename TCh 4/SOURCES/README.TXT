==========================================================
               TeeChart-Pro 3.0.
                  June 1997
     Copyright (c) 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

         TeeChart-Pro 3.0 SOURCE CODE
        ==============================

**********************************************************
****   FOR TEECHART-PRO 3.0 REGISTERED USERS ONLY     ****
**********************************************************

This folder contains a compressed ZIP file containing 
TeeChart-Pro 3.0 100% SOURCE CODE.
This file is present only in licensed TeeChart-Pro 3.0.

To decompress TEE3SRC.ZIP file, please type the following
command at DOS prompt:

pkunzip -s TEE3SRC.ZIP

Please type the password we supplied you when licensed
TeeChart-Pro 3.0.
The password should be typed with all letters in lowercase.

Note: If you wish to recompile the TeeChart-Pro packages in 
Delphi 3.0 Standard and Delphi 3.0 Professional versions, you
should manually apply a minor change at DCLTEE30.DPK file.
(See below)

Licence:
--------

This source code is copyrighted and under international 
copyright laws.
You are NOT allowed to distribute or sell this source code
without our explicit permission.
Please contact us should you have any question regarding 
TeeChart-Pro 3.0 source code licensing issues.

You are allowed to modify this source code to use it in your
applications.
Remember this source code is subject to changes in next coming
future TeeChart-Pro upgrades, so remember to backup the original
sources and to write down any changes you do to this source code
to make easier to port your applications to future TeeChart releases.

=======================
Sources Installation:
=======================

Delphi 1 and Delphi 2:
----------------------
You can directly install this source under Delphi 2.0 32bit version.
Just change the path from TEECHART\LIB20 to TEECHART\SOURCES folder
under Delphi 2.0 "Component->Install->Search Path".

If you want to compile this source code under Delphi 1.0 16bit, 
you should first manually replace the following file:

   TEECHART.DCR 

 with this file:

   TEECHART.D16

 Do this:

   cd\teechart\sources
   copy teechart.D16 teechart.DCR


Delphi 3.0:
-----------

For Standard and Professional versions of Delphi 3.0, the
TeeChart DCLTEE30.DPK file should be modified manually.
This is because the DBX.DCP file is included in Delphi 3.0
Client/Server version only.

(Only the "TeeChart Wizard" needs the DBX.DCP file)

1) Open the DCLTEE30.DPK file using NOTEPAD
2) Comment the following lines:

{ ,DCLDB30 }

{ ,ExpForm,
  ChartExp }

3) Save this file. Now you can recompile the TeeChart packages
in Delphi 3.0 Standard and Professional versions.

-------------------------------------------------------------------
