==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

TeeChart and ActiveX technology Information
=============================================

1 -- Introduction
2 -- Creating an ActiveX Form containing a TeeChart
3 -- Deploying the ActiveXForm
4 -- Testing the ActiveXForm
5 -- Improving the ActiveXForm with extended features.

-----------------------------------------------------------

1 -- Introduction
===================================

Now with Delphi 3.0 you can create ActiveX Forms containing
TeeChart components.

These Forms can be used by many applications, like Internet 
Explorer, Visual C++, Visual Basic, and Delphi 3 itself !

You can create ActiveX Forms containing any components and
code exactly as you create normal Forms today.

If you compile these ActiveX Forms using Delphi packages,
the resulting files are very small, which is very good for
redistributing them on the WWW.


2 --- Creating an ActiveX Form containing a TeeChart
==========================================================

Let's create a new ActiveX Form and test it using Microsoft
Internet Explorer 3.01 or greater.

1) Open Delphi 3
2) Select "File -> New -> ActiveX -> ActiveX Form"

You should see the ActiveXForm wizard dialog.

3) Type your desired name and file names and press "OK".
4) You might see a "Warning" if you had already an open project.

5) You should see now an ActiveX Form. Very similar to a normal Form.

6) Drop a TChart component on this Form and add a Series.
   (Set the Chart1.Align property to "alClient" )

7) Add this code to the Form.OnCreate event:

procedure TActiveFormX.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(10);
end;


3 --- Deploying the ActiveXForm
================================================

8) Go to "Project -> Web Deployment Options"

In this dialog you can specify several parameters related
to how and where your ActiveX form will be distributed.

For our simple test, type this:

Target dir:   c:\
Target URL:   /
HTML dir  :   c:\

9) Press OK to close the "Web Deployment" dialog.

Now let's create the ActiveXForm !!!

10) Select "Project -> Web Deploy"

You should see NOTHING. Just a very quick flashing of
some window appearing and disappearing.
Note: If you select "Web Deploy" again, Delphi will
ask you to confirm overwriting the ActiveX form file.
Just click "Yes".
------------

Ok, now... What has been created ? And where ?

Delphi 3 has created the following files:

File                      Contains
--------                  --------------------------
c:\ActiveXFormProj.HTM    A very simple HTM file for testing purposes.
c:\ActiveXFormProj.INF    Installation instructions for Internet Explorer.
c:\ActiveXFormProj.OCX    The compiled ActiveX Form.

File sizes:
-------------

You can compile ActiveXForms using Delphi 3 packages or not.
If using packages ( Project -> Options -> Packages -> "Build with runtime packages" )
you should see the resulting OCX file is much smaller.

Without using packages:   403456 bytes.    
Using packages        :    30720 bytes.

Remember that using packages requires the necessary packages to be 
in the client \Windows\System directory.

If you want to use packages, the minimum packages you will need to transfer
to the client machine are:  vcl30.dpl and tee30.dpl

You can specify what packages to be automatically distributed on the
www at "Web Deployment Options" dialog.


4 --- Testing the ActiveX Form
================================================

To test our ActiveX Form, let's run Internet Explorer ( 3.01 or greater )
and open the auto-generated HTM file.

You should see the Form inside Internet Explorer. 
If you see a small icon at the Top/Left side of the rectangle, it means
your Internet Explorer has to be configured to accept execution of 
ActiveX Forms.
To activate this feature, go to "View" "Options" "Security" in Internet
Explorer, and then click on "Safety Level" and select "None".

Reload again the HTM file to see if the ActiveX Form shows correctly.

Check Borland Delphi 3 documentation about Web deployment instructions
and extended configuration information.

If you see the ActiveX Form correctly, note you can scroll and zoom the 
Chart with the mouse, in the same way as if your Form was a stand-alone
executable.

You can add any kind of component to this Form to interact with the Chart
from inside Internet Explorer or other ActiveX "aware" environment like 
Visual Basic or Visual C++.



5 -- Improving the ActiveXForm with extended features.
========================================================


Now we have a Chart running from inside the web browser, we might want
to do the following things:

a) Change the Chart properties using HTML language from inside the HTM 
   file.
b) Load remote data into the Chart from any URL.
c) Load remote Chart properties from any URL.
d) Refresh Chart contents with a Timer to draw "pulled" Real-Time 
   remote data.
e) Let the user Edit, Print and Print Preview the Chart from inside    
   Internet Explorer.
f) Show more than one Chart in the same HTM page.
g) Compile the ActiveXForm using packages to make it very small.
h) Create the CABINET files to redistribute the packages to client
   machines via Internet or local network.

-----------------------------------------------------------------------------

a) Change the Chart properties using HTML language from inside the HTM 
   file.


   Using Visual Basic Script in HTML pages, you can change the ActiveX
   Form published properties.
 

   You can also add new properties to the ActiveX form and use VBScript
   to change them in HTML code.

   To add new properties using Delphi 3, select "Edit --> Add to Interface" 
   or "View --> Type Library" and create a new property. Then go to the
   implementation unit and type the source code for the new property.


   Example:
   This VBScript code will change our ActiveX Chart Color to clRed color:

   
    <SCRIPT LANGUAGE="VBS">
    SUB DoChart
        Form1.ChartColor=255
    END SUB
    </SCRIPT>
    <INPUT TYPE=BUTTON VALUE="Change the Chart Color" NAME="cdmChartColor" onClick="DoChart">


   WARNING:
   The "Form1" object name should be manually added to the "Object" section:

      <OBJECT
        ......
        id=Form1
        ......
      >
      </OBJECT>




b) Load remote data into the Chart from any URL and....
c) Load remote Chart properties from any URL.


   You can save both Chart and Series properties and Series Data 
   to a file, calling the "SaveChartToFile" method.
   This method creates a *.TEE file containing both properties and
   point values in a binary compact format.

   You can make your ActiveX Form to load this *.TEE files and display
   them in a Chart.
   The *.TEE file can be local to the Clients hard disk or it can be
   located at a remote internet address.

   For local files, an example is:

   "file://c|\myfiles\demo.tee"

   For remote files, an example is:

   "http://www.teemach.com/demos/demo.tee"

   The example ActiveX Form downloads a remote *.TEE file and 
   calls "LoadChartFromFile" procedure to show a Chart containing
   the remote file.

   This two operations (downloading and loading), are performed by
   this procedure:

   LoadChartFromURL( Chart1,'http://www.john.com/myfiles/teechart.tee' );


 


d) Refresh Chart contents with a Timer to draw "pulled" Real-Time 
   remote data.

   You can add a Timer component on your ActiveX Form and at every
   time refresh you can download a remote *.TEE file. (See above).
   
   At the Server side, there must be a program re-creating the *.TEE
   files constantly.

   This Server program can consist of a Form with a Chart that is
   continously being updated with new points.  
   The Server Form can have a Timer and at every timer refresh it can
   call the SaveChartToFile method to re-create the *.TEE file.

   Server:   SaveChartToFile( Chart1, 'c:\myfiles\mychart.tee' );

   Client:   LoadChartFromURL( Chart1,'http://www.john.com/myfiles/teechart.tee' );
  

e) Let the user Edit, Print and Print Preview the Chart from inside    
   Internet Explorer.

   The example ActiveX Form has buttons to Export, Print, Edit and Save Charts.   
   It uses the TeeChart units: teExport, EditChar, teePrevi and teeStore.


f) Show more than one Chart in the same HTM page.

   You can create more than one ActiveX Form in the same project
   (using File -> New -> ActiveX -> ActiveXForm ) and add Chart
   components to them.

   Using "Web Deploy", Delphi 3 will create an example HTM file containing
   the Form objects.

   

g) Compile the ActiveXForm using packages to make it very small.

   Compiling the ActiveX Form using packages has the very big advantadge
   of being a very small file able to transfer via slow Internet connections.

   The "price" for this is that Client machines must have Delphi's VCL
   and TeeChart packages in their \windows\system directory.


   Using "Web Deployment Options" dialog you can specify which packages
   you want to redistribute and how.
   Each package can be assigned to a remote URL containing the package
   DPL file.

   We will post in the future URL's for the latest versions of TeeChart
   and Delphi 3.0 packages.

   Internet Explorer will download new versions of packages to the
   client machines when the clients access your Forms and they have
   old packages installed.

   

h) Create the CABINET files to redistribute the packages to client
   machines via Internet or local network.

   If you want to compress and group the packages and ActiveX Forms 
   into a single file, click on "Use CAB file compression" option 
   at "Web Deployment Options" dialog.

   This will create a *.CAB file containing the ActiveX Form OCX file 
   and the optional packages.

   The *.HTM file contains the INF file name and path.
   The *.INF file contains the CAB file name and path.
   The *.CAB file contains the OCX Form and optional packages and files.

   When the clients load the HTM file, Internet Explorer looks the
   Server's INF file and determines if it's necessary to download the
   bigger CAB file.


-----------------------------------------------------------------------