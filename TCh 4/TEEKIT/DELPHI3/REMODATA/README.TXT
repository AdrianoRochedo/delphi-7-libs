==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

  Delphi 3.0 Remote DataSets and TeeChart 
================================================================

Now with Delphi 3.0 you can create applications with Database
Chart component without using the Borland Database Engine (BDE).


1 -- The SERVER side... (needs BDE)
==============================================

You must create first a database Server project.
This "Server" project is the responsible of accessing the real
tables and performing queries.

First step is to open and compile the "Server" application 
provided as an example in Borland Delphi 3.0.

Run Delphi 3.0 and open this project:

\Delphi 3.0\Demos\DB\ClientDS\Server.dpr 

Now you should only compile and run this project ONE time
in your machine.

The first time you run this application, it will "register"
as a database Server in the Windows registry database.

This is necessary for "client" applications to access the
database Server.

When you run this project, you will see a small Form:   

"Employee Data Server"

You can hide this Form to make a "hidden" database Server.
See the server.dpr source code.


2 -- The CLIENT side...  ( DO NOT needs BDE )
==============================================

Now you can stop and close the above Server project.

Let's create a new application to show how to connect
and chart data from the Server:

1) Start a new project
2) Drop a TRemoteServer component on Form1.
3) Set the ServerName property to "Serv.EmpServer"

(The "Serv.EmpServer" is automatically filled by Delphi.
It corresponds to our Server application).

4) Set the Connected property to True.

This will make Windows to start the Server application.

5) Drop a TClientDataSet component.

6) Set the RemoteServer property to: "RemoteServer1".

7) Set the ProviderName property to: "EmpQuery".

Now you have everything necessary to retrieve the data.

8) Set the ClientDataSet1 Active to True as usually.


Now the DataSet is connected and active. 
You can now:

9) Drop a TDBChart component.

10) Double-click, add a Series and connect the Series to
the ClientDataSet1 component using the Series Editor "DataSource"
tab.

-----------------------------------------------------------------

These are all the steps to create a minimal application using
remote datasets.

You can also connect Client and Server applications residing on
different machines.
To do so, you need to have the DCOM protocol installed on both
machines.

Windows NT 4.0 comes with DCOM preinstalled. For Windows 95,
you can go to Microsoft web site and download DCOM (it's free).

DCOM has it's own configuration parameters.
You'll find the DCOMCFG.EXE application in \Windows\System32
folder.

Please refer to Delphi 3.0 help and documentation for more
information about remote datasets and DCOM.

-----------------------------------------------------------------
