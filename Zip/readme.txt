VCLZip Native Delphi Zip/UnZip Component! (Version 2.18 April 18th, 1999)

IMPORTANT:  Please be sure to always re-install/rebuild the components (VCLZip and VCLUnZip) to the component pallette (or rebuild the design time package) so that the ThisVersion property and any other new properties will be properly updated.

Also, please check out the web site http://ourworld.compuserve.com/homepages/boylank for any additional or newer files.

Version 2.18: 

1) Thanks to the hard work of a fellow registered user, added the capability to remove all dependencies on the Dialogs, Forms, Controls, and FileCtrl units by defining the conditional MAKESMALL, which results in a smaller footprint.  This can be quite useful when putting VCLZip into a DLL for instance.  In order to make this work, go into your Project | Options and select the Directories/Conditionals tab and enter MAKESMALL in the conditional defines text box.  In Delphi you can add this conditinal define to the project options of your application that uses VCLZip and then do a "build all".  In BCB you will have to add this to the project options of the package that contains VCLZip and then rebuild the package.  

If you define MAKESMALL, the only things you lose are:

a) ZIP file open dialog box that appears when the ZipName is set to "?" 
b) Select Directory dialog box that appears when the DestDir is set to "?"
c) Changing the cursor to an hour glass during some operations.
d) No long filename support in Delphi 1

2) Made VCLZip completely BCB4 compatible.

3) Added some exception handling to KPUNZIPP and KPINFLT, mainly to handle unexpected situations when wrong passwords are entered.  This fixes the problem with PRP, the password recovery program.

4) For Borland C++ Builder, changed any COMP types to double, getting rid of the compiler warnings for unsupported comp type.  This affects the OnStartZipInfo and OnStartUnZipInfo events, so you'll have to change the comp parameter to double in these events if you use them (in both your header files and in the CPP files).

5) Modified OnStartUnZip event so that FName (the filename of the file that is about to be unzipped along with complete path) is now a VAR parameter and can be modified. This allows you to change the path and name of a file that is about to be unzipped.  This is especially helpfull in applications like Install Programs.  

NOTE: You will need to change your current code to add the VAR to the event definition and implementation if you already use this event in your application. (In BCB, add a & just before the parameter instead of VAR)

6) Moved many type definitions to VCLUNZIP.PAS so that kpZipObj won't have to be included in your USES list.

7) Fixed bug that caused GPF when setting Zip Comment to '' (empty string).

8) Moved strings in VCLZip/VCLUnZip into a string table, making the code size a little smaller as well as making it much easier to localize string information.  However you have the option of not using the new string table, for whatever reason, by defining NO_RES in your project options (in the conditional defines text box on the Directories/Conditionals tab).

9) Removed the need for several files.  No longer included are kpstrm.res, kpstrm.rc, kpsconst.res, kpsconst.rc, kpstres.pas, and for Delphi 1, kpdrvs.pas.  In some cases the need for these files was eliminated and in other cases just rolled into the newly included  kpzcnst.rc, kpzcnst.pas, and kpzcnst.res.  Definining NO_RES in your project options will elimiate the need for these new files but will make your code size slightly larger and you won't be able to localize your application without changing VCLZip source code.

10) Modified the OnFilePercentDone and OnTotalPercentDone progress events to work better when creating spanned disk sets and blocked zip sets.  They no longer report 100% when the compressed file still has to be copied to disk.

11) Added the ReplaceReadOnly property.  Setting this to true will allow files with the ReadOnly attribute to be replaced during the unzip process.

12) Added the ifNewer and ifOlder options to the OverwriteMode property. (This had somehow made it into the help file but not into VCLUnZip)

13) Added the SFXToZip method which will convert an SFX file to a regular zip file. The header pointers will be properly adjusted during the conversion.

14) Fixed a problem where the OnGetNextDisk event would always revert to the DefaultGetNextDisk method instead of what you entered into the Object Inspector each time your project was re-opened.

15) Fixed a bug that caused CRC errors when unzipping files from spanned disk sets if they were STORED (no compression) and spanned across disks.

16) Added the OnZipComplete and OnUnZipComplete events.  If defined, these will fire at the very end of a zip or unzip operation (after all files have been processed, not after each file).  These events will rarely be used since, normally you will be able to do the same thing at the point that the call to Zip or UnZip returns, but these events can be useful when using VCLZip in threads where in certain circumstances the return from the Zip or UnZip methods are not seen.

17) Creation of SFX files has never been easier!!!  The addition of the MakeNewSFX method allows you to create Self Extracting Executables without the need to create a zip file first.  The files that you specify in the FilesList property will be zipped, using all the normal VCLZip property settings, and the SFX will be created, all in one step!  In addition, you can create configurable SFX files using this method, and you can do this especially easy by adding the new unit kpSFXOpt to your application's USES list and using the new 32bit SFX stub that is now distributed with VCLZip.  This allows you to easily set things like SFX Dialog caption, default target extraction directory,  file to launch after extraction, etc.

18) Fixed a memory leak that only affects applications using VCLZip that are compiled with Delphi 2, and that use wildcard specifications in the FilesList property.


Version 2.17a:

1) Fixed a bug that was keeping VCLZip from reading truncated zip files or sfx files that did not have their headers adjusted.

2) Fixed a bug that was causing a directory to be created on the C drive when doing integrity checking with the FileIsOK property.

3) Added {$V-} to kpZipObj.PAS

4) Moved two AssignTo methods to public instead of private in kpZipObj.PAS


Version 2.17:

1) Added Memory zipping and unzipping capabilities through the UnZipToBuffer and ZipFromBuffer methods.  See the documentation for these methods in the Help File for more information.  

2) New FileIsOK Property allows you to check for the integrity of individual files within an archive without actually unzipping the file.

3) Fixed a bug that kept checking of volume labels from working on WIN31 when working with spanned disk sets.

4) Removed all references to ChDirectory so that VCLZip will be more thread safe allowing separate instances of VCLZip in separate threads to be performing zip/unzip operations at the same time.

5) A new public property PreserveStubs allows you to make modifications to sfx archives and have the archive remain an SFX rather than revert back to a normal zip file.

6) Added a default OnGetNextDisk event.  If one is not defined, then the default event will be called when the situation arises that a new disk is needed when zipping or unzipping a spanned or blocked zip archive.

7) Added more power to the wildcard capabilities.  Now you can qualify the * wildcard character, for instance:

  *<a-e> would satisfy any number of contiguous characters as long as they are all a thru e.
  *<!a-e> would satisfy any number of contiguous characters as long as none of them were a thru e.

  This allows you to do things like include files in specific direcories into your ExcludeList.  For instance:

     VCLZip1.ExcludeList.Add('c:\test\*<!\>.txt')

  would exclude the zipping of all .txt files in the test directory but not in any subdirectories.

8) Fixed other minor bugs and made other code enhancements.


Version 2.16:

***Please be aware that if you currently use the OnSkippingFile event in any of your applications, version 2.16 will require a small modification as this event has an added parameter and one of the current parameters is used a little differently when being called by the zip operation.  Please see the help file for more information.

1) The OnSkippingFile Event has been changed slightly, adding a parameter for the filename.

2) OnSkippingFile is now called when a file to be zipped is skipped because it is locked by another application.  See the Help File for more information.

3) Fixed a bug with the Exclude and NoCompressList where they were ignoring entries with anything before the extention (i.e. 'somefile.*' as opposed to '*.zip') if you were saving directory information.

4) Fixed a bug that caused an error if you added a wildcard with a non-existent directory to the FilesList.

5) A few other minor bug fixes.


Modifications for 2.15 include:

1) PackLevel can now be set to 0 (zero) which means no compression at all (STORED only).

2) New property ExcludeList is a new stringlist that you can add filenames and wildcards to in order to specify files that you do not wish to be included in an archive.

3) New property NoCompressList is a new stringlist that you can add filenames and wildcards to in order to specify files that you wish to be STORED with a PackLevel of 0 (zero), no compression.

4) All compiler warnings and hints were removed.


Modifications for 2.14 include:

1) Delphi 4 compatability.

2) Added ability to use complex wildcards when specifying which files are to be zipped.  This includes wildcard characters not only in the filename but also in the pathname.  This allows you to specify directories using wildcards, for instance:

  VCLZip1.FilesList.add('c:\test\w*\mycode*.pas');

  would get all PAS files beginning with mycode in subdirectories under TEST that begin with the letter w.  Wilcards may be much more complex than this.  Please see the help file for more information.

3) Added the ability to override the RECURSE property setting when specifying files to be zipped.  By adding the following characters to the beginning of the filenames being added, you can override whatever the current setting is for the RECURSE property:

   '>' will force recursion into subdirectories
   '|' will force NO-recursion

  For instance:

  VCLZip1.FilesList.add('>c:\windows\*.ini');

  will get all .ini files in and below the windows directory reguardless of what the recurse property setting is.

  and:

  VCLZip1.FilesList.add('|c:\windows\sys*\*.dll');

  will get all .dll files in subdirectories of the windows directories that start with 'sys' but will not recurse into any directories below the sys* directories.

4) The [ and ] characters previously used as special wildcard characters have been changed to < and > since [ and ] are valid filename characters.  If you still need to use the previous characters for backward compatability, I can show registered users how to easily modify a couple of constants in the source code in order to go back to the old style. See "Using Wildcards" in the help file for more information.

5) A few bug fixes.


Modifications for 2.13 include:

1) New property ResetArchiveBitOnZip causes each file's archive bit to be turned off after being zipped.

2) New Property SkipIfArchiveBitNotSet causes files who's archive bit is not set to be skipped during zipping operations.

3) A few modifications were made to allow more compatibility with BCB 1.

4) Cleaned up the Help File some.

5) KWF file now works for Delphi 1 and Delphi 2 again. Still can't get context sensitive help in Delphi 3.

6) Cleaned up some of the code that was causing compiler warnings and hints.


Modifications for 2.12 include:

1) Added a TempPath property to allow the temporary files path to be different from the Windows default.

2) Modified VCLZip so that any temporary files that are created receive a unique temporary filename so as not to clash with any other files in the temporary directory.  This also allows working with zip files residing in the temporary directory.

3) Fixed a bug in the relative path feature.

4) Fixed a bug that caused a "list out of bounds" error if a file in the FilesList did not actually exist.

Modifications for 2.11 include:

1) Fixed password encryption bug for 16 bit.

2) Fixed "invalid pointer operation" when closing application bug.

3) Fixed path device truncation bug which caused inability to modify existing archives in 16 bit.

4) Fixed inability to cancel during wilcard expansion bug.

5) Added capability to better handle corrupted timestamps.

6) Added capability to open and work with SFX files that were created with the COPY/B method (header files not adjusted).

7) Other small bug fixes.

I'm still working on a bug which causes a GPF when continually unzipping the same file thousands to millions of times.  This mainly affects programs like the Password Recovery Program (PRP) which uses the brute force method of searching for an archive's password.

Modifications for 2.10 include:

1) Capability for 16bit VCLZip to store long file/path names when running on a 32bit OS.

2) New property (Store83Names) which allows you to force DOS 8.3 file and path names to be stored.

3) Better UNC path support.

4) Fixed a bug to allow files to be added to an empty archive.

Modifications for 2.03 include:

1) Volume labels now get written correctly to spanned disk sets in Delphi 1 for all versions of Windows.

2) Delphi 1 VCLZip now correctly recognizes when it is running on Windows NT.

3) Fixed a problem with zipping files in the root directory when StorePaths = True.

4) File and Zip Comments are now read correctly from spanned/blocked zip archives.

5) Fixed a buf that was causing "Duplicate Object" errors. 


Modifications for 2.02 include:

1) Fix for file comments which were supposed to be fixed in version 2.01 but weren't.

2) Fix for stream zipping.  Version 2.01 would not create a new archive if using a stream.  (The Stream Demo now allows creating new zip files to streams too)

3) A few other minor modifications to further solidify the code.

4) A modification to the Zip Utility Demo which allows unzipping from Blocked zip files as if they were single zip files.

5) Added a read-only, published ThisVersion property which reflects the version of the VCLZip/VCLUnZip that you are currently working with.

Modifications for 2.01 include:

1) Fixes for exceptions that were caused when CANCELING a zip or unzip of a spanned zip file.

2) Fix for a possible problem when zipping or unzipping a spanned zip file when one or more of the compressed files resided on more than 2 of the spanned parts.

3) Fix for file comments which were broken in version 2.00.


Additional features for version 2.00 include:

1) Modify/Add internal file details (filename, pathname, timestamp, comment) for any file while zipping, in the OnStartZip event.

2) Add an Archive Comment while zipping in the OnStartZipInfo event.

3) Delphi 1 compatiblity for VCLZip. 

4) Stream to Stream Zipping - Archives themselves can now be TStreams!

5) New Relative Path Information option.

6) Unzip archives that weren't zipped with the Relative Path option turned on as if they had been by determining how much path information to use with the
   Rootpath property.

7) Modify timestamps for files in existing archives (you could already modify filenames and pathnames for files in existing archives)

8) The OnBadPassword event now allows you to supply a new password and try the same file again when unzipping.

9) Source code has been cleaned up so that it will compile under Borland C++ Builder with no modifications.

Also some bugs were fixed, most importantly:

1) An empty file, that had been compressed into an archive would cause any file added to the archive to cause the archive to approximately double in size. Any archives containing empty files are not corrupted, they are OK.  This was simply a fix to the way the archive was processed.

2) After creating an SFX file, you had to close the zip file before you could modify it in any way, otherwise a stream read error was encountered.


See the Help file for more information on new features.

This zip file is part of a self contained installation program.  Just run it and the installation program will begin.

***IMPORTANT: Please remember, if using Delphi 3, 4, or BCB 3 do not install these components into a package by the name of either VCLZip or VCLUnZip.  You will receive an error if you do.

VCLZip Description

VCLZip is a full featured zip/unzip component and is now available for Delphi 1, 2, 3 and 4.  Features include:

     - Create zip files fully compatable with PKZip
     - Completely native Delphi VCL  (NO DLLS!!!!!!!!!!!)
     - Create Disk Spanning and Blocked zip files
     - Zip directly from streams to zip files
     - Unzip directly to streams from zip files
     - Stream to Stream zipping and unzipping
     - Create and read Zip and File Comments
     - Create Self Extracting Zip Files (16 bit and 32 bit distributable 
       windows sfx stubs included or use your own stubs)
     - Complete support for encrypted files (encrypts as it zips)
     - Save Relative Path information
     - Plenty of events
     - Long filenames, even the 16 bit VCLZip/VCLUnZip
     - Includes comprehensive Zip Utility with source as demo
     - Includes a small stream zipping demo
     - Includes a context sensitive help file
     - No Royalties!
     - Currently priced at only $49.95!!!

VCLZip inherits from VCLUnZip, so for those applications that only need unzip capabilities (for instance installation programs), you only need to use the VCLUnZip component (which is ofcourse included with VCLZip) thereby decreasing your application's footprint.  When you need both zip and unzip capabilities, all you need is the VCLZip component.

Registration will get you:

     - All Source Code
     - Notification of future patches and updates
     - Free point release upgrades
     - A free zip password recovery program that uses VCLUnZip 

Contact boylank@bigfoot.com for further information

Thanks!

Kevin Boylan
