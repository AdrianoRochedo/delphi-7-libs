{*********************************************************}
{*                  STDEFINE.INC 1.05                    *}
{*             Shared defines and directives             *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFDEF Win32}
  {$DEFINE OS32}
{$ENDIF}
{$IFDEF VirtualPascal}
  {$DEFINE OS32}
{$ENDIF}
{ OS32 is used to enable 32-bit operating system options                    }

{ Because STDEFINE.INC is included in all SysTools units, you can specify   }
{ global compiler options here. STDEFINE is included *before* each units'   }
{ required compiler options, so options specified here might be overridden  }
{ by hardcoded options in the source file.                                  }

{$IFDEF OS32}
 {$IFDEF WIN32}
  {---Global compiler defines for Delphi 2.0---}
  {$H+} {Huge string support}
 {$ENDIF}
  {$Q-} {Overflow check}
  {$R-} {Range check}
  {$S-} {Stack check}
  {$T-} {Typed @ check}
  {$V-} {Var strings}
{$ELSE}
  {---Global compiler defines for Delphi 1.0---}
  {$F-} {Far calls
  {$G+} {80286 code}
  {$Q-} {Overflow check}
  {$R-} {Range check}
  {$S-} {Stack check}
  {$T-} {Typed @ check}
  {$V-} {Var strings}
{$ENDIF}

{$IFDEF OS32}
{$DEFINE ThreadSafe}
{$ENDIF}
{ This define determines whether SysTools operations are safe in a 32-bit   }
{ multi-threaded application. Leaving it undefined will improved performance}
{ slightly in a single-threaded 32-bit application.                         }

{$DEFINE Median}
{.$DEFINE MidPoint}
{.$DEFINE Random}
{ The three partitioning methods used by the merge sort algorithm in STSORT.}
{ Choose one, and one only. Median is on average fastest, and quite safe.   }
{ Midpoint is the simplest, but may degrade on some data sets. Random is    }
{ the safest, but on average the slowest. Safety in this context means the  }
{ prevention of the underlying quicksort degenerating into a sort of O(N^2) }
{ The default is Median.                                                    }


{$IFDEF Win32}
{.$DEFINE SuppressWarnings}
{$ENDIF}
{ This define enables you to suppress warnings that are generated for code  }
{ in SysTools by Delphi 2/3. The two most common warnings are (a) function  }
{ does not set Result and (b) variable used before being initialized. The   }
{ code however has been tested to be correct but is complex enough to fool  }
{ the compiler. You should leave this define activated.                     }



{===========!! DO NOT ALTER ANYTHING BEYOND THIS POINT !!==================}

{Set up the Warnings compiler define for Delphi 2.0 compilation}
{$IFDEF VER90}
  {$IFDEF SuppressWarnings}
    {$WARNINGS OFF}
    {$HINTS OFF}
  {$ELSE}
    {$WARNINGS ON}
    {$HINTS ON}
  {$ENDIF}
{$ENDIF}

{Set up the string type expected}
{$UNDEF HStrings}
{$UNDEF WStrings}
{$DEFINE NStrings}     {255-character strings under Win16}
{$IFDEF WIN32}
  {$IFOPT H+}
    {$DEFINE HStrings} {Huge strings under OS32}
    {$UNDEF NStrings}
  {$ELSE}
    {$DEFINE WStrings} {255-character strings under OS32}
    {$UNDEF NStrings}
  {$ENDIF}
{$ENDIF}

{$IFDEF VirtualPascal}
  {$AlignRec+,Delphi+,T-,X+}
  {$DEFINE WStrings} {255-character strings under Win32}
  {$UNDEF NStrings}
{$ENDIF}

{$IFDEF Median}
 {$IFDEF MidPoint}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
 {$IFDEF Random}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
{$ENDIF}
{$IFDEF MidPoint}
 {$IFDEF Random}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
{$ENDIF}
{$IFNDEF Median}
 {$IFNDEF MidPoint}
   {$IFNDEF Random}
     !! ERROR: you must define at least one of Median, MidPoint, Random
   {$ENDIF}
 {$ENDIF}
{$ENDIF}




































