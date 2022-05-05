{*********************************************************}
{*                     OVC.INC 2.15                      *}
{*     Copyright (c) 1995-97 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Conditional defines that affect compilation}

{General define indicating use under C++ Builder}
{$IFDEF VER93}
  {$DEFINE CBuilder}
{$ENDIF}

{$IFDEF CBuilder)
  {$A+} {Word Align Data}
{$ELSE}
  {$IFDEF Win32}
    {$A+} {Word Align Data}
  {$ELSE}
    {$A-} {Byte Align Data}
  {$ENDIF}
{$ENDIF}

{$Q-} {Overflow Checking}
{$R-} {Range-Checking}
{$S-} {Stack-Overflow Checking}
{$V-} {Var-String Checking}

{$IFNDEF Win32}
  {$F-} {Force Far Calls}
  {$K-} {Smart Callbacks}
{$ENDIF}

{$IFDEF Win32}
  {$H+} {Long strings}
{$ENDIF}

{$IFNDEF Ver100} {Work-around does not work under Delphi 3.0 or BCB}
  {$IFNDEF CBuilder}
  	{$DEFINE NVBUG}
  {$ENDIF}
{$ENDIF Ver100}
{Because of a problem in Delphi's Non-Visual component tracker     }
{code, TComponent based components like the TOvcController that    }
{are created dynamically at design-time, are not displayed on the  }
{form until the form is re-loaded. To work-around this problem,    }
{TOvcController is created using undocumented calls from LibMain   }
{and LibIntf.                                                      }
{                                                                  }
{This work-around can be removed by undefining the NVBUG define    }
{and then re-building the component library.                       }
{                                                                  }
{If you decide to remove this work-around, the only difference is  }
{that the TOvcController component will not be visible if it is    }
{created dynamically (a controller is created dynamically if a     }
{component that uses it is dropped on the from and cannot find     }
{an existing controller) until the form has been saved and loaded. }
{You can still select the controller from the object inspector's   }
{drop-down list of components and alter its properties even if it  }
{is not visible.}

{$IFDEF Win32}
{$DEFINE SuppressWarnings}
{$ENDIF}
{This define enables you to suppress warnings that are generated   }
{for code in Orpheus by Delphi 2.0. The two most common warnings   }
{are (a) function does not set Result and (b) variable used before }
{being initialized. The code however has been tested to be correct }
{but is complex enough to fool the compiler. You should leave this }
{define activated.                                                 }

{.$DEFINE ZeroDateAsNull}
{This define enables special handling for a zero date. If defined, }
{assigning 0 to a date field is treated the same as assigning the  }
{constant "BadDate" (see manual) using the AsDateTime property.    }
{Otherwise, 0 is treated as a valid date. (32-bit only)            }
