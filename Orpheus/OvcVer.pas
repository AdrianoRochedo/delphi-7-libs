{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   OVCVER.PAS 2.15                     *}
{*     Copyright (c) 1995-97 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OvcVer;
  {-Versioning defines and methods}

interface

const
  ProductName       = 'Orpheus';
  ProductVersionStr = '2.15.00';
  ProductVersionBin = 021500;
  ProductVersion    = ProductVersionBin / 10000;                       {!!.13}


implementation


end.
