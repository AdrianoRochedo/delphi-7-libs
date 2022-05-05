{$I STDEFINE.INC}

{$IFDEF WIN32}
  {$R STCONST.R32}
{$ELSE}
  {$R STCONST.R16}
{$ENDIF}

{*********************************************************}
{*                   STCONST.PAS 1.05                    *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit StConst;
  {-Resource constants for SysTools}

interface

{$I STCONST.INC}

const
  {value used to offset bitmap resource ids}
  stbmpBase      = $7999;

const
  stbmpText   = stbmpBase + 0; {bitmap for Reg/INI text in outline}
  stbmpBinary = stbmpBase + 1; {bitmap for Reg/INI binary value in outline}

implementation

end.
