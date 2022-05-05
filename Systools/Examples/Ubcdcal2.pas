{*********************************************************}
{*                  UBCDCAL2.PAS 1.05                    *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit UBCDCAL2;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons;

type
  TBCDCalcAbout = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BCDCalcAbout: TBCDCalcAbout;

implementation

{$R *.DFM}

end.
