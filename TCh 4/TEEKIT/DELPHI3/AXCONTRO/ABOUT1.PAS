unit About1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TTeeChartXAbout = class(TForm)
    CtlImage: TSpeedButton;
    NameLbl: TLabel;
    OkBtn: TButton;
    CopyrightLbl: TLabel;
    DescLbl: TLabel;
  end;

procedure ShowTeeChartXAbout;

implementation

{$R *.DFM}

procedure ShowTeeChartXAbout;
begin
  with TTeeChartXAbout.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
