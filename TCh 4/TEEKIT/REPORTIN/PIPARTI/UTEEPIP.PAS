unit uteepip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ppClass, ppCtrls, ppBands, ppEndUsr, TeEngine, Series, ExtCtrls,
  TeeProcs, Chart, ppViewr, StdCtrls, Buttons;

type
  TFormTeePiparti = class(TForm)
    ppReport1: TppReport;
    ppReport1HeaderBand1: TppHeaderBand;
    ppReport1DetailBand1: TppDetailBand;
    ppReport1FooterBand1: TppFooterBand;
    ppReport1Image1: TppImage;
    Chart1: TChart;
    Series1: TBarSeries;
    BitBtn1: TBitBtn;
    ppReport1Image2: TppImage;
    procedure ppReport1Image1Print(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTeePiparti: TFormTeePiparti;

implementation

{$R *.DFM}

procedure TFormTeePiparti.ppReport1Image1Print(Sender: TObject);
var tmp:TBitmap;
    tmpMeta:TMetafile;
begin
    { method 1: bitmap }
    tmp:=TBitmap.Create;
    try
      tmp.Width:=Chart1.Width;
      tmp.Height:=Chart1.Height;
      Chart1.Color:=clWhite;
      Chart1.BevelOuter:=bvNone;
      Chart1.Draw(tmp.Canvas,Rect(0,0,tmp.width,tmp.Height));
      Chart1.Color:=clBtnFace;
      Chart1.BevelOuter:=bvRaised;
      ppReport1Image1.Picture.Assign(tmp);
    finally
      tmp.Free;
    end;

    { method 2:  metafile }
    Chart1.Printing:=True;
    tmpMeta:=Chart1.TeeCreateMetafile(True,Rect(0,0,Chart1.Width,Chart1.Height));
    try
      ppReport1Image2.Picture.Assign(tmpMeta);
    finally
      tmpMeta.Free;
      Chart1.Printing:=False;
    end;
end;

procedure TFormTeePiparti.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(6);  { <-- random values }
end;

procedure TFormTeePiparti.BitBtn1Click(Sender: TObject);
begin
  ppReport1.PrintReport;
end;

end.
