
ACE Reporter
=============

TeeChart-Pro integration with ACE Reporter is 
possible by sending the Chart component as a
bitmap to an ACE report component.

There will be better integration and demos
in a near future at our web site.

You can reach the ACE Reporter developers
at this address:

SCT Associates, Inc.
9221 S. Kilpatrick Ave.
Oak Lawn, IL 60453-1813
USA

Email:

info@sct-associates.com
support@sct-associates.com

Web address:

http://www.sct-associates.com

----------------------------------------------

Source code snippet:

1) Add an ACE expression variable of type bitmap:

    ChartBitmap: TSctExprvar;

2) Implement the variable GetData event:

procedure TChartForm.ChartBitmapGetData(oVar: TSctvar);
var
  Stream: TStream;
begin
  with TBitmap.Create do
  try
    Width := Chart1.Width;
    Height := Chart1.Height;
    Chart1.Draw(Canvas, Rect(0,0,Width, Height));
    Stream := TMemoryStream.Create;
    try
      SaveToStream(Stream);
      oVar.AsStream := Stream;
    finally
      Stream.Free;
    end;
  finally
    free;
  end;
end;


