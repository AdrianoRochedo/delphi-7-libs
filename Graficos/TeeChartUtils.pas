unit TeeChartUtils;

{-----------------------------------------------------------------------------
           Versão Atualizada em 27/01/03 por Rafael V. Borges

  Contém rotinas para exportar gráficos TeeChart para o formato BXML.
  Já estão implementadas as seguintes propriedades:

   TeeChart:
     Width;
     Height;
     View3d;
     BackColor;

   TeeChart.Title:
     Text;

   TeeChart.Margin (MarginTop, MarginLeft, MarginRight, MarginBottom);

   TeeChart.Legend:
     Legend;
     Color;
     Inverted;

   TeeChart.Axis (TopAxis, LeftAxis, RightAxis, BottonAxis);
     Visible;
     Increment;
     Logarithmic;
     Title.Angle;
     Title.Caption;

   TeeChart.Series:
     Title;
     Color;
     ShowInLegend;
     ValueFormat;
     ColorEachPoint;
     PercentFormat;

     Marks.Visible;
     Marks.Transparent;
     Marks.Style;

     XValues;
     YValues;
     XLabel;
     ValueColor;

   TBarSeries:
     Style;
     Multibar;
     BarWidthPercent;
     OffSetPercent;
     SideMargins;
     YOrigin;

   TCandleSeries:
     Style;

   TPointerSeries:
     Pointer.Style;
     Pointer.HorizSize;
     Pointer.VertSize;

   As classes de Series já implementadas foram:
      TBarSeries;
      TBubbleSeries;
      TLineSeries;
      TPieSeries;
      TCandleSeries;
      TPointerSeries;
      TFastLineSeries;

-----------------------------------------------------------------------------}

interface
uses Classes, Chart, TeEngine, Series, XML_Utils;

  // Converte um Gráfico do tipo TeeChart para XML
  procedure ToXML(Chart: TChart; Buffer: TStrings; var Ident: Integer);

  // Insere valores abaixo e acima dos minimos e maximos em um grafico
  procedure InsertUpDownValues(Chart: TChart; const Percent: real);

implementation
uses SysUtils, CandleCh;

  procedure WriteDescendentSerie(Serie: TChartSeries; x: TXML_Writer);
  begin
    x.BeginTag('DescendentProperties');
    x.BeginIdent;
    if Serie is TBarSeries then
       with TBarSeries(Serie) do
          begin
          x.Write('Style', ord(BarStyle));
          x.Write('MultiBar', ord(MultiBar));
          x.Write('BarWidthPercent', BarWidthPercent);
          x.Write('OffsetPercent', OffsetPercent);
          x.Write('SideMargins', SideMargins);
          x.Write('YOrigin', YOrigin);
          end
    else
      if Serie is TCandleSeries then
         x.Write('Style', ord(TCandleSeries(Serie).CandleStyle))
      else
         if Serie is TPointSeries then
            with TPointSeries(Serie).Pointer do
              x.Write('Pointer', ['Style', 'HorizSize', 'VertSize'], [ord(Style), HorizSize, VertSize], '');
    x.EndIdent;
    x.EndTag('DescendentProperties');
  end;

  procedure WriteSerie(Serie: TChartSeries; x: TXML_Writer);
  var i: Integer;
  begin
    x.BeginTag('serie', ['Type'], [serie.ClassName]);
    x.BeginIdent;
      x.Write('Title', serie.Title);
      x.Write('Color', serie.SeriesColor);
      x.Write('ShowInLegend', serie.ShowInLegend);
      x.Write('ValueFormat', serie.ValueFormat);
      x.Write('ColorEachPoint', serie.ColorEachPoint);
      x.Write('PercentFormat', serie.PercentFormat);

      x.Write('Marks', ['Visible', 'Transparent', 'Style'],
                       [serie.Marks.Visible, serie.Marks.Transparent, ord(serie.Marks.Style)],
                       '');

      // ... outras propriedades

      x.BeginTag('data');
      x.BeginIdent;
        for i := 0 to Serie.Count-1 do
          x.Write('d', ['x', 'y', 'xLab', 'color'],
                  [Serie.XValues[i], Serie.YValues[i], Serie.XLabel[i], Serie.ValueColor[i]], '');
      x.EndIdent;
      x.EndTag('data');

      WriteDescendentSerie(Serie, x);

    x.EndIdent;
    x.EndTag('serie');
  end;

  procedure WriteAxis(Axis : TChartAxis; x : TXML_Writer; Kind : string);
    begin
    x.BeginTag(Kind, ['visible', 'increment', 'logarithmic'], [axis.visible, axis.logarithmic, axis.increment]);
    x.BeginIdent;
      x.BeginTag('title', ['angle', 'caption'], [Axis.Title.Angle, Axis.Title.Caption]);
      x.EndTag('title');
    x.EndIdent;
    x.EndTag(Kind);
    end;

  procedure ToXML(Chart: TChart; Buffer: TStrings; var Ident: Integer);
  var x: TXML_Writer;
      i: Integer;
      dc: Char;
  begin
    dc := SysUtils.DecimalSeparator;
    SysUtils.DecimalSeparator := '.';
    x := TXML_Writer.Create(Buffer);
    try
      x.IdentSize := Ident;
      x.BeginIdent;
        x.BeginTag('chart', ['Width', 'Height', 'View3D', 'BackColor'],
                   [Chart.Width, Chart.Height, Chart.View3D, Chart.BackColor]);
        x.BeginIdent;

          x.BeginTag('title');
          x.BeginIdent;
            for i := 0 to Chart.Title.Text.Count-1 do
              x.Write('string', Chart.Title.Text[i]);
          x.EndIdent;
          x.EndTag('title');

          x.BeginTag('margin');
          x.BeginIdent;
            x.Write('top', inttostr(Chart.MarginTop));
            x.Write('left', inttostr(Chart.MarginLeft));
            x.Write('right', inttostr(Chart.MarginRight));
            x.Write('bottom', inttostr(Chart.MarginBottom));
          x.EndIdent;
          x.EndTag('margin');

          x.BeginTag('legend', ['visible', 'color', 'inverted'], [Chart.legend.visible, Chart.legend.color, Chart.legend.inverted]);
          x.EndTag('legend');

          x.BeginTag('axis');
          x.BeginIdent;
            WriteAxis(chart.TopAxis, x, 'top');
            WriteAxis(chart.LeftAxis, x, 'left');
            WriteAxis(chart.RightAxis, x, 'right');
            WriteAxis(chart.BottomAxis, x, 'bottom');
          x.EndIdent;
          x.EndTag('axis');

          x.BeginTag('series');
          x.BeginIdent;
            for i := 0 to Chart.SeriesCount-1 do WriteSerie(Chart.Series[i], x);
          x.EndIdent;
          x.EndTag('series');

        x.EndIdent;
        x.EndTag('chart');
      x.EndIdent;
    finally
      x.Free;
      SysUtils.DecimalSeparator := dc;
    end;
  end;

  // Insere valores abaixo e acima dos minimos e maximos em um grafico
  procedure InsertUpDownValues(Chart: TChart; const Percent: real);
  var r1, r2: double;
  begin
    if Chart.SeriesCount > 0 then
       begin
       Chart.LeftAxis.CalcMinMax(r1, r2);
       r1 := r1 - (r1 * Percent / 100);
       r2 := r2 + (r2 * Percent / 100);
       Chart.LeftAxis.SetMinMax(r1, r2);
       Chart.Invalidate();
       end;
  end;

end.
