unit teeHTM;

interface

Uses Classes,SysUtils,Chart,Teengine;

{ WARNING: This component is intended to run on 32-bit only
   ( Delphi 2.0 or 3.0 )
}
type TImporterProc=Procedure(Var S:String) of object;

{ This component extracts an HTML table from an HTM file and
  fills a Series with points using the table cell values.

  Works with many different HTM Table configurations.
  Manual adjust of properties is needed.
}
     TImporter=class(TComponent)
     private
       Row,Col:Integer;
       tmpX:Double;
       tmpLabel:String;

       FTitleRows:Integer;
       FLabelColumn,
       FXColumn,
       FYColumn:Integer;
       FCloneCount:Integer;
       FInvertedDate,
       FSwapDecimal,
       FXDateTime,
       FYDateTime:Boolean;
       FChart:TCustomChart;
       Procedure ProcTag(Const HTMTag:String; Var s:String; Proc:TImporterProc; i:Integer);
     protected
       Procedure CleanTags(Var s:String);
       Procedure ProcessTags(Const HTMTag1,HTMTag2:String; Var s:String; Proc:TImporterProc);
       Procedure ProcessTagOnce(Const HTMTag:String; Var s:String; Proc:TImporterProc);
       Procedure ChartCaption(Var s:String);
       Procedure NewTitleCol(Var s:String);
       Procedure NewCol(Var s:String);
       Procedure NewRow(Var s:String);
       Procedure NewTable(Var s:String);
     public
       CloneCols:Array[1..10] of Integer;
       Constructor Create(AOwner:TComponent); override;
       Procedure ImportHTMTable(Const FileName:String);
       Procedure AddCloneCols(Const Cols:Array of Integer);
     published
       property TitleRows:Integer read FTitleRows write FTitleRows default -1;
       property XColumn:Integer read FXColumn write FXColumn default -1;
       property YColumn:Integer read FYColumn write FYColumn default -1;
       property LabelColumn:Integer read FLabelColumn write FLabelColumn default -1;
       property XDateTime:Boolean read FXDateTime write FXDateTime default False;
       property YDateTime:Boolean read FYDateTime write FYDateTime default False;
       property InvertedDate:Boolean read FInvertedDate write FInvertedDate default False;
       property SwapDecimal:Boolean read FSwapDecimal write FSwapDecimal default False;
       property CloneCount:Integer read FCloneCount write FCloneCount default 0;
       property Chart:TCustomChart read FChart write FChart;
     end;

{ Save a Series component to an HTML file, so the file will contain the
  Series point values in HTML Table format.
}
Procedure SaveSeriesToHTM(Const FileName:String; ASeries:TChartSeries);

{ Register TImporter in Delphi }
Procedure Register;

implementation

Constructor TImporter.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FTitleRows:=-1;
  FCloneCount:=0;
  FXColumn:=-1;
  FYColumn:=-1;
  FLabelColumn:=-1;
  FXDateTime:=False;
  FYDateTime:=False;
  FInvertedDate:=False;
  FSwapDecimal:=False;
  tmpLabel:='';
end;

Function IUpperCase(Var S: string): string;
var t: Integer;
begin
  for t:=1 to length(s) do
    if (s[t] >= 'a') and (s[t] <= 'z') then s[t]:=chr(ord(s[t])-32);
  result:=s;
end;

Procedure TImporter.AddCloneCols(Const Cols:Array of Integer);
var t:Integer;
begin
  CloneCount:=High(Cols)-Low(Cols)+1;
  for t:=Low(Cols) to High(Cols) do CloneCols[t-Low(Cols)]:=Cols[t];
end;

Function IPos(Const a,b:String):Integer;
var s:String;
begin
  s:=b;
  result:=Pos(a,{$IFNDEF WIN32}IUppercase{$ENDIF}(s));
end;

Procedure TImporter.ProcTag(Const HTMTag:String; Var s:String; Proc:TImporterProc; i:Integer);
Var st:String;
begin
  Delete(s,1,i+Length(HTMTAG));
  i:=Pos('>',s);
  if i>0 then Delete(s,1,i);
  i:=IPos('</'+HTMTag+'>',s);
  if i>0 then
  begin
    st:=Copy(s,1,i-1);
    Proc(st);
    Delete(s,1,i+2+Length(HTMTAG));
  end;
end;

Procedure TImporter.ProcessTags(Const HTMTag1,HTMTag2:String; Var s:String; Proc:TImporterProc);
var i1,i2,i:Integer;
    HTMTag:String;
begin
  Repeat
    i:=0;
    i1:=IPos('<'+HTMTag1,s);
    if HTMTag2<>'' then i2:=IPos('<'+HTMTag2,s)
                   else i2:=0;
    if i1>0 then
    begin
      if (i2<=0) or (i1<i2) then
      begin
        i:=i1;
        HTMTag:=HTMTag1;
      end;
    end
    else
    if i2>0 then
    begin
      i:=i2;
      HTMTag:=HTMTag2;
    end;
    if i>0 then ProcTag(HTMTAG,s,Proc,i);
  Until i=0;
end;

Procedure TImporter.ProcessTagOnce(Const HTMTag:String; Var s:String; Proc:TImporterProc);
var i:Integer;
begin
  i:=IPos('<'+HTMTag,s);
  if i>0 then ProcTag(HTMTAG,s,Proc,i);
end;

Procedure TImporter.NewTitleCol(Var s:String);
var t:Integer;
begin
  Inc(Col);
  CleanTags(s);
  if (Col=FXColumn) or (Col=FLabelColumn) then
     FChart.BottomAxis.Title.Caption:=s
  else
  if FCloneCount=-1 then
  begin
    FChart.LeftAxis.Title.Caption:=s;
    FChart[0].Title:=s;
  end
  else
  begin
    if Col=FYColumn then FChart[0].Title:=s
    else
    for t:=1 to FCloneCount do
    if CloneCols[t]=Col then
    begin
      FChart[t].Title:=s;
      break;
    end;
  end;
end;

Procedure TImporter.CleanTags(Var s:String);
var i1,i2:Integer;
    tmpTag:String;
    tmpChar:Char;
    t:Integer;
begin
  Repeat
    i1:=Pos('<',s);
    if i1>0 then
    begin
      i2:=Pos('>',copy(s,i1+1,Length(s)));
      if i2=0 then Delete(s,i1,1)
      else
      begin
        tmpTag:=Copy(s,i1,i2+1);
        if IUppercase(tmpTag)='<BR>' then s:=Copy(s,1,i1-1)+' '+Copy(s,i1+i2+1,Length(s))
                                     else Delete(s,i1,i2+1);
      end;
    end;
  Until i1=0;
  Repeat
    i1:=Pos('&#',s);
    if i1>0 then
    begin
      i2:=Pos(';',copy(s,i1+2,Length(s)));
      if i2>0 then
      begin
        tmpChar:=Chr(StrToInt(Copy(s,i1+2,i2-1)));
        s:=Copy(s,1,i1-1)+tmpChar+Copy(s,i1+i2+2,Length(s));
      end
      else i1:=0;
    end;
  Until i1=0;
  t:=1;
  While t<=Length(s) do
  if (s[t]=#13) or (s[t]=#10) then Delete(s,t,1)
                              else inc(t);
end;

Procedure TImporter.NewCol(Var s:String);

   Procedure InvertDate(Var s:String);
   var i1,i2:Integer;
   begin
     i1:=Pos('/',s);
     if i1>0 then
     begin
       i2:=Pos('/',copy(s,i1+1,Length(s)));
       if i2>0 then
       begin
         i2:=i2+i1+1;
         s:=copy(s,i2,length(s))+'/'+copy(s,i1+1,i2-i1-2)+'/'+copy(s,1,i1-1);
       end;
     end;
   end;

   Procedure SwapDecimal(Var s:String);
   var t:Integer;
   begin
     t:=1;
     While t<=Length(s) do
     begin
       if s[t]='.' then
       begin
         s[t]:=',';
         inc(t);
       end
       else
       if s[t]=',' then Delete(s,t,1)
                   else inc(t);
     end;
   end;

   { not: var tmp }
   Function ConvertToValue(Var tmp:String; IsDateTime:Boolean):Double;
   begin
     if IsDatetime then
     begin
       if FInvertedDate then InvertDate(tmp);
       result:=StrToDateTime(tmp);
     end
     else
     begin
       if FSwapDecimal then SwapDecimal(tmp);
       try
         result:=StrToFloat(tmp);
       except
         on EConvertError do result:=0;
       end;
     end;
   end;

var tmpSeries:TChartSeries;
    t:Integer;
begin
  Inc(Col);
  if Col=FLabelColumn then
  begin
    CleanTags(s);
    tmpLabel:=s;
  end
  else
  if Col=FXColumn then
  begin
    CleanTags(s);
    tmpX:=ConvertToValue(s,FXDateTime);
  end
  else
  begin
    if Col=FYColumn then tmpSeries:=FChart[0]
    else
    begin
      tmpSeries:=nil;
      for t:=1 to FCloneCount do
      if CloneCols[t]=Col then
      begin
        tmpSeries:=FChart[t];
        break;
      end;
    end;
    if Assigned(tmpSeries) then
    begin
      CleanTags(s);
      if tmpX=-1 then
         tmpSeries.Add(ConvertToValue(s,FYDateTime),tmpLabel,clTeeColor)
      else
         tmpSeries.AddXY(tmpX,ConvertToValue(s,FYDateTime),tmpLabel,clTeeColor);
    end;
  end;
end;

Procedure TImporter.NewRow(Var s:String);
begin
  Inc(Row);
  Col:=-1;
  tmpLabel:='';
  tmpX:=-1;
  if (FTitleRows<>-1) and (Row<=FTitleRows) then
     ProcessTags('TD','TH',s,NewTitleCol)
  else
     ProcessTags('TD','TH',s,NewCol)
end;

Procedure TImporter.ChartCaption(Var s:String);
begin
  CleanTags(s);
  FChart.Title.Text.Add(s);
end;

Procedure TImporter.NewTable(Var s:String);
begin
  Row:=-1;
  ProcessTags('CAPTION','',s,ChartCaption);
  ProcessTags('TR','',s,NewRow);
end;

Procedure TImporter.ImportHTMTable(Const FileName:String);
var tmp:TStringList;
    t:Integer;
    SeriesClass:TChartSeriesClass;
    tmpSeries:TChartSeries;
    s:String;
begin
  While FChart.SeriesCount>1 do FChart[1].Free;
  for t:=1 to FCloneCount do
  begin
    SeriesClass:=TChartSeriesClass(FChart[0].ClassType);
    tmpSeries:=SeriesClass.Create(FChart[0].Owner);
    tmpSeries.Name:=TeeGetUniqueName(FChart[0].Owner,'Series');
    tmpSeries.ParentChart:=FChart;
  end;
  tmp:=TStringList.Create;
  try
    FChart.BottomAxis.Title.Caption:='';
    FChart.LeftAxis.Title.Caption:='';
    tmp.LoadFromFile(FileName);
    With FChart do
    begin
      Title.Text.Clear;
      for t:=0 to SeriesCount-1 do
      With Series[t] do
      begin
        Clear;
        XValues.DateTime:=FXDateTime;
        YValues.DateTime:=FYDateTime;
      end;
      Foot.Text.Clear;
      Foot.Text.Add(FileName);
    end;
    {$IFDEF WIN32}
    s:=Uppercase(tmp.Text);
    {$ENDIF}
    ProcessTagOnce('TITLE',s,ChartCaption);
    ProcessTagOnce('TABLE',s,NewTable);
  finally
    tmp.Free;
  end;
end;

Procedure SaveSeriesToHTM(Const FileName:String; ASeries:TChartSeries);
var t,tt:Integer;
begin
  With TStringList.Create do
  try
    Add('<html>');
    Add('<body>');
    Add('<table border=1>');
    { title }
    Add('<tr>');
    Add('<td>'+ASeries.Title+'</td>');
    { header }
    for t:=0 to ASeries.ValuesListCount-1 do
        Add('<td>'+ASeries.ValueList[t].Name+'</td>');
    Add('</tr>');

    { rows... }
    for t:=0 to ASeries.Count-1 do
    begin
      Add('<tr>');
      Add('<td>'+ASeries.XLabel[t]+'</td>');
      for tt:=0 to ASeries.ValuesListCount-1 do
      With ASeries.ValueList[tt] do
      if DateTime then Add('<td>'+DateTimeToStr(Value[t])+'</td>')
                  else Add('<td>'+FloatToStr(Value[t])+'</td>');
      Add('</tr>');
    end;
    Add('</table>');
    Add('</body>');
    Add('</html>');
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

Procedure Register;
begin
  RegisterComponents('TeeChart',[TImporter]);
end;

end.
