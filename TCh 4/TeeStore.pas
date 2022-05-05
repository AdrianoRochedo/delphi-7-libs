{*********************************************}
{  TeeChart Storage functions                 }
{  Copyright (c) 1997-98 by David Berneda     }
{  All rights reserved                        }
{*********************************************}
{$I teedefs.inc}
unit TeeStore;

interface

Uses Classes, Teengine, Chart;

{ Read a Chart from a file ( Chart1,'c:\demo.tee' ) }
Procedure LoadChartFromFile(Var AChart:TCustomChart; Const AName:String);

{ Write a Chart to a file ( Chart1,'c:\demo.tee' ) }
Procedure SaveChartToFile(AChart:TCustomChart; Const AName:String);

{ The same using TStream components (good for BLOB fields, etc)  }
Procedure LoadChartFromStream(Var AChart:TCustomChart; AStream:TStream);
Procedure SaveChartToStream(AChart:TCustomChart; AStream:TStream);

{ (Advanced) Read charts and check for errors  }
{ return True if ok, False to stop loading }
type TProcTeeCheckError=function(const Message: string): Boolean of object;

Procedure LoadChartFromStreamCheck( Var AChart:TCustomChart;
                                    AStream:TStream;
                                    ACheckError:TProcTeeCheckError
                               );

Procedure LoadChartFromFileCheck( Var AChart:TCustomChart;
                                  Const AName:String;
                                  ACheckError:TProcTeeCheckError
                                  );

{ Returns if a Series has "X" values }
Function HasNoMandatoryValues(ASeries:TChartSeries):Boolean;

{ returns the "no mandatory" values of ASeries ( usually XValues ) }
Function NotMandatoryList(ASeries:TChartSeries):TChartValueList;

{ Convert a *.tee file to text format, without point values }
Procedure ConvertTeeFileToText(Const InputFile,OutputFile:String);

implementation

Uses Graphics, SysUtils, TeeProcs, TeCanvas;

Const MagicTeeFile  =$3060;
      {$IFDEF D5}
      VersionTeeFile=$0120; { Delphi 4 }
      {$ELSE}
      {$IFDEF C3}
      VersionTeeFile=$0110; { C++ Builder 3 }
      {$ELSE}
      {$IFDEF D3}
      VersionTeeFile=$0100; { Delphi 3 }
      {$ELSE}
      {$IFDEF D2}
      VersionTeeFile=$0090; { Delphi 2 }
      {$ELSE}
      {$IFDEF D1}
      VersionTeeFile=$0080; { Delphi 1 }
      {$ELSE}
      VersionTeeFile=$0099; { C++ Builder 1 }
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}


Type TTeeFileHeader=packed record
         Magic   : Word; { secure checking }
         Version : Word; { Chart file version }
     end;

     TeeFormatFlags=(tfNoMandatory,tfColor,tfLabel);
     TeeFormatFlag =set of TeeFormatFlags;

Procedure ReadSeriesData(AStream:TStream; ASeries:TChartSeries);

  Function ReadLabel:String;
  Var L:Byte;
  begin
    L := 0;
    { read the label length }
    AStream.Read(L, SizeOf(L));
    { read the label contents }
    {$IFDEF D1}
    Result[0]:=Char(L);
    AStream.Read(Pointer(@Result[1])^, L);
    {$ELSE}
    SetString(Result, PChar(nil), L);
    AStream.Read(Pointer(Result)^, L);
    {$ENDIF}
  end;

Var tmpFormat:TeeFormatFlag;

  Procedure ReadSeriesPoint(Index:Longint);
  Var tmpFloat : Double;
      Ax       : Double;
      Ay       : Double;
      AColor   : TColor;
      ALabel   : String;
      tmpIndex : Longint;
      tt       : Longint;
  begin
    { read the "X" value if exists }
    if tfNoMandatory in tmpFormat then AStream.Read(AX,Sizeof(AX));
    { read the "Y" value }
    AStream.Read(AY,Sizeof(AY));
    { read the Color value if exists }
    if tfColor in tmpFormat then AStream.Read(AColor,Sizeof(AColor))
                            else AColor:=clTeeColor;
    { read the Label value if exists }
    if tfLabel in tmpFormat then ALabel:=ReadLabel
                            else ALabel:='';

    With ASeries do
    begin
      { read the rest of lists values }
      for tt:=2 to ValuesLists.Count-1 do
      begin
        AStream.Read(tmpFloat,SizeOf(tmpFloat));
        ValuesLists.ValueList[tt].TempValue:=tmpFloat;
      end;
      { add the new point }
      if tfNoMandatory in tmpFormat then
         tmpIndex:=AddXY(AX,AY,ALabel,AColor)
      else
         tmpIndex:=Add(AY,ALabel,AColor);
      { finish adding the point with the rest of lists values }
      AddValue(tmpIndex);
    end;
  end;

Var tmpCount : Longint;
    t        : Longint;
begin
  { empty the Series }
  ASeries.Clear;
  { read the point flags }
  AStream.Read(tmpFormat,SizeOf(tmpFormat));
  { read the number of points }
  AStream.Read(tmpCount,Sizeof(tmpCount));
  { read each point }
  for t:=0 to tmpCount-1 do ReadSeriesPoint(t);
end;

Procedure ReadChartData(AStream:TStream; AChart:TCustomChart);
Var t : Longint;
begin
      { read each Series data }
  for t:=0 to AChart.SeriesCount-1 do ReadSeriesData(AStream,AChart[t]);
end;

{ Special reader to skip Delphi 3 or 4 new properties when
  reading Charts in Delphi 1.0 or 2.0 }
type TChartReader=class(TReader)
     protected
       function Error(const Message: string): Boolean; override;
     public
       CheckError:TProcTeeCheckError;
     end;

function TChartReader.Error(const Message: string): Boolean;
begin
  if Assigned(CheckError) then result:=CheckError(Message)
                          else result:=True;
end;

{ Reads Series and Points from a Stream into a Chart }
Procedure LoadChartFromStreamCheck( Var AChart:TCustomChart;
                                    AStream:TStream;
                                    ACheckError:TProcTeeCheckError);
Var Header : TTeeFileHeader;
    t      : Longint;
    Reader : TChartReader;
begin
  {$IFDEF TEETRIAL}
  TeeTrial(AChart.ComponentState);
  {$ENDIF}
  { read file header }
  AStream.Read(Header,SizeOf(Header));
  { check is a valid Tee file }
  if Header.Magic=MagicTeeFile then
  begin
    { remove all child Series }
    AChart.FreeAllSeries;
    { read the Chart and Series properties }
    Reader := TChartReader.Create(AStream, 4096);
    try
      Reader.CheckError:=ACheckError;
      Reader.ReadRootComponent(AChart);
    finally
      Reader.Free;
    end;
    if Assigned(AChart) then
    begin
      { read the Series points }
      ReadChartData(AStream,AChart);
      { change each Series ownership }
      if AChart.Owner<>nil then
      for t:=0 to AChart.SeriesCount-1 do
      begin
        AChart[t].Owner.RemoveComponent(AChart[t]);
        AChart.Owner.InsertComponent(AChart[t]);
      end;
    end
    else raise Exception.Create('Invalid Chart in *.TEE file');
  end
  else raise Exception.Create('Wrong *.TEE file format');
end;

Procedure LoadChartFromStream(Var AChart:TCustomChart; AStream:TStream);
begin
  LoadChartFromStreamCheck(AChart,AStream,nil);
end;

Procedure LoadChartFromFileCheck( Var AChart:TCustomChart;
                                  Const AName:String;
                                  ACheckError:TProcTeeCheckError );
Var tmp:TFileStream;
begin
  tmp:=TFileStream.Create(AName,fmOpenRead);
  try
    LoadChartFromStreamCheck(AChart,tmp,ACheckError);
  finally
    tmp.Free;
  end;
end;

Procedure LoadChartFromFile(Var AChart:TCustomChart; Const AName:String);
begin
  LoadChartFromFileCheck(AChart,AName,nil);
end;

{ Create a text file from a binary *.tee file }
Procedure ConvertTeeFileToText(Const InputFile,OutputFile:String);
var SInput  : TFileStream;
    SOutput : TFileStream;
    Header  : TTeeFileHeader;
begin
  SInput:=TFileStream.Create(InputFile,fmOpenRead);
  try
    { read file header }
    SInput.Read(Header,SizeOf(Header));
    { check is a valid Tee file }
    if Header.Magic=MagicTeeFile then
    begin
      SOutput:=TFileStream.Create(OutputFile,fmCreate);
      try
        ObjectBinaryToText(SInput,SOutput);
      finally
        SOutput.Free;
      end;
    end;
  finally
    SInput.Free;
  end;
end;

{ Returns if a Series has "X" values (or Y values for HorizBar series) }
Function HasNoMandatoryValues(ASeries:TChartSeries):Boolean;
var t        : Longint;
    tmpCount : Longint;
    tmp      : TChartValueList;
begin
  result:=False;
  With ASeries do
  if Count>0 then
  begin
    tmp:=NotMandatoryList(ASeries);
    if (tmp.First=0) and (tmp.Last=Count-1) then
    begin
      tmpCount:=MinLong(10000,Count-1);
      for t:=0 to tmpCount do
      if tmp[t]<>t then
      begin
        result:=True;
        Exit;
      end;
    end
    else result:=True;
  end;
end;

{ Returns if a Series has Colors }
Function HasColors(ASeries:TChartSeries):Boolean;
var t        : Longint;
    tmpCount : Longint;
    tmpColor : TColor;
    tmpSeriesColor:TColor;
begin
  result:=False;
  With ASeries do
  begin
    tmpSeriesColor:=SeriesColor;
    tmpCount:=MinLong(10000,Count-1);
    for t:=0 to tmpCount do
    begin
      tmpColor:=ValueColor[t];
      if (tmpColor<>clTeeColor) and
         (tmpColor<>tmpSeriesColor) then
      begin
        result:=True;
        exit;
      end;
    end;
  end;
end;

{ Returns if a Series has labels }
Function HasLabels(ASeries:TChartSeries):Boolean;
var t        : Longint;
    tmpCount : Longint;
begin
  result:=False;
  tmpCount:=MinLong(10000,ASeries.Count-1);
  for t:=0 to tmpCount do
  if ASeries.XLabel[t]<>'' then
  begin
    result:=True;
    exit;
  end;
end;

{ Determine what a Series point is made of }
Function SeriesGuessContents(ASeries:TChartSeries):TeeFormatFlag;
begin
  if HasNoMandatoryValues(ASeries) then result:=[tfNoMandatory]
                                   else result:=[];
  if HasColors(ASeries) then result:=result+[tfColor];
  if HasLabels(ASeries) then result:=result+[tfLabel];
end;

Procedure WriteSeriesData(AStream:TStream; ASeries:TChartSeries);

  Procedure WriteLabel(Const AString:String);
  Var L:Byte;
  begin
    L:=Length(AString);
    AStream.Write(L,SizeOf(L));
    {$IFDEF D1}
    AStream.Write((@AString[1])^,L);
    {$ELSE}
    AStream.Write(PChar(AString)^,L);
    {$ENDIF}
  end;

Var tmpFormat      : TeeFormatFlag;
    tmpNoMandatory : TChartValueList;

  Procedure WriteSeriesPoint(Index:Longint);
  Var tmpFloat : Double;
      tmpColor : TColor;
      tt       : Longint;
  begin
    { write the "X" point value, if exists }
    if tfNoMandatory in tmpFormat then
    begin
      tmpFloat:=tmpNoMandatory[Index];
      AStream.Write(tmpFloat,Sizeof(tmpFloat));
    end;
    { write the "Y" point value }
    tmpFloat:=ASeries.MandatoryValueList[Index];
    AStream.Write(tmpFloat,Sizeof(tmpFloat));

    { write the Color point value, if exists }
    if tfColor in tmpFormat then
    begin
      tmpColor:=ASeries.ValueColor[Index];
      AStream.Write(tmpColor,Sizeof(tmpColor));
    end;
    { write the Label point value, if exists }
    if tfLabel in tmpFormat then WriteLabel(ASeries.XLabel[Index]);
    { write the rest of values (always) }
    for tt:=2 to ASeries.ValuesLists.Count-1 do
    begin
      tmpFloat:=ASeries.ValuesLists.ValueList[tt][Index];
      AStream.Write(tmpFloat,SizeOf(tmpFloat));
    end;
  end;

var t   : Longint;
    tmp : Longint;
begin
  { write the "flag" containing the format of a point }
  tmpFormat:=SeriesGuessContents(ASeries);
  AStream.Write(tmpFormat,SizeOf(tmpFormat));
  { write all points. pre-calculate tmpNoMandatory }
  tmpNoMandatory:=NotMandatoryList(ASeries);
  { write the number of points }
  tmp:=ASeries.Count;
  AStream.Write(tmp,Sizeof(tmp));
  for t:=0 to tmp-1 do WriteSeriesPoint(t);
end;

Procedure WriteChartData(AStream:TStream; AChart:TCustomChart);
Var t : Longint;
begin
  for t:=0 to AChart.SeriesCount-1 do WriteSeriesData(AStream,AChart[t]);
end;

Procedure SaveChartToStream(AChart:TCustomChart; AStream:TStream);
var Header : TTeeFileHeader;
begin
  {$IFDEF TEETRIAL}
  TeeTrial(AChart.ComponentState);
  {$ENDIF}
  { write file header, with "magic" number and version }
  Header.Magic:=MagicTeeFile;
  Header.Version:=VersionTeeFile;
  AStream.Write(Header,SizeOf(Header));
  { write the Chart and Series properties }
  AStream.WriteComponent(AChart);
  { write the Series points }
  WriteChartData(AStream,AChart);
end;

Procedure SaveChartToFile(AChart:TCustomChart; Const AName:String);
Var tmp : TFileStream;
begin
  tmp:=TFileStream.Create(AName,fmCreate);
  try
    SaveChartToStream(AChart,tmp);
  finally
    tmp.Free;
  end;
end;

Function NotMandatoryList(ASeries:TChartSeries):TChartValueList;
begin
  With ASeries do if YMandatory then result:=XValues else result:=YValues;
end;

{$IFNDEF TEEOCX}
type TODBCChart=class(TChart)
     end;

initialization
  RegisterClass(TODBCChart);
{$ENDIF}
end.
