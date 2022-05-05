{****************************************}
{  TeeChart Series DB Virtual DataSet    }
{ Copyright (c) 1996-98 by David Berneda }
{          All Rights Reserved           }
{****************************************}
{$I teedefs.inc}
unit TeeData;

{ This unit contains a VIRTUAL DATASET component for Delphi 3 or 4

  The TSeriesDataSet component is an intermediary between a
  Series component and a TDataSource.

  You can show Series values in a DBGrid, for example:

  SeriesDataSet1.Series := Series1;
  DataSource1.DataSet   := SeriesDataSet1;
  DBGrid1.DataSource    := DataSource1;

  To refresh data:

  SeriesDataSet1.Close;
  Series1.Add(....)
  SeriesDataSet1.Open;

  Additional information under Delphi 3 or 4 \Demos\TextData
  
}
interface

uses DB,Classes,Teengine,Graphics;

Const MaxLabelLen=128;

type
  PFloat=^Double;
  PSeriesPoint=^TSeriesPoint;
  TSeriesPoint=packed record
    Color:TColor;
    X:Double;
    Values:Array[0..10] of Double;
    ALabel:String[MaxLabelLen];
  end;

  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

{ TSeriesDataSet }

  TSeriesDataSet = class(TDataSet)
  private
    FSeries: TChartSeries;
    FBookMarks:TList;
    FCurRec: Integer;
    FLastBookmark: Integer;
    Function RecInfoOfs: Integer;
    Function RecBufSize: Integer;
    Procedure DoCreateField(Const AFieldName:String; AType:TFieldType; ASize:Integer);
  protected
    { Overriden abstract methods (required) }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    { Additional overrides (optional) }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    Procedure SetSeries(ASeries:TChartSeries); virtual;
    Procedure AddSeriesPoint(Buffer:Pointer; ABookMark:Integer); virtual;
  public
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
  published
    property Series: TChartSeries read FSeries write SetSeries stored True;
    property Active;
  end;

implementation

uses Windows, SysUtils, Forms,TeeProco,TeCanvas;

{ TSeriesDataSet }
Procedure TSeriesDataSet.SetSeries(ASeries:TChartSeries);
Var WasActive:Boolean;
begin
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
  WasActive:=Active;
  Active:=False;
  FSeries:=ASeries;
  {$IFNDEF D1}
  if Assigned(FSeries) then FSeries.FreeNotification(Self);
  {$ENDIF}
  if Assigned(FSeries) and WasActive then Active:=True;
end;

procedure TSeriesDataSet.Notification( AComponent: TComponent;
                            Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if Assigned(FSeries) and (AComponent=FSeries) then
       SetSeries(nil);
  end;
end;

Function TSeriesDataSet.RecInfoOfs:Integer;
begin
  result:= SizeOf(TSeriesPoint);
end;

Function TSeriesDataSet.RecBufSize: Integer;
begin
  result:=RecInfoOfs + SizeOf(TRecInfo);
end;

procedure TSeriesDataSet.InternalOpen;
var I: Integer;
begin
  if not Assigned(FSeries) then Raise Exception.Create('No Series assigned!');
  { Fabricate integral bookmark values }
  FBookMarks:=TList.Create;
  for I := 1 to FSeries.Count do FBookMarks.Add(Pointer(I));
  FLastBookmark := FSeries.Count;

  FCurRec := -1;

  BookmarkSize := SizeOf(Integer);

  InternalInitFieldDefs;
  if DefaultFields then CreateFields;
  BindFields(True);
end;

procedure TSeriesDataSet.InternalClose;
begin
  FBookMarks.Free;
  FBookMarks:=nil;
  if DefaultFields then DestroyFields;
  FLastBookmark := 0;
  FCurRec := -1;
end;

function TSeriesDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FSeries);
end;

Procedure TSeriesDataSet.DoCreateField(Const AFieldName:String; AType:TFieldType; ASize:Integer);
begin
  {$IFDEF C3D5}
  With TFieldDef.Create(FieldDefs) do
  begin
    Name      := AFieldName;
    Size      := ASize;
    Required  := False;
    DataType  := AType;
  end;
  {$ELSE}
  TFieldDef.Create(FieldDefs, AFieldName, AType, ASize, False, FieldDefs.Count+1)
  {$ENDIF}
end;

procedure TSeriesDataSet.InternalInitFieldDefs;

  Function GetFieldName(Const ADefault,AName:String):String;
  begin
    if AName='' then result:=ADefault
                else result:=AName;
  end;

  Procedure AddField(IsDateTime:Boolean; Const FieldName:String);
  begin
    if IsDateTime then DoCreateField(FieldName,ftDateTime,0)
                  else DoCreateField(FieldName,ftFloat,0);
  end;

var tmp:String;
    t:Integer;
begin
  FieldDefs.Clear;
  if Assigned(FSeries) then
  begin
    {$IFDEF C3D5}
    With TFieldDef.Create(FieldDefs) do
    begin
      Name:='Color';
      DataType:=ftInteger;
      Size:=0;
      Required:=False;
      FieldNo:=1;
    end;
    {$ELSE}
    TFieldDef.Create(FieldDefs, 'Color', ftInteger, 0, False, 1);
    {$ENDIF}
    With FSeries.XValues do AddField(DateTime,GetFieldName('X',Name));
    With FSeries.YValues do AddField(DateTime,GetFieldName('Y',Name));
    {$IFDEF C3D5}
    With TFieldDef.Create(FieldDefs) do
    begin
      Name:='Label';
      DataType:=ftString;
      Size:=MaxLabelLen;
      Required:=False;
      FieldNo:=4;
    end;
    {$ELSE}
    TFieldDef.Create(FieldDefs, 'Label', ftString, MaxLabelLen, False, 4);
    {$ENDIF}
    for t:=2 to FSeries.ValuesLists.Count-1 do
    With FSeries.ValuesLists.ValueList[t] do
    begin
      tmp:=Name;
      if Name='' then tmp:='Value'+IntToStr(t)
                 else tmp:=Name;
      AddField(DateTime,tmp);
    end;
  end;
end;

procedure TSeriesDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TSeriesDataSet.InternalGotoBookmark(Bookmark: Pointer);
var Index: Integer;
begin
  Index := FBookMarks.IndexOf(Pointer(PInteger(Bookmark)^));
  if Index <> -1 then
    FCurRec := Index
  else
    DatabaseError('Bookmark not found');
end;

procedure TSeriesDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PRecInfo(Buffer + RecInfoOfs).Bookmark);
end;

function TSeriesDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + RecInfoOfs).BookmarkFlag;
end;

procedure TSeriesDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + RecInfoOfs).BookmarkFlag := Value;
end;

procedure TSeriesDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer + RecInfoOfs).Bookmark;
end;

procedure TSeriesDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecInfo(Buffer + RecInfoOfs).Bookmark := PInteger(Data)^;
end;

function TSeriesDataSet.GetRecordSize: Word;
begin
  if Assigned(FSeries) then result:=SizeOf(TSeriesPoint)
                       else result:=0;
end;

function TSeriesDataSet.AllocRecordBuffer: PChar;
begin
  GetMem(Result, RecBufSize);
end;

procedure TSeriesDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer, RecBufSize);
end;

function TSeriesDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var t:Integer;
begin
  result:=grError;
  if Assigned(FSeries) then
  begin
    if FSeries.Count < 1 then Result := grEOF
    else
    begin
      Result := grOK;
      case GetMode of
        gmNext: if FCurRec >= RecordCount - 1  then Result := grEOF
                                                 else Inc(FCurRec);
       gmPrior: if FCurRec <= 0 then Result := grBOF
                                  else Dec(FCurRec);
     gmCurrent: if (FCurRec < 0) or (FCurRec >= RecordCount) then
                   Result := grError;
      end;
      if Result = grOK then
      begin
        With PSeriesPoint(Buffer)^ do
        begin
          Color:=FSeries.ValueColor[FCurRec];
          X:=FSeries.XValue[FCurRec];
          ALabel:=FSeries.XLabel[FCurRec];
          for t:=1 to FSeries.ValuesLists.Count-1 do
              Values[t-1]:=FSeries.ValuesLists[t][FCurRec];
        end;
        with PRecInfo(Buffer + RecInfoOfs)^ do
        begin
          BookmarkFlag := bfCurrent;
          Bookmark := Integer(FBookMarks[FCurRec]);
        end;
      end else
        if (Result = grError) and DoCheck then DatabaseError('No Records');
    end;
  end
  else if DoCheck then DatabaseError('No Records');
end;

procedure TSeriesDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, RecordSize, 0);
end;

function TSeriesDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

   Function GetSeriesValue(AList:TChartValueList):Double;
   var t:Integer;
   begin
     if AList=FSeries.XValues then result:=PSeriesPoint(ActiveBuffer)^.X
     else
     begin
       result:=0;
       for t:=1 to FSeries.ValuesLists.Count-1 do
       if AList=FSeries.ValuesLists[t] then
       begin
         result:=PSeriesPoint(ActiveBuffer)^.Values[t-1];
         break;
       end;
     end;
     if AList.DateTime then result:=TimeStampToMSecs(DateTimeToTimeStamp(result));
   end;

begin
  Result :=True;
  if (ActiveBuffer<>nil) then
  Case Field.FieldNo of
    1: PInteger(Buffer)^:=PSeriesPoint(ActiveBuffer)^.Color;
    2: PFloat(Buffer)^:=GetSeriesValue(FSeries.XValues);
    3: PFloat(Buffer)^:=GetSeriesValue(FSeries.YValues);
    4: begin
         StrPCopy(Buffer, PSeriesPoint(ActiveBuffer)^.ALabel);
         result := PChar(Buffer)^ <> #0;
       end;
    else
    begin
      PFloat(Buffer)^:=GetSeriesValue(FSeries.ValuesLists[Field.FieldNo-3]);
    end;
  end
  else result:=False;
end;

procedure TSeriesDataSet.SetFieldData(Field: TField; Buffer: Pointer);

   Function GetValue(IsDateTime:Boolean):Double;
   begin
     result:=PFloat(Buffer)^;
     if IsDateTime then result:=TimeStampToDateTime(MSecsToTimeStamp(result));
   end;

begin
  if (ActiveBuffer<>nil) then
  Case Field.FieldNo of
    1: PSeriesPoint(ActiveBuffer)^.Color:=PInteger(Buffer)^;
    2: PSeriesPoint(ActiveBuffer)^.X:=GetValue(FSeries.XValues.DateTime);
    3: PSeriesPoint(ActiveBuffer)^.Values[0]:=GetValue(FSeries.YValues.DateTime);
    4: PSeriesPoint(ActiveBuffer)^.ALabel:=PChar(Buffer);
  else
    PSeriesPoint(ActiveBuffer)^.Values[Field.FieldNo-4]:=GetValue(FSeries.ValuesLists[Field.FieldNo-3].DateTime);
  end;
  DataEvent(deFieldChange, Longint(Field));
end;

procedure TSeriesDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TSeriesDataSet.InternalLast;
begin
  FCurRec := FSeries.Count;
end;

Procedure TSeriesDataSet.AddSeriesPoint(Buffer:Pointer; ABookMark:Integer);
var t,tmp:Integer;
begin
  With PSeriesPoint(Buffer)^ do
  begin
    tmp:=FSeries.AddXY(X,Values[0],ALabel,Color);
    for t:=2 to FSeries.ValuesLists.Count-1 do
        FSeries.ValuesLists[t].TempValue:=Values[t-1];
    FSeries.AddValue(tmp);
  end;
  FBookMarks.Add(Pointer(ABookMark));
end;

procedure TSeriesDataSet.InternalPost;
var t:Integer;
begin
  if State = dsEdit then
  With PSeriesPoint(ActiveBuffer)^ do
  Begin
    FSeries.ValueColor[FCurRec]:=Color;
    FSeries.XValue[FCurRec]:=X;
    FSeries.YValue[FCurRec]:=Values[0];
    FSeries.XLabel[FCurRec]:=ALabel;
    for t:=2 to FSeries.ValuesLists.Count-1 do
        FSeries.ValuesLists[t][FCurRec]:=Values[t-1];
  end
  else
  begin
    Inc(FLastBookmark);
    AddSeriesPoint(ActiveBuffer,FLastBookMark);
  end;
end;

procedure TSeriesDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  Inc(FLastBookmark);
  if Append then InternalLast;
  AddSeriesPoint(Buffer,FLastBookmark);
end;

procedure TSeriesDataSet.InternalDelete;
begin
  FSeries.Delete(FCurRec);
  FBookMarks.Delete(FCurRec);
  if FCurRec >= RecordCount then Dec(FCurRec);
end;

function TSeriesDataSet.GetRecordCount: Longint;
begin
  Result := FSeries.Count;
end;

function TSeriesDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
     Result := 1
  else
     Result := FCurRec + 1;
end;

procedure TSeriesDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= RecordCount) then
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
end;

end.
