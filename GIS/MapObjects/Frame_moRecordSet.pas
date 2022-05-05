unit Frame_moRecordSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, gridx32, MapObjects;

type
  TFrame_moRecordSet = class(TFrame)
    Grid: TdrStringAlignGrid;
  private
    FRecordSet: IMoRecordset;
    FFields: array of ImoField;
    procedure SetRecordSet(const Value: IMoRecordset);
    procedure ShowHeader;
    procedure ShowRecordSet;
    procedure ShowRecord(RecNo: Integer);
  public
    property RecordSet: IMoRecordset read FRecordSet write SetRecordSet;
  end;

implementation
uses WinUtils;

{$R *.dfm}

{ TFrame_moRecordSet }

procedure TFrame_moRecordSet.ShowHeader;
var i: Integer;
    fn: String;
begin
  SetLength(FFields, FRecordSet.TableDesc.FieldCount);
  Grid.RowCount := FRecordSet.Count + 1;
  Grid.ColCount := FRecordSet.TableDesc.FieldCount + 1;
  for i := 0 to FRecordSet.TableDesc.FieldCount-1 do
    begin
    fn := FRecordSet.TableDesc.FieldName[i];
    FFields[i] := FRecordSet.Fields.Item(fn);
    Grid.ColWidths[i + 1] := Grid.Canvas.TextWidth('  ' + fn + '  ');
    Grid.Cells[i + 1, 0] := ' ' + fn;
    end;
end;

procedure TFrame_moRecordSet.ShowRecord(RecNo: Integer);
var i: Integer;
    s: String;
begin
  Grid.Cells[0, RecNo] := ' ' + IntToStr(RecNo);
  for i := 0 to FRecordSet.TableDesc.FieldCount-1 do
    Grid.Cells[i + 1, RecNo] := FFields[i].ValueAsString;
end;

procedure TFrame_moRecordSet.ShowRecordSet;
var i: Integer;
begin
  i := 1;
  FRecordSet.MoveFirst();
  while not FRecordSet.EOF do
    begin
    ShowRecord(i);
    inc(i);
    FRecordSet.MoveNext();
    end;
end;

procedure TFrame_moRecordSet.SetRecordSet(const Value: IMoRecordset);
begin
  FRecordSet := Value;
  if FRecordSet <> nil then
     begin
     WinUtils.StartWait();
     ShowHeader();
     ShowRecordSet();
     WinUtils.StopWait();
     end;
end;

end.
