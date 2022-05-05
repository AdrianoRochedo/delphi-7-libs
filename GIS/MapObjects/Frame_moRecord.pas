unit Frame_moRecord;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, gridx32, MapObjects;

type
  TFrame_moRecord = class(TFrame)
    Grid: TdrStringAlignGrid;
  private
    FRecordSet: IMoRecordset;
    procedure SetRecordSet(const Value: IMoRecordset);
    procedure ShowRecord();
  public
    property RecordSet: IMoRecordset read FRecordSet write SetRecordSet;
  end;

implementation
uses Math;

{$R *.dfm}

{ TFrame_moRecord }

procedure TFrame_moRecord.ShowRecord;
var i, MaxW0, MaxW1: Integer;
    fn, fv: String;
begin
  if not FRecordSet.EOF then
     begin
     MaxW0 := 0;
     MaxW1 := 0;
     Grid.RowCount := FRecordSet.TableDesc.FieldCount;
     for i := 0 to FRecordSet.TableDesc.FieldCount-1 do
       begin
       fn := FRecordSet.TableDesc.FieldName[i];
       fv := FRecordSet.Fields.Item(fn).ValueAsString;
       Grid.Cells[0, i] := ' ' + fn;
       Grid.Cells[1, i] := ' ' + fv;
       MaxW0 := Math.Max(MaxW0, Grid.Canvas.TextWidth('  ' + fn + '  '));
       MaxW1 := Math.Max(MaxW1, Grid.Canvas.TextWidth('  ' + fv + '  '));
       end;
       Grid.ColWidths[0] := MaxW0;
       Grid.ColWidths[1] := MaxW1;
     end;
end;

procedure TFrame_moRecord.SetRecordSet(const Value: IMoRecordset);
begin
  FRecordSet := Value;
  if FRecordSet <> nil then
     ShowRecord();
end;

end.
