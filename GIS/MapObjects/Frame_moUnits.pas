unit Frame_moUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, MapObjects, StdCtrls;

type
  TFrame_moUnits = class(TFrame)
    Label1: TLabel;
    cbUnits: TComboBox;
  private
    function getAsString: String;
    function getCode: Integer;
    procedure SetAsString(const Value: String);
    { Private declarations }
  public
    procedure ShowUnits;
    property UnitAsString : String  read getAsString write SetAsString;
    property UnitCode     : Integer read getCode;
  end;

implementation
uses SysUtilsEx;

{$R *.dfm}

{ TFrame_moUnits }

function TFrame_moUnits.getAsString: String;
begin
  if cbUnits.ItemIndex > -1 then
     Result := cbUnits.Items[cbUnits.ItemIndex]
  else
     Result := '';
end;

function TFrame_moUnits.getCode: Integer;
begin
  if cbUnits.ItemIndex > -1 then
     Result := Integer(cbUnits.Items.Objects[cbUnits.ItemIndex])
  else
     Result := 0;
end;

procedure TFrame_moUnits.SetAsString(const Value: String);
begin
  cbUnits.ItemIndex := cbUnits.Items.IndexOf(Value);
end;

procedure TFrame_moUnits.ShowUnits;
var moSL: ImoStrings;
    i: Integer;
    s: String;
begin
  cbUnits.Clear();
  moSL := coStrings.Create;
  moSL.PopulateWithUnits();
  for i := 0 to moSL.Count-1 do
    begin
    s := moSL.Item(i);
    cbUnits.Items.AddObject(
      LeftStringOf(s, '[', True),
      Pointer(StrToInt(SubString(s, '[', ']'))));
    end;
end;

end.
