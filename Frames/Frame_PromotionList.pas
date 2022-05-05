unit Frame_PromotionList;

interface
uses
  Classes, Controls, Forms, Buttons, StdCtrls
  {$ifdef WinStat}
  ,wsMatrix, wsRotinasUteis;
  {$else}
  ;
  {$endif}

type
  TOnItemMove = procedure (Sender: TObject; FromIndex, ToIndex: Integer) of Object;

  TfrPL = class(TFrame)
    L_Text: TLabel;
    lbList: TListBox;
    btnUP: TSpeedButton;
    btnDown: TSpeedButton;
    procedure btnUPClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
  private
    FOnItemMove: TOnItemMove;
  public
    // procedimentos associativos
    function getItems: TStrings;
    procedure setItems(const Value: TStrings);

    {$ifdef WinStat}
    procedure ShowDatasetVars(DS: TwsDataSet);
    {$endif}

    property OnItemMove : TOnItemMove read FOnItemMove write FOnItemMove;
  end;

implementation

{$R *.dfm}

{ TfrDS_Vars }

procedure TfrPL.btnUPClick(Sender: TObject);
var i   : Integer;
    Aux : String;
begin
  for i := 0 to lbList.Items.Count-1 do
    if lbList.Selected[i] then
      if i >= 1 then
        begin
        Aux := lbList.Items[i-1];
        lbList.Items[i-1] := lbList.Items[i];
        lbList.Items[i] := Aux;
        lbList.ItemIndex := i-1;
        if Assigned(FOnItemMove) then FOnItemMove(Self, i, i-1);
        end;
end;

procedure TfrPL.btnDownClick(Sender: TObject);
var i    : Integer;
    Aux  : String;
begin
  for i := 0 to lbList.Items.Count-1 do
    begin
    if lbList.Selected[i] then
      if i < lbList.Items.Count-1 then
         begin
         Aux := lbList.Items[i+1];
         lbList.Items[i+1] := lbList.Items[i];
         lbList.Items[i] := Aux;
         lbList.ItemIndex := i+1;
         if Assigned(FOnItemMove) then FOnItemMove(Self, i, i+1);
         Break;
         end;
    end;
end;

{$ifdef WinStat}
procedure TfrPL.ShowDatasetVars(DS: TwsDataSet);
begin
  FillListWithColsDs(lbList.Items, DS);
end;
{$endif}

function TfrPL.getItems: TStrings;
begin
  Result := lbList.Items;
end;

procedure TfrPL.setItems(const Value: TStrings);
begin
  lbList.Items.Assign(Value);
end;

end.
