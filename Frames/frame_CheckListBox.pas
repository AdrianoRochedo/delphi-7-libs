unit Frame_CheckListBox;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, CheckLst, ExtCtrls;

type

  TFrameCheckListBox = class(TFrame)
    lbItems: TCheckListBox;
    Menu: TPopupMenu;
    MenuST: TMenuItem;
    MenuSN: TMenuItem;
    Panel: TPanel;
    procedure MenuSTClick(Sender: TObject);
    procedure MenuSNClick(Sender: TObject);
    procedure lbItemsClickCheck(Sender: TObject);
  private
    FItemsSel: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Retorna os Items selecionados
    property ItemsSel : TStrings read FItemsSel;
  end;

implementation

{$R *.DFM}

constructor TFrameCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemsSel := TStringList.Create;
end;

destructor TFrameCheckListBox.Destroy;
begin
  FItemsSel.Free;
  inherited;
end;

procedure TFrameCheckListBox.MenuSTClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to lbItems.Items.Count-1 do
    lbItems.Checked[i] := True;
  lbItemsClickCheck(Sender);
end;

procedure TFrameCheckListBox.MenuSNClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to lbItems.Items.Count-1 do
    lbItems.Checked[i] := False;
  lbItemsClickCheck(Sender);
end;

procedure TFrameCheckListBox.lbItemsClickCheck(Sender: TObject);
var i: Integer;
begin
  FItemsSel.Clear;
  for i := 0 to lbItems.Items.Count-1 do
    if lbItems.checked[i] then
       FItemsSel.AddObject(lbItems.Items[i], lbItems.Items.Objects[i]);
end;

end.
