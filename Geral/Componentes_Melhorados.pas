unit Componentes_Melhorados;

{ AUTOR ............................ Adriano Rochedo Conceição
  HISTÓRICO
    - 14/05/1999
      Criado um descendente de TCustomComboBox que sobreescreve alguns métodos
      para correção de um bug (eventos de OnChange disparados sem necessidade).
}

interface
uses StdCtrls, Messages, Windows, SysUtils, Classes, Controls, Forms;

type
  TdrComboBox = class(TCustomComboBox)
  private
    FoldItemIndex : Integer;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DropDown; override;
  published
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AD-2', [TdrComboBox]);
end;

procedure TdrComboBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_SELCHANGE:
      begin
      if ItemIndex <> FOldItemIndex then
         begin
         Text := Items[ItemIndex];
         Click;
         Change;
         end;
      end;
    Else
      inherited;
    end;
end;

constructor TdrComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FoldItemIndex := -1;
end;

procedure TdrComboBox.DropDown;
begin
  FoldItemIndex := ItemIndex;
  Inherited;
end;

end.
