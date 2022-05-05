unit drEdit;

{
  AUTOR: Adriano Rochedo Conceição
    1.00 - Primeira Versão
    1.01 - Incluída a possibilidade de entrada de valores em notação científica e valores negativos
}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

type
  TDataType = (dtInteger, dtFloat, dtString);

type
  TdrEdit = class(TCustomEdit)
  private
    FBeepOnError: Boolean;
    FDataType: TDataType;
    function GetAsFloat: Real;
    function GetAsInteger: Integer;
    function GetAsString: String;
    procedure SetAsFloat(const Value: Real);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
  protected
     procedure KeyPress(var Key: Char); override;
  public
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsFloat   : Real    read GetAsFloat   write SetAsFloat;
    property AsString  : String  read GetAsString  write SetAsString;
  published
    property BeepOnError : Boolean   read FBeepOnError write FBeepOnError;
    property DataType    : TDataType read FDataType    write FDataType;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation
uses SysUtilsEx;

procedure Register;
begin
  RegisterComponents('AD-2', [TdrEdit]);
end;

{ TdrEdit }

function TdrEdit.GetAsFloat: Real;
begin
  Result := StrToFloatDef(Text, 0);
end;

function TdrEdit.GetAsInteger: Integer;
begin
  if (FDataType = dtFloat) then
     Result := Trunc(StrToFloatDef(Text, 0))
  else
     Result := StrToIntDef(Text, 0);
end;

function TdrEdit.GetAsString: String;
begin
  Result := Text;
end;

procedure TdrEdit.KeyPress(var Key: Char);
{var
  DecimalSeparator : char;}
begin
  inherited;

//  DecimalSeparator := '.';
  if GetKeyState(VK_CONTROL) < 0 then Exit;

  if FDataType = dtInteger then
     Key := ValidateChar(Key, [Digitos, Controles, ['-']], FBeepOnError)
  else
     if FDataType = dtFloat then
        begin
        if Key in ['.', ','] then
           begin
           Key := DecimalSeparator;
           if System.Pos(Key, Self.Text) > 0 then
              begin
              if FBeepOnError then Beep;
              Key := #0;
              Exit;
              end;
           end;
        Key := ValidateChar(Key, [Digitos, Controles, Virgula, [DecimalSeparator], ['E', 'e', '-']], FBeepOnError);
        end;
end;

procedure TdrEdit.SetAsFloat(const Value: Real);
begin
  if (FDataType = dtFloat) or (FDataType = dtString) then
     Text := FloatToStr(Value)
  else
  if (FDataType = dtInteger) then
     Text := IntToStr(Trunc(Value));
end;

procedure TdrEdit.SetAsInteger(const Value: Integer);
begin
  Text := IntToStr(Value);
end;

procedure TdrEdit.SetAsString(const Value: String);
begin
  Text := Value;
end;

end.
