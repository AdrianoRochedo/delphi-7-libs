unit dr_comboForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TdrComboForm = class(TForm)
    Painel: TPanel;
    Lista: TListBox;
    procedure Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListaDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListaMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure ListaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListaKeyPress(Sender: TObject; var Key: Char);
  private
    procedure SetItem;
  public
    Controle: TWinControl;
  end;

implementation
uses ToolEdit;

{$R *.DFM}

procedure TdrComboForm.Click(Sender: TObject);
begin
  Close;
end;

procedure TdrComboForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdrComboForm.FormDeactivate(Sender: TObject);
begin
  Close;
  if Controle <> nil then controle.SetFocus;
  TdrComboBox(Controle).Combo := nil;
end;

procedure TdrComboForm.FormShow(Sender: TObject);
begin
  Lista.Items.Assign(TdrComboBox(Controle).Items);
  if TdrComboBox(Controle).ItemIndex > -1 then
     Lista.ItemIndex := TdrComboBox(Controle).ItemIndex;
  Lista.SetFocus;
end;

procedure TdrComboForm.ListaDrawItem(Control: TWinControl; Index: Integer;
                                     Rect: TRect; State: TOwnerDrawState);
var Offset : Integer;
    B      : TBitmap;
begin
  with Lista.Canvas do
    begin
    FillRect(Rect);
    Offset := 2;
    B := TBitmap(Lista.Items.Objects[index]);

    if (B <> nil) and (B is TBitmap) then
       begin
       BrushCopy(
         Bounds(Rect.Left + 2, (Rect.Top + Rect.Bottom - B.Height) div 2, B.Width, B.Height),
         B, Bounds(0, 0, B.Width, B.Height), B.TransparentColor
         );
       Offset := B.width + 6;
       end;

  TextOut(Rect.Left + Offset, Rect.Top + 2, Lista.Items[Index])
  end;
end;

procedure TdrComboForm.ListaMeasureItem(Control: TWinControl;
                       Index: Integer; var Height: Integer);
var B: TBitmap;
begin
  B := TBitmap(TdrComboBox(Controle).Items.Objects[index]);
  if (B <> nil) and (B is TBitmap) then
     begin
     Height := B.Height + 2;
     if Height < Lista.ItemHeight then Height := Lista.ItemHeight;
     end;
end;

procedure TdrComboForm.ListaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
  SetItem;
end;

procedure TdrComboForm.SetItem;
begin
  TdrComboBox(Controle).ItemIndex := Lista.ItemIndex;
  TdrComboBox(Controle).Text := Lista.Items[Lista.ItemIndex];
end;

procedure TdrComboForm.ListaKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(key) = VK_RETURN then
     begin
     close;
     SetItem;
     end
  else
     if ord(key) = VK_ESCAPE then Close;
end;

end.
