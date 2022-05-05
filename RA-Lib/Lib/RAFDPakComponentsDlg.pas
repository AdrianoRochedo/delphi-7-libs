unit RAFDPakComponentsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type

  TRAPakComponentsDlg = class(TForm)
    InstalledGroupBox: TGroupBox;
    ComponentsListBox: TListBox;
    OKButton: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComponentsListBoxDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmap: TBitmap;
    procedure ReadComponents(const Module: HModule);
  end;

  TPakComponentsDlg = class(TRAPakComponentsDlg);

  procedure Show(const Module: HModule);

implementation

uses RAFD, RAFDPalette;

{$R *.dfm}

procedure Show(const Module: HModule);
begin
  with TPakComponentsDlg.Create(Application) do
    try
      ReadComponents(Module);
      ShowModal;
    finally
      Free;
    end;
end;

{ TRAFDPakComponentsDlg }

procedure TRAPakComponentsDlg.ReadComponents(const Module: HModule);
var
  i: Integer;
begin
  ComponentsListBox.Clear;
  for i := 0 to ComponentList.Count - 1 do
    if ComponentList[i].Module = Module then
      ComponentsListBox.Items.AddObject(ComponentList[i].
        ComponentClass.ClassName, ComponentList[i]);

end;

procedure TRAPakComponentsDlg.FormCreate(Sender: TObject);
begin
  { locale }
  Caption := ResStr(dePakComponentsDlgCaption, Caption);
  OKButton.Caption := ResStr(deOK, OKButton.Caption);
  Button1.Caption := ResStr(deHelp, Button1.Caption);
  InstalledGroupBox.Caption := ResStr(dePakComponentsDlgInstalled,
    InstalledGroupBox.Caption);

  if not Button1.Visible then
    OKButton.Left := Button1.Left;

  FBitmap := TBitmap.Create;
  FBitmap.Transparent := True;
end;

procedure TRAPakComponentsDlg.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TRAPakComponentsDlg.ComponentsListBoxDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  oldColor: TColor;
  R: TRect;
begin
  with (Control as TListBox).Canvas do
  begin
    oldColor := Brush.Color;
    FillRect(Rect);

    TPaletteEntry((Control as TListBox).Items.Objects[Index]).LoadBitmap(FBitmap);

    Brush.Color := clBtnFace;
    with Control as TListBox do
      R := Bounds(Rect.Left + 1, Rect.Top + 1, ItemHeight - 2, ItemHeight - 2);
    FillRect(R);
    DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT);
    CopyMode := cmPatPaint;
    Draw(R.Left + 2, R.Top + 2, FBitmap);

    Brush.Color := oldColor;
    Inc(Rect.Left, R.Right - R.Left + 6);
    Inc(Rect.Top, 1);
    DrawText(Handle, PChar((Control as TListBox).Items[Index]), -1, Rect,
      DT_SINGLELINE or DT_VCENTER);
	end;
end;


end.
