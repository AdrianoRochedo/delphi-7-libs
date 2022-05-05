{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 2000 R&A

       description : View Module Dialog

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDViewDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type

  TRAViewDialog = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    Edit: TEdit;
    ListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxDblClick(Sender: TObject);
  private
    FUnits: Boolean;
    N: Integer;
    procedure Execute;
  end;

  TViewDialog = class(TRAViewDialog);

  procedure ViewUnits;
  procedure ViewForms;

implementation

uses RAFD, RAFDProjectManager, RAFDAppBuilder, RAFDEditor, RAFDDesigner;

{$R *.dfm}


function ProjectEnumProc(Param: Pointer; const FileName, UnitName,
  FormName: string): Boolean stdcall;
begin
  if TViewDialog(Param).FUnits then
    TViewDialog(Param).ListBox.Items.AddObject(UnitName, TObject(TViewDialog(Param).N))
  else if FormName <> '' then
    TViewDialog(Param).ListBox.Items.AddObject(FormName, TObject(TViewDialog(Param).N));
  Result := True;
  inc(TViewDialog(Param).N);
end;

procedure View(const Units: Boolean);
var
  ViewDialog: TViewDialog;
begin
  ViewDialog := TViewDialog.Create(Application);
  try
    if Units then
      ViewDialog.Caption := ResStr(deViewDlgUnit, ViewDialog.Caption)
    else
      ViewDialog.Caption := ResStr(deViewDlgForm, ViewDialog.Caption);

    ViewDialog.FUnits := Units;
    ProjectManager.EnumProjectUnits(ProjectEnumProc, ViewDialog);
    if AppBuilder.FormFileEditor <> nil then
      if Units then
        ViewDialog.Edit.Text := ProjectManager.GetFileUnitName(
          AppBuilder.FormFileEditor.FileName)
      else
        ViewDialog.Edit.Text := ProjectManager.GetFileFormName(
          AppBuilder.FormFileEditor.FileName);

    if ViewDialog.ShowModal = mrOk then
      ViewDialog.Execute;
  finally
    ViewDialog.Free;
  end;
end;

procedure ViewUnits;
begin
  View(True);
end;

procedure ViewForms;
begin
  View(False);
end;

procedure TRAViewDialog.FormCreate(Sender: TObject);
begin
  OKButton.Caption := ResStr(deOk, OKButton.Caption);
  CancelButton.Caption := ResStr(deCancel, CancelButton.Caption);
  HelpButton.Caption := ResStr(deHelp, HelpButton.Caption);
end;

procedure TRAViewDialog.ListBoxClick(Sender: TObject);
begin
  Edit.Text := ListBox.Items[ListBox.ItemIndex];
  Edit.SelStart := 0;
  Edit.SelLength := Length(Edit.Text);
end;

procedure TRAViewDialog.EditChange(Sender: TObject);
var
  i: Integer;
begin
  ListBox.ItemIndex := ListBox.Items.IndexOf(Edit.Text);
  OKButton.Enabled := ListBox.ItemIndex > -1;
  for i := 0 to ListBox.Items.Count - 1 do
    ListBox.Selected[i] := i = ListBox.ItemIndex;
end;

procedure TRAViewDialog.Execute;
begin
  if ListBox.ItemIndex > -1 then
  begin
    AppBuilder.OpenFile(ProjectManager.GetProjectItem(
      Integer(ListBox.Items.Objects[ListBox.ItemIndex])).FileName);
    if ActiveFileEditor <> nil then
      if FUnits then
        ActiveFileEditor.ToggleToFront
      else
        (ActiveFileEditor.Designer as TRAFormDesigner).ToggleToFront;
  end;
end;

procedure TRAViewDialog.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      begin
        SendMessage(ListBox.Handle, WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
  end;
end;

procedure TRAViewDialog.ListBoxDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
