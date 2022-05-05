{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       form        : TRAFDPakListDlg
       description : installed packages dialog

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDPakListDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst;

type

  TRAPakListDlg = class(TForm)
    DesignPackageList: TCheckListBox;
    PackageNameDialog: TOpenDialog;
    AddButton: TButton;
    RemoveButton: TButton;
    ButtonComponents: TButton;
    GroupBox1: TGroupBox;
    LabelPackageFile: TLabel;
    Panel1: TPanel;
    CancelButton: TButton;
    HelpButton: TButton;
    OKButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DesignPackageListClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DesignPackageListClickCheck(Sender: TObject);
    procedure ButtonComponentsClick(Sender: TObject);
  private
    procedure UpdatePackageList;
  end;

  TPakListDlg = class(TRAPakListDlg);

  procedure Show;

implementation

uses RAUtils, RAFD, RAFDIDE, RAFDPalette, RAFDAppBuilder, RAFDPakComponentsDlg;

{$R *.dfm}

const
 {$IFDEF RA_D3}
  PackageExt = '.dpl';
 {$ELSE}
  PackageExt = '.bpl';
 {$ENDIF RA_D3}


procedure Show;
begin
  with TPakListDlg.Create(Application) do
    try
      ShowModal;
    finally
      Free;
    end;
end;


 { TRAFDPakListDlg }

procedure TRAPakListDlg.FormCreate(Sender: TObject);
begin
  { locale }
  Caption := ResStr(dePakListDlgTitle, Caption);
  OKButton.Caption := ResStr(deOK, OKButton.Caption);
  CancelButton.Caption := ResStr(deCancel, CancelButton.Caption);
  HelpButton.Caption := ResStr(deHelp, HelpButton.Caption);
  PackageNameDialog.Title := ResStr(dePakListDlgAddTitle, PackageNameDialog.Title);
  PackageNameDialog.Filter := ResStr(dePakListDlgAddFilter, PackageNameDialog.Filter);
  GroupBox1.Caption := ResStr(dePakListDlgPackages, GroupBox1.Caption);
  AddButton.Caption := ResStr(dePakListDlgAdd, AddButton.Caption);
  RemoveButton.Caption := ResStr(dePakListDlgRemove, RemoveButton.Caption);
  ButtonComponents.Caption := ResStr(dePakListDlgComponents, ButtonComponents.Caption);

  PackageNameDialog.Filter := ReplaceString(PackageNameDialog.Filter, '.ext', PackageExt);
  if not HelpButton.Visible then
  begin
    OKButton.Left := CancelButton.Left;
    CancelButton.Left := HelpButton.Left;
  end;

  UpdatePackageList;
  if DesignPackageList.Items.Count > 0 then
  begin
    DesignPackageList.ItemIndex := 0;
    DesignPackageListClick(nil);
  end;
  PackageNameDialog.InitialDir := AppBuilder.raDesktop.ReadString(
    'Installed Packages Dialog', 'InitialDir', PackageNameDialog.InitialDir);
end;

procedure TRAPakListDlg.FormDestroy(Sender: TObject);
begin
  AppBuilder.raDesktop.WriteString('Installed Packages Dialog', 'InitialDir',
    PackageNameDialog.InitialDir);
end;

procedure TRAPakListDlg.UpdatePackageList;
var
  i: Integer;
  oldItemIndex: Integer;
begin
  oldItemIndex := DesignPackageList.ItemIndex;
  DesignPackageList.Clear;
  for i := 0 to RAFDPalette.DesignPackageList.Count - 1 do
    if not RAFDPalette.DesignPackageList[i].IDEPackage then
      DesignPackageList.Items.AddObject(
        RAFDPalette.DesignPackageList[i].Description,
        RAFDPalette.DesignPackageList[i]);
  for i := 0 to DesignPackageList.Items.Count - 1 do
    DesignPackageList.Checked[i] := TDesignPackage(DesignPackageList.Items.
      Objects[i]).Loaded;
  DesignPackageList.ItemIndex := oldItemIndex;
end;

procedure TRAPakListDlg.DesignPackageListClick(Sender: TObject);
begin
  if DesignPackageList.ItemIndex > - 1 then
  begin
    LabelPackageFile.Caption := TDesignPackage(DesignPackageList.Items.
      Objects[DesignPackageList.ItemIndex]).FileName;
    ButtonComponents.Enabled := TDesignPackage(DesignPackageList.Items.
      Objects[DesignPackageList.ItemIndex]).Loaded;
  end
  else
    LabelPackageFile.Caption := '';
end;

procedure TRAPakListDlg.RemoveButtonClick(Sender: TObject);
begin
  try
    RAFDPalette.DesignPackageList.Delete(RAFDPalette.DesignPackageList.IndexOf(
      TDesignPackage(DesignPackageList.Items.Objects[DesignPackageList.ItemIndex])));
  finally
    UpdatePackageList;
  end;
end;

procedure TRAPakListDlg.AddButtonClick(Sender: TObject);
begin
  if PackageNameDialog.Execute then
  begin
    try
      RAFDPalette.DesignPackageList.Add(PackageNameDialog.FileName, True, False, '');
    finally
      UpdatePackageList;
    end;
  end;
  PackageNameDialog.InitialDir := ExtractFilePath(PackageNameDialog.FileName);
end;

procedure TRAPakListDlg.DesignPackageListClickCheck(Sender: TObject);
begin
  try
    if DesignPackageList.Checked[DesignPackageList.ItemIndex] then
      RAFDPalette.DesignPackageList.LoadPackage(
        RAFDPalette.DesignPackageList.IndexOf(
        TDesignPackage(DesignPackageList.Items.Objects[DesignPackageList.ItemIndex])))
    else
      RAFDPalette.DesignPackageList.UnloadPackage(
        RAFDPalette.DesignPackageList.IndexOf(
        TDesignPackage(DesignPackageList.Items.Objects[DesignPackageList.ItemIndex])));
  finally
    UpdatePackageList;
  end;
end;

procedure TRAPakListDlg.ButtonComponentsClick(Sender: TObject);
begin
  RAFDPakComponentsDlg.Show(TDesignPackage(DesignPackageList.Items.
    Objects[DesignPackageList.ItemIndex]).Module);
end;

end.
