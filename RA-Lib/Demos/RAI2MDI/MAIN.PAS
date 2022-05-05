unit Main;

interface

uses Sysutils, Forms, Controls, Menus, Classes, ComCtrls,
     RAI2, RAI2Fm, Rai2_All;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    StatusBar: TStatusBar;
    N2: TMenuItem;
    RAI2Fm1: TRAI2Fm;
    procedure FileNew1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses ChildWin, About;

procedure TMainForm.FileNew1Execute(Sender: TObject);
begin
  RAI2Fm1.RunForm(ExtractFilePath(paramstr(0))+'childwin.pas');
end;

procedure TMainForm.HelpAbout1Execute(Sender: TObject);
begin
  RAI2RunFormModal(ExtractFilePath(paramstr(0))+'about.pas');
end;

procedure TMainForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

end.
