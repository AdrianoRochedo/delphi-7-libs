unit HidroSimulator.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, TB2Dock, TBX, TB2Item, TB2Toolbar,
  Rochedo.Simulators.Components,
  Rochedo.Simulators.Designer,
  HidroSimulator.Classes, StdCtrls;

type
  TMainForm = class(TForm, IToolBoxServices)
    MainMenu: TMainMenu;
    Menu_Arquivo: TMenuItem;
    Menu_Novo: TMenuItem;
    ActionList: TActionList;
    actNovo: TAction;
    Dock_Left: TTBXDock;
    Toolbar: TTBXToolbar;
    Dock_Top: TTBXDock;
    btnSelect: TTBXItem;
    btnDrag: TTBXItem;
    btnConnect: TTBXItem;
    btnPC: TTBXItem;
    N1: TMenuItem;
    Menu_Save: TMenuItem;
    Manu_Load: TMenuItem;
    procedure actNovoExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Menu_SaveClick(Sender: TObject);
    procedure Manu_LoadClick(Sender: TObject);
  private
    FDesigner: THidroDesigner;
    function newDesigner(): THidroDesigner;

    // IToolBoxServices interface
    function getActiveAction(): TenumToolBoxAction;
    function createComponent(Parent: TWinControl): TSimulatorComponent;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMainForm.actNovoExecute(Sender: TObject);
begin
  if FDesigner = nil then
     FDesigner := newDesigner();
end;

function TMainForm.newDesigner(): THidroDesigner;
begin
  result := THidroDesigner.Create();
end;

// IToolBoxServices
function TMainForm.createComponent(Parent: TWinControl): TSimulatorComponent;
begin
  if btnPC.Checked then
     result := THidroDesigner.CreateComponent(TComponent_PC.ClassName, Parent)
  else
     result := nil;
end;

// IToolBoxServices
function TMainForm.getActiveAction(): TenumToolBoxAction;
begin
  if btnSelect.Checked then
     result := tbaSelect
  else
  if btnDrag.Checked then
     result := tbaDrag
  else
  if btnConnect.Checked then
     result := tbaConnect
  else
     result := tbaNewComponent;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDesigner.Free();
end;

procedure TMainForm.Menu_SaveClick(Sender: TObject);
begin
  FDesigner.SaveToFile('d:\HidroSimulator.xml');
end;

procedure TMainForm.Manu_LoadClick(Sender: TObject);
var d: THidroDesigner;
begin
  d := newDesigner();
  d.LoadFromFile('d:\HidroSimulator.xml');
end;

end.
