unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Rochedo.Simulators.Components, StdCtrls, Buttons;

type
  TfoMain = class(TForm, IToolBoxServices, IPainter, IProject, IRules)
    btnSel: TSpeedButton;
    btnDrag: TSpeedButton;
    btnNC: TSpeedButton;
    Memo: TMemo;
    btnConnect: TSpeedButton;
  private
    // controle dos servicos
    Services: TServices;

    // IRules
    function canCreateComponent(ToolBox: IToolBoxServices; Target: TObject; var X, Y: integer): boolean;

    function getActiveAction(): TenumToolBoxAction;
    function createComponent(Parent: TWinControl): TSimulatorComponent;
    procedure Paint(Designer: IDesigner);
    function SelectObject(p: TPoint): IObject;
    procedure Notify(Action: TNotifyAction; Obj1, Obj2: IObject);
    procedure registerService(const Name: string; Service: IService);
    procedure unregisterService(const Name: string);
     function getService(const Name: string): IService;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.dfm}

{ TfoMain }

function TfoMain.createComponent(Parent: TWinControl): TSimulatorComponent;
begin
  if btnNC.Down then
     result := TStringListComponent.Create(Parent);
end;

function TfoMain.getActiveAction(): TenumToolBoxAction;
begin
  if btnSel.Down then
     result := tbaSelect
  else
  if btnDrag.Down then
     result := tbaDrag
  else
  if btnNC.Down then
     result := tbaNewComponent
  else
  if btnConnect.Down then
     result := tbaConnect
  else
end;

procedure TfoMain.Notify(Action: TNotifyAction; Obj1, Obj2: IObject);
begin
  case Action of
    naComponentCreation:
      Memo.Lines.Add('Component Creation');

    naComponentDestruction:
      Memo.Lines.Add('Component Destruction');

    naObjSelected:
      if Obj1 <> nil then
         Memo.Lines.Add(Obj1.getDescription());

    naConnectionBegin:
      Memo.Lines.Add( Format('Object %s selected',
                      [Obj1.getInstance().ClassName]) );

    naConnectionEnd:
      Memo.Lines.Add( Format('Object %s connected in %s',
                      [Obj1.getInstance().ClassName,
                       Obj2.getInstance().ClassName]) );
  end;
end;

procedure TfoMain.Paint(Designer: IDesigner);
var i: integer;
    canvas: TCanvas;
    p: TPoint;
    c: IComponent;
begin
  canvas := Designer.getCanvas();

  // pinta o fundo
  canvas.Brush.Color := TForm(Designer.getControl()).Color;
  canvas.FillRect( Designer.getControl().ClientRect );
(*
  // pinta as setas
  canvas.Pen.Color := clBLACK;
  for i := 0 to Designer.getComponentCount()-1 do
    begin
    c := Designer.getComponent(i).Component;
    p := c.getPos();

    if i = 0 then
       canvas.MoveTo(p.x, p.y)
    else
       canvas.LineTo(p.x, p.y);
    end;
*)
  // pinta os componentes
  Designer.PaintComponents();
end;

function TfoMain.SelectObject(p: TPoint): IObject;
begin
  Memo.Lines.Add( Format('SelectObject  x: %d,  y: %d', [p.x, p.y]) );
end;

procedure TfoMain.registerService(const Name: string; Service: IService);
begin
  Services.registerService(Name, Service);
end;

function TfoMain.getService(const Name: string): IService;
begin
  result := Services.getService(Name);
end;

procedure TfoMain.unregisterService(const Name: string);
begin
  Services.unregisterService(Name);
end;

constructor TfoMain.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  Services := TServices.Create();
end;

destructor TfoMain.Destroy;
begin
  Services.Free();
  inherited Destroy();
end;

function TfoMain.canCreateComponent(ToolBox: IToolBoxServices; Target: TObject; var X, Y: integer): boolean;
var c: IComponent;
    o: IObject;
    p: TPoint;
begin
  result := false;

  if SysUtils.Supports(Target, IDesigner) then
     result := true
  else

  if Target.GetInterface(IComponent, c) then
     begin
     o := c.getObject();
     result := (o <> nil);
     if result then
        begin
        p := Mouse.CursorPos;
        inc(p.X, 40);
        X := p.X;
        Y := p.Y;
        end;
     end;
end;

end.
