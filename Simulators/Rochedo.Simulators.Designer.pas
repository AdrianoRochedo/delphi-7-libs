unit Rochedo.Simulators.Designer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs, ActnList, ExtCtrls, Menus,
  Rochedo.Simulators.Components;

type
  TDesigner = class(TForm, IDesigner)
    Selector: TShape;
    PopupMenu: TPopupMenu;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    obj1, obj2: IObject;
    ObjActions: TActionList;
    procedure DrawDrag(Sender: TObject);
    procedure createComponent(Sender: TObject; X, Y: integer);
    procedure manageConnection(Sender: TObject);
    procedure endDrag(Sender: TObject);
    procedure resetConnectionManager();
  protected
    Components: TObjectList;

    // Referencias
    ToolBox: IToolBoxServices;
    Project: IProject;
    Painter: IPainter;
    Rules  : IRules;

    // Movimentacao de componentes
    LockDrag  : boolean; // bloqueio de arraste
    Draging   : boolean; // indica arraste de um componente
    ActualPos : TPoint;
    OldPos    : TPoint;

    // Representa o objeto selecionado - pode ser nil
    SelectedObj : IObject;

    procedure SelectObject(Sender: TObject);
    procedure Update();
    procedure PaintComponents();
    procedure ShowMenu(Obj: IObject);

     function getProject(): IProject;
     function getControl(): TWinControl; virtual;
     function getCanvas(): TCanvas; virtual;
    procedure addComponent(c: TSimulatorComponent); virtual;
    procedure removeComponent(c: TSimulatorComponent); virtual;
     function getComponentCount(): integer;
     function getComponent(i: integer): TSimulatorComponent;
     function getMouseDownEvent(): TMouseEvent; virtual;
     function getMouseMoveEvent(): TMouseMoveEvent; virtual;
     function getMouseUpEvent(): TMouseEvent; virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure setToolBox(ToolBox: IToolBoxServices);
    procedure setProject(Project: IProject);
    procedure setPainter(Painter: IPainter);
    procedure setRules(Rules: IRules);
  end;

implementation
uses Rochedo.Utils.Menu;

{$R *.dfm}

{ TDesigner }

constructor TDesigner.Create();
begin
  // nil - Aplicacao nao tentara destruir automaticamente este formulario.
  inherited Create(nil);

  // Componentes de simulacao possuidos pelo Designer
  Components := TObjectList.Create(false);

  // Inicializacoes
  SelectObject(nil);
end;

destructor TDesigner.Destroy();
var i: Integer;
    c: TSimulatorComponent;
    o: TObject;
begin
  pointer(ToolBox) := nil;
  pointer(Project) := nil;
  pointer(Painter) := nil;
  pointer(Rules  ) := nil;
  pointer(SelectedObj) := nil;

  for i := 0 to Components.Count-1 do
    begin
    c := TSimulatorComponent( Components[i] );
    c.DisconnectDesigner();
    c.Free();
    end;

  Components.Free();
  inherited Destroy();
end;

procedure TDesigner.AddComponent(c: TSimulatorComponent);
var o: TObject;
    s: IService;
begin
  Components.Add(c);
  if (Project <> nil) and (c.Obj <> nil) then
     begin
     // Verifica se este objeto é um servico, se for, registra-o
     o := c.Obj.getInstance();
     if o.GetInterface(IService, s) then
        Project.registerService(s.getServiceName(), s);

     // Notifica a criacao de um objeto
     Project.Notify(naCreation, c.Obj, nil);
     end;
end;

procedure TDesigner.RemoveComponent(c: TSimulatorComponent);
var o: TObject;
    s: IService;
begin
  if (Project <> nil) and (c.Obj <> nil) then
     begin
     // Verifica se este objeto é um servico, se for, remove seu registro
     o := c.Obj.getInstance();
     if o.GetInterface(IService, s) then
        Project.unregisterService(s.getServiceName());

     // Notifica que este objeto vai ser destruido
     Project.Notify(naDestruction, c.Obj, nil);
     end;

  Components.Remove(c);
end;

procedure TDesigner.createComponent(Sender: TObject; X, Y: integer);
var sc: TSimulatorComponent;
begin
  if Rules.canCreateComponent(ToolBox, Sender, {var} X, Y) then
     begin
     sc := ToolBox.createComponent(self);

     if Sender = Self then
        sc.Component.setPos( Point(X, Y) )
     else
        sc.Component.setPos( self.ScreenToClient(Point(X, Y)) );

     SelectObject( sc.Component.getInstance() );
     end;
end;

procedure TDesigner.manageConnection(Sender: TObject);
var c: IComponent;
    o: IObject;
begin
  if Sender.GetInterface(IComponent, c) then
     begin
     o := c.getObject();
     if o <> nil then
        begin
        if obj1 = nil then
           begin
           obj1 := o;
           if Project <> nil then
              Project.notify(naConnectionBegin, obj1, nil);
           end
        else
           if obj2 = nil then
              try
                obj2 := o;
                if obj1 = obj2 then
                   raise Exception.Create('Um objeto não pode ser conectado a si mesmo');

                obj1.Connect(Obj2);
                if Project <> nil then
                   Project.notify(naConnectionEnd, obj1, obj2);
              finally
                resetConnectionManager();
              end;
        end
     else
        resetConnectionManager();
     end
  else
     resetConnectionManager();
end;

procedure TDesigner.endDrag(Sender: TObject);
var sp: TPoint;
     c: IComponent;
begin
  if (Sender <> getControl()) and not LockDrag then
     begin
     Draging := False;
     getCanvas().Pen.Mode := pmCopy;
     sp := getControl().ScreenToClient(Mouse.CursorPos);
     if sp.x < 2 then sp.x := 2;
     if sp.y < 2 then sp.y := 2;
     {TODO 1 -cProgramacao: Gerenciamento de coordenadas}
     //SelectedObj.setPos( Project.PointToMapPoint(sp) );
     if Sender.GetInterface(IComponent, c) then c.setPos(sp);
     Update();
     end;
end;

// ---------------------------------- Eventos ------------------------------------- //

procedure TDesigner.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
     if (ToolBox <> nil) and (ToolBox.getActiveAction() = tbaDrag) then
        if Sender = getControl() then
           //...
        else
           if not LockDrag then
              begin
              ActualPos := Point(X, Y);
              Draging := true;
              OldPos := TControl(Sender).ScreenToClient(ActualPos);
              getCanvas().Pen.Mode := pmXor;
              end
           else
              // nada
     else
       //nada
  else
     if Sender = getControl() then
        //...
     else
        begin
        SelectObject(Sender);
        if SelectedObj <> nil then ShowMenu(SelectedObj);
        end;
end;

procedure TDesigner.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Draging and (Sender <> getControl()) then
     DrawDrag(Sender);
end;

procedure TDesigner.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
     begin
     if (ToolBox <> nil) and (ToolBox.getActiveAction() = tbaNewComponent) then
         begin
         resetConnectionManager();
         createComponent(Sender, X, Y);
         end
     else
     if (ToolBox <> nil) and (ToolBox.getActiveAction() = tbaConnect) then
         begin
         manageConnection(Sender);
         end
     else
        begin
        if (ToolBox <> nil) and (ToolBox.getActiveAction() = tbaDrag) then
           endDrag(Sender);

        resetConnectionManager();
        SelectObject(Sender);
        end;
     end;
end;

// ---------------------------------- Eventos ------------------------------------- //

function TDesigner.getMouseDownEvent(): TMouseEvent;
begin
  result := FormMouseDown;
end;

function TDesigner.getMouseMoveEvent(): TMouseMoveEvent;
begin
  result := FormMouseMove;
end;

function TDesigner.getMouseUpEvent(): TMouseEvent;
begin
  result := FormMouseUp;
end;

procedure TDesigner.setToolBox(ToolBox: IToolBoxServices);
begin
  self.ToolBox := ToolBox;
end;

function TDesigner.getControl(): TWinControl;
begin
  result := self;
end;

function TDesigner.getCanvas(): TCanvas;
begin
  result := self.Canvas;
end;

procedure TDesigner.DrawDrag(Sender: TObject);
var p: TPoint;
    c: TControl;
begin
  c := TControl(Sender);
  p := getControl.ScreenToClient(Mouse.CursorPos);
  getCanvas.Rectangle(OldPos.x,
                      OldPos.y,
                      OldPos.x + c.Width,
                      OldPos.y + c.Height);

  dec(p.x, ActualPos.x);
  dec(p.y, ActualPos.y);
  if p.x < 2 then p.x := 2;
  if p.y < 2 then p.y := 2;

  OldPos := p;
  getCanvas.Rectangle(OldPos.x,
                      OldPos.y,
                      OldPos.x + c.Width,
                      OldPos.y + c.Height);
end;

procedure TDesigner.SelectObject(Sender: TObject);
var c: IComponent;
    r: TRect;
begin
  if Sender <> nil then
     if Sender.GetInterface(IComponent, c) then
        begin
        SelectedObj := c.getObject();
        r := c.getBounds();
        Selector.SetBounds(r.Left-3, r.Top-3, r.Right-r.Left+7, r.Bottom-r.Top+7);
        end
     else
        if Sender = getControl() then
           if Project <> nil then
              SelectedObj := Project.SelectObject( getControl.ScreenToClient(Mouse.CursorPos) )
           else
              SelectedObj := nil
        else
           SelectedObj := nil
  else
     SelectedObj := nil;

  if SelectedObj = nil then
     Selector.Top := -1000;

  if Project <> nil then
     Project.Notify(naObjSelected, SelectedObj, nil);
end;

procedure TDesigner.setProject(Project: IProject);
begin
  self.Project := Project;
end;

procedure TDesigner.Update();
var c: TCanvas;
begin
  if Painter <> nil then
     Painter.Paint(self)
  else
     begin
     c := getCanvas();
     c.Brush.Color := self.Color;
     c.FillRect( getControl().ClientRect );
     PaintComponents();
     end;
end;

procedure TDesigner.setPainter(Painter: IPainter);
begin
  self.Painter := Painter;
end;

function TDesigner.getComponent(i: integer): TSimulatorComponent;
begin
  result := TSimulatorComponent( Components[i] );
end;

function TDesigner.getComponentCount(): integer;
begin
  result := Components.Count;
end;

procedure TDesigner.PaintComponents();
var i: Integer;
begin
  for i := 0 to Components.Count-1 do
    getComponent(i).Component.Paint();
end;

procedure TDesigner.FormPaint(Sender: TObject);
begin
  Update();
end;

procedure TDesigner.ShowMenu(Obj: IObject);
var Menu : TMenuItem;
     act : TContainedAction;
       i : Integer;
begin
  PopupMenu.Items.Clear();
  ObjActions.Free();
  ObjActions := TActionList.Create(nil);
  Obj.getActions(ObjActions);
  for i := 0 to ObjActions.ActionCount-1 do
    begin
    act := ObjActions[i];
    Menu := MenuUtils.CreateMenuItem(PopupMenu.Items, act);
    MenuUtils.CreateSubMenus(Menu, act, false);
    end;
  PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TDesigner.getProject(): IProject;
begin
  result := Project;
end;

procedure TDesigner.setRules(Rules: IRules);
begin
  self.Rules := Rules;
end;

procedure TDesigner.resetConnectionManager();
begin
  pointer(obj1) := nil;
  pointer(obj2) := nil;
  if Project <> nil then
     Project.notify(naConnectionReset, nil, nil);
end;

procedure TDesigner.FormShow(Sender: TObject);
begin
  if Rules = nil then
     begin
     MessageDlg('Rules not defined', mtError, [mbOK], 0);
     Free();
     end;
end;

procedure TDesigner.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var i: Integer;
begin
  if (Key = Windows.VK_DELETE) and (Shift = [ssCtrl]) then
     if SelectedObj <> nil then
        if SelectedObj.canDestroy() then
           if (MessageDlg('Tem certeza que deseja remover este componente ?',
               mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
               begin
               SelectedObj.Notify(naDestruction);

               // Remove o componente
               for i := 0 to Components.Count-1 do
                 if TSimulatorComponent(Components[i]).Obj = SelectedObj then
                    begin
                    TSimulatorComponent(Components[i]).Free();
                    break;
                    end;

               SelectObject(nil);
               Update();
               end
end;

end.
