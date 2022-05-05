unit Rochedo.Simulators.Components;

interface
uses Classes,
     Types,
     Controls,
     SysUtilsEx,
     WinUtils,
     Graphics,
     Dialogs,     // <---- DEBUG
     ActnList,
     XML_Utils,
     MSXML4,
     SysUtils;

type
  TSimulatorComponent = class;
  IObject = interface;

  // ------------------------------------------------------------------------ //

  IDisposable = interface
    procedure Dispose();
  end;

  TenumToolBoxAction = (tbaSelect, tbaDrag, tbaConnect, tbaNewComponent);

  IToolBoxServices = interface
    function getActiveAction(): TenumToolBoxAction;
    function createComponent(Parent: TWinControl): TSimulatorComponent;
  end;

  IRules = interface
    function canCreateComponent(ToolBox: IToolBoxServices; Target: TObject; var X, Y: integer): boolean;
  end;

  IService = interface
    ['{27B84DF3-699A-4125-99D4-5DCDA5F6DE28}']
    function getServiceName(): string;
    function getServiceDescription(): string;
    function getServiceInstance(): TObject;
  end;

  TNotifyAction = (naCreation,
                   naDestruction,
                   naObjSelected,
                   naConnectionBegin,
                   naConnectionEnd,
                   naConnectionReset);

  IProject = interface
     function selectObject(p: TPoint): IObject;
    procedure notify(Action: TNotifyAction; Obj1, Obj2: IObject);
    procedure registerService(const Name: string; Service: IService);
    procedure unregisterService(const Name: string);
     function getService(const Name: string): IService;
     function getInstance(): TObject;
  end;

  IDesigner = interface
    ['{9A1F9299-B80D-450B-B33C-43F6C8F56E81}']

    // component servises
    procedure update();
    procedure paintComponents();
    procedure addComponent(c: TSimulatorComponent);
    procedure removeComponent(c: TSimulatorComponent);
     function getComponentCount(): integer;
     function getComponent(i: integer): TSimulatorComponent;
     function getMouseDownEvent(): TMouseEvent;
     function getMouseMoveEvent(): TMouseMoveEvent;
     function getMouseUpEvent(): TMouseEvent;
     function getControl(): TWinControl;
     function getCanvas(): TCanvas;
     function getProject(): IProject;
  end;

  IPainter = interface
    procedure Paint(Designer: IDesigner);
  end;

  IObject = interface(IDisposable)
    ['{4A93EDFF-267E-4B76-BC8E-38AB3A691197}']

    procedure dblClickEvent(Sender: TSimulatorComponent);
    procedure getActions(Actions: TActionList);
    procedure setProject(Project: IProject);

    // Caso a coneccao nao possa ser realizada o objeto implementador devera lancar
    // uma excecao explicando o porque.
    procedure Connect(Target: IObject);

    // Envia notificaoes para os objetos implementadores
    procedure Notify(Action: TNotifyAction);

     // Confirma se o objeto implementador pode ser destruido
     function canDestroy(): boolean;

     // Responsavel por salvar os dados do objeto implementador
     procedure toXML(x: TXML_Writer);
     procedure fromXML(n: IXMLDOMNode);

     function getDescription(): string;
     function getInstance(): TObject;
  end;

  IVisualObject = interface(IObject)
    ['{4A93EDFF-267E-4B76-BC8E-38AB3A691197}']
    procedure setDesigner(Designer: IDesigner);
  end;

  IComponent = interface(IDisposable)
    ['{6F326D33-69A6-4923-AE98-D4C4178456DC}']

    // events
    procedure setMouseDownEvent(Event: TMouseEvent);
    procedure setMouseMoveEvent(Event: TMouseMoveEvent);
    procedure setMouseUpEvent(Event: TMouseEvent);
    procedure setClickEvent(Event: TNotifyEvent);
    procedure setDblClickEvent(Event: TNotifyEvent);

    // positions and bounds
    procedure setBounds(const Rect: TRect);
     function getBounds(): TRect;
    procedure setPos(Pos: TPoint);
     function getPos(): TPoint;

    // miscelaneos
    procedure setParent(Parent: TWinControl);
    procedure setObject(Obj: IVisualObject);
     function getObject(): IVisualObject;
    procedure setHint(const s: string);
    procedure setColor(const Color: TColor);
     function getColor(): TColor;
    procedure Paint();
     function getInstance(): TObject;
  end;

  // ------------------------------ TSimulatorComponent ------------------------------- //

  TSimulatorComponent = {abstract} class
  private
    FComponent: IComponent;
    FObject: IVisualObject;
    FDesigner: IDesigner;
    procedure DblClickEvent(Sender: TObject);
  protected
    function CreateComponent(): IComponent; virtual;
    function CreateObject(): IVisualObject; virtual;
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy(); override;

    procedure disconnectDesigner();
    procedure toXML(x: TXML_Writer);
    procedure fromXML(n: IXMLDOMNode);

    property Designer  : IDesigner     read FDesigner;
    property Component : IComponent    read FComponent;
    property Obj       : IVisualObject read FObject;
  end;

  // ------------------ Formas de Componentes pre-definidos ----------------------- //

  // Descendentes deste componente mostram um retangulo na tela
  TSquareComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // Descendentes deste componente mostram uma bandeira na tela
  TFlagComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // Descendentes deste componente mostram uma elipse/circulo na tela
  TEllipseComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // Descendentes deste componente mostram um Triangulo na tela
  TTriangleComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // Descendentes deste componente mostram uma seta na tela
  TArrowComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // Descendentes deste componente mostram um Bitmap na tela
  TBitmapComponent = class(TSimulatorComponent)
  protected
    function CreateComponent(): IComponent; override;
  end;

  // ------------------------------ TSimpleObject --------------------------------- //

  // Base para classes que implementam IVisualObject
  TSimpleObject = class(T_NRC_InterfacedObject, IVisualObject)
  private
     function getInstance(): TObject;
  protected
    Designer: IDesigner;
    Project: IProject;
    destructor destroy(); override;
    procedure Dispose();
    procedure setProject(Project: IProject); virtual;
    procedure setDesigner(Designer: IDesigner); virtual;
    procedure Connect(Target: IObject); virtual;
    procedure dblClickEvent(Sender: TSimulatorComponent); virtual;
    procedure getActions(Actions: TActionList); virtual;
    procedure Notify(Action: TNotifyAction); virtual;
    procedure toXML(x: TXML_Writer); virtual;
    procedure fromXML(n: IXMLDOMNode);virtual;
     function getDescription(): string; virtual;
     function canDestroy(): boolean; virtual;
  end;

  // ----------------------------- TStringListComponent ---------------------------- //

  // Objeto que pode ser apontado por um objeto de simulacao
  // Implementa uma lista de strings ou seja um texto
  TStringListObject = class(TSimpleObject)
  private
    FStringList: TStringList;
    procedure EditEvent(Sender: TObject);
  protected
    destructor Destroy(); override;
    procedure dblClickEvent(Sender: TSimulatorComponent); override;
    procedure getActions(Actions: TActionList); override;
     function getDescription(): string; override;
  public
    constructor Create();
    property Strings : TStringList read FStringList;
  end;

  TStringListComponent = class(TSquareComponent)
  protected
    function CreateObject(): IVisualObject; override;
  end;

  // ----------------------------- TServices ---------------------------- //

  // Classe que implementa uma lista de servicos
  TServices = class
  private
    Services: TStringList;
    Interfaces: TInterfaceList;
    destructor Destroy(); override;
  public
    procedure registerService(const Name: string; Service: IService);
    procedure unregisterService(const Name: string);
     function getService(const Name: string): IService;
  end;

  // ----------------------------- TServiceComponent ---------------------------- //

  // TService e TServiceComponent implementam as bases para todos os servicos

  TService = {abstract} class(TSimpleObject, IService)
  private
    function getServiceInstance(): TObject;
  protected
    function getServiceName(): string; virtual;
    function getServiceDescription(): string; virtual;
  end;

  TServiceComponent = {abstract} class(TSquareComponent);

  // ----------------------------- TNameServiceComponent ---------------------------- //

  TNameService = class(TService)
  private
    FNames: TStringList;
  protected
    destructor destroy(); override;
    function getServiceName(): string; override;
    function getServiceDescription(): string; override;
  public
    constructor create();

     function registerName(const name: string; obj: TObject): boolean;
    procedure unregisterName(const name: string);
     function changeName(const oldName, newName: string): boolean;
     function getObjectByName(const name: string): TObject;
  end;

  TNameServiceComponent = class(TServiceComponent)
  protected
    function CreateObject(): IVisualObject; override;  
  end;

implementation
uses Rochedo.Simulators.Shapes,
     Rochedo.Dialogs.EditorServices;

{ TSimulatorComponent }

constructor TSimulatorComponent.Create(Parent: TWinControl);
begin
  inherited Create();

  Parent.GetInterface(IDesigner, FDesigner);

  FComponent := CreateComponent();
  FComponent.setParent( Parent );
  FComponent.setDblClickEvent( DblClickEvent );
  FComponent.setMouseDownEvent( FDesigner.getMouseDownEvent() );
  FComponent.setMouseMoveEvent( FDesigner.getMouseMoveEvent() );
  FComponent.setMouseUpEvent( FDesigner.getMouseUpEvent() );

  FObject := CreateObject();
  if FObject <> nil then
     begin
     FObject.setDesigner( FDesigner );
     FObject.setProject( FDesigner.getProject() );
     FComponent.setObject( FObject );
     end;

  FDesigner.AddComponent( self );
end;

destructor TSimulatorComponent.Destroy();
begin
  FComponent.setObject(nil);

  if FObject <> nil then
     begin
     FObject.Dispose();
     pointer(FObject) := nil;
     end;

  if FDesigner <> nil then
     begin
     FDesigner.RemoveComponent(self);
     pointer(FDesigner) := nil;
     end;

  FComponent.Dispose();
  pointer(FComponent) := nil;

  inherited Destroy();
end;

procedure TSimulatorComponent.DisconnectDesigner();
begin
  pointer(FDesigner) := nil;
end;

function TSimulatorComponent.CreateComponent(): IComponent;
begin
  SysUtilsEx.VirtualError(self, 'CreateComponent');
end;

function TSimulatorComponent.CreateObject(): IVisualObject;
begin
  result := nil;
end;

procedure TSimulatorComponent.DblClickEvent(Sender: TObject);
begin
  if FObject <> nil then
     FObject.DblClickEvent(self);
end;

procedure TSimulatorComponent.toXML(x: TXML_Writer);
begin
  x.beginTag('component', ['class'], [ClassName]);
    x.beginIdent();

    // Component data
    x.Write('x', Component.getPos().x);
    x.Write('y', Component.getPos().y);
    x.Write('color', Component.getColor());

    // Object data
    x.beginTag('objectData');
      x.beginIdent();
      Obj.toXML(x);
      x.endIdent();
    x.endTag('objectData');

    x.endIdent();
  x.endTag('component');
end;

procedure TSimulatorComponent.fromXML(n: IXMLDOMNode);
var p: TPoint;
    c: TColor;
begin
  // Component Data
  n.childNodes.reset();

  p.X := toInt(n.childNodes.item[0].text);
  p.Y := toInt(n.childNodes.item[1].text);
  c   := toInt(n.childNodes.item[2].text);

  Component.setPos(p);
  Component.setColor(c);

  // Object Data
  Obj.fromXML(n.childNodes.item[3]);
end;

{ TSquareComponent }

function TSquareComponent.CreateComponent(): IComponent;
begin
  result := TdrRectangle.Create(nil);
end;

{ TStringListComponent }

function TStringListComponent.CreateObject(): IVisualObject;
begin
  result := TStringListObject.Create();
end;

{ TSimpleObject }

function TSimpleObject.canDestroy(): boolean;
begin
  result := true;
end;

procedure TSimpleObject.Connect(Target: IObject);
begin

end;

procedure TSimpleObject.dblClickEvent(Sender: TSimulatorComponent);
begin

end;

destructor TSimpleObject.Destroy();
begin
  pointer(Designer) := nil;
  pointer(Project) := nil;
  inherited Destroy();
end;

procedure TSimpleObject.Dispose();
begin
  Free();
end;

procedure TSimpleObject.fromXML(n: IXMLDOMNode);
begin

end;

procedure TSimpleObject.getActions(Actions: TActionList);
begin

end;

function TSimpleObject.getDescription(): string;
begin
  result := '';
end;

function TSimpleObject.getInstance(): TObject;
begin
  result := self;
end;

procedure TSimpleObject.Notify(Action: TNotifyAction);
begin

end;

procedure TSimpleObject.setDesigner(Designer: IDesigner);
begin
  self.Designer := Designer;
end;

procedure TSimpleObject.setProject(Project: IProject);
begin
  self.Project := Project;
end;

procedure TSimpleObject.toXML(x: TXML_Writer);
begin

end;

{ TStringListObject }

constructor TStringListObject.Create();
begin
  inherited Create();
  FStringList := TStringList.Create();
  FStringList.Text := 'Linha 1'#13'Linha 2';
end;

destructor TStringListObject.Destroy();
begin
  FStringList.Free();
  inherited Destroy();
end;

procedure TStringListObject.dblClickEvent(Sender: TSimulatorComponent);
begin
  EditEvent(Sender);
end;

procedure TStringListObject.EditEvent(Sender: TObject);
begin
  getEditorServices.CreateEditor('TSC_StringsDialog').edit(self);
end;

procedure TStringListObject.getActions(Actions: TActionList);
begin
  WinUtils.CreateAction(Actions, nil, 'Edit', false, EditEvent, self);
end;

function TStringListObject.getDescription(): string;
begin
  result := FStringList.Text;
end;

{ TService }

function TService.getServiceDescription(): string;
begin
  result := '';
end;

function TService.getServiceInstance(): TObject;
begin
  result := self;
end;

function TService.getServiceName(): string;
begin
  result := self.className;
end;

{ TNameService }

constructor TNameService.Create();
begin
  inherited Create();
  FNames := TStringList.Create();
  FNames.Sorted := true;
  FNames.Duplicates := Classes.dupIgnore;
end;

destructor TNameService.Destroy();
begin
  FNames.Free();
  inherited;
end;

function TNameService.changeName(const oldName, newName: string): boolean;
var i1, i2: Integer;
begin
  i1 := FNames.IndexOf(oldName);
  i2 := FNames.IndexOf(newName);
  result := (i1 > -1) and (i2 = -1);
  if result then
     begin
     FNames.Sorted := false;
     FNames[i1] := newName;
     FNames.Sorted := true;
     end;
end;

function TNameService.getObjectByName(const name: string): TObject;
var i: Integer;
begin
  i := FNames.IndexOf(name);
  if i > -1 then
     result := FNames.Objects[i]
  else
     result := nil;
end;

function TNameService.getServiceDescription(): string;
begin
  result := 'Gerencia os nomes dos objetos';
end;

function TNameService.registerName(const name: string; obj: TObject): boolean;
begin
  result := (FNames.IndexOf(name) = -1);
  if result then
     FNames.AddObject(name, obj);
end;

procedure TNameService.unregisterName(const name: string);
var i: Integer;
begin
  i := FNames.IndexOf(name);
  if i > -1 then FNames.Delete(i);
end;

function TNameService.getServiceName(): string;
begin
  result := 'NameService';
end;

{ TNameServiceComponent }

function TNameServiceComponent.CreateObject(): IVisualObject;
begin
  result := TNameService.Create();
end;

{ TServices }

destructor TServices.Destroy();
begin
  Services.Free();
  Interfaces.Free();
  inherited Destroy();
end;

procedure TServices.registerService(const Name: string; Service: IService);
begin
  if Services = nil then
     begin
     Services := TStringList.Create();
     Interfaces := TInterfaceList.Create();
     end;

  Services.Add(Name);
  Interfaces.Add(Service);
end;

function TServices.getService(const Name: string): IService;
var i: integer;
begin
  i := Services.IndexOf(Name);
  if i > -1 then
     result := IService(Interfaces[i])
  else
     result := nil;
end;

procedure TServices.unregisterService(const Name: string);
var i: integer;
begin
  i := Services.IndexOf(Name);
  if i > -1 then
     begin
     Services.Delete(i);
     Interfaces.Delete(i);
     end;
end;

{ TFlagComponent }

function TFlagComponent.CreateComponent(): IComponent;
var c: TdrFlag;
begin
  c := TdrFlag.Create(nil);
  c.Transparent := false;
  c.BackgroundColor := clBtnFace;
  c.SetBounds(50, 50, 50, 50);
  c.ThreeD := false;
  result := c;
end;

{ TElipseComponent }

function TEllipseComponent.CreateComponent(): IComponent;
begin
  result := TdrEllipse.Create(nil);
end;

{ TTriangleComponent }

function TTriangleComponent.CreateComponent: IComponent;
begin
  result := TdrTriangle.Create(nil);
end;

{ TArrowComponent }

function TArrowComponent.CreateComponent(): IComponent;
var c: TdrArrow;
begin
  c := TdrArrow.Create(nil);
  c.SetBounds(50, 50, 80, 30);
  c.ArrowHeight := 20;
  c.ThreeD := true;
  c.Transparent := false;
  c.BackgroundColor := clBtnFace;
  c.Color := clRED;
  result := c;
end;

{ TBitmapComponent }

function TBitmapComponent.CreateComponent(): IComponent;
begin
  result := TdrBitmap.Create(nil, '');
end;

end.
