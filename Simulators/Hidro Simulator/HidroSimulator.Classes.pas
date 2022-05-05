unit HidroSimulator.Classes;

interface
uses Classes, Types, ActnList, SysUtils, SysUtilsEx, Dialogs, Contnrs, WinUtils, XML_Utils,
     MSXML4, Controls,
     Rochedo.Simulators.Components,
     Rochedo.Simulators.Designer;

type
  // Enumerations
  TObjectState = (osReady, osLoading);

  // Foward declaretions
  TPC = class;

  // ----------------------------------- TSimulator ----------------------------------- //

  TSimulator = class
  private
    FPCs: TObjectList;
    constructor create();
    destructor destroy(); override;
  public
    procedure registerPC(PC: TPC);
    procedure unregisterPC(PC: TPC);
     function getPC(i: integer): TPC;
    procedure simulate(number: integer);
  end;

  // ------------------------------------- Project ----------------------------------- //

  TComponent_Project = class (TEllipseComponent)
  protected
    function createComponent(): IComponent; override;
    function createObject(): IVisualObject; override;
  end;

  TProject = class (TSimpleObject, IProject)
  private
    FNames: TStringList;
    FSimulator: TSimulator;

    constructor Create();
    destructor Destroy(); override;

    // eventos
    procedure simulateEvent(Sender: TObject);

     // IProject interface
     function selectObject(p: TPoint): IObject;
    procedure notify(Action: TNotifyAction; Obj1, Obj2: IObject);
    procedure registerService(const Name: string; Service: IService);
    procedure unregisterService(const Name: string);
     function getService(const Name: string): IService;
     function getInstance(): TObject;

    // TSimpleObject
    procedure getActions(Actions: TActionList); override;
     function canDestroy(): boolean; override;
  protected
  public
    // Names Manager ...

    // Obtera um novo nome e registra este Objeto com este nome
     function getNewName(const Prefix: string; Obj: TObject): string;
    procedure registerObject(const Name: string; Obj: TObject);
    procedure unregisterObject(const Name: string);
     function getObject(const Name: string): TObject;
  end;

  // ---------------------------------- HidroDesigner ------------------------------- //

  THidroDesigner = class (T_NRC_InterfacedObject, IRules, IPainter)
  private
    FDesigner: TDesigner;
  protected
    // IRules interface
    function canCreateComponent(ToolBox: IToolBoxServices; Target: TObject; var X, Y: integer): boolean;

    // IPainter interface
    procedure Paint(Designer: IDesigner);

    destructor Destroy(); override;
  public
    constructor Create();

    class function CreateComponent(const className: string; Parent: TWinControl): TSimulatorComponent;

    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
  end;

  // -------------------------------------- PC ------------------------------------ //

  TComponent_PC = class (TSquareComponent)
  protected
    function CreateComponent(): IComponent; override;
    function CreateObject(): IVisualObject; override;
  end;

  TPC = class (TSimpleObject)
  private
    FState: TObjectState;
    FName: string;
    FHierarchy: integer;
    FComponent: IComponent;
    FNextPC: TPC;
    FPriorsPCs: TObjectList;
    constructor Create( Component: IComponent );
    destructor Destroy(); override;
     function getPriorPC(i: integer): TPC;
     function getPriorsPCsCount(): integer;
    procedure setHierarchy(i: integer);
  protected
    // TSimpleObject
    procedure dblClickEvent(Sender: TSimulatorComponent); override;
    procedure getActions(Actions: TActionList); override;
    procedure Connect(Target: IObject); override;
    procedure setProject(Project: IProject); override;
    procedure Notify(Action: TNotifyAction); override;
    procedure toXML(x: TXML_Writer); override;
    procedure fromXML(n: IXMLDOMNode); override;
     function getDescription(): string; override;
  public
    // Indica o componente associado a este objeto
    property Component : IComponent read FComponent;

    // Nome do Objeto
    property Name : string read FName;

    // Indica o proximo PC
    property NextPC : TPC read FNextPC;

    // Retorna o numero de PCs a Montante
    property PriorsPCsCount : integer read getPriorsPCsCount;

    // Retorna o i-egimo PC a montante (0..PriorsPCsCount-1)
    property PriorPC[i: integer] : TPC read getPriorPC;

    // Indica a hierarquia do PC
    property Hierarchy : integer read FHierarchy;
  end;

implementation
uses Forms, Graphics, GraphicUtils,
     HidroSimulator.Application;

{ TPC }

// Estabelece o PC a Jusante (Target) e tambem estabelece este PC como um PC de Montante de "Target"
procedure TPC.Connect(Target: IObject);
begin
  if Target.getInstance() is TPC then
     begin
     if FNextPC <> nil then FNextPC.FPriorsPCs.Remove(Self);
     FNextPC := TPC( Target.getInstance() );
     FNextPC.FPriorsPCs.Add(Self);
     Designer.Update()
     end
  else
     raise Exception.Create('Um componente PC somente pode ser conectado a um outro PC');
end;

constructor TPC.Create(Component: IComponent);
begin
  inherited Create();
  FComponent := Component;
  FPriorsPCs := TObjectList.Create(false);
end;

procedure TPC.dblClickEvent(Sender: TSimulatorComponent);
begin
  Dialogs.ShowMessage(FName);
end;

destructor TPC.Destroy();
begin
  FPriorsPCs.Free();
  pointer(FComponent) := nil;
  inherited;
end;

procedure TPC.getActions(Actions: TActionList);
begin

end;

function TPC.getDescription(): string;
begin
  result := '';
end;

function TPC.getPriorPC(i: integer): TPC;
begin
  result := TPC( FPriorsPCs[i] );
end;

function TPC.getPriorsPCsCount(): integer;
begin
  result := FPriorsPCs.Count;
end;

procedure TPC.Notify(Action: TNotifyAction);

    procedure clearConnections();
    var i: Integer;
    begin
      FNextPC := nil;
      for i := 0 to FPriorsPCs.Count-1 do
        getPriorPC(i).FNextPC := nil;
    end;

begin
  inherited Notify(Action);
  if Action = naDestruction then
     begin
     TProject(Project.getInstance()).unregisterObject(FName);
     clearConnections();
     end;
end;

procedure TPC.setHierarchy(i: integer);
begin
  FHierarchy := i;
end;

procedure TPC.setProject(Project: IProject);
begin
  inherited setProject(Project);
  FName := TProject( Project.getInstance() ).getNewName('PC ', self);
end;

procedure TPC.toXML(x: TXML_Writer);
begin
  inherited toXML(x);
  x.Write('name', FName);
end;

procedure TPC.fromXML(n: IXMLDOMNode);
var p: TProject;
begin
  inherited fromXML(n);
  p := TProject(Project.getInstance());

  // object identification
  p.unregisterObject(FName);
  FName := n.childNodes.item[0].text;
  p.registerObject(FName, self);
end;

{ TComponent_PC }

function TComponent_PC.CreateComponent(): IComponent;
begin
  result := inherited CreateComponent();
  result.setBounds( Rect(0, 0, 15, 15) );
end;

function TComponent_PC.CreateObject(): IVisualObject;
begin
  result := TPC.Create( self.Component );
end;

{ THidroDesigner }

function THidroDesigner.canCreateComponent(ToolBox: IToolBoxServices; Target: TObject; var X, Y: integer): boolean;
begin
  result := SysUtils.Supports(Target, IDesigner);
end;

constructor THidroDesigner.Create();
var x: TComponent_Project;
    p: TProject;
begin
  inherited Create();

  FDesigner := TDesigner.Create();
  FDesigner.setToolBox(Applic.MainForm);
  FDesigner.setRules(self);
  FDesigner.setPainter(self);
  FDesigner.FormStyle := fsMDIChild;

  // Este componente será destruido automaticamente quando o designer for destruido
  // Todo componente criado apos este apontara automaticamente para IProject
  x := TComponent_Project.Create( FDesigner );
  x.Component.setPos( point(30, 30) );
  p := TProject( x.Obj.getInstance() );
  FDesigner.setProject( p );
end;

destructor THidroDesigner.Destroy();
begin
  FDesigner.Free();
  inherited Destroy();
end;

{TODO 1 -cBOOKMARK: THidroDesigner.SaveToFile}
procedure THidroDesigner.SaveToFile(const Filename: string);
var x: TXML_Writer;

    procedure SaveComponents();
    var i: Integer;
        d: IDesigner;
        c: TSimulatorComponent;
    begin
      x.beginTag('components');
        x.beginIdent();

        FDesigner.GetInterface(IDesigner, d);
        for i := 0 to d.getComponentCount()-1 do
          begin
          c := d.getComponent(i);
          c.ToXML(x);
          end;

        x.endIdent();
      x.endTag('components');
    end;

    procedure SaveRelations();
    var i: Integer;
        d: IDesigner;
        c: TSimulatorComponent;
        p: TPC;
    begin
      x.write();
      x.beginTag('relations');
        x.beginIdent();

        FDesigner.GetInterface(IDesigner, d);
        for i := 0 to d.getComponentCount()-1 do
          begin
          c := d.getComponent(i);
          if c.Obj.getInstance() is TPC then
             begin
             p := TPC( c.Obj.getInstance() );
             if p.NextPC <> nil then
                x.Write('link', ['PC1', 'PC2'], [p.Name, p.NextPC.Name], '');
             end;
          end;

        x.endIdent();
      x.endTag('relations');
    end;

begin
  x := TXML_Writer.Create(TStringList.Create(), 0);
  XML_Utils.WriteXMLHeader(x.Buffer, 'THidroDesigner.SaveToFile()', []);

  x.beginTag('HidroDesigner');
    x.beginIdent();
    SaveComponents();
    SaveRelations();
    x.endIdent();
  x.endTag('HidroDesigner');

  x.Buffer.SaveToFile(Filename);
  x.Free();
end;

procedure THidroDesigner.LoadFromFile(const Filename: string);
var d: IDesigner;

    procedure LoadComponents(no: IXMLDomNode);
    var i: Integer;
        n: IXMLDomNode;
        c: TSimulatorComponent;
    begin
      for i := 1 to no.childNodes.length-1 do
        begin
        n := no.childNodes.item[i];
        c := THidroDesigner.CreateComponent(n.attributes.item[0].text, FDesigner);
        c.fromXML(n);
        end;
    end;

    procedure LoadRelations(no: IXMLDomNode);
    var i: Integer;
        p: TProject;
        n: IXMLDOMNode;
        o1, o2: TPC;
        io2: IObject;
    begin
      p := TProject( d.getProject().getInstance() );
      for i := 0 to no.childNodes.length-1 do
        begin
        n := no.childNodes.item[i];
        o1 := TPC(p.getObject(n.attributes.item[0].text));
        o2 := TPC(p.getObject(n.attributes.item[1].text));
        o2.GetInterface(IObject, io2);
        o1.Connect(io2);
        end;
    end;

var x: IXMLDOMDocument;
    n: IXMLDomNode;
begin
  FDesigner.GetInterface(IDesigner, d);

  x := OpenXMLDocument(Filename);
  n := x.documentElement;

  if (n <> nil) and (n.nodeName = 'HidroDesigner') then
     begin
     LoadComponents ( n.childNodes.item[0] );
     LoadRelations ( n.childNodes.item[1] );

     d.update();
     end;
end;

procedure THidroDesigner.Paint(Designer: IDesigner);
var i: integer;
    canvas: TCanvas;
    p1, p2: TPoint;
    o1: IObject;
    c1, c2: IComponent;
    PC: TPC;
begin
  canvas := Designer.getCanvas();

  // pinta o fundo
  canvas.Brush.Color := TForm(Designer.getControl()).Color;
  canvas.FillRect( Designer.getControl().ClientRect );

  // Desenha as coneccoes dos PCs
  canvas.Pen.Color := clBLACK;
  for i := 0 to Designer.getComponentCount()-1 do
    begin
    c1 := Designer.getComponent(i).Component;
    o1 := c1.getObject();
    if o1.getInstance() is TPC then
       begin
       PC := TPC( o1.getInstance() ).NextPC;
       if PC <> nil then
          begin
          p1 := c1.getPos();
          p2 := PC.Component.getPos();
          GraphicUtils.DesenharSeta(canvas, p1, p2, 10,
            GraphicUtils.DistanciaEntre2Pontos(p1, p2) div 2);
          end;
       end;
    end;

  // pinta os componentes
  Designer.PaintComponents();
end;

class function THidroDesigner.CreateComponent(const className: string; Parent: TWinControl): TSimulatorComponent;
begin
  if className = TComponent_PC.ClassName then
     result := TComponent_PC.Create(Parent);
end;

{ TComponent_Project }

function TComponent_Project.CreateComponent(): IComponent;
begin
  result := inherited CreateComponent();
  result.setColor(clGREEN);
end;

function TComponent_Project.CreateObject(): IVisualObject;
begin
  result := TProject.Create();
end;

{ TProject }

constructor TProject.Create();
begin
  inherited Create();

  FNames := TStringList.Create();
  FNames.Sorted := true;

  FSimulator := TSimulator.Create();
end;

destructor TProject.Destroy();
begin
  FSimulator.Free();
  FNames.Free();
  inherited;
end;

function TProject.getInstance(): TObject;
begin
  result := self;
end;

function TProject.getNewName(const Prefix: string; Obj: TObject): string;
var i: Integer;
begin
  i := 1;
  result := Prefix + '1';
  while FNames.IndexOf(result) > -1 do
    begin
    inc(i);
    result := Prefix + intToStr(i);
    end;
  registerObject(result, Obj);
end;

procedure TProject.registerObject(const Name: string; Obj: TObject);
begin
  FNames.AddObject(Name, Obj);
end;

procedure TProject.unregisterObject(const Name: string);
var i: Integer;
begin
  i := FNames.IndexOf(Name);
  if i > -1 then
     FNames.Delete(i);
end;

function TProject.getService(const Name: string): IService;
begin
  result := nil;
end;

procedure TProject.notify(Action: TNotifyAction; Obj1, Obj2: IObject);
var PC: TPC;
begin
  if (Action in [naCreation, naDestruction]) and (Obj1.getInstance() is TPC) then
     begin
     PC := TPC( Obj1.getInstance() );
     case Action of
       naCreation    : FSimulator.registerPC(PC);
       naDestruction : FSimulator.unregisterPC(PC);
       end;
     end;
end;

procedure TProject.registerService(const Name: string; Service: IService);
begin

end;

function TProject.selectObject(p: TPoint): IObject;
begin
  result := nil;
end;

procedure TProject.unregisterService(const Name: string);
begin

end;

procedure TProject.simulateEvent(Sender: TObject);
begin
  FSimulator.simulate(1);
end;

procedure TProject.getActions(Actions: TActionList);
begin
  inherited;
  WinUtils.CreateAction(Actions, nil, 'Simulate', false, simulateEvent, self);
end;

function TProject.canDestroy(): boolean;
begin
  result := false;
  MessageDlg('Este componente representa o Projeto e não pode ser removido.', mtInformation, [mbOK], 0);
end;

function TProject.getObject(const Name: string): TObject;
var i: Integer;
begin
  i := FNames.IndexOf(Name);
  if i > -1 then
     result := FNames.Objects[i]
  else
     result := nil;
end;

{ TSimulator }

constructor TSimulator.create();
begin
  inherited Create();
  FPCs := TObjectList.Create(false);
end;

destructor TSimulator.destroy();
begin
  FPCs.Free();
  inherited;
end;

function TSimulator.getPC(i: integer): TPC;
begin
  result := TPC( FPCs[i] );
end;

procedure TSimulator.registerPC(PC: TPC);
begin
  FPCs.Add(PC);
end;

procedure TSimulator.simulate(number: integer);

    procedure CalculateHierarchies();
    var i, k: Integer;
        PC: TPC;
    begin
      // Inicia as Hierarquias
      for i := 0 to FPCs.Count-1 do
        getPC(i).setHierarchy(1);

      // Calcula as novas hierarquias
      for i := 0 to FPCs.Count-1 do
        if getPC(i).PriorsPCsCount = 0 then
           begin
           k := 1;
           PC := getPC(i);
           PC.setHierarchy(k);
           while PC.NextPC <> nil do
             begin
             inc(k);
             PC := PC.NextPC;
             if PC.Hierarchy < k then PC.setHierarchy(k);
             end;
           end;
    end;

    // Ordena os PCs pela Hierarquia
    function CompareFuntion(Item1, Item2: Pointer): Integer;
    begin
      Result := TPC(Item1).hierarchy - TPC(Item2).hierarchy;
    end;

var i: Integer;
begin
  CalculateHierarchies();
  FPCs.Sort(@CompareFuntion);
  for i := 0 to FPCs.Count-1 do
    begin
    Dialogs.ShowMessage( getPC(i).FName + ' - H: ' + toString(getPC(i).Hierarchy));
    {TODO 1 -cProgramacao: implementar corretamente TSimulator.simulate}
    end;
end;

procedure TSimulator.unregisterPC(PC: TPC);
begin
  FPCs.Remove(PC);
end;

end.
