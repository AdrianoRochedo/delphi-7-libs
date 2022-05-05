{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : compatibility declarations for Delphi 4, 5

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDCompat;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Forms, ActiveX, Menus,
  TypInfo,  {$IFDEF RA_D6H} DesignIntf {$ELSE} DsgnIntf {$ENDIF RA_D6H};

{******************* compatibility section }
{$IFNDEF RA_D4H}
type
  longword = integer;
{$ENDIF}

{$IFDEF RA_D4H}
type

  TIStream = IStream;

  IInstance = interface
    ['{FAE46340-F841-11D2-8AB2-EAE555E1DD52}']
    function GetInstance: TObject;
  end;

  TDesigner = class(TInterfacedObject, IDesigner, IInstance)
  private
    FCustomForm: TCustomForm;
  public
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  public
    procedure Modified; virtual; abstract;
    procedure Notification(AnObject: TPersistent; Operation: TOperation); virtual; abstract;
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean; virtual; abstract;
    procedure PaintGrid; virtual; abstract;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); virtual; abstract;
    function GetInstance: TObject;
   {$IFDEF RA_D5H}
    function UniqueName(const BaseName: string): string; virtual; abstract;
    function GetRoot: TComponent; virtual; abstract;
   {$ENDIF RA_D5H}
  end;

  TFormDesigner = class(TDesigner, IFormDesigner)
  public
   {$IFNDEF RA_D5H}
    function UniqueName(const BaseName: string): string; virtual; abstract;
    function GetRoot: TComponent; virtual; abstract;
   {$ENDIF RA_D5H}
    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod; virtual; abstract;
    function GetMethodName(const Method: TMethod): string; virtual; abstract;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc); virtual; abstract;
    function GetPrivateDirectory: string; virtual; abstract;
    procedure GetSelections(const List: IDesignerSelections); virtual; abstract;
    function MethodExists(const Name: string): Boolean; virtual; abstract;
    procedure RenameMethod(const CurName, NewName: string); virtual; abstract;
    procedure SelectComponent(Instance: TPersistent); virtual; abstract;
    procedure SetSelections(const List: IDesignerSelections); virtual; abstract;
    procedure ShowMethod(const Name: string); virtual; abstract;
    procedure GetComponentNames(TypeData: PTypeData; Proc: TGetStrProc); virtual; abstract;
    function GetComponent(const Name: string): TComponent; virtual; abstract;
    function GetComponentName(Component: TComponent): string; virtual; abstract;
    function GetObject(const Name: string): TPersistent; virtual; abstract;
    function GetObjectName(Instance: TPersistent): string; virtual; abstract;
    procedure GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc); virtual; abstract;
    function MethodFromAncestor(const Method: TMethod): Boolean; virtual; abstract;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent; virtual; abstract;
    function IsComponentLinkable(Component: TComponent): Boolean; virtual; abstract;
    procedure MakeComponentLinkable(Component: TComponent); virtual; abstract;
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo); virtual; abstract;
    function GetIsDormant: Boolean; virtual; abstract;
    function HasInterface: Boolean; virtual; abstract;
    function HasInterfaceMember(const Name: string): Boolean; virtual; abstract;
    procedure AddToInterface(InvKind: Integer; const Name: string; VT: Word;
      const TypeInfo: string); virtual; abstract;
    procedure GetProjectModules(Proc: TGetModuleProc); virtual; abstract;
    function GetAncestorDesigner: IFormDesigner; virtual; abstract;
    function IsSourceReadOnly: Boolean; virtual; abstract;
   {$IFDEF RA_D5H}
    function GetContainerWindow: TWinControl; virtual; abstract;
    procedure SetContainerWindow(const NewContainer: TWinControl); virtual; abstract;
    function GetScrollRanges(const ScrollPosition: TPoint): TPoint; virtual; abstract;
    procedure Edit(const Component: IComponent); virtual; abstract;
    function BuildLocalMenu(Base: TPopupMenu; Filter: TLocalMenuFilters): TPopupMenu; virtual; abstract;
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      TypeData: PTypeData); virtual; abstract;
    procedure CopySelection; virtual; abstract;
    procedure CutSelection; virtual; abstract;
    function CanPaste: Boolean; virtual; abstract;
    procedure PasteSelection; virtual; abstract;
    procedure DeleteSelection; virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure NoSelection; virtual; abstract;
    procedure ModuleFileNames(var ImplFileName, IntfFileName, FormFileName: string); virtual; abstract;
    function GetRootClassName: string; virtual; abstract;
   {$ENDIF RA_D5H}
  end;

  TRAFDPersistent = class(TInterfacedObject, IPersistent, IImplementation)
  private
    FPersistent: TPersistent;
  public { IPersistent }
    procedure DestroyObject;
    function Equals(const Other: IPersistent): Boolean;
    function GetClassname: string;
    function GetEventInfos: IEventInfos;
    function GetNamePath: string;
    function GetOwner: IPersistent;
    function InheritsFrom(const Classname: string): Boolean;
    function IsComponent: Boolean;  // object is stream createable
    function IsControl: Boolean;
    function IsWinControl: Boolean;
  public { IImplementation }
    function GetInstance: TObject;
  public
    constructor Create(Instance: TPersistent);
    destructor Destroy; override;
  end;

  TRAFDComponent = class(TRAFDPersistent, IComponent)
  private
    function FComponent: TComponent;
  public
    function FindComponent(const Name: string): IComponent;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponent;
    function GetComponentState: TComponentState;
    function GetComponentStyle: TComponentStyle;
    function GetDesignInfo: TSmallPoint;
    function GetDesignOffset: TPoint;
    function GetDesignSize: TPoint;
    function GetName: string;
    function IComponent_GetOwner: IComponent;
    function GetParent: IComponent;
    procedure SetDesignInfo(const Point: TSmallPoint);
    procedure SetDesignOffset(const Point: TPoint);
    procedure SetDesignSize(const Point: TPoint);
    procedure SetName(const Value: string);
    function IComponent.GetOwner = IComponent_GetOwner;
  end;

  procedure NotImplemented(const S: string);
  function ExtractFormDesigner(const Designer: IDesigner): TFormDesigner;

{$ENDIF RA_D4H}

 {$IFDEF RA_D5}
type 
  TComponentList = TDesignerSelectionList;
 {$ENDIF RA_D5}

{#################### compatibility section }

implementation

{$IFDEF RA_D4H}

uses RAUtils, iMTracer;

type
  THackControl = class(TControl);
  THackPersistent = class(TPersistent);

procedure NotImplemented(const S: string);
begin
  raise Exception.Create(S + ' not implemented. Sorry.');
end;


function TDesigner.GetInstance: TObject;
begin
  Result := Self;
end;

function TDesigner.GetIsControl: Boolean;
begin
  Result := (FCustomForm <> nil) and THackControl(FCustomForm).IsControl;
end;

procedure TDesigner.SetIsControl(Value: Boolean);
begin
  if (FCustomForm <> nil) then THackControl(FCustomForm).IsControl := Value;
end;

function TDesigner.GetCustomForm: TCustomForm;
begin
  Result := FCustomForm;
end;

procedure TDesigner.SetCustomForm(Value: TCustomForm);
begin
  FCustomForm := Value;
end;



constructor TRAFDPersistent.Create(Instance: TPersistent);
begin
  FPersistent := Instance;
end;

destructor TRAFDPersistent.Destroy;
begin
  inherited Destroy;
end;

procedure TRAFDPersistent.DestroyObject;
begin
  ODS('TRAFDPersistent.DestroyObject');
  FPersistent.Free;
  FPersistent := nil;
end;

function TRAFDPersistent.Equals(const Other: IPersistent): Boolean;
begin
  ODS('TRAFDPersistent.Equals');
  Result := ExtractPersistent(Other) = GetInstance;
end;

function TRAFDPersistent.GetClassname: string;
begin
  ODS('TRAFDPersistent.GetClassname');
  Result := FPersistent.Classname;
end;

function TRAFDPersistent.GetEventInfos: IEventInfos;
begin
 { not implemented }
  ODS('TRAFDPersistent.GetEventInfos');
  Result := nil;
end;

function TRAFDPersistent.GetNamePath: string;
begin
  ODS('TRAFDPersistent.GetNamePath');
  Result := FPersistent.GetNamePath;
end;

function TRAFDPersistent.GetOwner: IPersistent;
begin
  ODS('TRAFDPersistent.GetOwner');
  Result := MakeIPersistent(THackPersistent(FPersistent).GetOwner);
end;

function TRAFDPersistent.InheritsFrom(const Classname: string): Boolean;
begin
  ODS('TRAFDPersistent.InheritsFrom');
  Result := FPersistent.InheritsFrom(GetClass(Classname));
end;

function TRAFDPersistent.IsComponent: Boolean;
begin
  ODS('TRAFDPersistent.IsComponent');
  Result := FPersistent is TComponent;
end;

function TRAFDPersistent.IsControl: Boolean;
begin
  ODS('TRAFDPersistent.IsControl');
  Result := FPersistent is TControl;
end;

function TRAFDPersistent.IsWinControl: Boolean;
begin
  ODS('TRAFDPersistent.IsWinControl');
  Result := FPersistent is TWinControl;
end;

function TRAFDPersistent.GetInstance: TObject;
begin
  ODS('TRAFDPersistent.GetInstance');
  Result := FPersistent;
end;


function TRAFDComponent.FComponent: TComponent;
begin
  Result := FPersistent as TComponent;
end;

function TRAFDComponent.FindComponent(const Name: string): IComponent;
begin
  ODS('TRAFDComponent.FindComponent');
  Result := MakeIComponent(FComponent.FindComponent(Name));
end;

function TRAFDComponent.GetComponentCount: Integer;
begin
  ODS('TRAFDComponent.GetComponentCount');
  Result := FComponent.ComponentCount;
end;

function TRAFDComponent.GetComponents(Index: Integer): IComponent;
begin
  ODS('TRAFDComponent.GetComponents');
  Result := MakeIComponent(FComponent.Components[Index]);
end;

function TRAFDComponent.GetComponentState: TComponentState;
begin
  ODS('TRAFDComponent.GetComponentState');
  Result := FComponent.ComponentState;
end;

function TRAFDComponent.GetComponentStyle: TComponentStyle;
begin
  ODS('TRAFDComponent.GetComponentStyle');
  Result := FComponent.ComponentStyle;
end;

function TRAFDComponent.GetDesignInfo: TSmallPoint;
begin
  ODS('TRAFDComponent.GetDesignInfo');
  Result := TSmallPoint(FComponent.DesignInfo);
end;

function TRAFDComponent.GetDesignOffset: TPoint;
begin
  ODS('TRAFDComponent.GetDesignOffset');
  Result := SmallPointToPoint(GetDesignInfo);
end;

function TRAFDComponent.GetDesignSize: TPoint;
begin
  ODS('TRAFDComponent.GetDesignSize');
  if FComponent is TControl then
    with (FComponent as TControl) do
      Result := Point(Width, Height)
  else
    Result := Point(32, 32);
end;

function TRAFDComponent.GetName: string;
begin
  ODS('TRAFDComponent.GetName');
  Result := FComponent.Name;
end;

function TRAFDComponent.IComponent_GetOwner: IComponent;
begin
  ODS('TRAFDComponent.GetOwner');
  Result := MakeIComponent(FComponent.Owner);
end;

function TRAFDComponent.GetParent: IComponent;
begin
  ODS('TRAFDComponent.GetParent');
  Result := MakeIComponent(FComponent.GetParentComponent);
end;

procedure TRAFDComponent.SetDesignInfo(const Point: TSmallPoint);
begin
  ODS('TRAFDComponent.SetDesignInfo');
  FComponent.DesignInfo := Integer(Point);
end;

procedure TRAFDComponent.SetDesignOffset(const Point: TPoint);
begin
  ODS('TRAFDComponent.SetDesignOffset');
  SetDesignInfo(PointToSmallPoint(Point));
end;

procedure TRAFDComponent.SetDesignSize(const Point: TPoint);
begin
  ODS('TRAFDComponent.SetDesignSize');
  if FComponent is TControl then
    with (FComponent as TControl) do
      SetBounds(Left, Top, Point.X, Point.Y);
end;

procedure TRAFDComponent.SetName(const Value: string);
begin
  ODS('TRAFDComponent.SetName');
  FComponent.Name := Value;
end;



function RAFDMakeIPersistent(Instance: TPersistent): IPersistent;
begin
  ODS('RAFDMakeIPersistent');
  if Instance is TComponent then
  begin
    ODS(' TComponent !');
    Result := TRAFDComponent.Create(Instance)
  end
  else
    Result := TRAFDPersistent.Create(Instance);
end;

function RAFDMakeIComponent(Instance: TComponent): IComponent;
begin
  ODS('RAFDMakeIComponent');
  Result := TRAFDComponent.Create(Instance);
end;

function ExtractFormDesigner(const Designer: IDesigner): TFormDesigner;
var
  D: IInstance;
begin
  if Designer.QueryInterface(IInstance, D) = S_OK then
    Result := D.GetInstance as TFormDesigner
  else
    raise Exception.Create('Internal error - Can''t extract RAFormDesigner');
end;

{$ENDIF RA_D4H}


{$IFDEF RA_D4H}
initialization
  MakeIPersistentProc := RAFDMakeIPersistent;
  MakeIComponentProc := RAFDMakeIComponent;
{$ENDIF RA_D4H}
end.

