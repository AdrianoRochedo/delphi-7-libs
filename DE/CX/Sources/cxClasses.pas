
{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2003 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxClasses;

{$I cxVer.inc}

interface

uses
{$IFDEF VCL}
  Windows,
{$ELSE}
  Types,
{$ENDIF}
  Classes;

type
  TcxAlignmentVert = (vaTop, vaBottom, vaCenter);
  TcxDirection = (dirNone, dirLeft, dirRight, dirUp, dirDown);
  TcxGetComponent = function(ACaller: TComponent; Index: Integer): TComponent;

  { TcxInterfacedPersistent }

  TcxInterfacedPersistentClass = class of TcxInterfacedPersistent;

  TcxInterfacedPersistent = class({$IFDEF DELPHI6}TInterfacedPersistent{$ELSE}TPersistent, IUnknown{$ENDIF})
  private
    FOwner: TPersistent;
  {$IFNDEF DELPHI6}
    FOwnerInterface: IUnknown;
  {$ENDIF}
  protected
    function GetOwner: TPersistent; override;
  {$IFNDEF DELPHI6}
    { IUnknown }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  {$ENDIF}
  public
    constructor Create(AOwner: TPersistent); virtual;
  {$IFNDEF DELPHI6}
    procedure AfterConstruction; override;
  {$ENDIF}
    property Owner: TPersistent read FOwner;
  end;

  { TcxComponent}

  TcxComponent = class(TComponent {$IFNDEF DELPHI6}, IUnknown{$ENDIF})
  private
    FFakeComponentLink1: TComponent;
    FFakeComponentLink2: TComponent;
    FFakeComponentLink3: TComponent;
    function GetFakeComponentLinkCount: Integer;
    procedure SetFakeComponentLink(Index: Integer; Value: TComponent);
  protected
    procedure GetFakeComponentLinks(AList: TList); virtual;
    procedure Loaded; override;
    procedure UpdateFakeLinks;
  public
    {$IFDEF DELPHI4}
    procedure BeforeDestruction; override;
    {$ENDIF}
  published
    property FakeComponentLink1: TComponent read FFakeComponentLink1 write FFakeComponentLink1 stored False;
    property FakeComponentLink2: TComponent read FFakeComponentLink2 write FFakeComponentLink2 stored False;
    property FakeComponentLink3: TComponent read FFakeComponentLink3 write FFakeComponentLink3 stored False;
  end;

  { TcxObjectList }

  TcxObjectList = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TObject;
  protected
    procedure Add(AItem: TObject); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read GetItem; default;
  end;

  { TcxRegisteredClassList }

  PcxRegisteredClassListItemData = ^TcxRegisteredClassListItemData;
  TcxRegisteredClassListItemData = record
    ItemClass: TClass;
    RegisteredClass: TClass;
  end;

  TcxRegisteredClassList = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): PcxRegisteredClassListItemData;
  protected
    function Find(AItemClass: TClass; var AIndex: Integer): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FindClass(AItemClass: TClass): TClass;
    procedure Register(AItemClass, ARegisteredClass: TClass); virtual;
    procedure Unregister(AItemClass, ARegisteredClass: TClass); virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PcxRegisteredClassListItemData read GetItem; default;
  end;

  { TcxRegisteredClasses } // TODO: Name

  TcxRegisteredClasses = class
  private
    FItems: TStringList;
    FRegisterClasses: Boolean;
    function GetCount: Integer;
    function GetDescription(Index: Integer): string;
    function GetHint(Index: Integer): string;
    function GetItem(Index: Integer): TClass;
  public
    constructor Create(ARegisterClasses: Boolean = False);
    destructor Destroy; override;
    procedure Clear;
    function FindByClassName(const AClassName: string): TClass;
    function FindByDescription(const ADescription: string): TClass;
    function GetDescriptionByClass(AClass: TClass): string;
    function GetHintByClass(AClass: TClass): string;
    function GetIndexByClass(AClass: TClass): Integer;
    procedure Register(AClass: TClass; const ADescription: string);
    procedure Unregister(AClass: TClass);
    property Count: Integer read GetCount;
    property Descriptions[Index: Integer]: string read GetDescription;
    property Hints[Index: Integer]: string read GetHint;
    property Items[Index: Integer]: TClass read GetItem; default;
    property RegisterClasses: Boolean read FRegisterClasses write FRegisterClasses;
  end;
  
  { TcxAutoWidthObject }

  TcxAutoWidthItem = class
  public
    MinWidth: Integer;
    Width: Integer;
    Fixed: Boolean;
    AutoWidth: Integer;
    constructor Create;
  end;

  TcxAutoWidthObject = class
  private
    FAvailableWidth: Integer;
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxAutoWidthItem;
    function GetWidth: Integer;
  protected
    procedure Clear;
  public
    constructor Create(ACount: Integer);
    destructor Destroy; override;
    function AddItem: TcxAutoWidthItem;
    procedure Calculate;
    property AvailableWidth: Integer read FAvailableWidth write FAvailableWidth;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxAutoWidthItem read GetItem; default;
    property Width: Integer read GetWidth;
  end;

function GetValidName(AComponent: TComponent; const AName: string;
  AIsBaseName: Boolean = False): string;
{$IFNDEF VCL}
function MulDiv(Number, Numerator, Denominator: Integer): Integer;
procedure SetRectEmpty(var R: TRect);
{$ENDIF}
procedure RenameComponents(ACaller, AOwner: TComponent;
  ANewName, AOldName: TComponentName;
  AComponentCount: Integer; AGetComponent: TcxGetComponent);
function RoundDiv(I1, I2: Integer): Integer;

{$IFNDEF DELPHI5}
procedure FreeAndNil(var Obj);
{$ENDIF}

{$IFNDEF DELPHI6}
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
{$ENDIF}

function cxGetResourceString(const ResString: Pointer): string;
procedure cxSetResourceString(const ResString: Pointer; const Value: string);

implementation

uses
  SysUtils, Graphics;

function GetShortHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, 1, I - 1);
end;

function GetLongHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, I + 1, Maxint);
end;

{$IFNDEF DELPHI5}

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function Supports(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean; overload;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(Intf, Inst) = 0);
end;

function Supports(Instance: TObject; const Intf: TGUID; out Inst): Boolean; overload;
var
  Unk: IUnknown;
begin
  Result := (Instance <> nil) and Instance.GetInterface(IUnknown, Unk) and
    Supports(Unk, Intf, Inst);
end;

{$ENDIF}

{$IFNDEF DELPHI6}

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IUnknown;
begin
  Result := Supports(Instance, IID, Temp);
end;

{$ENDIF}

{ TcxInterfacedPersistent }

constructor TcxInterfacedPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxInterfacedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{$IFNDEF DELPHI6}

function TcxInterfacedPersistent._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := -1;
end;

function TcxInterfacedPersistent._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
    Result := -1;
end;

function TcxInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TcxInterfacedPersistent.AfterConstruction;
begin
  inherited;
  if GetOwner <> nil then
    GetOwner.GetInterface(IUnknown, FOwnerInterface);
end;

{$ENDIF}

{ TcxComponent }

procedure TcxComponent.Loaded;
begin
  inherited Loaded;
  UpdateFakeLinks;
end;

{$IFDEF DELPHI4}

procedure TcxComponent.BeforeDestruction;
begin
  if not (csDestroying in ComponentState) then
    Destroying;
end;

{$ENDIF}

procedure TcxComponent.GetFakeComponentLinks(AList: TList);
begin
end;

procedure TcxComponent.UpdateFakeLinks;
var
  I: Integer;
  AList: TList;
begin
  if not (csDesigning in ComponentState) or
    (csDestroying in ComponentState) or (Owner = nil) then exit;
  AList := TList.Create;
  try
    GetFakeComponentLinks(AList);
    for I := 0 to GetFakeComponentLinkCount - 1 do
      if I < AList.Count then
        SetFakeComponentLink(I, TComponent(AList[I]))
      else SetFakeComponentLink(I, nil);
  finally
    AList.Free;
  end;
end;

function TcxComponent.GetFakeComponentLinkCount: Integer;
begin
  Result := 3;
end;

procedure TcxComponent.SetFakeComponentLink(Index: Integer; Value: TComponent);
begin
  case Index of
    0: FFakeComponentLink1 := Value;
    1: FFakeComponentLink2 := Value;
    2: FFakeComponentLink3 := Value;
  end;
end;

{ TcxObjectList }

constructor TcxObjectList.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TcxObjectList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TcxObjectList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxObjectList.GetItem(Index: Integer): TObject;
begin
  Result := FItems[Index];
end;

procedure TcxObjectList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Clear;
end;

procedure TcxObjectList.Add(AItem: TObject);
begin
  FItems.Add(AItem);
end;

{ TcxRegisteredClassList }

constructor TcxRegisteredClassList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxRegisteredClassList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxRegisteredClassList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PcxRegisteredClassListItemData(FItems[I]));
  FItems.Clear;
end;

function TcxRegisteredClassList.FindClass(AItemClass: TClass): TClass;
var
  AIndex: Integer;
begin
  if Find(AItemClass, AIndex) then
    Result := Items[AIndex]^.RegisteredClass
  else
    Result := nil;
end;

procedure TcxRegisteredClassList.Register(AItemClass, ARegisteredClass: TClass);
var
  AIndex: Integer;
  PData: PcxRegisteredClassListItemData;
begin
  AIndex := -1;
  New(PData);
  PData^.ItemClass := AItemClass;
  PData^.RegisteredClass := ARegisteredClass;
  if Find(AItemClass, AIndex) then
    FItems.Insert(AIndex + 1, PData)
  else
    if AIndex <> -1 then
      FItems.Insert(AIndex, PData)
    else
      FItems.Add(PData);
end;

procedure TcxRegisteredClassList.Unregister(AItemClass, ARegisteredClass: TClass);
var
  I: Integer;
  PData: PcxRegisteredClassListItemData;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    PData := Items[I];
    if (PData^.ItemClass = AItemClass) and (PData^.RegisteredClass = ARegisteredClass) then
    begin
      Dispose(PData);
      FItems.Delete(I);
    end;
  end;
end;

function TcxRegisteredClassList.Find(AItemClass: TClass; var AIndex: Integer): Boolean;
var
  I: Integer;
  PData: PcxRegisteredClassListItemData;       
begin
  Result := False;
  for I := FItems.Count - 1 downto 0 do
  begin
    PData := Items[I];
    if AItemClass.InheritsFrom(PData.ItemClass) then
    begin
      AIndex := I;
      Result := True;
      Break;
    end
    else
      if PData.ItemClass.InheritsFrom(AItemClass) then
        AIndex := I;
  end;
end;

function TcxRegisteredClassList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxRegisteredClassList.GetItem(Index: Integer): PcxRegisteredClassListItemData;
begin
  Result := FItems[Index];
end;

{ TcxRegisteredClasses }

constructor TcxRegisteredClasses.Create(ARegisterClasses: Boolean = False);
begin
  inherited Create;
  FRegisterClasses := ARegisterClasses;
  FItems := TStringList.Create;
end;

destructor TcxRegisteredClasses.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TcxRegisteredClasses.Clear;
begin
  FItems.Clear;
end;

function TcxRegisteredClasses.FindByClassName(const AClassName: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ClassName = AClassName then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TcxRegisteredClasses.FindByDescription(const ADescription: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Descriptions[I] = ADescription then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TcxRegisteredClasses.GetDescriptionByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Descriptions[AIndex];
end;

function TcxRegisteredClasses.GetHintByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Hints[AIndex];
end;

function TcxRegisteredClasses.GetIndexByClass(AClass: TClass): Integer;
begin
  Result := FItems.IndexOfObject(TObject(AClass));
end;

procedure TcxRegisteredClasses.Register(AClass: TClass; const ADescription: string);
begin
  if GetIndexByClass(AClass) = -1 then
  begin
    FItems.AddObject(ADescription, TObject(AClass));
    if FRegisterClasses then RegisterClass(TPersistentClass(AClass));
  end;
end;

procedure TcxRegisteredClasses.Unregister(AClass: TClass);
var
  I: Integer;
begin
  I := GetIndexByClass(AClass);
  if I <> -1 then
    FItems.Delete(I);
end;

function TcxRegisteredClasses.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxRegisteredClasses.GetDescription(Index: Integer): string;
begin
  Result := GetShortHint(FItems[Index]);
end;

function TcxRegisteredClasses.GetHint(Index: Integer): string;
begin
  Result := GetLongHint(FItems[Index]);
end;

function TcxRegisteredClasses.GetItem(Index: Integer): TClass;
begin
  Result := TClass(FItems.Objects[Index]);
end;

{ TcxAutoWidthItem }

constructor TcxAutoWidthItem.Create;
begin
  inherited;
  AutoWidth := -1;
end;

{ TcxAutoWidthObject }

constructor TcxAutoWidthObject.Create(ACount: Integer);
begin
  inherited Create;
  FItems := TList.Create;
  FItems.Capacity := ACount;
end;

destructor TcxAutoWidthObject.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TcxAutoWidthObject.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxAutoWidthObject.GetItem(Index: Integer): TcxAutoWidthItem;
begin
  Result := FItems[Index];
end;

function TcxAutoWidthObject.GetWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].Width);
end;

procedure TcxAutoWidthObject.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do Items[I].Free;
end;

function TcxAutoWidthObject.AddItem: TcxAutoWidthItem;
begin
  Result := TcxAutoWidthItem.Create;
  FItems.Add(Result);
end;

procedure TcxAutoWidthObject.Calculate;
var
  AAvailableWidth, AWidth, ANewAvailableWidth, ANewWidth, AOffset, I,
    AItemAutoWidth: Integer;
  AAssignAllWidths, AItemWithMinWidthFound: Boolean;

  procedure RemoveItemFromCalculation(AItem: TcxAutoWidthItem);
  begin
    with AItem do
    begin
      Dec(ANewAvailableWidth, AutoWidth);
      Dec(ANewWidth, Width);
    end;
  end;

  procedure ProcessFixedItems;
  var
    I: Integer;

    procedure ProcessItem(AItem: TcxAutoWidthItem);
    begin
      with AItem do
        if Fixed then
        begin
          AutoWidth := Width;
          RemoveItemFromCalculation(AItem);
        end;
    end;

  begin
    for I := 0 to Count - 1 do ProcessItem(Items[I]);
  end;

  {procedure ProcessFixedColumns;
  var
    AFixedIndex, I: Integer;
  begin
    if not (gcsColumnSizing in GridDefinition.Controller.State) then Exit;
    AFixedIndex :=
      (GridDefinition.Controller.DragAndDropObject as TcxGridColumnHeaderSizingObject).Column.VisibleIndex;
    if AFixedIndex = Count - 1 then Exit;
    for I := 0 to Count - 1 do
      if I <= AFixedIndex then
      begin
        AColumnWidth := Items[I].CalculateWidth;
        Items[I].Width := AColumnWidth;
        Dec(AAvailableWidth, AColumnWidth);
        Dec(AWidth, AColumnWidth);
      end;
  end;}

  procedure ProcessItem(AItem: TcxAutoWidthItem);

    function CalculateItemAutoWidth: Integer;
    begin
      Result :=
        MulDiv(AOffset + AItem.Width, AAvailableWidth, AWidth) -
        MulDiv(AOffset, AAvailableWidth, AWidth);
    end;

  begin
    AItemAutoWidth := CalculateItemAutoWidth;
    if AAssignAllWidths then
      AItem.AutoWidth := AItemAutoWidth
    else
      if AItemAutoWidth <= AItem.MinWidth then
      begin
        AItem.AutoWidth := AItem.MinWidth;
        RemoveItemFromCalculation(AItem);
        AItemWithMinWidthFound := True;
      end;
    Inc(AOffset, AItem.Width);
  end;

begin
  AAvailableWidth := FAvailableWidth;
  AWidth := Width;

  ANewAvailableWidth := AAvailableWidth;
  ANewWidth := AWidth;
  ProcessFixedItems;
  AAssignAllWidths := False;
  repeat
    AAvailableWidth := ANewAvailableWidth;
    AWidth := ANewWidth;
    AOffset := 0;
    AItemWithMinWidthFound := False;

    for I := 0 to Count - 1 do
      if Items[I].AutoWidth = -1 then ProcessItem(Items[I]);

    if not AItemWithMinWidthFound then
      AAssignAllWidths := not AAssignAllWidths;
  until (ANewWidth = 0) or not AItemWithMinWidthFound and not AAssignAllWidths;
end;

{ functions }

function GetValidName(AComponent: TComponent; const AName: string;
  AIsBaseName: Boolean = False): string;
var
  AOwner: TComponent;
  I: Integer;

  function GetNextName: string;
  begin
    Result := AName + IntToStr(I);
    Inc(I);
  end;

begin
  Result := AName;
  AOwner := AComponent.Owner;
  if AOwner = nil then Exit;
  I := 1;
  if AIsBaseName then Result := GetNextName;
  while AOwner.FindComponent(Result) <> nil do
    Result := GetNextName;
end;

{$IFNDEF VCL}

function MulDiv(Number, Numerator, Denominator: Integer): Integer;
begin
  Result := Int64(Number) * Numerator div Denominator;
  if Int64(Number) * Numerator mod Denominator >= Denominator div 2 then
    Inc(Result);
end;

procedure SetRectEmpty(var R: TRect);
begin
  R := Rect(0, 0, 0, 0);
end;

{$ENDIF}

procedure RenameComponents(ACaller, AOwner: TComponent;
  ANewName, AOldName: TComponentName;
  AComponentCount: Integer; AGetComponent: TcxGetComponent);
var
  I: Integer;
  AComponent: TComponent;
  AComponentName, ANamePrefix: TComponentName;
begin
  for I := 0 to AComponentCount - 1 do
  begin
    AComponent := AGetComponent(ACaller, I);
    if AComponent.Owner = AOwner then
    begin
      AComponentName := AComponent.Name;
      if Length(AComponentName) > Length(AOldName) then
      begin
        ANamePrefix := Copy(AComponentName, 1, Length(AOldName));
        if CompareText(AOldName, ANamePrefix) = 0 then
        begin
          Delete(AComponentName, 1, Length(AOldName));
          Insert(ANewName, AComponentName, 1);
          try
            AComponent.Name := AComponentName;
          except
            on EComponentError do { Ignore rename errors };
          end;
        end;
      end;
    end;
  end;
end;

function RoundDiv(I1, I2: Integer): Integer;
begin
  Result := I1 div I2 + Ord(I1 mod I2 <> 0);
end;

var
  FResStrings: TStringList = nil;

function cxGetResourceString(const ResString: Pointer): string;
var
  AIndex: Integer;
begin
  if (FResStrings <> nil) then
  begin
    AIndex := FResStrings.IndexOfObject(TObject(ResString));
    if AIndex > -1 then
      Result := FResStrings[AIndex]
    else Result := LoadResString(PResStringRec(ResString));
  end
  else Result := LoadResString(PResStringRec(ResString));
end;

procedure cxSetResourceString(const ResString: Pointer; const Value: string);
var
  AIndex: Integer;
begin
  if (FResStrings = nil) then
    FResStrings := TStringList.Create;
  AIndex := FResStrings.IndexOfObject(TObject(ResString));
  if AIndex > -1 then
    FResStrings[AIndex] := Value
  else FResStrings.AddObject(Value, TObject(ResString));
end;

initialization

finalization
  if FResStrings <> nil then
  begin
    FResStrings.Free;
    FResStrings := nil;
  end;

end.
