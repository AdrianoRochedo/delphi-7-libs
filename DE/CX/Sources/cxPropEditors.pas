
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

unit cxPropEditors;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  Messages, SysUtils, Classes, TypInfo;

type
  TcxNestedEventProperty = class(TMethodProperty)
  protected
    function GetInstance: TPersistent; virtual; abstract;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    procedure GetProperties(Proc: {$IFDEF DELPHI6}TGetPropProc{$ELSE}TGetPropEditProc{$ENDIF}); override;
    function GetValue: string; override;

    property Instance: TPersistent read GetInstance;
  end;

  TcxNestedParentElementEventProperty = class(TNestedProperty)
  private
    FOwner: TPersistent;
    FPropInfo: PPropInfo;
    FParent: TPropertyEditor;

    function GetInstance: TPersistent;
  protected
    constructor Create(Parent: TPropertyEditor; AOwner: TPersistent; APropInfo: PPropInfo); reintroduce;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: {$IFDEF DELPHI6}TGetPropProc{$ELSE}TGetPropEditProc{$ENDIF}); override;
    function GetValue: string; override;
    function GetName: string; override;

    property Instance: TPersistent read GetInstance;
  end;

  TcxNestedElementEventProperty = class(TNestedProperty)
  private
    FParent: TPropertyEditor;
    FPropInfo: PPropInfo;

    function GetTrimmedEventName: string;
    function GetPersistentClassNames: string;
    function GetInstance: TPersistent;
  protected
    constructor Create(Parent: TPropertyEditor; APropInfo: PPropInfo); reintroduce;

    function GetFormMethodName: string; virtual;
    property Instance: TPersistent read GetInstance;
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;

implementation

uses
{$IFDEF DELPHI6}
  DesignConst
{$ELSE}
  Consts
{$ENDIF};

function cxNestedEventProperty_GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
end;

procedure cxNestedEventProperty_GetProperties(APropertyEditor: TPropertyEditor;
  AInstance: TPersistent; Proc: {$IFDEF DELPHI6}TGetPropProc{$ELSE}TGetPropEditProc{$ENDIF});
var
  APropList: TPropList;
  I, ACount : Integer;
  AClassType: TClass;
begin
  if AInstance = nil then Exit;
  ACount := GetPropList(AInstance.ClassInfo, [tkMethod], @APropList);
  for I := 0 to ACount - 1 do
    Proc(TcxNestedElementEventProperty.Create(APropertyEditor, APropList[I]));
  ACount := GetPropList(AInstance.ClassInfo, [tkClass], @APropList);
  for I := 0 to ACount - 1 do
  begin
    AClassType := GetTypeData(APropList[i].PropType^)^.ClassType;
    if not (AClassType.InheritsFrom(TComponent)) and
      (AClassType.InheritsFrom(TPersistent)) and
      (GetPropList(AClassType.ClassInfo, [tkMethod], nil) > 0) then
      Proc(TcxNestedParentElementEventProperty.Create(APropertyEditor, AInstance, APropList[I]));
  end;
end;

function cxNestedEventProperty_GetValue(AInstance: TPersistent): string;
begin
  if AInstance = nil then
    Result := '(None)'
  else
    Result := '(' + AInstance.ClassName + ')';
end;

{ TcxNestedEventProperty }

function TcxNestedEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := cxNestedEventProperty_GetAttributes;
end;

function TcxNestedEventProperty.GetName: string;
begin
  Result := inherited GetName;
  Result := Copy(Result, 1, Pos('Events', Result) - 1);
end;

procedure TcxNestedEventProperty.GetProperties(Proc: {$IFDEF DELPHI6}TGetPropProc{$ELSE}TGetPropEditProc{$ENDIF});
begin
  cxNestedEventProperty_GetProperties(Self, Instance, Proc);
end;

function TcxNestedEventProperty.GetValue: string;
begin
  Result := cxNestedEventProperty_GetValue(Instance);
end;

{ TcxNestedParentElementEventProperty }

constructor TcxNestedParentElementEventProperty.Create(Parent: TPropertyEditor;
  AOwner: TPersistent; APropInfo: PPropInfo);
begin
  inherited Create(Parent);
  FParent := Parent;
  FOwner := AOwner;
  FPropInfo := APropInfo;
end;

function TcxNestedParentElementEventProperty.GetInstance: TPersistent;
begin
  {$IFNDEF DELPHI5}
  Result := TPersistent(GetOrdProp(FOwner, FPropInfo))
  {$ELSE}
  Result := TPersistent(GetObjectProp(FOwner, FPropInfo));
  {$ENDIF}
end;

function TcxNestedParentElementEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := cxNestedEventProperty_GetAttributes;
end;

procedure TcxNestedParentElementEventProperty.GetProperties(Proc: {$IFDEF DELPHI6}TGetPropProc{$ELSE}TGetPropEditProc{$ENDIF});
begin
  cxNestedEventProperty_GetProperties(Self, Instance, Proc);
end;

function TcxNestedParentElementEventProperty.GetValue: string;
begin
  Result := cxNestedEventProperty_GetValue(Instance);
end;

function TcxNestedParentElementEventProperty.GetName: string;
begin
  Result := FPropInfo.Name;
end;

{ TNestedElementEventProperty }
constructor TcxNestedElementEventProperty.Create(Parent: TPropertyEditor; APropInfo: PPropInfo);
begin
  inherited Create(Parent);
  FPropInfo := APropInfo;
  FParent := Parent;
end;

function TcxNestedElementEventProperty.AllEqual: Boolean;
var
  I: Integer;
  V, T: TMethod;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetMethodValue;
    for I := 1 to PropCount - 1 do
    begin
      T := GetMethodValueAt(I);
      if (T.Code <> V.Code) or (T.Data <> V.Data) then Exit;
    end;
  end;
  Result := True;
end;

procedure TcxNestedElementEventProperty.Edit;
var
  FormMethodName: string;
begin
  FormMethodName := GetValue;
  if (FormMethodName = '') or
    Designer.MethodFromAncestor(GetMethodValue) then
  begin
    if FormMethodName = '' then
      FormMethodName := GetFormMethodName;
    if FormMethodName = '' then
    {$IFNDEF DELPHI5}
      raise EPropertyError.Create(SCannotCreateName);
    {$ELSE}
      raise EPropertyError.CreateRes(@SCannotCreateName);
    {$ENDIF}  
    SetValue(FormMethodName);
  end;
  Designer.ShowMethod(FormMethodName);
end;

function TcxNestedElementEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TcxNestedElementEventProperty.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TcxNestedElementEventProperty.GetName: string;
begin
  Result := FPropInfo.Name;
end;

function TcxNestedElementEventProperty.GetValue: string;
begin
  Result := Designer.GetMethodName(GetMethodProp(Instance, FPropInfo));
end;

procedure TcxNestedElementEventProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetMethods(GetTypeData(FPropInfo.PropType^), Proc);
end;

procedure TcxNestedElementEventProperty.SetValue(const AValue: string);

  {$IFDEF DELPHI5}
  procedure CheckChainCall(const MethodName: string; Method: TMethod);
  var
    Persistent: TPersistent;
    Component: TComponent;
    InstanceMethod: string;
    Instance: TComponent;
  begin
    Persistent := GetComponent(0);
    if Persistent is TComponent then
    begin
      Component := TComponent(Persistent);
      if (Component.Name <> '') and (Method.Data <> Designer.GetRoot) and
        (TObject(Method.Data) is TComponent) then
      begin
        Instance := TComponent(Method.Data);
        InstanceMethod := Instance.MethodName(Method.Code);
        if InstanceMethod <> '' then
          Designer.ChainCall(MethodName, Instance.Name, InstanceMethod,
            GetTypeData(GetPropType));
      end;
    end;
  end;
  {$ENDIF}
var
  NewMethod: Boolean;
  CurValue: string;
  OldMethod: TMethod;
begin
  CurValue:= GetValue;
  if (CurValue <> '') and (AValue <> '') and ((CompareText(CurValue, AValue) = 0) or
    not Designer.MethodExists(AValue)) and not Designer.MethodFromAncestor(GetMethodValue) then
    Designer.RenameMethod(CurValue, AValue)
  else
  begin
    NewMethod := (AValue <> '') and not Designer.MethodExists(AValue);
    OldMethod := GetMethodValue;
    SetMethodProp(Instance, FPropInfo, Designer.CreateMethod(AValue, GetTypeData(FPropInfo.PropType^)));
    if NewMethod then
    begin
      {$IFDEF DELPHI5}
      if (PropCount = 1) and (OldMethod.Data <> nil) and (OldMethod.Code <> nil) then
        CheckChainCall(AValue, OldMethod);
      {$ENDIF}  
      Designer.ShowMethod(AValue);
    end;
    Designer.Modified; 
  end;
end;

function TcxNestedElementEventProperty.GetFormMethodName: string;
var
  I: Integer;
begin
  if GetComponent(0) = Designer.GetRoot then
  begin
    {$IFNDEF DELPHI5}
    if Designer.GetRoot <> nil then
      Result := Designer.GetRoot.ClassName
    else Result := '';  
    {$ELSE}
    Result := Designer.GetRootClassName;
    {$ENDIF}
    if (Result <> '') and (Result[1] = 'T') then
      Delete(Result, 1, 1);
  end
  else
  begin
    Result := Designer.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if Result[I] in ['.','[',']','-','>'] then
        Delete(Result, I, 1);
  end;
  if Result = '' then
  {$IFNDEF DELPHI5}
    raise EPropertyError.Create(SCannotCreateName);
  {$ELSE}
    raise EPropertyError.CreateRes(@SCannotCreateName);
  {$ENDIF}  
  Result := Result + GetPersistentClassNames + GetTrimmedEventName;
end;

function TcxNestedElementEventProperty.GetTrimmedEventName: string;
begin
  Result := GetName;
  if (Length(Result) >= 2) and (Result[1] in ['O', 'o']) and
    (Result[2] in ['N', 'n']) then
    Delete(Result, 1, 2);
end;

function TcxNestedElementEventProperty.GetPersistentClassNames: string;
var
  APropertyEditor: TPropertyEditor;
begin
  Result := '';
  APropertyEditor := self;
  while APropertyEditor <> nil do
  begin
    if APropertyEditor is TcxNestedParentElementEventProperty then
      APropertyEditor := TcxNestedParentElementEventProperty(APropertyEditor).FParent
    else
      if APropertyEditor is TcxNestedElementEventProperty then
        APropertyEditor := TcxNestedElementEventProperty(APropertyEditor).FParent
      else APropertyEditor := nil;
    if APropertyEditor <> nil then
      Result := APropertyEditor.GetName + Result;
  end;
end;

function TcxNestedElementEventProperty.GetInstance: TPersistent;
begin
  Result := nil;
  if FParent is TcxNestedEventProperty then
    Result := TcxNestedEventProperty(FParent).Instance;
  if FParent is TcxNestedParentElementEventProperty then
    Result := TcxNestedParentElementEventProperty(FParent).Instance;
end;

end.
