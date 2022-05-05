
{********************************************************************}
{                                                                    }
{       Developer Express Visual Component Library                   }
{       ExpressCommonLibrary                                         }
{                                                                    }
{       Copyright (c) 1998-2003 Developer Express Inc.               }
{       ALL RIGHTS RESERVED                                          }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxLookAndFeels;

{$I cxVer.inc}

interface

uses
{$IFDEF VCL}
  dxThemeConsts, dxThemeManager,
{$ENDIF}
  SysUtils, Classes, Forms, cxClasses, cxLookAndFeelPainters;

type
  TcxLookAndFeelKind = (lfFlat, lfStandard, lfUltraFlat);
  TcxLookAndFeelValue = (lfvKind, lfvNativeStyle);
  TcxLookAndFeelValues = set of TcxLookAndFeelValue;

const
  cxDefaultLookAndFeelKind = lfUltraFlat;
  cxDefaultLookAndFeelNativeStyle = False;

type
  TcxLookAndFeel = class;

  { TcxLookAndFeel }

  IcxLookAndFeelNotificationListener = interface
  ['{205538BF-F19E-4285-B11F-B182D9635881}']
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
  end;

  TcxLookAndFeelChangedEvent = procedure(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues) of object;

  TcxLookAndFeelData = record
    Kind: TcxLookAndFeelKind;
    NativeStyle: Boolean;
  end;

  TcxLookAndFeel = class(TcxInterfacedPersistent, IcxLookAndFeelNotificationListener)
  private
    FAssignedValues: TcxLookAndFeelValues;
    FChangeListenerList: TList;
    FCurrentState: TcxLookAndFeelData;
    FData: TcxLookAndFeelData;
    FIsDestruction: Boolean;
    FIsRootLookAndFeel: Boolean;
    FMasterLookAndFeel: TcxLookAndFeel;
    FPrevState: TcxLookAndFeelData;
    FOnChanged: TcxLookAndFeelChangedEvent;
    function GetKind: TcxLookAndFeelKind;
    function GetMasterLookAndFeel: TcxLookAndFeel;
    function GetNativeStyle: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainterClass;
    procedure SetAssignedValues(Value: TcxLookAndFeelValues);
    procedure SetKind(Value: TcxLookAndFeelKind);
    procedure SetMasterLookAndFeel(Value: TcxLookAndFeel);
    procedure SetNativeStyle(Value: Boolean);

    procedure Changed(AChangedValues: TcxLookAndFeelValues);
    procedure CheckStateChanges;
    function GetDefaultKind: TcxLookAndFeelKind;
    function GetDefaultNativeStyle: Boolean;
    function IsKindStored: Boolean;
    function IsNativeStyleStored: Boolean;
    procedure SaveState;

    { IcxLookAndFeelNotificationListener }
    function GetObject: TObject;
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
  protected
    function InternalGetKind: TcxLookAndFeelKind; virtual;
    function InternalGetNativeStyle: Boolean; virtual;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddChangeListener(AListener: IcxLookAndFeelNotificationListener);
    function GetAvailablePainter{$IFDEF VCL}(ANeededThemedObjectTypes:
      TdxThemedObjectTypes = []){$ENDIF}: TcxCustomLookAndFeelPainterClass;
    procedure RemoveChangeListener(AListener: IcxLookAndFeelNotificationListener);
    procedure Reset;
    property MasterLookAndFeel: TcxLookAndFeel read GetMasterLookAndFeel write SetMasterLookAndFeel;
    property Painter: TcxCustomLookAndFeelPainterClass read GetPainter;
    property OnChanged: TcxLookAndFeelChangedEvent read FOnChanged write FOnChanged;
  published
    property AssignedValues: TcxLookAndFeelValues read FAssignedValues write SetAssignedValues stored False;
    property Kind: TcxLookAndFeelKind read GetKind write SetKind stored IsKindStored;
    property NativeStyle: Boolean read GetNativeStyle write SetNativeStyle stored IsNativeStyleStored;
  end;

  { TcxLookAndFeelController }

  TcxLookAndFeelController = class(TComponent, IcxLookAndFeelNotificationListener)
  private
    function GetKind: TcxLookAndFeelKind;
    function GetNativeStyle: Boolean;
    procedure SetKind(Value: TcxLookAndFeelKind);
    procedure SetNativeStyle(Value: Boolean);
    procedure Modified;
    { IcxLookAndFeelNotificationListener }
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Kind: TcxLookAndFeelKind read GetKind write SetKind default cxDefaultLookAndFeelKind;
    property NativeStyle: Boolean read GetNativeStyle write SetNativeStyle default cxDefaultLookAndFeelNativeStyle;
  end;

function RootLookAndFeel: TcxLookAndFeel;

implementation

const
  cxLookAndFeelValueAll = [lfvKind, lfvNativeStyle];

var
  FLookAndFeelControllerCount: Integer;
  FRootLookAndFeel: TcxLookAndFeel;

function RootLookAndFeel: TcxLookAndFeel;
begin
  Result := FRootLookAndFeel;
end;

{ TcxLookAndFeel }

constructor TcxLookAndFeel.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FChangeListenerList := TList.Create;
  with FData do
  begin
    Kind := cxDefaultLookAndFeelKind;
    NativeStyle := cxDefaultLookAndFeelNativeStyle;
  end;
  FCurrentState := FData;
end;

destructor TcxLookAndFeel.Destroy;
var
  I: Integer;
begin
  FIsDestruction := True;
  for I := 0 to FChangeListenerList.Count - 1 do
    IcxLookAndFeelNotificationListener(FChangeListenerList.Items[I]).MasterLookAndFeelDestroying(Self);
  FIsDestruction := False;
  FreeAndNil(FChangeListenerList);

  if MasterLookAndFeel <> nil then
    MasterLookAndFeel.RemoveChangeListener(Self);

  if FIsRootLookAndFeel then
  begin
    FRootLookAndFeel := nil;
    FIsRootLookAndFeel := False;
  end;
  inherited Destroy;
end;

procedure TcxLookAndFeel.AfterConstruction;
begin
  if RootLookAndFeel <> nil then
  begin
    RootLookAndFeel.AddChangeListener(Self);
    with FCurrentState do
    begin
      Kind := InternalGetKind;
      NativeStyle := InternalGetNativeStyle;
    end;
  end;
end;

procedure TcxLookAndFeel.AddChangeListener(AListener: IcxLookAndFeelNotificationListener);
var
  AIsLookAndFeelController: Boolean;
begin
  if AListener = nil then
    Exit;
  AIsLookAndFeelController := AListener.GetObject is TcxLookAndFeelController;
  if not FIsRootLookAndFeel and AIsLookAndFeelController then
    Exit;
  if FChangeListenerList.IndexOf(Pointer(AListener)) >= 0 then
    Exit;

  if FIsRootLookAndFeel and AIsLookAndFeelController then
    Inc(FLookAndFeelControllerCount);
  FChangeListenerList.Add(Pointer(AListener));
end;

function TcxLookAndFeel.GetAvailablePainter{$IFDEF VCL}(ANeededThemedObjectTypes:
  TdxThemedObjectTypes = []){$ENDIF}: TcxCustomLookAndFeelPainterClass;
const
  APainterMap: array[TcxLookAndFeelKind] of TcxCustomLookAndFeelPainterClass =
    (TcxFlatLookAndFeelPainter, TcxStandardLookAndFeelPainter, TcxUltraFlatLookAndFeelPainter);
begin
{$IFDEF VCL}
  if NativeStyle and AreVisualStylesAvailable(ANeededThemedObjectTypes) then
    Result := TcxWinXPLookAndFeelPainter
  else
{$ENDIF}
    Result := APainterMap[Kind];
end;

procedure TcxLookAndFeel.Assign(Source: TPersistent);
begin
  if Source is TcxLookAndFeel then
    with Source as TcxLookAndFeel do
    begin
      Self.SaveState;
      Self.FData := FData;
      Self.FAssignedValues := FAssignedValues;
      Self.MasterLookAndFeel := MasterLookAndFeel;
      Self.CheckStateChanges;
    end
  else
    inherited Assign(Source);
end;

procedure TcxLookAndFeel.RemoveChangeListener(AListener: IcxLookAndFeelNotificationListener);
var
  AIsLookAndFeelController: Boolean;
begin
  if AListener = nil then
    Exit;
  AIsLookAndFeelController := AListener.GetObject is TcxLookAndFeelController;
  if FChangeListenerList.IndexOf(Pointer(AListener)) < 0 then
    Exit;

  if not FIsDestruction then
    FChangeListenerList.Remove(Pointer(AListener));

  if FIsRootLookAndFeel and AIsLookAndFeelController then
  begin
    Dec(FLookAndFeelControllerCount);
    if FLookAndFeelControllerCount = 0 then
      Reset;
  end;
end;

procedure TcxLookAndFeel.Reset;
begin
  AssignedValues := [];
end;

function TcxLookAndFeel.InternalGetKind: TcxLookAndFeelKind;
begin
  if lfvKind in FAssignedValues then
    Result := FData.Kind
  else
    Result := GetDefaultKind;
end;

function TcxLookAndFeel.InternalGetNativeStyle: Boolean;
begin
  if lfvNativeStyle in FAssignedValues then
    Result := FData.NativeStyle
  else
    Result := GetDefaultNativeStyle;
end;

procedure TcxLookAndFeel.MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
var
  AOwnChangedValues: TcxLookAndFeelValues;
begin
  AOwnChangedValues := (cxLookAndFeelValueAll - FAssignedValues) * AChangedValues;
  Changed(AOwnChangedValues);
end;

function TcxLookAndFeel.GetKind: TcxLookAndFeelKind;
begin
  Result := FCurrentState.Kind;
end;

function TcxLookAndFeel.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  if FIsRootLookAndFeel then
    Result := nil
  else
    if FMasterLookAndFeel = nil then
      Result := FRootLookAndFeel
    else
      Result := FMasterLookAndFeel;
end;

function TcxLookAndFeel.GetNativeStyle: Boolean;
begin
  Result := FCurrentState.NativeStyle;
end;

function TcxLookAndFeel.GetPainter: TcxCustomLookAndFeelPainterClass;
const
  Painters: array[TcxLookAndFeelKind] of TcxCustomLookAndFeelPainterClass =
    (TcxFlatLookAndFeelPainter, TcxStandardLookAndFeelPainter, TcxUltraFlatLookAndFeelPainter);
begin
{$IFDEF VCL}
  if NativeStyle and AreVisualStylesAvailable then
    Result := TcxWinXPLookAndFeelPainter
  else
{$ENDIF}
    Result := Painters[Kind];
end;

procedure TcxLookAndFeel.SetAssignedValues(Value: TcxLookAndFeelValues);
begin
  if Value <> FAssignedValues then
  begin
    SaveState;
    FAssignedValues := Value;
    CheckStateChanges;
  end;
end;

procedure TcxLookAndFeel.SetKind(Value: TcxLookAndFeelKind);
var
  AOldKind: TcxLookAndFeelKind;
begin
  AOldKind := Kind;
  Include(FAssignedValues, lfvKind);
  FData.Kind := Value;
  if AOldKind <> InternalGetKind then
    Changed([lfvKind]);
end;

procedure TcxLookAndFeel.SetMasterLookAndFeel(Value: TcxLookAndFeel);
begin
  if FIsRootLookAndFeel or (Value = Self) then Exit;
  if Value <> MasterLookAndFeel then
  begin
    SaveState;
    if MasterLookAndFeel <> nil then
      MasterLookAndFeel.RemoveChangeListener(Self);
    FMasterLookAndFeel := Value;
    if MasterLookAndFeel <> nil then
      MasterLookAndFeel.AddChangeListener(Self);
    CheckStateChanges;
  end;
end;

procedure TcxLookAndFeel.SetNativeStyle(Value: Boolean);
var
  AOldNativeStyle: Boolean;
begin
  AOldNativeStyle := NativeStyle;
  Include(FAssignedValues, lfvNativeStyle);
  FData.NativeStyle := Value;
  if AOldNativeStyle <> InternalGetNativeStyle then
    Changed([lfvNativeStyle]);
end;

procedure TcxLookAndFeel.Changed(AChangedValues: TcxLookAndFeelValues);
var
  I: Integer;
begin
  if (AChangedValues = []) or FIsDestruction then Exit;
  FCurrentState.Kind := InternalGetKind;
  FCurrentState.NativeStyle := InternalGetNativeStyle;

  for I := 0 to FChangeListenerList.Count - 1 do
    IcxLookAndFeelNotificationListener(FChangeListenerList.Items[I]).MasterLookAndFeelChanged(Self, AChangedValues);

  if Assigned(FOnChanged) then
    FOnChanged(Self, AChangedValues);
end;

procedure TcxLookAndFeel.CheckStateChanges;
var
  AChangedValues: TcxLookAndFeelValues;
begin
  AChangedValues := [];
  if FPrevState.Kind <> InternalGetKind then
    Include(AChangedValues, lfvKind);
  if FPrevState.NativeStyle <> InternalGetNativeStyle then
    Include(AChangedValues, lfvNativeStyle);
  Changed(AChangedValues);
end;

function TcxLookAndFeel.GetDefaultKind: TcxLookAndFeelKind;
begin
  if FIsRootLookAndFeel then
    Result := cxDefaultLookAndFeelKind
  else
    if FMasterLookAndFeel = nil then
      Result := FRootLookAndFeel.Kind
    else
      Result := FMasterLookAndFeel.Kind;
end;

function TcxLookAndFeel.GetDefaultNativeStyle: Boolean;
begin
  if FIsRootLookAndFeel then
    Result := cxDefaultLookAndFeelNativeStyle
  else
    if FMasterLookAndFeel = nil then
      Result := FRootLookAndFeel.NativeStyle
    else
      Result := FMasterLookAndFeel.NativeStyle;
end;

function TcxLookAndFeel.IsKindStored: Boolean;
begin
  Result := lfvKind in FAssignedValues;
end;

function TcxLookAndFeel.IsNativeStyleStored: Boolean;
begin
  Result := lfvNativeStyle in FAssignedValues;
end;

procedure TcxLookAndFeel.SaveState;
begin
  FPrevState.Kind := Kind;
  FPrevState.NativeStyle := NativeStyle;
end;

function TcxLookAndFeel.GetObject: TObject;
begin
  Result := Self;
end;

procedure TcxLookAndFeel.MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
begin
  MasterLookAndFeel := nil;
end;

{ TcxLookAndFeelController }

constructor TcxLookAndFeelController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if RootLookAndFeel <> nil then
    RootLookAndFeel.AddChangeListener(Self);
end;

destructor TcxLookAndFeelController.Destroy;
begin
  if RootLookAndFeel <> nil then
    RootLookAndFeel.RemoveChangeListener(Self);
  inherited Destroy;
end;

function TcxLookAndFeelController.GetKind: TcxLookAndFeelKind;
begin
  if RootLookAndFeel = nil then
    Result := cxDefaultLookAndFeelKind
  else
    Result := RootLookAndFeel.Kind;
end;

function TcxLookAndFeelController.GetNativeStyle: Boolean;
begin
  if RootLookAndFeel = nil then
    Result := cxDefaultLookAndFeelNativeStyle
  else
    Result := RootLookAndFeel.NativeStyle;
end;

procedure TcxLookAndFeelController.SetKind(Value: TcxLookAndFeelKind);
begin
  if RootLookAndFeel <> nil then
    RootLookAndFeel.Kind := Value;
end;

procedure TcxLookAndFeelController.SetNativeStyle(Value: Boolean);
begin
  if RootLookAndFeel <> nil then
    RootLookAndFeel.NativeStyle := Value;
end;

procedure TcxLookAndFeelController.Modified;
begin
end;

function TcxLookAndFeelController.GetObject: TObject;
begin
  Result := Self;
end;

procedure TcxLookAndFeelController.MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  if csDesigning in ComponentState then
    Modified;
end;

procedure TcxLookAndFeelController.MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
var
  AOwnerForm: TCustomForm;
begin
  AOwnerForm := nil;
{$IFDEF DELPHI6}
  if (Owner is TFrame) and (TFrame(Owner).Owner is TForm) then
    AOwnerForm := TForm(TFrame(Owner).Owner)
  else
{$ENDIF}
    if Owner is TForm then
      AOwnerForm := TForm(Owner);
  if AOwnerForm = nil then
    Exit;
{$IFDEF VCL}
  if AOwnerForm.Designer <> nil then
    AOwnerForm.Designer.Modified;
{$ELSE}
  if AOwnerForm.DesignerHook <> nil then
    AOwnerForm.DesignerHook.Modified;
{$ENDIF}
end;

initialization
{$IFDEF DELPHI6}
  GroupDescendentsWith(TcxLookAndFeelController, TForm);
{$ENDIF}
  FRootLookAndFeel := TcxLookAndFeel.Create(nil);
  FRootLookAndFeel.FIsRootLookAndFeel := True;

finalization
  FreeAndNil(FRootLookAndFeel);

end.
