
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

unit cxDesignWindows;

{$I cxVer.inc}

interface

uses
  {$IFNDEF VCL}
  QTypes,
  {$ENDIF}
  {$IFDEF DELPHI5}
  DesignConst,
  {$ENDIF}
  {$IFDEF DELPHI6}
  DesignIntf, ComponentDesigner,
    {$IFDEF VCL}
    DesignWindows,
    {$ELSE}
    DesignEditors, Qt,
    {$ENDIF}
  {$ELSE}
  DsgnWnds, DsgnIntf, LibIntf,
  {$ENDIF}
  TypInfo, Classes, SysUtils, Controls, Graphics, Forms, StdCtrls,
  cxClasses;

{$IFNDEF DELPHI5}
const
  SIniEditorsName = 'Property Editors';
{$ENDIF}

type
  {$IFNDEF DELPHI6}
  IDesigner = IFormDesigner;
  {$ENDIF}

  {$IFDEF DELPHI6}
  TDesignerSelectionList = IDesignerSelections;
  {$ELSE}
    {$IFNDEF DELPHI5}
    TDesignerSelectionList = TComponentList;
    {$ENDIF}
  {$ENDIF}

{$IFDEF DELPHI6}
  {$IFNDEF VCL}
  TClxDesignWindow = class(TForm, IUnknown, IDesignWindow, IDesignNotification,
    IEditHandler, IActivatable
{$IFDEF LINUX}
    , IDesignerThreadAffinity
{$ENDIF}
    )
  private
    FSelection: IDesignerSelections;
    FOwner: TComponent;
    FDesigner: IDesigner;
    FComponentDesigner: IComponentDesigner;
    FActive: Boolean;
    procedure ComponentRead(Component: TComponent);
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var Name: string);
  protected
    procedure Activated; dynamic;
    procedure ActivateInspector(Ch: Char);
    function ClipboardComponents: Boolean;
    procedure CopyComponents(Root: TComponent;
      const Components: IDesignerSelections);
    procedure PasteComponents(AOwner, AParent: TComponent;
      const Components: IDesignerSelections);
    procedure SetSelection(const Components: IDesignerSelections);
    function UniqueName(Component: TComponent): string; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // IEditHandler
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;

    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); virtual;
    procedure ItemsModified(const Designer: IDesigner); virtual;

    // IDesignWindowActions
    procedure WindowHide; virtual;
    procedure WindowShow; virtual;

    // IActivatable
    procedure IActivatable.Activate = ForceActive;
    procedure ForceActive;
{$IFDEF LINUX}
    // IDesignerThreadAffinity
    function GetThreadAffinity: TThreadAffinity;
{$ENDIF}
    property Active: Boolean read FActive;
    property Designer: IDesigner read FDesigner write FDesigner;
    property ComponentDesigner: IComponentDesigner read FComponentDesigner;
  end;
  {$ENDIF}
{$ENDIF}

  { TcxDesignWindow }

  {$IFDEF DELPHI6}
    {$IFDEF VCL}
    TcxDesignWindow = class(TDesignWindow)
    {$ELSE}
    TcxDesignWindow = class(TClxDesignWindow)
    {$ENDIF}
  {$ELSE}
  TcxDesignWindow = class(TDesignWindow)
  {$ENDIF}
  private
    FLockCount: Integer;
  protected
    procedure Activated; override;
    function UniqueName(Component: TComponent): string; override;
  public
    procedure BeginUpdate;
    function CreateDesignerSelectionList: TDesignerSelectionList;
    procedure EndUpdate;
    class function GetBaseRegKey: string;
    procedure GetSelectionList(AList: TList);
    procedure SelectionChanged({$IFDEF DELPHI6}const ADesigner: IDesigner;{$ENDIF}
      {$IFDEF DELPHI6}const{$ENDIF}ASelection: TDesignerSelectionList); override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); virtual;
    procedure SetSelectionList(AList: TList);
    procedure UpdateSelection;
    property LockCount: Integer read FLockCount;
  end;

  { TcxDesignFormEditor }

  TcxDesignFormEditor = class(TcxDesignWindow)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FClosing: Boolean;
    procedure UpdateCaption;
  protected
    procedure CloseEditor; virtual;
    procedure InitFormEditor; virtual;
    procedure UpdateContent; virtual;
  public
    Component: TComponent;
    ComponentClassName: string;
    {$IFDEF DELPHI6}
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    {$ELSE}
    procedure FormClosed(AForm: TCustomForm); override;
    procedure ComponentDeleted(Component: IPersistent); override;
    procedure FormModified; override;
    {$ENDIF}
    procedure DoItemsModified; virtual;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); {$IFNDEF DELPHI6}virtual;{$ELSE}override;{$ENDIF}
    procedure ItemsModified(const Designer: IDesigner); {$IFNDEF DELPHI6}virtual;{$ELSE}override;{$ENDIF}
    procedure SelectComponents(AList: TList; ADefaultComponent: TPersistent); virtual;
    property Closing: Boolean read FClosing;
  end;

  TcxDesignFormEditorClass = class of TcxDesignFormEditor;

function ShowFormEditorClass(ADesigner: IDesigner; AComponent: TComponent;
  AFormEditorClass: TcxDesignFormEditorClass): TcxDesignFormEditor;
procedure UpdateDesignFormEditors(AComponent: TComponent);

// ListBox routines

type
  TcxListBoxReindexProc = procedure(AList: TList; ANewIndex: Integer) of object;

function LockListBox(AListBox: TListBox): TNotifyEvent;
procedure UnlockListBox(AListBox: TListBox; APrevOnClick: TNotifyEvent);

procedure ListBoxSetItemIndex(AListBox: TListBox; AItemIndex: Integer);
procedure ListBoxSetSelected(AListBox: TListBox; AItemIndex: Integer; ASelected: Boolean);

procedure ListBoxRestoreSelection(AListBox: TListBox; var ASelection: TStringList; AItemIndex, ATopIndex: Integer);
procedure ListBoxSaveSelection(AListBox: TListBox; var ASelection: TStringList; var AItemIndex, ATopIndex: Integer);

procedure ListBoxRestorePos(AListBox: TListBox; AItemIndex, ATopIndex: Integer);
procedure ListBoxSavePos(AListBox: TListBox; var AItemIndex, ATopIndex: Integer);

procedure ListBoxDeleteSelection(AListBox: TListBox; ASetFocus: Boolean);
procedure ListBoxGetSelection(AListBox: TListBox; AList: TList);
procedure ListBoxLoadCollection(AListBox: TListBox; ACollection: TCollection);
procedure ListBoxSelectByObject(AListBox: TListBox; AObject: TObject);
procedure ListBoxSyncSelection(AListBox: TListBox; AList: TList);
procedure ListBoxClearSelection(AListBox: TListBox);
procedure ListBoxSelectAll(AListBox: TListBox);

procedure ListBoxMoveUpItems(AListBox: TListBox; var APrevDragIndex: Integer;
  AReindexProc: TcxListBoxReindexProc);
procedure ListBoxMoveDownItems(AListBox: TListBox; var APrevDragIndex: Integer;
  AReindexProc: TcxListBoxReindexProc);

procedure ListBoxDragOver(AListBox: TListBox; Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; var APrevDragIndex: Integer);
procedure ListBoxDragDrop(AListBox: TListBox; Sender, Source: TObject; X, Y: Integer;
  var APrevDragIndex: Integer; AReindexProc: TcxListBoxReindexProc);
procedure ListBoxEndDrag(AListBox: TListBox; Sender, Target: TObject; X, Y: Integer;
  var APrevDragIndex: Integer);

// component rename routines

function CreateUniqueName(AOwnerForm, AOwnerComponent, AChildComponent: TComponent;
  const ATruncateClassName, AFieldName: string): string;

implementation

{$IFNDEF VCL}
uses
  Clipbrd;
{$ENDIF}

{$IFDEF VCL}
{$R *.dfm}
{$ELSE}
{$R *.xfm}
{$ENDIF}

var
  Editors: TList;

function ShowFormEditorClass(ADesigner: IDesigner; AComponent: TComponent;
  AFormEditorClass: TcxDesignFormEditorClass): TcxDesignFormEditor;
var
  I: Integer;
begin
  if Editors = nil then
    Editors := TList.Create;
  for I := 0 to Editors.Count - 1 do
  begin
    Result := TcxDesignFormEditor(Editors[I]);
    with Result do
      if (Designer = ADesigner) and (Component = AComponent) then
      begin
        Show;
        BringToFront;
        Exit;
      end;
  end;
  Result := AFormEditorClass.Create(Application);
  with Result do
  try
    Designer := ADesigner;
    Component := AComponent;
    ComponentClassName := AComponent.ClassName;
    InitFormEditor;
    Show;
  except
    Free;
  end;
end;

procedure UpdateDesignFormEditors(AComponent: TComponent);
var
  I: Integer;
  ADesignFormEditor: TcxDesignFormEditor;
begin
  if Editors = nil then
    Editors := TList.Create;
  for I := 0 to Editors.Count - 1 do
  begin
    ADesignFormEditor := TcxDesignFormEditor(Editors[I]);
    if ADesignFormEditor.Component = AComponent then
      ADesignFormEditor.UpdateContent;
  end;
end;

// ListBox Routines

function LockListBox(AListBox: TListBox): TNotifyEvent;
begin
  Result := AListBox.OnClick;
  AListBox.OnClick := nil;
end;

procedure UnlockListBox(AListBox: TListBox; APrevOnClick: TNotifyEvent);
begin
  {$IFNDEF VCL}
  QApplication_sendPostedEvents(AListBox.Handle, Integer(QEventType_LBClick));
  {$ENDIF}
  AListBox.OnClick := APrevOnClick;
end;

procedure ListBoxSetItemIndex(AListBox: TListBox; AItemIndex: Integer);
var
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    AListBox.ItemIndex := AItemIndex;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;

procedure ListBoxSetSelected(AListBox: TListBox; AItemIndex: Integer; ASelected: Boolean);
var
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    if AListBox.Selected[AItemIndex] <> ASelected then
      AListBox.Selected[AItemIndex] := ASelected;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;

procedure ListBoxRestoreSelection(AListBox: TListBox; var ASelection: TStringList;
  AItemIndex, ATopIndex: Integer);
var
  I: Integer;
  APrevOnClick: TNotifyEvent;
begin
  try
    APrevOnClick := LockListBox(AListBox);
    try
      with AListBox do
        for I := 0 to Items.Count - 1 do
          Selected[I] := ASelection.IndexOfObject(Items.Objects[I]) <> -1;
      if ATopIndex <> -1 then AListBox.TopIndex := ATopIndex;
      if AItemIndex <> -1 then AListBox.ItemIndex := AItemIndex;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
  finally
    AListBox.Items.EndUpdate;
    FreeAndNil(ASelection);
  end;
end;

procedure ListBoxSaveSelection(AListBox: TListBox; var ASelection: TStringList;
  var AItemIndex, ATopIndex: Integer);
var
  I: Integer;
begin
  ASelection := TStringList.Create;
  try
    AItemIndex := AListBox.ItemIndex;
    ATopIndex := AListBox.TopIndex;
    with AListBox do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then ASelection.AddObject(Items[I], Items.Objects[I]);
    AListBox.Items.BeginUpdate;
  except
    ASelection.Free;
    ASelection := nil;
  end;
end;

procedure ListBoxRestorePos(AListBox: TListBox; AItemIndex, ATopIndex: Integer);
var
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    if ATopIndex <> -1 then AListBox.TopIndex := ATopIndex;
    if AItemIndex <> -1 then AListBox.ItemIndex := AItemIndex;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
//  AListBox.Items.EndUpdate;
end;

procedure ListBoxSavePos(AListBox: TListBox; var AItemIndex, ATopIndex: Integer);
begin
  AItemIndex := AListBox.ItemIndex;
  ATopIndex := AListBox.TopIndex;
//  AListBox.Items.BeginUpdate;
end;

procedure ListBoxDeleteSelection(AListBox: TListBox; ASetFocus: Boolean);
var
  I, AIndex: Integer;
begin
  AIndex := -1;
  for I := 0 to AListBox.Items.Count - 1 do
    if AListBox.Selected[I] then
    begin
      if AIndex = -1 then AIndex := I;
      AListBox.Items.Objects[I].Free;
    end;
  // Show Last Item
  if AIndex >= AListBox.Items.Count then
    AIndex := AListBox.Items.Count - 1;
  if AIndex >= 0 then
  begin
    ListBoxSetItemIndex(AListBox, AIndex);
    if ASetFocus and AListBox.CanFocus then
      AListBox.SetFocus;
  end;
end;

procedure ListBoxGetSelection(AListBox: TListBox; AList: TList);
var
  I: Integer;
begin
  for I := 0 to AListBox.Items.Count - 1 do
    if AListBox.Selected[I] then
      AList.Add(AListBox.Items.Objects[I]);
end;

procedure ListBoxLoadCollection(AListBox: TListBox; ACollection: TCollection);
var
  I, AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
  S: string;
begin
  ListBoxSaveSelection(AListBox, ASelection, AItemIndex, ATopIndex);
  try
    AListBox.Items.Clear;
    for I := 0 to ACollection.Count - 1 do
    begin
      S := Format('%d - %s',[I, ACollection.Items[I].DisplayName]);
      AListBox.Items.AddObject(S, ACollection.Items[I]);
    end;
  finally
    ListBoxRestoreSelection(AListBox, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure ListBoxSelectByObject(AListBox: TListBox; AObject: TObject);
var
  AIndex: Integer;
begin
  AIndex := AListBox.Items.IndexOfObject(AObject);
  if AIndex <> -1 then
    ListBoxSetSelected(AListBox, AIndex, True);
end;

procedure ListBoxSyncSelection(AListBox: TListBox; AList: TList);
var
  I, AItemIndex, ATopIndex: Integer;
  ASelected: Boolean;
  APrevOnClick: TNotifyEvent;
begin
  ListBoxSavePos(AListBox, AItemIndex, ATopIndex);
  try
    APrevOnClick := LockListBox(AListBox);
    try
      for I := 0 to AListBox.Items.Count - 1 do
      begin
        ASelected := AList.IndexOf(AListBox.Items.Objects[I]) <> -1;
        if AListBox.Selected[I] <> ASelected then
          AListBox.Selected[I] := ASelected;
      end;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
  finally
    ListBoxRestorePos(AListBox, AItemIndex, ATopIndex);
  end;
end;

procedure ListBoxClearSelection(AListBox: TListBox);
var
  APrevOnClick: TNotifyEvent;
{$IFNDEF DELPHI6}
  I: Integer;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    if AListBox.MultiSelect then
      for I := 0 to AListBox.Items.Count - 1 do
        AListBox.Selected[I] := False
    else
      AListBox.ItemIndex := -1;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;
{$ELSE}
begin
  APrevOnClick := LockListBox(AListBox);
  try
    AListBox.ClearSelection;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;
{$ENDIF}

procedure ListBoxSelectAll(AListBox: TListBox);
var
  I: Integer;
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    with AListBox do
      for I := 0 to Items.Count - 1 do
        Selected[I] := True;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;

procedure ListBoxMoveItems(AListBox: TListBox; AIndex: Integer;
  var APrevDragIndex: Integer; AReindexProc: TcxListBoxReindexProc);
var
  I: Integer;
  APrevOnClick: TNotifyEvent;
  AList: TList;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    with AListBox do
    begin
      if (0 <= APrevDragIndex) and (APrevDragIndex < Items.Count) then
      begin
        Selected[APrevDragIndex] := False;
        APrevDragIndex := -1;
      end;
      if AIndex <> -1 then
      begin
        AList := TList.Create;
        try
          for I := 0 to Items.Count - 1 do
          begin
            if Selected[I] then
              AList.Add(Items.Objects[I]);
          end;
          AReindexProc(AList, AIndex);
        finally
          AList.Free;
        end;
      end;
    end;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
end;

procedure ListBoxMoveUpItems(AListBox: TListBox; var APrevDragIndex: Integer; AReindexProc: TcxListBoxReindexProc);
var
  AIndex: Integer;
  APrevOnClick: TNotifyEvent;
begin
  AIndex := AListBox.ItemIndex;
  Dec(AIndex);
  if AIndex >= 0 then
  begin
    APrevOnClick := LockListBox(AListBox);
    try
      ListBoxMoveItems(AListBox, AIndex, APrevDragIndex, AReindexProc);
      AListBox.ItemIndex := AIndex;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
  end;
end;

procedure ListBoxMoveDownItems(AListBox: TListBox; var APrevDragIndex: Integer; AReindexProc: TcxListBoxReindexProc);
var
  AIndex: Integer;
  APrevOnClick: TNotifyEvent;
begin
  AIndex := AListBox.ItemIndex;
  Inc(AIndex);
  if AIndex < AListBox.Items.Count then
  begin
    APrevOnClick := LockListBox(AListBox);
    try
      ListBoxMoveItems(AListBox, AIndex, APrevDragIndex, AReindexProc);
      AListBox.ItemIndex := AIndex;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
  end;
end;

procedure ListBoxDragOver(AListBox: TListBox; Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; var APrevDragIndex: Integer);
var
  AIndex: Integer;
  APrevOnClick: TNotifyEvent;
begin
  if Source <> AListBox then
    Accept := False
  else
  begin
    APrevOnClick := LockListBox(AListBox);
    try
      with AListBox do
      begin
        Accept := True;
        AIndex := ItemAtPos(Point(X, Y), True);
        if APrevDragIndex <> AIndex then
        begin
          if (0 <= APrevDragIndex) and (APrevDragIndex < Items.Count) then
            Selected[APrevDragIndex] := False;
          if AIndex <> -1 then
            if not Selected[AIndex] then
            begin
              Selected[AIndex] := True;
              APrevDragIndex := AIndex;
            end
            else
              APrevDragIndex := -1;
        end;
        ItemIndex := AIndex;
      end;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
  end;
end;

procedure ListBoxDragDrop(AListBox: TListBox; Sender, Source: TObject; X, Y: Integer;
  var APrevDragIndex: Integer; AReindexProc: TcxListBoxReindexProc);
var
  AIndex: Integer;
begin
  AIndex := AListBox.ItemAtPos(Point(X, Y), True);
  if AIndex <> -1 then
    ListBoxMoveItems(AListBox, AIndex, APrevDragIndex, AReindexProc);
end;

procedure ListBoxEndDrag(AListBox: TListBox; Sender, Target: TObject; X, Y: Integer;
  var APrevDragIndex: Integer);
begin
  if (0 <= APrevDragIndex) and (APrevDragIndex < AListBox.Items.Count) then
  begin
    ListBoxSetSelected(AListBox, APrevDragIndex, False);
    APrevDragIndex := -1;
  end;
end;

// component rename routines

procedure CrunchFieldName(var AFieldName: string);
var
  I: Integer;
begin
  I := 1;
  while I <= Length(AFieldName) do
  begin
    if AFieldName[I] in ['A'..'Z','a'..'z','_','0'..'9'] then
      Inc(I)
    else
      if AFieldName[I] in LeadBytes then
        Delete(AFieldName, I, 2)
      else
        Delete(AFieldName, I, 1);
  end;
end;

function GenerateName(AOwnerComponent: TComponent; const AClassName, ATruncateClassName, AFieldName: string;
  ANumber: Integer): string;
var
  S: string;
begin
  S := AFieldName;
  CrunchFieldName(S);
  if (S = '') or (S[1] in ['0'..'9']) then
  begin
    if AClassName <> '' then
    begin
      if (ATruncateClassName <> '') and
        (CompareText(ATruncateClassName, Copy(AClassName, 1, Length(ATruncateClassName))) = 0) then
      begin
        S := Copy(AClassName, Length(ATruncateClassName) + 1, Length(AClassName)) + S;
      end
      else
      begin
        S := AClassName + S;
        if S[1] = 'T' then Delete(S, 1, 1);
      end;
    end;
  end;
  if AOwnerComponent <> nil then
    Result := AOwnerComponent.Name + S
  else
    Result := S;
  if ANumber > 0 then
    Result := Result + IntToStr(ANumber);
end;

function CreateUniqueName(AOwnerForm, AOwnerComponent, AChildComponent: TComponent;
  const ATruncateClassName, AFieldName: string): string;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    with AOwnerForm do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] <> AChildComponent) and
          (CompareText(Components[I].Name, AName) = 0) then
        begin
          Result := False;
          Break;
        end;
  end;

var
  I, J: Integer;
begin
  if AFieldName <> '' then
    J := 0
  else
    J := 1;
  for I := J to MaxInt do
  begin
    Result := GenerateName(AOwnerComponent, AChildComponent.ClassName,
      ATruncateClassName, AFieldName, I);
    if IsUnique(Result) then
      Break;
  end;
end;

{$IFNDEF VCL}

{ TClxDesignWindow }

procedure CopyStreamToClipboard(S: TStream);
var
  T: TStringStream;
  I: TValueType;
  V: Integer;
begin
//  Clipboard.Open;
//  try
    //Clipboard.SetFormat(SDelphiComponent, S);
    S.Position := 0;
    T := TStringStream.Create('');
    try
      repeat
        S.Read(I, SizeOf(I));
        S.Seek(-SizeOf(I), 1);
        if I = vaNull then Break;
        ObjectBinaryToText(S, T);
      until False;
      V := 0;
      T.Write(V, 1);
      Clipboard.AsText := T.DataString;
    finally
      T.Free;
    end;
//  finally
//    Clipboard.Close;
//  end;
end;

function GetClipboardStream: TMemoryStream;
var
  S, T: TMemoryStream;
  Format: string;
  V: TValueType;

  function AnotherObject(S: TStream): Boolean;
  var
    Buffer: array[0..255] of Char;
    Position: Integer;
  begin
    Position := S.Position;
    Buffer[S.Read(Buffer, SizeOf(Buffer))-1] := #0;
    S.Position := Position;
    Result := PossibleStream(Buffer);
  end;

begin
  Result := TMemoryStream.Create;
  try
    {if Clipboard.Provides(SDelphiComponent) then
      Format := SDelphiComponent else}
      Format := 'text/plain';
    Clipboard.GetFormat(Format, Result);
    Result.Position := 0;
    Result.Size := StrLen(Result.Memory);
    {if Format <> SDelphiComponent then
    begin}
      S := TMemoryStream.Create;
      try
        while AnotherObject(Result) do ObjectTextToBinary(Result, S);
        V := vaNull;
        S.Write(V, SizeOf(V));
        T := Result;
        Result := nil;
        T.Free;
      except
        S.Free;
        raise;
      end;
      Result := S;
      Result.Position := 0;
    {end;}
  except
    Result.Free;
    raise;
  end;
end;

constructor TClxDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentDesigner := ActiveDesigner;
  RegisterDesignNotification(Self);
  SetBounds(200, ComponentDesigner.Environment.GetMainWindowSize.Bottom + 2, Width, Height);
end;

destructor TClxDesignWindow.Destroy;
begin
  UnregisterDesignNotification(Self);
  inherited Destroy;
end;

procedure TClxDesignWindow.Activated;
begin
end;

(*procedure TClxDesignWindow.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  FActive := Msg.Active <> 0;
  if FActive then
    Activated;
end;*)

procedure TClxDesignWindow.ActivateInspector(Ch: Char);
begin
  ComponentDesigner.Environment.ModalEdit(Ch, Self);
end;

function TClxDesignWindow.ClipboardComponents: Boolean;
begin
  try
    Result := False;
(*    Result := Clipboard.HasFormat(CF_COMPONENTS) or
      (Clipboard.HasFormat(CF_TEXT) and PossibleStream(Clipboard.AsText)); *)
  except
    Result := False;
  end;
end;

procedure TClxDesignWindow.CopyComponents(Root: TComponent;
  const Components: IDesignerSelections);
var
  S: TMemoryStream;
  W: TWriter;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    W := TWriter.Create(S, 1024);
    try
      W.Root := Root;
      for I := 0 to Components.Count - 1 do
      begin
        W.WriteSignature;
        W.WriteComponent(TComponent(Components[I]));
      end;
      W.WriteListEnd;
    finally
      W.Free;
    end;
    CopyStreamToClipboard(S);
  finally
    S.Free;
  end;
end;

function TClxDesignWindow.GetEditState: TEditState;
begin
  Result := [];
end;

function TClxDesignWindow.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TClxDesignWindow.ForceActive;
begin
  QWidget_setActiveWindow(Handle);
end;

procedure TClxDesignWindow.WindowHide;
begin
  if Visible then
    QWidget_hide(Handle);
end;

procedure TClxDesignWindow.WindowShow;
begin
  if Visible then
    QWidget_show(Handle);
end;

procedure TClxDesignWindow.ComponentRead(Component: TComponent);
begin
  FSelection.Add(Component);
end;

procedure TClxDesignWindow.ReaderSetName(Reader: TReader; Component: TComponent;
  var Name: string);
begin
  if (Reader.Root = FOwner) and (FOwner.FindComponent(Name) <> nil) then
    Name := UniqueName(Component);
end;

procedure TClxDesignWindow.PasteComponents(AOwner, AParent: TComponent;
  const Components: IDesignerSelections);
var
  S: TStream;
  R: TReader;
begin
  S := GetClipboardStream;
  try
    R := TReader.Create(S, 1024);
    try
      R.OnSetName := ReaderSetName;
      FOwner := AOwner;
      FSelection := Components;
      R.ReadComponents(AOwner, AParent, ComponentRead);
    finally
      R.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TClxDesignWindow.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
end;

procedure TClxDesignWindow.SetSelection(const Components: IDesignerSelections);
begin
  ComponentDesigner.SetSelection(Designer, Self, Components);
end;

procedure TClxDesignWindow.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin

end;

procedure TClxDesignWindow.ItemInserted(const ADesigner: IDesigner; Item: TPersistent);
begin

end;

procedure TClxDesignWindow.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if FDesigner = Designer then
  begin
    FDesigner := nil;
    FComponentDesigner := nil;
  end;
end;

procedure TClxDesignWindow.DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean);
begin
  FDesigner := Designer;
  FComponentDesigner := ActiveDesigner;
end;

procedure TClxDesignWindow.ItemsModified(const Designer: IDesigner);
begin
end;

{$IFDEF LINUX}
function TClxDesignWindow.GetThreadAffinity: TThreadAffinity;
begin
  Result := taQT;
end;
{$ENDIF}

{$ENDIF}

{ TcxDesignWindow }

procedure TcxDesignWindow.BeginUpdate;
begin
  Inc(FLockCount);
end;

function TcxDesignWindow.CreateDesignerSelectionList: TDesignerSelectionList;
begin
 {$IFDEF DELPHI6}
  Result := CreateSelectionList;
 {$ELSE}
  Result := TDesignerSelectionList.Create;
 {$ENDIF}
end;

procedure TcxDesignWindow.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    UpdateSelection;
end;

class function TcxDesignWindow.GetBaseRegKey: string;
begin
  {$IFDEF DELPHI6}
  Result := ActiveDesigner{ComponentDesigner?}.Environment.GetBaseRegKey + '\' + SIniEditorsName;
  {$ELSE}
  Result := DelphiIDE.GetBaseRegKey + '\' + SIniEditorsName;
  {$ENDIF}
end;

procedure TcxDesignWindow.GetSelectionList(AList: TList);
var
  ASelectionList: TDesignerSelectionList;
  I: Integer;
begin
  ASelectionList := CreateDesignerSelectionList;
  try
    Designer.GetSelections(ASelectionList);
    AList.Capacity := ASelectionList.Count;
    for I := 0 to ASelectionList.Count - 1 do
      AList.Add(ASelectionList[I]);
  finally
    {$IFNDEF DELPHI6}
    ASelectionList.Free;
    {$ENDIF}
  end;
end;

procedure TcxDesignWindow.SelectionChanged({$IFDEF DELPHI6}const ADesigner: IDesigner;{$ENDIF}
  {$IFDEF DELPHI6}const{$ENDIF}ASelection: TDesignerSelectionList);
begin
  if LockCount <> 0 then Exit;
  {$IFDEF DELPHI6}
  if ADesigner = Designer then
  {$ENDIF}
    SelectionsChanged(ASelection);
end;

procedure TcxDesignWindow.SelectionsChanged(const ASelection: TDesignerSelectionList);
begin
end;

procedure TcxDesignWindow.SetSelectionList(AList: TList);
var
  ASelectionList: TDesignerSelectionList;
  I: Integer;
begin
  ASelectionList := CreateDesignerSelectionList;
  try
    for I := 0 to AList.Count - 1 do
      ASelectionList.Add(AList[I]);
    Designer.SetSelections(ASelectionList);
  finally
    {$IFNDEF DELPHI6}
    ASelectionList.Free;
    {$ENDIF}
  end;
end;

procedure TcxDesignWindow.UpdateSelection;
var
  ASelectionList: TDesignerSelectionList;
begin
  ASelectionList := CreateDesignerSelectionList;
  try
    Designer.GetSelections(ASelectionList);
    SelectionChanged({$IFDEF DELPHI6}Designer, {$ENDIF}ASelectionList);
  finally
    {$IFNDEF DELPHI6}
    ASelectionList.Free;
    {$ENDIF}
  end;
end;

procedure TcxDesignWindow.Activated;
begin
//  UpdateSelection; // TODO:
end;

function TcxDesignWindow.UniqueName(Component: TComponent): string;
begin
  Result := ''; //inherited UniqueName(Component);
end;

{ TcxDesignFormEditor }

{$IFDEF DELPHI6}

procedure TcxDesignFormEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if Designer = ADesigner then
    CloseEditor;
  inherited;
end;

{$ELSE}

procedure TcxDesignFormEditor.FormClosed(AForm: TCustomForm);
begin
  if Designer.Form = AForm then
    CloseEditor;
  inherited;
end;

procedure TcxDesignFormEditor.ComponentDeleted(Component: IPersistent);
var
  AItem: TPersistent;
begin
  AItem := TryExtractPersistent(Component);
  if AItem <> nil then
    ItemDeleted(Designer, AItem);
end;

procedure TcxDesignFormEditor.FormModified;
begin
  ItemsModified(Designer);
end;

{$ENDIF}

procedure TcxDesignFormEditor.DoItemsModified;
begin
end;

procedure TcxDesignFormEditor.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem = nil) or Closing then Exit;
  if (Component = nil) or (csDestroying in Component.ComponentState) or
    (AItem = Component) then
    CloseEditor;
end;

procedure TcxDesignFormEditor.ItemsModified(const Designer: IDesigner);
begin
 if Closing or (Component = nil) or (csDestroying in Component.ComponentState) then
   Exit;
  UpdateCaption;
  DoItemsModified;
end;

procedure TcxDesignFormEditor.SelectComponents(AList: TList; ADefaultComponent: TPersistent);
begin
  {$IFDEF VCL}
  if not Active then Exit;
  {$ENDIF}
  if AList.Count > 0 then
    SetSelectionList(AList)
  else
    if Component <> nil then
    begin
      if ADefaultComponent <> nil then
        Designer.SelectComponent(ADefaultComponent)
      else
        Designer.SelectComponent(Component);
    end;
end;

procedure TcxDesignFormEditor.CloseEditor;
begin
  FClosing := True;
  Component := nil;
  Close;
end;

procedure TcxDesignFormEditor.InitFormEditor;
begin
  UpdateCaption;
end;

procedure TcxDesignFormEditor.UpdateContent;
begin
end;

procedure TcxDesignFormEditor.UpdateCaption;
var
  S: string;
begin
  if (Component <> nil) and (Component.Name <> '') then
  begin
    S := Component.Name;
    if Component.Owner <> nil then
      S := Component.Owner.Name + '.' + S;
    Caption := S;
  end;
end;

procedure TcxDesignFormEditor.FormCreate(Sender: TObject);
begin
  if Editors <> nil then
    Editors.Add(Self);
end;

procedure TcxDesignFormEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // TODO: save pos
  Action := caFree;
end;

procedure TcxDesignFormEditor.FormDestroy(Sender: TObject);
begin
  if Editors <> nil then
    Editors.Remove(Self);
end;

initialization

finalization
  FreeAndNil(Editors);

end.
