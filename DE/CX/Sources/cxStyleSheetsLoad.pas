
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

unit cxStyleSheetsLoad;

{$I cxVer.inc}
interface

uses
  {$IFDEF DELPHI6}
    //Variants, 
  {$ENDIF}
  {$IFDEF VCL}
  Windows, Messages,
  {$ELSE}
  Qt,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxStyles, ExtCtrls, cxStyleSheetEditor;

type

TcxPredefinedStyleSheets = class
private
  FList: TList;
protected
  procedure AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
  procedure AddStyleSheets; virtual; abstract;
public
  constructor Create; virtual;
  destructor Destroy; override;
  procedure GetStyleSheetsByClass(AStyleSheetClass: TcxCustomStyleSheetClass;
      const AList: TList);
end;

TcxPredefinedStyleSheetsList = class
private
  FList: TList;
  FStyleSheetClassComboBox: TComboBox;
  FStyleSheetsListBox: TListBox;
  FLoadButton: TButton;
  FPreview: TcxStyleSheetEditorPreview;
  procedure StyleSheetClassComboBoxClick(Sender: TObject);
  procedure StyleSheetsListBoxClick(Sender: TObject);
protected
  procedure UpdateButton;
public
  constructor Create(AStyleSheetClassComboBox: TComboBox;
              AStyleSheetsListBox: TListBox; ALoadButton: TButton);
  destructor Destroy; override;
  function CurrentStyleSheet: TcxCustomStyleSheet;
  function CurrentStyleSheetClass: TcxCustomStyleSheetClass;
  procedure FillListBox;
end;


TfrmcxStyleSheetsLoad = class(TForm)
  pnlBottom: TPanel;
  pnlBottomButtons: TPanel;
  btnLoad: TButton;
  btnClose: TButton;
  Bevel: TBevel;
  pnlStyles: TPanel;
  pnlStyleSheetClasses: TPanel;
  lbStyleSheetClass: TLabel;
  cbStyleSheetClasses: TComboBox;
  lbStyleSheets: TListBox;
  pnlPreview: TPanel;
  Panel1: TPanel;
  pnlPreviewClient: TPanel;
public
  constructor Create(AOwner: TComponent); override;
  function CurrentStyleSheetClass: TcxCustomStyleSheetClass;
  procedure SetShowPreview(Value: Boolean);
end;


TcxPredefinedStyleSheetsClass = class of TcxPredefinedStyleSheets;

procedure RegisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
procedure UnregisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);

procedure ShowLoadStyleSheetsFromIniFile(const AIniFileName: string;
                AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);

procedure ShowLoadStyleSheetsFromPreDefineStyles(AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  IniFiles;

var
  FPredefinedStyleSheetsList: TList = nil;
  
procedure RegisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
begin
  if FPredefinedStyleSheetsList = nil then
    FPredefinedStyleSheetsList := TList.Create;
  if FPredefinedStyleSheetsList.IndexOf(APredefinedStyleSheetsClass) = -1 then
   FPredefinedStyleSheetsList.Add(APredefinedStyleSheetsClass)
end;

procedure UnregisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
begin
  if FPredefinedStyleSheetsList <> nil then
   FPredefinedStyleSheetsList.Remove(APredefinedStyleSheetsClass)
end;
  
{ TcxPredefinedStyleSheets }
constructor TcxPredefinedStyleSheets.Create;
begin
  FList := TList.Create;
end;

destructor TcxPredefinedStyleSheets.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TcxPredefinedStyleSheets.GetStyleSheetsByClass(AStyleSheetClass: TcxCustomStyleSheetClass;
    const AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to FList.Count - 1 do
    if TcxCustomStyleSheetClass(TcxCustomStyleSheet(FList[I]).ClassType) = AStyleSheetClass then
      AList.Add(FList[I]);
end;
    
procedure TcxPredefinedStyleSheets.AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  if FList.IndexOf(AStyleSheet) = -1 then
    FList.Add(AStyleSheet);
end;

{ TcxPredefinedStyleSheetsList }
constructor TcxPredefinedStyleSheetsList.Create(AStyleSheetClassComboBox: TComboBox;
            AStyleSheetsListBox: TListBox; ALoadButton: TButton);
var
  I: Integer;          
begin
  FPreview := nil;
  FList := TList.Create;
  FStyleSheetClassComboBox := AStyleSheetClassComboBox;
  FStyleSheetsListBox := AStyleSheetsListBox;
  FLoadButton := ALoadButton;
  FStyleSheetClassComboBox.OnClick := StyleSheetClassComboBoxClick;
  FStyleSheetsListBox.OnClick := StyleSheetsListBoxClick;
  if FPredefinedStyleSheetsList <> nil then
    for I := 0 to FPredefinedStyleSheetsList.Count - 1 do
      FList.Add(TcxPredefinedStyleSheetsClass(FPredefinedStyleSheetsList[I]).Create);
end;

destructor TcxPredefinedStyleSheetsList.Destroy;
var
  I: Integer;
begin
  if FPreview <> nil then
    FPreview.Free;
  for I := 0 to FList.Count - 1 do
    TcxPredefinedStyleSheets(FList[I]).Free;
  FList.Free;
  inherited Destroy;
end;

function TcxPredefinedStyleSheetsList.CurrentStyleSheet: TcxCustomStyleSheet;
begin
  if FStyleSheetsListBox.ItemIndex < 0 then
    Result := nil
  else Result :=  TcxCustomStyleSheet(FStyleSheetsListBox.Items.Objects[FStyleSheetsListBox.ItemIndex]);
end;

function TcxPredefinedStyleSheetsList.CurrentStyleSheetClass: TcxCustomStyleSheetClass;
begin
  if FStyleSheetClassComboBox.ItemIndex > - 1 then
    Result := TcxCustomStyleSheetClass(FStyleSheetClassComboBox.Items.Objects[FStyleSheetClassComboBox.ItemIndex])
  else Result := nil;
end;

procedure TcxPredefinedStyleSheetsList.FillListBox;
var
  I: Integer;
  AList: TList;
  AForm: TfrmcxStyleSheetsLoad;
begin
  AForm := TfrmcxStyleSheetsLoad(GetParentForm(FStyleSheetsListBox));
  if CurrentStyleSheetClass = nil then
  begin
    FStyleSheetsListBox.Items.Clear;
    AForm.Width := AForm.pnlStyles.Width;
  end else
  begin
    AList := TList.Create;
    try
      for I := 0 to FList.Count - 1 do
        TcxPredefinedStyleSheets(FList[I]).GetStyleSheetsByClass(CurrentStyleSheetClass, AList);
      FStyleSheetsListBox.Items.BeginUpdate;
      try
        FStyleSheetsListBox.Items.Clear;
        for I := 0 to AList.Count - 1 do
          FStyleSheetsListBox.Items.AddObject(TcxCustomStyleSheet(AList[I]).Caption,
                  TcxCustomStyleSheet(AList[I]));
      finally
        FStyleSheetsListBox.Items.EndUpdate
      end;
    finally
      AList.Free;
    end;
  end;
  if (CurrentStyleSheetClass = nil) or
    (GetPreviewByStyleSheetClass(CurrentStyleSheetClass) = nil) then
  begin
    if FPreview <> nil then
    begin
      FPreview.Free;
      FPreview := nil;
      AForm.SetShowPreview(False);
    end;
  end else
  begin
    FPreview := GetPreviewByStyleSheetClass(CurrentStyleSheetClass).Create(AForm);
    AForm.SetShowPreview(True);
    FPreview.Control.Parent := AForm.pnlPreviewClient;
    FPreview.Control.Align := alClient;
    FPreview.SetStyleSheet(nil);
    if AForm.pnlPreviewClient.Width < FPreview.GetSize.X then
      AForm.Width := AForm.Width + FPreview.GetSize.X - AForm.pnlPreviewClient.Width;
    if AForm.pnlPreviewClient.Height < FPreview.GetSize.Y then
      AForm.Height := AForm.Height + FPreview.GetSize.Y - AForm.pnlPreviewClient.Height;
  end;
  UpdateButton;
end;

procedure TcxPredefinedStyleSheetsList.UpdateButton;
begin
  FLoadButton.Enabled := FStyleSheetsListBox.SelCount > 0;
end;

procedure TcxPredefinedStyleSheetsList.StyleSheetClassComboBoxClick(Sender: TObject);
begin
  FillListBox;
end;

procedure TcxPredefinedStyleSheetsList.StyleSheetsListBoxClick(Sender: TObject);
begin
  UpdateButton;
  if FPreview <> nil then
  begin
    FPreview.SetStyleSheet(CurrentStyleSheet);
  end;
end;

procedure ShowLoadStyleSheetsFromIniFile(const AIniFileName: string;
                AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);

  procedure SelectAllItems(AForm: TfrmcxStyleSheetsLoad);
    {$IFDEF VCL}
    {$IFDEF DELPHI6}
    {$ELSE}
  var
    I: Integer;
    {$ENDIF}
    {$ELSE}
  var
    I: Integer;
    {$ENDIF}
  begin
    {$IFDEF VCL}
    {$IFDEF DELPHI6}
    AForm.lbStyleSheets.SelectAll;
    {$ELSE}
    for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
      AForm.lbStyleSheets.Selected[I] := True;
    {$ENDIF}
    {$ELSE}
    for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
      AForm.lbStyleSheets.Selected[I] := True;
    {$ENDIF}
  end;

var
  AForm: TfrmcxStyleSheetsLoad;
  I: Integer;
  AIniFile: TIniFile;
  AStrings: TStringList;
begin
  AIniFile := TIniFile.Create(AIniFileName);
  AForm := TfrmcxStyleSheetsLoad.Create(nil);
  AForm.SetShowPreview(False);
  AStrings := TStringList.Create;
  try
    AIniFile.ReadSections(AForm.lbStyleSheets.Items);
    SelectAllItems(AForm);
    AForm.btnLoad.Enabled := (AForm.CurrentStyleSheetClass <> nil) and
        (AForm.lbStyleSheets.Items.Count > 0);
    AForm.ShowModal;
    if AForm.ModalResult = mrOk then
    begin
      for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
         if AForm.lbStyleSheets.Selected[I] then
           AStrings.Add(AForm.lbStyleSheets.Items[I]);
      LoadStyleSheetsFromIniFile(AIniFileName, AStyleRepository,
       AForm.CurrentStyleSheetClass,
       AStrings, AOwner, AStyleSheetList, AStyleGetName);
    end;
  finally
    AStrings.Free;
    AForm.Free;
    AIniFile.Free;
  end;
end;

procedure CreateStyleSheetByPredefine(AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName; ASource: TcxCustomStyleSheet);
var
  AStyleSheet: TcxCustomStyleSheet;
begin
  AStyleSheet := AStyleRepository.CreateStyleSheetEx(TcxCustomStyleSheetClass(ASource.ClassType),
        AOwner);
  if AStyleSheetList <> nil then
    AStyleSheetList.Add(AStyleSheet);
  if AOwner.FindComponent(ASource.Name) = nil then
    AStyleSheet.Name := ASource.Name;
  AStyleSheet.Caption := ASource.Caption;
  CreateStyleSheetStyles(AStyleSheet, ASource, AStyleGetName);
end;

procedure ShowLoadStyleSheetsFromPreDefineStyles(AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);
var
  AForm: TfrmcxStyleSheetsLoad;
  APredefinedList: TcxPredefinedStyleSheetsList;
  I: Integer;
begin
  AForm := TfrmcxStyleSheetsLoad.Create(nil);
  APredefinedList := TcxPredefinedStyleSheetsList.Create(
      AForm.cbStyleSheetClasses, AForm.lbStyleSheets, AForm.btnLoad);
  try
    APredefinedList.FillListBox;
    AForm.ShowModal;
    if AForm.ModalResult = mrOk then
    begin
      for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
         if AForm.lbStyleSheets.Selected[I] then
           CreateStyleSheetByPredefine(AStyleRepository,
             AOwner, AStyleSheetList, AStyleGetName,
             TcxCustomStyleSheet(AForm.lbStyleSheets.Items.Objects[I]));
    end;
  finally
    APredefinedList.Free;
    AForm.Free;
  end;
end;

{ TfrmcxStyleSheetsLoad }
constructor TfrmcxStyleSheetsLoad.Create(AOwner: TComponent);
var
  I: Integer;
  AList: TList;
begin
  inherited Create(AOwner);
  AList := TList.Create;
  try
    GetRegisteredStyleSheetClasses(AList);
    for I := 0 to AList.Count - 1 do
      cbStyleSheetClasses.Items.AddObject(TcxCustomStyleSheetClass(AList[I]).ClassName, TObject(TcxCustomStyleSheetClass(AList[I])));
    if AList.Count > 0 then
      cbStyleSheetClasses.ItemIndex := 0;
  finally
    AList.Free;
  end;
end;

function TfrmcxStyleSheetsLoad.CurrentStyleSheetClass: TcxCustomStyleSheetClass;
begin
  if cbStyleSheetClasses.ItemIndex > - 1 then
    Result := TcxCustomStyleSheetClass(cbStyleSheetClasses.Items.Objects[cbStyleSheetClasses.ItemIndex])
  else Result := nil;
end;

procedure TfrmcxStyleSheetsLoad.SetShowPreview(Value: Boolean);
begin
  pnlPreview.Visible := Value;
  if not Value then
    Width := pnlStyles.Width + lbStyleSheets.Left * 2;
end;



end.
