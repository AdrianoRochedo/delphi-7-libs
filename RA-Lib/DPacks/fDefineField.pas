{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fDefineField;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Db, TypInfo, DsgnIntf;

type
  TDefineField = class(TForm)
    LookupGroup: TGroupBox;
    DatasetLabel: TLabel;
    KeyFieldsLabel: TLabel;
    LookupKeysLabel: TLabel;
    ResultFieldLabel: TLabel;
    DatasetList: TComboBox;
    KeyFieldsList: TComboBox;
    LookupKeysList: TComboBox;
    ResultFieldList: TComboBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    FieldGroup: TGroupBox;
    ComponentNameLabel: TLabel;
    FieldNameLabel: TLabel;
    FieldTypeLabel: TLabel;
    FieldSizeLabel: TLabel;
    ComponentNameEdit: TEdit;
    FieldNameEdit: TEdit;
    FieldTypeList: TComboBox;
    SizeEdit: TEdit;
    FieldKind: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FieldNameEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FieldKindClick(Sender: TObject);
    procedure DatasetListChange(Sender: TObject);
  private
    DataSet: TDataSet;
    LookupDataSet: TDataSet;
    {$IFDEF RA_D3}
    Designer: TFormDesigner;
    {$ELSE}
    Designer: IFormDesigner;
    {$ENDIF}
    procedure EnumDataSet(const S: string);
  end;

  function DefineField(ADataSet: TDataSet): Boolean;

implementation

uses packconst;

{$R *.dfm}

var
  FieldTypes: TList;

function DefineField(ADataSet: TDataSet): Boolean;
begin
  with TDefineField.Create(Application) do
    try
      DataSet := ADataSet;
     {$IFDEF RA_D3}
      Designer := (DataSet.Owner as TForm).Designer as TFormDesigner;
     {$ELSE}
      Designer := (DataSet.Owner as TForm).Designer as IFormDesigner;
     {$ENDIF}
      Result := ShowModal = mrOk;
    finally
      Free;
    end;    { try/finally }
end;    {  }

procedure TDefineField.FormCreate(Sender: TObject);
begin
  Caption := SDENewFieldCaption;
  OkBtn.Caption := SOk;
  CancelBtn.Caption := SCancel;
  FieldGroup          .Caption := SDEFieldProperties   ;
  FieldNameLabel      .Caption := SDEFieldNameLabel    ;
  FieldTypeLabel      .Caption := SDEFieldTypeLabel    ;
  ComponentNameLabel  .Caption := SDEComponentNameLabel;
  FieldSizeLabel      .Caption := SDEFieldSizeLabel    ;
  FieldKind           .Caption := SDEFieldKind         ;
  LookupGroup         .Caption := SDELookupGroup       ;
  KeyFieldsLabel      .Caption := SDEKeyFieldsLabel    ;
  DatasetLabel        .Caption := SDEDatasetLabel      ;
  LookupKeysLabel     .Caption := SDELookupKeysLabel   ;
  ResultFieldLabel    .Caption := SDEResultFieldLabel  ;
  FieldKind           .Items.Text := SDEFieldKindItems;
end;

procedure TDefineField.FormShow(Sender: TObject);
var
  S: string;
  P: Integer;
  i: Integer;
begin
  for i := 0 to FieldTypes.Count - 1 do
  begin
    S := TFieldClass(FieldTypes[i]).ClassName;
    if S[1] = 'T' then
      Delete(S, 1, 1);
    P := Pos('Field', S);
    if P > 0 then
      Delete(S, P, Length(S));
    FieldTypeList.Items.Add(S);
  end;
  DatasetList.Clear;
  Designer.GetComponentNames(GetTypeData(TDataSet.ClassInfo), EnumDataSet);
  DataSet.GetFieldNames(KeyFieldsList.Items);
end;

procedure TDefineField.EnumDataSet(const S: string);
begin
  DatasetList.Items.Add(S);
end;

procedure RARegisterFieldsProc(const FieldClassess: array of TFieldClass);
var
  i: Integer;
begin
  for i := Low(FieldClassess) to High(FieldClassess) do
  begin
    FieldTypes.Add(FieldClassess[i]);
    RegisterClasses([FieldClassess[i]]);
  end;    { for }
end;

procedure TDefineField.FieldNameEditChange(Sender: TObject);
begin
  ComponentNameEdit.Text := DataSet.Name + FieldNameEdit.Text;
end;

procedure TDefineField.OkBtnClick(Sender: TObject);
var
  FI: Integer;
  Field: TField;
begin
  FI := FieldTypeList.Items.IndexOf(FieldTypeList.Text);
  if FI < 0 then
  begin
    FieldTypeList.SetFocus;
    raise Exception.Create(SDEFieldTypeMustBeSpecified);
  end;
  Field := TFieldClass(FieldTypes[FI]).Create(DataSet.Owner);
  try
    Field.Name := ComponentNameEdit.Text;
    Field.FieldName := FieldNameEdit.Text;
    if SizeEdit.Text <> '' then
      Field.Size := StrToInt(SizeEdit.Text);
    Field.FieldKind := TFieldKind(FieldKind.ItemIndex);
    if Field.FieldKind = fkLookup then
    begin
      Field.LookupDataSet := LookupDataSet;
      Field.KeyFields := KeyFieldsList.Text;
      Field.LookupKeyFields := LookupKeysList.Text;
      Field.LookupResultField := ResultFieldList.Text;
    end;
    Field.DataSet := DataSet;
  except
    Field.Free;
    raise;
  end;
  ModalResult := mrOk;
end;

procedure TDefineField.FieldKindClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to LookupGroup.ControlCount - 1 do
    LookupGroup.Controls[i].Enabled := FieldKind.ItemIndex = 2;
end;

procedure TDefineField.DatasetListChange(Sender: TObject);
begin
  LookupDataSet := Designer.GetComponent(DatasetList.Text) as TDataSet;
  LookupKeysList.Clear;
  ResultFieldList.Clear;
  if LookupDataSet <> nil then
  begin
    LookupDataSet.GetFieldNames(LookupKeysList.Items);
    ResultFieldList.Items := LookupKeysList.Items;
  end;
end;

initialization
  FieldTypes := TList.Create;
  RegisterFieldsProc := RARegisterFieldsProc;
finalization
  FieldTypes.Free;
end.
