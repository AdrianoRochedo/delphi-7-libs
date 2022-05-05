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

unit fAddFields;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DsgnIntf;

type
  TAddFields = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    FieldsList: TListBox;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
  private
    DataSet: TDataSet;
    procedure UpdateList;
    procedure AddFields;
  end;

  function AddFields(ADataSet: TDataSet): Boolean;


implementation

uses packconst;

{$R *.dfm}

function AddFields(ADataSet: TDataSet): Boolean;
begin
  with TAddFields.Create(Application) do
    try
      DataSet := ADataSet;
      UpdateList;
      Result := ShowModal = mrOk;
      if Result then
        AddFields;
    finally
      Free;
    end;    { try/finally }
end;    {  }

procedure TAddFields.FormCreate(Sender: TObject);
begin
  Caption := SDEAddFieldsCaption;
  OkBtn.Caption := SOk;
  CancelBtn.Caption := SCancel;
  GroupBox1.Caption := SDEAvailableFields;
end;

procedure TAddFields.UpdateList;
var
  i: Integer;
  FN: string;
begin
  FieldsList.Clear;
  DataSet.FieldDefs.Update;
  for i := 0 to DataSet.FieldDefs.Count - 1 do
  begin
    FN := DataSet.FieldDefs.Items[i].Name;
    if DataSet.DefaultFields or (DataSet.FindField(FN) = nil) then
    begin
      FieldsList.Items.Add(FN);
      FieldsList.Selected[FieldsList.Items.Count - 1] := True;
    end;
  end;    { for }
end;    { UpdateList }

procedure TAddFields.AddFields;
var
  i: Integer;
  Field: TField;
  FN: string;
begin
  DataSet.Designer.BeginDesign;
  try
    for i := 0 to FieldsList.Items.Count - 1 do
      if FieldsList.Selected[i] then
      begin
        Field := DataSet.FieldDefs.Find(FieldsList.Items[i]).
          CreateField(DataSet.Owner);
        FN := DataSet.Name + Field.FieldName;
        if DataSet.Owner.FindComponent(FN) = nil then
          Field.Name := DataSet.Name + Field.FieldName else
         {$IFDEF RA_D3}
          Field.Name := ((DataSet.Owner as TCustomForm).Designer as TFormDesigner).
            UniqueName(Field.ClassName);
         {$ELSE}
          Field.Name := ((DataSet.Owner as TCustomForm).Designer as IFormDesigner).
            UniqueName(Field.ClassName);
         {$ENDIF}
      end;
  finally
    DataSet.Designer.EndDesign;
  end;
end;    { AddFields }

end.
