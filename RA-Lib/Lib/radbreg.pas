{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       description : Register db-aware components

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit radbreg;

interface

uses Classes,
  {$IFDEF RA_D6H}
   DesignIntf, DesignEditors, VCLEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF RA_D6H}
  ;

type

 {**************** from Delphi2\Lib\DBReg.pas }
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDataFieldProperty = class(TDBStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TStrings); override;
  end;

  TListFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

  TDatabaseNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{################ from Delphi2\Lib\DBReg.pas }


procedure Register;

implementation

uses TypInfo, DB, DBTables,
  RADBUtil, RADBTreeView, RADBLookupTreeView, RADBMove, RADBCt;

{$R radb.dcr}


procedure Register;
begin
 {RADBCt unit}
  RegisterComponents('R&&A DB', [TRASQLScript, TRADBRadioGroupS]);
 {RADBTreeView unit}
  RegisterComponents('R&&A DB', [TRADBTreeView]);
  RegisterPropertyEditor(TypeInfo(string), TRADBTreeView, 'ItemField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBTreeView, 'MasterField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBTreeView, 'DetailField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBTreeView, 'IconField', TDataFieldProperty);

 {RADBLookupTreeView unit}
  RegisterComponents('R&&A DB', [TRADBLookupTreeView, TRADBLookupTreeViewCombo]);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeViewCombo, 'KeyField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeViewCombo, 'ListField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeViewCombo, 'MasterField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeViewCombo, 'DetailField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeViewCombo, 'IconField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeView, 'KeyField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeView, 'ListField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeView, 'MasterField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeView, 'DetailField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBLookupTreeView, 'IconField', TListFieldProperty);

 { RADBMove unit }
  RegisterComponents('R&&A DB', [TRADBMove]);
  RegisterPropertyEditor(TypeInfo(string), TRADBMove, 'Source', TDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRADBMove, 'Destination', TDatabaseNameProperty);
end;

{**************** from Delphi2\Lib\DBReg.pas }
function TListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;


function TDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

procedure TDatabaseNameProperty.GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;
{################ from Delphi2\Lib\DBReg.pas }

end.
 
