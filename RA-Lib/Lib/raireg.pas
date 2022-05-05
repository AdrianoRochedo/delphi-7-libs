{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov

       description : Register RAI2 components

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit raireg;

interface

procedure Register;

implementation

uses Windows, SysUtils, Classes, RAI2, RAI2Fm, RAUtils, TypInfo,
  {$IFDEF RA_D6H}
   DesignIntf, DesignEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF RA_D6H}
;

{$R rai.dcr}

{$IFDEF RAI2_INTEGERPROPERTY}
type

  TRAIntegerProperty = class(TIntegerProperty)
  private
    Component : TPersistent;
    RAI2P : TRAI2Program;
    procedure RAI2PGetValue(Sender: TObject; Identifer: string;
      var Value: Variant; Args: TArgs; var Done: Boolean);
  public
    procedure SetValue(const Value : string); override;
   {$IFDEF RAI2_INTEGERPROPERTY_DBLCLICK}
    procedure Edit; override;
   {$ENDIF RAI2_INTEGERPROPERTY_DBLCLICK}
  end;

{$ENDIF RAI2_INTEGERPROPERTY}

procedure Register;
begin
 {RAI2 unit}
  RegisterComponents('R&&A Controls', [TRAI2Program, TRAI2Fm]);
 {$IFDEF RAI2_INTEGERPROPERTY}
  RegisterPropertyEditor(TypeInfo(integer), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(cardinal), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(longint), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(smallint), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(shortint), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(word), TObject, '', TRAIntegerProperty);
  RegisterPropertyEditor(TypeInfo(byte), TObject, '', TRAIntegerProperty);
 {$ENDIF RAI2_INTEGERPROPERTY}
end;

{$IFDEF RAI2_INTEGERPROPERTY}

{ TRAIntegerProperty }

type
  THackRAI2Program = class(TRAI2Program);

procedure TRAIntegerProperty.RAI2PGetValue(Sender: TObject; Identifer: string;
	var Value: Variant; Args: TArgs; var Done: Boolean);
var
  Com: TComponent;
begin
  if (Component is TComponent) and (Args.Obj = nil) then
  begin
		Com := (Component as TComponent).Owner.FindComponent(Identifer);
    if Com <> nil then
    begin
    	Value := O2V(Com);
    	Done := True;
    end else
		begin
			Args.Obj := Component;
			Args.ObjTyp := varObject;
			try
				Done := THackRAI2Program(RAI2P).GetValue(Identifer, Value, Args);
			finally
				Args.Obj := nil;
				Args.ObjTyp := 0;
			end;
		end;
  end;
end;    { GetValue }

procedure TRAIntegerProperty.SetValue(const Value: string);

  function Calc : longint;
  begin
    if RAI2P = nil then
    begin
      RAI2P := TRAI2Program.Create(nil);
      RAI2P.OnGetValue := RAI2PGetValue;
    end;
    RAI2P.Source := Value;
    RAI2P.Run;
    Result := RAI2P.VResult;
  end;

var
  Int : longint;
  PropInf : PPropInfo;
  i : integer;
begin
  try
  { if Value is a simple integer we do not need to run RAI2 }
    Int := StrToInt(Value);
    SetOrdValue(Int);
  except
   { and now we really need do this }
    for i := 0 to PropCount - 1 do
    begin
      Component := GetComponent(i) as TPersistent;
      Int := Calc;
      PropInf := TypInfo.GetPropInfo(Component.ClassInfo, GetName);
      if PropInf <> nil then
        SetOrdProp(Component, PropInf, Int);
      Modified;
    end;
  end;
end;

{$IFDEF RAI2_INTEGERPROPERTY_DBLCLICK}
procedure TRAIntegerProperty.Edit;
var
  Int, P : longint;
  PropInf : PPropInfo;
  i : integer;
  Component : TPersistent;
begin
  for i := 0 to PropCount - 1 do
  begin
    Component := GetComponent(i) as TPersistent;
    PropInf := TypInfo.GetPropInfo(Component.ClassInfo, GetName);
    if PropInf <> nil then
    begin
      Int := GetOrdProp(Component, PropInf);
     {$R-}
      if KeyPressed(VK_CONTROL) then P := 10 else P := 1;
      if KeyPressed(VK_SHIFT) then dec(Int, P) else inc(Int, P);
     {$R+}
      SetOrdProp(Component, PropInf, Int);
      Modified;
    end;
  end;
end;
{$ENDIF RAI2_INTEGERPROPERTY_DBLCLICK}
{$ENDIF RAI2_INTEGERPROPERTY}


end.
