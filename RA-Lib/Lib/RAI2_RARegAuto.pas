{***********************************************************
                R&A Library
                   RAI2
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : adapter unit - converts RAI2 calls to delphi calls

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}
{$INCLUDE RA.INC}

unit RAI2_RARegAuto;

interface

uses RAI2;

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);

implementation

uses Classes, RARegAuto;


  { TRegAuto }

{ constructor Create(AOwner: TComponent) }
procedure TRegAuto_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TRegAuto.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Save; }
procedure TRegAuto_Save(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).Save;
end;

{  procedure Load; }
procedure TRegAuto_Load(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).Load;
end;

{  function ReadRootString(const Section, Ident, Default: string): string; }
procedure TRegAuto_ReadRootString(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadRootString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  function ReadRootInteger(const Section, Ident: string; Default: Longint): Longint; }
procedure TRegAuto_ReadRootInteger(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadRootInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteRootString(const Section, Ident, Value: string); }
procedure TRegAuto_WriteRootString(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteRootString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteRootInteger(const Section, Ident: string; Value: Longint); }
procedure TRegAuto_WriteRootInteger(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteRootInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  function ReadString(const Section, Ident, Default: string): string; }
procedure TRegAuto_ReadString(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteString(const Section, Ident, Value: String); }
procedure TRegAuto_WriteString(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  function ReadInteger(const Section, Ident: string; Default: Longint): Longint; }
procedure TRegAuto_ReadInteger(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteInteger(const Section, Ident: string; Value: Longint); }
procedure TRegAuto_WriteInteger(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteInteger(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; }
procedure TRegAuto_ReadBool(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadBool(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteBool(const Section, Ident: string; Value: Boolean); }
procedure TRegAuto_WriteBool(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteBool(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  function ReadFloat(const Section, Ident: string; Default: Double): Double; }
procedure TRegAuto_ReadFloat(var Value: Variant; Args: TArgs);
begin
  Value := TRegAuto(Args.Obj).ReadFloat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure WriteFloat(const Section, Ident: string; Value: Double); }
procedure TRegAuto_WriteFloat(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteFloat(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{  procedure ReadStrings(const Section, Ident: string; Strings: TStrings); }
procedure TRegAuto_ReadStrings(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).ReadStrings(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{  procedure WriteStrings(const Section, Ident: string; Value: TStrings); }
procedure TRegAuto_WriteStrings(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).WriteStrings(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{  procedure ReadSection(const Section: string; Ss: TStrings); }
procedure TRegAuto_ReadSection(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).ReadSection(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{  procedure ReadSections(Ss: TStrings); }
procedure TRegAuto_ReadSections(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).ReadSections(V2O(Args.Values[0]) as TStrings);
end;

{  procedure EraseSection(const Section: string); }
procedure TRegAuto_EraseSection(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).EraseSection(Args.Values[0]);
end;

{  procedure DeleteKey(const Section, Ident: string); }
procedure TRegAuto_DeleteKey(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).DeleteKey(Args.Values[0], Args.Values[1]);
end;

{  procedure ReadWholeSection(const Section: string; Ss: TStrings); }
procedure TRegAuto_ReadWholeSection(var Value: Variant; Args: TArgs);
begin
  TRegAuto(Args.Obj).ReadWholeSection(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;



procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
   { TRegAuto }
    AddClass('RARegAuto', TRegAuto, 'TRegAuto');
    AddGet(TRegAuto, 'Create', TRegAuto_Create, 1, [varEmpty], varEmpty);
    AddGet(TRegAuto, 'Save', TRegAuto_Save, 0, [0], varEmpty);
    AddGet(TRegAuto, 'Load', TRegAuto_Load, 0, [0], varEmpty);
    AddGet(TRegAuto, 'ReadRootString', TRegAuto_ReadRootString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadRootInteger', TRegAuto_ReadRootInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteRootString', TRegAuto_WriteRootString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteRootInteger', TRegAuto_WriteRootInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadString', TRegAuto_ReadString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteString', TRegAuto_WriteString, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadInteger', TRegAuto_ReadInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteInteger', TRegAuto_WriteInteger, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadBool', TRegAuto_ReadBool, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteBool', TRegAuto_WriteBool, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadFloat', TRegAuto_ReadFloat, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteFloat', TRegAuto_WriteFloat, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadStrings', TRegAuto_ReadStrings, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'WriteStrings', TRegAuto_WriteStrings, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadSection', TRegAuto_ReadSection, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadSections', TRegAuto_ReadSections, 1, [varEmpty], varEmpty);
    AddGet(TRegAuto, 'EraseSection', TRegAuto_EraseSection, 1, [varEmpty], varEmpty);
    AddGet(TRegAuto, 'DeleteKey', TRegAuto_DeleteKey, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRegAuto, 'ReadWholeSection', TRegAuto_ReadWholeSection, 2, [varEmpty, varEmpty], varEmpty);
   { ERegAutoError }
    AddClass('RARegAuto', ERegAutoError, 'ERegAutoError');
  end;    { with }
  RegisterClasses([TRegAuto]);
end;    { RegisterRAI2Adapter }

end.
