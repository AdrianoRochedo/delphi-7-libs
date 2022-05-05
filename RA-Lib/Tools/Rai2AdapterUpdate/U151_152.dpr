program U151_152;

uses
  Windows, SysUtils, Classes, RAUtilsW;

{$R *.RES}

var
  i: Integer;
  S: string;

function CmpL(const S1: string): Boolean;
begin
  Result := ANSIStrLIComp(PChar(S), PChar(S1), Length(S1)) = 0;
end;

begin
  Writeln('This program convert RAI2Adapter from version 1.51 to version 1.52');
  Writeln('Use: ', ExtractFileName(ParamStr(0)), ' filename');
  if (ParamCount < 1) or not (FileExists(ParamStr(1))) then exit;
  with TStringList.Create do
    try
      LoadFromFile(ParamStr(1));
      for i := 0 to Count - 1 do
      begin
        S := TrimLeft(Strings[i]);
        if CmpL('AddFun') or CmpL('AddGet') or CmpL('AddIGet') or CmpL('AddIDGet') or
          CmpL('AddIGet') or CmpL('AddIGet') then
          Strings[i] := ReplaceString(Strings[i], ');', ', varEmpty);')
      end;
      SaveToFile(ParamStr(1));
      Writeln('OK')
    finally
      Free;
    end;
end.
