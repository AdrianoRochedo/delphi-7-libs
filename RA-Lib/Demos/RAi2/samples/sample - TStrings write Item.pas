var
  SS: TStringList;
  S: string;
begin
  SS := TStringList.Create;
  //SS.Add('Line 0');
  //SS.Add('Line 1');
  SS.Text := 'Line 0 ' + #13 + 'Line 1';
  //SS.Strings[0] := '??????';
  SS.Strings[0] := '??????';
  S := SS[1];
  Result := '! ' + S[2] + ' !';
  SS.Free;
end;
