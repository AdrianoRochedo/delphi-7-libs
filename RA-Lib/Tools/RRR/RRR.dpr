program RRR;

uses
  Windows, SysUtils, Classes;

{$R *.RES}
{$APPTYPE CONSOLE}

const
  ReplaceChar = '%';

function RemoveFromFile(const FileName: string): Integer;
var
  SS: TStringList;
  S: string;
  i: Integer;
begin
  SS := TStringList.Create;
  Result := 0;
  try
    SS.LoadFromFile(FileName);
    S := SS.Text;
    for i := 1 to Length(S) do
      if Ord(S[i]) in [192..255, 168, 184] then
      begin
        S[i] := ReplaceChar;
        inc(Result);
      end;
    if Result > 0 then
    begin
      SS.Text := S;
      SS.SaveToFile(FileName);
    end;
  finally
    SS.Free;
  end;
end;

procedure RemoveFromFolder(const Folder: string);
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  DosError := FindFirst(Folder + '\*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if ((SearchRec.Attr and faDirectory) = faDirectory) then
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
         (LowerCase(SearchRec.Name) <> 'russian') then
        RemoveFromFolder(Folder + '\' + SearchRec.Name)
    end
    else if ExtractFileExt(ANSILowerCase(SearchRec.Name)) = '.pas' then
    begin
      Writeln(Folder + '\' + SearchRec.Name + ' .... ' +
        IntToStr(RemoveFromFile(Folder + '\' + SearchRec.Name)) + ' chars removed');
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

begin
  Writeln('This program convert russian chars from all pas-files to symbol ''%''');
  Writeln('All files into current folder (and subfolders) will be processed');
  Writeln('Use: put this program into RALib folder and start:');
  Writeln('   ', ExtractFileName(ParamStr(0)), ' /r');
  if (ParamCount < 1) then exit;
  if ParamStr(1) = '/r' then
    RemoveFromFolder(ExtractFilePath(ParamStr(0)));
end.
