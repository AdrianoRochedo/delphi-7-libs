(* this unit is used for memory leak test *)

unit test2;

function main: string;
begin
  Result := 'ok';
end;

var
  groups: string;
  groupsparam: string;

function process: string;
var
  S: string;
  i: Integer;
begin
  if Params.Count = 0 then
    Result := ReadItem('form')
  else if True{Authorized and (Field('sysadmin') = 'yes')} then
  begin
    try
      if Length(Param('words')) >= 2 then
        Result := Search
      else
        Result := ReadItem('shortwords');
      //Result := ReadItem('ok');
    except
     { on E: Exception do
        Result := ReplaceTag(ReadItem('error'), 'error', E.Message); }
      Result := ReplaceTag(ReadItem('error'), 'error', LastError.Message);
     // raise;
    end;
  end;
  Result := ReplaceTag(Result, 'query', ReadItem('query'));
  Result := ReplaceTag(Result, 'words', Param('words'));
  MakeGroups;
  GroupsParam := GetSelectedGroupsQuery;
end;

function Search: string;
var
  findcount: Integer;
  i: Integer;
  Words: string;
  tFind: string;
  S, S1, Si: string;

  Search: Variant;
  Item: Variant;
  Strings: TStrings;
  title: string;
  TimeFormat: string;
  NoName: string;
  Pager: string;
  PageSkip, PageSize: Integer;
  Sz: Integer;
  Size: string;
  UBoard: Integer;
  BD: string;
  url, gourl: string;
  p: Integer;
  c: string;
  SelectedGroups: string;
begin
  Words := AnsiUpperCase(Param('words'));
  TimeFormat := ReadString('program', 'var', 'timeformat', 'dd mmm yyyy  hh:nn:ss');
  NoName := ReadString('program', 'var', 'noname', '��� �����');
  PageSkip := Str2Int(Param('skip'));
  PageSize := Str2Int(DefStr(Param('size'), StrToInt(DefStr(ReadString('pager', 'var', 'size'), '10'))));

  SelectedGroups := GetSelectedGroups;
  if SelectedGroups = '' then
  begin
    Result := ReadItem('groupnotselected');
    Exit;
  end;

  Search := CreateOleObject('FMachine.Search');
  Search.Groups := SelectedGroups;
  Search.Words := Words;
  Search.PageSize := PageSize;
  Search.Start;

  S := '';
  for i := PageSkip to Min(PageSkip + PageSize - 1, Search.Count - 1) do
  begin
    Item := Search.Items(i);

    url := Item.Url;
    p := Pos('uboard=', url);
    if p > 0 then
      c := Copy(url, p + 7, 100);
    p := Pos('&', c);
    if p > 0 then
    begin
      c := Copy(c, 1, p - 1);
      UBoard := StrToInt(c);
    end
    else
      UBoard := 0;

    if UBoard <> 0 then
    begin
      tFind := ReadItem('findboard');
      BD := BoardDescription(UBoard);
      if BD = '' then
        BD := '� ' + IntToStr(UBoard);
      gourl := url + '&words=' + AnsiLowerCase(Words);
    end
    else
    begin
      tFind := ReadItem('findstatic');
      gourl := 'http://www.infa.ru' + url;
      url := 'http://www.infa.ru' + url;
    end;


    findcount := findcount + 1;
    S1 := tFind;
    S1 := ReplaceTag(S1, 'index', i + 1);
    S1 := ReplaceTag(S1, 'title', DefStr(messagetexthighlighted(Item.Title, Words), NoName));
    S1 := ReplaceTag(S1, 'url', url);
    S1 := ReplaceTag(S1, 'gourl', gourl);
    S1 := ReplaceTag(S1, 'description', messagetexthighlighted(Item.Description, Words));
    S1 := ReplaceTag(S1, 'boarddescription', BD);
    S1 := ReplaceTag(S1, 'time', FormatDateTime(TimeFormat, Item.Time));
    Size := Item.Size;
    if Size = -1 then
      Sz := '/-/'
    else
      Sz := FormatSize(Size);
    S1 := ReplaceTag(S1, 'size', Sz);

    S := S + S1;
  end;

  findcount := Search.Count;
  if findcount > 0 then
  begin
    Result := ReadItem('found');
    Result := ReplaceTag(Result, 'findcount', IntToStr(findcount));
    Result := ReplaceTag(Result, 'found', S);
    Pager := MakePager(CurrentHtmlTemplate, Request, Response, findcount);
    Result := ReplaceTag(Result, 'pager', Pager);
  end
  else 
    Result := ReadItem('notfound');
  Sleep(5000);
end;

function GetSelectedGroups: string;
var
  P: string;
  i: Integer;
begin
  Result := '';
  for i := 0 to Params.Count - 1 do
  begin
    P := Params[i];
    if SubStr(P, 0, '=') = 'g' then
      Result := ConcatSep(Result, GetDatabaseGroup(StrToInt(SubStr(P, 1, '='))) + '.' + SubStr(P, 1, '='), ',');
  end;
end;

function GetSelectedGroupsQuery: string;
var
  P: string;
  i: Integer;
begin
  Result := '';
  for i := 0 to Params.Count - 1 do
  begin
    P := Params[i];
    if SubStr(P, 0, '=') = 'g' then
      Result := ConcatSep(Result, P, '&');
  end;
end;

{ �������� ��������� ����� ������� ������ }
function messagetexthighlighted(Text: string; Words: string): string;
begin
  Result := highlightwords(Text, Words, '<b>', '</b>');
end;

function FormatSize(F: Integer): string;
begin
  if F < 1024 then
    Result := IntToStr(F) + ' ����'
  else if F < 1024 * 1024 then
    Result := FormatFloat('### ### ###.0', F / 1024) + ' �����'
end;

function IsGroupSelected(GroupUni: Integer): string;
begin
  Result := (Params.IndexOf('g=' + IntToStr(GroupUni)) > -1) or (Param('words') = '');
end;

procedure AddGroup(GroupUni: Integer; GroupName: string);
var
  Group: string;
begin
  Group := ReadItem('group');
  Group := ReplaceTag(Group, 'groupuni', IntToStr(GroupUni));
  Group := ReplaceTag(Group, 'groupname', GroupName);
  if IsGroupSelected(GroupUni) then
    Group := ReplaceTag(Group, 'selected', 'selected');
  Groups := Groups + Group;
end;

function GetDatabaseGroup(Group: Integer): string;
begin
  case Group of
    101: Result := 'general';
    else Result := 'mboard';
  end;
end;

procedure MakeGroups;
begin
  AddGroup(101, 'INFA On-Line - ����������� ��������');
  //AddGroup(-1, '������');
  AddGroup(16, '������ - ������� �������������� �������');
  AddGroup(10000, '������ - �������');
  AddGroup(20000, '������ - ��������������� ���������');
  AddGroup(30000, '������ - ������������ �����');
  AddGroup(40000, '������ - ������� �����');
  AddGroup(50000, '������ - �����������, �����');
  AddGroup(60000, '������ - ����� � �����������');
  AddGroup(70000, '������ - ��������');
  AddGroup(80000, '������ - ������������');
  AddGroup(90000, '������ - ���������');
  AddGroup(100000, '������ - ����������');
  AddGroup(110000, '������ - ������� �����������');
  AddGroup(120000, '������ - ������');
  AddGroup(130000, '������ - ����������');
  AddGroup(140000, '������ - ����� ������ ��������');
  AddGroup(150000, '������ - ������ �������');
  AddGroup(160000, '������ - ���, ����������');
  AddGroup(170000, '������ - ������');

 { AddGroup(310000, 'Russia - News');
  AddGroup(320000, 'Russia - Radio');
  AddGroup(330000, 'Russia - TV');
  AddGroup(340000, 'Russia - Magazine');
  AddGroup(350000, 'Russia - Electronic magazine'); }

  //AddGroup(-2, '���');
  AddGroup(17, '��� - ������� �������������� �������');
  AddGroup(1000000, '��� - ���-������');
  AddGroup(1001000, '��� - �����');
  AddGroup(1002000, '��� - �������');
  AddGroup(1003000, '��� - ������');
  AddGroup(1004000, '��� - �����������');
  AddGroup(1005000, '��� - ��������');
  AddGroup(1006000, '��� - �����');
  AddGroup(1007000, '��� - ����������');
  AddGroup(1008000, '��� - ����������');
  AddGroup(1009000, '��� - ���������');
  AddGroup(1010000, '��� - ������� �����');
  AddGroup(1011000, '��� - �����������');
  AddGroup(1012000, '��� - ������');
  AddGroup(1013000, '��� - ����������� ������');
  AddGroup(1014000, '��� - ����������');
  AddGroup(1015000, '��� - ����������');
  AddGroup(1016000, '��� - ������');

  if Param('x-files') <> '' then
    AddGroup(101, '��������� ��������� (���������)');
end;

end.
