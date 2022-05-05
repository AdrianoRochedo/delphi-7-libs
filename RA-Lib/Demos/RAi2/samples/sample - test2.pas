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
  NoName := ReadString('program', 'var', 'noname', 'Без Имени');
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
        BD := '№ ' + IntToStr(UBoard);
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

{ выделяет найденное слово красным цветом }
function messagetexthighlighted(Text: string; Words: string): string;
begin
  Result := highlightwords(Text, Words, '<b>', '</b>');
end;

function FormatSize(F: Integer): string;
begin
  if F < 1024 then
    Result := IntToStr(F) + ' Байт'
  else if F < 1024 * 1024 then
    Result := FormatFloat('### ### ###.0', F / 1024) + ' КБайт'
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
  AddGroup(101, 'INFA On-Line - статические страницы');
  //AddGroup(-1, 'Россия');
  AddGroup(16, 'Россия - Новости Информационной Системы');
  AddGroup(10000, 'Россия - Новости');
  AddGroup(20000, 'Россия - Государственные структуры');
  AddGroup(30000, 'Россия - Общественная жизнь');
  AddGroup(40000, 'Россия - Деловая жизнь');
  AddGroup(50000, 'Россия - Образование, наука');
  AddGroup(60000, 'Россия - Досуг и развлечение');
  AddGroup(70000, 'Россия - Культура');
  AddGroup(80000, 'Россия - Недвижимость');
  AddGroup(90000, 'Россия - Авторынок');
  AddGroup(100000, 'Россия - Компьютеры');
  AddGroup(110000, 'Россия - Деловое партнерство');
  AddGroup(120000, 'Россия - Работа');
  AddGroup(130000, 'Россия - Знакомства');
  AddGroup(140000, 'Россия - Поиск старых знакомых');
  AddGroup(150000, 'Россия - Аренда квартир');
  AddGroup(160000, 'Россия - СМИ, коррпункты');
  AddGroup(170000, 'Россия - Форумы');

 { AddGroup(310000, 'Russia - News');
  AddGroup(320000, 'Russia - Radio');
  AddGroup(330000, 'Russia - TV');
  AddGroup(340000, 'Russia - Magazine');
  AddGroup(350000, 'Russia - Electronic magazine'); }

  //AddGroup(-2, 'Мир');
  AddGroup(17, 'Мир - Новости Информационной Системы');
  AddGroup(1000000, 'Мир - Веб-камеры');
  AddGroup(1001000, 'Мир - Карты');
  AddGroup(1002000, 'Мир - Новости');
  AddGroup(1003000, 'Мир - Туризм');
  AddGroup(1004000, 'Мир - Справочники');
  AddGroup(1005000, 'Мир - Культура');
  AddGroup(1006000, 'Мир - Наука');
  AddGroup(1007000, 'Мир - Иммиграция');
  AddGroup(1008000, 'Мир - Посольства');
  AddGroup(1009000, 'Мир - Транспорт');
  AddGroup(1010000, 'Мир - Деловая жизнь');
  AddGroup(1011000, 'Мир - Партнерство');
  AddGroup(1012000, 'Мир - Работа');
  AddGroup(1013000, 'Мир - Разыскиваем друзей');
  AddGroup(1014000, 'Мир - Знакомства');
  AddGroup(1015000, 'Мир - Иммиграция');
  AddGroup(1016000, 'Мир - Разное');

  if Param('x-files') <> '' then
    AddGroup(101, 'Служебные материалы (секретные)');
end;

end.
