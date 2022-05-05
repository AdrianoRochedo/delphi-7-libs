{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       component   : none
       description : Small routines

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{ You don't need include this unit into uses list in
  normal delphi application. Use unit RAUtils, that are
  automatically includes this unit. }
{ The one reason to using this unit directly is
  developing non-VCL application, such as small console
  program, so you don't want increase program size
  with 'classes' and 'forms' units. }
{ This unit uses only headered units, such as Windows,
  So including this unit to you uses list will increase
  program size only with really used functions from this unit. }


{$IFNDEF RAUTILS}

{$INCLUDE RA.INC}

unit RAUtilsW;

{ћодуль простых функций}

{
 This unit does not contains any global objects and does not uses
 unit, which contains those objects.
}

interface

uses
  Windows, SysUtils
  {$IFDEF RA_D3H}, ShlObj {$ENDIF}
  {$IFDEF RA_D6H}, Variants {$ENDIF}
  ;

{
 Do not add to uses clause units Forms, Controls, Graphics, FileCtrl, etc.
}

 {$DEFINE INTERFACE}
{$ENDIF RAUTILS}

{$IFDEF INTERFACE}

{$IFNDEF RA_D4H}
type
  longword = integer;
{$ENDIF}

type
  TTickCount = cardinal;


 {**** string handling routines}

const
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\', '#', '"', '''',
    ':','+','%','*','(',')',';','=','{','}','[',']', '{', '}', '<', '>'];
 {const Separators is used in GetWordOnPos, RAUtils.ReplaceSokr and SubWord}

{$IFDEF RA_D}
type
  TSetOfChar = set of char;
{$ENDIF RA_D}
{$IFDEF RA_B}
type
  TSetOfChar = string;
{$ENDIF RA_B}

  { GetWordOnPos returns word from string, S,
    on the cursor position, P}

  function GetWordOnPos(const S : string; const P : integer) : string;

  { GetWordOnPosEx working like GetWordOnPos function, but
    also returns word position in iBeg, iEnd variables }

  function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;

  { SubStr returns substring from string, S,
    separated with Separator string}

  function SubStr(const S : string; const index : integer; const Separator : string) : string;

  { SubStrEnd same to previous function but index numerated
    from the end of string }

  function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;

  { SubWord returns next word from string, P, and
    offsets pointer to the end of word, P2 }

  function SubWord(P : PChar; var P2 : PChar) : string;

  { NumberByWord returns the text representation of
    the number, N, in normal russian language.
    Was typed from Monitor magazine }

  function NumberByWord(const N : longint): string;


//  function CurrencyByWord(Value : Currency) : string;

  { GetLineByPos returns the Line number, there
    the symbol Pos is pointed.
    Lines separated with #13 symbol }

  function GetLineByPos(const S : string; const Pos : integer) : integer;

  { GetXYByPos is same to previous function, but
    returns X position in line too}

  procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);

  { ReplaceString searches for all substrings, OldPattern,
    in a string, S, and replaces them with NewPattern }

  function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;

  { ConcatSep concatenate S and S2 strings with Separator.
    if S = '', separator don't included }

  function ConcatSep(const S, S2, Separator : string) : string;

  { ConcatLeftSep is same to previous function, but
    strings concatenate right to left }

  function ConcatLeftSep(const S, S2, Separator : string) : string;

  { MinimizeString trunactes long string, S, and appends
    '...' symbols, if length of S is more than MaxLen }

  function MinimizeString(const S : string; const MaxLen : integer) : string;

  { Next 4 function for russian chars transliterating.
    This functions are needed because Oem2Ansi and Ansi2Oem functions
    sometimes works sucks }

  procedure Dos2Win(var S : string);
  procedure Win2Dos(var S : string);
  function Dos2WinRes(const S : string) : string;
  function Win2DosRes(const S : string) : string;
  function Win2Koi(const S : string) : string;

  { Spaces returns string consists on N space chars }

  function Spaces(const N : integer) : string;

  { AddSpaces add spaces to string, S, if it length is smaller than N }

  function AddSpaces(const S : string; const N : integer) : string;

  { function LastDate for russian users only }
  { returns date relative to current date: 'два дн€ назад' }

  function LastDate(const Dat : TDateTime) : string;

  { CurrencyToStr format currency, Cur, using ffCurrency float format}

  function CurrencyToStr(const Cur : currency): string;

  { Cmp compares two strings and returns true if they
    are equal. Case-insensitive.}

  function Cmp(const S1, S2 : string) : boolean;

  { StringCat add S2 string to S1 and returns this string }

  function StringCat(var S1 : string; S2 : string) : string;

  { HasChar returns true, if char, Ch, contains in string, S }

  function HasChar(const Ch : Char; const S : string) : boolean;

  function HasAnyChar(const Chars : string; const S : string) : boolean;

  function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;

  function CountOfChar(const Ch : Char; const S : string) : Integer;

  function DefStr(const S: string; Default: string): string;
  
 {#### string handling routines}


 {**** files routines}

  { GetWinDir returns Windows folder name }

  function GetWinDir : TFileName;

  { GetTempDir returns Windows temporary folder name }

  function GetTempDir : string;

  { GenTempFileName returns temporary file name on
    drive, there FileName is placed }

  function GenTempFileName(FileName : string) : string;

  { GenTempFileNameExt same to previous function, but
    returning filename has given extension, FileExt }

  function GenTempFileNameExt(FileName : string; const FileExt : string) : string;

  { ClearDir clears folder Dir }

  function ClearDir(const Dir : string) : boolean;

  { DeleteDir clears and than delete folder Dir }

  function DeleteDir(const Dir : string) : boolean;

  { FileEquMask returns true if file, FileName,
    is compatible with given dos file mask, Mask }

  function FileEquMask(FileName, Mask : TFileName) : boolean;

  { FileEquMasks returns true if file, FileName,
    is compatible with given Masks.
    Masks must be separated with comma (';') }

  function FileEquMasks(FileName, Masks : TFileName) : boolean;


  procedure DeleteFiles(const Folder : TFileName; const Masks : string);

  { LZFileExpand expand file, FileSource,
    into FileDest. Given file must be compressed, using MS Compress program }

  function LZFileExpand(const FileSource, FileDest : string) : boolean;

  { FileGetInfo fills SearchRec record for specified file attributes}

  function FileGetInfo(FileName : TFileName; var SearchRec : TSearchRec) : boolean;

  { HasSubFolder returns true, if folder APath contains other folders }

  function HasSubFolder(APath : TFileName) : boolean;

  { IsEmptyFolder returns true, if there are no files or
    folders in given folder, APath}

  function IsEmptyFolder(APath : TFileName) : boolean;

  { AddSlash add slash char to Dir parameter, if needed }

  procedure AddSlash(var Dir : TFileName);

  { AddSlash returns string with added slash char to Dir parameter, if needed }

  function AddSlash2(const Dir : TFileName) : string;

  { AddPath returns FileName with Path, if FileName not contain any path }

  function AddPath(const FileName, Path : TFileName) : TFileName;

  function AddPaths(const PathList, Path: string): string;

  function ParentPath(const Path: TFileName): TFileName;

  function FindInPath(const FileName, PathList: string): TFileName;

 {$IFNDEF RA_B1}

  { BrowseForFolder displays Browse For Folder dialog }

  function BrowseForFolder(const Handle : HWnd; const Title : string; var Folder : string) : boolean;

 {$ENDIF RA_B1}

  { DeleteReadOnlyFile clears R/O file attribute and delete file }

  function DeleteReadOnlyFile(const FileName : TFileName) : boolean;

  { HasParam returns true, if program running with
    specified parameter, Param }

  function HasParam(const Param : string) : boolean;

  function HasSwitch(const Param : string) : boolean;
  function Switch(const Param : string) : string;

  { ExePath returns ExtractFilePath(ParamStr(0)) }

  function ExePath : TFileName;

  function CopyDir(const SourceDir, DestDir: TFileName): Boolean;

  function FileTimeToDateTime(const FT: TFileTime): TDateTime;

  function MakeValidFileName(const FileName : TFileName;
    const ReplaceBadChar : Char) : TFileName;

 {#### files routines }


 {**** Graphic routines }

  { TTFontSelected returns true, if True Type font
    is selected in specified device context }

  function TTFontSelected(const DC : HDC) : boolean;

  { TrueInflateRect inflates rect in other
    method, than InflateRect API function }

  function TrueInflateRect(const R : TRect; const I : integer) : TRect;

 {#### Graphic routines }



 {**** Windows routines }

  { SetWindowTop put window to top without recreating window }

  procedure SetWindowTop(const Handle : HWND; const Top : boolean);

 {#### Windows routines }



 {**** other routines }

  { KeyPressed returns true, if Key VK is now pressed }

  function KeyPressed(VK : integer) : boolean;

  { functions Max and Min not need comments }

  function Max(x, y :integer):integer;

  function Min(x, y :integer):integer;

  procedure SwapInt(var Int1, Int2: Integer);

  function IntPower(Base, Exponent : integer) : integer;

  function ChangeTopException(E : TObject): TObject;

  function StrToBool(const S: string): Boolean;

 {#### other routines }



 {$IFNDEF RA_D3H}

{ AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current Windows locale. The return value is the same as for CompareStr. }

  function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
  function AnsiStrIComp(S1, S2: PChar): Integer;

 {$ENDIF}

  function Var2Type(V : Variant; const VarType : integer) : variant;

  function VarToInt(V : Variant) : Integer;

  function VarToFloat(V : Variant) : Double;

 { following functions are not documented
   because they are don't work properly sometimes,
   so don't use them }

  function ReplaceSokr1(S : string; const Word, Frase : string) : string;
  { ReplaceSokr1 is full equal to ReplaceString function
    - only for compatibility - don't use }

  { GetSubStr is full equal to SubStr function
    - only for compatibility - don't use }
  { GetSubStr - устаревша€, используйте SubStr }

  function GetSubStr(const S : string; const index : integer; const Separator : Char) : string;

  function GetParameter : string;
  function GetLongFileName(FileName : string) : string;
  {* from unit FileCtrl}
  function DirectoryExists(const Name: string): Boolean;
  procedure ForceDirectories(Dir: string);
  {# from unit FileCtrl}
  function FileNewExt(const FileName, NewExt : TFileName) : TFileName;
  function GetComputerID : string;

  function GetComputerName: string;

{$ENDIF INTERFACE}

{$IFNDEF RAUTILS}
implementation
 {$DEFINE IMPLEMENTATION}
{$ENDIF RAUTILS}


{$IFDEF IMPLEMENTATION}

function GetLineByPos(const S : string; const Pos : integer) : integer;
var
  i : integer;
begin
  if Length(S) < Pos then Result := -1
  else begin
    i := 1;
    Result := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then inc(Result);
      inc(i);
    end;
  end;
end;

procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
var
  i, iB : integer;
begin
  X := -1; Y := -1; iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then begin
    i := 1;
    Y := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then begin inc(Y); iB := i+1 end;
      inc(i);
    end;
    X := Pos - iB;
  end;
end;

function GetWordOnPos(const S : string; const P : integer) : string;
var
  i, Beg : integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    if S[i] in Separators then break;
  Beg := i + 1;
  for i := P to Length(S) do
    if S[i] in Separators then break;
  if i > Beg then
    Result := Copy(S, Beg, i-Beg) else
    Result := S[P];
end;

function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  iBeg := P;
  if P > 1 then
    if S[P] in Separators then
      if (P < 1) or ((P - 1 > 0) and (S[P-1] in Separators)) then
        inc(iBeg)
      else if not ((P - 1 > 0) and (S[P-1] in Separators)) then
        dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then break else dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then break else inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg) else
    Result := S[P];
end;

function GetWinDir : TFileName;
var
  WinDir  : array[0..MAX_PATH] of char;
begin
  WinDir[GetWindowsDirectory(WinDir, MAX_PATH)] := #0;
  Result := WinDir;
end;

function GenTempFileName(FileName : string) : string;
var
  TempDir  : array[0..MAX_PATH] of char;
  TempFile : array[0..MAX_PATH] of char;
  STempDir : TFileName;
  Res : integer;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  if FileName <> '' then begin
    if Length(FileName) < 4 then FileName := ExpandFileName(FileName);
    if (Length(FileName) > 4) and (FileName[2] = ':')
        and (StrLen(@TempDir[0]) > 4)
        and (ANSICompareText(TempDir[0], FileName[1]) <> 0)
    then begin
      STempDir := ExtractFilePath(FileName);
      Move(STempDir[1], TempDir, Length(STempDir)+1);
    end;
  end;
  Res := GetTempFileName(
    TempDir, { address of directory name for temporary file}
    '~RA',   { address of filename prefix}
    0,       { number used to create temporary filename}
    TempFile { address of buffer that receives the new filename}
   );
  if Res <> 0 then Result := TempFile else Result := '~R&ATemp.tmp';
  DeleteFile(Result);
end;

function GenTempFileNameExt(FileName : string; const FileExt : string) : string;
begin
  Result := ChangeFileExt(GenTempFileName(FileName), FileExt);
end;

function GetTempDir : string;
var
  TempDir  : array[0..MAX_PATH] of char;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  Result := TempDir;
end;

function ClearDir(const Dir : string) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path : TFileName;
begin
  Result := True;
  Path := Dir;
  AddSlash(Path);
  DosError := FindFirst(Path+'*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory)then
        Result := Result and DeleteDir(Path+SearchRec.Name)
      else
        Result := Result and DeleteFile(Path+SearchRec.Name);
      // if not Result then exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeleteDir(const Dir : string) : boolean;
begin
  ClearDir(Dir);
{  if Dir[Length(Dir)] = '\' then Dir[Length(Dir)] := #0;}
  Result := RemoveDir(Dir);
end;

procedure DeleteFiles(const Folder : TFileName; const Masks : string);
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path : TFileName;
begin
  Path := AddSlash2(Folder);
  DosError := FindFirst(Path + '*.*', faAnyFile and not faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if FileEquMasks(Path + SearchRec.Name, Masks) then
      DeleteFile(Path + SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;


function GetParameter : string;
var
  FN, FN1 : PChar;
begin
  if ParamCount = 0 then begin Result := ''; exit end;
  FN := cmdLine;
  if FN[0] = '"' then begin
    FN := StrScan(FN+1, '"');
    if (FN[0] = #00) or (FN[1] = #00)
    then Result := ''
    else begin
      inc(FN, 2);
      if FN[0] = '"' then begin
        inc(FN, 1);
        FN1 := StrScan(FN+1, '"');
        if FN1[0] <> #00 then FN1[0] := #00;
      end;
      Result := FN;
    end;
  end else Result := Copy(CmdLine, Length(ParamStr(0))+1, 260);{Ќе работает дл€ длинных имен с пробелами}
  while (Length(Result)>0) and (Result[1] = ' ') do Delete(Result, 1, 1);
  Result := ReplaceString(Result, '"', '');
  if FileExists(Result) then
    Result := GetLongFileName(Result);
end;


function GetLongFileName(FileName : string) : string;
var
  SearchRec : TSearchRec;
begin
  if FileGetInfo(FileName, SearchRec) then
    Result := ExtractFilePath(ExpandFileName(FileName))+SearchRec.FindData.cFileName
  else
    Result := FileName;
end;

function FileEquMask(FileName, Mask : TFileName) : boolean;
var
  i : integer;
  C : char;
  P : PChar;
begin
  FileName := ANSIUpperCase(ExtractFileName(FileName));
  Mask := ANSIUpperCase(Mask);
  Result := false;
  if Pos('.', FileName) = 0 then FileName := FileName+'.';
  i := 1; P := PChar(FileName);
  while (i <= length(Mask)) do begin
    C := Mask[i];
    if (P[0] = #0) and (C <> '*') then exit;
    case C of
      '*' :
        if i = length(Mask) then begin
          Result := true;
          exit;
        end else begin
          P := StrScan(P, Mask[i+1]);
          if P = nil then exit;
        end;
      '?' : inc(P);
      else if C = P[0] then inc(P) else exit;
    end;
    inc(i);
  end;
  if P[0] = #0 then Result := true;
end;

function FileEquMasks(FileName, Masks : TFileName) : boolean;
var
  i : integer;
  Mask : string;
begin
  Result := false;
  i := 0;
  Mask := Trim(GetSubStr(Masks, i, ';'));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask) then begin
      Result := true;
      break;
    end else begin
      inc(i);
      Mask := Trim(GetSubStr(Masks, i, ';'));
    end;
end;

function NumberByWord(const N : longint): string;
const
  Ten : array[0..9] of string = ('ноль',  'один',  'два',    'три', 'четыре',
                                 'п€ть', 'шесть', 'семь', 'восемь', 'дев€ть');
  Hun : array[1..9] of string = ('сто', 'двести', 'триста', 'четыреста', 'п€тьсот',
                                 'шестьсот', 'семьсот', 'восемьсот', 'дев€тьсот');
  OnTen : array[10..19] of string = ('дес€ть', 'одиннадцать', 'двенадцать', 'тринадцать',
                                     'четырнадцать', 'п€тнадцать', 'шестнадцать',
                                     'семнадцать', 'восемнадцать', 'дев€тнадцать');
  HunIn : array[2..9] of string = ('двадцать', 'тридцать', 'сорок', 'п€тьдес€т',
                                   'шестьдес€т', 'семьдес€т', 'восемьдес€т', 'дев€носто');

var
  StrVsp  : string;
  NumStr  : string;
  StrVsp2 : string;
  i       : byte;

  function IndNumber(Stri : string; Place : byte) : byte;
  begin
    IndNumber := Ord(Stri[Place]) - 48;
  end;

  function Back(Stri : string) : longint;
  var
    code : integer;
    LI   : longint;
  begin
    Result := 0;
    Val(Stri, LI, code);
    if (code = 0) then Result := LI;
  end;

begin
  NumStr := IntToStr(N);
  if (Length(NumStr) > 9) then begin
    Result := '*****';
    Exit;
  end;
  case Length(NumStr) of
    1 : StrVsp := Ten[N];
    2 : case NumStr[1] of
          '1'      :   StrVsp := OnTen[N];
          '0'      :   StrVsp := NumberByWord(IndNumber(NumStr, 2));
          '2'..'9' : begin
            StrVsp := HunIn[IndNumber(NumStr, 1)];
            if NumStr[2] <> '0' then
              StrVsp := StrVsp + ' ' + NumberByWord(IndNumber(NumStr, 2));
          end;
        end;
    3 : begin
          StrVsp := Hun[IndNumber(NumStr, 1)];
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 2)));
        end;
    4 : begin
          StrVsp := Ten[IndNumber(NumStr, 1)];
          case NumStr[1] of
            '1'      : StrVsp := 'одна тыс€ча';
            '2'      : StrVsp := 'две тыс€чи';
            '3', '4' : StrVsp := StrVsp + ' тыс€чи';
            '5'..'9' : StrVsp := StrVsp + ' тыс€ч';
          end;
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 3)));
        end;
    5 : begin
          StrVsp2 := NumberByWord(Back(Copy(NumStr, 1, 2)));
          i := Pos(' два', StrVsp2);
          if (Pos(' два', StrVsp2) = i) then i := 0;
          if (i <> 0) then StrVsp2[i+3] := 'e';
          i := Pos(' один', StrVsp2);
          if (Pos(' одинн', StrVsp2) = i) then i := 0;
          if (i <> 0) then begin
            StrVsp2[i+3] := 'н';
            StrVsp2[i+4] := 'а';
          end;
          if NumStr[1] <> '1' then case NumStr[2] of
            '1'      : StrVsp := ' тыс€ча ';
            '2'..'4' : StrVsp := ' тыс€чи ';
            '5'..'9' : StrVsp := ' тыс€ч ';
          end else StrVsp := ' тыс€ч ';
          StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 3, 3)));
        end;
    6 : begin
          StrVsp2 :=NumberByWord(Back(Copy(NumStr, 1, 3)));
          i := Pos(' два', StrVsp2);
          if (Pos(' двад', StrVsp2) = i) then i := 0;
          if (i <> 0) then StrVsp2[i+3] := 'е';
          i := Pos(' один', Strvsp2);
          if (Pos(' одинн', StrVsp2) = i) then i := 0;
          if (i <> 0) then begin
            StrVsp2[i+3] := 'н';
            StrVsp2[i+4] := 'а';
          end;
          if NumStr[2] <> '1' then case numStr[3] of
            '1'      : StrVsp := ' тыс€ча ';
            '2'..'4' : StrVsp := ' тыс€чи ';
            '5'..'9' : StrVsp := ' тыс€ч ';
          end else StrVsp := ' тыс€ч ';
          StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 4, 3)));
        end;
    7 : begin
          StrVsp := Ten[IndNumber(NumStr, 1)];
          case NumStr[1] of
            '1'      : StrVsp := 'один миллион';
            '2'..'4' : StrVsp := StrVsp + ' миллиона';
            '5'..'9' : StrVsp := StrVsp + ' миллионов';
          end;
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 6)));
        end;
    8 : begin
          StrVsp := NumberByWord(Back(Copy(NumStr, 1, 2)));
          StrVsp := StrVsp + ' миллион';
          if (NumStr[1] <> '1') then case NumStr[2] of
            '2'..'4'     : StrVsp := StrVsp + 'а';
            '0','5'..'9' : StrVsp := StrVsp + 'ов';
          end else StrVsp := StrVsp + 'ов';
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 3, 6)));
        end;
    9 : begin
          StrVsp := NumberByWord(Back(Copy(Numstr, 1, 3)));
          StrVsp := StrVsp + ' миллион';
          if (NumStr[2] <> '1') then case NumStr[3] of
            '2'..'4'      : StrVsp := StrVsp + 'а';
            '0', '5'..'9' : StrVsp := StrVsp + 'ов';
          end else StrVsp := StrVsp + 'ов';
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 4, 6)));
        end;
  end;
  if ((Length(StrVsp) > 4) and (Copy(StrVsp, Length(StrVsp)-3, 4) = Ten[0])) then
    StrVsp := Copy(StrVsp, 1, Length(StrVsp) - 4);
  Result := StrVsp;
end;

function GetSubStr(const S : string; const index : integer; const Separator : Char) : string;
begin
  Result := SubStr(S, index, Separator);
end;

function SubStr(const S : string; const index : integer; const Separator : string) : string;
 {¬ырезает подстроку. ѕодстроки раздел€ютс€ символом Sep}
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
    if pB[0] = #0 then exit;
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;
 {то же что и SubStr, но подстроки нумеруютс€ с конца}
var
  MaxIndex : integer;
  pB : PChar;
begin
 {неоптимальна€ реализаци€}
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do begin
    inc(MaxIndex);
    pB := StrPos(pB+Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - index, Separator);
end;

function FileGetInfo(FileName : TFileName; var SearchRec : TSearchRec) : boolean;
var
  DosError  : integer;
  Path : TFileName;
begin
  Result := false;
  Path := ExtractFilePath(ExpandFileName(FileName))+'*.*';
  FileName := ANSIUpperCase(ExtractFileName(FileName));
  DosError := FindFirst(Path, faAnyFile, SearchRec);
  while DosError = 0 do begin
    if (ANSICompareText(SearchRec.FindData.cFileName, FileName) = 0)
    or (ANSICompareText(SearchRec.FindData.cAlternateFileName, FileName) = 0)
    then begin
      Result := true;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function HasSubFolder(APath : TFileName) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  Result := false;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do begin
    if (SearchRec.Attr and faDirectory = faDirectory) and (SearchRec.Name[1] <> '.') then begin
      Result := true;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function IsEmptyFolder(APath : TFileName) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  Result := true;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do begin
    if SearchRec.Name[1] <> '.' then begin
      Result := false;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function TTFontSelected(const DC : HDC) : boolean;
var
  TM : TTEXTMETRIC;
begin
  GetTextMetrics(DC, TM);
  Result := TM.tmPitchAndFamily and TMPF_TRUETYPE <> 0;
end;

function SubWord(P : PChar; var P2 : PChar) : string;
var
  i : integer;
begin
  i := 0;
  while not (P[i] in Separators) do inc(i);
  SetString(Result, P, i);
  P2 := P+i;
end;

function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;
var
  LW : integer;
  P : PChar;
  Sm : integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do begin
    Sm := P-PChar(S);
    S := Copy(S, 1, Sm)+NewPattern+Copy(S, Sm+LW+1, Length(S));
    P := StrPos(PChar(S)+Sm+Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function ReplaceSokr1(S : string; const Word, Frase : string) : string;
begin
  Result := ReplaceString(S, Word, Frase);
end;

function ConcatSep(const S, S2, Separator : string) : string;
begin
  Result := S;
  if Result <> '' then Result := Result + Separator;
  Result := Result + S2;
end;

function ConcatLeftSep(const S, S2, Separator : string) : string;
begin
  Result := S;
  if Result <> '' then Result := Separator + Result;
  Result := S2 + Result;
end;

function MinimizeString(const S : string; const MaxLen : integer) : string;
begin
  if Length(S) > MaxLen then
    if MaxLen < 3 then
      Result := Copy(S, 1, MaxLen)
    else
      Result := Copy(S, 1, MaxLen-3) + '...'
  else
    Result := S;
end;

function TrueInflateRect(const R : TRect; const I : integer) : TRect;
begin
  with R do SetRect(Result, Left - I, Top - I, Right + I, Bottom + I);
end;

procedure SetWindowTop(const Handle : HWND; const Top : boolean);
const
  TopFlag : array[boolean] of longword = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, TopFlag[Top], 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

{* from unit FileCtrl}

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := Integer(GetFileAttributes(PChar(Name)));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

procedure ForceDirectories(Dir: string);
begin
  if Dir[Length(Dir)] = '\' then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; { avoid 'xyz:\' problem.}
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

{# from unit FileCtrl}

function LZFileExpand(const FileSource, FileDest : string) : boolean;
type
  TLZCopy     = function (Source, Dest: Integer): Longint; stdcall;
  TLZOpenFile = function (Filename: PChar; var ReOpenBuff: TOFStruct; Style: Word): Integer; stdcall;
  TLZClose    = procedure (hFile: Integer); stdcall;
var
  Source, Dest : integer;
  OSSource, OSDest : TOFSTRUCT;
  Res : integer;
  Ins : integer;
  LZCopy     : TLZCopy;
  LZOpenFile : TLZOpenFile;
  LZClose    : TLZClose;
begin
  Result := false;
  Ins := LoadLibrary('LZ32.dll');
  try
    LZCopy     := GetProcAddress(Ins, 'LZCopy');
    LZOpenFile := GetProcAddress(Ins, 'LZOpenFileA');
    LZClose    := GetProcAddress(Ins, 'LZClose');
    OSSource.cBytes := sizeof(TOFSTRUCT);
    OSDest.cBytes := sizeof(TOFSTRUCT);
    Source := LZOpenFile(
      PChar(FileSource), // address of name of file to be opened
      OSSource, // address of open file structure
      OF_READ or OF_SHARE_DENY_NONE// action to take
     );
    if Source < 0 then begin
      DeleteFile(FileDest);
      Dest := LZOpenFile(
        PChar(FileDest), // address of name of file to be opened
        OSDest, // address of open file structure
        OF_CREATE or OF_WRITE or OF_SHARE_EXCLUSIVE// action to take
       );
      if Dest >= 0 then begin
        Res := LZCopy(Source, Dest);
        if Res >= 0 then Result := true;
      end;
      LZClose(Source);
      LZClose(Dest);
    end;
  finally
    FreeLibrary(Ins);
  end;
end;

procedure Dos2Win(var S : string);
var
  i : integer;
begin
  for i := 1 to Length(S) do
    case S[i] of
      #$80..#$AF : S[i] := char(byte(S[i])+(192-$80));
      #$E0..#$EF : S[i] := char(byte(S[i])+(240-$E0));
    end;
end;

procedure Win2Dos(var S : string);
var
  i : integer;
begin
  for i := 1 to Length(S) do
    case S[i] of
      #$C0..#$EF : S[i] := char(byte(S[i])-(192-$80));
      #$F0..#$FF : S[i] := char(byte(S[i])-(240-$E0));
    end;
end;

function Dos2WinRes(const S : string) : string;
begin
  Result := S;
  Dos2Win(Result);
end;

function Win2DosRes(const S : string) : string;
begin
  Result := S;
  Win2Dos(Result);
end;

function Win2Koi(const S : string) : string;
const
  W = 'абвгдеЄжзийклмнопрстуфхчцшщьыъэю€јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’„÷Ўў№џЏЁёя';
  K = 'Ѕ¬„«ƒ≈£÷Џ… ЋћЌќѕ–“”‘’∆»ё√џЁЎўя№ј—бвчзде≥цъйклмнопртуфхжиюгыэшщ€ьас';
var
  i, j : integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    j := Pos(Result[i], W);
    if j > 0 then
      Result[i] := K[j];
  end;
end;

function Spaces(const N : integer) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to N do Result := Result+' ';
end;

function AddSpaces(const S : string; const N : integer) : string;
begin
  Result := S;
  while Length(Result) < N do Result := Result+' ';
end;

function KeyPressed(VK : integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

{$IFNDEF RA_B1}
function BrowseForFolder(const Handle : HWnd; const Title : string; var Folder : string) : boolean;
{$IFDEF RA_D2}
type
  TSHItemID = packed record           { mkid }
    cb: Word;                         { Size of the ID (including cb itself) }
    abID: array[0..0] of Byte;        { The item ID (variable length) }
  end;
  PItemIDList = ^TItemIDList;
  TItemIDList = packed record         { idl }
     mkid: TSHItemID;
   end;
  TFNBFFCallBack = function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  TBrowseInfo = packed record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PAnsiChar;  { Return display name of item selected. }
    lpszTitle: PAnsiChar;      { text to go in the banner over the tree. }
    ulFlags: UINT;           { Flags that control the return stuff }
    lpfn: TFNBFFCallBack;
    lParam: LPARAM;          { extra info that's passed back in callbacks }
    iImage: Integer;         { output var: where to return the Image index. }
  end;
  function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall; external 'shell32.dll' name 'SHBrowseForFolderA';
  function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall external 'shell32.dll' name 'SHGetPathFromIDListA';
{$ENDIF}
var
  browseinfo : TBrowseinfo;
  Id : PItemIDList;
  FN : array[0..MAX_PATH] of char;
begin
  with browseinfo do begin
    hwndOwner := Handle;
    pidlRoot  := nil;
    pszDisplayName := FN;
    lpszTitle := PChar(Title);
    ulFlags := 0;
    lpfn := nil;
  end;
  Id := SHBrowseForFolder(browseinfo);
  Result := Id <> nil;
  if Result then begin
    SHGetPathFromIDList(Id, FN);
    Folder := FN;
  end;
end;
{$ENDIF RA_B1}

function LastDate(const Dat : TDateTime) : string;
const
  D2D : array [0..9] of 1..3 = (3, 1, 2, 2, 2, 3, 3, 3, 3, 3);
  Day   : array [1..3] of string = ('день', 'дн€', 'дней');
  Month : array [1..3] of string = ('мес€ц', 'мес€ца', 'мес€цев');
  Year  : array [1..3] of string = ('год', 'года', 'лет');
  Week  : array [1..4] of string = ('неделю', '2 недели', '3 недели', 'мес€ц');
var
  Y, M, D : integer;
begin
  if Date = Dat then Result := 'сегодн€'
  else if Dat = Date - 1 then Result := 'вчера'
  else if Dat = Date - 2 then Result := 'позавчера'
  else if Dat > Date then Result := 'в будущем'
  else begin
    D := Trunc(Date - Dat);
    Y := Round(D / 365);
    M := Round(D / 30);
    if Y > 0 then
      Result := IntToStr(Y)+' '+Year[D2D[StrToInt(IntToStr(Y)[Length(IntToStr(Y))])]]+' назад'
    else if M > 0 then
      Result := IntToStr(M)+' '+Month[D2D[StrToInt(IntToStr(M)[Length(IntToStr(M))])]]+' назад'
    else if D > 6 then
      Result := Week[D div 7]+' назад'
    else if D > 0 then
      Result := IntToStr(D)+' '+Day[D2D[StrToInt(IntToStr(D)[Length(IntToStr(D))])]]+' назад'
  end;
end;

procedure AddSlash(var Dir : TFileName);
begin
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Dir := Dir +'\';
end;

function AddSlash2(const Dir : TFileName) : string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir +'\';
end;

function AddPath(const FileName, Path : TFileName) : TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
  else
    Result := FileName;
end;

function AddPaths(const PathList, Path: string): string;
var
  i: Integer;
  S: string;
begin
  Result := '';
  i := 0;
  S := SubStr(PathList, i, ';');
  while S <> '' do
  begin
    Result := ConcatSep(Result, AddPath(S, Path), ';');
    inc(i);
    S := SubStr(PathList, i, ';');
  end;
end;

function ParentPath(const Path: TFileName): TFileName;
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[Length(Result)] = '\') then
    Delete(Result, Length(Result), 1);
  Result := ExtractFilePath(Result);
end;

function FindInPath(const FileName, PathList: string): TFileName;
var
  i: Integer;
  S: string;
begin
  i := 0;
  S := SubStr(PathList, i, ';');
  while S <> '' do                  
  begin
    Result := AddSlash2(S) + FileName;
    if FileExists(Result) then
      Exit;
    inc(i);
    S := SubStr(PathList, i, ';');
  end;
  Result := '';
end;

function GetComputerID : string;
var
  SN  : DWORD;
  Nul : DWORD;
  WinDir  : array[0..MAX_PATH] of char;
begin
  GetWindowsDirectory(WinDir, MAX_PATH);
  WinDir[3] := #0;
  if GetVolumeInformation(
    WinDir,   // address of root directory of the file system
    nil,   // address of name of the volume
    0,     // length of lpVolumeNameBuffer
    @SN,    // address of volume serial number
    Nul,   // address of system's maximum filename length
    Nul,   // address of file system flags
    nil,   // address of name of file system
    0      // length of lpFileSystemNameBuffer
   )
  then
    Result := IntToHex(SN, 8)
  else
    Result := 'None';
end;

function CurrencyToStr(const Cur : currency): string;
begin
  Result := CurrToStrF(Cur, ffCurrency, CurrencyDecimals)
end;

function Cmp(const S1, S2 : string) : boolean;
begin
  //Result := ANSICompareText(S1, S2) = 0;
  Result := ANSIStrIComp(PChar(S1), PChar(S2)) = 0;
end;

function StringCat(var S1 : string; S2 : string) : string;
begin
  S1 := S1 + S2;
  Result := S1;
end;

function HasChar(const Ch : Char; const S : string) : boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function HasAnyChar(const Chars : string; const S : string) : boolean;
var
  i : integer;
begin
  for i := 1 to Length(Chars) do
    if HasChar(Chars[i], S) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function CountOfChar(const Ch : Char; const S : string) : Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = Ch then
      inc(Result);
end;

function Max(x,y:integer):integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x,y:integer):integer;
begin
  if x < y then Result := x else Result := y;
end;

procedure SwapInt(var Int1, Int2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Int1;
  Int1 := Int2;
  Int2 := Tmp;
end;

function DeleteReadOnlyFile(const FileName : TFileName) : boolean;
begin
  FileSetAttr(FileName, 0); {clear Read Only Flag}
  Result := DeleteFile(FileName);
end;

function HasParam(const Param : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to ParamCount do begin
    Result := Cmp(ParamStr(i), Param);
    if Result then exit;
  end;
end;

function HasSwitch(const Param : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to ParamCount do
    if HasChar(ParamStr(i)[1], '-/') then
    begin
      Result := Cmp(Copy(ParamStr(i), 2, Length(Param)), Param);
      if Result then exit;
    end;
end;

function Switch(const Param : string) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    if HasChar(ParamStr(i)[1], '-/\') and
       Cmp(Copy(ParamStr(i), 2, Length(Param)), Param) then
    begin
      Result := Copy(ParamStr(i), 2 + Length(Param), 260);
      exit;
    end;
end;

function ExePath : TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function FileNewExt(const FileName, NewExt : TFileName) : TFileName;
begin
  Result := Copy(FileName, 1, Length(FileName) - Length(ExtractFileExt(FileName))) + NewExt;
end;

{$IFNDEF RA_D3H}
function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;
{$ENDIF}


function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;
begin
{$IFDEF RA_D}
  Result := Ch in SetOfChar;
{$ENDIF RA_D}
{$IFDEF RA_B}
  Result := Pos(Ch, SetOfChar) > 0;
{$ENDIF RA_B}
end;

function IntPower(Base, Exponent : integer) : integer;
begin
  if Exponent > 0 then
  begin
    Result := Base;
    dec(Exponent);
    while Exponent > 0 do
    begin
      Result := Result * Base;
      dec(Exponent);
    end;
  end else
  if Exponent < 0 then
  begin
    Result := 1;
    inc(Exponent);
    while Exponent < 0 do
    begin
      Result := Result div Base;
      inc(Exponent);
    end;
  end else
    Result := Base;
end;

function ChangeTopException(E : TObject): TObject;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
begin
  { CBuilder 3 Warning !}
  { if linker error occured with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end else
   Result := nil;
//    raise Exception.Create('Not in exception');
end;

function MakeValidFileName(const FileName : TFileName;
	const ReplaceBadChar : Char) : TFileName;
var
  i : Integer;
begin
  Result := FileName;
  for i := 1 to Length(Result) do
    if HasChar(Result[i], '''":?*\/') then
      Result[i] := ReplaceBadChar;
end;

function Var2Type(V : Variant; const VarType : integer) : variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString,
      varOleStr    : Result := '';
      varInteger,
      varSmallint,
      varByte      : Result := 0;
      varBoolean   : Result := false;
      varSingle,
      varDouble,
      varCurrency,
      varDate      : Result := 0.0;
      varVariant   : Result := Null;
      else Result := VarAsType(V, VarType);
    end;
  end else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
    Result := Integer(V = True);
end;

function VarToInt(V : Variant) : Integer;
begin
   Result := Var2Type(V, varInteger);
end;

function VarToFloat(V : Variant) : Double;
begin
   Result := Var2Type(V, varDouble);
end;

function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path, DestPath : TFileName;
begin
  Result := false;
  if not CreateDir(DestDir) then Exit;
  Path := SourceDir;
  DestPath := AddSlash2(DestDir);
  AddSlash(Path);
  DosError := FindFirst(Path+'*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory) then
        Result := CopyDir(Path + SearchRec.Name, AddSlash2(DestDir) + SearchRec.Name)
      else
        Result := CopyFile(PChar(Path+SearchRec.Name),
          PChar(DestPath+SearchRec.Name), true);
      if not Result then exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := true;
end;

function FileTimeToDateTime(const FT: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  FileDate: Integer;
begin
  FileTimeToLocalFileTime(FT, LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime, LongRec(FileDate).Hi, LongRec(FileDate).Lo);
  Result := FileDateToDateTime(FileDate);
end;

function DefStr(const S: string; Default: string): string;
begin
  if S <> '' then
    Result := S
  else
    Result := Default;
end;

function GetComputerName: string;
var
  nSize: DWORD;
begin
  nSize := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, nSize);
  if Windows.GetComputerName(
     PChar(Result), // address of name buffer
     nSize) then // address of size of name buffer
    SetLength(Result, nSize)
  else
    Result := '';
end;

function StrToBool(const S: string): Boolean;
begin
  Result := (S = '1') or Cmp(S, 'true') or Cmp(S, 'yes');
end;

{$ENDIF IMPLEMENTATION}

{$IFNDEF RAUTILS}
end.
{$ENDIF RAUTILS}



