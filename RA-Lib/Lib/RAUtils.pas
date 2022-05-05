{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov, Roman Tkachev

       description : common routines

       authors     : Andrei Prygounkov, Roman Tkachev
       e-mail      : black@infa.ru, whiteman@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAUtils;

interface

{$DEFINE INCLUDE_RAUTILSW}

uses
  Windows, Messages, Forms, Controls, Graphics, SysUtils, Classes,
  StdCtrls, ExtCtrls, Dialogs, Menus {$IFDEF RA_D3H}, ShlObj{$ENDIF},
  Clipbrd,
  {$IFDEF RA_D3H} ActiveX {$ELSE} Ole2 {$ENDIF},
  {$IFDEF RA_D6H} Variants, {$ENDIF}
  TypInfo;

{$DEFINE RAUTILS}
{$DEFINE INTERFACE}
{$IFDEF INCLUDE_RAUTILSW}
  {$INCLUDE RAUtilsW}
{$ENDIF INCLUDE_RAUTILSW}
{$UNDEF RAUTILS}


 { This unit contain a lot of routines. Also see RAUtilsW unit. }

 {**** string routines - строковые функции}

  { ReplaceAllSokr searches for all substrings, Words,
    in a string, S, and replaces them with Frases with the same index.
    Also see RAUtilsW.ReplaceSokr1 function }

  function ReplaceAllSokr(S : string; Words, Frases : TStrings) : string;

  { ReplaceSokr searches the word in a string, S, on PosBeg position,
    in the list, Words, and if founds, replaces this word
    with string from another list, Frases, with the same index,
    and then update NewSelStart variable }

  function ReplaceSokr(S : string; PosBeg, Len : integer; Words, Frases : TStrings; var NewSelStart : integer) : string;

  { CountOfLines calculates the lines count in a string, S,
    each line must be separated from another with #13#10 sequence }

  function CountOfLines(const S : string) : integer;

  { DeleteEmptyLines deletes all empty lines from strings, Ss.
    Lines contained only spaces also deletes. }

  procedure DeleteEmptyLines(Ss : TStrings);

  { SQLAddWhere addes or modifies existing where-statement, where,
    to the strings, SQL.
    Note: If strings SQL allready contains where-statement,
    it must be started on the begining of any line }

  procedure SQLAddWhere(SQL : TStrings; const where : string);

 {#### string routines - строковые функции}


 {**** files routines - файловые функции}

  { ResSaveToFile save resource named as Name with Typ type into file FileName.
    Resource can be compressed using MS Compress program}

  function ResSaveToFile(const Typ, Name : string; const Compressed : boolean; const FileName : string) : boolean;

  function ResSaveToFileEx(Instance: HINST; Typ, Name : PChar;
    const Compressed : boolean; const FileName : string) : boolean;

  function ResSaveToString(Instance: HINST; const Typ, Name : string;
    var S: string) : boolean;

  { Execute executes other program and waiting for it
    terminating, then return it exit code }

  function Execute(const CommandLine, WorkingDirectory : string) : integer;

  { IniReadSection read section, Section, from ini-file,
    IniFileName, into strings, Ss.
    This function reads ALL strings from specified section.
    Note: TIninFile.ReadSection function reads only strings with '=' symbol.}

  function IniReadSection(const IniFileName : TFileName; const Section : string; Ss : TStrings) : boolean;

  { LoadTextFile load text file, FileName, into string }

  function LoadTextFile(const FileName : TFileName): string;

  procedure SaveTextFile(const FileName : TFileName; const Source : string);

  { ReadFolder reads files list from disk folder, Folder,
    that are equal to mask, Mask, into strings, FileList}

  function ReadFolder(const Folder, Mask : TFileName; FileList : TStrings) : integer;

  function ReadFolders(const Folder : TFileName; FolderList : TStrings) : integer;
 {#### files routines - файловые функции}

 {$IFDEF RA_D3H}
  { TargetFileName - if FileName is ShortCut returns filename ShortCut linked to }
  function TargetFileName(const FileName: TFileName): TFileName;

  { return filename ShortCut linked to }
  function ResolveLink(const hwnd: HWND; const LinkFile: TFileName;
    var FileName: TFileName): HRESULT;
 {$ENDIF RA_D3H}

 {**** Graphic routines - графические функции}

  { LoadIcoToImage loads two icons from resource named NameRes,
    into two image lists ALarge and ASmall}

  procedure LoadIcoToImage(ALarge, ASmall : TImageList; const NameRes : string);

  { RATextOut same with TCanvas.TextOut procedure, but
    can clipping drawing with rectangle, RClip. }

  procedure RATextOut(Canvas : TCanvas; const R, RClip : TRect; const S : string);

  { RATextOutEx same with RATextOut function, but
    can calculate needed height for correct output }

  function RATextOutEx(Canvas : TCanvas; const R, RClip : TRect; const S : string; const CalcHeight : boolean) : integer;

  { RATextCalcHeight calculate needed height for
    correct output, using RATextOut or RATextOutEx functions }

  function RATextCalcHeight(Canvas : TCanvas; const R: TRect; const S : string) : integer;

  { Cinema draws some visual effect }

  procedure Cinema(Canvas : TCanvas; rS{Source}, rD{Dest} : TRect);

  { Roughed fills rect with special 3D pattern }

  procedure Roughed(ACanvas : TCanvas; const ARect : TRect; const AVert : boolean);

  { BitmapFromBitmap creates new small bitmap from part
    of source bitmap, SrcBitmap, with specified width and height,
    AWidth, AHeight and placed on a specified index, index in the
    source bitmap }

  function BitmapFromBitmap(SrcBitmap : TBitmap; const AWidth, AHeight, index : integer) : TBitmap;

  { TextWidth calculate text with for writing using standard desktop font }

  function TextWidth(AStr : string) : integer;

  { DefineCursor load cursor from resource, and return
    available cursor number, assigned to it }

  function DefineCursor(Identifer : PChar) : TCursor;

 {#### Graphic routines - графические функции}



 {**** other routines - прочие функции}

  { FindFormByClass returns first form with specified
    class, FormClass, owned by Application global variable }

  function FindFormByClass(FormClass : TFormClass) : TForm;

  function FindFormByClassName(FormClassName : string) : TForm;

  { FindByTag returns the control with specified class,
    ComponentClass, from WinContol.Controls property,
    having Tag property value, equaled to Tag parameter }

  function FindByTag(WinControl : TWinControl; ComponentClass : TComponentClass; const Tag : integer) : TComponent;

  { ControlAtPos2 equal to TWinControl.ControlAtPos function,
    but works better }

  function ControlAtPos2(Parent : TWinControl; X, Y : integer) : TControl;

  { RBTag searches WinControl.Controls for checked
    RadioButton and returns its Tag property value }
   {Возвращает Tag выбранного TRadioButton для заданного Parent}

  function RBTag(Parent : TWinControl) : integer;

  { AppMinimized returns true, if Application is minimized }

  function AppMinimized : boolean;

  { MessageBox is Application.MessageBox with string (not PChar) parameters.
    if Caption parameter = '', it replaced with Application.Title }

  function MessageBox(const Message : string; Caption : string;
    const Flags : integer) : integer;

  function MsgDlg2(const Msg, ACaption : string; DlgType : TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpContext : integer; Control : TWinControl): Integer;

  function MsgDlgDef(const Msg, ACaption : string; DlgType : TMsgDlgType;
    Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; HelpContext : integer;
    Control : TWinControl): Integer;

  { Delay stop program execution to MSec msec }

  procedure Delay(MSec : longword);


  procedure CenterHor(Parent : TControl; MinLeft : integer; Controls : array of TControl);

  procedure EnableControls(Control : TWinControl; const Enable : boolean);

  procedure EnableMenuItems(MenuItem : TMenuItem; const Tag : integer; const Enable : boolean);

  procedure ExpandWidth(Parent : TControl; MinWidth : integer; Controls : array of TControl);

  function PanelBorder(Panel : TCustomPanel) : integer;

  function Pixels(Control : TControl; APixels : integer) : integer;

  procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);

  procedure Error(const Message : string);

  procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean; var PlainItem: string;
    var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }

  function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): string;

  function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): Integer;

  function ItemHtPlain(const Text : string): string;

 { ClearList - clears list of TObject }
  procedure ClearList(List: TList);

 {#### other routines - прочие функции}

  procedure MemStreamToClipBoard(MemStream : TMemoryStream; const Format : word);

  procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format : word);


 { RTTI support }
  function GetPropType(Obj: TObject; const PropName: string): TTypeKind;

  function GetPropStr(Obj: TObject; const PropName: string): string;

  function GetPropOrd(Obj: TObject; const PropName: string): Integer;

  function GetPropMethod(Obj: TObject; const PropName: string): TMethod;


  procedure PrepareIniSection(SS: TStrings);

 { following functions are not documented because
   they are don't work properly, so don't use them }

 {$IFDEF RA_D2}
  function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
 {$ENDIF}

type
  TMenuAnimation = (maNone, maRandom, maUnfold, maSlide);

  procedure ShowMenu(Form : TForm; MenuAni : TMenuAnimation);

type
  TProcObj = procedure of object;

  procedure ExecAfterPause(Proc : TProcObj; Pause : integer);

const
  NoHelp = 0; { for MsgDlg2 }
  MsgDlgCharSet : Integer = DEFAULT_CHARSET;

implementation

uses RACnst
{$DEFINE RAUTILS}
{$DEFINE IMPLEMENTATION}
{$UNDEF INTERFACE}
{$IFDEF INCLUDE_RAUTILSW}
  ;{$INCLUDE RAUtilsW}
{$ELSE}
  ,RAUtilsW;
{$ENDIF INCLUDE_RAUTILSW}
{$UNDEF RAUTILS}


procedure LoadIcoToImage(ALarge, ASmall : TImageList; const NameRes : string);
var
  Ico  : TIcon;
begin
  Ico := TIcon.Create;
  if ALarge <> nil then begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 32, 32, 0);
    ALarge.AddIcon(Ico);
  end;
  if ASmall <> nil then begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 16, 16, 0);
    ASmall.AddIcon(Ico);
  end;
  Ico.Free;
end;

procedure WordBreak(Canvas : TCanvas; const S : string; Ss : TStrings);
begin
  Ss.Text := S;
end;

procedure RATextOut(Canvas : TCanvas; const R, RClip : TRect; const S : string);
begin
  RATextOutEx(Canvas, R, RClip, S, false);
end;

function RATextCalcHeight(Canvas : TCanvas; const R: TRect; const S : string) : integer;
begin
  Result := RATextOutEx(Canvas, R, R, S, true);
end;

function RATextOutEx(Canvas : TCanvas; const R, RClip : TRect; const S : string; const CalcHeight : boolean) : integer;
var
  Ss : TStrings;
  i  : integer;
  H  : integer;
begin
  Ss := TStringList.Create;
  try
    WordBreak(Canvas, S, Ss);
    H := Canvas.TextHeight('A');
    Result := H * Ss.Count;
    if not CalcHeight then
      for i := 0 to Ss.Count-1 do
        ExtTextOut(
          Canvas.Handle, // handle of device context
          R.Left,        // x-coordinate of reference point
          R.Top + H*i,   // y-coordinate of reference point
          ETO_CLIPPED,   // text-output options
          @RClip,        // optional clipping and/or opaquing rectangle
          PChar(Ss[i]),
          Length(Ss[i]), // number of characters in string
          nil            // address of array of intercharacter spacing values
        );
  finally
    Ss.Free;
  end;
end;

procedure Cinema(Canvas : TCanvas; rS, rD : TRect);
const
  Pause = 30; {milliseconds}
  Steps = 7;
  Width = 1;
var
  R : TRect;

  procedure FrameR(R : TRect);
  begin
    with Canvas do begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      LineTo(R.Right, R.Bottom);
      LineTo(R.Right, R.Top);
      LineTo(R.Left, R.Top);
    end;
  end;

  procedure Frame;
  begin
    FrameR(R);
    with Canvas do begin
      MoveTo(rS.Left, rS.Top);
      LineTo(R.Left, R.Top);
      if (R.Top <> rS.Top) then begin
        MoveTo(rS.Right, rS.Top);
        LineTo(R.Right, R.Top);
      end;
      if (R.Left <> rS.Left) then begin
        MoveTo(rS.Left, rS.Bottom);
        LineTo(R.Left, R.Bottom);
      end;
      if (R.Bottom <> rS.Bottom) and (R.Right <> rS.Right) then begin
        MoveTo(rS.Right, rS.Bottom);
        LineTo(R.Right, R.Bottom);
      end;
    end;
  end;
var
  i : integer;
  PenOld : TPen;
begin
  PenOld := TPen.Create;
  PenOld.Assign(Canvas.Pen);
  Canvas.Pen.Mode := pmNot;
  Canvas.Pen.Width := Width;
  Canvas.Pen.Style := psDot;
  FrameR(rS);
  R := rS;
  for i := 1 to Steps do begin
    R.Left   := rS.Left   + (rD.Left   - rS.Left  ) div Steps * i;
    R.Top    := rS.Top    + (rD.Top    - rS.Top   ) div Steps * i;
    R.Bottom := rS.Bottom + (rD.Bottom - rS.Bottom) div Steps * i;
    R.Right  := rS.Right  + (rD.Right  - rS.Right ) div Steps * i;
    Frame;
    Sleep(Pause);
    Frame;
  end;
  FrameR(rS);
  Canvas.Pen.Assign(PenOld);
end;

function FindFormByClass(FormClass : TFormClass) : TForm;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = FormClass.ClassName then
    begin
      Result := Application.Components[i] as TForm;
      Break;
    end;
end;

function FindFormByClassName(FormClassName : string) : TForm;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = FormClassName then
    begin
      Result := Application.Components[i] as TForm;
      Break;
    end;
end;

function FindByTag(WinControl : TWinControl; ComponentClass : TComponentClass; const Tag : integer) : TComponent;
var
  i : integer;
begin
  for i := 0 to WinControl.ControlCount-1 do begin
    Result := WinControl.Controls[i];
    if (Result is ComponentClass) and (Result.Tag = Tag) then exit;
  end;
  Result := nil;
end;

function ControlAtPos2(Parent : TWinControl; X, Y : integer) : TControl;
var
  i : integer;
  P : TPoint;
begin
  for I := Parent.ControlCount - 1 downto 0 do
  begin
    Result := Parent.Controls[I];
    with Result do
    begin
      P := Point(X - Left, Y - Top);
      if PtInRect(ClientRect, P) then Exit;
    end;
  end;
  Result := nil;
end;

function RBTag(Parent : TWinControl) : integer;
var
  RB : TRadioButton;
  i : integer;
begin
  RB := nil;
  with Parent do
    for i := 0 to ControlCount-1 do
      if (Controls[i] is TRadioButton) and
         (Controls[i] as TRadioButton).Checked
      then begin
        RB := Controls[i] as TRadioButton;
        break;
      end;
  if RB <> nil then
    Result := RB.Tag else
    Result := 0;
end;

function IniReadSection(const IniFileName : TFileName; const Section : string; Ss : TStrings) : boolean;
var
  F : integer;
  S : string;
begin
  with TStringList.Create do
  try
    LoadFromFile(IniFileName);
    F := IndexOf('['+Section+']');
    Result := F > -1;
    if Result then begin
      Ss.Clear;
      inc(F);
      while F < Count do begin
        S := Strings[F];
        if (Length(S) > 0) and (Trim(S[1])= '[') then break;
        Ss.Add(S);
        inc(F);
      end;
    end;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName : TFileName; const Source : string);
begin
  with TStringList.Create do
  try
		Text := Source;
		SaveToFile(FileName);
  finally
    Free;
  end;
end;

function LoadTextFile(const FileName : TFileName): string;
begin
  with TStringList.Create do
  try
		LoadFromFile(FileName);
		Result := Text;
  finally
    Free;
  end;
end;

function ReadFolder(const Folder, Mask : TFileName; FileList : TStrings) : integer;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  FileList.Clear;
  Result := FindFirst(AddSlash2(Folder)+Mask, faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do begin
    if not ((SearchRec.Attr and faDirectory) = faDirectory)  then
      FileList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function ReadFolders(const Folder : TFileName; FolderList : TStrings) : integer;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  FolderList.Clear;
  Result := FindFirst(AddSlash2(Folder)+'*.*', faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do begin
    if ((SearchRec.Attr and faDirectory) = faDirectory) and
       (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      FolderList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

{$IFDEF RA_D3H}
function TargetFileName(const FileName: TFileName): TFileName;
begin
  Result := FileName;
  if Cmp(ExtractFileExt(FileName), '.lnk') then
    if ResolveLink(Application.Handle, FileName, Result) <> 0 then
      raise Exception.CreateFmt(SCantGetShortCut, [FileName]);
end;

function ResolveLink(const hwnd: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
var
  psl: IShellLink;
  WLinkFile: array [0..MAX_PATH] of WideChar;
  wfd: TWIN32FINDDATA;
  ppf: IPersistFile;
begin
  pointer(psl) := nil;
  pointer(ppf) := nil;
  Result := CoInitialize(nil);
  if SUCCEEDED(Result) then
  begin
    // Get a pointer to the IShellLink interface.
    Result := CoCreateInstance(CLSID_ShellLink, nil,
            CLSCTX_INPROC_SERVER, IShellLink, psl);
    if SUCCEEDED(Result) then
    begin

        // Get a pointer to the IPersistFile interface.
        Result := psl.QueryInterface(IPersistFile, ppf);
        if SUCCEEDED(Result) then
        begin
            StringToWideChar(LinkFile, WLinkFile, SizeOf(WLinkFile)-1);
            // Load the shortcut.
            Result := ppf.Load(WLinkFile, STGM_READ);
            if SUCCEEDED(Result) then
            begin
                // Resolve the link.
                Result := psl.Resolve(hwnd, SLR_ANY_MATCH);
                if SUCCEEDED(Result) then
                begin
                    // Get the path to the link target.
                    SetLength(FileName, MAX_PATH);
                    Result := psl.GetPath(PChar(FileName), MAX_PATH, wfd,
                      SLGP_UNCPRIORITY);
                    if not SUCCEEDED(Result) then
                      Exit;
                    SetLength(FileName, Length(PChar(FileName)));
                end;
            end;
        // Release the pointer to the IPersistFile interface.
        ppf._Release;
        end;
    // Release the pointer to the IShellLink interface.

    psl._Release;
    end;
    CoUnInitialize;
  end;
  pointer(psl) := nil;
  pointer(ppf) := nil;
end;
{$ENDIF RA_D3H}


{ Функция 'Словарь Сокращений', пример вызова:
    with memEdit do begin
      Text := ReplaceSokr(Text, SelStart+1, SelLength, memWords.Lines, memFrases.Lines, NewSelStart);
      SelStart := NewSelStart-1;
    end; }
function ReplaceSokr(S : string; PosBeg, Len : integer; Words, Frases : TStrings; var NewSelStart : integer) : string;
var
  i, Beg, Ent, LS, F : integer;
  Word : string;
begin
  NewSelStart := PosBeg; Result := S;
  LS := Length(S);
  if Len = 0 then begin
    if PosBeg < 1 then exit;
    if PosBeg = 1 then PosBeg := 2;
    for i := PosBeg-1 downto 1 do
      if S[i] in Separators then break;
    Beg := i+1;
    for Ent := PosBeg to LS do
      if S[Ent] in Separators then break;
    if Ent > Beg then
      Word := Copy(S, Beg, Ent-Beg) else
      Word := S[PosBeg];
  end else begin
    Word := Copy(S, PosBeg, Len);
    Beg := PosBeg;
    Ent := PosBeg + Len;
  end;
  if Word = '' then exit;
  F := Words.IndexOf(Word);
  if (F > -1) and (F < Frases.Count) then begin
    Result := Copy(S, 1, Beg-1)+Frases[F]+Copy(S, Ent, LS);
    NewSelStart := Beg + Length(Frases[F]);
  end;
end;

{ Функция 'Словарь Сокращений для всего текста', пример вызова:
    with memEdit do
      Text := ReplaceAllSokr(Text, memWords.Lines, memFrases.Lines);
}
function ReplaceAllSokr(S : string; Words, Frases : TStrings) : string;
var
  i, LW : integer;
  P : PChar;
  Sm : integer;
begin
  for i := 0 to Words.Count-1 do begin
    LW := Length(Words[i]);
    P := StrPos(PChar(S), PChar(Words[i]));
    while P <> nil do begin
      Sm := P-PChar(S);
      S := Copy(S, 1, Sm)+Frases[i]+Copy(S, Sm+LW+1, Length(S));
      P := StrPos(PChar(S)+Sm+Length(Frases[i]), PChar(Words[i]));
    end;
  end;
  Result := S;
end;

function CountOfLines(const S : string) : integer;
begin
  with TStringList.Create do
  try
    Text := S;
    Result := Count;
  finally
    Free;
  end;
end;

procedure DeleteEmptyLines(Ss : TStrings);
var
  i : integer;
begin
  i := 0;
  while i < Ss.Count do
    if Trim(Ss[i]) = '' then
      Ss.Delete(i) else
      inc(i);
end;

procedure SQLAddWhere(SQL : TStrings; const where : string);
var
  i, j : integer;
begin
  j := SQL.Count-1;
  for i := 0 to SQL.Count-1 do
    if StrLIComp(PChar(SQL[i]), 'where ', 6) = 0 then
    begin
      j := i+1;
      while j < SQL.Count do
      begin
        if (StrLIComp(PChar(SQL[j]), 'order ', 6) = 0) or
           (StrLIComp(PChar(SQL[j]), 'group ', 6) = 0) then
          break;
        inc(j);
      end;
    end;
  SQL.Insert(j, 'and '+Where);
end;

{-----------------------------------------------------------------------}
var
  ProcList : TList = nil;
type
  TProcItem = class
    ProcObj : TProcObj;
    constructor Create(AProcObj : TProcObj);
  end;

constructor TProcItem.Create(AProcObj : TProcObj);
begin
  ProcObj := AProcObj;
end;

procedure TmrProc(hwnd : HWND; uMsg : integer; idEvent : integer; dwTime : integer); stdcall;
var
  Pr : TProcObj;
begin
  if ProcList[idEvent] <> nil then begin
    Pr := TProcItem(ProcList[idEvent]).ProcObj;
    TProcItem(ProcList[idEvent]).Free;
  end else Pr := nil;
  ProcList.Delete(idEvent);
  KillTimer(hwnd, idEvent);
  if ProcList.Count <= 0 then begin
    ProcList.Free;
    ProcList := nil;
  end;
  if Assigned(Pr) then Pr;
end;

procedure ExecAfterPause(Proc : TProcObj; Pause : integer);
var
  Num : integer;
  i : integer;
begin
  if ProcList = nil then ProcList := TList.Create;
  Num := -1;
  for i := 0 to ProcList.Count-1 do
    if @TProcItem(ProcList[i]).ProcObj = @Proc then begin
      Num := i;
      break;
    end;
  if Num <> -1 then KillTimer(Application.Handle, Num)
  else Num := ProcList.Add(TProcItem.Create(Proc));
  SetTimer(Application.Handle, Num, Pause, @TmrProc);
end;
{=======================================================================}

procedure Roughed(ACanvas : TCanvas; const ARect : TRect; const AVert : boolean);
var
  i : integer;
  j : integer;
  R : TRect;
  V : boolean;
  H : boolean;
begin
  H := true;
  V := true;
  for i := 0 to (ARect.Right - ARect.Left) div 4 do begin
    for j := 0 to (ARect.Bottom - ARect.Top) div 4 do begin
      if AVert then begin
        if V then
          R := Bounds(ARect.Left + i * 4 + 2, ARect.Top + j * 4, 2, 2)
        else
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4, 2, 2);
      end else begin
        if H then
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4 + 2, 2, 2)
        else
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4, 2, 2);
      end;
      Frame3D(ACanvas, R, clBtnHighlight, clBtnShadow, 1);
      V := not V;
    end;
    H := not H;
  end;
end;

function BitmapFromBitmap(SrcBitmap : TBitmap; const AWidth, AHeight, index : integer) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Canvas.CopyRect(Rect(0, 0 , AWidth, AHeight), SrcBitmap.Canvas, Bounds(AWidth * index, 0, AWidth, AHeight));
end;

procedure ShowMenu(Form : TForm; MenuAni : TMenuAnimation);
var
  i : integer;
  h : integer;
  w : integer;
begin
  case MenuAni of
    maNone : Form.Show;
    maRandom : ;
    maUnfold : begin
      h := Form.Height;
      Form.Height := 0;
      Form.Show;
      for i := 0 to h div 10 do
        if Form.Height < h then Form.Height := Form.Height + 10;
    end;
    maSlide : begin
      h := Form.Height;
      w := Form.Width;
      Form.Height := 0;
      Form.Width  := 0;
      Form.Show;
      for i := 0 to max(h div 5, w div 5) do begin
        if Form.Height < h then Form.Height := Form.Height + 5;
        if Form.Width  < w then Form.Width  := Form.Width  + 5;
      end;
//      CS_SAVEBITS
    end;
  end;
end;

function ResSaveToFileEx(Instance: HINST; Typ, Name : PChar;
  const Compressed : boolean; const FileName : string) : boolean;
var
  Rhrsrc   : HRSRC;
  Rhglobal : HGLOBAL;
  RAddr    : Pointer;
  RLen     : DWORD;
  Stream   : TFileStream;
  FileDest : string;
begin
  Result := false;
  Rhrsrc := FindResource(
    Instance,// resource-module handle
    Name, // address of resource name
    Typ // address of resource type
    );
  if Rhrsrc = 0 then exit;
  Rhglobal := LoadResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
  );
  if Rhglobal = 0 then exit;
  RAddr := LockResource(
    Rhglobal // handle to resource to lock
   );
  FreeResource(Rhglobal);
  if RAddr = nil then exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
   );
  if RLen = 0 then exit;
  {А вот теперь можно копировать}
  Stream := nil;{для Free}
  if Compressed then
    FileDest := GenTempFileName(FileName) else
    FileDest := FileName;
  try
    try
      Stream := TFileStream.Create(FileDest, fmCreate or fmOpenWrite or fmShareExclusive);
      Stream.WriteBuffer(RAddr^, RLen);
    finally
     Stream.Free;
    end;
    if Compressed then begin
      Result := LZFileExpand(FileDest, FileName);
      DeleteFile(FileDest);
    end else Result := true;
  except end;
end;

function ResSaveToFile(const Typ, Name : string; const Compressed : boolean;
  const FileName : string) : boolean;
begin
  Result := ResSaveToFileEx(hInstance, PChar(Typ), PChar(Name), Compressed, FileName);
end;

function ResSaveToString(Instance: HINST; const Typ, Name : string;
  var S: string) : boolean;
var
  Rhrsrc   : HRSRC;
  Rhglobal : HGLOBAL;
  RAddr    : Pointer;
  RLen     : DWORD;
begin
  Result := false;
  Rhrsrc := FindResource(
    Instance,// resource-module handle
    PChar(Name), // address of resource name
    PChar(Typ) // address of resource type
    );
  if Rhrsrc = 0 then exit;
  Rhglobal := LoadResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
  );
  if Rhglobal = 0 then exit;
  RAddr := LockResource(
    Rhglobal // handle to resource to lock
   );
  FreeResource(Rhglobal);
  if RAddr = nil then exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
   );
  if RLen = 0 then exit;
  {А вот теперь можно копировать}
  SetString(S, PChar(RAddr), RLen);
end;

function Execute(const CommandLine, WorkingDirectory : string) : integer;
var
  R : boolean;
  ProcessInformation : TProcessInformation;
  StartupInfo : TStartupInfo;
{$IFDEF RA_D4H}
  ExCode : cardinal;
{$ELSE}
  ExCode : integer;
{$ENDIF}
begin
  Result := 0;
  FillChar(StartupInfo, sizeof(TStartupInfo), 0);
  with StartupInfo do begin
    cb := sizeof(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end;
  R := CreateProcess(
    nil, // pointer to name of executable module
    PChar(CommandLine), // pointer to command line string
    nil,// pointer to process security attributes
    nil,// pointer to thread security attributes
    false, // handle inheritance flag
    0, // creation flags
    nil,// pointer to new environment block
    PChar(WorkingDirectory), // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInformation // pointer to PROCESS_INFORMATION
   );
  if R then
    while (GetExitCodeProcess(ProcessInformation.hProcess, ExCode) and
          (ExCode = STILL_ACTIVE))
    do Application.ProcessMessages
  else
    Result := GetLastError;
end;

function TextWidth(AStr : string) : integer;
var
  Canvas : TCanvas;
  DC : HDC;
begin
  DC := GetDC(0);
  Canvas := TCanvas.Create;
  Canvas.Handle := DC;
  Result := Canvas.TextWidth(AStr);
  Canvas.Handle := 0;
  Canvas.Free;
  ReleaseDC(0, DC);
end;

function AppMinimized : boolean;
begin
  Result := IsIconic(Application.Handle);
end;

function MessageBox(const Message : string; Caption : string; const Flags : integer) : integer;
begin
  if Caption = '' then Caption := Application.Title;
  Result := Application.MessageBox(PChar(Message), PChar(Caption), Flags);
end;

function MsgDlgDef1(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; UseDefButton : boolean;
  AHelpContext : integer; Control : TWinControl): Integer;
const
 {$IFNDEF RA_D2}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');
 {$ELSE}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'Help');
 {$ENDIF RA_D2}
var
  P : TPoint;
  i : integer;
  Btn : TButton;
  StayOnTop : boolean;
begin
  if AHelpContext <> 0 then Buttons := Buttons + [mbHelp];
  StayOnTop := false;
  with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      Font.CharSet := MsgDlgCharSet;
      if (Screen.ActiveForm <> nil) and
         (Screen.ActiveForm.FormStyle = fsStayOnTop) then
      begin
        StayOnTop := true;
        SetWindowTop(Screen.ActiveForm.Handle, false);
      end;
      if ACaption <> '' then Caption := ACaption;
      if Control = nil then
      begin
        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;
      end else
      begin
        P := Point((Control.Width - Width) div 2,
            (Control.Height - Height) div 2);
        P := Control.ClientToScreen(P);
        Left := P.X;
        Top := P.Y
      end;
      if Left < 0 then Left := 0
      else if Left > Screen.Width then Left := Screen.Width - Width;
      if Top < 0 then Top := 0
      else if Top > Screen.Height then Top := Screen.Height - Height;
      HelpContext := AHelpContext;

      Btn := FindComponent(ButtonNames[DefButton]) as TButton;
      if UseDefButton and (Btn <> nil) then
      begin
        for i := 0 to ComponentCount - 1 do
          if Components[i] is TButton then
            (Components[i] as TButton).Default := false;
        Btn.Default := true;
        ActiveControl := Btn;
      end;
      Btn := FindComponent(ButtonNames[mbIgnore]) as TButton;
      if Btn <> nil then
      begin
       // Btn.Width := Btn.Width * 5 div 4;
        {сдвинуть кнопку Help}
      end;
      Result := ShowModal;
    finally
      Free;
      if (Screen.ActiveForm <> nil) and StayOnTop then
        SetWindowTop(Screen.ActiveForm.Handle, true);
    end;
end;

function MsgDlgDef(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; HelpContext : integer;
  Control : TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, DefButton, true, HelpContext, Control);
end;

function MsgDlg2(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext : integer;
  Control : TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, mbHelp, false, HelpContext, Control);
end;

procedure CenterHor(Parent : TControl; MinLeft : integer; Controls : array of TControl);
var
  i : integer;
begin
  for i := Low(Controls) to High(Controls) do
    Controls[i].Left := Max(MinLeft, (Parent.Width - Controls[i].Width) div 2)
end;

procedure EnableControls(Control : TWinControl; const Enable : boolean);
var
  i : integer;
begin
  for i := 0 to Control.ControlCount - 1 do
    Control.Controls[i].Enabled := Enable;
end;

procedure EnableMenuItems(MenuItem : TMenuItem; const Tag : integer; const Enable : boolean);
var
  i : integer;
begin
  for i := 0 to MenuItem.Count - 1 do
    if MenuItem[i].Tag <> Tag then
      MenuItem[i].Enabled := Enable;
end;

procedure ExpandWidth(Parent : TControl; MinWidth : integer; Controls : array of TControl);
var
  i : integer;
begin
  for i := Low(Controls) to High(Controls) do
   Controls[i].Width := Max(MinWidth, Parent.ClientWidth - 2 * Controls[i].Left);
end;

function PanelBorder(Panel : TCustomPanel) : integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
  if TPanel(Panel).BevelInner <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
end;

{$IFDEF RA_D2}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF RA_D2}

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

function DefineCursor(Identifer : PChar) : TCursor;
var
  Handle : HCursor;
begin
  Handle := LoadCursor(hInstance, Identifer);
  if Handle = 0 then
    raise EOutOfResources.Create('Cannot load cursor resource');
  for Result := 1 to High(TCursor) do
    if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
    begin
      Screen.Cursors[Result] := Handle;
      exit;
    end;
  raise EOutOfResources.Create('Too many user-defined cursors');
end;

procedure Delay(MSec : longword);
var
  T : longword;
begin
  T := GetTickCount;
  while GetTickCount - T < MSec do
    Application.ProcessMessages;
end;

function Pixels(Control : TControl; APixels : integer) : integer;
var
  Form : TForm;
begin
  Result := APixels;
  if Control is TForm then
    Form := TForm(Control) else
    Form := TForm(GetParentForm(Control));
  if Form.Scaled then
    Result := Result * Form.PixelsPerInch div 96;
end;

procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);
var
  i : integer;
  PropInfo : PPropInfo;
begin
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    PropInfo := GetPropInfo(Owner.Components[i].ClassInfo, PropName);
    if PropInfo <> nil then
      SetOrdProp(Owner.Components[i], PropInfo, Value);
  end;
end;

procedure Error(const Message : string);
begin
  raise Exception.Create(Message);
end;

procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL : string;
  i : integer;
  M1 : string;

  function Cmp(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
    if Result then inc(i, Length(M1));
  end;

  function CmpL(M1 : string) : boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1 : string) : boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M : string);
  begin
    if not Assigned(Canvas) then Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left :=  Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: boolean);
  begin
    if not Assigned(Canvas) then Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;    { if }

var
  oldFontStyles: TFontStyles;
  oldFontColor: TColor;
begin
  PlainItem := '';
  oldFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    oldFontStyles := Canvas.Font.Style;
    oldFontColor := Canvas.Font.Color;
  end;
  try
  if HideSelColor and Assigned(Canvas) then
  begin
    Canvas.Brush.Color := clWindow;
    Canvas.Font.Color := clWindowText;
  end;
  if Assigned(Canvas) then
    Canvas.FillRect(Rect);

  Width := Rect.Left;
  Rect.Left := Rect.Left + 2;
  M1 := '';
  i := 1;
  while i <= Length(Text) do
  begin
    if (Text[i] = '<') and
      (CmpL('b') or CmpL('/b') or
       CmpL('i') or CmpL('/i') or
       CmpL('u') or CmpL('/u') or
       Cmp('c:') )then
    begin
      Draw(M1);
      PlainItem := PlainItem + M1;

      if CmpL1('b') then
        Style(fsBold, True)
      else if CmpL1('/b') then
        Style(fsBold, False)
      else if CmpL1('i') then
        Style(fsItalic, True)
      else if CmpL1('/i') then
        Style(fsItalic, False)
      else if CmpL1('u') then
        Style(fsUnderline, True)
      else if CmpL1('/u') then
        Style(fsUnderline, False)
      else if Cmp1('c:') then
      begin
        CL := SubStr(PChar(Text)+ i, 0, '>');
        if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
        inc(i, Length(CL) + 1 {'>'});
      end;

      M1 := '';
    end else
      M1 := M1 + Text[i];
    inc(i);
  end;    { for }
  Draw(M1);
  PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := oldFontStyles;
      Canvas.Font.Color := oldFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
	const State: TOwnerDrawState; const Text : string;
	const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text : string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
  const HideSelColor: Boolean): Integer;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
  Result := W;
end;

procedure ClearList(List: TList);
var
	i: Integer;
begin
  if not Assigned(List) then Exit;
	for i := 0 to List.Count - 1 do    { Iterate }
		TObject(List[i]).Free;
	List.Clear;
end;    { ClearList }


procedure MemStreamToClipBoard(MemStream : TMemoryStream; const Format : word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  ClipBoard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE, MemStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(MemStream.Memory^, DataPtr^, MemStream.Size);
        Clipboard.Clear;
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    ClipBoard.Close;
  end;
end;

procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format : word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  ClipBoard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    DataPtr := GlobalLock(Data);
    if DataPtr = nil then Exit;
    try
      MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
      MemStream.Position := 0;
    finally
      GlobalUnlock(Data);
    end;
  finally
    ClipBoard.Close;
  end;
end;


{*******************  *********************}
function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    Result := tkUnknown
  else
    Result := PropInf^.PropType^.Kind;
end;

function GetPropStr(Obj: TObject; const PropName: string): string;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in [tkString, tkLString {$IFDEF RA_D3H}, tkWString {$ENDIF RA_D3H}]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetStrProp(Obj, PropInf);
end;

function GetPropOrd(Obj: TObject; const PropName: string): Integer;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in [tkInteger, tkChar, tkWChar,
     tkEnumeration, tkClass]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetOrdProp(Obj, PropInf);
end;

function GetPropMethod(Obj: TObject; const PropName: string): TMethod;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind = tkMethod) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetMethodProp(Obj, PropInf);
end;

procedure PrepareIniSection(SS: TStrings);
var
  i: Integer;
  S: string;
begin
  i := 0;
  while i < Ss.Count do
  begin
    S := Trim(Ss[i]);
    if (Length(S) = 0) or (S[1] in [';', '#']) then
      Ss.Delete(i)
    else
      inc(i);
  end;
end;

end.
