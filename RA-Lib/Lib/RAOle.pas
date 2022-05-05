{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       component   : none
       description : Ole2 support routines

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAOle;

interface

uses Windows, Ole2;

{ TEnumFormatEtc - format enumerator for TDataObject }
type
  PFormatList = ^TFormatList;
  TFormatList = array[0..255] of TFormatEtc;

type
  TEnumFormatEtc = class(IEnumFormatEtc)
  private
    FRefCount: Integer;
    FFormatList: PFormatList;
    FFormatCount: Integer;
    FIndex: Integer;
  public
    constructor Create(FormatList: PFormatList; FormatCount, Index: Integer);
    function QueryInterface(const iid: TIID; var obj): HResult; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
    function Next(celt: Longint; var elt;
      pceltFetched: PLongint): HResult; override;
    function Skip(celt: Longint): HResult; override;
    function Reset: HResult; override;
    function Clone(var enum: IEnumFormatEtc): HResult; override;
  end;

  ITrueDropTarget = class(IUnknown)
  public
    function DragEnter(dataObj: IDataObject; grfKeyState: longint;
      pt: TPoint; var dwEffect: Longint): HResult; virtual; stdcall; abstract;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; virtual; stdcall; abstract;
    function DragLeave: HResult; virtual; stdcall; abstract;
    function Drop(dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; virtual; stdcall; abstract;
  end;

  TRADropTarget = class(ITrueDropTarget)
    FRefCount : integer;
  public
    constructor Create;
    function QueryInterface(const iid: TIID; var obj): HResult; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
    function DragLeave: HResult; override;
  end;

  function OleStdGetDropEffect(grfKeyState: Longint) : Longint;

implementation

constructor TEnumFormatEtc.Create(FormatList: PFormatList;
  FormatCount, Index: Integer);
begin
  FRefCount := 1;
  FFormatList := FormatList;
  FFormatCount := FormatCount;
  FIndex := Index;
end;

function TEnumFormatEtc.QueryInterface(const iid: TIID; var obj): HResult;
begin
  if IsEqualIID(iid, IID_IUnknown) or IsEqualIID(iid, IID_IEnumFormatEtc) then
  begin
    Pointer(obj) := Self;
    AddRef;
    Result := S_OK;
  end else
  begin
    Pointer(obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;

function TEnumFormatEtc.AddRef: Longint;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TEnumFormatEtc.Release: Longint;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if FRefCount = 0 then Free;
end;

function TEnumFormatEtc.Next(celt: Longint; var elt;
  pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  while (I < celt) and (FIndex < FFormatCount) do
  begin
    TFormatList(elt)[I] := FFormatList[FIndex];
    Inc(FIndex);
    Inc(I);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;

function TEnumFormatEtc.Skip(celt: Longint): HResult;
begin
  if celt <= FFormatCount - FIndex then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end else
  begin
    FIndex := FFormatCount;
    Result := S_FALSE;
  end;
end;

function TEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumFormatEtc.Clone(var enum: IEnumFormatEtc): HResult;
begin
  enum := TEnumFormatEtc.Create(FFormatList, FFormatCount, FIndex);
  Result := S_OK;
end;

{********************* TRADropTarget ********************}
constructor TRADropTarget.Create;
begin
  FRefCount := 1;
end;

function TRADropTarget.QueryInterface(const iid: TIID; var obj): HResult;
begin
  if IsEqualIID(iid, IID_IUnknown) or IsEqualIID(iid, IID_IDropTarget) then
  begin
    Pointer(obj) := Self;
    AddRef;
    Result := S_OK;
  end else
  begin
    Pointer(obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;

function TRADropTarget.AddRef: Longint;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TRADropTarget.Release: Longint;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if FRefCount = 0 then Free;
end;

function TRADropTarget.DragLeave: HResult;
begin
  Result := S_OK;
end;
{##################### TRADropTarget ####################}


function OleStdGetDropEffect(grfKeyState: Longint) : Longint;
begin
  if (grfKeyState and MK_CONTROL) = MK_CONTROL then
    if (grfKeyState and MK_SHIFT) = MK_SHIFT then Result := DROPEFFECT_LINK
    else Result := DROPEFFECT_COPY
  else if (grfKeyState and MK_SHIFT) = MK_SHIFT then Result := DROPEFFECT_MOVE
  else Result := 0;
end;

end.
