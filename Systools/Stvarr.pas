{$I STDEFINE.INC}

{$A-} {Packed records}
{$B-} {Incomplete boolean evaluation}
{$I+} {trap I/O exceptions here}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   STVARR.PAS 1.05                     *}
{*                  Virtual matrix class                 *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Notes:
  - The virtual matrix uses a disk file for the main storage of a
    two-dimensional array. A specified number of rows from the matrix can
    be stored in a memory cache.

  - The cache must be large enough to hold at least 2 rows. In 16-bit mode,
    the cache can hold at most about 5460 rows. In 32-bit mode, the number
    of cached rows is essentially unlimited.

  - Normally the disk file is treated as a pure file of rows, where each
    row is composed of cell columns. By overriding the HeaderSize, WriteHeader,
    and ReadHeader methods, the application can use a file that has a header
    prior to the array data.

  - By defining a matrix of one column, the TStVMatrix class can be used
    as a cache manager for any file of record.
}

unit STVArr;
{-Virtual matrix class}

interface

uses
  Classes,
  SysUtils,
  STConst,
  STBase;

type
 {.Z-}
  TStCacheRec = record
    crRow     : Cardinal;    {row number in cache}
    crRowData : Pointer;     {pointer to row buffer}
    crTime    : LongInt;     {quasi-time last used}
    crDirty   : Integer;     {non-zero if Row changed in memory}
  end;
  TStCacheArray = array[0..(MaxBlockSize div SizeOf(TStCacheRec))-1] of TStCacheRec;
  PStCacheArray = ^TStCacheArray;
 {.Z-}

  TStVMatrix = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FRows     : Cardinal;   {number of rows}
    FCacheRows: Integer;    {number of cached rows}
    FCols     : Cardinal;   {number of columns}
    FElSize   : Cardinal;   {size of each array element}

    {private instance variables}
    vmRowSize  : LongInt;    {number of bytes in a row}
    vmCacheCnt : Integer;    {number of used rows in cache}
    vmCacheTime: LongInt;    {quasi-time for LRU}
    vmCache    : PStCacheArray; {sorted collection of cached rows}
    vmDataF    : Integer;    {data file}

    {protected undocumented methods}
    procedure conForEachUntypedVar(Action : TIterateUntypedFunc;
                                   OtherData : pointer); override;
    procedure conGetArraySizes(var RowCount, ColCount, ElSize : Cardinal); override;
    procedure conSetArraySizes(RowCount, ColCount, ElSize : Cardinal); override;
    function conStoresUntypedVars : boolean; override;
    procedure vmSetCacheRows(CacheRows : Integer);
    procedure vmAllocateCache;
    procedure vmDeallocateCache;
    procedure vmInvalidateCache;
    procedure vmFlushCacheNode(CacheIndex : Integer);
    function vmIncCacheTime : LongInt;
    function vmSearchCache(Row : Cardinal; var CacheIndex : Integer) : Boolean;
    function vmGetRowData(Row : Cardinal; MakeDirty : Boolean) : Pointer;
    procedure vmWriteRow(Row : Cardinal; Data : Pointer; Seek : Boolean);
    procedure vmSetRows(Rows : Cardinal);

  {.Z-}
  public
    constructor Create(Rows, Cols, ElementSize : Cardinal;
                       CacheRows : Integer;
                       const DataFile : string; OpenMode : Word); virtual;
      {-Initialize a virtual 2D matrix}
    destructor Destroy; override;
      {-Free a virtual 2D matrix}
    procedure FlushCache;
      {-Write any dirty cache rows to disk}

    function HeaderSize : LongInt; virtual;
      {-Return the header size of the array file, default 0}
    procedure WriteHeader; virtual;
      {-Write a header to the array file, default none}
    procedure ReadHeader; virtual;
      {-Read a header from the array file, default none}

    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Clear; override;
      {-Fill the matrix with zeros}
    procedure Fill(const Value);
      {-Fill matrix with specified element value}

    procedure Put(Row, Col : Cardinal; const Value);
      {-Set an element}
    procedure Get(Row, Col : Cardinal; var Value);
      {-Return an element}

    procedure PutRow(Row : Cardinal; const RowValue);
      {-Set an entire row}
    procedure GetRow(Row : Cardinal; var RowValue);
      {-Return an entire row}

    procedure ExchangeRows(Row1, Row2 : Cardinal);
      {-Exchange the specified rows}
    procedure SortRows(KeyCol : Cardinal; Compare : TUntypedCompareFunc);
      {-Sort the array rows using the given comparison function and
        the elements in the given column}

    property Rows : Cardinal
      {-Read or write the number of rows in the array}
      read FRows
      write vmSetRows;

    property CacheRows : Integer
      {-Read or write the number of cache rows in the array}
      read FCacheRows
      write vmSetCacheRows;
    property Cols : Cardinal
      {-Read the number of columns in the array}
      read FCols;

    property ElementSize : Cardinal
      {-Read the size of each element in the array}
      read FElSize;
  end;

{======================================================================}

implementation

uses
{$IFDEF SYSDEMO} SysUtil, {$ENDIF}
  STUtils {used for ExchangeStructs}
{$IFDEF VirtualPascal}
  ;
{$ELSE}
 {$IFDEF WIN32}
  , Windows;
 {$ELSE}
  , WinTypes,
  WinProcs;
 {$ENDIF}
{$ENDIF}

function AssignMatrixData(Container : TStContainer;
                      var Data;
                          OtherData : Pointer) : Boolean; far;
  var
    OurMatrix : TStVMatrix absolute OtherData;
    RD : TAssignRowData absolute Data;
  begin
    OurMatrix.PutRow(RD.RowNum, RD.Data);
    Result := true;
  end;

procedure TStVMatrix.Assign(Source: TPersistent);
begin
  {$IFDEF ThreadSafe}
  EnterCS;
  try
  {$ENDIF}
    {The only containers that we allow to be assigned to a large matrix
     are:
       - a SysTools large array (TStLArray)
       - a SysTools large matrix (TStLMatrix)
       - another SysTools virtual matrix (TStVMatrix)}
    if not conAssignUntypedVars(Source, AssignMatrixData) then
      inherited Assign(Source);
  {$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;{try..finally}
  {$ENDIF}
end;

procedure TStVMatrix.Clear;
var
  Row : Cardinal;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    vmInvalidateCache;
    vmCacheCnt := 1;
    with vmCache^[0] do begin
      HugeFillChar(crRowData^, vmRowSize, 0);
      crRow := 0;
      crTime := vmIncCacheTime;
      crDirty := 0;
      FileSeek(vmDataF, 0, 0);
      WriteHeader;
      for Row := 0 to FRows-1 do
        vmWriteRow(Row, crRowData, False);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.conForEachUntypedVar(Action : TIterateUntypedFunc;
                                          OtherData : pointer);
  var
    FullRow : ^TAssignRowData;
    i       : Cardinal;
  begin
  {$IFDEF ThreadSafe}
    EnterCS;
    try
  {$ENDIF}
      GetMem(FullRow, sizeof(Cardinal) + vmRowSize);
      try
        for i := 0 to pred(Rows) do
          begin
            FullRow^.RowNum := i;
            GetRow(i, FullRow^.Data);
            Action(Self, FullRow^, OtherData);
          end;
      finally
        FreeMem(FullRow, sizeof(Cardinal) + vmRowSize);
      end;
  {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;
  {$ENDIF}
  end;

procedure TStVMatrix.conGetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
  begin
    RowCount := Rows;
    ColCount := Cols;
    ElSize := ElementSize;
  end;

procedure TStVMatrix.conSetArraySizes(RowCount, ColCount, ElSize : Cardinal);
  begin
    if (ColCount <> Cols) then
      RaiseContainerError(stscBadColCount);
    if (ElSize <> ElementSize) then
      RaiseContainerError(stscBadElSize);
    if (RowCount <> Rows) then
      begin
        Rows := RowCount;
      end;
  end;

function TStVMatrix.conStoresUntypedVars : boolean;
  begin
    Result := true;
  end;

constructor TStVMatrix.Create(Rows, Cols, ElementSize : Cardinal;
                              CacheRows : Integer;
                              const DataFile : string; OpenMode : Word);
begin
  FElSize := ElementSize;
  FRows := Rows;
  FCols := Cols;
  FCount := LongInt(Rows)*Cols;
  vmRowSize := LongInt(Cols)*ElementSize;
  FCacheRows := CacheRows;
  vmDataF := -1;

{$IFNDEF OS32}
  if GetWinFlags and Wf_Enhanced = 0 then
    RaiseContainerError(stscBadWinMode);
{$ENDIF}

  CreateContainer(TStNode);

  if (Rows = 0) or (Cols = 0) or (ElementSize = 0) or (CacheRows < 2) or
  ProductOverflow(Cols, ElementSize) or
  ProductOverflow(LongInt(Cols)*ElementSize, Rows) or
  (LongInt(Cols)*ElementSize*Rows > MaxLongInt-HeaderSize) or
  (CacheRows > MaxBlockSize div SizeOf(TStCacheRec)) then
    RaiseContainerError(stscBadSize);

  vmAllocateCache;

  {open the data file}
  vmDataF := FileOpen(DataFile, OpenMode);
  if vmDataF < 0 then begin
    {file not found, create it}
    vmDataF := FileCreate(DataFile);
    if vmDataF < 0 then
      RaiseContainerError(stscFileCreate)
    else begin
      FileClose(vmDataF);
      vmDataF := FileOpen(DataFile, OpenMode);
      if vmDataF < 0 then
        RaiseContainerError(stscFileOpen);
      {write user defined header to file}
      WriteHeader;
      FileSeek(vmDataF, 0, 0);
    end;
  end;

  {read user defined header from file}
  ReadHeader;
end;

destructor TStVMatrix.Destroy;
begin
  if Assigned(vmCache) then begin
    if vmDataF > 0 then
      FlushCache;
    vmDeallocateCache;
  end;

  if vmDataF > 0 then begin
    {write user defined header to file}
    FileSeek(vmDataF, 0, 0);
    WriteHeader;
    FileClose(vmDataF);
  end;

  IncNodeProtection;
  inherited Destroy;
end;

procedure TStVMatrix.ExchangeRows(Row1, Row2 : Cardinal);
{$IFDEF OS32}
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (Row1 >= Rows) or (Row2 >= Rows) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    ExchangeStructs(vmGetRowData(Row1, True)^, vmGetRowData(Row2, True)^, vmRowSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;
{$ELSE}
var
  P1, P2 : Pointer;
begin
{$IFOPT R+}
  if (Row1 >= Rows) or (Row2 >= Rows) then
    RaiseContainerError(stscBadIndex);
{$ENDIF}
  P1 := vmGetRowData(Row1, True);
  P2 := vmGetRowData(Row2, True);
  asm
    push ds
    les di,Self                             {es:di -> Self}

    db $66
    mov cx,word ptr TStVMatrix(es:[di]).vmRowSize {ecx = vmRowSize}

    db $66
    xor si,si
    lds si,P1
    db $66
    xor di,di
    les di,P2

    mov dx,cx
    db $66
    shr cx,2
    jz @2

    {following bizarre stuff required to get working machine code}
    {16-bit bp addressing is equivalent to 32-bit esi addressing}
    {16-bit bx addressing is equivalent to 32-bit edi addressing}
  @1:
    db $67,$66
    mov ax,[bp]
    db $67,$66
    xchg ax,es:[bx]
    db $67,$66
    mov [bp],ax
    db $66
    add si,4
    db $66
    add di,4
    db $66
    dec cx
    jnz @1

  @2:
    mov cx,dx
    and cx,3
    jz @4

  @3:
    db $67
    mov al,[bp]
    db $67
    xchg al,es:[bx]
    db $67
    mov [bp],al
    db $66
    inc si
    db $66
    inc di
    dec cx
    jnz @3

  @4:
    pop ds
  end;

end;
{$ENDIF}

procedure TStVMatrix.Fill(const Value);
var
  Row : Cardinal;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    vmInvalidateCache;
    vmCacheCnt := 1;
    with vmCache^[0] do begin
      HugeFillStruc(crRowData^, FCols, Value, FElSize);
      crRow := 0;
      crTime := vmIncCacheTime;
      crDirty := 0;
      FileSeek(vmDataF, 0, 0);
      WriteHeader;
      for Row := 0 to FRows-1 do
        vmWriteRow(Row, crRowData, False);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.FlushCache;
var
  I : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    for I := 0 to vmCacheCnt-1 do
      vmFlushCacheNode(I);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.Get(Row, Col : Cardinal; var Value);
{$IFDEF OS32}
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (Row >= Rows) or (Col >= Cols) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    Move(PChar(vmGetRowData(Row, False))[Col*FElSize], Value, FElSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;
{$ELSE}
var
  P : Pointer;
begin
{$IFOPT R+}
  if (Row >= Rows) or (Col >= Cols) then
    RaiseContainerError(stscBadIndex);
{$ENDIF}
  P := vmGetRowData(Row, True);
  asm
    les di,Self                             {es:di -> Self}
    db $66
    xor ax,ax
    mov ax,Col                              {eax = Col}
    db $66
    xor cx,cx
    mov cx,TStVMatrix(es:[di]).FElSize      {ecx = FElSize}
    db $66
    mul cx                                  {eax = array offset}
    mov dx,ds
    db $66
    xor si,si
    lds si,P
    db $66
    add si,ax                               {ds:esi -> element in row}
    db $66
    xor di,di
    les di,Value                            {es:edi -> Value}
    cld
    mov bx,cx                               {save cx}
    shr cx,2                                {will move dwords}
    rep
    db $67
    db $66
    movsw                                   {move dwords}
    mov cx,bx
    and cx,3
    rep
    db $67
    movsb                                   {move up to 3 bytes}
    mov ds,dx
  end;
end;
{$ENDIF}

procedure TStVMatrix.GetRow(Row : Cardinal; var RowValue);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if Row >= Rows then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    HugeMove(vmGetRowData(Row, False)^, RowValue, vmRowSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStVMatrix.HeaderSize : LongInt;
begin
  Result := 0;
end;

procedure TStVMatrix.ReadHeader;
begin
  {does nothing by default}
  {can assume that FilePos = 0 when this is called}
end;

procedure TStVMatrix.Put(Row, Col : Cardinal; const Value);
{$IFDEF OS32}
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (Row >= Rows) or (Col >= Cols) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    Move(Value, PChar(vmGetRowData(Row, True))[Col*FElSize], FElSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;
{$ELSE}
var
  P : Pointer;
begin
{$IFOPT R+}
  if (Row >= Rows) or (Col >= Cols) then
    RaiseContainerError(stscBadIndex);
{$ENDIF}
  P := vmGetRowData(Row, True);
  asm
    les di,Self                             {es:di -> Self}
    db $66
    xor ax,ax
    mov ax,Col                              {eax = Col}
    db $66
    xor cx,cx
    mov cx,TStVMatrix(es:[di]).FElSize      {ecx = FElSize}
    db $66
    mul cx                                  {eax = array offset}
    db $66
    xor di,di
    les di,P
    db $66
    add di,ax                               {es:edi -> element in row}
    mov dx,ds
    db $66
    xor si,si
    lds si,Value                            {ds:esi -> Value}
    cld
    mov bx,cx                               {save cx}
    shr cx,2                                {will move dwords}
    rep
    db $67
    db $66
    movsw                                   {move dwords}
    mov cx,bx
    and cx,3
    rep
    db $67
    movsb                                   {move up to 3 bytes}
    mov ds,dx
  end;
end;
{$ENDIF}

procedure TStVMatrix.PutRow(Row : Cardinal; const RowValue);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if Row >= Rows then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    HugeMove(RowValue, vmGetRowData(Row, True)^, vmRowSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.SortRows(KeyCol : Cardinal; Compare : TUntypedCompareFunc);
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of LongInt;
var
  L : LongInt;
  R : LongInt;
  PL : LongInt;
  PR : LongInt;
  CurEl : Pointer;
  PivEl : Pointer;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if KeyCol >= Cols then
      RaiseContainerError(stscBadIndex);

    {Need at least 2 rows to sort}
    if FRows <= 1 then
      Exit;

    GetMem(CurEl, FElSize);
    try
      GetMem(PivEl, FElSize);

      {Initialize the stacks}
      StackP := 0;
      LStack[0] := 0;
      RStack[0] := FRows-1;

      {Repeatedly take top partition from stack}
      repeat

        {Pop the stack}
        L := LStack[StackP];
        R := RStack[StackP];
        Dec(StackP);

        {Sort current partition}
        repeat

          {Load the pivot element}
          Get((L+R) div 2, KeyCol, PivEl^);
          PL := L;
          PR := R;

          {Swap items in sort order around the pivot index}
          repeat
            Get(PL, KeyCol, CurEl^);
            while Compare(CurEl^, PivEl^) < 0 do begin
              Inc(PL);
              Get(PL, KeyCol, CurEl^);
            end;
            Get(PR, KeyCol, CurEl^);
            while Compare(PivEl^, CurEl^) < 0 do begin
              Dec(PR);
              Get(PR, KeyCol, CurEl^);
            end;
            if PL <= PR then begin
              if PL <> PR then
                {Swap the two elements}
                ExchangeRows(PL, PR);
              Inc(PL); {assume we'll never sort 2 billion elements}
              Dec(PR);
            end;
          until PL > PR;

          {Decide which partition to sort next}
          if (PR-L) < (R-PL) then begin
            {Right partition is bigger}
            if PL < R then begin
              {Stack the request for sorting right partition}
              Inc(StackP);
              LStack[StackP] := PL;
              RStack[StackP] := R;
            end;
            {Continue sorting left partition}
            R := PR;
          end else begin
            {Left partition is bigger}
            if L < PR then begin
              {Stack the request for sorting left partition}
              Inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := PR;
            end;
            {Continue sorting right partition}
            L := PL;
          end;

        until L >= R;
      until StackP < 0;

      FreeMem(PivEl, FElSize);
    finally
      FreeMem(CurEl, FElSize);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.vmAllocateCache;
var
  I : Integer;
begin
  GetMem(vmCache, FCacheRows*SizeOf(TStCacheRec));
  FillChar(vmCache^, FCacheRows*SizeOf(TStCacheRec), 0);
  try
    for I := 0 to FCacheRows-1 do
      with vmCache^[I] do
        HugeGetMem(crRowData, vmRowSize);
  except
    vmDeallocateCache;
    raise;
  end;
  vmInvalidateCache;
end;

procedure TStVMatrix.vmDeallocateCache;
var
  I : Integer;
begin
  if Assigned(vmCache) then begin
    for I := FCacheRows-1 downto 0 do
      HugeFreeMem(vmCache^[I].crRowData, vmRowSize);
    if Assigned(vmCache) then
      FreeMem(vmCache, FCacheRows*SizeOf(TStCacheRec));
    vmCache := nil;
  end;
  FCacheRows := 0;
end;

procedure TStVMatrix.vmFlushCacheNode(CacheIndex : Integer);
begin
  with vmCache^[CacheIndex] do
    if crDirty > 0 then begin
      vmWriteRow(crRow, crRowData, True);
      crDirty := 0;
    end;
end;

function TStVMatrix.vmGetRowData(Row : Cardinal; MakeDirty : Boolean) : Pointer;
var
  CacheIndex, OldestIndex : Integer;
  OldestTime, Bytes : LongInt;
  TmpRowData : Pointer;
begin
  if not vmSearchCache(Row, CacheIndex) then begin
    {row not found in cache}
    if vmCacheCnt = FCacheRows then begin
      {cache full, must throw out oldest row in cache}
      OldestTime := MaxLongInt;
      OldestIndex := 0; {prevent D32 from generating a warning}
      for CacheIndex := 0 to vmCacheCnt-1 do
        with vmCache^[CacheIndex] do
          if crTime < OldestTime then begin
            OldestIndex := CacheIndex;
            OldestTime := crTime;
          end;
      vmFlushCacheNode(OldestIndex);
      dec(vmCacheCnt);
      TmpRowData := vmCache^[OldestIndex].crRowData;
      Move(vmCache^[OldestIndex+1], vmCache^[OldestIndex],
            (vmCacheCnt-OldestIndex)*SizeOf(TStCacheRec));
      vmCache^[vmCacheCnt].crRowData := TmpRowData;
      {find spot where row should now be inserted}
      vmSearchCache(Row, CacheIndex);
    end;

    {add row to cache}
    TmpRowData := vmCache^[vmCacheCnt].crRowData;
    Move(vmCache^[CacheIndex], vmCache^[CacheIndex+1],
          (vmCacheCnt-CacheIndex)*SizeOf(TStCacheRec));
    inc(vmCacheCnt);
    with vmCache^[CacheIndex] do begin
      crRowData := TmpRowData;
      crRow := Row;
      Bytes := FileSeek(vmDataF, HeaderSize+Row*vmRowSize, 0);
      if Bytes >= 0 then
        Bytes := FileRead(vmDataF, crRowData^, vmRowSize);
      if Bytes < 0 then
        RaiseContainerError(stscFileRead);
      {else if Bytes = 0 then}
        {row hasn't been written to yet}
        {HugeFillChar(crRowData^, vmRowSize, 0);}
      crDirty := 0;
    end;
  end;

  with vmCache^[CacheIndex] do begin
    Result := crRowData;
    if MakeDirty then
      crDirty := 1;
    crTime := vmIncCacheTime;
  end;
end;

function TStVMatrix.vmIncCacheTime : LongInt;
var
  I : Integer;
begin
  if vmCacheTime = MaxLongInt-1 then begin
    {reset time for all buffers}
    for I := 0 to vmCacheCnt-1 do
      vmCache^[I].crTime := 0;
    vmCacheTime := 0;
  end;
  inc(vmCacheTime);
  Result := vmCacheTime;
end;

procedure TStVMatrix.vmInvalidateCache;
begin
  vmCacheCnt := 0;
  vmCacheTime := 0;
end;

function TStVMatrix.vmSearchCache(Row : Cardinal; var CacheIndex : Integer) : Boolean;
var
  L, R, M : Integer;
  Comp : LongInt;
begin
  if vmCacheCnt = 0 then begin
    Result := False;
    CacheIndex := 0;
    Exit;
  end;

  {search cache for row using binary search}
  L := 0;
  R := vmCacheCnt-1;
  repeat
    M := (L+R) div 2;
    with vmCache^[M] do begin
      Comp := LongInt(Row)-crRow;
      if Comp = 0 then begin
        {found row in cache}
        Result := True;
        CacheIndex := M;
        Exit;
      end else if Comp < 0 then
        R := M-1
      else
        L := M+1;
    end;
  until L > R;

  {not found, return where it should be inserted}
  Result := False;
  CacheIndex := M;
  if Comp > 0 then
    inc(CacheIndex);
end;

procedure TStVMatrix.vmSetCacheRows(CacheRows : Integer);
var
  I : Integer;
  NewCache : PStCacheArray;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if CacheRows = FCacheRows then
      Exit;

    if (CacheRows < 2) or (CacheRows > MaxBlockSize div SizeOf(TStCacheRec)) then
      RaiseContainerError(stscBadSize);

    {allocate new cache descriptor array}
    GetMem(NewCache, CacheRows*SizeOf(TStCacheRec));
    FillChar(NewCache^, CacheRows*SizeOf(TStCacheRec), 0);

    try
      {allocate new buffers if any}
      for I := FCacheRows to CacheRows-1 do
        with NewCache^[I] do
          HugeGetMem(crRowData, vmRowSize);

      {transfer old cache buffers to new array}
      for I := 0 to FCacheRows-1 do
        if I < CacheRows then
          NewCache^[I] := vmCache^[I]
        else begin
          {number of buffers shrunk, get rid of excess buffers}
          if I < vmCacheCnt then
            vmFlushCacheNode(I);
          HugeFreeMem(vmCache^[I].crRowData, vmRowSize);
        end;

    except
      for I := CacheRows-1 downto 0 do
        HugeFreeMem(NewCache^[I].crRowData, vmRowSize);
      FreeMem(NewCache, CacheRows*SizeOf(TStCacheRec));
    end;

    {update cache in-use count}
    if vmCacheCnt > CacheRows then
      vmCacheCnt := CacheRows;

    {deallocate old cache}
    FreeMem(vmCache, FCacheRows*SizeOf(TStCacheRec));
    vmCache := NewCache;
    FCacheRows := CacheRows;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.vmSetRows(Rows : Cardinal);
var
  I : Integer;
  NewSize : LongInt;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Rows = FRows then
      Exit;

    {validate new size}
    if (Rows = 0) or
    ProductOverflow(Rows, Cols) or
    ProductOverflow(LongInt(Rows)*Cols, FElSize) then
      RaiseContainerError(stscBadSize);

    if Rows < FRows then begin
      {dump now-irrelevant rows from cache}
      for I := 0 to vmCacheCnt-1 do
        if vmCache^[I].crRow >= Rows then begin
          vmCacheCnt := I;
          break;
        end;
      {truncate data file}
      NewSize := HeaderSize+LongInt(Rows)*Cols*FElSize;
      if FileSeek(vmDataF, 0, 2) > NewSize then begin
        FileSeek(vmDataF, NewSize, 0);
{$IFDEF WIN32}
        if not SetEndOfFile(vmDataF) then
          RaiseContainerError(stscFileWrite);
{$ELSE}
        NewSize := FileWrite(vmDataF, I, 0);
        if NewSize < 0 then
          RaiseContainerError(stscFileWrite);
{$ENDIF}
      end;
    end;

    FRows := Rows;
    FileSeek(vmDataF, 0, 0);
    WriteHeader;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStVMatrix.vmWriteRow(Row : Cardinal; Data : Pointer; Seek : Boolean);
var
  Bytes : LongInt;
begin
  if Seek then
    FileSeek(vmDataF, HeaderSize+Row*vmRowSize, 0);
  Bytes := FileWrite(vmDataF, Data^, vmRowSize);
  if (Bytes < 0) or (Bytes <> vmRowSize) then
    RaiseContainerError(stscFileWrite);
end;

procedure TStVMatrix.WriteHeader;
begin
  {does nothing by default}
  {can assume that FilePos = 0 when this is called}
end;

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.
