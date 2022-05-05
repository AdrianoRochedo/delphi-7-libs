{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclSysUtils.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: July 8, 2000                                                  }
{                                                                              }
{******************************************************************************}

unit JclSysUtils;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, TypInfo,
  JclBase;

//------------------------------------------------------------------------------
// Pointer manipulation
//------------------------------------------------------------------------------

{$IFNDEF DELPHI5_UP}
procedure FreeAndNil(var Obj);
{$ENDIF} // DELPHI5_UP

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
procedure FreeMemAndNil(var P: Pointer);
function PCharOrNil(const S: AnsiString): PChar;
{$IFDEF SUPPORTS_WIDESTRING}
function PWideCharOrNil(const W: WideString): PWideChar;
{$ENDIF}

function SizeOfMem(const APointer: Pointer): Integer;

//------------------------------------------------------------------------------
// Guards
//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INTERFACE}

type
  ISafeGuard = interface
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem;
    property Item: Pointer read GetItem;
  end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;

{$ENDIF}

//------------------------------------------------------------------------------
// Object lists
//------------------------------------------------------------------------------

procedure ClearObjectList(List: TList);
procedure FreeObjectList(var List: TList);

//------------------------------------------------------------------------------
// replacement for the C ternary conditional operator ? :
//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
{$IFDEF SUPPORTS_INT64}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
{$ENDIF}

//------------------------------------------------------------------------------
// Classes information and manipulation
//------------------------------------------------------------------------------

type
  EJclVMTError = class (EJclError);

//------------------------------------------------------------------------------
// Virtual Methods
//------------------------------------------------------------------------------

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);

//------------------------------------------------------------------------------
// Dynamic Methods
//------------------------------------------------------------------------------

type
  TDynamicIndexList = array [0..MaxInt div 16] of Word;
  PDynamicIndexList = ^TDynamicIndexList;
  TDynamicAddressList = array [0..MaxInt div 16] of Pointer;
  PDynamicAddressList = ^TDynamicAddressList;

function GetDynamicMethodCount(AClass: TClass): Integer;
function GetDynamicIndexList(AClass: TClass): PDynamicIndexList;
function GetDynamicAddressList(AClass: TClass): PDynamicAddressList;
function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean;
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;

{ init table methods }
function GetInitTable(AClass: TClass): PTypeInfo;

{ field table methods }

type
  PFieldEntry = ^TFieldEntry;
  TFieldEntry = packed record
    OffSet: Integer;
    IDX: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    EntryCount: Word;
    FieldClassTable: PFieldClassTable;
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

function GetFieldTable(AClass: TClass): PFieldTable;

{ method table }

type
  PMethodEntry = ^TMethodEntry;
  TMethodEntry = packed record
    EntrySize: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    FirstEntry: TMethodEntry;
   {Entries: array [1..65534] of TMethodEntry;}
  end;

function GetMethodTable(AClass: TClass): PMethodTable;
function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;

//------------------------------------------------------------------------------
// Class Parent
//------------------------------------------------------------------------------

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
function GetClassParent(AClass: TClass): TClass;

function IsClass(Address: Pointer): Boolean;

implementation

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SysUtils,
  JclResources;

//==============================================================================
// Pointer manipulation
//==============================================================================

{$IFNDEF DELPHI5_UP}

procedure FreeAndNil(var Obj);
var
  O: TObject;
begin
  O := TObject(Obj);
  Pointer(Obj) := nil;
  O.Free;
end;

{$ENDIF} // DELPHI5_UP

//------------------------------------------------------------------------------

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
begin
  GetMem(P, Size);
  FillChar(P^, Size, Value);
end;

//------------------------------------------------------------------------------

procedure FreeMemAndNil(var P: Pointer);
var
  Q: Pointer;
begin
  Q := P;
  P := nil;
  FreeMem(Q);
end;

//------------------------------------------------------------------------------

function PCharOrNil(const S: AnsiString): PChar;
begin
  if Length(S) = 0 then
    Result := nil
  else
    Result := PAnsiChar(S);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_WIDESTRING}

function PWideCharOrNil(const W: WideString): PWideChar;
begin
  if Length(W) = 0 then
    Result := nil
  else
    Result := PWideChar(W);
end;

{$ENDIF}

//------------------------------------------------------------------------------

type
  PUsed = ^TUsed;
  TUsed = record
    SizeFlags: Integer;
  end;

const
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function SizeOfMem(const APointer: Pointer): Integer;
var
  U: PUsed;
begin
  Result := 0;
  if APointer <> nil then
  begin
    U := APointer;
    U := PUsed(PChar(U) - SizeOf(TUsed));
    if (U.SizeFlags and cThisUsedFlag) <> 0 then
      Result := (U.SizeFlags) and (not cFlags - SizeOf(TUsed));
  end;
end;

//==============================================================================
// Guards
//==============================================================================

{$IFDEF SUPPORTS_INTERFACE}

type
  TSafeGuard = class (TInterfacedObject, ISafeGuard)
  private
    FItem: Pointer;
  public
    constructor Create(Mem: Pointer);
    destructor Destroy; override;
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem; virtual;
  end;

  TObjSafeGuard = class (TSafeGuard, ISafeGuard)
  public
    constructor Create(Obj: TObject);
    procedure FreeItem; override;
  end;

//------------------------------------------------------------------------------

constructor TSafeGuard.Create(Mem: Pointer);
begin
  FItem := Mem;
end;

//------------------------------------------------------------------------------

destructor TSafeGuard.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TSafeGuard.ReleaseItem: Pointer;
begin
  Result := FItem;
  FItem := nil;
end;

//------------------------------------------------------------------------------

function TSafeGuard.GetItem: Pointer;
begin
  Result := FItem;
end;

//------------------------------------------------------------------------------

procedure TSafeGuard.FreeItem;
begin
  if FItem <> nil then
    FreeMem(FItem);
  FItem := nil;
end;

//------------------------------------------------------------------------------

constructor TObjSafeGuard.Create(Obj: TObject);
begin
  inherited Create(Obj);
end;

//------------------------------------------------------------------------------

procedure TObjSafeGuard.FreeItem;
begin
  if FItem <> nil then
  begin
    TObject(FItem).Free;
    FItem := nil;
  end;
end;

//------------------------------------------------------------------------------

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
begin
  Result := Mem;
  SafeGuard := TSafeGuard.Create(Mem);
end;

//------------------------------------------------------------------------------

function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;
begin
  Result := Obj;
  SafeGuard := TObjSafeGuard.Create(Obj);
end;

//------------------------------------------------------------------------------

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  GetMem(Result, Size);
  Guard(Result, SafeGuard);
end;

//------------------------------------------------------------------------------

function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  Result := AllocMem(Size);
  Guard(Result, SafeGuard);
end;

{$ENDIF}

//==============================================================================
// Object lists
//==============================================================================

procedure ClearObjectList(List: TList);
var
  I: Integer;
begin
  for I := 0 to List.Count-1 do
  begin
    if TObject(List[I]) is TList then
      ClearObjectList(TList(List[I]));
    TObject(List[I]).Free;
    List[I] := nil;
  end;
  List.Clear;
end;

//------------------------------------------------------------------------------

procedure FreeObjectList(var List: TList);
begin
  ClearObjectList(List);
  FreeAndNil(List);
end;

//==============================================================================
// replacement for the C distfix operator ? :
//==============================================================================

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{$ENDIF}

//==============================================================================
// Classes information and manipulation
//==============================================================================

//==============================================================================
// Virtual Methods
//==============================================================================

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PLongint(Longint(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

//------------------------------------------------------------------------------

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
end;

//------------------------------------------------------------------------------

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, {@}Method,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);

  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

//==============================================================================
// Dynamic Methods
//==============================================================================

type
  TvmtDynamicTable = packed record
    Count: Word;
   {IndexList: array [1..Count] of Word;
    AddressList: array [1..Count] of Pointer;}
  end;

//------------------------------------------------------------------------------

function GetDynamicMethodCount(AClass: TClass): Integer; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@EXIT
        MOVZX   EAX, WORD PTR [EAX]
@@EXIT:
end;

//------------------------------------------------------------------------------

function GetDynamicIndexList(AClass: TClass): PDynamicIndexList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
end;

//------------------------------------------------------------------------------

function GetDynamicAddressList(AClass: TClass): PDynamicAddressList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        MOVZX   EDX, Word ptr [EAX]
        ADD     EAX, EDX
        ADD     EAX, EDX
        ADD     EAX, 2
end;

//------------------------------------------------------------------------------

function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean; assembler;
// Mainly copied from System.GetDynaMethod
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@haveVMT
@@outerLoop:
        MOV     EDX, [EDX]
@@haveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@found
        POP     ECX
@@parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@outerLoop
        MOV     EAX, 0
        JMP     @@exit
@@found:
        POP     EAX
        MOV     EAX, 1
@@exit:
        POP     EDI
end;

//------------------------------------------------------------------------------

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@FindDynaClass
end;

//==============================================================================
// Interface Table
//==============================================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        MOV     EAX, [EAX].vmtInitTable
end;

//------------------------------------------------------------------------------

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        MOV     EAX, [EAX].vmtFieldTable
end;

//------------------------------------------------------------------------------

function GetMethodTable(AClass: TClass): PMethodTable; assembler;
asm
        MOV     EAX, [EAX].vmtMethodTable
end;

//------------------------------------------------------------------------------

function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;
begin
  Result := Pointer(Cardinal(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(Cardinal(Result), Result^.EntrySize);
end;

//==============================================================================
// Class Parent methods
//==============================================================================

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, @NewClassParent,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);
  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);
  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

//------------------------------------------------------------------------------

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [AClass].vmtParent
        TEST    Result, EAX
        JE      @@EXIT
        MOV     EAX, [EAX]
@@EXIT:
end;

//------------------------------------------------------------------------------

function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @FALSE
        MOV     Result, True
        JMP     @EXIT
@FALSE:
        MOV     Result, False
@EXIT:
end;

//------------------------------------------------------------------------------

function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @FALSE
        MOV     Result, True
        JMP     @EXIT
@FALSE:
        MOV     Result, False
@EXIT:
end;

end.
