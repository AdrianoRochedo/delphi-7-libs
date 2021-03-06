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
{ The Original Code is JclSysInfo.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: August 9, 2000                                                  }
{                                                                              }
{******************************************************************************}

//*****************************************************************************

//  CURRENT MODIFICATIONS BY ESF    2000/06/04

//  BUG0001    Fixed according to specification
//  BUG0002    Scan for #13#10 removed per MVB's suggestion
//  CCP0004    Tabs have been replaced with spaces per suggestion
//  [MVB]      IsWinNT initialization restored per MVB's request
//  (rom)      Trim() ed Win32CSDVersion per Petr Vones bug report
//  (rom)      added new RunningProcessesList from Petr Vones

unit JclSysInfo;

{$I JCL.INC}

interface

uses
  Windows, Classes;

//------------------------------------------------------------------------------
// Environment
//------------------------------------------------------------------------------

type
  TEnvironmentOptions = set of (eoLocalMachine, eoCurrentUser, eoAdditional);

function DelEnvironmentVar(const Name: string): Boolean;
function ExpandEnvironmentVar(var Value: string): Boolean;
function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
function SetEnvironmentVar(const Name, Value: string): Boolean;
function CreateEnvironmentBlock(const Options: TEnvironmentOptions;
  const AdditionalVars: TStrings): PChar;

//------------------------------------------------------------------------------
// Common Folders
//------------------------------------------------------------------------------

function GetCommonFilesFolder: string;
function GetCurrentFolder: string;
function GetProgramFilesFolder: string;
function GetWindowsFolder: string;
function GetWindowsSystemFolder: string;
function GetWindowsTempFolder: string;

//------------------------------------------------------------------------------
// Identification
//------------------------------------------------------------------------------

function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetIPAddress(const HostName: string): string;
function GetLocalComputerName: string;
function GetLocalUserName: string;
function GetUserDomainName(const CurUser: string): string;
function GetDomainName: string;
function GetRegisteredCompany: string;
function GetRegisteredOwner: string;
function RunningProcessesList(List: TStrings): Boolean;

//------------------------------------------------------------------------------
// Version Information
//------------------------------------------------------------------------------

type
  TWindowsVersion = (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE,
                     wvWinNT3, wvWinNT4, wvWin2000);
  TNtProductType = (ptUnknown, ptWorkStation, ptServer, ptAdvancedServer);

function GetWindowsVersion: TWindowsVersion;

var
  IsWin95: Boolean = False;
  IsWin95OSR2: Boolean = False;
  IsWin98: Boolean = False;
  IsWin98SE: Boolean = False;
  IsWinNT: Boolean = False;
  IsWinNT3: Boolean = False;
  IsWinNT4: Boolean = False;
  IsWin2K: Boolean = False;

function NtProductType: TNtProductType;

//------------------------------------------------------------------------------
// Hardware
//------------------------------------------------------------------------------

function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;

function ReadTimeStampCounter: Int64;

type
  TIntelSpecific = record
    L2Cache: Cardinal;
    CacheDescriptors: array [0..15] of Byte;
  end;

  TCyrixSpecific = record
    L1CacheInfo: array [0..3] of Byte;
    TLBInfo: array [0..3] of Byte;
  end;

  TAMDSpecific = record
    DataTLB: array [0..1] of Byte;
    InstructionTLB: array [0..1] of Byte;
    L1DataCache: array [0..3] of Byte;
    L1ICache: array [0..3] of Byte;
  end;

  TCacheInfo = record
    D: Byte;
    I: string[74];
  end;

  TFreqInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

  TCpuInfo = record
    HasInstruction: Boolean;
    MMX: Boolean;
    IsFDIVOK: Boolean;
    HasCacheInfo: Boolean;
    HasExtendedInfo: Boolean;
    CpuType: Byte;
    PType: Byte;
    Family: Byte;
    Model: Byte;
    Stepping: Byte;
    Features: Cardinal;
    FrequencyInfo: TFreqInfo;
    VendorIDString: array [0..11] of Char;
    Manufacturer: array [0..9] of Char;
    CpuName: array [0..47] of Char;
    IntelSpecific: TIntelSpecific;
    CyrixSpecific: TCyrixSpecific;
    AMDSpecific: TAMDSpecific;
  end;

const
  CPU_TYPE_INTEL = 1;
  CPU_TYPE_CYRIX = 2;
  CPU_TYPE_AMD   = 3;

// Constants to be used with Feature Flag set of a CPU
// eg. IF (Features and FPU_FLAG = FPU_FLAG) THEN CPU has Floating-Point unit on
// chip. However, Intel claims that in future models, a zero in the feature
// flags will mean that the chip has that feature, however, the following flags
// will work for any production 80x86 chip or clone.
// eg. IF (Features and FPU_FLAG = 0) then CPU has Floating-Point unit on chip.

const

{ Standard (Intel) Feature Flags }

  FPU_FLAG   = $00000001; // Floating-Point unit on chip
  VME_FLAG   = $00000002; // Virtual Mode Extention
  DE_FLAG    = $00000004; // Debugging Extention
  PSE_FLAG   = $00000008; // Page Size Extention
  TSC_FLAG   = $00000010; // Time Stamp Counter
  MSR_FLAG   = $00000020; // Model Specific Registers
  PAE_FLAG   = $00000040; // Physical Address Extention
  MCE_FLAG   = $00000080; // Machine Check Exception
  CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  BIT_10     = $00000400; // Reserved, do not count on value
  SEP_FLAG   = $00000800; // Fast System Call
  MTRR_FLAG  = $00001000; // Memory Type Range Registers
  PGE_FLAG   = $00002000; // Page Global Enable
  MCA_FLAG   = $00004000; // Machine Check Architecture
  CMOV_FLAG  = $00008000; // Conditional Move Instruction
  PAT_FLAG   = $00010000; // Page Attribute Table
  PSE36_FLAG = $00020000; // 36-bit Page Size Extention
  BIT_18     = $00040000; // Reserved, do not count on value
  BIT_19     = $00080000; // Reserved, do not count on value
  BIT_20     = $00100000; // Reserved, do not count on value
  BIT_21     = $00200000; // Reserved, do not count on value
  BIT_22     = $00400000; // Reserved, do not count on value
  MMX_FLAG   = $00800000; // MMX technology
  FXSR_FLAG  = $01000000; // Fast Floating Point Save and Restore
  BIT_25     = $02000000; // Reserved, do not count on value
  BIT_26     = $04000000; // Reserved, do not count on value
  BIT_27     = $08000000; // Reserved, do not count on value
  BIT_28     = $10000000; // Reserved, do not count on value
  BIT_29     = $20000000; // Reserved, do not count on value
  BIT_30     = $40000000; // Reserved, do not count on value
  BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ AMD Standard Feature Flags }

  AMD_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  AMD_VME_FLAG   = $00000002; // Virtual Mode Extention
  AMD_DE_FLAG    = $00000004; // Debugging Extention
  AMD_PSE_FLAG   = $00000008; // Page Size Extention
  AMD_TSC_FLAG   = $00000010; // Time Stamp Counter
  AMD_MSR_FLAG   = $00000020; // Model Specific Registers
  AMD_BIT_6      = $00000040; // Reserved, do not count on value
  AMD_MCE_FLAG   = $00000080; // Machine Check Exception
  AMD_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  AMD_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  AMD_BIT_10     = $00000400; // Reserved, do not count on value
  AMD_BIT_11     = $00000800; // Reserved, do not count on value
  AMD_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  AMD_PGE_FLAG   = $00002000; // Page Global Enable
  AMD_BIT_14     = $00004000; // Reserved, do not count on value
  AMD_CMOV_FLAG  = $00008000; // Conditional Move Instruction
  AMD_BIT_16     = $00010000; // Reserved, do not count on value
  AMD_BIT_17     = $00020000; // Reserved, do not count on value
  AMD_BIT_18     = $00040000; // Reserved, do not count on value
  AMD_BIT_19     = $00080000; // Reserved, do not count on value
  AMD_BIT_20     = $00100000; // Reserved, do not count on value
  AMD_BIT_21     = $00200000; // Reserved, do not count on value
  AMD_BIT_22     = $00400000; // Reserved, do not count on value
  AMD_MMX_FLAG   = $00800000; // MMX technology
  AMD_BIT_24     = $01000000; // Reserved, do not count on value
  AMD_BIT_25     = $02000000; // Reserved, do not count on value
  AMD_BIT_26     = $04000000; // Reserved, do not count on value
  AMD_BIT_27     = $08000000; // Reserved, do not count on value
  AMD_BIT_28     = $10000000; // Reserved, do not count on value
  AMD_BIT_29     = $20000000; // Reserved, do not count on value
  AMD_BIT_30     = $40000000; // Reserved, do not count on value
  AMD_BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ AMD Enhanced Feature Flags }

  EAMD_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  EAMD_VME_FLAG   = $00000002; // Virtual Mode Extention
  EAMD_DE_FLAG    = $00000004; // Debugging Extention
  EAMD_PSE_FLAG   = $00000008; // Page Size Extention
  EAMD_TSC_FLAG   = $00000010; // Time Stamp Counter
  EAMD_MSR_FLAG   = $00000020; // Model Specific Registers
  EAMD_BIT_6      = $00000040; // Reserved, do not count on value
  EAMD_MCE_FLAG   = $00000080; // Machine Check Exception
  EAMD_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  EAMD_BIT_9      = $00000200; // Reserved, do not count on value
  EAMD_BIT_10     = $00000400; // Reserved, do not count on value
  EAMD_SEP_FLAG   = $00000800; // Fast System Call
  EAMD_BIT_12     = $00001000; // Reserved, do not count on value
  EAMD_PGE_FLAG   = $00002000; // Page Global Enable
  EAMD_BIT_14     = $00004000; // Reserved, do not count on value
  EAMD_ICMOV_FLAG = $00008000; // Integer Conditional Move Instruction
  EAMD_FCMOV_FLAG = $00010000; // Floating Point Conditional Move Instruction
  EAMD_BIT_17     = $00020000; // Reserved, do not count on value
  EAMD_BIT_18     = $00040000; // Reserved, do not count on value
  EAMD_BIT_19     = $00080000; // Reserved, do not count on value
  EAMD_BIT_20     = $00100000; // Reserved, do not count on value
  EAMD_BIT_21     = $00200000; // Reserved, do not count on value
  EAMD_BIT_22     = $00400000; // Reserved, do not count on value
  EAMD_MMX_FLAG   = $00800000; // MMX technology
  EAMD_BIT_24     = $01000000; // Reserved, do not count on value
  EAMD_BIT_25     = $02000000; // Reserved, do not count on value
  EAMD_BIT_26     = $04000000; // Reserved, do not count on value
  EAMD_BIT_27     = $08000000; // Reserved, do not count on value
  EAMD_BIT_28     = $10000000; // Reserved, do not count on value
  EAMD_BIT_29     = $20000000; // Reserved, do not count on value
  EAMD_BIT_30     = $40000000; // Reserved, do not count on value
  EAMD_3DNOW_FLAG = DWORD($80000000); // AMD 3DNOW! Technology

{ Cyrix Standard Feature Flags }

  CYRIX_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  CYRIX_VME_FLAG   = $00000002; // Virtual Mode Extention
  CYRIX_DE_FLAG    = $00000004; // Debugging Extention
  CYRIX_PSE_FLAG   = $00000008; // Page Size Extention
  CYRIX_TSC_FLAG   = $00000010; // Time Stamp Counter
  CYRIX_MSR_FLAG   = $00000020; // Model Specific Registers
  CYRIX_PAE_FLAG   = $00000040; // Physical Address Extention
  CYRIX_MCE_FLAG   = $00000080; // Machine Check Exception
  CYRIX_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  CYRIX_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  CYRIX_BIT_10     = $00000400; // Reserved, do not count on value
  CYRIX_BIT_11     = $00000800; // Reserved, do not count on value
  CYRIX_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  CYRIX_PGE_FLAG   = $00002000; // Page Global Enable
  CYRIX_MCA_FLAG   = $00004000; // Machine Check Architecture
  CYRIX_CMOV_FLAG  = $00008000; // Conditional Move Instruction
  CYRIX_BIT_16     = $00010000; // Reserved, do not count on value
  CYRIX_BIT_17     = $00020000; // Reserved, do not count on value
  CYRIX_BIT_18     = $00040000; // Reserved, do not count on value
  CYRIX_BIT_19     = $00080000; // Reserved, do not count on value
  CYRIX_BIT_20     = $00100000; // Reserved, do not count on value
  CYRIX_BIT_21     = $00200000; // Reserved, do not count on value
  CYRIX_BIT_22     = $00400000; // Reserved, do not count on value
  CYRIX_MMX_FLAG   = $00800000; // MMX technology
  CYRIX_BIT_24     = $01000000; // Reserved, do not count on value
  CYRIX_BIT_25     = $02000000; // Reserved, do not count on value
  CYRIX_BIT_26     = $04000000; // Reserved, do not count on value
  CYRIX_BIT_27     = $08000000; // Reserved, do not count on value
  CYRIX_BIT_28     = $10000000; // Reserved, do not count on value
  CYRIX_BIT_29     = $20000000; // Reserved, do not count on value
  CYRIX_BIT_30     = $40000000; // Reserved, do not count on value
  CYRIX_BIT_31     = DWORD($80000000); // Reserved, do not count on value

{ Cyrix Enhanced Feature Flags }

  ECYRIX_FPU_FLAG   = $00000001; // Floating-Point unit on chip
  ECYRIX_VME_FLAG   = $00000002; // Virtual Mode Extention
  ECYRIX_DE_FLAG    = $00000004; // Debugging Extention
  ECYRIX_PSE_FLAG   = $00000008; // Page Size Extention
  ECYRIX_TSC_FLAG   = $00000010; // Time Stamp Counter
  ECYRIX_MSR_FLAG   = $00000020; // Model Specific Registers
  ECYRIX_PAE_FLAG   = $00000040; // Physical Address Extention
  ECYRIX_MCE_FLAG   = $00000080; // Machine Check Exception
  ECYRIX_CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  ECYRIX_APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  ECYRIX_SEP_FLAG   = $00000400; // Fast System Call
  ECYRIX_BIT_11     = $00000800; // Reserved, do not count on value
  ECYRIX_MTRR_FLAG  = $00001000; // Memory Type Range Registers
  ECYRIX_PGE_FLAG   = $00002000; // Page Global Enable
  ECYRIX_MCA_FLAG   = $00004000; // Machine Check Architecture
  ECYRIX_ICMOV_FLAG = $00008000; // Integer Conditional Move Instruction
  ECYRIX_FCMOV_FLAG = $00010000; // Floating Point Conditional Move Instruction
  ECYRIX_BIT_17     = $00020000; // Reserved, do not count on value
  ECYRIX_BIT_18     = $00040000; // Reserved, do not count on value
  ECYRIX_BIT_19     = $00080000; // Reserved, do not count on value
  ECYRIX_BIT_20     = $00100000; // Reserved, do not count on value
  ECYRIX_BIT_21     = $00200000; // Reserved, do not count on value
  ECYRIX_BIT_22     = $00400000; // Reserved, do not count on value
  ECYRIX_MMX_FLAG   = $00800000; // MMX technology
  ECYRIX_EMMX_FLAG  = $01000000; // Extended MMX Technology
  ECYRIX_BIT_25     = $02000000; // Reserved, do not count on value
  ECYRIX_BIT_26     = $04000000; // Reserved, do not count on value
  ECYRIX_BIT_27     = $08000000; // Reserved, do not count on value
  ECYRIX_BIT_28     = $10000000; // Reserved, do not count on value
  ECYRIX_BIT_29     = $20000000; // Reserved, do not count on value
  ECYRIX_BIT_30     = $40000000; // Reserved, do not count on value
  ECYRIX_BIT_31     = DWORD($80000000); // Reserved, do not count on value

const
  IntelCacheDescription: array [0..13] of TCacheInfo = (
    (D: $01; I: 'Instruction TLB, 4Kb pages, 4-way set associative, 32 entries';),
    (D: $02; I: 'Instruction TLB, 4Mb pages, fully associative, 2 entries'),
    (D: $03; I: 'Data TLB, 4Kb pages, 4-way set associative, 64 entries'),
    (D: $04; I: 'Data TLB, 4Mb pages, 4-way set associative, 8 entries'),
    (D: $06; I: '8KB instruction cache, 4-way set associative, 32 byte line size'),
    (D: $08; I: '16KB instruction cache, 4-way set associative, 32 byte line size'),
    (D: $0A; I: '8KB data cache 2-way set associative, 32 byte line size'),
    (D: $0C; I: '16KB data cache, 4-way set associative, 32 byte line size'),
    (D: $40; I: 'No L2 cache'),
    (D: $41; I: 'Unified cache, 32 byte cache line, 4-way set associative, 128Kb'),
    (D: $42; I: 'Unified cache, 32 byte cache line, 4-way set associative, 256Kb'),
    (D: $43; I: 'Unified cache, 32 byte cache line, 4-way set associative, 512Kb'),
    (D: $44; I: 'Unified cache, 32 byte cache line, 4-way set associative, 1Mb'),
    (D: $45; I: 'Unified cache, 32 byte cache line, 4-way set associative, 2Mb'));

procedure GetCpuInfo(var CpuInfo: TCpuInfo);

function GetIntelCacheDescription(const D: Byte): string;
function RoundFrequency(const Frequency: Integer): Integer;
function GetCPUSpeed: TFreqInfo;  // TODO CCP Change to procedure
function CPUID: TCpuInfo;
function TestFDIVInstruction: Boolean;

//------------------------------------------------------------------------------
// Alloc granularity
//------------------------------------------------------------------------------

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);

//------------------------------------------------------------------------------
// Public global variables
//------------------------------------------------------------------------------

var
  ProcessorCount: Cardinal = 0;
  AllocGranularity: Cardinal = 0;

implementation

uses
  SysUtils, TLHelp32, Winsock,
  JclFileUtils, JclRegistry, JclStrings, JclWin32;

//==============================================================================
// Environment
//==============================================================================

function DelEnvironmentVar(const Name: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), nil);
end;

//------------------------------------------------------------------------------

function ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
begin
  R := ExpandEnvironmentStrings(PChar(Value), nil, 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
//    SetLength(Expanded, StrLen(PChar(Value)));       BUG0001  ESF  2000/06/04
//    SetLength(Expanded, StrLen(PChar(Expanded)));   // BUG0001  ESF  2000/06/04
      StrResetLength(Expanded);
    Value := Expanded;
  end;
end;

//------------------------------------------------------------------------------

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
var
  R: DWORD;
//  P: Integer;
begin
  R := GetEnvironmentVariable(PChar(Name), nil, 0);
  SetLength(Value, R);
  R := GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R <> 0;
  if not Result then
    Value := ''
  else
  begin
    SetLength(Value, R);
//    P := Pos(#13#10, Value);                         BUG0002  ESF  2000/06/04
//    if P > 0 then Delete(Value, P, Length(Value) - P + 1);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;

//------------------------------------------------------------------------------

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.Clear;
  Raw := GetEnvironmentStrings;
  try
    MultiSzToStrings(Vars, Raw);
    Result := True;
  finally
    FreeEnvironmentStrings(Raw);
  end;
  if Expand then
  begin
    for I := 0 to Vars.Count - 1 do
    begin
      Expanded := Vars[I];
      if ExpandEnvironmentVar(Expanded) then
        Vars[I] := Expanded;
    end;
  end;
end;

//------------------------------------------------------------------------------

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

//==============================================================================
// Common Folders
//==============================================================================

const
  HKLM_CURRENT_VERSION_WINDOWS = 'Software\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'Software\Microsoft\Windows NT\CurrentVersion';

// Utility function which returns the Windows independent CurrentVersion key
// inside HKEY_LOCAL_MACHINE

function REG_CURRENT_VERSION: string;
begin
  if IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

//------------------------------------------------------------------------------

function GetCommonFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS,
    'CommonFilesDir', '');
end;

//------------------------------------------------------------------------------

function GetCurrentFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetCurrentDirectory(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    // Required := GetCurrentDirectory(Required, PChar(Result));
    // SetLength(Result, Required);
    GetCurrentDirectory(Required, PChar(Result));
    StrResetLength(Result);
  end;
end;

//------------------------------------------------------------------------------

function GetProgramFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS,
    'ProgramFilesDir', '');
end;

//------------------------------------------------------------------------------

function GetWindowsFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetWindowsDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    // Required := GetWindowsDirectory(PChar(Result), Required);
    // SetLength(Result, Required);
    GetWindowsDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

//------------------------------------------------------------------------------

function GetWindowsSystemFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    // Required := GetSystemDirectory(PChar(Result), Required);
    // SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

//------------------------------------------------------------------------------

function GetWindowsTempFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    // Required := GetTempPath(Required, PChar(Result));
    // SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
    Result := PathRemoveSeparator(Result);
  end;
end;

//==============================================================================
// Identification
//==============================================================================

type
  TVolumeInfoKind = (vikName, vikSerial, vikFileSystem);

function GetVolumeInfoHelper(const Drive: string; InfoKind: TVolumeInfoKind): string;
var
  VolumeSerialNumber: DWORD;
  MaximumComponentLength: DWORD;
  Flags: DWORD;
  Name: array [0..MAX_PATH] of Char;
  FileSystem: array [0..15] of Char;
  ErrorMode: Cardinal;
  DriveStr: string;
begin
  // TODO Perform better checking of Drive param or document that no checking is
  // performed. RM Suggested:
  // DriveStr := Drive;
  // if (Length(Drive) < 2) or (Drive[2] <> ':') then DriveStr := GetCurrentFolder;
  // DriveStr  := DriveStr[1] + ':\';
  Result := '';
  DriveStr := Drive + ':\';
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(DriveStr), Name, SizeOf(Name), @VolumeSerialNumber,
      MaximumComponentLength, Flags, FileSystem, SizeOf(FileSystem)) then
    case InfoKind of
      vikName:
        Result := StrPas(Name);
      vikSerial:
        begin
          Result := IntToHex(HiWord(VolumeSerialNumber), 4) + '-' +
          IntToHex(LoWord(VolumeSerialNumber), 4);
        end;
      vikFileSystem:
        Result := StrPas(FileSystem);
    end;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function GetVolumeName(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikName);
end;

//------------------------------------------------------------------------------

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikSerial);
end;

//------------------------------------------------------------------------------

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikFileSystem);
end;

//------------------------------------------------------------------------------

function GetIPAddress(const HostName: string): string;
var
  R: Integer;
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  Host: string;
  SockAddr: TSockAddrIn;
begin
  Result := '';
  R := WSAStartup(MakeWord(1, 1), WSAData);
  if R = 0 then
  try
    Host := HostName;
    if Host = '' then
    begin
      SetLength(Host, MAX_PATH);
      GetHostName(PChar(Host), MAX_PATH);
      // Host := GetLocalComputerName;
    end;
    HostEnt := GetHostByName(PChar(Host));
    if HostEnt <> nil then
    begin
      SockAddr.sin_addr.S_addr := Longint(PLongint(HostEnt^.h_addr_list^)^);
      Result := inet_ntoa(SockAddr.sin_addr);
    end;
    //if Assigned(HostEnt) then with HostEnt^ do
    //begin
    //  Result := Format('%u.%u.%u.%u', [Byte(h_addr^[0]), Byte(h_addr^[1]),
    //    Byte(h_addr^[2]), Byte(h_addr^[3])]);
    //end;
  finally
    WSACleanup;
  end;
end;

//------------------------------------------------------------------------------

function GetLocalComputerName: string;
var
  Count: DWORD;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, Count);
  GetComputerName(PChar(Result), Count);
  // SetLength(Result, StrLen(PChar(Result)));
  StrResetLength(Result);
end;

//------------------------------------------------------------------------------

function GetLocalUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  SetLength(Result, Count);
  GetUserName(PChar(Result), Count);
  // SetLength(Result, StrLen(PChar(Result)));
  StrResetLength(Result);
end;

//------------------------------------------------------------------------------

function GetRegisteredCompany: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION,
    'RegisteredOrganization', '');
end;

//------------------------------------------------------------------------------

function GetRegisteredOwner: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION,
    'RegisteredOwner', '');
end;

//------------------------------------------------------------------------------

function RunningProcessesList(List: TStrings): Boolean;

  function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if SnapProcHandle <> INVALID_HANDLE_VALUE then
    begin
      Result := True;
      ProcEntry.dwSize := SizeOf(ProcEntry);
      NextProc := Process32First(SnapProcHandle, ProcEntry);
      while NextProc do
      begin
        List.AddObject(ProcEntry.szExeFile, Pointer(ProcEntry.th32ProcessID));
        NextProc := Process32Next(SnapProcHandle, ProcEntry);
      end;
      CloseHandle(SnapProcHandle);
    end
    else
      Result := False;
  end;

  function BuildListPS: Boolean;
  var
    PIDs: array [0..1024] of DWORD;
    Handle: THandle;
    Needed: DWORD;
    I: Integer;
    ModuleFileName: array [0..MAX_PATH] of Char;
  begin
    Result := EnumProcesses(@PIDs, SizeOf(PIDs), Needed);
    if not Result then
      Exit;
    for I := 0 to (Needed div SizeOf(DWORD)) - 1 do
      if PIDs[I] <> 0 then
      begin
        Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PIDs[I]);
        if Handle <> 0 then
        begin
          if GetModuleFileNameEx(Handle, 0, ModuleFileName, SizeOf(ModuleFileName)) = 0 then
            List.AddObject('[System]', Pointer(INVALID_HANDLE_VALUE))
          else
            List.AddObject(ModuleFileName, Pointer(PIDs[I]));
          CloseHandle(Handle);
        end;
      end;
  end;

begin
  List.BeginUpdate;
  try
    List.Clear;
    if GetWindowsVersion in [wvWinNT3, wvWinNT4] then
      Result := BuildListPS
    else
      Result := BuildListTH;
  finally
    List.EndUpdate;
  end;
end;

//==============================================================================
// Version Information
//==============================================================================

function GetWindowsVersion: TWindowsVersion;
const
  BuildNumbers: array [TWindowsVersion] of Integer =
    (0, 0, 67109975, 67766222, 67766446, 0, 1381, 2195);
begin
// TWindowsVersion = (wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinNT3, wvWinNT4, wvWin2000);
//===========================================================================================
// Win32Platform        1           1           1        1          2         2          2
// Win32MajorVersion    4           4           4        4          3         4          5
// Win32MinorVersion    0           0           10       10         ?         0          0
// Win32BuildNumber     ?        67109975    67766222  67766446     ?        1381       2195
// Win32CSDVersion      ?        '[ ]B'         ''      '[ ]A'      SP        SP         SP
// "Qamar Ali" <MQamar@geocities.com>
// "Brian Cahill" <BrianCahill@SoftHome.net>
// "Anthony Steele"
// "Thomas Siebers" <thomas.siebers@planet-interkom.de>
// "Nial R. Scott" <niall@gryphonsd.co.uk>
  Result := wvUnknown;
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0:
          if Trim(Win32CSDVersion) = 'B' then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10:
          if Trim(Win32CSDVersion) = 'A' then
            Result := wvWin98SE
          else
            Result := wvWin98;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          Result := wvWinNT3;
        4:
          Result := wvWinNT4;
        5:
          Result := wvWin2000;
      end;
  end;
  if (BuildNumbers[Result] <> 0) and
     (Win32BuildNumber <> BuildNumbers[Result]) then
    Result := wvUnknown;
end;

//------------------------------------------------------------------------------

function NtProductType: TNtProductType;
const
  ProductType = 'System\CurrentControlSet\Control\ProductOptions';
var
  Product: string;
begin
  Product := RegReadStringDef(HKEY_LOCAL_MACHINE, ProductType, 'ProductType', '');
  if CompareText(Product, 'WinNT') = 0 then
    Result :=  ptWorkStation
  else
  if CompareText(Product, 'ServerNT') = 0 then
    Result := ptServer
  else
  if CompareText(Product, 'LanmanNT') = 0 then
    Result := ptAdvancedServer
  else
    Result := ptUnknown;
end;

//==============================================================================
// Hardware
//==============================================================================

// Helper function for GetMacAddress()
// Converts the adapter_address array to a string

function AdapterToString(Adapter: TAdapterStatus): string;
begin
  with Adapter do
    Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x', [
      Integer(adapter_address[0]), Integer(adapter_address[1]),
      Integer(adapter_address[2]), Integer(adapter_address[3]),
      Integer(adapter_address[4]), Integer(adapter_address[5])]);
end;

//------------------------------------------------------------------------------

{ TODO
  Return value should be Integer. Tells expected number of network cards.
  -1 == Machine not found.  0 == (own) machine has no network card.
  Machine should be converted to upper case.
  TODO: complete the set of functions to connect the tuples of
  <netbios name, host name, IP address, Mac address> }

function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;
var
  NCB: TNCB;
  Enum: TLanaEnum;
  I: Integer;
  Adapter: ASTAT;
  MachineName: string;
begin
  Result := -1;
  Addresses.Clear;
  MachineName := UpperCase(Machine);
  if MachineName = '' then
    MachineName := '*';
  FillChar(NCB, SizeOf(NCB), #0);
  NCB.ncb_command := NCBENUM;
  NCB.ncb_buffer := Pointer(@Enum);
  NCB.ncb_length := SizeOf(Enum);
  if NetBios(@NCB) = NRC_GOODRET then
  begin
    Result := Enum.Length;
    for I := 0 to Ord(Enum.Length) - 1 do
    begin
      FillChar(NCB, SizeOf(TNCB), #0);
      NCB.ncb_command := NCBRESET;
      NCB.ncb_lana_num := Enum.lana[I];
      if NetBios(@NCB) = NRC_GOODRET then
      begin
        FillChar(NCB, SizeOf(TNCB), #0);
        NCB.ncb_command := NCBASTAT;
        NCB.ncb_lana_num := Enum.lana[i];
        StrLCopy(NCB.ncb_callname, PChar(MachineName), NCBNAMSZ);
        StrPCopy(@NCB.ncb_callname[Length(MachineName)],
          StringOfChar(' ', NCBNAMSZ - Length(MachineName)));
        NCB.ncb_buffer := PChar(@Adapter);
        NCB.ncb_length := SizeOf(Adapter);
        if NetBios(@NCB) = NRC_GOODRET then
          Addresses.Add(AdapterToString(Adapter.adapt));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function ReadTimeStampCounter: Int64; assembler;
asm
        {$IFNDEF SUPPORTS_INT64}
        MOV     ECX, EAX
        DW      $310F
        MOV     [ECX], EAX
        MOV     [ECX+$04], EDX
        {$ELSE}
        DW      $310F
        {$ENDIF}
end;

//------------------------------------------------------------------------------

function GetIntelCacheDescription(const D: Byte): string;
var
  I: Integer;
begin
  Result := '';
  if D <> 0 then
  begin
    for I := Low(IntelCacheDescription) to High(IntelCacheDescription) do
    begin
      if IntelCacheDescription[I].D = D then
      begin
        Result := IntelCacheDescription[I].I;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure GetCpuInfo(var CpuInfo: TCpuInfo);
begin
  CpuInfo := CPUID;
  CpuInfo.IsFDIVOK := TestFDIVInstruction;
  if CpuInfo.HasInstruction then
  begin
    if (CpuInfo.Features and TSC_FLAG = TSC_FLAG) then
      CpuInfo.FrequencyInfo := GetCpuSpeed;
    if (CpuInfo.Features and MMX_FLAG) = MMX_FLAG then
      CpuInfo.MMX := True
    else
      CpuInfo.MMX := False;
  end;
end;

//------------------------------------------------------------------------------

function RoundFrequency(const Frequency: Integer): Integer;
const
  NF: array [0..8] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100);
var
  Freq, RF: Integer;
  I: Byte;
  Hi, Lo: Byte;
begin
  RF := 0;
  Freq := Frequency mod 100;
  for I := 0 to 8 do
  begin
    if Freq < NF[i] then
    begin
      Hi := I;
      Lo := I - 1;
      if (NF[Hi] - Freq) > (Freq - NF[Lo]) then
        RF := NF[Lo] - Freq
      else
        RF := NF[Hi] - Freq;
      Break;
    end;
  end;
  Result := Frequency + RF;
end;

//------------------------------------------------------------------------------

function GetCPUSpeed: TFreqInfo;
var
  {$IFNDEF COMPILER4_UP}
  T0, T1: TLargeInteger;
  CountFreq: TLargeInteger;
  {$ELSE}
  T0, T1: TULargeInteger;
  CountFreq: TULargeInteger;
  {$ENDIF}
  CpuSpeed: TFreqInfo;
  Freq, Freq2, Freq3, Total: Integer;
  TotalCycles, Cycles: Cardinal;
  Stamp0, Stamp1: Cardinal;
  TotalTicks, Ticks: Cardinal;
  Tries, IPriority: Integer;
  hThread: THandle;
begin
  Freq  := 0;
  Freq2 := 0;
  Freq3 := 0;
  Tries := 0;
  TotalCycles := 0;
  TotalTicks := 0;
  Total := 0;

  hThread := GetCurrentThread();
  {$IFNDEF COMPILER4_UP}
  if not QueryPerformanceFrequency(CountFreq) then
  {$ELSE}
  if not QueryPerformanceFrequency(Int64(CountFreq)) then
  {$ENDIF}
  begin
    Result := CpuSpeed;
  end
  else
  begin
    while ((Tries < 3 ) or ((Tries < 20) and ((Abs(3 * Freq - Total) > 3) or
          (Abs(3 * Freq2 - Total) > 3) or (Abs(3 * Freq3 - Total) > 3)))) do
    begin
      Inc(Tries);
      Freq3 := Freq2;
      Freq2 := Freq;
      {$IFNDEF COMPILER4_UP}
      QueryPerformanceCounter(T0);
      {$ELSE}
      QueryPerformanceCounter(Int64(T0));
      {$ENDIF}
      T1.LowPart := T0.LowPart;
      T1.HighPart := T0.HighPart;

      iPriority := GetThreadPriority(hThread);
      if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      begin
        SetThreadPriority(hThread, THREAD_PRIORITY_TIME_CRITICAL);
      end;
      while (T1.LowPart - T0.LowPart) < 50 do
      begin
        {$IFNDEF COMPILER4_UP}
        QueryPerformanceCounter(T1);
        {$ELSE}
        QueryPerformanceCounter(Int64(T1));
        {$ENDIF}
        asm
          PUSH    EAX
          PUSH    EDX
          DB      0Fh             // Read Time
          DB      31h             // Stamp Counter
          MOV     Stamp0, EAX
          POP     EDX
          POP     EAX
        end;
      end;
      T0.LowPart := T1.LowPart;
      T0.HighPart := T1.HighPart;

      while (T1.LowPart - T0.LowPart) < 1000 do
      begin
        {$IFNDEF COMPILER4_UP}
        QueryPerformanceCounter(T1);
        {$ELSE}
        QueryPerformanceCounter(Int64(T1));
        {$ENDIF}
        asm
          PUSH    EAX
          PUSH    EDX
          DB      0Fh             // Read Time
          DB      31h             // Stamp Counter
          MOV     Stamp1, EAX
          POP     EDX
          POP     EAX
        end;
      end;

      if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      begin
        SetThreadPriority(hThread, iPriority);
      end;

      Cycles := Stamp1 - Stamp0;
      Ticks := T1.LowPart - T0.LowPart;
      Ticks := Ticks * 100000;
      Ticks := Round(Ticks / (CountFreq.LowPart / 10));
      TotalTicks := TotalTicks + Ticks;
      TotalCycles := TotalCycles + Cycles;

      Freq := Round(Cycles / Ticks);

      Total := Freq + Freq2 + Freq3;
    end;
    Freq3 := Round((TotalCycles * 10) / TotalTicks);
    Freq2 := Round((TotalCycles * 100) / TotalTicks);

    if Freq2 - (Freq3 * 10) >= 6 then
      Inc(Freq3);

    CpuSpeed.RawFreq := Round(TotalCycles / TotalTicks);
    CpuSpeed.NormFreq := CpuSpeed.RawFreq;

    Freq := CpuSpeed.RawFreq * 10;
    if (Freq3 - Freq) >= 6 then
      Inc(CpuSpeed.NormFreq);

    CpuSpeed.ExTicks := TotalTicks;
    CpuSpeed.InCycles := TotalCycles;

    CpuSpeed.NormFreq := RoundFrequency(CpuSpeed.NormFreq);
    Result := CpuSpeed;
  end;
end;

//------------------------------------------------------------------------------

// Helper function for CPUID. Initializes Intel specific fields.

procedure IntelSpecific(var CpuInfo: TCpuInfo);
var
  I: Integer;
begin
  with CpuInfo do
  begin
    Manufacturer := 'Intel';
    CpuType := CPU_TYPE_INTEL;
    if HasCacheInfo = True then
      with CPUInfo.IntelSpecific do
      begin
        L2Cache := 0;
        for I := 1 to 15 do
          case CacheDescriptors[I] of
            $40:
              L2Cache := 0;
            $41:
              L2Cache := 128;
            $42:
              L2Cache := 256;
            $43:
              L2Cache := 512;
            $44:
              L2Cache := 1024;
            $45:
              L2Cache := 2048;
          end;
      end;
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          case Model of
            1:
              CpuName := 'Intel 486DX Processor';
            2:
              CpuName := 'Intel 486SX Processor';
            3:
              CpuName := 'Intel DX2 Processor';
            4:
              CpuName := 'Intel 486 Processor';
            5:
              CpuName := 'Intel SX2 Processor';
            7:
              CpuName := 'Write-Back Enhanced Intel DX2 Processor';
            8:
              CpuName := 'Intel DX4 Processor';
          else
            CpuName := 'Intel 486 Processor';
          end;
        5:
          CpuName := 'Pentium';
        6:
          case Model of
            1:
              CpuName := 'Pentium Pro';
            3:
              CpuName := 'Pentium II';
            5:
              case IntelSpecific.L2Cache of
                0:
                  CpuName := 'Celeron';
                1024:
                  CpuName := 'Pentium II Xeon';
                2048:
                  CpuName := 'Pentium II Xeon';
              else
                CpuName := 'Pentium II';
              end;
          else
            StrPCopy(CpuName, Format('P6 (Model %d)', [Model]));
          end;
      else
        StrPCopy(CpuName, Format('P%d', [Family]));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Helper function for CPUID. Initializes Cyrix specific fields.

procedure CyrixSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'Cyrix';
    CpuType := CPU_TYPE_CYRIX;
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          CpuName := 'Cyrix MediaGX';
        5:
          case Model of
            2:
              CpuName := 'Cyrix 6x86';
            4:
              CpuName := 'Cyrix GXm';
          end;
        6:
          CpuName := '6x86MX';
      else
        StrPCopy(CpuName, Format('%dx86', [Family]));
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Helper function for CPUID. Initializes AMD specific fields.

procedure AMDSpecific(var CpuInfo: TCpuInfo);
begin
  with CpuInfo do
  begin
    Manufacturer := 'AMD';
    CpuType := CPU_TYPE_AMD;
    if not HasExtendedInfo then
    begin
      case Family of
        4:
          CpuName := 'Am486(R) or Am5x86';
        5:
          case Model of
            0:
              CpuName := 'AMD-K5 (Model 0)';
            1:
              CpuName := 'AMD-K5 (Model 1)';
            2:
              CpuName := 'AMD-K5 (Model 2)';
            3:
              CpuName := 'AMD-K5 (Model 3)';
            6:
              CpuName := 'AMD-K6(R)';
            7:
              CpuName := 'AMD-K6';
            8:
              CpuName := 'AMD-K6(R) -2';
            9:
              CpuName := 'AMD-K6(R) -3';
          else
            CpuName := 'Unknown AMD Model';
          end;
      else
        CpuName := 'Unknown AMD Chip';
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function CPUID: TCpuInfo;
var
  CPUInfo: TCpuInfo;
  HiVal: Cardinal;
  TimesToExecute, CurrentLoop: Byte;
begin
  asm
    MOV     [CPUInfo.HasInstruction], 0
    MOV     [CPUInfo.HasExtendedInfo], 0
    MOV     [CPUInfo.HasCacheInfo], 0
    MOV     [CPUInfo.PType], 0
    MOV     [CPUInfo.Model], 0
    MOV     [CPUInfo.Stepping], 0
    MOV     [CPUInfo.Features], 0
    MOV     [CPUInfo.FrequencyInfo.RawFreq], 0
    MOV     [CPUInfo.FrequencyInfo.NormFreq], 0
    MOV     [CPUInfo.FrequencyInfo.InCycles], 0
    MOV     [CPUInfo.FrequencyInfo.ExTicks], 0

    PUSH    EAX
    PUSH    EBP
    PUSH    EBX
    PUSH    ECX
    PUSH    EDI
    PUSH    EDX
    PUSH    ESI

  @@Check_80486:
    MOV     [CPUInfo.Family], 4
    PUSHFD
    POP     EAX
    MOV     ECX, EAX
    XOR     EAX, 200000H
    PUSH    EAX
    POPFD
    PUSHFD
    POP     EAX
    XOR     EAX, ECX
    JE      @@DONE_CPU_TYPE

  @@Has_CPUID_Instruction:
    MOV     [CPUInfo.HasInstruction], 1
    MOV     EAX, 0
    DB      0FH
    DB      0A2H

    MOV     HiVal, EAX
    MOV     DWORD PTR [CPUInfo.VendorIDString], EBX
    MOV     DWORD PTR [CPUInfo.VendorIDString + 4], EDX
    MOV     DWORD PTR [CPUInfo.VendorIDString + 8], ECX

  @@CHECK_INTEL:
    CMP     DWORD PTR [CPUInfo.VendorIDString], 'uneG'
    JNE     @@CHECK_AMD
    CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'Ieni'
    JNE     @@CHECK_AMD
    CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'letn'
    JNE     @@CHECK_AMD
    MOV     [CPUInfo.CpuType], CPU_TYPE_INTEL
    JMP     @@Standard_Functions

  @@CHECK_AMD:
    CMP     DWORD PTR [CPUInfo.VendorIDString], 'htuA'
    JNE     @@CHECK_CYRIX
    CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'itne'
    JNE     @@CHECK_CYRIX
    CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'DMAc'
    JNE     @@CHECK_CYRIX
    MOV     [CPUInfo.CpuType], CPU_TYPE_AMD
    JMP     @@CHECK_AMD_EXTENDED

  @@CHECK_CYRIX:
    CMP     DWORD PTR [CPUInfo.VendorIDString], 'iryC'
    JNE     @@Standard_Functions
    CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'snIx'
    JNE     @@Standard_Functions
    CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'daet'
    JNE     @@Standard_Functions
    MOV     [CPUInfo.CpuType], CPU_TYPE_CYRIX
    JMP     @@CHECK_CYRIX_EXTENDED

  @@CHECK_AMD_EXTENDED:
    MOV     EAX, 80000000h
    DB      0Fh
    DB      0A2h
    CMP     EAX, 0
    JE      @@Standard_Functions
    JMP     @@AMD_ONLY

  @@CHECK_CYRIX_EXTENDED:
    MOV     EAX, 80000000h
    DB      0Fh
    DB      0A2h
    CMP     EAX, 0
    JE      @@Standard_Functions
    JMP     @@CYRIX_ONLY

  @@Standard_Functions:
    CMP     HiVal, 1
    JL      @@DONE_CPU_TYPE
    MOV     EAX, 1
    DB      0FH
    DB      0A2H
    MOV     [CPUInfo.Features], EDX
    MOV     ECX, EAX
    AND     EAX, 3000H
    SHR     EAX, 12
    MOV     [CPUInfo.PType], AL
    MOV     EAX, ECX
    AND     EAX, 0F00H
    SHR     EAX, 8
    MOV     [CPUInfo.Family], AL
    MOV     EAX, ECX
    AND     EAX, 00F0H
    SHR     EAX, 4
    MOV     [CPUInfo.MODEL], AL
    MOV     EAX, ECX
    AND     EAX, 000FH
    MOV     [CPUInfo.Stepping], AL
    CMP     DWORD PTR [CPUInfo.VendorIDString], 'uneG'
    JNE     @@DONE_CPU_TYPE
    CMP     DWORD PTR [CPUInfo.VendorIDString + 4], 'Ieni'
    JNE     @@DONE_CPU_TYPE
    CMP     DWORD PTR [CPUInfo.VendorIDString + 8], 'letn'
    JNE     @@DONE_CPU_TYPE

  @@INTEL_STANDARD:
    CMP     HiVal, 2
    JL      @@DONE_CPU_TYPE
    MOV     CurrentLoop, 0
    MOV     [CPUInfo.HasCacheInfo], 1
    PUSH    ECX

  @@REPEAT_CACHE_QUERY:
    POP     ECX
    MOV     EAX, 2
    DB      0FH
    DB      0A2H
    INC     CurrentLoop
    CMP     CurrentLoop, 1
    JNE     @@DONE_CACHE_QUERY
    MOV     TimesToExecute, AL
    CMP     AL, 0
    JE      @@DONE_CPU_TYPE

  @@DONE_CACHE_QUERY:
    PUSH    ECX
    MOV     CL, CurrentLoop
    SUB     CL, TimesToExecute
    JNZ     @@REPEAT_CACHE_QUERY
    POP     ECX
    MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors], EAX
    MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 4], EBX
    MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 8], ECX
    MOV     DWORD PTR [CPUInfo.IntelSpecific.CacheDescriptors + 12], EDX
    JMP     @@DONE_CPU_TYPE

  @@AMD_ONLY:
    MOV     HiVal, EAX
    MOV     EAX, 80000001h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    MOV     [CPUInfo.HasExtendedInfo], 1
    DB      0Fh
    DB      0A2h
    MOV     ECX, EAX
    AND     EAX, 0F000H
    SHR     EAX, 12
    MOV     [CPUInfo.PType], AL
    MOV     EAX, ECX
    AND     EAX, 0F00H
    SHR     EAX, 8
    MOV     [CPUInfo.Family], AL
    MOV     EAX, ECX
    AND     EAX, 00F0H
    SHR     EAX, 4
    MOV     [CPUInfo.MODEL], AL
    MOV     EAX, ECX
    AND     EAX, 000FH
    MOV     [CPUInfo.Stepping], AL
    MOV     [CPUInfo.Features], EDX

    MOV     EAX, 80000002h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

    MOV     EAX, 80000003h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

    MOV     EAX, 80000004h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

    MOV     EAX, 80000005h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    MOV     [CPUInfo.HasCacheInfo], 1
    DB      0Fh
    DB      0A2h
    MOV     WORD PTR [CPUInfo.AMDSpecific.InstructionTLB], BX
    SHR     EBX, 16
    MOV     WORD PTR [CPUInfo.AMDSpecific.DataTLB], BX
    MOV     DWORD PTR [CPUInfo.AMDSpecific.L1DataCache], ECX
    MOV     DWORD PTR [CPUInfo.AMDSpecific.L1ICache], EDX
    JMP     @@DONE_CPU_TYPE

  @@CYRIX_ONLY:
    MOV     HiVal, EAX
    MOV     EAX, 80000001h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    MOV     [CPUInfo.HasExtendedInfo], 1
    DB      0Fh
    DB      0A2h
    MOV     ECX, EAX
    AND     EAX, 0F000H
    SHR     EAX, 12
    MOV     [CPUInfo.PType], AL
    MOV     EAX, ECX
    AND     EAX, 0F00H
    SHR     EAX, 8
    MOV     [CPUInfo.Family], AL
    MOV     EAX, ECX
    AND     EAX, 00F0H
    SHR     EAX, 4
    MOV     [CPUInfo.MODEL], AL
    MOV     EAX, ECX
    AND     EAX, 000FH
    MOV     [CPUInfo.Stepping], AL
    MOV     [CPUInfo.Features], EDX

    MOV     EAX, 80000002h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 4], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 8], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 12], EDX

    MOV     EAX, 80000003h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName + 16], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 20], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 24], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 28], EDX

    MOV     EAX, 80000004h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CpuName + 32], EAX
    MOV     DWORD PTR [CPUInfo.CpuName + 36], EBX
    MOV     DWORD PTR [CPUInfo.CpuName + 40], ECX
    MOV     DWORD PTR [CPUInfo.CpuName + 44], EDX

    MOV     EAX, 80000005h
    CMP     HiVal, EAX
    JL      @@DONE_CPU_TYPE
    MOV     [CPUInfo.HasCacheInfo], 1
    DB      0Fh
    DB      0A2h
    MOV     DWORD PTR [CPUInfo.CyrixSpecific.TLBInfo], EBX
    MOV     DWORD PTR [CPUInfo.CyrixSpecific.L1CacheInfo], ECX

  @@DONE_CPU_TYPE:
    POP     ESI
    POP     EDX
    POP     EDI
    POP     ECX
    POP     EBX
    POP     EBP
    POP     EAX
  end;
  if CPUInfo.VendorIDString = 'GenuineIntel' then
    IntelSpecific(CpuInfo);
  if CPUInfo.VendorIDString = 'CyrixInstead' then
    CyrixSpecific(CpuInfo);
  if CPUInfo.VendorIDString = 'AuthenticAMD' then
    AMDSpecific(CpuInfo);
  Result := CPUInfo;
end;

//------------------------------------------------------------------------------

function TestFDIVInstruction: Boolean;
var
  TopNum: Double;
  BottomNum: Double;
  One: Double;
  ISOK: Boolean;
begin
  // The following code was found in Borlands fdiv.asm file in the
  // Delphi 3\Source\RTL\SYS directory, (I made some minor modifications)
  // therefore I cannot take credit for it.
  TopNum := 2658955;
  BottomNum := PI;
  One := 1;
  asm
    PUSH    EAX
    FLD     [TopNum]
    FDIV    [BottomNum]
    FMUL    [BottomNum]
    FSUBR   [TopNum]
    FCOMP   [One]
    FSTSW   AX
    SHR     EAX, 8
    AND     EAX, 01H
    MOV     ISOK, AL
    POP     EAX
  end;
  Result := ISOK;
end;

//==============================================================================
// Alloc granularity
//==============================================================================

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
begin
  if (Value mod AllocGranularity) <> 0 then
    if Up then
      Value := ((Value div AllocGranularity) + 1) * AllocGranularity
    else
      Value := (Value div AllocGranularity) * AllocGranularity;
end;

//------------------------------------------------------------------------------

procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);
begin
  if (Cardinal(Value) mod AllocGranularity) <> 0 then
    if Up then
      Value := Pointer(((Cardinal(Value) div AllocGranularity) + 1) * AllocGranularity)
    else
      Value := Pointer((Cardinal(Value) div AllocGranularity) * AllocGranularity);
end;

//==============================================================================
// Initialization
//==============================================================================

procedure InitSysInfo;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  ProcessorCount := SystemInfo.dwNumberOfProcessors;
  AllocGranularity := SystemInfo.dwAllocationGranularity;

  //  added per MVB request                ESF    2000/06/04
  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;

  case GetWindowsVersion of
    wvUnknown: ;
    wvWin95:
      IsWin95 := True;
    wvWin95OSR2:
      IsWin95OSR2 := True;
    wvWin98:
      IsWin98 := True;
    wvWin98SE:
      IsWin98SE := True;
    wvWinNT3:
      IsWinNT3 := True;
    wvWinNT4:
      IsWinNT4 := True;
    wvWin2000:
      IsWin2K := True;
  end;
end;

//------------------------------------------------------------------------------

function GetUserDomainName(const CurUser: string): string;
var
  Count1, Count2: DWORD;
  Sd: PSecurityDescriptor;
  Snu: SID_Name_Use;
begin
  Count1 := 0;
  Count2 := 0;
  Sd := nil;
  Snu := SIDTypeUser;

  // have the the API function determine the required buffer sizes
  LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu);
  SetLength(Result, Count2 + 1);

  Sd := AllocMem(Count1);

  if LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu) then
    StrResetLength(Result)
  else
    Result := EmptyStr;

  FreeMem(Sd);
end;

//------------------------------------------------------------------------------

function GetDomainName: string;
begin
  Result := GetUserDomainName(GetLocalUserName);
end;

//------------------------------------------------------------------------------

function CreateEnvironmentBlock(const Options: TEnvironmentOptions;
  const AdditionalVars: TStrings): PChar;
const
  RegLocalEnvironment = '\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\';
  RegUserEnvironment = '\Environment\';
var
  KeyNames, stlTemp: TStrings;
  strTemp, strName, strValue: string;
  I: Integer;
begin
  stlTemp := TStringList.Create;

  // get additional environment strings
  if eoAdditional in Options then
    for I := 0 to AdditionalVars.Count - 1 do
    begin
      strTemp := AdditionalVars[I];
      ExpandEnvironmentVar(strTemp);
      stlTemp.Add(strTemp);
    end;

  // get environment strings from local machine
  if eoLocalMachine in Options then
  begin
    KeyNames := TStringList.Create;
    if RegGetValueNames(HKEY_LOCAL_MACHINE, RegLocalEnvironment, KeyNames) then
    begin
      for I := 0 to KeyNames.Count - 1 do
      begin
        strName := KeyNames[I];
        strValue := RegReadString(HKEY_LOCAL_MACHINE, RegLocalEnvironment, strName);
        ExpandEnvironmentVar(strValue);
        stlTemp.Add(strName + '=' + strValue);
      end;
      KeyNames.Free;
    end;
  end;

  // get environment strings from current user
  if eoCurrentUser in Options then
  begin
    KeyNames := TStringLIst.Create;
    if RegGetValueNames(HKEY_CURRENT_USER, RegUserEnvironment, KeyNames) then
    begin
      for I := 0 to KeyNames.Count - 1 do
      begin
        strName := KeyNames[I];
        strValue := RegReadString(HKEY_CURRENT_USER, RegUserEnvironment, strName);
        ExpandEnvironmentVar(strValue);
        stlTemp.Add(strName + '=' + strValue);
      end;
      KeyNames.Free;
    end;
  end;

  // transform stringlist into multi-PChar
  StringsToMultiSz(Result, stlTemp);
  stlTemp.Free;
end;

initialization
  InitSysInfo;
end.
