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
{ The Original Code is JclWin32.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: June 01, 2000                                                 }
{                                                                              }
{******************************************************************************}

unit JclWin32;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows,
  {$IFDEF COMPILER5_UP}
  AccCtrl, AclApi,
  {$ENDIF}
  ShlObj;

//------------------------------------------------------------------------------
// Various Base Services declarations
//------------------------------------------------------------------------------

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
  stdcall; external 'kernel32.dll' name 'InterlockedExchangePointer';

function SignalObjectAndWait(hObjectToSignal: THandle; hObjectToWaitOn: THandle;
  dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall; external kernel32
  name 'SignalObjectAndWait';

//------------------------------------------------------------------------------
// Shell related declarations (missing/incorrect in different delphi versions)
//------------------------------------------------------------------------------

{$IFNDEF COMPILER4_UP}
{$IFDEF SUPPORTS_INTERFACE}

type
  IContextMenu2 = interface (IContextMenu)
    [SID_IContextMenu2]
    function HandleMenuMsg(uMsg: UINT; WParam: WPARAM; LParam: LPARAM): HRESULT; stdcall;
  end;

function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: PChar;
  nFolder: Integer; fCreate: BOOL): BOOL; stdcall; external
  'shell32.dll' name 'SHGetSpecialFolderPathA'

const
  SID_IQueryInfo = '{00021500-0000-0000-C000-000000000046}';

type
  IQueryInfo = interface (IUnknown)
    [SID_IQueryInfo]
    function GetInfoTip(dwFlags: DWORD; var ppwszTip: PWideChar): HRESULT; stdcall;
    function GetInfoFlags(out pdwFlags: DWORD): HRESULT; stdcall;
  end;

{$ENDIF} // SUPPORTS_INTERFACE
{$ENDIF} // COMPILER4_UP

//==============================================================================
// Miscellanuous (missing in delphi 3)
//==============================================================================

{$IFNDEF COMPILER4_UP}

const
  TIME_ZONE_ID_INVALID  = DWORD($FFFFFFFF);
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

{$ENDIF} // COMPILER4_UP

//==============================================================================
// Security related declarations from winnt.h
//==============================================================================

const
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;
  DOMAIN_ALIAS_RID_GUESTS     = $00000222;

  SECURITY_NULL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 0));
  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));
  SECURITY_LOCAL_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 2));
  SECURITY_CREATOR_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 3));
  SECURITY_NON_UNIQUE_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 4));
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';

{$IFNDEF COMPILER5_UP}

type
  SE_OBJECT_TYPE = (
    SE_UNKNOWN_OBJECT_TYPE,
    SE_FILE_OBJECT,
    SE_SERVICE,
    SE_PRINTER,
    SE_REGISTRY_KEY,
    SE_LMSHARE,
    SE_KERNEL_OBJECT,
    SE_WINDOW_OBJECT,
    SE_DS_OBJECT,
    SE_DS_OBJECT_ALL,
    SE_PROVIDER_DEFINED_OBJECT,
    SE_WMIGUID_OBJECT
  );

type
  PPSID = ^PSID;

{$ENDIF} // COMPILER5_UP

{$IFNDEF COMPILER4_UP}
const
  SECURITY_DESCRIPTOR_REVISION = 1;
  SECURITY_DESCRIPTOR_REVISION1 = 1;
  SECURITY_DESCRIPTOR_MIN_LENGTH = 20;
{$ENDIF} // COMPILER4_UP

// TODO SetNamedSecurityInfo is incorrectly declared, at least for Windows 2000
// it is. D5 unit tries to import from aclapi.dll but it is located in advapi3.dll
// Have to check whether this is also true for Windows NT 4.

function SetNamedSecurityInfoW(pObjectName: PWideChar; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
  ppSacl: PACL): DWORD; stdcall; external 'advapi32.dll' name 'SetNamedSecurityInfoW';

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD;
  PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
  external 'advapi32.dll' name 'AdjustTokenPrivileges'

//==============================================================================
// NTFS related I/O control codes, types and constants from winnt.h, winioctl.h
//==============================================================================

type
  PFileAllocatedRangeBuffer = ^TFileAllocatedRangeBuffer;
  _FILE_ALLOCATED_RANGE_BUFFER = record
    FileOffset: TLargeInteger;
    Length: TLargeInteger;
  end;
  TFileAllocatedRangeBuffer = _FILE_ALLOCATED_RANGE_BUFFER;

  PFileZeroDataInformation = ^TFileZeroDataInformation;
  _FILE_ZERO_DATA_INFORMATION = record
    FileOffset: TLargeInteger;
    BeyondFinalZero: TLargeInteger;
  end;
  TFileZeroDataInformation = _FILE_ZERO_DATA_INFORMATION;

const
  COMPRESSION_FORMAT_NONE     = $0000;
  COMPRESSION_FORMAT_DEFAULT  = $0001;
  COMPRESSION_FORMAT_LZNT1    = $0002;

  FILE_SUPPORTS_SPARSE_FILES         = $00000040;
  FILE_SUPPORTS_REPARSE_POINTS       = $00000080;

  IO_REPARSE_TAG_MOUNT_POINT = DWORD($A0000003);
  IO_REPARSE_TAG_HSM         = DWORD($C0000004);
  IO_REPARSE_TAG_SIS         = DWORD($80000007);

  FILE_ATTRIBUTE_DEVICE              = $00000040;
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;

  FILE_DEVICE_FILE_SYSTEM = $00000009;

  METHOD_BUFFERED         = 0;
  METHOD_IN_DIRECT        = 1;
  METHOD_OUT_DIRECT       = 2;
  METHOD_NEITHER          = 3;

  FILE_ANY_ACCESS         = 0;
  FILE_SPECIAL_ACCESS     = FILE_ANY_ACCESS;
  FILE_READ_ACCESS        = $0001;
  FILE_WRITE_ACCESS       = $0002;

  FILE_WRITE_DATA         = $0002;
  FILE_READ_DATA          = $0001;

  FSCTL_GET_COMPRESSION   = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (15 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_COMPRESSION   = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
                            (16 shl 2) or METHOD_BUFFERED;

  FSCTL_LOCK_VOLUME       = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (6 shl 2) or METHOD_BUFFERED;

  FSCTL_UNLOCK_VOLUME     = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (7 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_SPARSE        = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (49 shl 2) or METHOD_BUFFERED;

  FSCTL_SET_ZERO_DATA     = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_WRITE_DATA shl 14) or
                            (50 shl 2) or METHOD_BUFFERED;

  FSCTL_QUERY_ALLOCATED_RANGES =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_READ_DATA shl 14) or
                            (51 shl 2) or METHOD_NEITHER;

  FSCTL_SET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (41 shl 2) or METHOD_BUFFERED;

  FSCTL_GET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (42 shl 2) or METHOD_BUFFERED;

  FSCTL_DELETE_REPARSE_POINT =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_SPECIAL_ACCESS shl 14) or
                            (43 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_OPLOCK_LEVEL_1 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (0 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_OPLOCK_LEVEL_2 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (1 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_BATCH_OPLOCK =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (2 shl 2) or METHOD_BUFFERED;

  FSCTL_REQUEST_FILTER_OPLOCK =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (23 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_ACKNOWLEDGE =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (3 shl 2) or METHOD_BUFFERED;

  FSCTL_OPBATCH_ACK_CLOSE_PENDING =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (4 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_NOTIFY =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (5 shl 2) or METHOD_BUFFERED;

  FSCTL_OPLOCK_BREAK_ACK_NO_2 =
                            (FILE_DEVICE_FILE_SYSTEM shl 16) or
                            (FILE_ANY_ACCESS shl 14) or
                            (20 shl 2) or METHOD_BUFFERED;

function GetVolumeNameForVolumeMountPointA(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
function GetVolumeNameForVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR; lpszVolumeName: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;
function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;

function SetVolumeMountPointA(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPCSTR): BOOL; stdcall;
function SetVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR; lpszVolumeName: LPCWSTR): BOOL; stdcall;
function SetVolumeMountPoint(lpszVolumeMountPoint: LPCSTR; lpszVolumeName: LPCSTR): BOOL; stdcall;

function DeleteVolumeMountPointA(lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;
function DeleteVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR): BOOL; stdcall;
function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;

//------------------------------------------------------------------------------
// NTFS Reparse Points
//------------------------------------------------------------------------------

//
// The reparse structure is used by layered drivers to store data in a
// reparse point. The constraints on reparse tags are defined below.
// This version of the reparse data buffer is only for Microsoft tags.
//

type
  PReparseDataBuffer = ^TReparseDataBuffer;
  _REPARSE_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: WORD;
    Reserved: WORD;
    case Integer of
      0: ( // SymbolicLinkReparseBuffer and MountPointReparseBuffer
        SubstituteNameOffset: WORD;
        SubstituteNameLength: WORD;
        PrintNameOffset: WORD;
        PrintNameLength: WORD;
        PathBuffer: array [0..0] of WCHAR);
      1: ( // GenericReparseBuffer
        DataBuffer: array [0..0] of BYTE);
  end;
  TReparseDataBuffer = _REPARSE_DATA_BUFFER;

const
  REPARSE_DATA_BUFFER_HEADER_SIZE = 8;
  REPARSE_GUID_DATA_BUFFER_HEADER_SIZE = 24;

type
  PReparsePointInformation = ^TReparsePointInformation;
  _REPARSE_POINT_INFORMATION = record
    ReparseDataLength: WORD;
    UnparsedNameLength: WORD;
  end;
  TReparsePointInformation = _REPARSE_POINT_INFORMATION;

const
  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;

  IO_REPARSE_TAG_RESERVED_ZERO  = (0);
  IO_REPARSE_TAG_RESERVED_ONE   = (1);
  IO_REPARSE_TAG_RESERVED_RANGE = IO_REPARSE_TAG_RESERVED_ONE;
  IO_REPARSE_TAG_VALID_VALUES   = DWORD($E000FFFF);

type
  PReparseGuidDataBuffer = ^TReparseGuidDataBuffer;
  _REPARSE_GUID_DATA_BUFFER = record
    ReparseTag: DWORD;
    ReparseDataLength: WORD;
    Reserved: WORD;
    ReparseGuid: TGUID;
    DataBuffer: array [0..0] of BYTE;
  end;
  TReparseGuidDataBuffer = _REPARSE_GUID_DATA_BUFFER;

const
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;

//==============================================================================
// PSAPI (not included in delphi 3)
//==============================================================================

type
  PModuleInfo = ^TModuleInfo;
  _MODULEINFO = packed record
    lpBaseOfDll: Pointer;
    SizeOfImage: DWORD;
    EntryPoint: Pointer;
  end;
  TModuleInfo = _MODULEINFO;

procedure ExitPsapi;
function InitPsapi: Boolean;
function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL;
function EnumProcessModules(hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL;
function GetModuleBaseNameA(hProcess: THandle; hModule: HMODULE;
  lpBaseName: PAnsiChar; nSize: DWORD): DWORD;
function GetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
  nSize: DWORD): DWORD;
function GetModuleInformation(hProcess: THandle; hModule: HMODULE; lpmodinfo: PModuleInfo;
  cb: DWORD): BOOL;

//==============================================================================
// ToolHelp32 (static linkage in delphi 3)
//==============================================================================

const
  MAX_MODULE_NAME32 = 255;

type
  tagMODULEENTRY32 = record
    dwSize: DWORD;
    th32ModuleID: DWORD;  // This module
    th32ProcessID: DWORD; // owning process
    GlblcntUsage: DWORD;  // Global usage count on the module
    ProccntUsage: DWORD;  // Module usage count in th32ProcessID's context
    modBaseAddr: PBYTE;   // Base address of module in th32ProcessID's context
    modBaseSize: DWORD;   // Size in bytes of module starting at modBaseAddr
    hModule: HMODULE;     // The hModule of this module in th32ProcessID's context
    szModule: array [0..MAX_MODULE_NAME32] of Char;
    szExePath: array [0..MAX_PATH - 1] of Char;
  end;
  TModuleEntry32 = tagMODULEENTRY32;

const
  TH32CS_SNAPMODULE   = $00000008;

procedure ExitTLHelp;
function InitTLHelp: Boolean;
function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle;
function Module32First(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
function Module32Next(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;

//==============================================================================
// Netbios (incorrect/inconvenient declarations in rtl)
//==============================================================================

const
  NCBNAMSZ    = 16;        // absolute length of a net name
  MAX_LANA    = 254;       // lana's in range 0 to MAX_LANA inclusive

  NRC_GOODRET = $00;       // good return
  NCBASTAT    = $33;       // NCB ADAPTER STATUS
  NCBRESET    = $32;       // NCB RESET
  NCBENUM     = $37;       // NCB ENUMERATE LANA NUMBERS

type
  PNCB = ^TNCB;
  TNCBPostProc = procedure (P: PNCB); stdcall;
  TNCB = record
    ncb_command: Byte;
    ncb_retcode: Byte;
    ncb_lsn: Byte;
    ncb_num: Byte;
    ncb_buffer: PChar;
    ncb_length: Word;
    ncb_callname: array [0..NCBNAMSZ - 1] of Char;
    ncb_name: array [0..NCBNAMSZ - 1] of Char;
    ncb_rto: Byte;
    ncb_sto: Byte;
    ncb_post: TNCBPostProc;
    ncb_lana_num: Byte;
    ncb_cmd_cplt: Byte;
    ncb_reserve: array [0..9] of Char;
    ncb_event: THandle;
  end;

  PAdapterStatus = ^TAdapterStatus;
  TAdapterStatus = record
    adapter_address: array [0..5] of Char;
    // Remaining fields are unused so let's not declare them and save space
    filler: array [1..4*SizeOf(Char)+19*SizeOf(Word)+3*SizeOf(DWORD)] of Byte;
  end;

  PNameBuffer = ^TNameBuffer;
  TNameBuffer = record
    name: array [0..NCBNAMSZ - 1] of Char;
    name_num: Byte;
    name_flags: Byte;
  end;

  ASTAT = record
    adapt: TAdapterStatus;
    namebuf: array [0..29] of TNameBuffer;
  end;

  PLanaEnum = ^TLanaEnum;
  TLanaEnum = record
    length: Byte;
    lana: array [0..MAX_LANA] of Byte;
  end;

procedure ExitNetbios;
function InitNetbios: Boolean;
function NetBios(P: PNCB): Byte;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_EXTSYM}

// centralized EXTERNALSYMs to keep this Delphi 3 compatible

{$EXTERNALSYM InterlockedExchangePointer}
{$EXTERNALSYM SignalObjectAndWait}

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM IContextMenu2}
  {$EXTERNALSYM SHGetSpecialFolderPath}
  {$EXTERNALSYM SID_IQueryInfo}
  {$EXTERNALSYM IQueryInfo}

{$ENDIF} // COMPILER4_UP

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM TIME_ZONE_ID_INVALID}
  {$EXTERNALSYM TIME_ZONE_ID_UNKNOWN}
  {$EXTERNALSYM TIME_ZONE_ID_STANDARD}
  {$EXTERNALSYM TIME_ZONE_ID_DAYLIGHT}

{$ENDIF} // COMPILER4_UP

  {$EXTERNALSYM SECURITY_BUILTIN_DOMAIN_RID}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_ADMINS}
  {$EXTERNALSYM DOMAIN_ALIAS_RID_GUESTS}

  {$EXTERNALSYM SECURITY_NULL_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_WORLD_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_LOCAL_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_CREATOR_SID_AUTHORITY}
  {$EXTERNALSYM SECURITY_NON_UNIQUE_AUTHORITY}
  {$EXTERNALSYM SECURITY_NT_AUTHORITY}

  {$EXTERNALSYM SE_CREATE_TOKEN_NAME}
  {$EXTERNALSYM SE_ASSIGNPRIMARYTOKEN_NAME}
  {$EXTERNALSYM SE_LOCK_MEMORY_NAME}
  {$EXTERNALSYM SE_INCREASE_QUOTA_NAME}
  {$EXTERNALSYM SE_UNSOLICITED_INPUT_NAME}
  {$EXTERNALSYM SE_MACHINE_ACCOUNT_NAME}
  {$EXTERNALSYM SE_TCB_NAME}
  {$EXTERNALSYM SE_SECURITY_NAME}
  {$EXTERNALSYM SE_TAKE_OWNERSHIP_NAME}
  {$EXTERNALSYM SE_LOAD_DRIVER_NAME}
  {$EXTERNALSYM SE_SYSTEM_PROFILE_NAME}
  {$EXTERNALSYM SE_SYSTEMTIME_NAME}
  {$EXTERNALSYM SE_PROF_SINGLE_PROCESS_NAME}
  {$EXTERNALSYM SE_INC_BASE_PRIORITY_NAME}
  {$EXTERNALSYM SE_CREATE_PAGEFILE_NAME}
  {$EXTERNALSYM SE_CREATE_PERMANENT_NAME}
  {$EXTERNALSYM SE_BACKUP_NAME}
  {$EXTERNALSYM SE_RESTORE_NAME}
  {$EXTERNALSYM SE_SHUTDOWN_NAME}
  {$EXTERNALSYM SE_DEBUG_NAME}
  {$EXTERNALSYM SE_AUDIT_NAME}
  {$EXTERNALSYM SE_SYSTEM_ENVIRONMENT_NAME}
  {$EXTERNALSYM SE_CHANGE_NOTIFY_NAME}
  {$EXTERNALSYM SE_REMOTE_SHUTDOWN_NAME}
  {$EXTERNALSYM SE_UNDOCK_NAME}
  {$EXTERNALSYM SE_SYNC_AGENT_NAME}
  {$EXTERNALSYM SE_ENABLE_DELEGATION_NAME}

{$IFNDEF COMPILER5_UP}

  {$EXTERNALSYM SE_OBJECT_TYPE}
  {$EXTERNALSYM PPSID}

{$ENDIF} // COMPILER5_UP

{$IFNDEF COMPILER4_UP}

  {$EXTERNALSYM SECURITY_DESCRIPTOR_REVISION}
  {$EXTERNALSYM SECURITY_DESCRIPTOR_REVISION1}
  {$EXTERNALSYM SECURITY_DESCRIPTOR_MIN_LENGTH}

{$ENDIF} // COMPILER4_UP

{$EXTERNALSYM SetNamedSecurityInfoW}
{$EXTERNALSYM AdjustTokenPrivileges}

  {$EXTERNALSYM _FILE_ALLOCATED_RANGE_BUFFER}
  {$EXTERNALSYM _FILE_ZERO_DATA_INFORMATION}

  {$EXTERNALSYM COMPRESSION_FORMAT_NONE}
  {$EXTERNALSYM COMPRESSION_FORMAT_DEFAULT}
  {$EXTERNALSYM COMPRESSION_FORMAT_LZNT1}

  {$EXTERNALSYM FILE_SUPPORTS_SPARSE_FILES}
  {$EXTERNALSYM FILE_SUPPORTS_REPARSE_POINTS}

  {$EXTERNALSYM REPARSE_GUID_DATA_BUFFER_HEADER_SIZE}

  {$EXTERNALSYM IO_REPARSE_TAG_MOUNT_POINT}
  {$EXTERNALSYM IO_REPARSE_TAG_HSM}
  {$EXTERNALSYM IO_REPARSE_TAG_SIS}

  {$EXTERNALSYM FILE_ATTRIBUTE_DEVICE}
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}

  {$EXTERNALSYM FILE_DEVICE_FILE_SYSTEM}

  {$EXTERNALSYM METHOD_BUFFERED}
  {$EXTERNALSYM METHOD_IN_DIRECT}
  {$EXTERNALSYM METHOD_OUT_DIRECT}
  {$EXTERNALSYM METHOD_NEITHER}

  {$EXTERNALSYM FILE_ANY_ACCESS}
  {$EXTERNALSYM FILE_SPECIAL_ACCESS}
  {$EXTERNALSYM FILE_READ_ACCESS}
  {$EXTERNALSYM FILE_WRITE_ACCESS}

  {$EXTERNALSYM FILE_WRITE_DATA}
  {$EXTERNALSYM FILE_READ_DATA}

  {$EXTERNALSYM FSCTL_GET_COMPRESSION}
  {$EXTERNALSYM FSCTL_SET_COMPRESSION}
  {$EXTERNALSYM FSCTL_LOCK_VOLUME}
  {$EXTERNALSYM FSCTL_UNLOCK_VOLUME}
  {$EXTERNALSYM FSCTL_SET_SPARSE}
  {$EXTERNALSYM FSCTL_SET_ZERO_DATA}
  {$EXTERNALSYM FSCTL_QUERY_ALLOCATED_RANGES}
  {$EXTERNALSYM FSCTL_SET_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_GET_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_DELETE_REPARSE_POINT}
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_1}
  {$EXTERNALSYM FSCTL_REQUEST_OPLOCK_LEVEL_2}
  {$EXTERNALSYM FSCTL_REQUEST_BATCH_OPLOCK}
  {$EXTERNALSYM FSCTL_REQUEST_FILTER_OPLOCK}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACKNOWLEDGE}
  {$EXTERNALSYM FSCTL_OPBATCH_ACK_CLOSE_PENDING}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_NOTIFY}
  {$EXTERNALSYM FSCTL_OPLOCK_BREAK_ACK_NO_2}

  {$EXTERNALSYM GetVolumeNameForVolumeMountPointA}
  {$EXTERNALSYM GetVolumeNameForVolumeMountPointW}
  {$EXTERNALSYM GetVolumeNameForVolumeMountPoint}

  {$EXTERNALSYM SetVolumeMountPointA}
  {$EXTERNALSYM SetVolumeMountPointW}
  {$EXTERNALSYM SetVolumeMountPoint}

  {$EXTERNALSYM DeleteVolumeMountPointA}
  {$EXTERNALSYM DeleteVolumeMountPointW}
  {$EXTERNALSYM DeleteVolumeMountPoint}

  {$EXTERNALSYM _MODULEINFO}

  {$EXTERNALSYM MAX_MODULE_NAME32}
  {$EXTERNALSYM tagMODULEENTRY32}
  {$EXTERNALSYM TH32CS_SNAPMODULE}

  {$EXTERNALSYM NCBNAMSZ}
  {$EXTERNALSYM MAX_LANA}

  {$EXTERNALSYM NRC_GOODRET}
  {$EXTERNALSYM NCBASTAT}
  {$EXTERNALSYM NCBRESET}
  {$EXTERNALSYM NCBENUM}

  {$EXTERNALSYM PNCB}
  {$EXTERNALSYM ASTAT}

{$ENDIF} // SUPPORTS_EXTSYM

implementation

function GetVolumeNameForVolumeMountPointA; external kernel32 name 'GetVolumeNameForVolumeMountPointA';
function GetVolumeNameForVolumeMountPointW; external kernel32 name 'GetVolumeNameForVolumeMountPointW';
function GetVolumeNameForVolumeMountPoint; external kernel32 name 'GetVolumeNameForVolumeMountPointA';

function SetVolumeMountPointA; external kernel32 name 'SetVolumeMountPointA';
function SetVolumeMountPointW; external kernel32 name 'SetVolumeMountPointW';
function SetVolumeMountPoint; external kernel32 name 'SetVolumeMountPointA';

function DeleteVolumeMountPointA; external kernel32 name 'DeleteVolumeMountPointA';
function DeleteVolumeMountPointW; external kernel32 name 'DeleteVolumeMountPointW';
function DeleteVolumeMountPoint; external kernel32 name 'DeleteVolumeMountPointA';

//==============================================================================
// Netbios
//==============================================================================

type
  TNetBios = function (P: PNCB): Byte; stdcall;

var
  NetBiosLib: HINST = 0;
  _NetBios: TNetBios = nil;

//------------------------------------------------------------------------------

procedure ExitNetbios;
begin
  if NetBiosLib <> 0 then
  begin
    FreeLibrary(NetBiosLib);
    NetBiosLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitNetbios: Boolean;
begin
  Result := True;
  if NetBiosLib = 0 then
  begin
    NetBiosLib := LoadLibrary(PChar('netapi32.dll'));
    Result := NetBiosLib <> 0;
    if Result then
    begin
      @_NetBios := GetProcAddress(NetBiosLib, PChar('Netbios'));
      Result := Assigned(@_NetBios);
      if not Result then
        ExitNetbios;
    end;
  end;
end;

//------------------------------------------------------------------------------

function NetBios(P: PNCB): Byte;
begin
  if InitNetbios then
    Result := _NetBios(P)
  else
    Result := 1; // anything other then NRC_GOODRET will do
end;

//==============================================================================
// PSAPI
//==============================================================================

type
  TEnumProcesses = function (lpidProcess: LPDWORD; cb: DWORD;
    var cbNeeded: DWORD): BOOL stdcall;
  TEnumProcessModules = function (hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  TGetModuleFileNameEx = function (hProcess: THandle; hModule: HMODULE;
    lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
  TGetModuleInformation = function (hProcess: THandle; hModule: HMODULE;
    lpmodinfo: PModuleInfo; cb: DWORD): BOOL; stdcall;
  TGetModuleBaseNameA = function (hProcess: THandle; hModule: HMODULE;
    lpBaseName: PAnsiChar; nSize: DWORD): DWORD; stdcall;

var
  PsapiLib: HINST = 0;
  _EnumProcesses: TEnumProcesses;
  _EnumProcessModules: TEnumProcessModules;
  _GetModuleFileNameEx: TGetModuleFileNameEx;
  _GetModuleInformation: TGetModuleInformation;
  _GetModuleBaseNameA: TGetModuleBaseNameA;

//------------------------------------------------------------------------------

procedure ExitPsapi;
begin
  if PsapiLib <> 0 then
  begin
    FreeLibrary(PsapiLib);
    PsapiLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitPsapi: Boolean;
begin
  Result := True;
  if PsapiLib = 0 then
  begin
    PsapiLib := LoadLibrary('PSAPI.dll');
    Result := PsapiLib <> 0;
    if Result then
    begin
      @_EnumProcesses := GetProcAddress(PsapiLib, PChar('EnumProcesses'));
      @_EnumProcessModules := GetProcAddress(PsapiLib, PChar('EnumProcessModules'));
      @_GetModuleFileNameEx := GetProcAddress(PsapiLib, PChar('GetModuleFileNameExA'));
      @_GetModuleInformation := GetProcAddress(PsapiLib, PChar('GetModuleInformation'));
      @_GetModuleBaseNameA := GetProcAddress(PsapiLib, PChar('GetModuleBaseNameA'));
    end;
  end;
end;

//------------------------------------------------------------------------------

function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _EnumProcesses(lpidProcess, cb, cbNeeded);
end;

//------------------------------------------------------------------------------

function EnumProcessModules(hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _EnumProcessModules(hProcess, lphModule, cb, lpcbNeeded);
end;

//------------------------------------------------------------------------------

function GetModuleBaseNameA(hProcess: THandle; hModule: HMODULE;
  lpBaseName: PAnsiChar; nSize: DWORD): DWORD;
begin
  Result := 0;
  if InitPsapi then
    Result := _GetModuleBaseNameA(hProcess, hModule, lpBaseName, nSize);
end;

//------------------------------------------------------------------------------

function GetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
  nSize: DWORD): DWORD;
begin
  if InitPsapi then
    Result := _GetModuleFileNameEx(hProcess, hModule, lpFileName, nSize)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function GetModuleInformation(hProcess: THandle; hModule: HMODULE; lpModInfo: PModuleInfo;
  cb: DWORD): BOOL;
begin
  Result := InitPsapi;
  if Result then
    Result := _GetModuleInformation(hProcess, hModule, lpModInfo, cb);
end;

//==============================================================================
// ToolHelp32
//==============================================================================

type
  TCreateToolhelp32Snapshot = function (dwFlags, th32ProcessID: DWORD): THandle; stdcall;
  TModule32First = function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
  TModule32Next = function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;

var
  TLHelpLib: HINST = 0;
  _CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  _Module32First: TModule32First;
  _Module32Next: TModule32Next;

//------------------------------------------------------------------------------

procedure ExitTLHelp;
begin
  if TLHelpLib <> 0 then
  begin
    FreeLibrary(TLHelpLib);
    TLHelpLib := 0;
  end;
end;

//------------------------------------------------------------------------------

function InitTLHelp: Boolean;
begin
  Result := True;
  if TLHelpLib = 0 then
  begin
    TLHelpLib := GetModuleHandle('kernel32.dll');
    Result := TLHelpLib <> 0;
    if Result then
    begin
      @_CreateToolhelp32Snapshot := GetProcAddress(TLHelpLib, PChar('CreateToolhelp32Snapshot'));
      @_Module32First := GetProcAddress(TLHelpLib, PChar('Module32First'));
      @_Module32Next := GetProcAddress(TLHelpLib, PChar('Module32Next'));
    end;
  end;
end;

//------------------------------------------------------------------------------

function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle;
begin
  if InitTLHelp then
    Result := _CreateToolhelp32Snapshot(dwFlags, th32ProcessID)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function Module32First(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
begin
  if InitTLHelp then
    Result := _Module32First(hSnapshot, lpme)
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function Module32Next(hSnapshot: THandle; var lpme: TModuleEntry32): BOOL;
begin
  if InitTLHelp then
    Result := _Module32Next(hSnapshot, lpme)
  else
    Result := False;
end;

end.
