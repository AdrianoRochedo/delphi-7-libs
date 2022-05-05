{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Merkuriev                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZDbcInterbase6Utils;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZDbcConnection,
  ZPlainInterbaseDriver, ZCompatibility, ZDbcCachedResultSet,
  ZDbcResultSetMetadata;

type
  { Interbase Statement Type }
  TZIbSqlStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete, stDDL,
    stGetSegment, stPutSegment, stExecProc, stStartTrans, stCommit, stRollback,
    stSelectForUpdate, stSetGenerator);

  { Interbase Error Class}
  EZIBConvertError = class(Exception);

  { Paparameter string name and it value}
  TZIbParam = record
    Name: string;
    Number: word;
  end;
  PZIbParam = ^TZIbParam;

  { Interbase blob Information structure
    contain iformation about blob size in bytes,
    segments count, segment size in bytes and blob type
    Note: blob type can be text an binary }
  TIbBlobInfo = record
    NumSegments: SmallInt;
    MaxSegmentSize: SmallInt;
    BlobType: SmallInt;
    TotalSize: LongInt;
  end;

  { Base interface for sqlda }
  IZSQLDA = interface
    ['{2D0D6029-B31C-4E39-89DC-D86D20437C35}']
    procedure SetCountFields(Value: Word);
    procedure AllocateData;

    function GetSqlName(const Index: Word): string;
    function GetRelName(const Index: Word): string;
    function GetOwnName(const Index: Word): string;
    function GetAliasName(const Index: Word): string;


    function GetData: PXSQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;
    function GetFieldCount: Integer;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetSQLType(const Index: Word): Smallint;
    function GetSQLSubType(const Index: Word): Smallint;
    function GetSQLLen(const Index: Word): Smallint;
  end;

  { parameters interface sqlda}
  IZParamsSQLDA = interface(IZSQLDA)
    ['{D2C3D5E1-F3A6-4223-9A6E-3048B99A06C4}']
    procedure WriteBlob(const Index: Integer; Stream: TStream);
    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: ShortInt);
    procedure UpdateShort(const Index: Integer; Value: SmallInt);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateLong(const Index: Integer; Value: LongInt);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Int64);
    procedure UpdatePChar(const Index: Integer; Value: PChar);
    procedure UpdateString(const Index: Integer; Value: string);
    procedure UpdateBytes(const Index: Integer; Value: TByteDynArray);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure UpdateQuad(const Index: Word; const Value: TISC_QUAD);
  end;

  { Result interface for sqlda}
  IZResultSQLDA = interface(IZSQLDA)
    ['{D2C3D5E1-F3A6-4223-9A6E-3048B99A06C4}']
    procedure ReadBlobFromStream(const Index: Word; Stream: TStream);
    procedure ReadBlobFromString(const Index: Word; var str: string);
    procedure ReadBlobFromVariant(const Index: Word; var Value: Variant);

    function IsNull(const Index: Integer): Boolean;
    function GetPChar(const Index: Integer): PChar;
    function GetString(const Index: Integer): string;
    function GetBoolean(const Index: Integer): Boolean;
    function GetByte(const Index: Integer): ShortInt;
    function GetShort(const Index: Integer): SmallInt;
    function GetInt(const Index: Integer): Integer;
    function GetLong(const Index: Integer): LongInt;
    function GetFloat(const Index: Integer): Single;
    function GetDouble(const Index: Integer): Double;
    function GetBigDecimal(const Index: Integer): Int64;
    function GetBytes(const Index: Integer): TByteDynArray;
    function GetDate(const Index: Integer): TDateTime;
    function GetTime(const Index: Integer): TDateTime;
    function GetTimestamp(const Index: Integer): TDateTime;
    function GetValue(const Index: Word): Variant;
    function GetQuad(const Index: Integer): TISC_QUAD;
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }
  TZSQLDA = class (TInterfacedObject, IZSQLDA)
  private
    FXSQLDA: PXSQLDA;
    FPlainDriver: IZInterbasePlainDriver;
    procedure CheckRange(const Index: Word);
  public
    procedure SetCountFields(Value: Word);
    procedure AllocateData; virtual;

    function GetSqlName(const Index: Word): string;
    function GetRelName(const Index: Word): string;
    function GetOwnName(const Index: Word): string;
    function GetAliasName(const Index: Word): string;

    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetData: PXSQLDA;

    function GetSQLType(const Index: Word): Smallint;
    function GetSQLSubType(const Index: Word): Smallint;
    function GetSQLLen(const Index: Word): Smallint;
  end;

  { Parameters class for sqlda structure.
    It clas can only write data to parameters/fields }
  TZParamsSQLDA = class (TZSQLDA, IZParamsSQLDA)
  protected
    FHandle: PISC_DB_HANDLE;
    FTransactionHandle: PISC_TR_HANDLE;
  private
    procedure EncodeString(Code: Smallint; const Index: Word; const Str: String);
  public
    constructor Create(PlainDriver: IZInterbasePlainDriver;
      Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE);
    destructor Destroy; override;

    procedure WriteBlob(const Index: Integer; Stream: TStream);

    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: ShortInt);
    procedure UpdateShort(const Index: Integer; Value: SmallInt);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateLong(const Index: Integer; Value: LongInt);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Int64);
    procedure UpdatePChar(const Index: Integer; Value: PChar);
    procedure UpdateString(const Index: Integer; Value: string);
    procedure UpdateBytes(const Index: Integer; Value: TByteDynArray);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure UpdateQuad(const Index: Word; const Value: TISC_QUAD);
  end;

  { Resultset class for sqlda structure.
    It class read data from sqlda fields }
  TZResultSQLDA = class (TZSQLDA, IZResultSQLDA)
  protected
    FDefaults: array of Variant;
    FHandle: PISC_DB_HANDLE;
    FTransactionHandle: PISC_TR_HANDLE;
  private
    function DecodeString(const Code: Smallint; const Index: Word): string;
    procedure DecodeString2(const Code: Smallint; const Index: Word; out Str: string);
  public
    constructor Create(PlainDriver: IZInterbasePlainDriver;
      Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE);
    destructor Destroy; override;

    procedure AllocateData; override;

    procedure ReadBlobFromStream(const Index: Word; Stream: TStream);
    procedure ReadBlobFromString(const Index: Word; var str: string);
    procedure ReadBlobFromVariant(const Index: Word; var Value: Variant);

    function IsNull(const Index: Integer): Boolean;
    function GetPChar(const Index: Integer): PChar;
    function GetString(const Index: Integer): string;
    function GetBoolean(const Index: Integer): Boolean;
    function GetByte(const Index: Integer): ShortInt;
    function GetShort(const Index: Integer): SmallInt;
    function GetInt(const Index: Integer): Integer;
    function GetLong(const Index: Integer): LongInt;
    function GetFloat(const Index: Integer): Single;
    function GetDouble(const Index: Integer): Double;
    function GetBigDecimal(const Index: Integer): Int64;
    function GetBytes(const Index: Integer): TByteDynArray;
    function GetDate(const Index: Integer): TDateTime;
    function GetTime(const Index: Integer): TDateTime;
    function GetTimestamp(const Index: Integer): TDateTime;
    function GetValue(const Index: Word): Variant;
    function GetQuad(const Index: Integer): TISC_QUAD;
  end;

  function RandomString(Len: integer): string;
  function GetCachedResultSet(SQL: string; Statement: IZStatement;
    NativeResultSet: IZResultSet): IZResultSet;

  {Interbase6 Connection Functions}
  function GenerateDPB(Info: TStrings; var FDPBLength, Dialect: Word): PChar;
  function GenerateTPB(Params: TStrings; var Handle: TISC_DB_HANDLE): PISC_TEB;
  function GetInterbase6DatabaseParamNumber(const Value: string): word;
  function GetInterbase6TransactionParamNumber(const Value: string): word;

  { Interbase6 errors functions }
  function GetNameSqlType(Value: Word): string;
  procedure CheckInterbase6Error(PlainDriver: IZInterbasePlainDriver;
    StatusVector: TARRAY_ISC_STATUS; SQL: string = '');

  { Interbase satatement functions}
  function PrepareStatement(PlainDriver: IZInterbasePlainDriver;
    Handle: PISC_DB_HANDLE; TrHandle: PISC_TR_HANDLE; Dialect: Word; Sql: string;
    var StmtHandle: TISC_STMT_HANDLE; SqlData: IZResultSQLDA): TZIbSqlStatementType;
  function GetStatementType(PlainDriver: IZInterbasePlainDriver;
    StmtHandle: TISC_STMT_HANDLE): TZIbSqlStatementType;
  function GetAffectedRows(PlainDriver: IZInterbasePlainDriver;
    StmtHandle: TISC_STMT_HANDLE; StatementType: TZIbSqlStatementType): integer;
  procedure PrepareParameters(PlainDriver: IZInterbasePlainDriver; Sql: string;
    InParamValues: TVarArray; InParamTypes: TZSQLTypeArray; InParamCount: integer;
    Dialect: Word; var StmtHandle: TISC_STMT_HANDLE; ParamSqlData: IZParamsSQLDA);
  procedure FreeStatement(PlainDriver: IZInterbasePlainDriver;
    StatementHandle: TISC_STMT_HANDLE);

  function ConvertInterbase6ToSqlType(SqlType, SqlSubType: Integer): TZSqlType;

  { interbase blob routines }
  procedure GetBlobInfo(PlainDriver: IZInterbasePlainDriver;
    BlobHandle: TISC_BLOB_HANDLE; var BlobInfo: TIbBlobInfo);
  procedure ReadBlobBufer(PlainDriver: IZInterbasePlainDriver;
    Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE;
    BlobId: TISC_QUAD; var Size: Integer; var Buffer: Pointer);


const
  { Default Interbase blob size for readig }
  DefaultBlobSegmentSize = 16 * 1024;

  IBScaleDivisor: array[-15..-1] of Int64 = (1000000000000000,100000000000000,
    10000000000000,1000000000000,100000000000,10000000000,1000000000,100000000,
    10000000,1000000,100000,10000,1000,100,10);

  { count database parameters }
  MAX_DPB_PARAMS = 69;
  { prefix database parameters names it used in paramters scann procedure }
  BPBPrefix = 'isc_dpb_';
  { list database parameters and their apropriate numbers }
  DatabaseParams: array [0..MAX_DPB_PARAMS]of TZIbParam = (
    (Name:'version1';         Number: isc_dpb_version1),
    (Name:'cdd_pathname';     Number: isc_dpb_cdd_pathname),
    (Name:'allocation';       Number: isc_dpb_allocation),
    (Name:'journal';          Number: isc_dpb_journal),
    (Name:'page_size';        Number: isc_dpb_page_size),
    (Name:'num_buffers';      Number: isc_dpb_num_buffers),
    (Name:'buffer_length';    Number: isc_dpb_buffer_length),
    (Name:'debug';            Number: isc_dpb_debug),
    (Name:'garbage_collect';  Number: isc_dpb_garbage_collect),
    (Name:'verify';           Number: isc_dpb_verify),
    (Name:'sweep';            Number: isc_dpb_sweep),
    (Name:'enable_journal';   Number: isc_dpb_enable_journal),
    (Name:'disable_journal';  Number: isc_dpb_disable_journal),
    (Name:'dbkey_scope';      Number: isc_dpb_dbkey_scope),
    (Name:'number_of_users';  Number: isc_dpb_number_of_users),
    (Name:'trace';            Number: isc_dpb_trace),
    (Name:'no_garbage_collect'; Number: isc_dpb_no_garbage_collect),
    (Name:'damaged';          Number: isc_dpb_damaged),
    (Name:'license';          Number: isc_dpb_license),
    (Name:'sys_user_name';    Number: isc_dpb_sys_user_name),
    (Name:'encrypt_key';      Number: isc_dpb_encrypt_key),
    (Name:'activate_shadow';  Number: isc_dpb_activate_shadow),
    (Name:'sweep_interval';   Number: isc_dpb_sweep_interval),
    (Name:'delete_shadow';    Number: isc_dpb_delete_shadow),
    (Name:'force_write';      Number: isc_dpb_force_write),
    (Name:'begin_log';        Number: isc_dpb_begin_log),
    (Name:'quit_log';         Number: isc_dpb_quit_log),
    (Name:'no_reserve';       Number: isc_dpb_no_reserve),
    (Name:'uid';              Number: isc_dpb_user_name), //REMOVE IT
    (Name:'pwd';              Number: isc_dpb_password), //REMOVE IT
    (Name:'username';         Number: isc_dpb_user_name), //REMOVE IT
    (Name:'password';         Number: isc_dpb_password), //REMOVE IT
    (Name:'password_enc';     Number: isc_dpb_password_enc),
    (Name:'sys_user_name_enc';  Number: isc_dpb_sys_user_name_enc),
    (Name:'interp';           Number: isc_dpb_interp),
    (Name:'online_dump';      Number: isc_dpb_online_dump),
    (Name:'old_file_size';    Number: isc_dpb_old_file_size),
    (Name:'old_num_files';    Number: isc_dpb_old_num_files),
    (Name:'old_file';         Number: isc_dpb_old_file),
    (Name:'old_start_page';   Number: isc_dpb_old_start_page),
    (Name:'old_start_seqno';  Number: isc_dpb_old_start_seqno),
    (Name:'old_start_file';   Number: isc_dpb_old_start_file),
    (Name:'drop_walfile';     Number: isc_dpb_drop_walfile),
    (Name:'old_dump_id';      Number: isc_dpb_old_dump_id),
    (Name:'wal_backup_dir';   Number: isc_dpb_wal_backup_dir),
    (Name:'wal_chkptlen';     Number: isc_dpb_wal_chkptlen),
    (Name:'wal_numbufs';      Number: isc_dpb_wal_numbufs),
    (Name:'wal_bufsize';      Number: isc_dpb_wal_bufsize),
    (Name:'wal_grp_cmt_wait'; Number: isc_dpb_wal_grp_cmt_wait),
    (Name:'lc_messages';      Number: isc_dpb_lc_messages),
    (Name:'lc_ctype';         Number: isc_dpb_lc_ctype),
    (Name:'cache_manager';    Number: isc_dpb_cache_manager),
    (Name:'shutdown';         Number: isc_dpb_shutdown),
    (Name:'online';           Number: isc_dpb_online),
    (Name:'shutdown_delay';   Number: isc_dpb_shutdown_delay),
    (Name:'reserved';         Number: isc_dpb_reserved),
    (Name:'overwrite';        Number: isc_dpb_overwrite),
    (Name:'sec_attach';       Number: isc_dpb_sec_attach),
    (Name:'disable_wal';      Number: isc_dpb_disable_wal),
    (Name:'connect_timeout';  Number: isc_dpb_connect_timeout),
    (Name:'dummy_packet_interval'; Number: isc_dpb_dummy_packet_interval),
    (Name:'gbak_attach';      Number: isc_dpb_gbak_attach),
    (Name:'sql_role_name';    Number: isc_dpb_sql_role_name),
    (Name:'set_page_buffers'; Number: isc_dpb_set_page_buffers),
    (Name:'working_directory';  Number: isc_dpb_working_directory),
    (Name:'SQL_dialect';      Number: isc_dpb_SQL_dialect),
    (Name:'set_db_readonly';  Number: isc_dpb_set_db_readonly),
    (Name:'set_db_sql_dialect'; Number: isc_dpb_set_db_SQL_dialect),
    (Name:'gfix_attach';      Number: isc_dpb_gfix_attach),
    (Name:'gstat_attach';     Number: isc_dpb_gstat_attach)
  );

  { count transaction parameters }
  MAX_TPB_PARAMS = 16;
  { prefix transaction parameters names it used in paramters scann procedure }
  TPBPrefix = 'isc_tpb_';
  { list transaction parameters and their apropriate numbers }
  TransactionParams: array [0..MAX_TPB_PARAMS]of TZIbParam = (
    (Name:'version1';         Number: isc_tpb_version1),
    (Name:'version3';         Number: isc_tpb_version3),
    (Name:'consistency';      Number: isc_tpb_consistency),
    (Name:'concurrency';      Number: isc_tpb_concurrency),
    (Name:'exclusive';        Number: isc_tpb_exclusive),
    (Name:'shared';           Number: isc_tpb_shared),
    (Name:'protected';        Number: isc_tpb_protected),
    (Name:'wait';             Number: isc_tpb_wait),
    (Name:'nowait';           Number: isc_tpb_nowait),
    (Name:'read';             Number: isc_tpb_read),
    (Name:'write';            Number: isc_tpb_write),
    (Name:'ignore_limbo';     Number: isc_tpb_ignore_limbo),
    (Name:'read_committed';   Number: isc_tpb_read_committed),
    (Name:'rec_version';      Number: isc_tpb_rec_version),
    (Name:'no_rec_version';   Number: isc_tpb_no_rec_version),
    (Name:'lock_read';        Number: isc_tpb_lock_read),
    (Name:'lock_write';       Number: isc_tpb_lock_write)
    );

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZSysUtils, Math, ZDbcInterbase6ResultSet;

{**
   Generate specific length random string and return it
   @param Len a length result string
   @return random string
}
function RandomString(Len: integer): string;
begin
  Result := '';
  while Length(Result) < Len do
    Result := Result + IntToStr(Trunc(Random(High(Integer))));
  if Length(Result) > Len then
    Result := Copy(Result, 1, Len);
end;

{**
  Create CachedResultSet with using TZCachedResultSet and return it.
  @param SQL a sql query command
  @param Statement a zeos statement object
  @param NativeResultSet a native result set
  @return cached ResultSet
}
function GetCachedResultSet(SQL: string;
  Statement: IZStatement; NativeResultSet: IZResultSet): IZResultSet;
var
  CachedResultSet: TZCachedResultSet;
begin
  if (Statement.GetResultSetConcurrency <> rcReadOnly)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(Statement.GetResultSetConcurrency);
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
end;

{**
  Generate database connection string by connection information
  @param DPB - a database connection string
  @param Dialect - a sql dialect number
  @param Info - a list connection interbase parameters
  @return a generated string length
}
function GenerateDPB(Info: TStrings; var FDPBLength, Dialect: Word): PChar;
var
  I, Pos, PValue: Integer;
  ParamNo: Word;
  DPB, Buffer, ParamName, ParamValue: String;
begin
  FDPBLength := 1;
  DPB := Char(isc_dpb_version1);

  for I := 0 to Info.Count-1 do
  begin
    Buffer := Info.Strings[I];
    Pos := FirstDelimiter(' ='#9#10#13, Buffer);
    ParamName := Copy(Buffer, 1, Pos-1);
    Delete(Buffer, 1, Pos);
    ParamValue := Buffer;
    ParamNo := GetInterbase6DatabaseParamNumber(ParamName);

    case ParamNo of
      0: Continue;
      isc_dpb_set_db_SQL_dialect:
        Dialect := StrToIntDef(ParamValue, 0);
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_sql_role_name:
        begin
          DPB := DPB + Char(ParamNo) + Char(Length(ParamValue)) + ParamValue;
          Inc(FDPBLength, 2 + Length(ParamValue));
        end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
        begin
          DPB := DPB + Char(ParamNo) + #1 + Char(StrToInt(ParamValue));
          Inc(FDPBLength, 3);
        end;
      isc_dpb_sweep:
        begin
          DPB := DPB + Char(ParamNo) + #1 + Char(isc_dpb_records);
          Inc(FDPBLength, 3);
        end;
      isc_dpb_sweep_interval:
        begin
          PValue := StrToInt(ParamValue);
          DPB := DPB + Char(ParamNo) + #4 + PChar(@PValue)[0] + PChar(@PValue)[1] +
            PChar(@PValue)[2] + PChar(@PValue)[3];
          Inc(FDPBLength, 6);
        end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
        begin
          DPB := DPB + Char(ParamNo) + #1 + #0;
          Inc(FDPBLength, 3);
        end;
    end;
  end;

  Result := StrAlloc(FDPBLength + 1);
  StrPCopy(Result, DPB);
end;

{**
   Generate transaction structuer by connection information
   @param Params - a transaction parameters list
   @param Dialect - a database connection handle
   @return a transaction ISC structure
}
function GenerateTPB(Params: TStrings; var Handle: TISC_DB_HANDLE): PISC_TEB;
var
  I: Integer;
  TPBLength,ParamNo: Word;
  TempStr, ParamValue: String;
  TPB: PChar;
  IsolationLevel: Boolean;
begin
  TPBLength := 0;
  IsolationLevel := False;

  { Prepare transaction parameters string }
  for I := 0 to Params.Count - 1 do
  begin
    ParamValue := Params.Strings[I];
    ParamNo := GetInterbase6TransactionParamNumber(ParamValue);

    case ParamNo of
      0: Continue;
      isc_tpb_lock_read, isc_tpb_lock_write:
        begin
          TempStr := TempStr + Char(ParamNo) + Char(Length(ParamValue)) + ParamValue;
          Inc(TPBLength, Length(ParamValue) + 2);
        end;
      else
        begin
          TempStr := TempStr + Char(ParamNo);
          Inc(TPBLength, 1);
        end;
    end;

    { Check what was set use transaction isolation level }
    if not IsolationLevel then
      case ParamNo of
        isc_tpb_concurrency, isc_tpb_consistency,
        isc_tpb_read_committed:
          IsolationLevel := True
        else
          IsolationLevel := False;
      end;

  end;

  { Allocate transaction parameters PChar buffer
    if temporally parameters string is empty the set null pointer for
    default database transaction}
  if (TPBLength > 0) and (IsolationLevel) then
  begin
    TPB := StrAlloc(TPBLength + 1);
    TPB := StrPCopy(TPB, TempStr);
  end else
    TPB := nil;

  { Allocate transaction structure }
  Result := AllocMem(Sizeof(TISC_TEB));
  with Result^ do
  begin
    db_handle := @Handle;
    tpb_length := TPBLength;
    tpb_address := TPB;
  end;
end;

{**
  Return interbase connection parameter number by it name
  @param Value - a connection parameter name
  @return - connection parameter number
}
function GetInterbase6DatabaseParamNumber(const Value: string): Word;
var
 I: Integer;
 ParamName: string;
begin
  ParamName := LowerCase(Value);
  Result := 0;
  if System.Pos(BPBPrefix, ParamName) = 1 then
    Delete(ParamName, 1, Length(BPBPrefix));
  for I := 1 to MAX_DPB_PARAMS do
  begin
    if ParamName = DatabaseParams[I].Name then
    begin
      Result := DatabaseParams[I].Number;
      Break;
    end;
  end;
end;

{**
  Return interbase transaction parameter number by it name
  @param Value - a transaction parameter name
  @return - transaction parameter number
}
function GetInterbase6TransactionParamNumber(const Value: string): Word;
var
 I: Integer;
 ParamName: string;
begin
  ParamName := LowerCase(Value);
  Result := 0;
  if System.Pos(TPBPrefix, ParamName) = 1 then
    Delete(ParamName, 1, Length(TPBPrefix));
  for I := 1 to MAX_TPB_PARAMS do
  begin
    if ParamName = TransactionParams[I].Name then
    begin
      Result := TransactionParams[I].Number;
      Break;
    end;
  end;
end;

{**
  Converts a Interbase6 native types into ZDBC SQL types.
  @param the interbase type
  @param the interbase subtype
  @return a SQL undepended type.

  <b>Note:</b> The interbase type and subtype get from RDB$TYPES table
}
function ConvertInterbase6ToSqlType(SqlType, SqlSubType: Integer):
  TZSQLType;
begin
  Result := ZDbcIntfs.stUnknown;

  case SqlType of
    RDB_VARCHAR, RDB_CSTRING: Result := stString;
    RDB_CHAR:
      begin
        case SqlSubType of
          RDB_CHAR_NONE: Result := stString;
          RDB_CHAR_BIN: Result := stBytes;
        end;
      end;
    RDB_D_FLOAT: Result := stDouble;
    RDB_DOUBLE: Result := stDouble;
    RDB_FLOAT: Result := stFloat;
    RDB_INT64, RDB_BLOB_ID, RDB_QUAD:
      begin
        case SqlSubType of
          RDB_NUMBERS_NONE: Result := stBigDecimal;
          RDB_NUMBERS_NUMERIC: Result := stDouble;
          RDB_NUMBERS_DECIMAL: Result := stDouble;
        end;
      end;
    RDB_INTEGER:
      begin
        case SqlSubType of
          RDB_NUMBERS_NONE: Result := stInteger;
          RDB_NUMBERS_NUMERIC: Result := stDouble;
          RDB_NUMBERS_DECIMAL: Result := stDouble;
        end;
      end;
    RDB_SMALLINT:
      begin
        case SqlSubType of
          RDB_NUMBERS_NONE: Result := stShort;
          RDB_NUMBERS_NUMERIC: Result := stInteger;
          RDB_NUMBERS_DECIMAL: Result := stFloat;
        end;
      end;
    RDB_DATE: Result := stDate;
    RDB_TIME: Result := stTime;
    RDB_TIMESTAMP: Result := stTimestamp;
    RDB_BLOB:
      begin
       case SqlSubType of
          RDB_BLOB_NONE: Result := stBinaryStream;
          RDB_BLOB_TEXT: Result := stAsciiStream;
          RDB_BLOB_BLR: Result := stBinaryStream;
          RDB_BLOB_ACL: Result := stAsciiStream;
          RDB_BLOB_RESERVED: Result := ZDbcIntfs.stUnknown;
          RDB_BLOB_ENCODED: Result := stAsciiStream;
          RDB_BLOB_DESCRIPTION: Result := stAsciiStream;
        end;
      end;
    else
      Result := ZDbcIntfs.stUnknown;
  end;
end;

{**
   Return Interbase SqlType by it number
   @param Value the SqlType number 
}
function GetNameSqlType(Value: Word): string;
begin
  case Value of
    SQL_VARYING: Result := 'SQL_VARYING';
    SQL_TEXT: Result := 'SQL_TEXT';
    SQL_DOUBLE: Result := 'SQL_DOUBLE';
    SQL_FLOAT: Result := 'SQL_FLOAT';
    SQL_LONG: Result := 'SQL_LONG';
    SQL_SHORT: Result := 'SQL_SHORT';
    SQL_TIMESTAMP: Result := 'SQL_TIMESTAMP';
    SQL_BLOB: Result := 'SQL_BLOB';
    SQL_D_FLOAT: Result := 'SQL_D_FLOAT';
    SQL_ARRAY: Result := 'SQL_ARRAY';
    SQL_QUAD: Result := 'SQL_QUAD';
    SQL_TYPE_TIME: Result := 'SQL_TYPE_TIME';
    SQL_TYPE_DATE: Result := 'SQL_TYPE_DATE';
    SQL_INT64: Result := 'SQL_INT64';
    SQL_BOOLEAN: Result := 'SQL_BOOLEAN';
  else
    Result := 'Unknown';
  end
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a Interbase Plain drver
  @param StatusVector a status vector. It contain information about error
  @param Sql a sql query commend
}
procedure CheckInterbase6Error(PlainDriver: IZInterbasePlainDriver;
  StatusVector: TARRAY_ISC_STATUS; SQL: string = '');
var
  Msg: array[0..1024] of Char;
  PStatusVector: PISC_STATUS;
  ErrorMessage, ErrorSqlMessage: string;
  ErrorCode: LongInt;
begin
  if (StatusVector[0] = 1) and (StatusVector[1] > 0) then
  begin
    PStatusVector := @StatusVector;
    PlainDriver.isc_interprete(Msg, @PStatusVector);
    ErrorMessage := StrPas(Msg);

    ErrorCode := PlainDriver.isc_sqlcode(@StatusVector);
    PlainDriver.isc_sql_interprete(ErrorCode, Msg, 1024);
    ErrorSqlMessage := StrPas(Msg);

    if SQL <> '' then
      SQL := Format('The SQL: %s; ', [SQL]);

    if ErrorMessage <> '' then
    begin
      raise EZSQLException.Create(SQL + Format(
        'SQL Error: %s. Erorr code: %d. %s',
        [ErrorMessage, ErrorCode, ErrorSqlMessage]));
    end;
  end;
end;

{**
   Prepare statement, create statement handle and allocate memry for
   result set values.
   @param PlainDriver a interbase plain driver
   @param Handle a interbase connection handle
   @param TrHandle a transaction handle
   @param Dialect a interbase sql dialect number
   @param Sql a sql query
   @param StmtHandle a statement handle
   @param SqlData a interbase sql result data
   @return sql statement type
}
function PrepareStatement(PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; TrHandle: PISC_TR_HANDLE; Dialect: Word;
  SQL: string; var StmtHandle: TISC_STMT_HANDLE; SqlData: IZResultSQLDA):
  TZIbSqlStatementType;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  { Allocate an sql statement }
  PlainDriver.isc_dsql_alloc_statement2(@StatusVector, Handle, @StmtHandle);
  CheckInterbase6Error(PlainDriver, StatusVector, Sql);

  { Prepare an sql statement }
  PlainDriver.isc_dsql_prepare(@StatusVector, TrHandle, @StmtHandle,
    0, PChar(SQL), Dialect, nil);
  CheckInterbase6Error(PlainDriver, StatusVector, SQL);

  { Set Statement Type }
  Result := GetStatementType(PlainDriver, StmtHandle);

  if Result in [stUnknown, stGetSegment, stPutSegment, stStartTrans] then
  begin
    FreeStatement(PlainDriver, StmtHandle);
    raise EZSQLException.Create('Statement not permitted');
  end;

  if not Assigned(SqlData) then
    Exit;

  { Initialise ouput param and fields }
  PlainDriver.isc_dsql_describe(@StatusVector, @StmtHandle, Dialect,
    SqlData.GetData);
  CheckInterbase6Error(PlainDriver, StatusVector, SQL);

  if Result in [stSelect, stSelectForUpdate, stExecProc] then
  begin
    if SqlData.GetData^.sqld > SqlData.GetData^.sqln then
    begin
      SqlData.SetCountFields(SqlData.GetData^.sqld);
      PlainDriver.isc_dsql_describe(@StatusVector, @StmtHandle,
        Dialect, SqlData.GetData);
      CheckInterbase6Error(PlainDriver, StatusVector, Sql);
    end;
    SqlData.AllocateData;
  end;
end;

{**
   Return interbase statement type by statement handle
   @param PlainDriver a interbase plain driver
   @param StmtHandle a statement handle
   @return interbase statement type
}
function GetStatementType(PlainDriver: IZInterbasePlainDriver;
  StmtHandle: TISC_STMT_HANDLE): TZIbSqlStatementType;
var
  TypeItem: Char;
  StatusVector: TARRAY_ISC_STATUS;
  StatementLength: integer;
  StatementBuffer: array [0..7] of Char;
begin
  Result := stUnknown;
  TypeItem := Char(isc_info_sql_stmt_type);

  { Get information about a prepared DSQL statement. }
  PlainDriver.isc_dsql_sql_info(@StatusVector, @StmtHandle, 1,
    @TypeItem, SizeOf(StatementBuffer), StatementBuffer);
  CheckInterbase6Error(PlainDriver, StatusVector);

  if StatementBuffer[0] = Char(isc_info_sql_stmt_type) then
  begin
    StatementLength := PlainDriver.isc_vax_integer(
      @StatementBuffer[1], 2);
    Result := TZIbSqlStatementType(PlainDriver.isc_vax_integer(
      @StatementBuffer[3], StatementLength));
  end;
end;

{**
   Free interbse allocated statement and SQLDA for input and utput parameters
   @param  the interbase plain driver
   @param  the interbse statement handle
}
procedure FreeStatement(PlainDriver: IZInterbasePlainDriver;
  StatementHandle: TISC_STMT_HANDLE);
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  if StatementHandle <> nil then
  begin
    PlainDriver.isc_dsql_free_statement(@StatusVector, @StatementHandle,
      DSQL_drop);
    CheckInterbase6Error(PlainDriver, StatusVector);
  end;
end;

{**
   Get affected rows.
   <i>Note:<i> it function may call after statement execution
   @param PlainDriver a interbase plain driver
   @param StmtHandle a statement handle
   @param StatementType a statement type
   @return affected rows
}
function GetAffectedRows(PlainDriver: IZInterbasePlainDriver;
  StmtHandle: TISC_STMT_HANDLE; StatementType: TZIbSqlStatementType): Integer;
var
  ReqInfo: Char;
  OutBuffer: array[0..255] of Char;
  StatusVector: TARRAY_ISC_STATUS;
begin
  Result := -1;
  ReqInfo := Char(isc_info_sql_records);

  if PlainDriver.isc_dsql_sql_info(@StatusVector, @StmtHandle, 1,
    @ReqInfo, SizeOf(OutBuffer), OutBuffer) > 0 then
    Exit;
  if OutBuffer[0] = Char(isc_info_sql_records) then
  begin
    case StatementType of
      stUpdate: Result := PlainDriver.isc_vax_integer(@OutBuffer[6], 4);
      stDelete: Result := PlainDriver.isc_vax_integer(@OutBuffer[13], 4);
      stInsert: Result := PlainDriver.isc_vax_integer(@OutBuffer[27], 4);
      else Result := -1;
    end;
  end;
end;

{**
   Prepare sql statement parameters and fill parameters by values
   @param PlainDriver a interbase plain driver
   @param InParamValues a array of parameters values
   @param InParamTypes a array of parameters sql types
   @param InParamCount a parameters count
   @param Dialect a interbase sql dialect number
   @param StmtHandle a statement handle
   @param SqlData a interbase sql result data
}
procedure PrepareParameters(PlainDriver: IZInterbasePlainDriver; SQL: string;
  InParamValues: TVarArray; InParamTypes: TZSQLTypeArray; InParamCount: Integer;
  Dialect: Word; var StmtHandle: TISC_STMT_HANDLE; ParamSqlData: IZParamsSQLDA);
var
  I: Integer;
  TempBlob: IZBlob;
  TempStream: TStream;
  StatusVector: TARRAY_ISC_STATUS;
begin
  {check dynamic sql}
  PlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, Dialect,
    ParamSqlData.GetData);
  CheckInterbase6Error(PlainDriver, StatusVector, SQL);

  { Resize XSQLDA structure if needed }
  if ParamSqlData.GetData^.sqld > ParamSqlData.GetData^.sqln then
  begin
    ParamSqlData.SetCountFields(ParamSqlData.GetData^.sqld);
    PlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, Dialect,
      ParamSqlData.GetData);
    CheckInterbase6Error(PlainDriver, StatusVector, SQL);
  end;

  ParamSqlData.AllocateData;

  if InParamCount <> ParamSqlData.GetFieldCount then
    raise EZSQLException.Create('Input parameter count is not then expected.');

  {$RANGECHECKS OFF}
  for I := 0 to ParamSqlData.GetFieldCount-1 do
  begin
    if VarIsEmpty(InParamValues[I]) or (VarIsNull(InParamValues[I]))then
    begin
      ParamSqlData.UpdateNull(I, True);
      Continue;
    end;
    case InParamTypes[I] of
      stBoolean: ParamSqlData.UpdateBoolean(I, InParamValues[I]);
      stByte: ParamSqlData.UpdateByte(I, InParamValues[I]);
      stShort: ParamSqlData.UpdateShort(I, InParamValues[I]);
      stInteger: ParamSqlData.UpdateInt(I, InParamValues[I]);
      stLong: ParamSqlData.UpdateLong(I, InParamValues[I]);
      stFloat: ParamSqlData.UpdateFloat(I, InParamValues[I]);
      stDouble: ParamSqlData.UpdateDouble(I, InParamValues[I]);
{$IFDEF VER130BELOW}
      stBigDecimal: ParamSqlData.UpdateBigDecimal(I,
        Integer(VarAsType(InParamValues[I], varInteger)));
{$ELSE}
      stBigDecimal: ParamSqlData.UpdateBigDecimal(I,
        VarAsType(InParamValues[I], varInt64));
{$ENDIF}
      stString: ParamSqlData.UpdateString(I, InParamValues[I]);
      stUnicodeString: ParamSqlData.UpdateString(I, InParamValues[I]);
      stBytes: ParamSqlData.UpdateBytes(I, InParamValues[I]);
      stDate: ParamSqlData.UpdateDate(I, InParamValues[I]);
      stTime: ParamSqlData.UpdateTime(I, InParamValues[I]);
      stTimestamp: ParamSqlData.UpdateTimestamp(I, InParamValues[I]);
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        begin
          if VarType(InParamValues[I]) = varUnknown then
          begin
            TempBlob := IZBlob(TVarData(InParamValues[I]).VUnknown);
            if not TempBlob.IsEmpty then
            begin
              TempStream := TempBlob.GetStream;
              try
                ParamSqlData.WriteBlob(I, TempStream);
              finally
                TempStream.Free;
              end;
            end else
              ParamSqlData.UpdateNull(I, True);
          end;
        end;  
      else
        raise EZIBConvertError.Create('Unsupported parameter type.');
    end;
  end;
 {$RANGECHECKS ON}
end;

{ TSQLDA }

{**
   Allocate memory for SQLVar in SQLDA structure for every
   fields by it length.
}
procedure TZSQLDA.AllocateData;
var
  I: Integer;
  SqlVar: PXSQLVAR;
begin
  {$RANGECHECKS OFF}
  for I := 0 to FXSQLDA.sqld - 1 do
  begin
    SqlVar := @FXSQLDA.SqlVar[I];
    case SqlVar.sqltype and (not 1) of
      SQL_BOOLEAN, SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_DATE,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
      SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        begin
          if SqlVar.sqllen = 0 then
            GetMem(SqlVar.sqldata, 1)
          else
            GetMem(SqlVar.sqldata, SqlVar.sqllen)
        end;
      SQL_VARYING:
          GetMem(SqlVar.sqldata, SqlVar.sqllen + 2);
    end;

    if (SqlVar.sqltype and 1) <> 0 then
      GetMem(SqlVar.sqlind, SizeOf(Short))
    else
      SqlVar.sqlind := nil;
  end;
  {$RANGECHECKS ON}
end;

{**
   Chech reange count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZSQLDA.CheckRange(const Index: Word);
begin
  Assert(Index < Word(FXSQLDA.sqln), 'Out of Range.');
end;

{**
   Return alias name for field
   @param Index the index fields
   @return the alias name
}
function TZSQLDA.GetAliasName(const Index: Word): string;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  SetString(Result, FXSQLDA.sqlvar[Index].aliasname,
    FXSQLDA.sqlvar[Index].aliasname_length);
  {$RANGECHECKS ON}
end;

{**
   Return pointer to SQLDA structure
}
function TZSQLDA.GetData: PXSQLDA;
begin
  result := FXSQLDA;
end;

{**
   Get fields count not allocated.
   @return fields count
}
function TZSQLDA.GetFieldCount: Integer;
begin
  Result := FXSQLDA.sqln;
end;

{**
   Return field index by it name
   @param Index the index fields
   @return the index field
}
function TZSQLDA.GetFieldIndex(const Name: String): Word;
begin
  {$RANGECHECKS OFF}
  for Result := 0 to GetFieldCount - 1 do
    if FXSQLDA.sqlvar[Result].aliasname_length = Length(name) then
      if StrLIComp(@FXSQLDA.sqlvar[Result].aliasname, PChar(Name),
        FXSQLDA.sqlvar[Result].aliasname_length) = 0 then Exit;
  raise Exception.Create('Field '+ name +' not found.');
  {$RANGECHECKS ON}
end;

{**
   Return field length
   @param Index the index fields
   @return the field lenth
}
function TZSQLDA.GetFieldLength(const Index: Word): SmallInt;
begin
  {$RANGECHECKS OFF}
  case GetSQLType(Index) of
    SQL_TEXT: Result := GetSQLLen(Index);
    SQL_VARYING: Result := GetSQLLen(Index);
    //SQL_VARYING: Result := FPlainDriver.isc_vax_integer(GetData.sqlvar[Index].sqldata, 2);
    else
      Result := GetSQLLen(Index);
  end;
  {$RANGECHECKS ON}
end;

{**
   Return field scale
   @param Index the index fields
   @return the field scale
}
function TZSQLDA.GetFieldScale(const Index: Word): integer;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  result := FXSQLDA.sqlvar[Index].sqlscale;
  {$RANGECHECKS ON}
end;

{**
   Convert Interbase sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZSQLDA.GetFieldSqlType(const Index: Word): TZSQLType;
var
  SqlScale: Integer;
  SqlSubType: Integer;
begin
  SqlScale := GetFieldScale(Index);
  SqlSubType := GetSQLSubType(Index);

  case GetSQLType(Index) of
    SQL_VARYING, SQL_TEXT: Result := stString;
    SQL_LONG:
      begin
        if SqlScale = 0 then
          Result := stInteger
        else if Abs(SqlScale) <= 4 then
          Result := stFloat
        else
          Result := stDouble;
      end;
    SQL_SHORT:
      begin
        if SqlScale = 0 then
          Result := stShort
        else Result := stFloat;
      end;
    SQL_FLOAT: Result := stFloat;
    SQL_DOUBLE: Result := stDouble;
    SQL_DATE: Result := stTimestamp;
    SQL_TYPE_TIME: Result := stTime;
    SQL_TYPE_DATE: Result := stDate;
    SQL_INT64:
      begin
        if SqlScale = 0 then
          Result := stLong
        else if Abs(SqlScale) <= 4 then
          Result := stFloat
        else
          Result := stDouble;
      end;
    SQL_QUAD, SQL_ARRAY, SQL_BLOB:
      begin
        if SqlSubType = isc_blob_text then
          Result := stAsciiStream
        else Result := stBinaryStream;
      end;
    //SQL_ARRAY: Result := stBytes;
    else Result := stString;
  end;
end;

{**
   Return own name for field
   @param Index the index fields
   @return the own name
}
function TZSQLDA.GetOwnName(const Index: Word): string;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  SetString(Result, FXSQLDA.sqlvar[Index].OwnName,
    FXSQLDA.sqlvar[Index].OwnName_length);
  {$RANGECHECKS ON}  
end;

{**
   Return real name for field
   @param Index the index fields
   @return the real name
}
function TZSQLDA.GetRelName(const Index: Word): string;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  SetString(Result, FXSQLDA.sqlvar[Index].RelName,
    FXSQLDA.sqlvar[Index].RelName_length);
  {$RANGECHECKS ON}  
end;

{**
   Get Interbase sql fields lenth
   @param Index the index fields
   @return Interbase sql fields lenth
}
function TZSQLDA.GetSQLLen(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  result := FXSQLDA.sqlvar[Index].sqllen;
  {$RANGECHECKS ON}
end;

{**
   Return sql name for field
   @param Index the index fields
   @return the sql name
}
function TZSQLDA.GetSqlName(const Index: Word): string;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  SetString(Result, FXSQLDA.sqlvar[Index].sqlname,
    FXSQLDA.sqlvar[Index].sqlname_length);
  {$RANGECHECKS ON}  
end;

{**
   Get Interbase subsql type
   @param Index the index fields
   @return the Interbase subsql
}
function TZSQLDA.GetSQLSubType(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  result := FXSQLDA.sqlvar[Index].sqlsubtype;
  {$RANGECHECKS ON}
end;

{**
   Get Interbase sql type
   @param Index the index fields
   @return the interbase sql type
}
function TZSQLDA.GetSQLType(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  result := FXSQLDA.sqlvar[Index].sqltype and not (1);
  {$RANGECHECKS ON}
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZSQLDA.IsBlob(const Index: Word): boolean;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_BLOB);
  {$RANGECHECKS ON}
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if field nullable overwise false
}
function TZSQLDA.IsNullable(const Index: Word): boolean;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  Result := FXSQLDA.sqlvar[Index].sqltype and 1 = 1
  {$RANGECHECKS ON}
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
procedure TZSQLDA.SetCountFields(Value: Word);
begin
  ReallocMem(FXSQLDA, XSQLDA_LENGTH(Value));
  FXSQLDA.sqln := Value;
  FXSQLDA.sqld := Value;
end;

{ TParamsSQLDA }

{**
   Constructs this object and assignes the main properties.
   param PlainDriver the interbase plain driver
}
constructor TZParamsSQLDA.Create(PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE);
begin
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FTransactionHandle := TransactionHandle;

  GetMem(FXSQLDA, XSQLDA_LENGTH(0));
  FillChar(FXSQLDA^, XSQLDA_LENGTH(0), 0);
  FXSQLDA.sqln := 0;
  FXSQLDA.sqld := 0;

  FXSQLDA.version := SQLDA_VERSION1;
end;

{**
   Free allocated memory and free object
}
destructor TZParamsSQLDA.Destroy;
var
  I: integer;
begin
  {$RANGECHECKS OFF}
  for I := 0 to FXSQLDA.sqln - 1 do
  with FXSQLDA.sqlvar[i] do
  begin
    if (sqldata <> nil) then
      FreeMem(sqldata);
    FreeMem(sqlind);
  end;
  FreeMem(FXSQLDA);
  {$RANGECHECKS ON}
  inherited Destroy;
end;

{**
   Encode pascal string to Interbase paramter buffer
   @param Code the Interbase data type
   @param Index the index target filed
   @param Str the source string
}
procedure TZParamsSQLDA.EncodeString(Code: Smallint; const Index: Word;
  const Str: String);
var
  len: Cardinal;
begin
  {$RANGECHECKS OFF}
   with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT :
        begin
          len := Length(str);
          if sqllen = 0 then
            getmem(sqldata, len) else
            ReallocMem(sqldata, len);
          sqllen := len;
          Move(PChar(str)^, sqldata^, sqllen);
        end;
      SQL_VARYING :
        begin
          len := Length(str);
          if sqllen = 0 then
            getmem(sqldata, len+2) else
            ReallocMem(sqldata, len+2);
          sqllen := len + 2;
          PISC_VARYING(sqldata).strlen := len;
          Move(PChar(str)^, PISC_VARYING(sqldata).str,
            PISC_VARYING(sqldata).strlen);
        end;
    end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter BigDecimal value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBigDecimal(const Index: Integer; Value: Int64);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter Boolean value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBoolean(const Index: Integer; Value: boolean);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := ord(Value);
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := ord(Value);
        SQL_LONG      : PInteger(sqldata)^ := ord(Value);
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := ord(Value);
        SQL_BOOLEAN   : PSmallint(sqldata)^ := ord(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := ord(Value);
        SQL_INT64     : PInt64(sqldata)^ := ord(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(ord(Value)));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(ord(Value)));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter Byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateByte(const Index: Integer; Value: ShortInt);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN: begin
                       if FPlainDriver.GetProtocol <> 'interbase-7' then
                         raise EZIBConvertError.Create('Data type does not supported');
                       PSmallint(sqldata)^ := Value;
                     end;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBytes(const Index: Integer; Value: TByteDynArray);
begin

end;

{**
   Set up parameter Date value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateDate(const Index: Integer; Value: TDateTime);
begin
  UpdateTimestamp(Index, Value);
end;

{**
   Set up parameter Double value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateDouble(const Index: Integer; Value: Double);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_LONG   : PInteger(sqldata)^  := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Trunc(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
        SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter Float value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateFloat(const Index: Integer; Value: Single);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_LONG   : PInteger(sqldata)^  := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Trunc(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
        SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter integer value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateInt(const Index: Integer; Value: Integer);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter Long value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateLong(const Index: integer; Value: Integer);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter null value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateNull(const Index: Integer; Value: boolean);
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
    if (sqlind <> nil) then
      case Value of
        True  : sqlind^ := -1;
        False : sqlind^ :=  0;
      end;
  {$RANGECHECKS ON}    
end;

{**
   Set up parameter PChar value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdatePChar(const Index: Integer; Value: PChar);
var
  TempString: string;
begin
  TempString := Value;
  UpdateString(Index, TempString);
end;

{**
   Set up parameter Interbase QUAD value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateQuad(const Index: Word; const Value: TISC_QUAD);
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
    if not ((sqlind <> nil) and (sqlind^ = -1)) then
    begin
      case (sqltype and not(1)) of
        SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: PISC_QUAD(sqldata)^ := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
    end else
      raise EZIBConvertError.Create('Invalid State.');
  {$RANGECHECKS ON}  
end;

{**
   Set up parameter short value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateShort(const Index: Integer; Value: SmallInt);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create('Invalid Data Type.');
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_D_FLOAT,
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EZIBConvertError.Create('Error Convertion.');
      end;
      if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter String value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateString(const Index: Integer; Value: string);
var
 SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));
    case SQLCode of
      SQL_TEXT      : EncodeString(SQL_TEXT, Index, Value);
      SQL_VARYING   : EncodeString(SQL_VARYING, Index, Value);
    else
      raise EZIBConvertError.Create('Error Convertion.');
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Set up parameter Time value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateTime(const Index: Integer; Value: TDateTime);
begin
  UpdateTimestamp(Index, Value);
end;

{**
   Set up parameter Timestamp value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateTimestamp(const Index: Integer; Value: TDateTime);
var
  y, m, d: word;
  hr, min, sec, msec: word;
  SQLCode: SmallInt;
  TmpDate: TCTimeStructure;
begin
  CheckRange(Index);
 {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    DecodeDate(Value, y, m, d);
    DecodeTime(Value, hr, min, sec, msec);
    TmpDate.tm_year := y - 1900;
    TmpDate.tm_mon := m - 1;
    TmpDate.tm_mday := d;
    TmpDate.tm_hour := hr;
    TmpDate.tm_min := min;
    TmpDate.tm_sec := sec;
    TmpDate.tm_wday := 0;
    TmpDate.tm_yday := 0;
    TmpDate.tm_isdst := 0;

    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    case SQLCode of
      SQL_TYPE_DATE : FPlainDriver.isc_encode_sql_date(@TmpDate, PISC_DATE(sqldata));
      SQL_TYPE_TIME : FPlainDriver.isc_encode_sql_time(@TmpDate, PISC_TIME(sqldata));
      SQL_TIMESTAMP :
        FPlainDriver.isc_encode_date(@TmpDate, PISC_QUAD(sqldata));
      else
        raise EZIBConvertError.Create('Invalid State.');
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  {$RANGECHECKS ON}
end;

{**
   Write stream to blob field
   @param Index an index field number
   @param Stream the souse data stream
}
procedure TZParamsSQLDA.WriteBlob(const Index: Integer; Stream: TStream);
var
  Buffer: PChar;
  BlobId: TISC_QUAD;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
  BlobSize, CurPos, SegLen: Integer;
begin
  BlobHandle := nil;
  Stream.Seek(0, 0);

  { create blob handle }
  FPlainDriver.isc_create_blob2(@StatusVector, FHandle, FTransactionHandle,
    @BlobHandle, @BlobId, 0, nil);
  CheckInterbase6Error(FPlainDriver, StatusVector);

  Stream.Position := 0;
  BlobSize := Stream.Size;
  Buffer := AllocMem(BlobSize);
  Stream.ReadBuffer(Buffer^, BlobSize);

  { put data to blob }
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if FPlainDriver.isc_put_segment(@StatusVector, @BlobHandle, SegLen,
         PChar(@Buffer[CurPos])) > 0 then
    CheckInterbase6Error(FPlainDriver, StatusVector);
    Inc(CurPos, SegLen);
  end;

  { close blob handle }
  FPlainDriver.isc_close_blob(@StatusVector, @BlobHandle);
  CheckInterbase6Error(FPlainDriver, StatusVector);

  Stream.Seek(0, 0);
  UpdateQuad(Index, BlobId);
end;

{ TResultSQLDA }

procedure TZResultSQLDA.AllocateData;
begin
  inherited AllocateData;
  SetLength(FDefaults, GetFieldCount);
end;

{**
   Decode Interbase field value to pascal string
   @param Code the Interbase data type
   @param Index field index
   @result the field string
}
function TZResultSQLDA.DecodeString(const Code: Smallint;
  const Index: Word): String;
begin
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  case Code of
    SQL_TEXT    : Result := TrimRight(BufferToStr(sqldata, sqllen));
    SQL_VARYING : SetString(Result, PISC_VARYING(sqldata).str,
                    PISC_VARYING(sqldata).strlen);
  end;
  {$RANGECHECKS ON}
end;


{**
   Constructs this object and assignes the main properties.
   param PlainDriver the interbase plain driver
}
constructor TZResultSQLDA.Create(PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE);
begin
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FTransactionHandle := TransactionHandle;

  GetMem(FXSQLDA, XSQLDA_LENGTH(0));
  FillChar(FXSQLDA^, XSQLDA_LENGTH(0), 0);
  FXSQLDA.sqln := 0;
  FXSQLDA.sqld := 0;

  FXSQLDA.version := SQLDA_VERSION1;
end;

{**
   Decode Interbase field value to pascal string
   @param Code the Interbase data type
   @param Index field index
   @param the field string
}
procedure TZResultSQLDA.DecodeString2(const Code: Smallint; const Index: Word;
  out Str: string);
begin
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  case Code of
    SQL_TEXT    : Str := TrimRight(BufferToStr(sqldata, sqllen));
    SQL_VARYING : SetString(Str, PISC_VARYING(sqldata).str,
      PISC_VARYING(sqldata).strlen);
  end;
  {$RANGECHECKS ON}
end;

{**
   Return BigDecimal field value
   @param Index the field index
   @return the field BigDecimal value
}
function TZResultSQLDA.GetBigDecimal(const Index: Integer): Int64;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
        SQL_INT64     : Result := PInt64(sqldata)^;
        SQL_TEXT      : Result := StrToInt64(DecodeString(SQL_TEXT, Index));
        SQL_VARYING   : Result := StrToInt64(DecodeString(SQL_VARYING, Index));
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Boolean field value
   @param Index the field index
   @return the field boolean value
}
function TZResultSQLDA.GetBoolean(const Index: Integer): Boolean;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := False;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale] <> 0;
        SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale] <> 0;
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale] <> 0;
        SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
        SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^ <> 0;
        SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
        SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
        SQL_TEXT      : Result := StrToInt(DecodeString(SQL_TEXT, Index)) <> 0;
        SQL_VARYING   : Result := StrToInt(DecodeString(SQL_VARYING, Index)) <> 0;
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Byte field value
   @param Index the field index
   @return the field Byte value
}
function TZResultSQLDA.GetByte(const Index: Integer): ShortInt;
begin
  Result := ShortInt(GetShort(Index));
end;

{**
   Return Bytes field value
   @param Index the field index
   @return the field Bytes value
}
function TZResultSQLDA.GetBytes(const Index: Integer): TByteDynArray;
begin
  Result := nil;
end;

{**
   Return Date field value
   @param Index the field index
   @return the field Date value
}
function TZResultSQLDA.GetDate(const Index: Integer): TDateTime;
begin
  Result := Trunc(GetTimestamp(Index));
end;

{**
   Return Double field value
   @param Index the field index
   @return the field Double value
}
function TZResultSQLDA.GetDouble(const Index: Integer): Double;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := PDouble(sqldata)^;
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := PDouble(sqldata)^;
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := PSingle(sqldata)^;
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
        SQL_INT64     : Result := PInt64(sqldata)^;
        SQL_TEXT      : Result := StrToFloat(DecodeString(SQL_TEXT, Index));
        SQL_VARYING   : Result := StrToFloat(DecodeString(SQL_VARYING, Index));
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Float field value
   @param Index the field index
   @return the field Float value
}
function TZResultSQLDA.GetFloat(const Index: Integer): Single;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := PDouble(sqldata)^;
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := PDouble(sqldata)^;
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := PSingle(sqldata)^;
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
        SQL_INT64     : Result := PInt64(sqldata)^;
        SQL_TEXT      : Result := StrToFloat(DecodeString(SQL_TEXT, Index));
        SQL_VARYING   : Result := StrToFloat(DecodeString(SQL_VARYING, Index));
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Integer field value
   @param Index the field index
   @return the field Integer value
}
function TZResultSQLDA.GetInt(const Index: Integer): Integer;
begin
  Result := Integer(GetLong(Index));
end;

{**
   Return Long field value
   @param Index the field index
   @return the field Long value
}
function TZResultSQLDA.GetLong(const Index: Integer): LongInt;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
        SQL_INT64     : Result := PInt64(sqldata)^;
        SQL_TEXT      : Result := StrToInt(DecodeString(SQL_TEXT, Index));
        SQL_VARYING   : Result := StrToInt(DecodeString(SQL_VARYING, Index));
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Pchar field value
   @param Index the field index
   @return the field PChar value
}
function TZResultSQLDA.GetPChar(const Index: Integer): PChar;
var
  TempStr: string;
begin
  TempStr := GetString(Index);
  Result := PChar(TempStr);
end;

{**
   Return Short field value
   @param Index the field index
   @return the field Short value
}
function TZResultSQLDA.GetShort(const Index: Integer): SmallInt;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
        SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
        SQL_INT64     : Result := PInt64(sqldata)^;
        SQL_TEXT      : Result := StrToInt(DecodeString(SQL_TEXT, Index));
        SQL_VARYING   : Result := StrToInt(DecodeString(SQL_VARYING, Index));
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return String field value
   @param Index the field index
   @return the field String value
}
function TZResultSQLDA.GetString(const Index: Integer): string;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  Result := '';
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
        SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
        SQL_INT64,
        SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
        SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
        SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
        SQL_BOOLEAN   :
          if Boolean(PSmallint(sqldata)^) = True then
            Result := 'YES'
          else
            Result := 'NO';
        SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
        SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
        SQL_TEXT      : DecodeString2(SQL_TEXT, Index, Result);
        SQL_VARYING   : DecodeString2(SQL_VARYING, Index, Result);
        SQL_BLOB      : if VarIsEmpty(FDefaults[Index]) then
                        begin
                          ReadBlobFromString(Index, Result);
                          FDefaults[Index] := Result;
                        end
                        else
                          Result := FDefaults[Index];

      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
  {$RANGECHECKS ON}
end;

{**
   Return Time field value
   @param Index the field index
   @return the field Time value
}
function TZResultSQLDA.GetTime(const Index: Integer): TDateTime;
begin
  Result := Frac(GetTimestamp(Index));
end;

{**
   Return Timestamp field value
   @param Index the field index
   @return the field Timestamp value
}
function TZResultSQLDA.GetTimestamp(const Index: Integer): TDateTime;
var
  TempDate: TCTimeStructure;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;

    case (sqltype and not(1)) of
        SQL_TIMESTAMP : begin
                          FPlainDriver.isc_decode_date(PISC_QUAD(sqldata), @TempDate);
                          Result := SysUtils.EncodeDate(TempDate.tm_year + 1900,
                            TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
                            TempDate.tm_min, TempDate.tm_sec, 0);
                        end;
        SQL_TYPE_DATE : begin
                          FPlainDriver.isc_decode_sql_date(PISC_DATE(sqldata), @TempDate);
                          Result := SysUtils.EncodeDate(Word(TempDate.tm_year + 1900),
                            Word(TempDate.tm_mon + 1), Word(TempDate.tm_mday));
                        end;
        SQL_TYPE_TIME : begin
                          FPlainDriver.isc_decode_sql_time(PISC_TIME(sqldata), @TempDate);
                          Result := SysUtils.EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
                            Word(TempDate.tm_sec), 0);
                        end;
        else
          Result := Trunc(GetDouble(Index));
        end;
  end;
 {$RANGECHECKS ON}
end;

{**
   Indicate field null
   @param Index the field index
   @return true if fied value NULL overwise false
}
function TZResultSQLDA.IsNull(const Index: Integer): Boolean;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
    Result := (sqlind <> nil) and (sqlind^ = ISC_NULL);
  {$RANGECHECKS ON}
end;

{**
   Return Interbase QUAD field value
   @param Index the field index
   @return the field Interbase QUAD value
}
function TZResultSQLDA.GetQuad(const Index: Integer): TISC_QUAD;
begin
  CheckRange(Index);
  {$RANGECHECKS OFF}
  with FXSQLDA.sqlvar[Index] do
  if not ((sqlind <> nil) and (sqlind^ = -1)) then
    case (sqltype and not(1)) of
      SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISC_QUAD(sqldata)^;
    else
      raise EZIBConvertError.Create('Invalid Data Type.' + inttostr((sqltype and not(1))));
    end
  else
    raise EZIBConvertError.Create('Invalid State.');
  {$RANGECHECKS ON}  
end;

{**
   Return Variant field value
   @param Index the field index
   @return the field Variant value
}
function TZResultSQLDA.GetValue(const Index: Word): Variant;
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  with FXSQLDA.sqlvar[Index] do
  begin
    VarClear(Result);
    if (sqlind <> nil) and (sqlind^ = -1) then Exit;
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
        SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
        SQL_DOUBLE : Result := PDouble(sqldata)^;
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
    end else
      case SQLCode of
        SQL_DOUBLE    : Result := PDouble(sqldata)^;
        SQL_TIMESTAMP : Result := GetTimestamp(Index);
        SQL_TYPE_DATE : Result := GetDate(Index);
        SQL_TYPE_TIME : Result := GetTime(Index);
        SQL_LONG      : Result := PInteger(sqldata)^;
        SQL_D_FLOAT,
        SQL_FLOAT     : Result := PSingle(sqldata)^;
        SQL_BOOLEAN: begin
                       if FPlainDriver.GetProtocol <> 'interbase-7' then
                         raise EZIBConvertError.Create('Data type does not supported');
                       Result := IntToStr(PSmallint(sqldata)^);
                     end;
        SQL_SHORT     : Result := PSmallint(sqldata)^;
  {$IFDEF COMPILER6_UP}
        SQL_INT64     : Result := PInt64(sqldata)^;
  {$ELSE}
        SQL_INT64     : Result := Integer(PInt64(sqldata)^);
  {$ENDIF}
        SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
        SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
        SQL_BLOB      : if VarIsEmpty(FDefaults[Index]) then
                        begin
                          ReadBlobFromVariant(Index, FDefaults[Index]);
                          Result := FDefaults[Index];
                        end
                        else
                          Result := Double(FDefaults[Index]);
      else
        raise EZIBConvertError.Create(Format('Error Convertion. Field - %s. SQLType - %s',
          [GetAliasName(Index), GetNameSqlType(SQLCode)]));
      end;
  end;
end;

destructor TZResultSQLDA.Destroy;
var
  I: integer;
begin
  {$RANGECHECKS OFF}
  for I := 0 to FXSQLDA.sqln - 1 do
  with FXSQLDA.sqlvar[i] do
  begin
    if (sqldata <> nil) then
      FreeMem(sqldata);
    FreeMem(sqlind);
  end;
  FreeMem(FXSQLDA);
  {$RANGECHECKS ON}
  inherited Destroy;
end;

{**
   Read blob data to string
   @param Index an filed index
   @param Str destination string
}
procedure TZResultSQLDA.ReadBlobFromString(const Index: Word; var Str: string);
var
  Size: LongInt;
  Buffer: Pointer;
begin
  ReadBlobBufer(FPlainDriver, FHandle, FTransactionHandle, GetQuad(Index),
    Size, Buffer);
  try
    SetLength(Str, Size);
    SetString(Str, PChar(Buffer), Size);
  finally
    FreeMem(Buffer, Size);
  end;
end;

{**
   Read blob data to stream
   @param Index an filed index
   @param Stream destination stream object
}
procedure TZResultSQLDA.ReadBlobFromStream(const Index: Word; Stream: TStream);
var
  Size: LongInt;
  Buffer: Pointer;
begin
  ReadBlobBufer(FPlainDriver, FHandle, FTransactionHandle, GetQuad(Index),
    Size, Buffer);
  try
    Stream.Seek(0, 0);
    Stream.Write(Buffer^, Size);
    Stream.Seek(0, 0);
  finally
    FreeMem(Buffer, Size);
  end;
end;

{**
   Read blob data to variant value
   @param Index an filed index
   @param Value destination variant value
}
procedure TZResultSQLDA.ReadBlobFromVariant(const Index: Word;
  var Value: Variant);
var
  Size: LongInt;
  Buffer: Pointer;
  PData: Pointer;
begin
  ReadBlobBufer(FPlainDriver, FHandle, FTransactionHandle, GetQuad(Index),
    Size, Buffer);
  Value := VarArrayCreate([0, Size-1], varByte);
  PData := VarArrayLock(Value);
  try
    move(Buffer^, PData^, Size);
  finally
    VarArrayUnlock(Value);
    FreeMem(Buffer, Size);
  end;
end;

{**
   Read blob information by it handle such as blob segment size, segments count,
   blob size and type.
   @param PlainDriver
   @param BlobInfo the blob information structure
}
procedure GetBlobInfo(PlainDriver: IZInterbasePlainDriver;
  BlobHandle: TISC_BLOB_HANDLE; var BlobInfo: TIbBlobInfo);
var
  Items: array[0..3] of Char;
  Results: array[0..99] of Char;
  I, ItemLength: Integer;
  Item: Integer;
  StatusVector: TARRAY_ISC_STATUS;
begin
  I := 0;
  Items[0] := Char(isc_info_blob_num_segments);
  Items[1] := Char(isc_info_blob_max_segment);
  Items[2] := Char(isc_info_blob_total_length);
  Items[3] := Char(isc_info_blob_type);

  if integer(PlainDriver.isc_blob_info(@StatusVector, @BlobHandle, 4, @items[0],
    SizeOf(Results), @Results[0])) > 0 then
  CheckInterbase6Error(PlainDriver, StatusVector);

  while (I < SizeOf(Results)) and (Results[I] <> Char(isc_info_end)) do
  begin
    Item := Integer(Results[I]); Inc(I);
    ItemLength := PlainDriver.isc_vax_integer(@results[I], 2); Inc(I, 2);
    case Item of
      isc_info_blob_num_segments:
        BlobInfo.NumSegments := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_max_segment:
        BlobInfo.MaxSegmentSize := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_total_length:
        BlobInfo.TotalSize := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_type:
        BlobInfo.BlobType := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
    end;
    Inc(i, ItemLength);
  end;
end;

{**
   Read blob field data to stream by it ISC_QUAD value
   Note: DefaultBlobSegmentSize constant used for limit segment size reading
   @param Handle the database connection handle
   @param TransactionHandle the transaction handle
   @param BlobId the ISC_QUAD structure
   @param Size the result buffer size
   @param Buffer the pointer to result buffer

   Note: Buffer must be nill. Function self allocate memory for data
    and return it size
}
procedure ReadBlobBufer(PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE;
  BlobId: TISC_QUAD; var Size: Integer; var Buffer: Pointer);
var
  TempBuffer: PChar;
  BlobInfo: TIbBlobInfo;
  BlobSize, CurPos: LongInt;
  BytesRead, SegLen: UShort;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
begin
  BlobHandle := nil;

  { open blob }
  PlainDriver.isc_open_blob2(@StatusVector, Handle,
         TransactionHandle, @BlobHandle, @BlobId, 0 , nil);
  CheckInterbase6Error(PlainDriver, StatusVector);

  { get blob info }
  GetBlobInfo(PlainDriver, BlobHandle, BlobInfo);
  BlobSize := BlobInfo.TotalSize;
  Size := BlobSize;

  { Allocates a blob buffer }
  CurPos := 0;
  SegLen := UShort(DefaultBlobSegmentSize);
  Buffer := AllocMem(BlobSize);
  TempBuffer := Buffer;

  { Copies data to blob buffer }
  try
    while (CurPos < BlobSize) do
    begin
      if (CurPos + SegLen > BlobSize) then
        SegLen := BlobSize - CurPos;
      if not ((PlainDriver.isc_get_segment(@StatusVector, @BlobHandle,
        @BytesRead, SegLen,TempBuffer) = 0) or
        (StatusVector[1] = isc_segment)) then
      CheckInterbase6Error(PlainDriver, StatusVector);
      Inc(TempBuffer, BytesRead);
      Inc(CurPos, BytesRead);
      BytesRead := 0;
    end;
  except
    FreeMemory(Buffer);
  end;

  { close blob handle }
  PlainDriver.isc_close_blob(@StatusVector, @BlobHandle);
  CheckInterbase6Error(PlainDriver, StatusVector);
end;

end.
