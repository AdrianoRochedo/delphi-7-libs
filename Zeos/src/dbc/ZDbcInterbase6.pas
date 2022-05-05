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

unit ZDbcInterbase6;

interface

uses ZCompatibility, Classes, SysUtils, ZDbcUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainInterbaseDriver, ZDbcInterbase6Metadata,
  ZSysUtils, ZDbcInterbase6Utils;

type

  {** Implements Interbase6 Database Driver. }
  TZInterbase6Driver = class(TZAbstractDriver)
  private
    FInterbase6PlainDriver: IZInterbase6PlainDriver;
    FInterbase5PlainDriver: IZInterbase5PlainDriver;
    FFirebird10PlainDriver: IZFirebird10PlainDriver;
    FFirebird15PlainDriver: IZFirebird15PlainDriver;
  protected
    function GetPlainDriver(Url: string): IZInterbasePlainDriver;
  public
    constructor Create;
    function Connect(Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
  end;

  {** Represents a Interbase specific connection interface. }
  IZInterbase6Connection = interface (IZConnection)
    ['{E870E4FE-21EB-4725-B5D8-38B8A2B12D0B}']
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetPlainDriver: IZInterbasePlainDriver;
  end;

  {** Implements Interbase6 Database Connection. }
  TZInterbase6Connection = class(TZAbstractConnection, IZInterbase6Connection)
  private
    FDialect: Word;
    FHandle: TISC_DB_HANDLE;
    FTrHandle: TISC_TR_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FPlainDriver: IZInterbasePlainDriver;
  private
    procedure StartTransaction; virtual;
  public
    constructor Create(PlainDriver: IZInterbasePlainDriver;
      HostName: string; Port: Integer; Database: string;
      User: string; Password: string; Info: TStrings);
    destructor Destroy; override;

    function GetDBHandle: PISC_DB_HANDLE;
    function GetTrHandle: PISC_TR_HANDLE;
    function GetDialect: Word;
    function GetPlainDriver: IZInterbasePlainDriver;

    function CreateStatement: IZStatement; override;
    function PrepareStatement(Sql: string): IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    function GetMetadata: IZDatabaseMetadata; override;
  end;

var
  {** The common driver manager object. }
  Interbase6Driver: IZDriver;

implementation

uses ZDbcInterbase6Statement;

{ TZInterbase6Driver }

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZInterbase6Driver.Connect(Url: string;
  Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZInterbasePlainDriver;
begin
 TempInfo := TStringList.Create;
 try
   ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
   PlainDriver := GetPlainDriver(Url);
   Result := TZInterbase6Connection.Create(PlainDriver, HostName, Port, Database, UserName,
      Password, TempInfo);
 finally
   TempInfo.Free;
 end;
end;

{**
  Constructs this object with default properties.
}
constructor TZInterbase6Driver.Create;
begin
  FInterbase6PlainDriver := TZInterbase6PlainDriver.Create;
  FInterbase5PlainDriver := TZInterbase5PlainDriver.Create;
  FFirebird10PlainDriver := TZFirebird10PlainDriver.Create;
  FFirebird15PlainDriver := TZFirebird15PlainDriver.Create;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZInterbase6Driver.GetMajorVersion: Integer;
begin
 Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZInterbase6Driver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZInterbase6Driver.GetPlainDriver(
  Url: string): IZInterbasePlainDriver;
var
  Protocol: string;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);

  if Protocol = FInterbase5PlainDriver.GetProtocol then
    Result := FInterbase5PlainDriver
  else if Protocol = FInterbase6PlainDriver.GetProtocol then
    Result := FInterbase6PlainDriver
  else if Protocol = FFirebird10PlainDriver.GetProtocol then
    Result := FFirebird10PlainDriver
  else if Protocol = FFirebird15PlainDriver.GetProtocol then
    Result := FFirebird15PlainDriver
  else Result := FInterbase6PlainDriver;
  Result.Initialize;
end;

{**
  Get a name of the supported subprotocol.
  For example: mysql, oracle8 or postgresql72
}
function TZInterbase6Driver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 4);
  Result[0] := 'interbase-5';
  Result[1] := 'interbase-6';
  Result[2] := 'firebird-1.0';
  Result[3] := 'firebird-1.5';
end;

{ TZInterbase6Connection }

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZInterbase6Connection.Close;
begin
  if Closed then
    Exit;

  if FTrHandle <> nil then
  begin
    if AutoCommit then
      FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle)
    else
      FPlainDriver.isc_rollback_transaction(@FStatusVector, @FTrHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
  end;

  if FHandle <> nil then
  begin
    FPlainDriver.isc_detach_database(@FStatusVector, @FHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
  end;

  FHandle := nil;
  FTrHandle := nil;

  inherited Close;
end;

{**
   Commit current transaction
}
procedure TZInterbase6Connection.Commit;
begin
  if Closed then Exit;

  if FTrHandle <> nil then
  begin
    FPlainDriver.isc_commit_retaining(@FStatusVector, @FTrHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
  end;
end;

{**
  Constructs this object and assignes the main properties.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZInterbase6Connection.Create(PlainDriver: IZInterbasePlainDriver;
  HostName: string; Port: Integer;
  Database, User, Password: string; Info: TStrings);
begin
  inherited Create(HostName, Port, Database, User, Password, Info);

  FPlainDriver := PlainDriver;

  { Sets a default PostgreSQL port }
  if Self.Port = 0 then
    Self.Port := 3050;

  { set default sql dialect it can be overriden }
  if FPlainDriver.GetProtocol = 'interbase-5' then
    FDialect := 1
  else
    FDialect := 3;
    
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @return a new Statement object
}
function TZInterbase6Connection.CreateStatement: IZStatement;
begin
  if IsClosed then Open;
  Result := TZInterbase6Statement.Create(Self);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZInterbase6Connection.Destroy;
begin
  if not Closed then
    Close;

  inherited Destroy;
end;

{**
   Get database connection handle.
   @return database handle
}
function TZInterbase6Connection.GetDBHandle: PISC_DB_HANDLE;
begin
  Result := @FHandle;
end;

{**
   Return Interbase dialect number. Dialect a dialect Interbase SQL
   must be 1 or 2 or 3.
   @return dialect number
}
function TZInterbase6Connection.GetDialect: Word;
begin
  Result := FDialect;
end;

{**
  Gets the metadata regarding this connection's database.
  A Connection's database is able to provide information
  describing its tables, its supported SQL grammar, its stored
  procedures, the capabilities of this connection, and so on. This
  information is made available through a DatabaseMetaData
  object.

  @return a DatabaseMetaData object for this Connection
}
function TZInterbase6Connection.GetMetadata: IZDatabaseMetadata;
var
  Url: string;
begin
  Url := Format('zdbc:interbase-6://%s:%d/%s?UID=%s;PWD=%s;%s',
    [Hostname, Port, Database, User, Password, ComposeString(Info, ';')]);
  Result := TZInterbase6DatabaseMetadata.Create(Self, Url, Info);
end;

{**
   Return native interbase plain driver
   @return plain driver
}
function TZInterbase6Connection.GetPlainDriver: IZInterbasePlainDriver;
begin
  Result := FPlainDriver;
end;

{**
   Get Interbase transaction handle
   @return transaction handle
}
function TZInterbase6Connection.GetTrHandle: PISC_TR_HANDLE;
begin
  Result := @FTrHandle;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZInterbase6Connection.Open;
var
  DPB: PChar;
  FDPBLength: Word;
  DBName: array[0..512] of Char;
begin
  if not Closed then Exit;

  DPB := GenerateDPB(Info, FDPBLength, FDialect);

  if HostName <> '' then
    StrPCopy(DBName, HostName + ':' + Database)
  else StrPCopy(DBName, Database);

  try
    { Connect to Interbase6 database.}
    FHandle := nil;
    FPlainDriver.isc_attach_database(@FStatusVector, StrLen(DBName), DBName,
        @FHandle, FDPBLength, DPB);

    { Check connection error }
    CheckInterbase6Error(FPlainDriver, FStatusVector);

    {Start transaction}
    StartTransaction;

    inherited Open;
  finally
    StrDispose(DPB);
  end;
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZInterbase6Connection.PrepareStatement(
  Sql: string): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZInterbase6PreparedStatement.Create(self, Sql);
end;

{**
   Conver parameters list to Interbase6 parameter index and values
   and sore it in the list.
   <P><B>Note:</B>
   Parameter value sored in list as value.
   Interbase6 parameter index store as object link.
   @see #GenerateDPB
   @see #GenerateTPB
   @param the list of Interbase6 prepared parameters
}

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZInterbase6Connection.Rollback;
begin
  if FTrHandle <> nil then
  begin
    FPlainDriver.isc_rollback_retaining(@FStatusVector, @FTrHandle);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
  end;
end;

procedure TZInterbase6Connection.StartTransaction;
var
  Params: TStrings;
  PTEB: PISC_TEB;
begin
  PTEB := nil;
  Params := TStringList.Create;

  { Set transaction parameters by TransactIsolationLevel }
  Params.Add('isc_tpb_version3');
  if Params.IndexOf('isc_tpb_wait') < 0 then
    Params.Add('isc_tpb_nowait');
  case TransactIsolationLevel of
    tiReadCommitted:
      begin
        Params.Add('isc_tpb_read_committed');
        Params.Add('isc_tpb_no_rec_version');
      end;
    tiRepeatableRead:
      begin
        Params.Add('isc_tpb_concurrency');
      end;
    tiSerializable:
      begin
        Params.Add('isc_tpb_consistency');
      end;
  else
    begin
      { Add user defined parameters for traansaction }
      Params.Clear;
      Params.AddStrings(Info);
    end;
  end;

  try
    { GenerateTPB return PTEB with null pointer tpb_address from defaul
      transaction }
    PTEB := GenerateTPB(Params, FHandle);
    FPlainDriver.isc_start_multiple(@FStatusVector, @FTrHandle, 1, PTEB);
    CheckInterbase6Error(FPlainDriver, FStatusVector);
  finally
    Params.Free;
    StrDispose(PTEB.tpb_address);
    FreeMem(PTEB);
  end
end;

initialization
  Interbase6Driver := TZInterbase6Driver.Create;
  DriverManager.RegisterDriver(Interbase6Driver);

finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(Interbase6Driver);
  Interbase6Driver := nil;
end.
