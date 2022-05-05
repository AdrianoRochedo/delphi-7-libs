{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               SQL Query Strings component               }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
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

unit ZSqlStrings;

interface

{$I ZComponent.inc}

uses
  Classes, SysUtils, DB, ZSysUtils, ZDbcIntfs, ZParseIntfs,
  ZGenericSqlToken, ZCompatibility;

type
  {** Represents a SQL statement description object. }
  TZSQLStatement = class (TObject)
  private
    FSQL: string;
    FParamIndices: TIntegerDynArray;
    FParams: TStrings;

    function GetParamCount: Integer;
    function GetParamName(Index: Integer): string;
    function GetParamNamesArray: TStringDynArray;
  protected
    constructor Create(SQL: string; ParamIndices: TIntegerDynArray;
      Params: TStrings);
  public
    property SQL: string read FSQL;
    property ParamCount: Integer read GetParamCount;
    property ParamNames[Index: Integer]: string read GetParamName;
    property ParamIndices: TIntegerDynArray read FParamIndices;
    property ParamNamesArray: TStringDynArray read GetParamNamesArray;
  end;

  {** Imlements a string list with SQL statements. }
  TZSQLStrings = class (TStringList)
  private
    FTokenizer: IZTokenizer;
    FParamCheck: Boolean;
    FStatements: TList;
    FParams: TStringList;

    function GetParamCount: Integer;
    function GetParamName(Index: Integer): string;
    function GetStatement(Index: Integer): TZSQLStatement;
    function GetStatementCount: Integer;
    procedure SetTokenizer(Value: IZTokenizer);
    procedure SetParamCheck(Value: Boolean);
  protected
    procedure Changed; override;
    function FindParam(ParamName: string): Integer;
    procedure RebuildAll;
  public
    constructor Create(Tokenizer: IZTokenizer);
    destructor Destroy; override;

    property Tokenizer: IZTokenizer read FTokenizer write SetTokenizer;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck;
    property ParamCount: Integer read GetParamCount;
    property ParamNames[Index: Integer]: string read GetParamName;
    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: TZSQLStatement read GetStatement;
  end;

implementation

{ TZSQLStatement }

{**
  Creates a SQL statement object and assignes the main properties.
  @param SQL a SQL statement.
  @param ParamIndices a parameter indices.
  @param Params a list with all parameter names.
}
constructor TZSQLStatement.Create(SQL: string;
  ParamIndices: TIntegerDynArray; Params: TStrings);
begin
  FSQL := SQL;
  FParamIndices := ParamIndices;
  FParams := Params;
end;

{**
  Gets a parameters count for this statement.
  @return a parameters count.
}
function TZSQLStatement.GetParamCount: Integer;
begin
  if Assigned(FParamIndices) then
    Result := High(FParamIndices) - Low(FParamIndices) + 1
  else Result := 0;
end;

{**
  Gets a parameter name by it's index inside the statement.
  @return a parameter name.
}
function TZSQLStatement.GetParamName(Index: Integer): string;
begin
  if Assigned(FParamIndices) then
    Result := FParams[FParamIndices[Index + Low(FParamIndices)]]
  else Result := '';
end;

{**
  Gets an array of parameter names.
  @return an array of parameter names.
}
function TZSQLStatement.GetParamNamesArray: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, High(FParamIndices) - Low(FParamIndices) + 1);
  for I := Low(Result) to High(Result) do
    Result[I] := FParams[FParamIndices[I + Low(FParamIndices)]];
end;

{ TZSQLStrings }

{**
  Creates a SQL strings object and assigns the main properties.
  @param Tokenizer a string tokenizer for specific SQL.
}
constructor TZSQLStrings.Create(Tokenizer: IZTokenizer);
begin
  FParams := TStringList.Create;
  FParamCheck := True;
  FTokenizer := Tokenizer;
  FStatements := TList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLStrings.Destroy;
begin
  FTokenizer := nil;
  FParams.Free;
  ClearObjectList(FStatements);
  FStatements.Free;
  inherited Destroy;
end;

{**
  Gets a parameter count.
  @return a count of SQL parameters.
}
function TZSQLStrings.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{**
  Gets parameter name by it's index.
  @param Index a parameter index.
  @return a parameter name.
}
function TZSQLStrings.GetParamName(Index: Integer): string;
begin
  Result := FParams[Index];
end;

{**
  Gets a SQL statements count.
  @return a SQL statements count.
}
function TZSQLStrings.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

{**
  Gets a SQL statement by it's index.
  @param Index a SQL statement index.
  @return a SQL statement object.
}
function TZSQLStrings.GetStatement(Index: Integer): TZSQLStatement;
begin
  Result := TZSQLStatement(FStatements[Index]);
end;

{**
  Sets a new ParamCheck value.
  @param Value a new ParamCheck value.
}
procedure TZSQLStrings.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then
  begin
    FParamCheck := Value;
//    RebuildAll;
  end;
end;

{**
  Sets a new tokenizer object.
  @param Value a new tokenizer object.
}
procedure TZSQLStrings.SetTokenizer(Value: IZTokenizer);
begin
  if FTokenizer <> Value then
  begin
    FTokenizer := Value;
//    RebuildAll;
  end;
end;

{**
  Finds a parameter by it's name.
  @param ParamName a parameter name.
  @return an index of found parameters or -1 if nothing was found.
}
function TZSQLStrings.FindParam(ParamName: string): Integer;
begin
{$IFNDEF VER130BELOW}
  FParams.CaseSensitive := False;
{$ENDIF}  
  Result := FParams.IndexOf(ParamName);
end;

{**
  Rebuilds all SQL statements.
}
procedure TZSQLStrings.RebuildAll;
var
  Token: IZToken;
  Statement: TZSQLStatement;
  ParamIndex: Integer;
  ParamIndices: TIntegerDynArray;
  ParamIndexCount: Integer;
  ParamName, SQL: string;
begin
  FParams.Clear;
  ClearObjectList(FStatements);
  SQL := '';
  ParamIndexCount := 0;
  SetLength(ParamIndices, ParamIndexCount);

  { Optimization for single query without parameters. }
  if (not FParamCheck or (Pos(';', Text) = 0)) and (Pos(':', Text) = 0) then
  begin
    Statement := TZSQLStatement.Create(Text, ParamIndices, FParams);
    FStatements.Add(Statement);
    Exit;
  end;

  FTokenizer.SetBuffer(Text);
  FTokenizer.DefaultWhitespaceState.SetVisible(True);
  FTokenizer.DefaultCommentState.SetVisible(True);

  repeat
    Token := Tokenizer.NextToken;

    { Processes parameters. }
    if ParamCheck and (Token.GetString = ':') then
    begin
      Token := Tokenizer.NextToken;
      if (Token.GetTokenType <> TT_EOF) and (Token.GetString <> ':') then
      begin
        { Check for correct parameter type. }
        if (Token.GetTokenType <> TT_WORD)
          and (Token.GetTokenType <> TT_QUOTED) then
        begin
          DatabaseError('Incorrect token followed by ":".');
        end;

        ParamName := Token.GetString;
        { Removes quotes for quoted parameter names. }
        if ParamName[1] in ['''', '"'] then
          ParamName := Copy(ParamName, 2, Length(ParamName) - 2);

        SQL := SQL + '?';

        ParamIndex := FindParam(ParamName);
        if ParamIndex < 0 then
          ParamIndex := FParams.Add(ParamName);

        Inc(ParamIndexCount);
        SetLength(ParamIndices, ParamIndexCount);
        ParamIndices[ParamIndexCount - 1] := ParamIndex;

        Continue;
      end;
    end;

    { Adds a DML statement. }
    if ((Token.GetTokenType = TT_EOF) or (Token.GetString = ';'))
      and (Trim(SQL) <> '') then
    begin
      Statement := TZSQLStatement.Create(SQL, ParamIndices, FParams);
      FStatements.Add(Statement);

      SQL := '';
      ParamIndexCount := 0;
      SetLength(ParamIndices, ParamIndexCount);
    end
    { Adds a whitespace token. }
    else if Token.GetTokenType = TT_WHITESPACE then
    begin
      if SQL <> '' then
        SQL := SQL + ' ';
    end
    { Adds a default token. }
    else
    begin
      SQL := SQL + Token.GetString;
    end;
  until Token.GetTokenType = TT_EOF;
end;

{**
  Performs action when the content of this string list is changed.
}
procedure TZSQLStrings.Changed;
begin
  RebuildAll;
  inherited Changed;
end;

end.




