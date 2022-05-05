{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        PostgreSQL Select Statements Parsing classes     }
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

unit ZPostgreSqlSelectParser;

interface

uses ZParseIntfs, ZParseCore, ZParseToken, ZGenericSelectParser;

type

  {**
    Implements a PostgreSQL select parser class.

    SELECT [DISTINCT ALL]
    select_expression,...
    [FROM table_references
        [WHERE where_definition]
        [GROUP BY (unsigned_integer | col_name | formula) [ASC | DESC], ...]
        [HAVING where_definition]
        [ORDER BY (unsigned_integer | col_name | formula) [ASC | DESC] ,...]
        [LIMIT rows]
        [OFFSET offset]
        [FOR UPDATE]]

    Select ::= 'SELECT' [QueryOptions] ColumnList [NonFromClause]
            [ [FromClause] [NonFromClause] ]
    QueryOptions ::= 'DISTINCT' | 'ALL'
    NonFromClause ::= ( NonFromClauseName ( Expression | ',' )* )*
    NonFromClauseName ::= 'WHERE' | 'INTO' | ( 'GROUP' 'BY' ) | 'HAVING'
            | ( 'ORDER' 'BY' ) | ( 'FOR' 'UPDATE' ) | 'LIMIT' | 'OFFSET'
    ClauseName ::= 'FROM' | NonFromClauseName
    ColumnList ::= ColumnRef ( ',' ColumnRef )*
    ColumnRef ::= FieldRef | ExpressionRef
    FieldRef ::= [ [ Catalog '.' ] Table '.' ] Field [ ['AS'] Alias ]
    ExpressionRef ::= Expression [ ['AS'] Alias ]
    FromClause ::= 'FROM' TableLink ( ',' TableLink )*
    TableLink ::= TableRef ( JoinType TableRef JoinClause )*
    JoinType ::= [ 'CROSS' | ( ['NATURAL'] ['RIGHT' | 'LEFT' | 'FULL']
            ['INNER' | 'OUTER'] ) ] 'JOIN'
    JoinClause ::= [ ( 'ON' | 'USING' ) Expression ]
    TableRef ::= [ Catalog '.' ] Table [ ['AS'] Alias ]
    Catalog ::= WORD
    Table ::= WORD
    Field ::= WORD | '*'
    Alias ::= WORD
    Expression ::= ( Expression1 )+
    Expression1 ::= !( ',' | ';' | '(' | ')' | ClauseName ) | Expression2
    Expression2 ::= '(' ( Expression1 | ',' | ClauseName )* ')'
  }
  TZPostgreSQLSelectParser = class (TZGenericSelectParser, IZSelectParser)
  private
    FQueryOptions: IZAlternation;
    FNonFromClauseName: IZAlternation;
    FExpressionRef: IZSequence;
    FJoinType: IZSequence;
    FJoinClause: IZAlternation;
  public
    function GetQueryOptionsParser: IZParser; override;
    function GetNonFromClauseNameParser: IZParser; override;
    function GetExpressionRefParser: IZParser; override;
    function GetJoinTypeParser: IZParser; override;
    function GetJoinClauseParser: IZParser; override;
  public
    constructor Create;
  end;

implementation

uses ZSelectSchema, ZPostgreSqlToken;

{ TZPostgreSQLSelectParser }

{**
  Constructs this parser class.
}
constructor TZPostgreSQLSelectParser.Create;
begin
  inherited Create;
end;

{**
  Gets this parser for the following expression:
  <code>
  ExpressionRef ::= Expression [ ['AS'] Alias ]
  </code>

  @return the constructed parser object.
}
function TZPostgreSQLSelectParser.GetExpressionRefParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FExpressionRef = nil then
  begin
    FExpressionRef := TZSequence.CreateWithName('ExpressionRef');
    FExpressionRef.Add(GetExpressionParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZOption.Create(TZCaselessLiteral.
      CreateWithValue('', 'AS').Discard));
    Sequence.Add(GetAliasParser);
    FExpressionRef.Add(TZOption.Create(Sequence));
  end;
  Result := FExpressionRef;
end;

{**
  Gets this parser for the following expression:
  <code>
  JoinClause ::= [ ( 'ON' | 'USING' ) Expression ]
  </code>

  @return the constructed parser object.
}
function TZPostgreSQLSelectParser.GetJoinClauseParser: IZParser;
var
  Alternation: IZAlternation;
  Sequence: IZSequence;
begin
  if FJoinClause = nil then
  begin
    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'ON').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'USING').Discard);

    Sequence := TZSequence.Create;
    Sequence.Add(Alternation);
    Sequence.Add(GetExpressionParser);

    FJoinClause := TZOption.CreateWithName(Sequence, 'JoinClause');
  end;
  Result := FJoinClause;
end;

{**
  Gets this parser for the following expression:
  <code>
  JoinType ::= [ 'CROSS' | ( ['NATURAL'] ['RIGHT' | 'LEFT' | 'FULL']
    ['INNER' | 'OUTER'] ) ] 'JOIN'
  </code>

  @return the constructed parser object.
}
function TZPostgreSQLSelectParser.GetJoinTypeParser: IZParser;
var
  Alternation: IZAlternation;
  Sequence: IZSequence;
begin
  if FJoinType = nil then
  begin
    FJoinType := TZSequence.CreateWithName('JoinType');

    Sequence := TZSequence.Create;
    Sequence.Add(TZOption.Create(TZCaselessLiteral.
      CreateWithValue('', 'NATURAL').Discard));

    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'RIGHT').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'LEFT').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'FULL').Discard);
    Sequence.Add(TZOption.Create(Alternation));

    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'INNER').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'OUTER').Discard);
    Sequence.Add(TZOption.Create(Alternation));

    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'CROSS').Discard);
    Alternation.Add(Sequence);

    FJoinType.Add(TZOption.Create(Alternation));
    FJoinType.Add(TZCaselessLiteral.CreateWithValue('', 'JOIN').Discard);
  end;
  Result := FJoinType;
end;

{**
  Gets this parser for the following expression:
  <code>
    NonFromClauseName ::= 'WHERE' | 'INTO' | ( 'GROUP' 'BY' ) | 'HAVING'
      | ( 'ORDER' 'BY' ) | ( 'FOR' 'UPDATE' ) | 'LIMIT' | 'OFFSET'
  </code>

  @return the constructed parser object.
}
function TZPostgreSQLSelectParser.GetNonFromClauseNameParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FNonFromClauseName = nil then
  begin
    FNonFromClauseName := TZAlternation.CreateWithName('NonFromClauseName');
    FNonFromClauseName.Add(TZCaselessLiteral.CreateWithValue('', 'WHERE').Discard);
    FNonFromClauseName.Add(TZCaselessLiteral.CreateWithValue('', 'INTO').Discard);

    Sequence := TZSequence.Create;
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'GROUP').Discard);
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'BY').Discard);
    FNonFromClauseName.Add(Sequence);

    FNonFromClauseName.Add(TZCaselessLiteral.
      CreateWithValue('', 'HAVING').Discard);

    Sequence := TZSequence.Create;
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'ORDER').Discard);
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'BY').Discard);
    FNonFromClauseName.Add(Sequence);

    Sequence := TZSequence.Create;
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'FOR').Discard);
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'UPDATE').Discard);
    FNonFromClauseName.Add(Sequence);

    FNonFromClauseName.Add(TZCaselessLiteral.
      CreateWithValue('', 'LIMIT').Discard);
    FNonFromClauseName.Add(TZCaselessLiteral.
      CreateWithValue('', 'OFFSET').Discard);
  end;
  Result := FNonFromClauseName;
end;

{**
  Gets this parser for the following expression:
  <code>
  QueryOptions ::= 'DISTINCT' | 'ALL'
  </code>

  @return the constructed parser object.
}
function TZPostgreSQLSelectParser.GetQueryOptionsParser: IZParser;
begin
  if FQueryOptions = nil then
  begin
    FQueryOptions := TZAlternation.CreateWithName('QueryOptions');
    FQueryOptions.Add(TZCaselessLiteral.CreateWithValue('', 'DISTINCT').Discard);
    FQueryOptions.Add(TZCaselessLiteral.CreateWithValue('', 'ALL').Discard);
  end;
  Result := FQueryOptions;
end;

end.

