{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          SQL Select Statements Parsing classes          }
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

unit ZGenericSelectParser;

interface

uses ZClasses, ZParseIntfs, ZParseCore, ZParseToken;

type

  {** Select parser interface definition. }
  IZSelectParser = interface (IZSequence)
    ['{20309BF2-AFF8-46F4-9F3B-E7A9A4F857B6}']

    function GetQueryOptionsParser: IZParser;
    function GetNonFromClauseParser: IZParser;
    function GetNonFromClauseNameParser: IZParser;
    function GetClauseNameParser: IZParser;
    function GetColumnListParser: IZParser;
    function GetColumnRefParser: IZParser;
    function GetExpressionRefParser: IZParser;
    function GetFromClauseParser: IZParser;
    function GetTableLinkParser: IZParser;
    function GetJoinTypeParser: IZParser;
    function GetJoinClauseParser: IZParser;
    function GetExpressionParser: IZParser;
    function GetFastExpressionParser: IZParser;
    function GetFieldRefParser: IZParser;
    function GetTableRefParser: IZParser;
    function GetCatalogParser: IZParser;
    function GetTableParser: IZParser;
    function GetFieldParser: IZParser;
    function GetAliasParser: IZParser;
  end;

  {**
    Implements a SQL select parser class.

    SELECT [DISTINCT | ALL]
       select_expression,...
       [FROM table_references
         [WHERE where_definition]
         [GROUP BY (unsigned_integer | col_name | formula) [ASC | DESC], ...]
         [HAVING where_definition]
         [ORDER BY (unsigned_integer | col_name | formula) [ASC | DESC] ,...]
         [FOR UPDATE]]

    Select ::= 'SELECT' [QueryOptions] ColumnList [NonFromClause]
            [ [FromClause] [NonFromClause] ]
    QueryOptions ::= 'DISTINCT' | 'ALL'
    NonFromClause ::= ( NonFromClauseName ( Expression | ',' )* )*
    NonFromClauseName ::= 'WHERE' | ( 'GROUP' 'BY' ) | 'HAVING'
            | ( 'ORDER' 'BY' ) | ( 'FOR' 'UPDATE' )
    ClauseName ::= 'FROM' | NonFromClauseName
    ColumnList ::= ColumnRef ( ',' ColumnRef )*
    ColumnRef ::= FieldRef | ExpressionRef
    FieldRef ::= [ [ Catalog '.' ] Table '.' ] Field [ 'AS' Alias ]
    ExpressionRef ::= Expression [ ['AS'] Alias ]
    FromClause ::= 'FROM' TableLink ( ',' TableLink )*
    TableLink ::= TableRef ( JoinType TableRef JoinClause )*
    JoinType ::= ['NATURAL'] [ 'RIGHT' | 'LEFT' ] [ 'INNER' | 'OUTER' ] 'JOIN'
    JoinClause ::= 'ON' Expression
    TableRef ::= [ Catalog '.' ] Table [ ['AS'] Alias ]
    Catalog ::= WORD
    Table ::= WORD
    Field ::= WORD | '*'
    Alias ::= WORD
    Expression ::= ( Expression1 )+
    Expression1 ::= !( ',' | ';' | '(' | ')' | ClauseName ) | Expression2
    Expression2 ::= '(' ( Expression1 | ',' | ClauseName )* ')'
  }
  TZGenericSelectParser = class (TZSequence, IZSelectParser)
  private
    FQueryOptions: IZAlternation;
    FNonFromClause: IZRepetition;
    FNonFromClauseName: IZAlternation;
    FClauseName: IZAlternation;
    FColumnList: IZSequence;
    FColumnRef: IZAlternation;
    FExpressionRef: IZSequence;
    FFromClause: IZSequence;
    FTableLink: IZSequence;
    FJoinType: IZSequence;
    FJoinClause: IZSequence;
    FExpression: IZRepetition;
    FFastExpression: IZRepetition;
    FFieldRef: IZSequence;
    FTableRef: IZSequence;
    FCatalog: IZTerminal;
    FTable: IZTerminal;
    FField: IZAlternation;
    FAlias: IZTerminal;
  public
    constructor Create;

    function GetQueryOptionsParser: IZParser; virtual;
    function GetNonFromClauseParser: IZParser; virtual;
    function GetNonFromClauseNameParser: IZParser; virtual;
    function GetClauseNameParser: IZParser; virtual;
    function GetColumnListParser: IZParser; virtual;
    function GetColumnRefParser: IZParser; virtual;
    function GetExpressionRefParser: IZParser; virtual;
    function GetFromClauseParser: IZParser; virtual;
    function GetTableLinkParser: IZParser; virtual;
    function GetJoinTypeParser: IZParser; virtual;
    function GetJoinClauseParser: IZParser; virtual;
    function GetExpressionParser: IZParser; virtual;
    function GetFastExpressionParser: IZParser; virtual;
    function GetFieldRefParser: IZParser; virtual;
    function GetTableRefParser: IZParser; virtual;
    function GetCatalogParser: IZParser; virtual;
    function GetTableParser: IZParser; virtual;
    function GetFieldParser: IZParser; virtual;
    function GetAliasParser: IZParser; virtual;
  end;

implementation

uses ZSelectSchema, ZGenericSqlToken;

{ TZGenericSelectParser }

{**
  Constructs this parser for the following expression:
  <code>
  Select ::= 'SELECT' [QueryOptions] ColumnList [NonFromClause]
    [ [FromClause] [NonFromClause] ]
  </code>
}
constructor TZGenericSelectParser.Create;
begin
  inherited CreateWithName('Select');

  { Creates a parsers sequence. }
  SubParsers.Add(TZCaselessLiteral.CreateWithValue('', 'SELECT').Discard);
  SubParsers.Add(TZOption.Create(GetQueryOptionsParser));
  SubParsers.Add(GetColumnListParser);
  SubParsers.Add(TZOption.Create(GetNonFromClauseParser));
  SubParsers.Add(TZOption.Create(GetFromClauseParser));
  SubParsers.Add(TZOption.Create(GetNonFromClauseParser));

  { Assignes assemblers to the parsers. }
  GetFieldParser.SetAssembler(TZFieldAssembler.Create);
  GetTableParser.SetAssembler(TZTableAssembler.Create);
  GetCatalogParser.SetAssembler(TZCatalogAssembler.Create);
  GetAliasParser.SetAssembler(TZAliasAssembler.Create);
  GetFieldRefParser.SetAssembler(TZFieldRefAssembler.Create);
  GetTableRefParser.SetAssembler(TZTableRefAssembler.Create);
  GetExpressionRefParser.SetAssembler(TZExpressionRefAssembler.Create);
  Assembler := TZSelectAssembler.Create;
end;

{**
  Gets this parser for the following expression:
  <code>
  Alias ::= WORD
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetAliasParser: IZParser;
begin
  if FAlias = nil then
    FAlias := TZWord.CreateWithName('Alias');
  Result := FAlias;
end;

{**
  Gets this parser for the following expression:
  <code>
  Catalog ::= WORD
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetCatalogParser: IZParser;
begin
  if FCatalog = nil then
    FCatalog := TZWord.CreateWithName('Catalog');
  Result := FCatalog;
end;

{**
  Gets this parser for the following expression:
  <code>
  ClauseName ::= 'FROM' | NonFromClauseName
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetClauseNameParser: IZParser;
begin
  if FClauseName = nil then
  begin
    FClauseName := TZAlternation.CreateWithName('ClauseName');
    FClauseName.Add(TZCaselessLiteral.CreateWithValue('', 'FROM').Discard);
    FClauseName.Add(GetNonFromClauseNameParser);
  end;
  Result := FClauseName;
end;

{**
  Gets this parser for the following expression:
  <code>
  ColumnList ::= ColumnRef ( ',' ColumnRef )*
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetColumnListParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FColumnList = nil then
  begin
    FColumnList := TZSequence.CreateWithName('ColumnList');
    FColumnList.Add(GetColumnRefParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Sequence.Add(GetColumnRefParser);
    FColumnList.Add(TZRepetition.Create(Sequence));
  end;
  Result := FColumnList;
end;

{**
  Gets this parser for the following expression:
  <code>
  ColumnRef ::= FieldRef | ExpressionRef
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetColumnRefParser: IZParser;
begin
  if FColumnRef = nil then
  begin
    FColumnRef := TZAlternation.CreateWithName('ColumnRef');
    FColumnRef.Add(GetFieldRefParser);
    FColumnRef.Add(GetExpressionRefParser);
  end;
  Result := FColumnRef;
end;

{**
  Gets this parser for the following expression:
  <code>
  Expression ::= ( Expression1 )+
  Expression1 ::= !( ',' | ';' | '(' | ')' | ClauseName ) | Expression2
  Expression2 ::= '(' ( Expression1 | ',' | ClauseName )* ')'
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetExpressionParser: IZParser;
var
  Alternation: IZAlternation;
  Expression1: IZAlternation;
  Expression2: IZSequence;
begin
  if FExpression = nil then
  begin
    Expression1 := TZAlternation.CreateWithName('Expression1');
    Expression2 := TZSequence.CreateWithName('Expression2');

    FExpression := TZRepetition.CreateWithName(Expression1, 'Expression');
    FExpression.SetMinCount(1);

    Alternation := TZAlternation.Create;
    Alternation.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', ';').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', '(').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', ')').Discard);
    Alternation.Add(GetClauseNameParser);
    Expression1.Add(TZInvertion.Create(Alternation).Discard);
    Expression1.Add(Expression2);

    Alternation := TZAlternation.Create;
    Alternation.Add(Expression1);
    Alternation.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Alternation.Add(GetClauseNameParser);
    Expression2.Add(TZLiteral.CreateWithValue('', '(').Discard);
    Expression2.Add(TZRepetition.Create(Alternation));
    Expression2.Add(TZLiteral.CreateWithValue('', ')').Discard);

    { Removing a cycle reference to avoid memory leaks. }
    Expression1._Release;
  end;
  Result := FExpression;
end;

{**
  Gets this optimal parser for the following expression:
  <code>
  Expression ::= ( Expression1 )+
  Expression1 ::= !( ',' | ';' | '(' | ')' | ClauseName ) | Expression2
  Expression2 ::= '(' ( Expression1 | ',' | ClauseName )* ')'
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetFastExpressionParser: IZParser;
var
  Alternation: IZAlternation;
  Expression1: IZAlternation;
  Expression2: IZSequence;
begin
  if FFastExpression = nil then
  begin
    Expression1 := TZAlternation.CreateWithName('Expression1');
    Expression2 := TZSequence.CreateWithName('Expression2');

    FFastExpression := TZRepetition.CreateWithName(Expression1, 'Expression');
    FFastExpression.SetMinCount(1);

    Alternation := TZAlternation.Create;
    Alternation.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', ';').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', '(').Discard);
    Alternation.Add(TZLiteral.CreateWithValue('', ')').Discard);
    Alternation.Add(GetClauseNameParser);
    Expression1.Add(TZFastInvertion.Create(Alternation).Discard);
    Expression1.Add(Expression2);

    Alternation := TZAlternation.Create;
    Alternation.Add(Expression1);
    Alternation.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Alternation.Add(GetClauseNameParser);
    Expression2.Add(TZLiteral.CreateWithValue('', '(').Discard);
    Expression2.Add(TZRepetition.Create(Alternation));
    Expression2.Add(TZLiteral.CreateWithValue('', ')').Discard);

    { Removing a cycle reference to avoid memory leaks. }
    Expression1._Release;
  end;
  Result := FFastExpression;
end;

{**
  Gets this parser for the following expression:
  <code>
  ExpressionRef ::= Expression [ 'AS' Alias ]
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetExpressionRefParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FExpressionRef = nil then
  begin
    FExpressionRef := TZSequence.CreateWithName('ExpressionRef');
    FExpressionRef.Add(GetExpressionParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZCaselessLiteral.CreateWithValue('', 'AS').Discard);
    Sequence.Add(GetAliasParser);
    FExpressionRef.Add(TZOption.Create(Sequence));
  end;
  Result := FExpressionRef;
end;

{**
  Gets this parser for the following expression:
  <code>
  Field ::= WORD
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetFieldParser: IZParser;
begin
  if FField = nil then
  begin
    FField := TZAlternation.CreateWithName('Field');
    FField.Add(TZWord.Create);
    FField.Add(TZLiteral.CreateWithValue('', '*'));
  end;
  Result := FField;
end;

{**
  Gets this parser for the following expression:
  <code>
  FieldRef ::= [ [ Catalog '.' ] Table '.' ] Field [ ['AS'] Alias ]
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetFieldRefParser: IZParser;
var
  Sequence: IZSequence;
  Sequence1: IZSequence;
begin
  if FFieldRef = nil then
  begin
    FFieldRef := TZSequence.CreateWithName('FieldRef');

    Sequence1 := TZSequence.Create;
    Sequence1.Add(GetCatalogParser);
    Sequence1.Add(TZLiteral.CreateWithValue('', '.').Discard);

    Sequence := TZSequence.Create;
    Sequence.Add(TZOption.Create(Sequence1));
    Sequence.Add(GetTableParser);
    Sequence.Add(TZLiteral.CreateWithValue('', '.').Discard);
    FFieldRef.Add(TZOption.Create(Sequence));
    FFieldRef.Add(GetFieldParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZOption.Create(TZCaselessLiteral.
      CreateWithValue('', 'AS').Discard));
    Sequence.Add(GetAliasParser);
    FFieldRef.Add(TZOption.Create(Sequence));
  end;
  Result := FFieldRef;
end;

{**
  Gets this parser for the following expression:
  <code>
  FromClause ::= 'FROM' TableLink ( ',' TableLink )*
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetFromClauseParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FFromClause = nil then
  begin
    FFromClause := TZSequence.CreateWithName('FromClause');
    FFromClause.Add(TZCaselessLiteral.CreateWithValue('', 'FROM').Discard);
    FFromClause.Add(GetTableLinkParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Sequence.Add(GetTableLinkParser);
    FFromClause.Add(TZRepetition.Create(Sequence));
  end;
  Result := FFromClause;
end;

{**
  Gets this parser for the following expression:
  <code>
  JoinClause ::= 'ON' Expression
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetJoinClauseParser: IZParser;
begin
  if FJoinClause = nil then
  begin
    FJoinClause := TZSequence.CreateWithName('JoinClause');
    FJoinClause.Add(TZCaselessLiteral.CreateWithValue('', 'ON').Discard);
    FJoinClause.Add(GetExpressionParser);
  end;
  Result := FJoinClause;
end;

{**
  Gets this parser for the following expression:
  <code>
  JoinType ::= ['NATURAL'] [ 'RIGHT' | 'LEFT' ] [ 'INNER' | 'OUTER' ] 'JOIN'
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetJoinTypeParser: IZParser;
var
  Alternation: IZAlternation;
begin
  if FJoinType = nil then
  begin
    FJoinType := TZSequence.CreateWithName('JoinType');
    FJoinType.Add(TZOption.Create(TZCaselessLiteral.
      CreateWithValue('', 'NATURAL').Discard));

    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'RIGHT').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'LEFT').Discard);
    FJoinType.Add(TZOption.Create(Alternation));

    Alternation := TZAlternation.Create;
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'INNER').Discard);
    Alternation.Add(TZCaselessLiteral.CreateWithValue('', 'OUTER').Discard);
    FJoinType.Add(TZOption.Create(Alternation));

    FJoinType.Add(TZCaselessLiteral.CreateWithValue('', 'JOIN').Discard);
  end;
  Result := FJoinType;
end;

{**
  Gets this parser for the following expression:
  <code>
  NonFromClauseName ::= 'WHERE' | ( 'GROUP' 'BY' ) | 'HAVING'
    | ( 'ORDER' 'BY' ) | ( 'FOR' 'UPDATE' )
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetNonFromClauseNameParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FNonFromClauseName = nil then
  begin
    FNonFromClauseName := TZAlternation.CreateWithName('NonFromClauseName');
    FNonFromClauseName.Add(TZCaselessLiteral.
      CreateWithValue('', 'WHERE').Discard);

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
  end;
  Result := FNonFromClauseName;
end;

{**
  Gets this parser for the following expression:
  <code>
  NonFromClause ::= ( NonFromClauseName ( Expression | ',' )* )*
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetNonFromClauseParser: IZParser;
var
  Alternation: IZAlternation;
  Sequence: IZSequence;
begin
  if FNonFromClause = nil then
  begin
    Sequence := TZSequence.Create;
    Sequence.Add(GetNonFromClauseNameParser);

    Alternation := TZAlternation.Create;
    Alternation.Add(GetFastExpressionParser);
    Alternation.Add(TZLiteral.CreateWithValue('', ',').Discard);
    Sequence.Add(TZRepetition.Create(Alternation));

    FNonFromClause := TZRepetition.CreateWithName(Sequence, 'NonFromClause');
  end;
  Result := FNonFromClause;
end;

{**
  Gets this parser for the following expression:
  <code>
  QueryOptions ::= 'DISTINCT' | 'ALL'
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetQueryOptionsParser: IZParser;
begin
  if FQueryOptions = nil then
  begin
    FQueryOptions := TZAlternation.CreateWithName('QueryOptions');
    FQueryOptions.Add(TZCaselessLiteral.CreateWithValue('', 'DISTINCT').Discard);
    FQueryOptions.Add(TZCaselessLiteral.CreateWithValue('', 'ALL').Discard);
  end;
  Result := FQueryOptions;
end;

{**
  Gets this parser for the following expression:
  <code>
  TableLink ::= TableRef ( JoinType TableRef JoinClause )*
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetTableLinkParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FTableLink = nil then
  begin
    FTableLink := TZSequence.CreateWithName('TableLink');
    FTableLink.Add(GetTableRefParser);

    Sequence := TZSequence.Create;
    Sequence.Add(GetJoinTypeParser);
    Sequence.Add(GetTableRefParser);
    Sequence.Add(GetJoinClauseParser);
    FTableLink.Add(TZRepetition.Create(Sequence));
  end;
  Result := FTableLink;
end;

{**
  Gets this parser for the following expression:
  <code>
  Table ::= WORD
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetTableParser: IZParser;
begin
  if FTable = nil then
    FTable := TZWord.CreateWithName('Table');
  Result := FTable;
end;

{**
  Gets this parser for the following expression:
  <code>
  TableRef ::= [ Catalog '.' ] Table [ ['AS'] Alias ]
  </code>

  @return the constructed parser object.
}
function TZGenericSelectParser.GetTableRefParser: IZParser;
var
  Sequence: IZSequence;
begin
  if FTableRef = nil then
  begin
    FTableRef := TZSequence.CreateWithName('TableRef');

    Sequence := TZSequence.Create;
    Sequence.Add(GetCatalogParser);
    Sequence.Add(TZLiteral.CreateWithValue('', '.').Discard);
    FTableRef.Add(TZOption.Create(Sequence));
    FTableRef.Add(GetTableParser);

    Sequence := TZSequence.Create;
    Sequence.Add(TZOption.Create(TZCaselessLiteral.
      CreateWithValue('', 'AS').Discard));
    Sequence.Add(GetAliasParser);
    FTableRef.Add(TZOption.Create(Sequence));
  end;
  Result := FTableRef;
end;

end.

