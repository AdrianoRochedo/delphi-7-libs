{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            SQL Statements Analysing classes             }
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

unit ZGenericSqlAnalyser;

interface

uses Classes, ZClasses, ZParseIntfs, ZTokenizer, ZGenericSqlToken,
  ZSelectSchema;

type

  {** Defines a section of the parsed SQL statement. }
  IZStatementSection = interface(IZClonnable)
    ['{BE1CF3E3-3410-4DDF-A789-8689D9F64C55}']
    function GetName: string;
    function GetTokens: IZCollection;
    procedure SetTokens(Value: IZCollection);
  end;

  {** Implements a section of the parsed SQL statement. }
  TZStatementSection = class (TZAbstractObject, IZStatementSection, IZClonnable)
  private
    FName: string;
    FTokens: IZCollection;
  public
    constructor Create(Name: string; Tokens: IZCollection);

    function GetName: string;
    function GetTokens: IZCollection;
    procedure SetTokens(Value: IZCollection);

    function Clone: IZInterface; override;
  end;

  {** Implements a publicly available interface to statement analyser. }
  IZStatementAnalyser = interface(IZInterface)
    ['{967635B6-411B-4DEF-990C-9C6C01F3DC0A}']

    function SplitTokens(Tokenizer: IZTokenizer; Buffer: string;
      Cleanup: Boolean): IZCollection;
    function SplitSections(Tokens: IZCollection): IZCollection;

    function ComposeTokens(Tokens: IZCollection): string;
    function ComposeSections(Sections: IZCollection): string;

    function ExtractSelectSchemaFromSections(Sections: IZCollection):
      IZSelectSchema;
    function ExtractSelectSchemaFromBuffer(Tokenizer: IZTokenizer;
      Buffer: string): IZSelectSchema;

    function ReplaceIsNull(Tokens: IZCollection): IZCollection;
  end;

  {** Implements an SQL statements analyser. }
  TZGenericStatementAnalyser = class (TZAbstractObject, IZStatementAnalyser)
  private
    FSectionNames: TStrings;
    FSelectOptions: TStrings;
    FFromJoins: TStrings;
    FFromClauses: TStrings;
  protected
    function ArrayToStrings(Value: array of string): TStrings;
    function FindKeyword(Tokens: IZCollection; TokenIndex: Integer;
      Keywords: TStrings; var Keyword: string; var WordCount: Integer): Boolean;
    function FindSection(Sections: IZCollection; Name: string): IZCollection;

    function TokensToStrings(Tokens: IZCollection): TStrings;

    function ExtractFieldRefs(SelectTokens: IZCollection): IZCollection;
    function ExtractTableRefs(FromTokens: IZCollection): IZCollection;

    function SkipOptionTokens(Tokens: IZCollection; var TokenIndex: Integer;
      Options: TStrings): Boolean;
    function SkipBracketTokens(Tokens: IZCollection; var TokenIndex: Integer):
      Boolean;

    property SectionNames: TStrings read FSectionNames write FSectionNames;
    property SelectOptions: TStrings read FSelectOptions write FSelectOptions;
    property FromJoins: TStrings read FFromJoins write FFromJoins;
    property FromClauses: TStrings read FFromClauses write FFromClauses;
  public
    constructor Create;
    destructor Destroy; override;

    function SplitTokens(Tokenizer: IZTokenizer; Buffer: string;
      Cleanup: Boolean): IZCollection;
    function SplitSections(Tokens: IZCollection): IZCollection;

    function ComposeTokens(Tokens: IZCollection): string;
    function ComposeSections(Sections: IZCollection): string;

    function ExtractSelectSchemaFromSections(Sections: IZCollection):
      IZSelectSchema;
    function ExtractSelectSchemaFromBuffer(Tokenizer: IZTokenizer;
      Buffer: string): IZSelectSchema;

    function ReplaceIsNull(Tokens: IZCollection): IZCollection;
  end;

implementation

uses SysUtils, ZCollections;

{ TZStatementSection }

{**
  Create SQL statement section object.
}
constructor TZStatementSection.Create(Name: string; Tokens: IZCollection);
begin
  FName := Name;
  FTokens := Tokens;
end;

{**
  Gets a section name.
  @return a name of the section.
}
function TZStatementSection.GetName: string;
begin
  Result := FName;
end;

{**
  Gets a section tokens.
  @returns a section tokens.
}
function TZStatementSection.GetTokens: IZCollection;
begin
  Result := FTokens;
end;

{**
  Sets a new section tokens.
  @param Value a new section tokens.
}
procedure TZStatementSection.SetTokens(Value: IZCollection);
begin
  FTokens := Value;
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZStatementSection.Clone: IZInterface;
var
  Temp: IZCollection;
begin
  Temp := TZCollection.Create;
  Temp.AddAll(FTokens);
  Result := TZStatementSection.Create(FName, Temp);
end;

const
  {** The generic constants.}
  GenericSectionNames: array[0..12] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'FOR*UPDATE', 'LIMIT', 'OFFSET'
  );
  GenericSelectOptions: array[0..1] of string = (
    'DISTINCT', 'ALL'
  );
  GenericFromJoins: array[0..5] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'INNER', 'OUTER', 'JOIN'
  );
  GenericFromClauses: array[0..0] of string = (
    'ON'
  );


{ TZGenericStatementAnalyser }

{**
  Creates the object and assignes the main properties.
}
constructor TZGenericStatementAnalyser.Create;
begin
  FSectionNames := ArrayToStrings(GenericSectionNames);
  FSelectOptions := ArrayToStrings(GenericSelectOptions);
  FFromJoins := ArrayToStrings(GenericFromJoins);
  FFromClauses := ArrayToStrings(GenericFromClauses);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZGenericStatementAnalyser.Destroy;
begin
  if Assigned(FSectionNames) then
    FSectionNames.Free;
  if Assigned(FSelectOptions) then
    FSelectOptions.Free;
  if Assigned(FFromJoins) then
    FFromJoins.Free;
  if Assigned(FFromClauses) then
    FFromClauses.Free;

  inherited Destroy;
end;

{**
  Converts an array of strings into TStrings object.
  @param Value an array of strings to be converted.
  @return a TStrings object with specified strings.
}
function TZGenericStatementAnalyser.ArrayToStrings(
  Value: array of string): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := Low(Value) to High(Value) do
    Result.Add(Value[I]);
end;

{**
  Converts a list of tokens into the list or token values.
}
function TZGenericStatementAnalyser.TokensToStrings(
  Tokens: IZCollection): TStrings;
var
  I: Integer;
  Current: IZToken;
begin
  Result := TStringList.Create;
  for I := 0 to Tokens.Count - 1 do
  begin
    Current := Tokens[I] as IZToken;
    if Current.IsWord then
      Result.Add(UpperCase(Current.GetString))
    else Result.Add(Current.GetString);
  end;
end;

{**
  Finds a keyword with one, two or three consisted words in the list
  @param Tokens a list or tokens
  @param TokenIndex an index of the current token
  @param Keywords a list of keywords (in uppers case delimited with '*')
  @param Keyword an out parameter with found keyword.
  @param WordCount a count of words in the found keyword.
}
function TZGenericStatementAnalyser.FindKeyword(Tokens: IZCollection;
  TokenIndex: Integer; Keywords: TStrings; var Keyword: string;
  var WordCount: Integer): Boolean;
var
  I: Integer;
  Current: IZToken;
  TokenType: string;
begin
  WordCount := 0;
  Keyword := '';
  Result := False;

  for I := 1 to 3 do
  begin
    if (Tokens.Count <= TokenIndex) then
      Break;
    Current := Tokens[TokenIndex] as IZToken;
    if not Current.IsWord then
      Break;
    if Keyword <> '' then
      Keyword := Keyword + '*';
    Keyword := Keyword + UpperCase(Current.GetString);
    Inc(WordCount);
    if Keywords.IndexOf(Keyword) >= 0 then
    begin
      Result := True;
      Break;
    end;
    Inc(TokenIndex);
    { Skips whitespaces. }
    while Tokens.Count > TokenIndex do
    begin
      TokenType := (Tokens[TokenIndex] as IZToken).GetTokenType;
      if (TokenType <> TT_WHITESPACE) and (TokenType <> TT_COMMENT) then
        Break;
      Inc(TokenIndex);
      Inc(WordCount);
    end;
  end;

  if not Result then
  begin
    WordCount := 0;
    Keyword := '';
  end;
end;

{**
  Finds a section by it's name.
  @param Sections a list of sections.
  @param Name a name of the section to be found.
  @return a list of section tokens or <code>null</code>
    if section is was not found.
}
function TZGenericStatementAnalyser.FindSection(
  Sections: IZCollection; Name: string): IZCollection;
var
  I: Integer;
  Current: IZStatementSection;
begin
  Result := nil;
  for I := 0 to Sections.Count - 1 do
  begin
    Current := Sections[I] as IZStatementSection;
    if Current.GetName = Name then
    begin
      Result := Current.GetTokens;
      Break;
    end;
  end;
end;

{**
  Splits a given string into a list of tokens with tokenizer.
  @param Tokenizer a tokenizer object.
  @param Buffer a string buffer to be split.
  @return a list with tokens.
}
function TZGenericStatementAnalyser.SplitTokens(
  Tokenizer: IZTokenizer; Buffer: string; Cleanup: Boolean): IZCollection;
var
  Token: IZToken;
  SkipWhitespace: Boolean;
  TokenType: string;
begin
  Tokenizer.DefaultWhitespaceState.SetVisible(True);
  Tokenizer.DefaultCommentState.SetVisible(True);
  Tokenizer.SetBuffer(Buffer);

  Result := TZCollection.Create;
  SkipWhitespace := True;
  while True do
  begin
    Token := Tokenizer.NextToken;
    TokenType := Token.GetTokenType;
    if TokenType = TT_EOF then
      Break;
    if Cleanup and ((TokenType = TT_WHITESPACE)
      or (TokenType = TT_COMMENT)) then
    begin
      if not SkipWhitespace then
        Result.Add(TZToken.CreateWithType(TT_WHITESPACE, ' '));
      SkipWhitespace := True;
    end
    else
    begin
      Result.Add(Token);
      SkipWhitespace := False;
    end;
  end;
end;

{**
  Splits a given list of tokens into the list named sections.
  @param Tokens a list of tokens.
  @return a list of section names where object property contains
    a list of tokens in the section. It initial list is not started
    with a section name the first section is unnamed ('').
}
function TZGenericStatementAnalyser.SplitSections(Tokens: IZCollection):
  IZCollection;
var
  I: Integer;
  Keyword: string;
  Current: IZToken;
  TokenValue: string;
  WordCount: Integer;
  TokenIndex: Integer;
  Elements: IZCollection;
  FoundSection: Boolean;
  BracketCount: Integer;
begin
  Result := TZCollection.Create;
  TokenIndex := 0;
  FoundSection := True;
  Elements := nil;
  FindKeyword(Tokens, TokenIndex, SectionNames, Keyword, WordCount);
  while TokenIndex < Tokens.Count do
  begin
    if FoundSection then
    begin
      Elements := TZCollection.Create;
      for I := 0 to WordCount - 1 do
        Elements.Add(Tokens[TokenIndex + I]);
      Inc(TokenIndex, WordCount);
      Result.Add(TZStatementSection.Create(Keyword, Elements));
    end;
    FoundSection := FindKeyword(Tokens, TokenIndex, SectionNames,
      Keyword, WordCount);
    if not FoundSection and (TokenIndex < Tokens.Count) then
    begin
      BracketCount := 0;
      repeat
        Current := Tokens[TokenIndex] as IZToken;
        TokenValue := Current.GetString;
        Elements.Add(Current);
        if TokenValue = '(' then
          Inc(BracketCount)
        else if TokenValue = ')' then
          Dec(BracketCount);
        Inc(TokenIndex);
      until (BracketCount <= 0) or (TokenIndex >= Tokens.Count);
    end;
  end;
end;

{**
  Composes a string from the list of tokens.
  @param Tokens a list of tokens.
  @returns a composes string.
}
function TZGenericStatementAnalyser.ComposeTokens(
  Tokens: IZCollection): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Tokens.Count - 1 do
    Result := Result + (Tokens[I] as IZToken).GetString;
end;

{**
  Composes a string from the list of statement sections.
  @param Tokens a list of statement sections.
  @returns a composes string.
}
function TZGenericStatementAnalyser.ComposeSections(
  Sections: IZCollection): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Sections.Count - 1 do
  begin
    Result := Result + ComposeTokens(
      (Sections[I] as IZStatementSection).GetTokens);
  end;
end;

{**
  Replaces '=NULL' to 'IS NULL' tokens.
  @param Tokens a string tokens list.
  @return a string token list with replaced symbol.
}
function TZGenericStatementAnalyser.ReplaceIsNull(
  Tokens: IZCollection): IZCollection;
var
  TokenIndex: Integer;
  NextIndex: Integer;
begin
  Result := TZCollection.Create;
  TokenIndex := 0;
  while TokenIndex < Tokens.Count do
  begin
    if (Tokens[TokenIndex] as IZToken).GetString = '=' then
    begin
      NextIndex := TokenIndex + 1;
      { Skips whitespaces. }
      while (Tokens.Count > NextIndex) and (((Tokens[NextIndex] as IZToken).
        GetTokenType = TT_WHITESPACE) or ((Tokens[NextIndex] as IZToken).
        GetTokenType = TT_COMMENT)) do
      begin
        Inc(NextIndex);
      end;
      { Replaces '=' to ' IS ' }
      if (Tokens.Count > NextIndex) and
        (UpperCase((Tokens[NextIndex] as IZToken).GetString) = 'NULL') then
      begin
        Result.Add(TZToken.CreateWithType(TT_WORD, ' IS '));
      end else
        Result.Add(Tokens[TokenIndex]);
    end
    else if (Tokens[TokenIndex] as IZToken).GetString = '<>' then
    begin
      NextIndex := TokenIndex + 1;
      { Skips whitespaces. }
      while (Tokens.Count > NextIndex) and (((Tokens[NextIndex] as IZToken).
        GetTokenType = TT_WHITESPACE) or ((Tokens[NextIndex] as IZToken).
        GetTokenType = TT_COMMENT)) do
      begin
        Inc(NextIndex);
      end;
      { Replaces '<>' to ' IS NOT ' }
      if (Tokens.Count > NextIndex) and
        (UpperCase((Tokens[NextIndex] as IZToken).GetString) = 'NULL') then
      begin
        Result.Add(TZToken.CreateWithType(TT_WORD, ' IS NOT '));
      end else
        Result.Add(Tokens[TokenIndex]);
    end else
      Result.Add(Tokens[TokenIndex]);
    Inc(TokenIndex);
  end;
end;

{**
  Skips tokens inside brackets.
  @param Tokens a list of tokens to scan.
  @param TokenIndex the index of the current token.
  @return <code>true</code> if some tokens were skipped.
}
function TZGenericStatementAnalyser.SkipBracketTokens(Tokens: IZCollection;
  var TokenIndex: Integer): Boolean;
var
  BracketCount: Integer;
  Current: string;
begin
  { Checks for the start bracket. }
  if (TokenIndex < Tokens.Count)
    and ((Tokens[TokenIndex] as IZToken).GetString <> '(') then
  begin
    Result := False;
    Exit;
  end;

  { Skips the expression in brackets. }
  Result := True;
  BracketCount := 1;
  Inc(TokenIndex);
  while (TokenIndex < Tokens.Count) and (BracketCount > 0) do
  begin
    Current := (Tokens[TokenIndex] as IZToken).GetString;
    if Current = '(' then
      Inc(BracketCount)
    else if Current = ')' then
      Dec(BracketCount);
    Inc(TokenIndex);
  end;
end;

{**
  Skips option tokens specified in the string list.
  @param Tokens a list of tokens to scan.
  @param TokenIndex the index of the current token.
  @param Options a list of option keyword strings in the upper case.
  @return <code>true</code> if some tokens were skipped.
}
function TZGenericStatementAnalyser.SkipOptionTokens(Tokens: IZCollection;
  var TokenIndex: Integer; Options: TStrings): Boolean;
var
  Current: IZToken;
  TokenType: string;
begin
  Result := False;
  while TokenIndex < Tokens.Count do
  begin
    Current := Tokens[TokenIndex] as IZToken;
    TokenType := Current.GetTokenType;
    if (TokenType <> TT_WHITESPACE)
      and (TokenType <> TT_COMMENT)
      and (Options.IndexOf(UpperCase(Current.GetString)) < 0) then
    begin
      Break;
    end;
    Inc(TokenIndex);
    Result := True;
  end;
end;

{**
  Extracts a collection of field references from the select section.
  @param SelectTokens a list of tokens in select section.
  @return a list of field references.
}
function TZGenericStatementAnalyser.ExtractFieldRefs(
  SelectTokens: IZCollection): IZCollection;
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Field: string;
  Alias: string;
  Current: IZToken;
  CurrentValue: string;
  CurrentType: string;
  CurrentUpper: string;
  ReadField: Boolean;

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Field := '';
    Alias := '';
    ReadField := True;
  end;

begin
  Result := TZCollection.Create;
  TokenIndex := 1;
  SkipOptionTokens(SelectTokens, TokenIndex, Self.SelectOptions);

  ClearElements;
  while TokenIndex < SelectTokens.Count do
  begin
    Current := SelectTokens[TokenIndex] as IZToken;
    CurrentValue := Current.GetString;
    CurrentUpper := UpperCase(CurrentValue);
    CurrentType := Current.GetTokenType;

    { Switches to alias part. }
    if (CurrentUpper = 'AS') or (CurrentType = TT_WHITESPACE) then
    begin
      ReadField := ReadField and (CurrentUpper <> 'AS') and (Field = '');
    end
    { Reads field. }
    else if (ReadField = True) and ((CurrentType = TT_WORD) or
      (CurrentValue = '*')) then
    begin
      Catalog := Schema;
      Schema := Table;
      Table := Field;
      Field := CurrentValue;
    end
    { Skips a '.' in field part. }
    else if (ReadField = True) and (CurrentValue = '.') then
    begin
    end
    { Reads alias. }
    else if (ReadField = False) and (CurrentType = TT_WORD) then
    begin
      Alias := CurrentValue;
    end
    { Ends field reading. }
    else if CurrentValue = ',' then
    begin
      if Field <> '' then
      begin
        Result.Add(TZFieldReference.Create(True, Catalog, Schema, Table,
          Field, Alias, nil));
      end;
      ClearElements;
    end
    { Skips till the next field. }
    else
    begin
      ClearElements;
      Inc(TokenIndex);
      while (TokenIndex < SelectTokens.Count) and (CurrentValue <> ',') do
      begin
        Current := SelectTokens[TokenIndex] as IZToken;
        CurrentValue := Current.GetString;
        if CurrentValue = '(' then
          SkipBracketTokens(SelectTokens, TokenIndex)
        else begin
          CurrentType := Current.GetTokenType;
          if CurrentType = TT_WORD then
            Alias := CurrentValue
          else if (CurrentType <> TT_WHITESPACE)
            and (CurrentType <> TT_COMMENT)
            and (CurrentValue <> ',') then
            Alias := '';
          Inc(TokenIndex);
        end;
      end;
      if Alias <> '' then
      begin
        Result.Add(TZFieldReference.Create(False, '', '', '', '', Alias, nil));
        ClearElements;
      end;
    end;
    Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Field <> '' then
  begin
    Result.Add(TZFieldReference.Create(True, Catalog, Schema, Table,
      Field, Alias, nil));
  end;
end;

{**
  Extracts a collection of table references from the from section.
  @param FromTokens a list of tokens in from section.
  @return a list of table references.
}
function TZGenericStatementAnalyser.ExtractTableRefs(
  FromTokens: IZCollection): IZCollection;
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Alias: string;
  Current: IZToken;
  CurrentValue: string;
  CurrentType: string;
  CurrentUpper: string;
  ReadTable: Boolean;

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Alias := '';
    ReadTable := True;
  end;

begin
  Result := TZCollection.Create;
  TokenIndex := 1;

  ClearElements;
  while TokenIndex < FromTokens.Count do
  begin
    Current := FromTokens[TokenIndex] as IZToken;
    CurrentValue := Current.GetString;
    CurrentUpper := UpperCase(CurrentValue);
    CurrentType := Current.GetTokenType;

    { Processes from join keywords. }
    if FromJoins.IndexOf(CurrentUpper) >= 0 then
    begin
      if Table <> '' then
        Result.Add(TZTableReference.Create(Catalog, Schema, Table, Alias));
      ClearElements;
      SkipOptionTokens(FromTokens, TokenIndex, FromJoins);
      Continue;
    end
    { Skips from clause keywords. }
    else if FromClauses.IndexOf(CurrentUpper) >= 0 then
    begin
      Inc(TokenIndex);
      while (TokenIndex < FromTokens.Count)
        and (FromJoins.IndexOf(CurrentUpper) < 0) and (CurrentUpper <> ',') do
      begin
        CurrentUpper := UpperCase((FromTokens[TokenIndex] as IZToken).GetString);
        if CurrentValue = '(' then
          SkipBracketTokens(FromTokens, TokenIndex)
        else Inc(TokenIndex);
      end;
    end
    { Switches to alias part. }
    else if (CurrentUpper = 'AS') or (CurrentType = TT_WHITESPACE) then
    begin
      ReadTable := ReadTable and (CurrentUpper <> 'AS') and (Table = '');
    end
    { Reads table. }
    else if (ReadTable = True) and (CurrentType = TT_WORD) then
    begin
      Catalog := Schema;
      Schema := Table;
      Table := CurrentValue;
    end
    { Skips a '.' in table part. }
    else if (ReadTable = True) and (CurrentValue = '.') then
    begin
    end
    { Reads alias. }
    else if (ReadTable = False) and (CurrentType = TT_WORD) then
    begin
      Alias := CurrentValue;
    end
    { Ends field reading. }
    else if CurrentValue = ',' then
    begin
      if Table <> '' then
        Result.Add(TZTableReference.Create(Catalog, Schema, Table, Alias));
      ClearElements;
    end;
    { Skips till the next field. }
    if CurrentValue = '(' then
      SkipBracketTokens(FromTokens, TokenIndex)
    else Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Table <> '' then
    Result.Add(TZTableReference.Create(Catalog, Schema, Table, Alias));
end;

{**
  Extracts a select schema from the specified parsed select statement.
  @param Sections a list of sections.
  @return a select statement schema.
}
function TZGenericStatementAnalyser.ExtractSelectSchemaFromSections(
  Sections: IZCollection): IZSelectSchema;
var
  SelectTokens: IZCollection;
  FromTokens: IZCollection;
begin
  Result := nil;
  { Checks for the correct select statement. }
  if not ((Sections[0] as IZStatementSection).GetName = 'SELECT')
    or (((Sections[0] as IZStatementSection).GetName = '')
    and ((Sections[1] as IZStatementSection).GetName = 'SELECT')) then
    Exit;
  SelectTokens := FindSection(Sections, 'SELECT');
  FromTokens := FindSection(Sections, 'FROM');
  if (SelectTokens = nil) or (FromTokens = nil) then
    Exit;

  { Creates and fills the result object. }
  Result := TZSelectSchema.Create;
  Result.GetFields.AddAll(ExtractFieldRefs(SelectTokens));
  Result.GetTables.AddAll(ExtractTableRefs(FromTokens));
end;

{**
  Extracts a select schema from the specified parsed select statement.
  @param Sections a list of sections.
  @return a select statement schema.
}
function TZGenericStatementAnalyser.ExtractSelectSchemaFromBuffer(
  Tokenizer: IZTokenizer; Buffer: string): IZSelectSchema;
var
  Tokens: IZCollection;
  Sections: IZCollection;
begin
  Tokens := SplitTokens(Tokenizer, Buffer, True);
  Sections := SplitSections(Tokens);
  Result := ExtractSelectSchemaFromSections(Sections);
end;

end.

