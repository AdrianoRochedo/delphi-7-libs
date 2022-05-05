{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Parsing classes and interfaces               }
{                                                         }
{    Copyright (c) 1999 Steven J. Metsker.                }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Ported by Sergey Seroukhov                   }
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

unit ZParseToken;

interface

uses Classes, SysUtils, ZClasses, ZParseIntfs, ZParseCore, ZTokenizer;

type

  {**
    A TokenAssembly is an Assembly whose elements are Tokens.
    Tokens are, roughly, the chunks of text that a <code>
    Tokenizer</code> returns.
  }
  TZTokenAssembly = class (TZAbstractAssembly)
  private
    FTokenString: IZTokenString;
  protected
    property TokenString: IZTokenString read FTokenString write FTokenString;
  public
    constructor CreateWithBuffer(Buffer: string);
    constructor CreateWithTokenizer(Tokenizer: IZTokenizer);
    constructor CreateWithTokenString(TokenString: IZTokenString);

    function Clone: IZInterface; override;

    function DefaultDelimiter: string; override;
    function Consumed(Delimiter: string): string; override;
    function Remainder(Delimiter: string): string; override;
    function ElementsCount: Integer; override;
    function NextElement: string; override;

    function Peek: IZObject; override;
  end;

  {** A Literal matches a specific String from an assembly. }
  TZLiteral = class (TZTerminal)
  private
    FLiteral: IZToken;
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;

    property Literal: IZToken read FLiteral write FLiteral;
  public
    constructor CreateWithValue(Name: string; Value: string);
  end;

  {**
    A CaselessLiteral matches a specified String from an assembly,
    disregarding case.
  }
  TZCaselessLiteral = class (TZLiteral)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
  public
    constructor CreateWithValue(Name: string; Value: string);
  end;

  {** A Num matches a number from a token assembly. }
  TZNum = class (TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  end;

  {**
    A QuotedString matches a quoted string, like "this one"
    from a token assembly.
  }
  TZQuotedString = class(TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  end;

  {**
    A Symbol matches a specific sequence, such as
    <code><</code>, or <code><=</code> that a tokenizer returns as a symbol.
  }
  TZSymbol = class (TZTerminal)
  private
    FSymbol: IZToken;
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;

    property Symbol: IZToken read FSymbol write FSymbol;
  public
    constructor CreateWithChar(Name: string; Value: Char);
    constructor CreateWithString(Name: string; Value: string);
  end;

  {** A Word matches a word from a token assembly. }
  TZWord = class (TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  end;

  {** Implements a Token Tester class. }
  TZTokenTester = class (TZAbstractParserTester)
  protected
    function GetAssembly(Value: string): IZAssembly; override;
  public
    constructor Create(Parser: IZParser);
  end;

implementation

{ TZTokenAssembly }

{**
  Constructs a TokenAssembly from the given TokenString.
  @param tokenString the tokenString to consume
}
constructor TZTokenAssembly.CreateWithTokenString(TokenString: IZTokenString);
begin
  inherited Create;
  FTokenString := TokenString;
end;

{**
  Constructs a TokenAssembly on a TokenString constructed
  from the given String.
  @param   string   the string to consume
}
constructor TZTokenAssembly.CreateWithBuffer(Buffer: string);
begin
  CreateWithTokenString(TZTokenString.CreateWithBuffer(Buffer));
end;

{**
  Constructs a TokenAssembly on a TokenString constructed
  from the given Tokenizer.
  @param   Tokenizer   the tokenizer to consume tokens from
}
constructor TZTokenAssembly.CreateWithTokenizer(Tokenizer: IZTokenizer);
begin
  CreateWithTokenString(TZTokenString.CreateWithTokenizer(Tokenizer));
end;

{**
  Return a copy of this object.
  @return a copy of this object
}
function TZTokenAssembly.Clone: IZInterface;
begin
  Result := CloneProperties(TZTokenAssembly.CreateWithTokenString(
    FTokenString.Clone as IZTokenString));
end;

{**
  Returns a textual representation of the amount of this
  tokenAssembly that has been consumed.

  @param delimiter the mark to show between consumed elements
  @return a textual description of the amount of this
    assembly that has been consumed
}
function TZTokenAssembly.Consumed(Delimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ElementsConsumed - 1 do
  begin
    if I > 0 then
      Result := Result + Delimiter;
    Result := Result + FTokenString[I].ToString;
  end;
end;

{**
  Returns the default string to show between elements consumed or remaining.
  @return the default string to show between elements consumed or remaining
}
function TZTokenAssembly.DefaultDelimiter: string;
begin
  Result := '/';
end;

{**
  Returns the number of elements in this assembly.
  @return   the number of elements in this assembly
}
function TZTokenAssembly.ElementsCount: Integer;
begin
  Result := FTokenString.Count;
end;

{**
  Returns the next token.
  @return   the next token from the associated token string.
}
function TZTokenAssembly.NextElement: string;
begin
  Result := FTokenString[ElementIndex].ToString;
  ElementIndex := ElementIndex + 1;
end;

{**
  Shows the next object in the assembly, without removing it
  @return   the next object
}
function TZTokenAssembly.Peek: IZObject;
begin
  if ElementIndex < ElementsCount then
    Result := FTokenString[ElementIndex] as IZToken
  else Result := nil;
end;

{**
  Returns a textual representation of the amount of this
  tokenAssembly that remains to be consumed.

  @param delimiter the mark to show between consumed elements
  @return a textual description of the amount of this
    assembly that remains to be consumed
}
function TZTokenAssembly.Remainder(Delimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := ElementsConsumed to FTokenString.Count - 1 do
  begin
    if I > ElementsConsumed then
      Result := Result + Delimiter;
    Result := Result + FTokenString[I].ToString;
  end;
end;

{ TZLiteral }

{**
  Constructs a literal that will match the specified string.
  @param Name a name of this parser.
  @param Value the string to match as a token
}
constructor TZLiteral.CreateWithValue(Name: string; Value: string);
begin
  inherited CreateWithName(Name);
  FLiteral := TZToken.Create(Value);
end;

{**
  Returns true if the literal this object equals an assembly's next element.
  @param Value an element from an assembly
  @return true, if the specified literal equals the next token from an assembly
}
function TZLiteral.Qualifies(Value: IZObject): Boolean;
begin
  Result := FLiteral.Equals(Value);
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return string a textual description of this parser
}
function TZLiteral.UnvisitedString(Visited: IZCollection): string;
begin
  Result := FLiteral.ToString;
end;

{ TZCaselessLiteral }

{**
  Constructs a literal that will match the specified string,
  given mellowness about case.
  @param Name a name of this parser.
  @param Value the string to match as a token
}
constructor TZCaselessLiteral.CreateWithValue(Name: string; Value: string);
begin
  inherited CreateWithValue(Name, Value);
end;

{**
  Returns true if the literal this object equals an
  assembly's next element, disregarding case.

  @param Value an element from an assembly
  @return true, if the specified literal equals the next
    token from an assembly, disregarding case
}
function TZCaselessLiteral.Qualifies(Value: IZObject): Boolean;
begin
  Result := Literal.EqualsIgnoreCase(Value);
end;

{ TZNum }

{**
  Returns true if an assembly's next element is a number.
  @param   object   an element from an assembly
  @return   true, if an assembly's next element is a number
   as recognized the tokenizer
}
function TZNum.Qualifies(Value: IZObject): Boolean;
begin
  if Value.InstanceOf(IZToken) then
    Result := (Value as IZToken).IsNumber
  else Result := False;
end;

{**
  Create a set with one random number (between 0 and 100).
}
function TZNum.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
begin
  Result := TStringList.Create;
  Result.Add(FloatToStr(Trunc(Random(1000)) / 10));
end;

{**
  Returns a textual description of this parser.
  @param   vector   a list of parsers already printed in this description
  @return   string   a textual description of this parser
}
function TZNum.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'Num';
end;

{ TZQuotedString }

{**
  Returns true if an assembly's next element is a quoted string.
  @param Value an element from a assembly
  @return true, if a assembly's next element is a quoted
    string, like "chubby cherubim".
}
function TZQuotedString.Qualifies(Value: IZObject): Boolean;
begin
  if Value.InstanceOf(IZToken) then
    Result := (Value as IZToken).IsQuotedString
  else Result := False;
end;

{**
  Create a set with one random quoted string (with 2 to 6 characters).
}
function TZQuotedString.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
var
  I, Count: Integer;
  TempStr: string;
begin
  TempStr := '';
  Count := Random(5);
  for I := 1 to Count do
    TempStr := TempStr + Chr(Random(26) + Ord('a'));
  TempStr := '"' + TempStr + '"';

  Result := TStringList.Create;
  Result.Add(TempStr);
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return a textual description of this parser
}
function TZQuotedString.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'QuotedString';
end;

{ TZSymbol }

{**
  Constructs a symbol that will match the specified char.
  @param Name a name of this parser.
  @param Value the character to match. The char must be one that
    the tokenizer will return as a symbol token. This typically includes most
    characters except letters and digits.
}
constructor TZSymbol.CreateWithChar(Name: string; Value: Char);
begin
  inherited CreateWithName(Name);
  FSymbol := TZToken.CreateWithType(TT_SYMBOL, Value);
end;

{**
  Constructs a symbol that will match the specified sequence
  of characters.
  @param Name a name of this parser.
  @param Value the characters to match. The characters must be a sequence
    that the tokenizer will return as a symbol token, such as <code><=</code>.
}
constructor TZSymbol.CreateWithString(Name: string; Value: string);
begin
  inherited CreateWithName(Name);
  FSymbol := TZToken.CreateWithType(TT_SYMBOL, Value);
end;

{**
  Returns true if the symbol this object represents equals an
  assembly's next element.

  @param Value an element from an assembly
  @return true, if the specified symbol equals the next
    token from an assembly
}
function TZSymbol.Qualifies(Value: IZObject): Boolean;
begin
  Result := FSymbol.Equals(Value);
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in
    this description
  @return   string   a textual description of this parser
}
function TZSymbol.UnvisitedString(Visited: IZCollection): string;
begin
  Result := FSymbol.ToString;
end;

{ TZWord }

{**
  Returns true if an assembly's next element is a word.
  @param Value an element from an assembly
  @return true, if an assembly's next element is a word
}
function TZWord.Qualifies(Value: IZObject): Boolean;
begin
  if Value.InstanceOf(IZToken) then
    Result := (Value as IZToken).IsWord
  else Result := False;
end;

{**
  Create a set with one random word (with 3 to 7 characters).
}
function TZWord.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
var
  I, Count: Integer;
  TempStr: string;
begin
  TempStr := '';
  Count := Random(5) + 3;
  for I := 1 to Count do
    TempStr := TempStr + Chr(Random(26) + Ord('a'));

  Result := TStringList.Create;
  Result.Add(TempStr);
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return a textual description of this parser
}
function TZWord.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'Word';
end;

{ TZTokenTester }

{**
  Constructs this parser and assignes the main properties.
  @param Parser a parent parser object.
}
constructor TZTokenTester.Create(Parser: IZParser);
begin
  inherited Create(Parser);
end;

{**
  Gets a typed assembly object.
  @return an assembly object.
}
function TZTokenTester.GetAssembly(Value: string): IZAssembly;
begin
  Result := TZTokenAssembly.CreateWithBuffer(Value);
end;

end.

