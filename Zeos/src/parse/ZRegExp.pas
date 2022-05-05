{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Regular Expressions                    }
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

unit ZRegExp;

interface

uses Classes, SysUtils, ZClasses, ZParseIntfs;

type

{** Signals that a given string is not recognizable as a regular expression. }
EZRegularExpressionException = class (Exception);

{**
  This class provides a parser that recognizes regular expressions.
  <p>
  Regular expressions are a "metalanguage", which means they
  form a language for describing languages. For example,
  <code>a*</code> is a regular expression that describes a
  simple language whose elements are strings composed of 0
  or more <code>a's</code>. Thus the result of parsing
  <code>a*</code> is a new parser, namely a
  parser that will match strings of <code>a's</code>.
  <p>
  This class exists to show how a simple regular expression
  parser works. It recognizes expressions according to
  the following rules.
  <blockquote><pre>
      expression    = term orTerm*;
      term          = factor nextFactor*;
      orTerm        = '|' term;
      factor        = phrase | phraseZeroMore | phraseOneMore | phraseZeroOne;
      nextFactor    = factor;
      phrase        = escapeChar | letterOrDigit | '(' expression ')';
      phraseZeroMore = phrase '*';
      phraseOneMore = phrase '+';
      phraseZeroOne = phrase '?';
      letterOrDigit = Char;
      escapeChar = '\' Char;
  </pre></blockquote>
  These rules recognize conventional operator precedence.
  They also avoid the problem of left recursion, and their
  implementation avoids problems with the infinite loop
  inherent in the cyclic dependencies of the rules.
}
TZRegularParserFactory = class
private
  FParser: IZSequence;
protected
  function GetFactor: IZParser;
  function GetLetterOrDigit: IZParser;
  function GetEscapeChar: IZParser;
  function GetNextFactor: IZParser;
  function GetOrTerm: IZParser;
  function GetPhrase: IZParser;
  function GetPhraseZeroMore: IZParser;
  function GetPhraseOneMore: IZParser;
  function GetPhraseZeroOne: IZParser;
  function GetTerm: IZParser;

  property Parser: IZSequence read FParser write FParser;
public
  function GetParser: IZParser;

  class function GetDefaultParser: IZParser;
  class function ParseRegExp(Value: string): IZParser;
end;

{** Calculator of regular expressions. }
TZRegExp = class
private
  FParser: IZParser;
public
  constructor Create(const Pattern: string);
  function IsMatch(const Value: string): Boolean;

  class function IsMatchToPattern(const Pattern, Value: string): Boolean;
end;

implementation

uses ZParseCore, ZParseChars;

type

{**
  Pop a <code>Character</code> from the stack and push a
  <code>SpecificChar</code> parser in its place.
}
TZCharAssembler = class (TZAbstractAssembler)
public
  procedure WorkOn(Assembly: IZAssembly); override;
end;

{**
  Pop an escape <code>Character</code> from the stack and push a
  specific parser in its place.
}
TZEscapeAssembler = class (TZAbstractAssembler)
public
  procedure WorkOn(Assembly: IZAssembly); override;
end;

{**
  Pop two Parsers from the stack and push a new <code> Sequence</code> of them.
}
TZAndAssembler = class (TZAbstractAssembler)
public
  procedure WorkOn(Assembly: IZAssembly); override;
end;

{**
  Pop two parsers from the stack and push a new <code>
  Alternation</code> of them.
}
TZOrAssembler = class (TZAbstractAssembler)
public
  procedure WorkOn(Assembly: IZAssembly); override;
end;

{**
  Pop a parser from the stack and push a new <code> Repetition</code> of it.
}
TZRepeatAssembler = class (TZAbstractAssembler)
private
  FMinCount, FMaxCount: Integer;
public
  constructor Create(MinCount, MaxCount: Integer);
  procedure WorkOn(Assembly: IZAssembly); override;
end;

{ TZCharAssembler }

{**
  Pop a <code>Character</code> from the stack and push a
  <code>SpecificChar</code> interpeter in its place.
  @param   Assembly   the assembly whose stack to use
}
procedure TZCharAssembler.WorkOn(Assembly: IZAssembly);
var
  TempChar: Char;
begin
  TempChar := (Assembly.Pop as IZAnyValue).GetString[1];
  if TempChar = '.' then
    Assembly.Push(TZChar.Create)
  else
    Assembly.Push(TZSpecificChar.CreateWithValue('', TempChar));
end;

{ TZEscapeAssembler }

{**
  Pop a <code>Character</code> from the stack and push a
  <code>SpecificChar</code> interpeter in its place.
  @param   Assembly   the assembly whose stack to use
}
procedure TZEscapeAssembler.WorkOn(Assembly: IZAssembly);
var
  TempChar: Char;
begin
  TempChar := (Assembly.Pop as IZAnyValue).GetString[1];
  if TempChar = 'd' then
    Assembly.Push(TZDigit.Create)
  else if TempChar = 'w' then
    Assembly.Push(TZLetter.Create)
  else
    Assembly.Push(TZSpecificChar.CreateWithValue('', TempChar));
end;

{ TZAndAssembler }

{**
  Pop two parsers from the stack and push a new
  <code>Sequence</code> of them.
  @param Assembly the assembly whose stack to use
}
procedure TZAndAssembler.WorkOn(Assembly: IZAssembly);
var
  Top: IZParser;
  Sequence: IZSequence;
begin
  Top := Assembly.Pop as IZParser;
  Sequence := TZSequence.Create;
  Sequence.Add(Assembly.Pop as IZParser);
  Sequence.Add(Top);
  Assembly.Push(Sequence);
end;

{ TZOrAssembler }

{**
  Pop two parsers from the stack and push a new
  <code>Alternation</code> of them.
  @param   Assembly   the assembly whose stack to use
}
procedure TZOrAssembler.WorkOn(Assembly: IZAssembly);
var
  Top: IZParser;
  Alternation: IZAlternation;
begin
  Top := Assembly.Pop as IZParser;
  Alternation := TZAlternation.Create;
  Alternation.Add(Assembly.Pop as IZParser);
  Alternation.Add(Top);
  Assembly.Push(Alternation);
end;

{ TZRepeatAssembler }

{**
  Constructs this object and assignes main properties.
  @param MinCount a minimum repetition count.
  @param MaxCount a maximum repetition count.
}
constructor TZRepeatAssembler.Create(MinCount, MaxCount: Integer);
begin
  inherited Create;
  FMinCount := MinCount;
  FMaxCount := MaxCount;
end;

{**
  Pop a parser from the stack and push a new <code> Repetition</code> of it.
}
procedure TZRepeatAssembler.WorkOn(Assembly: IZAssembly);
begin
  Assembly.Push(TZRepetition.CreateWithLimits(
    Assembly.Pop as IZParser, FMinCount, FMaxCount));
end;

{ TZRegularParserFactory }

{**
  Returns a parser that for the grammar rule:
     factor = phrase | phraseStar;
}
function TZRegularParserFactory.GetFactor: IZParser;
var
  Alternation: IZAlternation;
begin
  Alternation := TZAlternation.Create;
  Alternation.Add(GetPhrase);
  Alternation.Add(GetPhraseZeroMore);
  Alternation.Add(GetPhraseOneMore);
  Alternation.Add(GetPhraseZeroOne);
  Result := Alternation;
end;

{**
  Returns a parser that for the grammar rule:
     letterOrDigit = Letter | Digit;
  This parser has an assembler that will pop a
  character and push a SpecificChar parser in its place.
}
function TZRegularParserFactory.GetLetterOrDigit: IZParser;
var
  Alternation: IZAlternation;
begin
  Alternation := TZAlternation.Create;
  Alternation.Add(TZLetter.Create);
  Alternation.Add(TZDigit.Create);
  Alternation.Add(TZSpecificChar.CreateWithValue('', '.'));
  Alternation.SetAssembler(TZCharAssembler.Create);
  Result := Alternation;
end;

{**
  Returns a parser that for the grammar rule:
     escapeChar = '\' Char;
  This parser has an assembler that will pop a
  character and push a SpecificChar parser in its place.
}
function TZRegularParserFactory.GetEscapeChar: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(TZSpecificChar.CreateWithValue('', '\').Discard);
  Sequence.Add(TZChar.Create);
  Sequence.SetAssembler(TZEscapeAssembler.Create);
  Result := Sequence;
end;

{**
  Returns a parser that for the grammar rule:
     nextFactor = factor;
  This parser has an assembler that will pop two
  parsers and push a Sequence of them.
}
function TZRegularParserFactory.GetNextFactor: IZParser;
begin
  Result := GetFactor;
  Result.SetAssembler(TZAndAssembler.Create);
end;

{**
  Returns a parser that for the grammar rule:
     orTerm = '|' term;
  This parser has an assembler that will pop two
  parsers and push an Alternation of them.
}
function TZRegularParserFactory.GetOrTerm: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(TZSpecificChar.CreateWithValue('', '|').Discard);
  Sequence.Add(GetTerm);
  Sequence.SetAssembler(TZOrAssembler.Create);
  Result := Sequence;
end;

{**
  Returns a parser that for the grammar rule:
      phrase = letterOrDigit | '(' expression ')';
}
function TZRegularParserFactory.GetPhrase: IZParser;
var
  Alternation: IZAlternation;
  Sequence: IZSequence;
begin
  Alternation := TZAlternation.Create;
  Alternation.Add(GetEscapeChar);
  Alternation.Add(GetLetterOrDigit);

  Sequence := TZSequence.Create;
  Sequence.Add(TZSpecificChar.CreateWithValue('', '(').Discard);
  Sequence.Add(GetParser);
  Sequence.Add(TZSpecificChar.CreateWithValue('', ')').Discard);

  Alternation.Add(Sequence);
  Result := Alternation;

  { Decrease reference count for cyclic references to avoid memory leaks. }
  GetParser._Release;
end;

{**
  Returns a parser that for the grammar rule:
     phraseStar = phrase '+';
  This parser has an assembler that will pop a
  parser and push a Repetition of it.
}
function TZRegularParserFactory.GetPhraseOneMore: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(GetPhrase);
  Sequence.Add(TZSpecificChar.CreateWithValue('', '+').Discard);
  Sequence.SetAssembler(TZRepeatAssembler.Create(1, MaxInt));
  Result := Sequence;
end;

{**
  Returns a parser that for the grammar rule:
     phraseStar = phrase '*';
  This parser has an assembler that will pop a
  parser and push a Repetition of it.
}
function TZRegularParserFactory.GetPhraseZeroMore: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(GetPhrase);
  Sequence.Add(TZSpecificChar.CreateWithValue('', '*').Discard);
  Sequence.SetAssembler(TZRepeatAssembler.Create(0, MaxInt));
  Result := Sequence;
end;

{**
  Returns a parser that for the grammar rule:
     phraseStar = phrase '?';
  This parser has an assembler that will pop a
  parser and push a Repetition of it.
}
function TZRegularParserFactory.GetPhraseZeroOne: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(GetPhrase);
  Sequence.Add(TZSpecificChar.CreateWithValue('', '?').Discard);
  Sequence.SetAssembler(TZRepeatAssembler.Create(0, 1));
  Result := Sequence;
end;

{**
  Returns a parser that for the grammar rule:
     term = factor nextFactor*;
}
function TZRegularParserFactory.GetTerm: IZParser;
var
  Sequence: IZSequence;
begin
  Sequence := TZSequence.Create;
  Sequence.Add(GetFactor);
  Sequence.Add(TZRepetition.Create(GetNextFactor));
  Result := Sequence;
end;

{**
  Returns a parser that will recognize a regular
  expression. (Identical to <code>start()</code>).
  @return a parser that will recognize a regular expression
}
function TZRegularParserFactory.GetParser: IZParser;
begin
  if FParser = nil then
  begin
    // expression = term orTerm*;
    FParser := TZSequence.Create;
    FParser.Add(GetTerm);
    FParser.Add(TZRepetition.Create(GetOrTerm));
  end;
  Result := FParser;
end;

{**
  Returns a parser that will recognize a regular expression.
  @return a parser that will recognize a regular expression
}
class function TZRegularParserFactory.GetDefaultParser: IZParser;
var
  Factory: TZRegularParserFactory;
begin
  Factory := TZRegularParserFactory.Create;
  try
    Result := Factory.GetParser;
  finally
    Factory.Free;
  end;
end;

{**
  Return a parser that will match a <code>CharacterAssembly</code>,
  according to the value of a regular expression given in a string.

  For example, given the string <code>a*</code>, this
  method will return a parser which will match any element
  of the set <code>["", "a", "aa", "aaa", ...]</code>.

  @param Value the string to evaluate
  @return a parser that will match a <code> CharacterAssembly</code>,
    according to the value of a regular expression in the given string
}
class function TZRegularParserFactory.ParseRegExp(Value: string): IZParser;
var
  Assembly: IZAssembly;
begin
  Assembly := TZCharacterAssembly.Create(Value);
  Assembly := GetDefaultParser.CompleteMatch(Assembly);
  if Assembly = nil then
  begin
    raise EZRegularExpressionException.Create(
      'Improperly formed regular expression');
  end;

  try
    Result := Assembly.Pop as IZParser;
  except
    raise EZRegularExpressionException.Create(
      'Internal error in RegularParser');
  end;
end;

{ TZRegExp }

{**
  Constructs this parser and assignes the main properties.
  @param Pattern a pattern for regular expression.
}
constructor TZRegExp.Create(const Pattern: string);
begin
  FParser := TZRegularParserFactory.ParseRegExp(Pattern);
end;

{**
  Checks does the value match to this regular expression.
  @param Value a string value to be matched.
}
function TZRegExp.IsMatch(const Value: string): Boolean;
begin
  Result := FParser.CompleteMatch(TZCharacterAssembly.Create(Value)) <> nil;
end;

{**
  Checks does the value match to the regular expression pattern.
  @param Pattern a pattern for regular expression.
  @param Value a string value to be matched.
}
class function TZRegExp.IsMatchToPattern(const Pattern, Value: string): Boolean;
var
  RegExpression: TZRegExp;
begin
  RegExpression := TZRegExp.Create(Pattern);
  try
    Result := RegExpression.IsMatch(Value);
  finally
    RegExpression.Free;
  end;
end;

end.

