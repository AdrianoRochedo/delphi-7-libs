{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for PostgreSQL         }
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

unit ZPostgreSqlToken;

interface

uses
  Classes, ZClasses, ZParseIntfs, ZTokenizer, ZMySqlToken;

type

  {** Implements a PostgreSQL-specific number state object. }
  TZPostgreSQLNumberState = class (TZMySQLNumberState)
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {** Implements a PostgreSQL-specific quote string state object. }
  TZPostgreSQLQuoteState = class (TZMySQLQuoteState)
  protected
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZPostgreSQLCommentState = class (TZCppCommentState)
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {** Implements a symbol state object. }
  TZPostgreSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZPostgreSQLWordState = class (TZWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZPostgreSQLTokenizer = class (TZTokenizer)
  public
    constructor Create;
    constructor CreateWithBuffer(Buffer: string);
  end;

implementation

{ TZPostgreSQLNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZPostgreSQLNumberState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  TempChar: Char;
begin
  FloatPoint := FirstChar = '.';
  Value := FirstChar;
  LastChar := #0;

  // Reads the first part of the number before decimal point
  if not FloatPoint then
  begin
    Value := Value + ReadDecDigits(Stream);
    FloatPoint := LastChar = '.';
    if FloatPoint then
    begin
      Stream.Read(TempChar, 1);
      Value := Value + TempChar;
    end;
  end;

  // Reads the second part of the number after decimal point
  if FloatPoint then
    Value := Value + ReadDecDigits(Stream);

  // Reads a power part of the number
  if LastChar in ['e','E'] then
  begin
    Stream.Read(TempChar, 1);
    Value := Value + TempChar;
    FloatPoint := True;

    Stream.Read(TempChar, 1);
    if TempChar in ['0'..'9','-','+'] then
      Value := Value + TempChar + ReadDecDigits(Stream)
    else
    begin
      Value := Copy(Value, 1, Length(Value) - 1);
      Stream.Seek(-2, soFromCurrent);
    end;
  end;

  // Prepare the result
  if Value = '.' then
  begin
    CheckExceptionState;
    Result := ExceptionState.GetNextToken(Stream, FirstChar, Tokenizer);
  end
  else if not Separation then
    Result := TZToken.CreateWithType(TT_NUMBER, Value)
  else if FloatPoint then
    Result := TZToken.CreateWithType(TT_FLOAT, Value)
  else Result := TZToken.CreateWithType(TT_INTEGER, Value);
end;

{ TZPostgreSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZPostgreSQLQuoteState.GetNextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: IZTokenizer): IZToken;
begin
  Result := inherited GetNextToken(Stream, FirstChar, Tokenizer);
  if FirstChar = '"' then
    Result := TZToken.CreateWithType(TT_WORD, Result.GetValue);
end;

{ TZPostgreSQLCommentState }

{**
  Gets a MySQL specific comments like -- or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZPostgreSQLCommentState.GetNextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: IZTokenizer): IZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  if FirstChar = '-' then
  begin
    ReadNum := Stream.Read(ReadChar, 1);
    if (ReadNum > 0) and (ReadChar = '-') then
    begin
      if Visible then
      begin
        Result := TZToken.CreateWithType(TT_COMMENT,
          '--' + GetSingleLineComment(Stream));
      end
      else
      begin
        GetSingleLineComment(Stream);
        Result := Tokenizer.NextToken;
      end;
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-1, soFromCurrent);
      CheckExceptionState;
      Result := ExceptionState.GetNextToken(Stream, FirstChar, Tokenizer);
    end;
  end
  else if FirstChar = '/' then
  begin
    ReadNum := Stream.Read(ReadChar, 1);
    if (ReadNum > 0) and (ReadChar = '*') then
    begin
      if Visible then
      begin
        Result := TZToken.CreateWithType(TT_COMMENT,
          '/*' + GetMultiLineComment(Stream));
      end
      else
      begin
        GetMultiLineComment(Stream);
        Result := Tokenizer.NextToken;
      end;
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-1, soFromCurrent);
      CheckExceptionState;
      Result := ExceptionState.GetNextToken(Stream, FirstChar, Tokenizer);
    end;
  end
  else
  begin
    CheckExceptionState;
    Result := ExceptionState.GetNextToken(Stream, FirstChar, Tokenizer);
  end;
end;

{ TZPostgreSQLSymbolState }

{**
  Creates this PostgreSQL-specific symbol state object.
}
constructor TZPostgreSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZPostgreSQLWordState }

{**
  Constructs this PostgreSQL-specific word state object.
}
constructor TZPostgreSQLWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars(Char($c0), Char($ff), True);
end;

{ TZPostgreSQLTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZPostgreSQLTokenizer.Create;
begin
  inherited Create;

  SymbolState := TZPostgreSQLSymbolState.Create;
  NumberState := TZPostgreSQLNumberState.Create;
  NumberState.SetExceptionState(SymbolState);
  NumberState.SetSeparation(True);
  QuoteState := TZPostgreSQLQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  WhitespaceState.SetVisible(False);
  WordState := TZPostgreSQLWordState.Create;
  CommentState := TZPostgreSQLCommentState.Create;
  CommentState.SetExceptionState(SymbolState);
  CommentState.SetVisible(False);

  SetCharacterState(#0, #255, SymbolState);
  SetCharacterState(#0, ' ', WhitespaceState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState(Chr($c0),  Chr($ff), WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{**
  Constructs a tokenizer to read from the supplied string.
  @param Buffer the string to read from
}
constructor TZPostgreSQLTokenizer.CreateWithBuffer(Buffer: string);
begin
  Create;
  SetBuffer(Buffer);
end;

end.

