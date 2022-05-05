{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           String tokenizing classes for MySQL           }
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

unit ZMySqlToken;

interface

uses
  Classes, ZClasses, ZParseIntfs, ZTokenizer, ZGenericSqlToken;

type

  {** Implements a MySQL-specific number state object. }
  TZMySQLNumberState = class (TZAbstractObject, IZNumberState)
  private
    FExceptionState: IZTokenizerState;
    FSeparation: Boolean;
    FHexDecimal: Boolean;
    FFloatPoint: Boolean;
    FValue: string;
    FLastChar: Char;
  protected
    procedure CheckExceptionState;
    function ReadHexDigits(Stream: TStream): string;
    function ReadDecDigits(Stream: TStream): string;

    property ExceptionState: IZTokenizerState read FExceptionState
      write FExceptionState;
    property Separation: Boolean read FSeparation write FSeparation;
    property HexDecimal: Boolean read FHexDecimal write FHexDecimal;
    property FloatPoint: Boolean read FFloatPoint write FFloatPoint;
    property Value: string read FValue write FValue;
    property LastChar: Char read FLastChar write FLastChar;
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
    procedure SetExceptionState(Value: IZTokenizerState);
    procedure SetSeparation(Value: Boolean);
  end;

  {** Implements a MySQL-specific quote string state object. }
  TZMySQLQuoteState = class (TZQuoteState)
  protected
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZMySQLCommentState = class (TZCppCommentState)
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {** Implements a symbol state object. }
  TZMySQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZMySQLWordState = class (TZWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZMySQLTokenizer = class (TZGenericSQLTokenizer)
  public
    constructor Create;
    constructor CreateWithBuffer(Buffer: string);
  end;

implementation

{ TZMySQLNumberState }

{**
  Checks does the exception state exist.
}
procedure TZMySQLNumberState.CheckExceptionState;
begin
  if FExceptionState = nil then
  begin
    raise EZTokenizingError.Create(
      'Exception state is not assigned to process exception char.');
  end;
end;

{**
  Sets an exception state which processes exception characters.
  @param Value an excepton state object.
}
procedure TZMySQLNumberState.SetExceptionState(Value: IZTokenizerState);
begin
  FExceptionState := Value;
end;

{**
  Sets a separation flag which divides integer from float numbers.
  @param Value <code>True</code> to separate integers from float numbers.
}
procedure TZMySQLNumberState.SetSeparation(Value: Boolean);
begin
  FSeparation := Value;
end;

{**
  Reads hexdecimal digits of the number.
  @return a string with number digits.
}
function TZMySQLNumberState.ReadHexDigits(Stream: TStream): string;
begin
  Result := '';
  FLastChar := #0;
  while Stream.Read(FLastChar, 1) > 0 do
  begin
    if FLastChar in ['0'..'9','a'..'f','A'..'F'] then
    begin
      Result := Result + FLastChar;
      FHexDecimal := FHexDecimal or (FLastChar in ['a'..'f','A'..'F']);
      FLastChar := #0;
    end
    else
    begin
      Stream.Seek(-1, soFromCurrent);
      Break;
    end;
  end;
end;

{**
  Reads decimal digits of the number.
  @return a string with number digits.
}
function TZMySQLNumberState.ReadDecDigits(Stream: TStream): string;
begin
  Result := '';
  FLastChar := #0;
  while Stream.Read(FLastChar, 1) > 0 do
  begin
    if FLastChar in ['0'..'9'] then
    begin
      Result := Result + FLastChar;
      FLastChar := #0;
    end
    else
    begin
      Stream.Seek(-1, soFromCurrent);
      Break;
    end;
  end;
end;

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZMySQLNumberState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
begin
  FHexDecimal := False;
  FFloatPoint := FirstChar = '.';
  FValue := FirstChar;
  FLastChar := #0;

  { Reads the first part of the number before decimal point }
  if not FFloatPoint then
  begin
    FValue := FValue + ReadDecDigits(Stream);
    FFloatPoint := (FLastChar = '.') and not FHexDecimal;
    if FFloatPoint then
    begin
      Stream.Read(FLastChar, 1);
      FValue := FValue + FLastChar;
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FFloatPoint then
    FValue := FValue + ReadDecDigits(Stream);

  { Reads a power part of the number }
  if not FHexDecimal and (FLastChar in ['e','E']) then
  begin
    Stream.Read(FLastChar, 1);
    FValue := FValue + FLastChar;
    FFloatPoint := True;

    Stream.Read(FLastChar, 1);
    if FLastChar in ['0'..'9','-','+'] then
      FValue := FValue + FLastChar + ReadDecDigits(Stream)
    else
    begin
      FValue := Copy(FValue, 1, Length(FValue) - 1);
      Stream.Seek(-2, soFromCurrent);
    end;
  end;

  { Reads the nexdecimal number }
  if (FValue = '0') and (FLastChar in ['x','X']) then
  begin
    Stream.Read(FLastChar, 1);
    FValue := FValue + FLastChar + ReadHexDigits(Stream);
    FHexDecimal := True;
  end;

  { Prepare the result }
  if FValue = '.' then
  begin
    CheckExceptionState;
    Result := FExceptionState.GetNextToken(Stream, FirstChar, Tokenizer);
  end
  else if not FSeparation then
    Result := TZToken.CreateWithType(TT_NUMBER, FValue)
  else if FHexDecimal then
    Result := TZToken.CreateWithType(TT_HEXDECIMAL, FValue)
  else if FFloatPoint then
    Result := TZToken.CreateWithType(TT_FLOAT, FValue)
  else Result := TZToken.CreateWithType(TT_INTEGER, FValue);
end;

{ TZMySQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZMySQLQuoteState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadChar, LastChar: Char;
  ReadStr: string;
begin
  ReadStr := FirstChar;
  LastChar := #0;
  while Stream.Read(ReadChar, 1) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then
    begin
      Stream.Seek(-1, soFromCurrent);
      Break;
    end;
    ReadStr := ReadStr + ReadChar;
    if LastChar = '\' then
      LastChar := #0
    else if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;

  Result := TZToken.CreateWithType(TT_QUOTED, ReadStr);
end;

{ TZMySQLCommentState }

{**
  Gets a MySQL specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZMySQLCommentState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  if FirstChar = '#' then
  begin
    if Visible then
    begin
      Result := TZToken.CreateWithType(TT_COMMENT,
        '#' + GetSingleLineComment(Stream));
    end
    else
    begin
      GetSingleLineComment(Stream);
      Result := Tokenizer.NextToken;
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
          '/*' + GetMultiLineComment(Stream))
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

{ TZMySQLSymbolState }

{**
  Creates this MySQL-specific symbol state object.
}
constructor TZMySQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZMySQLWordState }

{**
  Constructs this MySQL-specific word state object.
}
constructor TZMySQLWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
  SetWordChars(Char($c0), Char($ff), True);
end;

{ TZMySQLTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZMySQLTokenizer.Create;
begin
  inherited Create;

  SymbolState := TZMySQLSymbolState.Create;
  NumberState := TZMySQLNumberState.Create;
  NumberState.SetExceptionState(SymbolState);
  NumberState.SetSeparation(True);
  QuoteState := TZMySQLQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  WhitespaceState.SetVisible(False);
  WordState := TZMySQLWordState.Create;
  CommentState := TZMySQLCommentState.Create;
  CommentState.SetExceptionState(SymbolState);
  CommentState.SetVisible(False);

  SetCharacterState(#0, #255, SymbolState);
  SetCharacterState(#0, ' ', WhitespaceState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState(Chr($c0),  Chr($ff), WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('#', '#', CommentState);
end;

{**
  Constructs a tokenizer to read from the supplied string.
  @param Buffer the string to read from
}
constructor TZMySQLTokenizer.CreateWithBuffer(Buffer: string);
begin
  Create;
  SetBuffer(Buffer);
end;

end.

