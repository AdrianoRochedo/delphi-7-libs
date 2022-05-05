{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for Interbase          }
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

unit ZInterbaseToken;

interface

uses
  Classes, ZClasses, ZParseIntfs, ZTokenizer, ZPostgreSqlToken;

type

  {** Implements a Interbase-specific number state object. }
  TZInterbaseNumberState = class (TZPostgreSQLNumberState)
  end;

  {** Implements a Interbase-specific quote string state object. }
  TZInterbaseQuoteState = class (TZPostgreSQLQuoteState)
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZInterbaseCommentState = class (TZCCommentState)
  end;

  {** Implements a symbol state object. }
  TZInterbaseSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZInterbaseWordState = class (TZWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZInterbaseTokenizer = class (TZTokenizer)
  public
    constructor Create;
    constructor CreateWithBuffer(Buffer: string);
  end;

implementation

{ TZInterbaseSymbolState }

{**
  Creates this Interbase-specific symbol state object.
}
constructor TZInterbaseSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
  Add('!<');
  Add('!>');
end;

{ TZInterbaseWordState }

{**
  Constructs this Interbase-specific word state object.
}
constructor TZInterbaseWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars('$', '$', True);
end;

{ TZInterbaseTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZInterbaseTokenizer.Create;
begin
  inherited Create;

  SymbolState := TZInterbaseSymbolState.Create;
  NumberState := TZInterbaseNumberState.Create;
  NumberState.SetExceptionState(SymbolState);
  NumberState.SetSeparation(True);
  QuoteState := TZInterbaseQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  WhitespaceState.SetVisible(False);
  WordState := TZInterbaseWordState.Create;
  CommentState := TZInterbaseCommentState.Create;
  CommentState.SetExceptionState(SymbolState);
  CommentState.SetVisible(False);

  SetCharacterState(#0, #255, SymbolState);
  SetCharacterState(#0, ' ', WhitespaceState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);

  SetCharacterState('/', '/', CommentState);
end;

{**
  Constructs a tokenizer to read from the supplied string.
  @param Buffer the string to read from
}
constructor TZInterbaseTokenizer.CreateWithBuffer(Buffer: string);
begin
  Create;
  SetBuffer(Buffer);
end;

end.

