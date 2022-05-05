{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes and interfaces         }
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

unit ZTokenizer;

interface

{$I ZParse.inc}

uses
  Classes, ZClasses, ZParseIntfs;

type

  {** Implements a token object. }
  TZToken = class (TZAbstractObject, IZToken, IZComparable)
  private
    FTokenType: TZTokenType;
    FValue: Variant;
  protected
    property TokenType: TZTokenType read FTokenType write FTokenType;
    property Value: Variant read FValue write FValue;
  public
    constructor Create(Value: Variant);
    constructor CreateWithType(TokenType: TZTokenType; Value: Variant);

    function Equals(const Value: IZInterface): Boolean; override;
    function EqualsIgnoreCase(const Value: IZInterface): Boolean;

    function IsNumber: Boolean;
    function IsQuotedString: Boolean;
    function IsSymbol: Boolean;
    function IsWord: Boolean;

    function GetValue: Variant;
    function GetString: string;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetTokenType: TZTokenType;

    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

  {** Implements a number state object. }
  TZNumberState = class (TZAbstractObject, IZNumberState)
  private
    FCharacter: Char;
    FReadNum: Integer;
    FValue: Variant;
    FAbsorbedLeadingMinus: Boolean;
    FAbsorbedDot: Boolean;
    FGotAdigit: Boolean;
    FExceptionState: IZTokenizerState;
    FSeparation: Boolean;
  protected
    procedure CheckExceptionState;
    function AbsorbDigits(Stream: TStream; Fraction: Boolean): Variant;
    procedure ParseLeft(Stream: TStream);
    procedure ParseRight(Stream: TStream);
    procedure Reset(FirstChar: Char);
    function GetValue(Stream: TStream; Tokenizer: IZTokenizer): IZToken;

    property Character: Char read FCharacter write FCharacter;
    property Value: Variant read FValue write FValue;
    property AbsorbedLeadingMinus: Boolean read FAbsorbedLeadingMinus
      write FAbsorbedLeadingMinus;
    property AbsorbedDot: Boolean read FAbsorbedDot write FAbsorbedDot;
    property GotAdigit: Boolean read FGotAdigit write FGotAdigit;
  public
    constructor Create;

    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
    procedure SetExceptionState(Value: IZTokenizerState);
    procedure SetSeparation(Value: Boolean);
  end;

  {** Implements a quote string state object. }
  TZQuoteState = class (TZAbstractObject, IZQuoteState)
  protected
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
  end;

  {** This state will either processes a basic comment-handling state. }
  TZBasicCommentState = class (TZAbstractObject, IZCommentState)
  private
    FVisible: Boolean;
    FExceptionState: IZTokenizerState;
  protected
    procedure CheckExceptionState;

    property Visible: Boolean read FVisible write FVisible;
    property ExceptionState: IZTokenizerState read FExceptionState
      write FExceptionState;
  public
    constructor Create;

    procedure SetVisible(Value: Boolean);
    procedure SetExceptionState(Value: IZTokenizerState);
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCppCommentState = class (TZBasicCommentState)
  protected
    function GetMultiLineComment(Stream: TStream): string;
    function GetSingleLineComment(Stream: TStream): string;
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCCommentState = class (TZCppCommentState)
  public
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; override;
  end;

  {**
    A <code>SymbolNode</code> object is a member of a tree that
    contains all possible prefixes of allowable symbols. Multi-
    character symbols appear in a <code>SymbolNode</code> tree
    with one node for each character.

    For example, the symbol <code>=:~</code> will appear in a
    tree as three nodes. The first node contains an equals sign,
    and has a child; that child contains a colon and has a
    child; this third child contains a tilde, and has no
    children of its own. If the colon node had another child
    for a dollar sign character, then the tree would contain
    the symbol <code>=:$</code>.

    A tree of <code>SymbolNode</code> objects collaborate to
    read a (potentially multi-character) symbol from an input
    stream. A root node with no character of its own finds an
    initial node that represents the first character in the
    input. This node looks to see if the next character in the
    stream matches one of its children. If so, the node
    delegates its reading task to its child. This approach
    walks down the tree, pulling symbols from the input that
    match the path down the tree.

    When a node does not have a child that matches the next
    character, we will have read the longest possible symbol
    prefix. This prefix may or may not be a valid symbol.
    Consider a tree that has had <code>=:~</code> added and has
    not had <code>=:</code> added. In this tree, of the three
    nodes that contain <code>=:~</code>, only the first and
    third contain complete symbols. If, say, the input contains
    <code>=:a</code>, the colon node will not have a child that
    matches the 'a' and so it will stop reading. The colon node
    has to "unread": it must push back its character, and ask
    its parent to unread. Unreading continues until it reaches
    an ancestor that represents a valid symbol.
  }
  IZSymbolNode = interface (IZObject)
    ['{44B0D690-22FD-4AA6-A737-FFBEDDD1B9B9}']
    function GetCharacter: Char;
    function Ancestry: string;
    procedure AddDescendantLine(Value: string);
    function DeepestRead(Stream: TStream): IZSymbolNode;
    function FindDescendant(Value: string): IZSymbolNode;
    procedure SetValid(Value: Boolean);
    function UnreadToValid(Stream: TStream): IZSymbolNode;
  end;

  {** Implements a SymbolNode object. }
  TZSymbolNode = class (TZAbstractObject, IZSymbolNode)
  private
    FCharacter: Char;
    FChildren: IZCollection;
    FValid: Boolean;
    FParent: TZSymbolNode;
  protected
    procedure AddDescendantLine(Value: string);
    function DeepestRead(Stream: TStream): IZSymbolNode;
    function EnsureChildWithChar(Value: Char): IZSymbolNode;
    function FindChildWithChar(Value: Char): IZSymbolNode; virtual;
    function FindDescendant(Value: string): IZSymbolNode;
    procedure SetValid(Value: Boolean);
    function UnreadToValid(Stream: TStream): IZSymbolNode;

    property Character: Char read FCharacter write FCharacter;
    property Children: IZCollection read FChildren write FChildren;
    property Valid: Boolean read FValid write FValid;
    property Parent: TZSymbolNode read FParent write FParent;
  public
    constructor Create(Parent: TZSymbolNode; Character: Char);

    function GetCharacter: Char;
    function Ancestry: string; virtual;
    function ToString: string; override;
  end;

  {**
    This class is a special case of a <code>SymbolNode</code>. A
    <code>SymbolRootNode</code> object has no symbol of its
    own, but has children that represent all possible symbols.
  }
  IZSymbolRootNode = interface (IZSymbolNode)
    ['{D634C956-1774-426B-930E-C150642E6611}']

    procedure Add(Value: string);
    function NextSymbol(Stream: TStream; FirstChar: Char): string;
  end;

  {** Implements a Root symbol node object. }
  TZSymbolRootNode = class (TZSymbolNode, IZSymbolRootNode)
  private
    FChildrenMatch: array[0..255] of IZSymbolNode;
  protected
    procedure Init;
    function FindChildWithChar(Value: Char): IZSymbolNode; override;
  public
    constructor Create;

    procedure Add(Value: string);
    function Ancestry: string; override;
    function NextSymbol(Stream: TStream; FirstChar: Char): string;
  end;

  {** Implements a symbol state object. }
  TZSymbolState = class (TZAbstractObject, IZSymbolState)
  private
    FSymbols: IZSymbolRootNode;
  protected
    property Symbols: IZSymbolRootNode read FSymbols write FSymbols;
  public
    constructor Create;
    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
    procedure Add(Value: string); virtual;
  end;

  {** Implements a whitespace state object. }
  TZWhitespaceState = class (TZAbstractObject, IZWhitespaceState)
  private
    FWhitespaceChars: array[0..255] of Boolean;
    FVisible: Boolean;
  public
    constructor Create;

    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
    procedure SetVisible(Value: Boolean);
    procedure SetWhitespaceChars(FromChar: Char; ToChar: Char; Enable: Boolean);
  end;

  {** Implements a word state object. }
  TZWordState = class (TZAbstractObject, IZWordState)
  private
    FWordChars: array[0..255] of Boolean;
  public
    constructor Create;

    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken; virtual;
    procedure SetWordChars(FromChar: Char; ToChar: Char; Enable: Boolean);
  end;

  {** Implements a default tokenizer object. }
  TZTokenizer = class (TZAbstractObject, IZTokenizer)
  private
    FStream: TStream;
    FCharacterStates: array[0..255] of IZTokenizerState;
    FCommentState: IZCommentState;
    FNumberState: IZNumberState;
    FQuoteState: IZQuoteState;
    FSymbolState: IZSymbolState;
    FWhitespaceState: IZWhitespaceState;
    FWordState: IZWordState;
  protected
    property Stream: TStream read FStream write FStream;
    property CommentState: IZCommentState read FCommentState write FCommentState;
    property NumberState: IZNumberState read FNumberState write FNumberState;
    property QuoteState: IZQuoteState read FQuoteState write FQuoteState;
    property SymbolState: IZSymbolState read FSymbolState write FSymbolState;
    property WhitespaceState: IZWhitespaceState read FWhitespaceState
      write FWhitespaceState;
    property WordState: IZWordState read FWordState write FWordState;
  public
    constructor Create;
    constructor CreateWithBuffer(Buffer: string);
    destructor Destroy; override;

    function GetStream: TStream;
    procedure SetStream(Stream: TStream);
    procedure SetBuffer(Buffer: string);
    procedure SetBufferWithLength(Buffer: string; SymbolMax: Integer);

    function GetNextToken: IZToken;

    function GetDefaultCommentState: IZCommentState;
    function GetDefaultNumberState: IZNumberState;
    function GetDefaultQuoteState: IZQuoteState;
    function GetDefaultSymbolState: IZSymbolState;
    function GetDefaultWhiteSpaceState: IZWhitespaceState;
    function GetDefaultWordState: IZWordState;

    function GetCharacterState(StartChar: Char): IZTokenizerState;
    procedure SetCharacterState(FromChar, ToChar: Char; State: IZTokenizerState);
  end;

  {** Implements a token string object. }
  TZTokenString = class (TZAbstractObject, IZTokenString)
  private
    FTokens: IZCollection;
  public
    constructor CreateWithTokens(Tokens: IZCollection);
    constructor CreateWithBuffer(Buffer: string);
    constructor CreateWithTokenizer(Tokenizer: IZTokenizer);

    function GetCount: Integer;
    function GetToken(Index: Integer): IZToken;

    function Clone: IZInterface; override;
    function ToString: string; override;

    property Count: Integer read GetCount;
    property Tokens[Index: Integer]: IZToken read GetToken; default;
  end;

  {**
    A TokenStringSource enumerates over a specified reader,
    returning TokenStrings delimited by a specified delimiter.
    <p>
    For example,
    <blockquote><pre>
       String s = "I came; I saw; I left in peace;";

       TokenStringSource tss =
           new TokenStringSource(new Tokenizer(s), ";");

       while (tss.hasMoreTokenStrings()) (
           System.out.println(tss.nextTokenString());
       )

    </pre></blockquote>
    prints out:
    <blockquote><pre>
        I came
        I saw
        I left in peace
    </pre></blockquote>
  }
  TZTokenStringSource = class (TZAbstractObject)
  private
    FTokenizer: IZTokenizer;
    FDelimiter: string;
    FCachedTokenString: IZTokenString;
  protected
    procedure EnsureCacheIsLoaded;
    procedure LoadCache;
    function NextVector: IZCollection;
  public
    constructor Create(Tokenizer: IZTokenizer; Delimiter: string);

    function HasMoreTokenStrings: Boolean;
    function NextTokenString: IZTokenString;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  SysUtils, ZCompatibility, Math, ZCollections;

var
  {** A constant indicating that there are no more tokens }
  TOKEN_EOF: IZToken;

{ TZToken }

{**
  Constructs this object and assignes the main properties.
  @param Value a token value
}
constructor TZToken.Create(Value: Variant);
begin
  FValue := Value;
  if VarIsNumeric(Value) then
    FTokenType := TT_NUMBER
  else if VarIsStr(Value) and (Length(VarToStrDef(Value, '')) = 1) then
    FTokenType := TT_SYMBOL
  else FTokenType := TT_WORD;
end;

{**
  Constructs this object and assignes the main properties.
  @param TokenType a token type.
  @param Value a token value
}
constructor TZToken.CreateWithType(TokenType: TZTokenType; Value: Variant);
begin
  FTokenType := TokenType;
  FValue := Value;
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZToken.Clone: IZInterface;
begin
  Result := TZToken.CreateWithType(FTokenType, FValue);
end;

{**
  Returns true if the supplied object is an equivalent token.
  @param Value the object to compare
  @return true, if the supplied object is of the same type and value
}
function TZToken.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZToken;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZToken, Temp) = 0 then
    begin
      Result := (FTokenType = Temp.GetTokenType) and (FValue = Temp.GetValue);
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Returns true if the supplied object is an equivalent token,
  given mellowness about case in strings and characters.

  @param Value the object to compare
  @return true, if the supplied object is of the same type and value.
    This method disregards case when comparing the string value of tokens.
}
function TZToken.EqualsIgnoreCase(const Value: IZInterface): Boolean;
var
  Temp: IZToken;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZToken, Temp) = 0 then
    begin
      if FTokenType <> Temp.GetTokenType then
        Result := False
      else if IsNumber then
        Result := FValue = Temp.GetValue
      else
        Result := CompareText(GetString, Temp.GetString) = 0;
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Returns the double value of this token.
  @return the double value of this token
}
function TZToken.GetFloat: Extended;
begin
  try
    Result := VarAsType(FValue, varDouble);
  except
    Result := 0;
  end;
end;

{**
  Returns the integer value of this token.
  @return the integer value of this token
}
function TZToken.GetInteger: Int64;
begin
  try
{$IFDEF VER130BELOW}
    Result := Integer(VarAsType(FValue, varInteger));
{$ELSE}
    Result := VarAsType(FValue, varInt64);
{$ENDIF}
  except
    Result := 0;
  end;
end;

{**
  Returns the string value of this token.
  @return the string value of this token
}
function TZToken.GetString: string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  Result := VarToStrDef(FValue, '');
  DecimalSeparator := OldDecimalSeparator;
end;

{**
  Returns the type of this token.
  @return   the type of this token, typically one
    of the constants this class defines
}
function TZToken.GetTokenType: TZTokenType;
begin
  Result := FTokenType;
end;

{**
  Returns an object that represents the value of this token.
  @return  an object that represents the value of this token
}
function TZToken.GetValue: Variant;
begin
  Result := FValue;
end;

{**
  Returns true if this token is a number.
  @return   true, if this token is a number
}
function TZToken.IsNumber: Boolean;
begin
  Result := (FTokenType = TT_NUMBER) or (FTokenType = TT_INTEGER)
    or (FTokenType = TT_FLOAT);
end;

{**
  Returns true if this token is a quoted string.
  @return   true, if this token is a quoted string
}
function TZToken.IsQuotedString: Boolean;
begin
  Result := FTokenType = TT_QUOTED;
end;

{**
  Returns true if this token is a symbol.
  @return   true, if this token is a symbol
}
function TZToken.IsSymbol: Boolean;
begin
  Result := FTokenType = TT_SYMBOL;
end;

{**
  Returns true if this token is a word.
  @return   true, if this token is a word.
}
function TZToken.IsWord: Boolean;
begin
  Result := (FTokenType = TT_WORD) or (FTokenType = TT_KEYWORD);
end;

{**
  Return a textual description of this object.
  @return a textual description of this object
}
function TZToken.ToString: string;
begin
  if FTokenType = TT_EOF then
    Result := 'EOF'
  else Result := GetString;
end;

{ TZNumberState }

{**
  Constructs this number state and assignes the main properties.
}
constructor TZNumberState.Create;
begin
  FExceptionState := nil;
  FSeparation := False;
end;

{**
  Checks does the exception state exist.
}
procedure TZNumberState.CheckExceptionState;
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
procedure TZNumberState.SetExceptionState(Value: IZTokenizerState);
begin
  FExceptionState := Value;
end;

{**
  Sets a separation flag which divides integer from float numbers.
  @param Value <code>True</code> to separate integers from float numbers.
}
procedure TZNumberState.SetSeparation(Value: Boolean);
begin
  FSeparation := Value;
end;

{**
  Convert a stream of digits into a number, making this
  number a fraction if the boolean parameter is true.
}
function TZNumberState.AbsorbDigits(Stream: TStream; Fraction: Boolean): Variant;
var
  DivideBy: Int64;
  Temp: Int64;
begin
  DivideBy := 1;
  Temp := 0;
  while (FCharacter >= '0') and (FCharacter <= '9') do
  begin
    FGotAdigit := True;
    Temp := Temp * 10 + (Ord(FCharacter) - Ord('0'));
    FReadNum := Stream.Read(FCharacter, 1);
    if Fraction then
      DivideBy := DivideBy * 10;
    if FReadNum = 0 then
      Break;
  end;
  if Fraction then
    Result := Extended(Temp / DivideBy)
{$IFDEF VER130BELOW}
  else Result := Integer(Temp);
{$ELSE}
  else Result := Temp;
{$ENDIF}
end;

{**
  Put together the pieces of a number.
}
function TZNumberState.GetValue(Stream: TStream;
  Tokenizer: IZTokenizer): IZToken;
begin
  Result := nil;
  if not FGotAdigit then
  begin
    CheckExceptionState;
    if FAbsorbedLeadingMinus and FAbsorbedDot then
    begin
      Stream.Seek(-1, soFromCurrent);
      Result := FExceptionState.GetNextToken(Stream, '-', Tokenizer);
    end
    else if FAbsorbedLeadingMinus then
      Result := FExceptionState.GetNextToken(Stream, '-', Tokenizer)
    else if FAbsorbedDot then
      Result := FExceptionState.GetNextToken(Stream, '.', Tokenizer);
  end;

  if Result = nil then
  begin
    if FAbsorbedLeadingMinus then
      FValue := -FValue;
    if FSeparation then
    begin
      if FAbsorbedDot then
        Result := TZToken.CreateWithType(TT_FLOAT, FValue)
      else Result := TZToken.CreateWithType(TT_INTEGER, FValue);
    end else
      Result := TZToken.CreateWithType(TT_NUMBER, FValue);
  end;
end;

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZNumberState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
begin
  Reset(FirstChar);
  ParseLeft(Stream);
  ParseRight(Stream);
  Stream.Seek(-FReadNum, soFromCurrent);
  Result := GetValue(Stream, Tokenizer);
end;

{**
  Parse up to a decimal point.
}
procedure TZNumberState.ParseLeft(Stream: TStream);
begin
  if FCharacter = '-' then
  begin
    FReadNum := Stream.Read(FCharacter, 1);
    FAbsorbedLeadingMinus := True;
  end;
  FValue := AbsorbDigits(Stream, False);
end;

{**
  Parse from a decimal point to the end of the number.
}
procedure TZNumberState.ParseRight(Stream: TStream);
begin
  if FCharacter = '.' then
  begin
    FAbsorbedDot := True;
    FReadNum := Stream.Read(FCharacter, 1);
    if FReadNum > 0 then
      FValue := Extended(FValue) + AbsorbDigits(Stream, True);
  end;
end;

{**
  Prepare to assemble a new number.
}
procedure TZNumberState.Reset(FirstChar: Char);
begin
  FCharacter := FirstChar;
  FValue := 0;
  FAbsorbedLeadingMinus := False;
  FAbsorbedDot := False;
  FGotAdigit := False;
end;

{ TZQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZQuoteState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  TempChar: Char;
  TempStr: string;
begin
  TempStr := FirstChar;
  repeat
    if Stream.Read(TempChar, 1) = 0 then
      TempChar := FirstChar;
    TempStr := TempStr + TempChar;
  until TempChar = FirstChar;

  Result := TZToken.CreateWithType(TT_QUOTED, TempStr);
end;

{ TZBasicCommentState }

{**
  Constructs this state and assignes the main properties.
}
constructor TZBasicCommentState.Create;
begin
  FVisible := False;
  FExceptionState := nil;
end;

{**
  Checks does the exception state exist.
}
procedure TZBasicCommentState.CheckExceptionState;
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
procedure TZBasicCommentState.SetExceptionState(Value: IZTokenizerState);
begin
  FExceptionState := Value;
end;

{**
  Sets does the whitespaces are visible as tokens.
  @param Value <code>True</code> to set whitespaces visible.
}
procedure TZBasicCommentState.SetVisible(Value: Boolean);
begin
  FVisible := Value;
end;

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZBasicCommentState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadChar: Char;
  ReadStr: string;
begin
  ReadStr := FirstChar;
  while (Stream.Read(ReadChar, 1) > 0) and not (ReadChar in [#10, #13]) do
  begin
    if FVisible then
      ReadStr := ReadStr + ReadChar;
  end;

  if ReadChar in [#10, #13] then
    Stream.Seek(-1, soFromCurrent);

  if FVisible then
   Result := TZToken.CreateWithType(TT_COMMENT, ReadStr)
  else Result := Tokenizer.NextToken;
end;

{ TZCppCommentState }

{**
  Ignore everything up to a closing star and slash, and
  then return the tokenizer's next token.
  @return the tokenizer's next token
}
function TZCppCommentState.GetMultiLineComment(Stream: TStream): string;
var
  ReadChar, LastChar: Char;
begin
  LastChar := #0;
  Result := '';
  while Stream.Read(ReadChar, 1) > 0 do
  begin
    if Visible then
      Result := Result + ReadChar;
    if (LastChar = '*') and (ReadChar = '/') then
      Break;
    LastChar := ReadChar;
  end;
end;

{**
  Ignore everything up to an end-of-line and return the tokenizer's next token.
  @return the tokenizer's next token
}
function TZCppCommentState.GetSingleLineComment(Stream: TStream): string;
var
  ReadChar: Char;
begin
  Result := '';
  while (Stream.Read(ReadChar, 1) > 0) and not (ReadChar in [#10, #13]) do
  begin
    if Visible then
      Result := Result + ReadChar;
  end;

  if ReadChar in [#10, #13] then
    Stream.Seek(-1, soFromCurrent);
end;

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCppCommentState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
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
  else if (ReadNum > 0) and (ReadChar = '/') then
  begin
    if Visible then
    begin
      Result := TZToken.CreateWithType(TT_COMMENT,
        '//' + GetSingleLineComment(Stream));
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
end;

{ TZCCommentState }

{**
  Gets a C specific comments like /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCCommentState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  if FirstChar = '/' then
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

{ TZSymbolNode }

{**
  Constructs a SymbolNode with the given parent, representing
  the given character.
  @param Parent this node's parent
  @param Character this node's character
}
constructor TZSymbolNode.Create(Parent: TZSymbolNode; Character: Char);
begin
  FParent := Parent;
  FCharacter := Character;
  FValid := False;
  FChildren := TZCollection.Create;
end;

{**
  Gets the associated character.
  @return the associated character. 
}
function TZSymbolNode.GetCharacter: Char;
begin
  Result := FCharacter;
end;

{**
  Add a line of descendants that represent the characters in the given string.
}
procedure TZSymbolNode.AddDescendantLine(Value: string);
var
  Node: IZSymbolNode;
begin
  if Length(Value) > 0 then
  begin
    Node := EnsureChildWithChar(Value[1]);
    Node.AddDescendantLine(Copy(Value, 2, Length(Value) - 1));
  end;
end;

{**
  Show the symbol this node represents.
  @return the symbol this node represents
}
function TZSymbolNode.Ancestry: string;
begin
  Result := FParent.Ancestry + FCharacter;
end;

{**
  Find the descendant that takes as many characters as possible from the input.
}
function TZSymbolNode.DeepestRead(Stream: TStream): IZSymbolNode;
var
  TempChar: Char;
  Node: IZSymbolNode;
  ReadNum: Integer;
begin
  ReadNum := Stream.Read(TempChar, 1);
  if ReadNum > 0 then
    Node := FindChildWithChar(TempChar)
  else Node := nil;

  if Node = nil then
  begin
    Stream.Seek(-ReadNum, soFromCurrent);
    Result := Self;
  end else
    Result := Node.DeepestRead(Stream);
end;

{**
  Find or create a child for the given character.
}
function TZSymbolNode.EnsureChildWithChar(Value: Char): IZSymbolNode;
begin
  Result := FindChildWithChar(Value);
  if Result = nil then
  begin
    Result := TZSymbolNode.Create(Self, Value);
    FChildren.Add(Result);
  end;
end;

{**
  Find a child with the given character.
}
function TZSymbolNode.FindChildWithChar(Value: Char): IZSymbolNode;
var
  I: Integer;
  Current: IZSymbolNode;
begin
  Result := nil;
  for I := 0 to FChildren.Count - 1 do
  begin
    Current := FChildren[I] as IZSymbolNode;
    if Current.GetCharacter = Value then
    begin
      Result := Current;
      Break;
    end;
  end;
end;

{**
  Find a descendant which is down the path the given string indicates.
}
function TZSymbolNode.FindDescendant(Value: string): IZSymbolNode;
var
  TempChar: Char;
begin
  if Length(Value) > 0 then
    TempChar := Value[1]
  else TempChar := #0;
  Result := FindChildWithChar(TempChar);
  if (Length(Value) > 1) and (Result <> nil) then
    Result := Result.FindDescendant(Copy(Value, 2, Length(Value) - 1));
end;

{**
  Mark this node as valid, which means its ancestry is a
  complete symbol, not just a prefix.
}
procedure TZSymbolNode.SetValid(Value: Boolean);
begin
  FValid := Value;
end;

{**
  Give a string representation of this node.
  @return a string representation of this node
}
function TZSymbolNode.ToString: string;
begin
  if FValid then
    Result := '' + FCharacter + '(True)'
  else Result := '' + FCharacter + '(False)';
end;

{**
  Unwind to a valid node; this node is "valid" if its
  ancestry represents a complete symbol. If this node is
  not valid, put back the character and ask the parent to unwind.
}
function TZSymbolNode.UnreadToValid(Stream: TStream): IZSymbolNode;
begin
  if not FValid then
  begin
    Stream.Seek(-1, soFromCurrent);
    Result := FParent.UnreadToValid(Stream);
  end else
    Result := Self;
end;

{ TZSymbolRootNode }

{**
  Create and initialize a root node.
}
constructor TZSymbolRootNode.Create;
begin
  inherited Create(nil, #0);
  Init;
end;

{**
  Add the given string as a symbol.
  @param   String   the character sequence to add
}
procedure TZSymbolRootNode.Add(Value: string);
var
  TempChar: Char;
  Node: IZSymbolNode;
begin
  if Length(Value) > 0 then
    TempChar := Value[1]
  else TempChar := #0;
  Node := EnsureChildWithChar(TempChar);
  Node.AddDescendantLine(Copy(Value, 2, Length(Value) - 1));
  FindDescendant(Value).SetValid(True);
end;

{**
  A root node has no parent and no character of its own, so its ancestry is "".
  @return an empty string
}
function TZSymbolRootNode.Ancestry: string;
begin
  Result := '';
end;

{**
  A root node maintains its children in an array instead of
  a Vector, to be faster.
}
function TZSymbolRootNode.FindChildWithChar(Value: Char): IZSymbolNode;
begin
  Result := FChildrenMatch[Ord(Value)];
end;

{**
  Set all possible symbols to be valid children. This means
  that the decision of which characters are valid one-
  character symbols lies outside this tree. If a tokenizer
  asks this tree to produce a symbol, this tree assumes that
  the first available character is a valid symbol.
}
procedure TZSymbolRootNode.Init;
var
  I: Integer;
begin
  for I := 0 to 255 do
  begin
    FChildrenMatch[I] := TZSymbolNode.Create(Self, Chr(I));
    FChildrenMatch[I].SetValid(True);
  end;
end;

{**
  Return a symbol string from a reader.

  @param Stream a reader to read from
  @param FirstChar the first character of this symbol, already
    read from the reader
  @return a symbol string from a reader
}
function TZSymbolRootNode.NextSymbol(Stream: TStream; FirstChar: Char): string;
var
  Node: IZSymbolNode;
begin
  Node := FindChildWithChar(FirstChar);
  Node := Node.DeepestRead(Stream);
  Node := Node.UnreadToValid(Stream);
  Result := Node.Ancestry;
end;

{ TZSymbolState }

{**
  Constructs a symbol state with a default idea of what
  multi-character symbols to accept (as described in the class comment).
}
constructor TZSymbolState.Create;
begin
  FSymbols := TZSymbolRootNode.Create;
end;

{**
  Add a multi-character symbol.
  @param Value the symbol to add, such as "=:="
}
procedure TZSymbolState.Add(Value: string);
begin
  FSymbols.Add(Value);
end;

{**
  Return a symbol token from a reader.
  @return a symbol token from a reader
}
function TZSymbolState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  TempStr: string;
begin
  TempStr := FSymbols.NextSymbol(Stream, FirstChar);
  Result := TZToken.CreateWithType(TT_SYMBOL, TempStr);
end;

{ TZWhitespaceState }

{**
  Constructs a whitespace state with a default idea of what
  characters are, in fact, whitespace.
}
constructor TZWhitespaceState.Create;
begin
  SetWhitespaceChars(' ', Chr(255), False);
  SetWhitespaceChars(Chr(0), ' ', True);
  SetVisible(False);
end;

{**
  Sets does the whitespaces are visible as tokens.
  @param Value <code>True</code> to set whitespaces visible.
}
procedure TZWhitespaceState.SetVisible(Value: Boolean);
begin
  FVisible := Value;
end;

{**
  Ignore whitespace (such as blanks and tabs), and return
  the tokenizer's next token.
  @return the tokenizer's next token
}
function TZWhitespaceState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  ReadNum: Integer;
  ReadChar: Char;
  ReadStr: string;
begin
  ReadStr := FirstChar;
  ReadNum := 0;
  while True do
  begin
    ReadNum := Stream.Read(ReadChar, 1);
    if (ReadNum = 0) or not FWhitespaceChars[Ord(ReadChar)] then
      Break;
    if FVisible then
      ReadStr := ReadStr + ReadChar;
  end;

  if ReadNum > 0 then
    Stream.Seek(-1, soFromCurrent);
  if FVisible then
    Result := TZToken.CreateWithType(TT_WHITESPACE, ReadStr)
  else Result := Tokenizer.NextToken;
end;

{**
  Establish the given characters as whitespace to ignore.
  @param FromChar first character index.
  @param ToChar last character index.
  @param Enable true, if this state should ignore characters in the given range
}
procedure TZWhitespaceState.SetWhitespaceChars(FromChar, ToChar: Char;
  Enable: Boolean);
var
  I: Integer;
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), 255]) do
    FWhitespaceChars[I] := Enable;
end;

{ TZWordState }

{**
  Constructs a word state with a default idea of what characters
  are admissible inside a word (as described in the class comment).
}
constructor TZWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('-', '-', True);
  SetWordChars('_', '_', True);
  SetWordChars(#39, #39, True);
  SetWordChars(Char($c0), Char($ff), True);
end;

{**
  Return a word token from a reader.
  @return a word token from a reader
}
function TZWordState.GetNextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: IZTokenizer): IZToken;
var
  TempChar: Char;
  ReadNum: Integer;
  Value: string;
begin
  Value := FirstChar;
  repeat
    ReadNum := Stream.Read(TempChar, 1);
    if (ReadNum = 0) or not FWordChars[Ord(TempChar)] then
      Break;
    Value := Value + TempChar;
  until False;

  if ReadNum > 0 then
    Stream.Seek(-1, soFromCurrent);
  Result := TZToken.CreateWithType(TT_WORD, Value);
end;

{**
  Establish characters in the given range as valid
  characters for part of a word after the first character.
  Note that the tokenizer must determine which characters
  are valid as the beginning character of a word.
  @param FromChar first character index.
  @param ToChar last character index.
  @param Enable true, if this state should ignore characters in the given range
}
procedure TZWordState.SetWordChars(FromChar, ToChar: Char; Enable: Boolean);
var
  I: Integer;
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), 255]) do
    FWordChars[I] := Enable;
end;

{ TZTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZTokenizer.Create;
begin
  FSymbolState := TZSymbolState.Create;
  with FSymbolState do
  begin
    Add('!=');
    Add(':-');
    Add('<=');
    Add('>=');
  end;

  FNumberState := TZNumberState.Create;
  FNumberState.SetExceptionState(FSymbolState);
  FQuoteState := TZQuoteState.Create;
  FWhitespaceState := TZWhitespaceState.Create;
  FWhitespaceState.SetVisible(False);
  FWordState := TZWordState.Create;
  FCommentState := TZCppCommentState.Create;
  FCommentState.SetExceptionState(FSymbolState);
  FCommentState.SetVisible(False);

  SetCharacterState(#0, #255, FSymbolState);

  SetCharacterState(#0, ' ', FWhitespaceState);
  SetCharacterState('a', 'z', FWordState);
  SetCharacterState('A', 'Z', FWordState);
  SetCharacterState(Chr($c0),  Chr($ff), FWordState);
  SetCharacterState('0', '9', FNumberState);
  SetCharacterState('-', '-', FNumberState);
  SetCharacterState('.', '.', FNumberState);
  SetCharacterState('"', '"', FQuoteState);
  SetCharacterState(#39, #39, FQuoteState);
  SetCharacterState('/', '/', FCommentState);
end;

{**
  Constructs a tokenizer to read from the supplied string.
  @param   String   the string to read from
}
constructor TZTokenizer.CreateWithBuffer(Buffer: string);
begin
  Create;
  SetBuffer(Buffer);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZTokenizer.Destroy;
begin
  if FStream <> nil then
    FStream.Free;
  inherited Destroy;
end;

{**
  Gets an initial state object for the specified character.
  @return an initial state object for the character.
}
function TZTokenizer.GetCharacterState(StartChar: Char): IZTokenizerState;
begin
  Result := FCharacterStates[Ord(StartChar)];
end;

{**
  Return the state this tokenizer uses to build numbers.
  @return  the state this tokenizer uses to build numbers
}
function TZTokenizer.GetDefaultNumberState: IZNumberState;
begin
  Result := FNumberState;
end;

{**
  Return the state this tokenizer uses to build quoted strings.
  @return the state this tokenizer uses to build quoted strings
}
function TZTokenizer.GetDefaultQuoteState: IZQuoteState;
begin
  Result := FQuoteState;
end;

{**
  Return the reader this tokenizer will read from.
  @return the reader this tokenizer will read from
}
function TZTokenizer.GetStream: TStream;
begin
  Result := FStream;
end;

{**
  Return the state this tokenizer uses to recognize comments.
  @return  the state this tokenizer uses to recognize comments
}
function TZTokenizer.GetDefaultCommentState: IZCommentState;
begin
  Result := FCommentState;
end;

{**
  Return the state this tokenizer uses to recognize symbols.
  @return  the state this tokenizer uses to recognize symbols
}
function TZTokenizer.GetDefaultSymbolState: IZSymbolState;
begin
  Result := FSymbolState;
end;

{**
  Return the state this tokenizer uses to recognize (and ignore) whitespace.
  @return the state this tokenizer uses to recognize whitespace
}
function TZTokenizer.GetDefaultWhiteSpaceState: IZWhitespaceState;
begin
  Result := FWhitespaceState;
end;

{**
  Return the state this tokenizer uses to build words.
  @return  the state this tokenizer uses to build words
}
function TZTokenizer.GetDefaultWordState: IZWordState;
begin
  Result := FWordState;
end;

{**
  Return the next token.
  @return the next token.
}
function TZTokenizer.GetNextToken: IZToken;
var
  TempChar: Char;
  ReadNum: Integer;
begin
  if FStream <> nil then
  begin
    ReadNum := FStream.Read(TempChar, 1);
    { There was a defect here, that resulted from the fact
      that unreading a -1 results in the next read having a
       value of (int)(char)-1, which is 65535. This may be
      a defect in PushbackReader. }

    if (ReadNum > 0) and (FCharacterStates[Ord(TempChar)] <> nil) then
    begin
      Result := FCharacterStates[Ord(TempChar)].
        GetNextToken(FStream, TempChar, Self)
    end else
      Result := TOKEN_EOF;
  end else
    Result := TOKEN_EOF;
end;

{**
  Change the state the tokenizer will enter upon reading
  any character between "from" and "to".

  @param FromChar first character index.
  @param ToChar last character index.
  @param State the state to enter upon reading a
    character between "fromChar" and "toChar"
}
procedure TZTokenizer.SetCharacterState(FromChar, ToChar: Char;
  State: IZTokenizerState);
var
  I: Integer;
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), 255]) do
    FCharacterStates[I] := State;
end;

{**
  Set the reader to read from.
  @param   PushbackReader   the reader to read from
}
procedure TZTokenizer.SetStream(Stream: TStream);
begin
  if FStream <> nil then
    FStream.Free;
  FStream := Stream;
end;

{**
  Set the string to read from.
  @param Buffer the string to read from
}
procedure TZTokenizer.SetBuffer(Buffer: string);
begin
  SetStream(TStringStream.Create(Buffer));
end;

{**
  Set the string to read from.
  @param Buffer the string to read from
  @param SymbolMax the maximum length of a symbol, which
    establishes the size of pushback buffer we need
}
procedure TZTokenizer.SetBufferWithLength(Buffer: string; SymbolMax: Integer);
begin
  SetStream(TStringStream.Create(Copy(Buffer, 1, SymbolMax)));
end;

{ TZTokenString }

{**
  Constructs a tokenString from the supplied string.
  @param Buffer the string to tokenize
}
constructor TZTokenString.CreateWithBuffer(Buffer: string);
begin
  CreateWithTokenizer(TZTokenizer.CreateWithBuffer(Buffer));
end;

{**
  Constructs a tokenString from the supplied tokens.
  @param tokens the tokens to use
}
constructor TZTokenString.CreateWithTokens(Tokens: IZCollection);
begin
  FTokens := TZCollection.Create;
  if Tokens <> nil then
    FTokens.AddAll(Tokens);
end;

{**
  Constructs a tokenString from the supplied reader and tokenizer.
  @param Tokenizer the tokenizer that will produces the tokens
}
constructor TZTokenString.CreateWithTokenizer(Tokenizer: IZTokenizer);
var
  Current: IZToken;
begin
  FTokens := TZCollection.Create;
  while True do
  begin
    Current := Tokenizer.NextToken;
    if Current.GetTokenType = TT_EOF then
      Break;
    FTokens.Add(Current);
  end;
end;

{**
  Return a copy of this object.
  @return a copy of this object
}
function TZTokenString.Clone: IZInterface;
var
  Tokens: IZCollection;
begin
  Tokens := TZCollection.Create;
  Tokens.AddAll(FTokens);
  Result := TZTokenString.CreateWithTokens(Tokens);
end;

{**
  Returns the number of tokens in this tokenString.
  @return   the number of tokens in this tokenString
}
function TZTokenString.GetCount: Integer;
begin
  Result := FTokens.Count;
end;

{**
  Returns the token at the specified index.
  @param index the index of the desired token
  @return token the token at the specified index
}
function TZTokenString.GetToken(Index: Integer): IZToken;
begin
  Result := FTokens[Index] as IZToken;
end;

{**
  Returns a string representation of this tokenString.
  @return   a string representation of this tokenString
}
function TZTokenString.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FTokens.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + (FTokens[I] as IZToken).ToString;
  end;
end;

{ TZTokenStringSource }

{**
  Constructs a TokenStringSource that will read TokenStrings
  using the specified tokenizer, delimited by the specified delimiter.

  @param tokenizer a tokenizer to read tokens from
  @param delimiter the character that fences off where one
    TokenString ends and the next begins
}
constructor TZTokenStringSource.Create(Tokenizer: IZTokenizer;
  Delimiter: string);
begin
  FTokenizer := Tokenizer;
  FDelimiter := Delimiter;
end;

{**
  The design of <code>nextTokenString</code> is that is
  always returns a cached value. This method will (at least
  attempt to) load the cache if the cache is empty.
}
procedure TZTokenStringSource.EnsureCacheIsLoaded;
begin
  if FCachedTokenString = nil then
    LoadCache;
end;

{**
  Returns true if the source has more TokenStrings.
  @return true, if the source has more TokenStrings that
    have not yet been popped with <code> nextTokenString</code>.
}
function TZTokenStringSource.HasMoreTokenStrings: Boolean;
begin
  EnsureCacheIsLoaded;
  Result := FCachedTokenString <> nil;
end;

{**
  Loads the next TokenString into the cache, or sets the
  cache to null if the source is out of tokens.
}
procedure TZTokenStringSource.LoadCache;
var
  Tokens: IZCollection;
begin
  Tokens := NextVector;
  if Tokens.Count > 0 then
    FCachedTokenString := TZTokenString.CreateWithTokens(Tokens)
  else FCachedTokenString := nil;
end;

{**
  Returns the next TokenString from the source.
  @return the next TokenString from the source
}
function TZTokenStringSource.NextTokenString: IZTokenString;
begin
  EnsureCacheIsLoaded;
  Result := FCachedTokenString;
  FCachedTokenString := nil;
end;

{**
  Returns a Vector of the tokens in the source up to either
  the delimiter or the end of the source.

  @return a Vector of the tokens in the source up to either
    the delimiter or the end of the source.
}
function TZTokenStringSource.NextVector: IZCollection;
var
  Current: IZToken;
begin
  Result := TZCollection.Create;
  while True do
  begin
    Current := FTokenizer.NextToken;
    if (Current.GetTokenType = TT_EOF) or (Current.GetString = FDelimiter) then
      Break;
    Result.Add(Current);
  end;
end;

initialization
  TOKEN_EOF := TZToken.CreateWithType(TT_EOF, Unassigned);
finalization
  TOKEN_EOF := nil;
end.

