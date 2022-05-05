{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                   Parsing interfaces                    }
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

unit ZParseIntfs;

interface

uses Classes, SysUtils, ZClasses;

type

  {**
    An assembly maintains a stream of language elements along
    with stack and target objects.

    Parsers use assemblers to record progress at
    recognizing language elements from assembly's string.
  }
  IZAssembly = interface (IZClonnable)
    ['{C07ACFB9-4D82-43A8-B61E-182CC2AA840B}']

    function DefaultDelimiter: string;
    function Consumed(Delimiter: string): string;
    function ElementsConsumed: Integer;
    function Remainder(Delimiter: string): string;
    function ElementsRemaining: Integer;
    function ElementsCount: Integer;
    function HasMoreElements: Boolean;
    function NextElement: string;
    procedure UngetElements(Number: Integer);

    function GetTarget: IZClonnable;
    procedure SetTarget(Target: IZClonnable);

    function GetStack: IZStack;
    function Peek: IZObject;
    function Pop: IZObject;
    procedure Push(Value: IZObject);
    function StackIsEmpty: Boolean;

    property Target: IZClonnable read GetTarget write SetTarget;
    property Stack: IZStack read GetStack;
  end;

  {**
    Parsers that have an Assembler ask it to work on an
    assembly after a successful match.
    <p>
    By default, terminals push their matches on a assembly's
    stack after a successful match.
    <p>
    Parsers recognize text, and assemblers provide any
    sort of work that should occur after this recognition.
    This work usually has to do with the state of the assembly,
    which is why assemblies have a stack and a target.
    Essentially, parsers trade advancement on a assembly
    for work on the assembly's stack or target.
  }
  IZAssembler = interface(IZObject)
    ['{5B001D64-F7B2-4E4D-AB4F-EFB69AA38DF4}']
    procedure WorkOn(Assembly: IZAssembly);
  end;

  IZParserVisitor = interface;

  {**
   * A <code>Parser</code> is an object that recognizes the
   * elements of a language.
   * <p>
   * Each <code>Parser</code> object is either a <code>
   * Terminal</code> or a composition of other parsers.
   * The <code>Terminal</code> class is a subclass of <code>
   * Parser</code>, and is itself a hierarchy of
   * parsers that recognize specific patterns of text. For
   * example, a <code>Word</code> recognizes any word, and a
   * <code>Literal</code> matches a specific string.
   * <p>
   * In addition to <code>Terminal</code>, other subclasses of
   * <code>Parser</code> provide composite parsers,
   * describing sequences, alternations, and repetitions of
   * other parsers. For example, the following <code>
   * Parser</code> objects culminate in a <code>good
   * </code> parser that recognizes a description of good
   * coffee.
   *
   * <blockquote><pre>
   *     Alternation adjective = new Alternation();
   *     adjective.add(new Literal("steaming"));
   *     adjective.add(new Literal("hot"));
   *     Sequence good = new Sequence();
   *     good.add(new Repetition(adjective));
   *     good.add(new Literal("coffee"));
   *     String s = "hot hot steaming hot coffee";
   *     Assembly a = new TokenAssembly(s);
   *     System.out.println(good.bestMatch(a));
   * </pre></blockquote>
   *
   * This prints out:
   *
   * <blockquote><pre>
   *     [hot, hot, steaming, hot, coffee]
   *     hot/hot/steaming/hot/coffee^
   * </pre></blockquote>
   *
   * The parser does not match directly against a string,
   * it matches against an <code>Assembly</code>.  The
   * resulting assembly shows its stack, with four words on it,
   * along with its sequence of tokens, and the index at the
   * end of these. In practice, parsers will do some work
   * on an assembly, based on the text they recognize.
  }
  IZParser = interface (IZObject)
    ['{154114F0-957F-49B5-952D-09B893F7DD4C}']

    procedure Accept(Visitor: IZParserVisitor);
    procedure AcceptWithVisited(Visitor: IZParserVisitor; Visited: IZCollection);

    function Best(Vector: IZCollection): IZAssembly;
    function BestMatch(Assembly: IZAssembly): IZAssembly;
    function CompleteMatch(Assembly: IZAssembly): IZAssembly;
    function GetName: string;
    function Match(InVector: IZCollection): IZCollection;
    function MatchAndAssemble(InVector: IZCollection): IZCollection;
    function SetAssembler(Value: IZAssembler): IZParser;

    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
    function RandomInput(MaxDepth: Integer; Separator: string): string;
    function VisitedToString(Visited: IZCollection): string;
  end;

  {**
    An <code>Empty</code> parser matches any assembly once,
    and applies its assembler that one time.
    <p>
    Language elements often contain empty parts. For example,
    a language may at some point allow a list of parameters
    in parentheses, and may allow an empty list. An empty
    parser makes it easy to match, within the
    parenthesis, either a list of parameters or "empty".
  }
  IZEmpty = interface (IZParser)
    ['{42B5C932-529A-48E8-A04A-DBF427230C4D}']
  end;

  {**
    This class abstracts the behavior common to parsers
    that consist of a series of other parsers.
  }
  IZCollectionParser = interface (IZParser)
    ['{4400D4F1-25F8-4E90-8431-963DF245E109}']

    function Add(Parser: IZParser): IZCollectionParser;
    function GetSubParsers: IZCollection;
  end;

  {**
    An <code>Alternation</code> object is a collection of parsers,
    any one of which can successfully match against an assembly.
  }
  IZAlternation = interface(IZCollectionParser)
    ['{371EF0EB-B1F8-42C8-A1B1-262715BAD5EF}']
  end;

  {**
    A <code>Repetition</code> matches its underlying parser
    repeatedly against a assembly.
  }
  IZRepetition = interface(IZParser)
    ['{371EF0EB-B1F8-42C8-A1B1-262715BAD5EF}']

    function GetSubParser: IZParser;
    function SetPreAssembler(PreAssembler: IZAssembler): IZParser;

    function GetMinCount: Integer;
    procedure SetMinCount(Value: Integer);
    function GetMaxCount: Integer;
    procedure SetMaxCount(Value: Integer);
  end;

  {**
    A <code>Sequence</code> object is a collection of
    parsers, all of which must in turn match against an
    assembly for this parser to successfully match.
  }
  IZSequence = interface (IZCollectionParser)
    ['{6F09B6AF-B7FB-41CE-883D-16ED03DB9B15}']
  end;

  {**
    A <code>Terminal</code> is a parser that is not a
    composition of other parsers. Terminals are "terminal"
    because they do not pass matching work on to other
    parsers. The criterion that terminals use to check a
    match is something other than another parser. Terminals
    are also the only parsers that advance an assembly.
  }
  IZTerminal = interface (IZParser)
    ['{DF49C083-F10C-454F-8E0A-C5A2DF9C24DB}']

    function Discard: IZTerminal;
    function SetDiscard(Value: Boolean): IZTerminal;
  end;

  {**
    An <code>Invertion</code> matches all items until assembly accepted by
    underlying parser.
  }
  IZInvertion = interface(IZParser)
    ['{7756DC0A-694D-4E53-A65D-22067DC65C20}']

    function GetSubParser: IZParser;
    function SetPreAssembler(PreAssembler: IZAssembler): IZParser;
    function Discard: IZInvertion;
    function SetDiscard(Value: Boolean): IZInvertion;
  end;

  {**
    This class provides a "visitor" hierarchy in support of
    the Visitor pattern -- see the book, "Design Patterns" for
    an explanation of this pattern.
  }
  IZParserVisitor = interface (IZInterface)
    ['{9273C6E9-E477-4454-9D4A-324F2819110D}']

    procedure VisitEmpty(Empty: IZEmpty; Visited: IZCollection);
    procedure VisitAlternation(Alternation: IZAlternation;
      Visited: IZCollection);
    procedure VisitRepetition(Repetition: IZRepetition; Visited: IZCollection);
    procedure VisitSequence(Sequence: IZSequence; Visited: IZCollection);
    procedure VisitTerminal(Terminal: IZTerminal; Visited: IZCollection);
    procedure VisitInvertion(Invertion: IZInvertion; Visited: IZCollection);
  end;

//======================================================================
// Token processing interfaces
//======================================================================

type

  {** Defines an exception during tokenizing. }
  EZTokenizingError = class (Exception);

  {**
    Objects of this class represent a type of token,
    such as "number", "symbol" or "word".
  }
  TZTokenType = string;

const
  {** A constant indicating that the end of the stream has been read. }
  TT_EOF = 'eof';

  {** A constant indicating that a token is a number, like 3.14 }
  TT_NUMBER = 'number';

  {** A float constant indicating that a token is a number, like 3.14 }
  TT_FLOAT = 'float';

  {** An integer constant indicating that a token is a number, like 123 }
  TT_INTEGER = 'integer';

  {** A hexdecimal constant indicating that a token is a number, like 0xFF }
  TT_HEXDECIMAL = 'hexdecimal';

  {** A constant indicating that a token is a symbol like "<=". }
  TT_SYMBOL = 'symbol';

  {** A constant indicating that a token is a quoted string, like "Launch Mi". }
  TT_QUOTED = 'quoted';

  {** A constant indicating a token is a word, like "cat" }
  TT_WORD = 'word';

  {** A constant indicating a token is a keyword, like "while" }
  TT_KEYWORD = 'keyword';

  {** A constant indicating a token is a whitespace, like " \r\n\t" }
  TT_WHITESPACE = 'whitespace';

  {** A constant indicating a token is a comment, like "/* comment */" }
  TT_COMMENT = 'comment';

type

  {**
    A token represents a logical chunk of a string. For
    example, a typical tokenizer would break the string
    <code>"1.23 <= 12.3"</code> into three tokens: the number
    1.23, a less-than-or-equal symbol, and the number 12.3. A
    token is a receptacle, and relies on a tokenizer to decide
    precisely how to divide a string into tokens.
  }
  IZToken = interface (IZClonnable)
    ['{01B94736-2F86-4441-BBCC-0FD60F10C852}']

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
  end;

  // Forward declaration
  IZTokenizer = interface;

  {**
    A tokenizerState returns a token, given a reader, an initial character
    read from the reader, and a tokenizer that is conducting an overall
    tokenization of the reader. The tokenizer will typically have a character
    state table that decides which state to use, depending on an initial
    character. If a single character is insufficient, a state such
    as <code>SlashState</code> will read a second character, and may delegate
    to another state, such as <code>SlashStarState</code>. This prospect
    of delegation is the reason that the <code>nextToken()</code> method has a
    tokenizer argument.
  }
  IZTokenizerState = interface (IZObject)
    ['{1C91B36A-535C-43E3-8FB6-6CD96BCA3541}']

    function GetNextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: IZTokenizer): IZToken;
  end;

  {**
    The idea of a symbol is a character that stands on its
    own, such as an ampersand or a parenthesis. For example,
    when tokenizing the expression <code>(isReady)&
    (isWilling) </code>, a typical tokenizer would return 7
    tokens, including one for each parenthesis and one for
    the ampersand. Thus a series of symbols such as
    <code>)&( </code> becomes three tokens, while a series
    of letters such as <code>isReady</code> becomes a single
    word token.
    <p>
    Multi-character symbols are an exception to the rule
    that a symbol is a standalone character.  For example, a
    tokenizer may want less-than-or-equals to tokenize as a
    single token. This class provides a method for
    establishing which multi-character symbols an object of
    this class should treat as single symbols. This allows,
    for example, <code>"cat <= dog"</code> to tokenize as
    three tokens, rather than splitting the less-than and
    equals symbols into separate tokens.
    <p>
    By default, this state recognizes the following multi-
    character symbols: <code>!=, :-, <=, >=</code>
  }
  IZSymbolState = interface (IZTokenizerState)
    ['{5D26B3BC-AA78-4F9D-AE09-56CA4DC2CCDE}']
    procedure Add(Value: string);
  end;

  {**
    A CommentState object returns a comment from a reader.
  }
  IZCommentState = interface (IZTokenizerState)
    ['{561C5937-3434-4885-B01F-93FAD9373D66}']
    procedure SetVisible(Value: Boolean);
    procedure SetExceptionState(Value: IZTokenizerState);
  end;

  {**
    A NumberState object returns a number from a reader. This
    state's idea of a number allows an optional, initial
    minus sign, followed by one or more digits. A decimal
    point and another string of digits may follow these digits.
  }
  IZNumberState = interface (IZTokenizerState)
    ['{561C5937-3434-4885-B01F-93FAD9373D66}']
    procedure SetExceptionState(Value: IZTokenizerState);
    procedure SetSeparation(Value: Boolean);
  end;

  {**
    A quoteState returns a quoted string token from a reader.
    This state will collect characters until it sees a match
    to the character that the tokenizer used to switch to
    this state. For example, if a tokenizer uses a double-
    quote character to enter this state, then <code>
    nextToken()</code> will search for another double-quote
    until it finds one or finds the end of the reader.
  }
  IZQuoteState = interface (IZTokenizerState)
    ['{895F3E4F-0D58-4DEB-B64A-B1110FD11E56}']
  end;

  {**
    A whitespace state ignores whitespace (such as blanks
    and tabs), and returns the tokenizer's next token. By
    default, all characters from 0 to 32 are whitespace.
  }
  IZWhitespaceState = interface (IZTokenizerState)
    ['{9348C1F1-AF47-4803-B906-0501CCD7D66B}']
    procedure SetVisible(Value: Boolean);
    procedure SetWhitespaceChars(FromChar, ToChar: Char; Enable: Boolean);
  end;

  {**
    A wordState returns a word from a reader. Like other
    states, a tokenizer transfers the job of reading to this
    state, depending on an initial character. Thus, the
    tokenizer decides which characters may begin a word, and
    this state determines which characters may appear as a
    second or later character in a word. These are typically
    different sets of characters; in particular, it is typical
    for digits to appear as parts of a word, but not as the
    initial character of a word.
    <p>
    By default, the following characters may appear in a word.
    The method <code>setWordChars()</code> allows customizing
    this.
    <blockquote><pre>
        From    To
         'a', 'z'
         'A', 'Z'
         '0', '9'

        as well as: minus sign, underscore, and apostrophe.
    </pre></blockquote>
  }
  IZWordState = interface (IZTokenizerState)
    ['{9C36B991-8767-402F-8908-7A1D0774FED6}']
    procedure SetWordChars(FromChar: Char; ToChar: Char; Enable: Boolean);
  end;

  {**
    A tokenizer divides a string into tokens. This class is
    highly customizable with regard to exactly how this division
    occurs, but it also has defaults that are suitable for many
    languages. This class assumes that the character values read
    from the string lie in the range 0-255. For example, the
    Unicode value of a capital A is 65, so
    <code> System.out.println((char)65); </code> prints out a
    capital A.
    <p>
    The behavior of a tokenizer depends on its character state
    table. This table is an array of 256 <code>TokenizerState
    </code>  states. The state table decides which state to
    enter upon reading a character from the input string.
    <p>
    For example, by default, upon reading an 'A', a tokenizer
    will enter a "word" state. This means the tokenizer will
    ask a <code>WordState</code> object to consume the 'A',
    along with the characters after the 'A' that form a word.
    The state's responsibility is to consume characters and
    return a complete token.
    <p>
    The default table sets a SymbolState for every character
    from 0 to 255, and then overrides this with:
    <blockquote><pre>
        From    To     State
          0     ' '    whitespaceState
         'a'    'z'    wordState
         'A'    'Z'    wordState
        160     255    wordState
         '0'    '9'    numberState
         '-'    '-'    numberState
         '.'    '.'    numberState
         '"'    '"'    quoteState
        '\''   '\''    quoteState
         '/'    '/'    slashState
    </pre></blockquote>
    In addition to allowing modification of the state table,
    this class makes each of the states above available. Some
    of these states are customizable. For example, wordState
    allows customization of what characters can be part of a
    word, after the first character.
  }
  IZTokenizer = interface (IZObject)
    ['{C7CF190B-C45B-4AB4-A406-5999643DF6A0}']

    function GetStream: TStream;
    procedure SetStream(Stream: TStream);
    procedure SetBuffer(Buffer: string);
    procedure SetBufferWithLength(Buffer: string; SymbolMax: Integer);

    function GetNextToken: IZToken;

    function GetDefaultCommentState: IZCommentState;
    function GetDefaultNumberState: IZNumberState;
    function GetDefaultQuoteState: IZQuoteState;
    function GetDefaultSymbolState: IZSymbolState;
    function GetDefaultWhitespaceState: IZWhitespaceState;
    function GetDefaultWordState: IZWordState;

    function GetCharacterState(StartChar: Char): IZTokenizerState;
    procedure SetCharacterState(FromChar, ToChar: Char; State: IZTokenizerState);

    property Stream: TStream read GetStream write SetStream;
    property NextToken: IZToken read GetNextToken;

    property DefaultCommentState: IZCommentState read GetDefaultCommentState;
    property DefaultNumberState: IZNumberState read GetDefaultNumberState;
    property DefaultQuoteState: IZQuoteState read GetDefaultQuoteState;
    property DefaultSymbolState: IZSymbolState read GetDefaultSymbolState;
    property DefaultWhitespaceState: IZWhitespaceState
      read GetDefaultWhitespaceState;
    property DefaultWordState: IZWordState read GetDefaultWordState;
  end;

  {**
    A TokenString is like a String, but it is a series of
    Tokens rather than a series of chars. Once a TokenString is
    created, it is "immutable", meaning it cannot change. This
    lets you freely copy TokenStrings without worrying about their state.
  }
  IZTokenString = interface (IZClonnable)
    ['{B1D3B319-1398-48CF-8C31-F40E325E1222}']

    function GetCount: Integer;
    function GetToken(Index: Integer): IZToken;

    property Count: Integer read GetCount;
    property Tokens[Index: Integer]: IZToken read GetToken; default;
  end;

implementation

end.
