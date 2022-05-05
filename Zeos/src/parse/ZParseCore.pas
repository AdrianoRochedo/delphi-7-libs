{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                    Parsing classes                      }
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

unit ZParseCore;

interface

uses Classes, SysUtils, ZClasses, ZParseIntfs;

type

  {** Implements an abstract assembly object. }
  TZAbstractAssembly = class (TZAbstractObject, IZAssembly)
  private
    FStack: IZStack;
    FTarget: IZClonnable;
    FElementIndex: Integer;
  protected
    constructor Create;
    function CloneProperties(Assembly: TZAbstractAssembly): IZInterface;

    property Stack: IZStack read FStack write FStack;
    property Target: IZClonnable read FTarget write FTarget;
    property ElementIndex: Integer read FElementIndex write FElementIndex;
  public
    destructor Destroy; override;

    function Clone: IZInterface; override; abstract;
    function ToString: string; override;

    function GetTarget: IZClonnable;
    procedure SetTarget(Target: IZClonnable);

    function DefaultDelimiter: string; virtual; abstract;
    function Consumed(Delimiter: string): string; virtual; abstract;
    function ElementsConsumed: Integer; virtual;
    function Remainder(Delimiter: string): string; virtual; abstract;
    function ElementsRemaining: Integer; virtual;
    function ElementsCount: Integer; virtual; abstract;
    function HasMoreElements: Boolean; virtual;
    function NextElement: string; virtual; abstract;
    procedure UngetElements(Number: Integer); virtual;

    function GetStack: IZStack;
    function Peek: IZObject; virtual; abstract;
    function Pop: IZObject;
    procedure Push(Value: IZObject);
    function StackIsEmpty: Boolean;
  end;

  {** Implements an abstract assembler object. }
  TZAbstractAssembler = class (TZAbstractObject, IZAssembler)
  protected
    class function ElementsAbove(Assembly: IZAssembly;
      Fence: IZObject): IZCollection;
    procedure WorkOn(Assembly: IZAssembly); virtual; abstract;
  end;

  {** Implements an abstract parser object. }
  TZAbstractParser = class (TZAbstractObject, IZParser)
  private
    FName: string;
    FAssembler: IZAssembler;
  protected
    constructor Create;
    constructor CreateWithName(Name: string);

    function AppendAssembly(InVector: IZCollection;
      Assembly: IZAssembly): Boolean;
    function AppendAssemblies(InVector: IZCollection;
      Assemblies: IZCollection): IZCollection;

    class function CloneElements(Vector: IZCollection): IZCollection;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      virtual; abstract;
    function UnvisitedString(Visited: IZCollection): string; virtual; abstract;

    property Name: string read FName write FName;
    property Assembler: IZAssembler read FAssembler write FAssembler;
  public
    destructor Destroy; override;

    procedure Accept(Visitor: IZParserVisitor);
    procedure AcceptWithVisited(Visitor: IZParserVisitor; Visited: IZCollection);
      virtual; abstract;

    function Best(Vector: IZCollection): IZAssembly;
    function BestMatch(Assembly: IZAssembly): IZAssembly;
    function CompleteMatch(Assembly: IZAssembly): IZAssembly;

    function GetName: string;
    function SetAssembler(Value: IZAssembler): IZParser;

    function Match(InVector: IZCollection): IZCollection; virtual; abstract;
    function MatchAndAssemble(InVector: IZCollection): IZCollection;

    function RandomInput(MaxDepth: Integer; Separator: string): string;
    function ToString: string; override;
    function VisitedToString(Visited: IZCollection): string;
  end;

  {** Implements an Empty parser class. }
  TZEmpty = class (TZAbstractParser, IZEmpty)
  protected
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
    function UnvisitedString(Visited: IZCollection): string; override;
  public
    constructor Create;
    constructor CreateWithName(Name: string);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;
  end;

  {** Implements an abstract collection parser class. }
  TZCollectionParser = class (TZAbstractParser, IZCollectionParser)
  private
    FSubParsers: IZCollection;
  protected
    constructor Create;
    constructor CreateWithName(Name: string);
    function UnvisitedString(Visited: IZCollection): string; override;
    function ToStringSeparator: string; virtual; abstract;

    property SubParsers: IZCollection read FSubParsers write FSubParsers;
  public
    function Add(Parser: IZParser): IZCollectionParser;
    function GetSubParsers: IZCollection;
  end;

  {** Implements an alternation parser class. }
  TZAlternation = class (TZCollectionParser, IZAlternation)
  protected
    function RandomSettle(MaxDepth: Integer; Depth: Integer): TStrings;
    function ToStringSeparator: string; override;

    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  public
    constructor Create;
    constructor CreateWithName(Name: string);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;
  end;

  {** Implements an repetition parser class. }
  TZRepetition = class (TZAbstractParser, IZRepetition)
  private
    FSubParser: IZParser;
    FPreAssembler: IZAssembler;
    FMinCount: Integer;
    FMaxCount: Integer;
  protected
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
    function UnvisitedString(Visited: IZCollection): string; override;

    property SubParser: IZParser read FSubParser write FSubParser;
    property PreAssembler: IZAssembler read FPreAssembler write FPreAssembler;
    property MinCount: Integer read FMinCount write FMinCount;
    property MaxCount: Integer read FMaxCount write FMaxCount;
  public
    constructor Create(SubParser: IZParser);
    constructor CreateWithLimits(SubParser: IZParser; MinCount, MaxCount: Integer);
    constructor CreateWithName(SubParser: IZParser; Name: string);
    constructor CreateWithNameAndLimits(SubParser: IZParser; Name: string;
      MinCount, MaxCount: Integer);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;

    function GetSubParser: IZParser;
    function SetPreAssembler(PreAssembler: IZAssembler): IZParser;

    function GetMinCount: Integer;
    procedure SetMinCount(Value: Integer);
    function GetMaxCount: Integer;
    procedure SetMaxCount(Value: Integer);
  end;

  {** Implements a sequence parser class. }
  TZSequence = class(TZCollectionParser, IZSequence)
  protected
    function ToStringSeparator: string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  public
    constructor Create;
    constructor CreateWithName(Name: string);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;
  end;

  {** Implements a terminal parser class. }
  TZTerminal = class (TZAbstractParser, IZTerminal)
  private
    FDiscard: Boolean;
  protected
    function MatchOneAssembly(Assembly: IZAssembly): IZAssembly;
    function Qualifies(Value: IZObject): Boolean; virtual;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
    function UnvisitedString(Visited: IZCollection): string; override;
  public
    constructor Create;
    constructor CreateWithName(Name: string);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;

    function Discard: IZTerminal;
    function SetDiscard(Value: Boolean): IZTerminal;
  end;

  {** Implements an invertion parser class. }
  TZInvertion = class (TZAbstractParser, IZInvertion)
  private
    FDiscard: Boolean;
    FSubParser: IZParser;
    FPreAssembler: IZAssembler;
  protected
    function MatchOneAssembly(Assembly: IZAssembly): IZAssembly;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
    function UnvisitedString(Visited: IZCollection): string; override;

    property SubParser: IZParser read FSubParser write FSubParser;
    property PreAssembler: IZAssembler read FPreAssembler write FPreAssembler;
  public
    constructor Create(SubParser: IZParser);
    constructor CreateWithName(SubParser: IZParser; Name: string);

    procedure AcceptWithVisited(Visitor: IZParserVisitor;
      Visited: IZCollection); override;
    function Match(InVector: IZCollection): IZCollection; override;

    function GetSubParser: IZParser;
    function SetPreAssembler(PreAssembler: IZAssembler): IZParser;
    function Discard: IZInvertion;
    function SetDiscard(Value: Boolean): IZInvertion;
  end;

  {** Implements an optimal invertion parser class. }
  TZFastInvertion = class (TZInvertion)
  public
    function Match(InVector: IZCollection): IZCollection; override;
  end;

  {** Implements an option parser class. }
  TZOption = class (TZAlternation)
  private
    FSubParser: IZParser;
  public
    constructor Create(SubParser: IZParser);
    constructor CreateWithName(SubParser: IZParser; Name: string);

    function GetSubParser: IZParser;
  end;

  {**
    This class generates random language elements for a
    parser and tests that the parser can accept them.
  }
  TZAbstractParserTester = class
  private
    FParser: IZParser;
    FLogTestStrings: Boolean;
  protected
    constructor Create(Parser: IZParser);
    function GetAssembly(Str: string): IZAssembly; virtual; abstract;
    function CanGenerateProblem(Depth: Integer): Boolean;
    function FreshTarget: IZClonnable; virtual;
    procedure LogDepthChange(Depth: Integer);
    procedure LogPassed;
    procedure LogProblemFound(Str: string; MatchSize: Integer);
    procedure LogTestString(Str: string);
    function GetSeparator: string; virtual;

    property Parser: IZParser read FParser write FParser;
    property LogTestStrings: Boolean read FLogTestStrings write FLogTestStrings;
  public
    class function CompleteMatches(InVector: IZCollection): IZCollection;
    procedure SetLogTestStrings(Value: Boolean);
    procedure Test;
  end;

implementation

uses ZCollections;

const
  { The width of a random expansion }
  EXPWIDTH = 4;

{ TZAbstractAssembly }

{**
  Constructs this object and assignes the main properties.
}
constructor TZAbstractAssembly.Create;
begin
  FStack := TZStack.Create;
  FElementIndex := 0;
  FTarget := nil;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractAssembly.Destroy;
begin
  FStack := nil;
  inherited Destroy;
end;

{**
  Clones the properties of this assembly.
  @param Assembly an object which receives clonned properties.
}
function TZAbstractAssembly.CloneProperties(Assembly: TZAbstractAssembly):
  IZInterface;
begin
  Assembly.Stack := Stack.Clone as IZStack;
  Assembly.ElementIndex := ElementIndex;
  if Target <> nil then
    Assembly.Target := Target.Clone as IZClonnable;
  Result := Assembly;
end;

{**
  Returns the number of elements that have been consumed.
  @return   the number of elements that have been consumed
}
function TZAbstractAssembly.ElementsConsumed: Integer;
begin
  Result := FElementIndex;
end;

{**
  Returns the number of elements that have not been consumed.
  @return the number of elements that have not been consumed.
}
function TZAbstractAssembly.ElementsRemaining: Integer;
begin
  Result := ElementsCount - ElementsConsumed;
end;

{**
  Removes this assembly's stack.
  @return this assembly's stack
}
function TZAbstractAssembly.GetStack: IZStack;
begin
  Result := FStack;
end;

{**
  Returns the object identified as this assembly's "target".
  Clients can set and retrieve a target, which can be a
  convenient supplement as a place to work, in addition to
  the assembly's stack. For example, a parser for an
  HTML file might use a web page object as its "target". As
  the parser recognizes markup commands like <head>, it
  could apply its findings to the target.

  @return   the target of this assembly
}
function TZAbstractAssembly.GetTarget: IZClonnable;
begin
  Result := FTarget;
end;

{**
  Returns true if this assembly has unconsumed elements.
  @return   true, if this assembly has unconsumed elements
}
function TZAbstractAssembly.HasMoreElements: Boolean;
begin
  Result := ElementsConsumed < ElementsCount;
end;

{**
  Removes the object at the top of this assembly's stack and
  returns it.
  @return   the object at the top of this assembly's stack
}
function TZAbstractAssembly.Pop: IZObject;
begin
  Result := FStack.Pop as IZObject;
end;

{**
  Pushes an object onto the top of this assembly's stack.
  @param Value the object to be pushed.
}
procedure TZAbstractAssembly.Push(Value: IZObject);
begin
  FStack.Push(Value);
end;

{**
  Sets the target for this assembly. Targets must implement
  <code>clone()</code> as a public method.
  @param   target   a publicly cloneable object
}
procedure TZAbstractAssembly.SetTarget(Target: IZClonnable);
begin
  FTarget := Target;
end;

{**
  Returns true if this assembly's stack is empty.
  @return true, if this assembly's stack is empty
}
function TZAbstractAssembly.StackIsEmpty: Boolean;
begin
  Result := FStack.Count = 0;
end;

{**
  Returns a textual description of this assembly.
  @return   a textual description of this assembly
}
function TZAbstractAssembly.ToString: string;
var
  Delimiter: string;
begin
  Delimiter := DefaultDelimiter;
  Result := Stack.ToString + Consumed(Delimiter) + '^' + Remainder(Delimiter);
end;

{**
  Put back number of objects.
  @param Number a number of elements to unget.
}
procedure TZAbstractAssembly.UngetElements(Number: Integer);
begin
  Dec(FElementIndex, Number);
  if FElementIndex < 0 then
    FElementIndex := 0;
end;

{ TZAbstractAssembler }

{**
  Returns a vector of the elements on an assembly's stack
  that appear before a specified fence.
  <p>
  Sometimes a parser will recognize a list from within
  a pair of parentheses or brackets. The parser can mark
  the beginning of the list with a fence, and then retrieve
  all the items that come after the fence with this method.

  @param  Assembly   a assembly whose stack should contain
    some number of items above a fence marker
  @param  Fence the fence, a marker of where to stop popping the stack
  @return   Vector   the elements above the specified fence
}
class function TZAbstractAssembler.ElementsAbove(Assembly: IZAssembly;
  Fence: IZObject): IZCollection;
var
  Top: IZInterface;
begin
  Result := TZCollection.Create;
  while not Assembly.StackIsEmpty do
  begin
    Top := Assembly.Pop;
    if Fence.Equals(Top) then
      Break;
    Result.Add(Top);
  end;
end;

{ TZAbstractParser }

{**
  Creates this object and assignes the main properties.
}
constructor TZAbstractParser.Create;
begin
  FName := '';
  FAssembler := nil;
end;

{**
  Creates this object and assignes the main properties.
  @param Name a parser name.
}
constructor TZAbstractParser.CreateWithName(Name: string);
begin
  FName := Name;
  FAssembler := nil;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractParser.Destroy;
begin
  inherited Destroy;
end;

{**
  Appends a list of assemblies to the specified vector.
  @param InVector a vector which contains assemblies.
  @param Assemblies a collection of assemblies to be added.
}
function TZAbstractParser.AppendAssemblies(
  InVector, Assemblies: IZCollection): IZCollection;
var
  I: Integer;
begin
  if Assemblies <> nil then
  begin
    Result := TZCollection.Create;
    for I := 0 to Assemblies.Count - 1 do
    begin
      if AppendAssembly(InVector, Assemblies[I] as IZAssembly) then
        Result.Add(Assemblies[I]);
    end;
  end else
    Result := nil;
end;

{**
  Appends one single assembly to the specified vector.
  @param InVector a vector which contains assemblies.
  @param Assembly an assembly to be added.
}
function TZAbstractParser.AppendAssembly(InVector: IZCollection;
  Assembly: IZAssembly): Boolean;
var
  I: Integer;
  Current: IZAssembly;
begin
  Result := False;
  if InVector <> nil then
  begin
    for I := 0 to InVector.Count - 1 do
    begin
      Current := InVector[I] as IZAssembly;
      if Current.ElementsConsumed = Assembly.ElementsConsumed then
        Exit;
    end;
    Result := True;
    InVector.Add(Assembly);
  end;
end;

{**
  Accepts a "visitor" which will perform some operation on
  a parser structure. The book, "Design Patterns", explains
  the visitor pattern.
  @param Visitor the visitor to accept
}
procedure TZAbstractParser.Accept(Visitor: IZParserVisitor);
begin
  AcceptWithVisited(Visitor, TZCollection.Create);
end;

{**
  Returns the most-matched assembly in a collection.
  @return the most-matched assembly in a collection.
  @param Vector the collection to look through
}
function TZAbstractParser.Best(Vector: IZCollection): IZAssembly;
var
  I: Integer;
  Current: IZAssembly;
begin
  Result := nil;
  for I := 0 to Vector.Count - 1 do
  begin
    Current := Vector[I] as IZAssembly;
    if not Current.HasMoreElements then
    begin
      Result := Current;
      Break;
    end;
    if Result = nil then
      Result := Current
    else if Current.ElementsConsumed > Result.ElementsConsumed then
      Result := Current;
  end;
end;

{**
  Returns an assembly with the greatest possible number of
  elements consumed by matches of this parser.

  @return   an assembly with the greatest possible number of
   elements consumed by this parser
  @param   Assembly   an assembly to match against
}
function TZAbstractParser.BestMatch(Assembly: IZAssembly): IZAssembly;
var
  InVector, OutVector: IZCollection;
begin
  InVector := TZCollection.Create;
  InVector.Add(Assembly);
  OutVector := MatchAndAssemble(InVector);
  Result := Best(OutVector);
end;

{**
  Returns either null, or a completely matched version of the supplied assembly.
  @return either null, or a completely matched version of the supplied assembly.
  @param Assembly an assembly to match against
}
function TZAbstractParser.CompleteMatch(Assembly: IZAssembly): IZAssembly;
begin
  Result := BestMatch(Assembly);
  if (Result <> nil) and (Result.HasMoreElements) then
    Result := nil;
end;

{**
  Create a copy of a vector, cloning each element of the vector.
  @param Vector the vector to copy
  @return   a copy of the input vector, cloning each element of the vector.
}
class function TZAbstractParser.CloneElements(Vector: IZCollection): IZCollection;
var
  I: Integer;
begin
  Result := TZCollection.Create;
  for I := 0 to Vector.Count - 1 do
    Result.Add((Vector[I] as IZAssembly).Clone);
end;

{**
  Returns the name of this parser.
  @return   the name of this parser
}
function TZAbstractParser.GetName: string;
begin
  Result := FName;
end;

{**
  Match this parser against an input state, and then
  apply this parser's assembler against the resulting state.

  @return a Vector of assemblies that result from matching
    against a beginning set of assemblies
  @param   Vector   a vector of assemblies to match against
}
function TZAbstractParser.MatchAndAssemble(InVector: IZCollection): IZCollection;
var
  I: Integer;
begin
  Result := Match(InVector);
  if FAssembler <> nil then
  begin
    for I := 0 to Result.Count - 1 do
      FAssembler.WorkOn(Result[I] as IZAssembly);
  end;
end;

{**
  Return a random element of this parser's language.
  @return  a random element of this parser's language
}
function TZAbstractParser.RandomInput(MaxDepth: Integer; Separator: string): string;
var
  I: Integer;
  Values: TStrings;
begin
  Result := '';
  Values := RandomExpansion(MaxDepth, 0);
  try
    for I := 0 to Values.Count - 1 do
    begin
      if I > 0 then
        Result := Result + Separator;
      Result := Result + Values[I];
    end;
  finally
    Values.Free;
  end;
end;

{**
  Sets the object that will work on an assembly whenever
  this parser successfully matches against the assembly.

  @param Value the assembler to apply
  @return Parser this parser object.
}
function TZAbstractParser.SetAssembler(Value: IZAssembler): IZParser;
begin
  FAssembler := Value;
  Result := Self;
end;

{**
  Returns a textual description of this parser.
  @return   String   a textual description of this parser, taking care to avoid
    infinite recursion
}
function TZAbstractParser.ToString: string;
begin
  Result := VisitedToString(TZCollection.Create);
end;

{**
  Returns a textual description of this parser.
  Parsers can be recursive, so when building a descriptive string,
  it is important to avoid infinite recursion by keeping track
  of the objects already described. This method keeps an object
  from printing twice, and uses <code>unvisitedString</code> which
  subclasses must implement.

  @param Visited a list of objects already printed
  @return a textual version of this parser, avoiding recursion
}
function TZAbstractParser.VisitedToString(Visited: IZCollection): string;
begin
  if FName <> '' then
    Result := FName
  else if Visited.Contains(Self) then
    Result := '...'
  else begin
    Visited.Add(Self);
    Result := UnvisitedString(Visited);
  end;
end;

{ TZEmpty }

{**
  Creates this object and assignes the main properties.
}
constructor TZEmpty.Create;
begin
  inherited Create;
end;

{**
  Creates this object and assignes the main properties.
  @param Name a parser name.
}
constructor TZEmpty.CreateWithName(Name: string);
begin
  inherited CreateWithName(Name);
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param Visitor the visitor to accept.
  @param Visited a collection of previously visited parsers
}
procedure TZEmpty.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitEmpty(Self, Visited);
end;

{**
  Given a set of assemblies, this method returns the set as a successful match.
  @return the input set of states
  @param InVector a vector of assemblies to match against
}
function TZEmpty.Match(InVector: IZCollection): IZCollection;
begin
  Result := CloneElements(InVector);
end;

{*
  There really is no way to expand an empty parser, so return an empty vector.
}
function TZEmpty.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
begin
  Result := TStringList.Create;
end;

{**
  Returns a textual description of this parser.
  @param Visited a collection of previously visited parsers
  @return a unvisited string description.
}
function TZEmpty.UnvisitedString(Visited: IZCollection): string;
begin
  Result := ' empty ';
end;

{ TZCollectionParser }

{**
  Creates this object and assignes the main properties.
}
constructor TZCollectionParser.Create;
begin
  inherited Create;
  FSubParsers := TZCollection.Create;
end;

{**
  Creates this object and assignes the main properties.
  @param Name a parser name.
}
constructor TZCollectionParser.CreateWithName(Name: string);
begin
  inherited CreateWithName(Name);
  FSubParsers := TZCollection.Create;
end;

{**
  Adds a parser to the collection.
  @param Parser the parser to add
  @return this parser object
}
function TZCollectionParser.Add(Parser: IZParser): IZCollectionParser;
begin
  FSubParsers.Add(Parser);
  Result := Self;
end;

{**
  Return this parser's subparsers.
  @return   Vector   this parser's subparsers
}
function TZCollectionParser.GetSubParsers: IZCollection;
begin
  Result := FSubParsers;
end;

{**
  Returns a textual description of this parser.
}
function TZCollectionParser.UnvisitedString(Visited: IZCollection): string;
var
  I: Integer;
  Current: IZParser;
begin
  Result := '<';
  for I := 0 to FSubParsers.Count - 1 do
  begin
    Current := FSubParsers[I] as IZParser;
    if I > 0 then
      Result := Result + ToStringSeparator;
    Result := Result + Current.VisitedToString(Visited);
  end;
  Result := Result + '>';
end;

{ TZAlternation }

{**
  Creates this object and assignes the main properties.
}
constructor TZAlternation.Create;
begin
  inherited Create;
end;

{**
  Creates this object and assignes the main properties.
  @param Name a parser name.
}
constructor TZAlternation.CreateWithName(Name: string);
begin
  inherited CreateWithName(Name);
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param ParserVisitor the visitor to accept
  @param Vector a collection of previously visited parsers
}
procedure TZAlternation.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitAlternation(Self, Visited);
end;

{**
  Given a set of assemblies, this method matches this
  alternation against all of them, and returns a new set
  of the assemblies that result from the matches.

  @param   Vector   a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZAlternation.Match(InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZParser;
begin
  Result := TZCollection.Create;
  for I := 0 to SubParsers.Count - 1 do
  begin
    Current := SubParsers[I] as IZParser;
    AppendAssemblies(Result, Current.MatchAndAssemble(InVector));
  end;
end;

{*
  Create a random collection of elements that correspond to this alternation.
}
function TZAlternation.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
var
  Parser: IZParser;
begin
  if Depth >= MaxDepth then
    Result := RandomSettle(MaxDepth, Depth)
  else begin
    Parser := SubParsers[Random(SubParsers.Count)] as IZParser;
    Result := Parser.RandomExpansion(MaxDepth, Depth + 1);
  end;
end;

{*
  This method is similar to randomExpansion, but it will
  pick a terminal if one is available.
}
function TZAlternation.RandomSettle(MaxDepth, Depth: Integer): TStrings;
var
  I: Integer;
  Current, Parser: IZParser;
  Terminals, Which: IZCollection;
begin
  { Which alternatives are terminals? }
  Terminals := TZCollection.Create;
  for I := 0 to SubParsers.Count - 1 do
  begin
    Current := SubParsers[I] as IZParser;
    if Current.InstanceOf(IZTerminal) then
      Terminals.Add(Current);
  end;

  { Pick one of the terminals or, if there are no terminals,
    pick any subparser }
  Which := Terminals;
  if Which.Count = 0 then
    Which := SubParsers;

  Parser := Which[Random(Which.Count)] as IZParser;
  Result := Parser.RandomExpansion(MaxDepth, Depth + 1);
end;

{**
  Returns the string to show between the parsers
  this parser is an alternation of.
}
function TZAlternation.ToStringSeparator: string;
begin
  Result := '|';
end;

{ TZRepetition }

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
}
constructor TZRepetition.Create(SubParser: IZParser);
begin
  CreateWithNameAndLimits(SubParser, '', 0, MaxInt);
end;

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
  @param Name a parser name.
}
constructor TZRepetition.CreateWithName(SubParser: IZParser; Name: string);
begin
  CreateWithNameAndLimits(SubParser, Name, 0, MaxInt);
end;

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
  @param MinCount a minimum count.
  @param MaxCount a maximum count.
}
constructor TZRepetition.CreateWithLimits(SubParser: IZParser; MinCount,
  MaxCount: Integer);
begin
  CreateWithNameAndLimits(SubParser, '', MinCount, MaxCount);
end;

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
  @param MinCount a minimum count.
  @param MaxCount a maximum count.
}
constructor TZRepetition.CreateWithNameAndLimits(SubParser: IZParser;
  Name: string; MinCount, MaxCount: Integer);
begin
  inherited CreateWithName(Name);
  FSubParser := SubParser;
  FMinCount := MinCount;
  FMaxCount := MaxCount;
end;

{**
  Gets a maximum repetition count.
  @return a maximum repetition count.
}
function TZRepetition.GetMaxCount: Integer;
begin
  Result := FMaxCount;
end;

{**
  Gets a minimum repetition count.
  @return a minimum repetition count.
}
function TZRepetition.GetMinCount: Integer;
begin
  Result := FMinCount;
end;

{**
  Sets a new maximum repetition count.
  @param Value a new manimum repetition count.
}
procedure TZRepetition.SetMaxCount(Value: Integer);
begin
  FMaxCount := Value;
end;

{**
  Sets a new minimum repetition count.
  @param Value a new minimum repetition count.
}
procedure TZRepetition.SetMinCount(Value: Integer);
begin
  FMinCount := Value;
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param ParserVisitor the visitor to accept
  @param Vector a collection of previously visited parsers
}
procedure TZRepetition.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitRepetition(Self, Visited);
end;

{**
  Return this parser's subparser.
  @return this parser's subparser
}
function TZRepetition.GetSubParser: IZParser;
begin
  Result := FSubParser;
end;

{**
  Given a set of assemblies, this method applies a preassembler
  to all of them, matches its subparser repeatedly against each
  of them, applies its post-assembler against each, and returns
  a new set of the assemblies that result from the matches.
  <p>
  For example, matching the regular expression <code>a*
  </code> against <code>(^aaab)</code> results in <code>
  (^aaab, a^aab, aa^ab, aaa^b)</code>.

  @param InVector   a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZRepetition.Match(InVector: IZCollection): IZCollection;
var
  I, Count: Integer;
  Temp: IZCollection;
begin
  if FPreAssembler <> nil then
  begin
    for I := 0 to InVector.Count - 1 do
      FPreAssembler.WorkOn(InVector[I] as IZAssembly);
  end;

  if FMinCount = 0 then
    Result := CloneElements(InVector)
  else Result := TZCollection.Create;

  Temp := FSubParser.MatchAndAssemble(InVector); // a working state
  Count := 0;
  while (Temp.Count > 0) and (Count < FMaxCount) do
  begin
    Temp := AppendAssemblies(Result, Temp);
    Inc(Count);
    Temp := FSubParser.MatchAndAssemble(Temp);
  end;
  if Count < FMinCount then
    Result := TZCollection.Create;
end;

{**
  Create a collection of random elements that correspond to this repetition.
}
function TZRepetition.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
var
  I, N: Integer;
  Temp: TStrings;
begin
  Result := TStringList.Create;
  if Depth < MaxDepth then
  begin
    N := Random(EXPWIDTH);
    for I := 0 to N do
    begin
      Inc(Depth);
      Temp := FSubParser.RandomExpansion(MaxDepth, Depth);
      Result.AddStrings(Temp);
    end;
  end;
end;

{**
  Sets the object that will work on every assembly before matching against it.
  @param Assembler the assembler to apply
  @return Parser this parser object.
}
function TZRepetition.SetPreAssembler(PreAssembler: IZAssembler): IZParser;
begin
  FPreAssembler := PreAssembler;
  Result := Self;
end;

{**
  Returns a textual description of this parser.
}
function TZRepetition.UnvisitedString(Visited: IZCollection): string;
begin
  Result := FSubParser.VisitedToString(Visited) + '*';
end;

{ TZSequence }

{**
  Constructs a nameless sequence.
}
constructor TZSequence.Create;
begin
  inherited Create;
end;

{**
  Constructs a sequence with the given name.
  @param Name a name to be known by
}
constructor TZSequence.CreateWithName(Name: string);
begin
  inherited CreateWithName(Name);
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param ParserVisitor the visitor to accept.
  @param InVector a collection of previously visited parsers
}
procedure TZSequence.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitSequence(Self, Visited);
end;

{**
  Given a set of assemblies, this method matches this
  sequence against all of them, and returns a new set
  of the assemblies that result from the matches.

  @param InVector a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZSequence.Match(InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZParser;
begin
  Result := InVector;
  for I := 0 to SubParsers.Count - 1 do
  begin
    Current := SubParsers[I] as IZParser;
    Result := Current.MatchAndAssemble(Result);
    if Result.Count = 0 then
      Break;
  end;
end;

{**
  Create a random expansion for each parser in this
  sequence and return a collection of all these expansions.
}
function TZSequence.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
var
  I: Integer;
  Current: IZParser;
  Temp: TStrings;
begin
  Result := TStringList.Create;
  for I := 0 to SubParsers.Count - 1 do
  begin
    Current := SubParsers[I] as IZParser;
    Inc(Depth);
    Temp := Current.RandomExpansion(MaxDepth, Depth);
    Result.AddStrings(Temp);
  end;
end;

{**
  Returns the string to show between the parsers this
  parser is a sequence of. This is an empty string,
  since convention indicates sequence quietly. For
  example, note that in the regular expression
  <code>(a|b)c</code>, the lack of a delimiter between
  the expression in parentheses and the 'c' indicates a
  sequence of these expressions.
}
function TZSequence.ToStringSeparator: string;
begin
  Result := '';
end;

{ TZTerminal }

{**
  Creates this object and assignes the main properties.
}
constructor TZTerminal.Create;
begin
  inherited Create;
  FDiscard := False;
end;

{**
  Creates this object and assignes the main properties.
  @param Name a parser name.
}
constructor TZTerminal.CreateWithName(Name: string);
begin
  inherited CreateWithName(Name);
  FDiscard := False;
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param ParserVisitor the visitor to accept
  @param Visited a collection of previously visited parsers
}
procedure TZTerminal.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitTerminal(Self, Visited);
end;

{**
  A convenience method that sets discarding to be true.
  @return this parser object.
}
function TZTerminal.Discard: IZTerminal;
begin
  Result := SetDiscard(True);
end;

{**
  Given a collection of assemblies, this method matches
  this terminal against all of them, and returns a new
  collection of the assemblies that result from the matches.

  @param InVector a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZTerminal.Match(InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZAssembly;
begin
  Result := TZCollection.Create;
  for I := 0 to InVector.Count - 1 do
  begin
    Current := InVector[I] as IZAssembly;
    Current := MatchOneAssembly(Current);
    if Current <> nil then
      AppendAssembly(Result, Current);
  end;
end;

{**
  Returns an assembly equivalent to the supplied assembly,
  except that this terminal will have been removed from the
  front of the assembly. As with any parser, if the
  match succeeds, this terminal's assembler will work on
  the assembly. If the match fails, this method returns null.

  @param   Assembly  the assembly to match against
  @return a copy of the incoming assembly, advanced by this terminal
}
function TZTerminal.MatchOneAssembly(Assembly: IZAssembly): IZAssembly;
var
  Element: IZObject;
begin
  if not Assembly.HasMoreElements then
    Result := nil
  else if Qualifies(Assembly.Peek) then
  begin
    Result := Assembly.Clone as IZAssembly;
    Element := TZAnyValue.Create(Result.NextElement);
    if not FDiscard then
      Result.Push(Element);
  end else
    Result := nil;
end;

{**
  The mechanics of matching are the same for many terminals,
  except for the check that the next element on the assembly
  qualifies as the type of terminal this terminal looks for.
  This method performs that check.

  @param Value an element from a assembly
  @return true, if the object is the kind of terminal this parser seeks
}
function TZTerminal.Qualifies(Value: IZObject): Boolean;
begin
  Result := True;
end;

{**
  By default, create a collection with this terminal's
  string representation of itself. (Most subclasses override this.)
}
function TZTerminal.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
begin
  Result := TStringList.Create;
  Result.Add(Self.ToString);
end;

{**
  By default, terminals push themselves upon a assembly's stack,
  after a successful match. This routine will turn off (or turn back on)
  that behavior.

  @param Value true, if this terminal should push itself on a assembly's stack
  @return this parser object.
}
function TZTerminal.SetDiscard(Value: Boolean): IZTerminal;
begin
  FDiscard := Value;
  Result := Self;
end;

{**
  Returns a textual description of this parser.
}
function TZTerminal.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'any';
end;

{ TZInvertion }

{**
  Creates this object and assignes the main properties.
  @param SubParser an matching sub parser.
}
constructor TZInvertion.Create(SubParser: IZParser);
begin
  CreateWithName(SubParser, '');
end;

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
}
constructor TZInvertion.CreateWithName(SubParser: IZParser; Name: string);
begin
  inherited CreateWithName(Name);
  FSubParser := SubParser;
end;

{**
  Accept a "visitor" and a collection of previously visited parsers.
  @param ParserVisitor the visitor to accept
  @param Vector a collection of previously visited parsers
}
procedure TZInvertion.AcceptWithVisited(Visitor: IZParserVisitor;
  Visited: IZCollection);
begin
  Visitor.VisitInvertion(Self, Visited);
end;

{**
  Return this parser's subparser.
  @return this parser's subparser
}
function TZInvertion.GetSubParser: IZParser;
begin
  Result := FSubParser;
end;

{**
  By default, terminals push themselves upon a assembly's stack,
  after a successful match. This routine will turn off (or turn back on)
  that behavior.

  @param Value true, if this terminal should push itself on a assembly's stack
  @return this parser object.
}
function TZInvertion.SetDiscard(Value: Boolean): IZInvertion;
begin
  FDiscard := Value;
  Result := Self;
end;

{**
  A convenience method that sets discarding to be true.
  @return this parser object.
}
function TZInvertion.Discard: IZInvertion;
begin
  Result := SetDiscard(True);
end;

{**
  Given a set of assemblies, this method applies a preassembler
  to all of them, matches its subparser repeatedly against each
  of them, applies its post-assembler against each, and returns
  a new set of the assemblies that result from the matches.
  <p>
  For example, matching the regular expression <code>!a
  </code> against <code>(^bcdaab)</code> results in <code>
  (^bcdaab, b^cdaab, bc^daab, bcd^aab)</code>.

  @param InVector   a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZInvertion.Match(InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZAssembly;
begin
  if FPreAssembler <> nil then
  begin
    for I := 0 to InVector.Count - 1 do
      FPreAssembler.WorkOn(InVector[I] as IZAssembly);
  end;

  Result := TZCollection.Create;
  for I := 0 to InVector.Count - 1 do
  begin
    Current := InVector[I] as IZAssembly;
    repeat
      Current := MatchOneAssembly(Current);
      if Current <> nil then
        AppendAssembly(Result, Current);
    until Current = nil;
  end;
end;

{**
  Returns an assembly equivalent to the rejected assemblies by
  substitute parser.

  @param   Assembly  the assembly to match against
  @return a copy of the incoming assembly, advanced by this terminal
}
function TZInvertion.MatchOneAssembly(Assembly: IZAssembly): IZAssembly;
var
  Element: IZObject;
  Temp: IZCollection;
begin
  if not Assembly.HasMoreElements then
    Result := nil
  else
  begin
    Temp := TZCollection.Create;
    Temp.Add(Assembly);
    if FSubParser.MatchAndAssemble(Temp).Count = 0 then
    begin
      Result := Assembly.Clone as IZAssembly;
      Element := TZAnyValue.Create(Result.NextElement);
      if not FDiscard then
        Result.Push(Element);
    end else
      Result := nil;
  end;
end;

{**
  Create a collection of random elements that correspond to this repetition.
}
function TZInvertion.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
(*
var
  I, N: Integer;
  Temp: TStrings;
*)
begin
  Result := TStringList.Create;
(*
  if Depth < MaxDepth then
  begin
    N := Random(EXPWIDTH);
    for I := 0 to N do
    begin
      Inc(Depth);
      Temp := FSubParser.RandomExpansion(MaxDepth, Depth);
      Result.AddStrings(Temp);
    end;
  end;
*)
end;

{**
  Sets the object that will work on every assembly before matching against it.
  @param Assembler the assembler to apply
  @return Parser this parser object.
}
function TZInvertion.SetPreAssembler(PreAssembler: IZAssembler): IZParser;
begin
  FPreAssembler := PreAssembler;
  Result := Self;
end;

{**
  Returns a textual description of this parser.
}
function TZInvertion.UnvisitedString(Visited: IZCollection): string;
begin
  Result := '!' + FSubParser.VisitedToString(Visited);
end;

{ TZFastInvertion }

{**
  Given a set of assemblies, this method applies a preassembler
  to all of them, matches its subparser repeatedly against each
  of them, applies its post-assembler against each, and returns
  a new set of the assemblies that result from the matches.
  <p>
  For example, matching the regular expression <code>!a
  </code> against <code>(^bcdaab)</code> results in <code>
  (bcd^aab)</code>.

  @param InVector   a vector of assemblies to match against
  @return a Vector of assemblies that result from
    matching against a beginning set of assemblies
}
function TZFastInvertion.Match(InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZAssembly;
  Temp: IZCollection;
begin
  if FPreAssembler <> nil then
  begin
    for I := 0 to InVector.Count - 1 do
      FPreAssembler.WorkOn(InVector[I] as IZAssembly);
  end;

  Result := TZCollection.Create;
  for I := 0 to InVector.Count - 1 do
  begin
    Current := InVector[I] as IZAssembly;
    Temp := TZCollection.Create;
    repeat
      Current := MatchOneAssembly(Current);
      if Current <> nil then
      begin
        Temp.Clear;
        AppendAssembly(Temp, Current);
      end;
    until Current = nil;
    Result.AddAll(Temp);
  end;
end;

{ TZOption }

{**
  Creates this object and assignes the main properties.
  @param SubParser an matching sub parser.
}
constructor TZOption.Create(SubParser: IZParser);
begin
  CreateWithName(SubParser, '');
end;

{**
  Creates this object and assignes the main properties.
  @param SubParser a repeatable sub parser.
}
constructor TZOption.CreateWithName(SubParser: IZParser; Name: string);
begin
  inherited CreateWithName(Name);
  FSubParser := SubParser;

  SubParsers.Add(SubParser);
  SubParsers.Add(TZEmpty.Create);
end;

{**
  Return this parser's subparser.
  @return this parser's subparser
}
function TZOption.GetSubParser: IZParser;
begin
  Result := FSubParser;
end;

{ TZAbstractParserTester }

{**
  Constructs a tester for the given parser.
}
constructor TZAbstractParserTester.Create(Parser: IZParser);
begin
  FParser := Parser;
end;

{**
  Generate a random language element, and return true if
  the parser cannot unambiguously parse it.
}
function TZAbstractParserTester.CanGenerateProblem(Depth: Integer): Boolean;
var
  TempStr: string;
  Assembly: IZAssembly;
  TempCollection: IZCollection;
begin
  TempStr := FParser.RandomInput(Depth, GetSeparator);
  LogTestString(TempStr);
  Assembly := GetAssembly(TempStr);
  Assembly.SetTarget(FreshTarget);

  TempCollection := TZCollection.Create;
  TempCollection.Add(Assembly);
  TempCollection := CompleteMatches(FParser.Match(TempCollection));
  if TempCollection.Count <> 1 then
  begin
    LogProblemFound(TempStr, TempCollection.Count);
    Result := True;
  end else
    Result := False;
end;

{**
  Return a subset of the supplied vector of assemblies,
  filtering for assemblies that have been completely matched.

  @param InVector a collection of partially or completely matched assemblies
  @return a collection of completely matched assemblies
}
class function TZAbstractParserTester.CompleteMatches(
  InVector: IZCollection): IZCollection;
var
  I: Integer;
  Current: IZAssembly;
begin
  Result := TZCollection.Create;
  for I := 0 to InVector.Count - 1 do
  begin
    Current := InVector[I] as IZAssembly;
    if not Current.HasMoreElements then
      Result.Add(Current);
  end;
end;

{**
  Give subclasses a chance to provide fresh target at
  the beginning of a parse.
}
function TZAbstractParserTester.FreshTarget: IZClonnable;
begin
  Result := nil;
end;

{**
  By default, place a blank between randomly generated "words" of a language.
}
function TZAbstractParserTester.GetSeparator: string;
begin
  Result := ' ';
end;

{**
  This method is broken out to allow subclasses to create
  less verbose tester, or to direct logging to somewhere
  other than System.out.
}
procedure TZAbstractParserTester.LogDepthChange(Depth: Integer);
begin
  System.Writeln(Format('Testing depth %d...', [Depth]));
end;

{**
  This method is broken out to allow subclasses to create less verbose tester,
  or to direct logging to somewhere other than System.out.
}
procedure TZAbstractParserTester.LogPassed;
begin
  System.Writeln('No problems found.');
end;

{**
  This method is broken out to allow subclasses to create less verbose tester,
  or to direct logging to somewhere other than System.out.
}
procedure TZAbstractParserTester.LogProblemFound(Str: string; MatchSize: Integer);
begin
  System.WriteLn('Problem found for string:');
  System.WriteLn(Str);
  if MatchSize = 0 then
    System.WriteLn('Parser cannot match this apparently valid string.')
  else
  begin
    System.Writeln(
      Format('The parser found %d ways to parse this string.', [MatchSize]));
  end;
end;

{**
  This method is broken out to allow subclasses to create less verbose tester,
  or to direct logging to somewhere other than System.out.
}
procedure TZAbstractParserTester.LogTestString(Str: string);
begin
  if FLogTestStrings then
    System.Writeln(Format('    Testing string %s', [Str]));
end;

{**
  Set the boolean which determines if this class displays every test string.
  @param Value true, if the user wants to see every test string
}
procedure TZAbstractParserTester.SetLogTestStrings(Value: Boolean);
begin
  FLogTestStrings := Value;
end;

{**
  Create a series of random language elements, and test
  that the parser can unambiguously parse each one.
}
procedure TZAbstractParserTester.Test;
var
  Depth, K: Integer;
begin
  for Depth := 2 to 7 do
  begin
    LogDepthChange(Depth);
    for K := 0 to 99 do
    begin
      if CanGenerateProblem(Depth) then
        Exit;
    end;
  end;
  LogPassed;
end;

end.
