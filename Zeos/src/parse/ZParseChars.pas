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

unit ZParseChars;

interface

uses Classes, SysUtils, ZClasses, ZParseIntfs, ZParseCore;

type

  {** A CharacterAssembly is an Assembly whose elements are characters. }
  TZCharacterAssembly  = class (TZAbstractAssembly)
  private
    FBuffer: string;
  protected
    property Buffer: string read FBuffer write FBuffer;
  public
    constructor Create(Buffer: string);
    function Clone: IZInterface; override;

    function DefaultDelimiter: string; override;
    function Consumed(Delimiter: string): string; override;
    function Remainder(Delimiter: string): string; override;
    function ElementsCount: Integer; override;
    function NextElement: string; override;

    function Peek: IZObject; override;
  end;

  {** A Char matches a character from a character assembly. }
  TZChar = class (TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
  end;

  {** A SpecificChar matches a specified character from a character assembly. }
  TZSpecificChar = class (TZTerminal)
  private
    FCharacter: Char;
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;

    property Character: Char read FCharacter write FCharacter;
  public
    constructor CreateWithValue(Name: string; Character: Char);
  end;

  {** A Letter matches any letter from a character assembly. }
  TZLetter = class (TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  end;

  {** A Digit matches a digit from a character assembly. }
  TZDigit = class (TZTerminal)
  protected
    function Qualifies(Value: IZObject): Boolean; override;
    function UnvisitedString(Visited: IZCollection): string; override;
    function RandomExpansion(MaxDepth: Integer; Depth: Integer): TStrings;
      override;
  end;

  {** Implements a tester for character parsers. }
  TZCharacterTester = class (TZAbstractParserTester)
  protected
    function GetAssembly(Value: string): IZAssembly; override;
    function GetSeparator: string; override;
  public
    constructor Create(Parser: IZParser);
  end;

implementation

uses ZCollections;

{ TZCharacterAssembly }

{**
  Constructs a CharacterAssembly from the given String.
  @param Buffer the String to consume
}
constructor TZCharacterAssembly.Create(Buffer: string);
begin
  inherited Create;
  FBuffer := Buffer;
end;

{**
  Return a copy of this object.
  @return a copy of this object
}
function TZCharacterAssembly.Clone: IZInterface;
begin
  Result := CloneProperties(TZCharacterAssembly.Create(FBuffer));
end;

{**
  Returns a textual representation of the amount of this
  characterAssembly that has been consumed.

  @param delimiter the mark to show between consumed elements.
  @return a textual description of the amount of this
    assembly that has been consumed
}
function TZCharacterAssembly.Consumed(Delimiter: string): string;
var
  I: Integer;
begin
  if Delimiter = '' then
    Result := Copy(FBuffer, 1, ElementsConsumed)
  else begin
    Result := '';
    for I := 1 to ElementsConsumed do
    begin
      if I > 1 then
        Result := Result + 'Delimiter';
      Result := Result + FBuffer[I];
    end;
  end;
end;

{**
  Returns the default string to show between elements consumed or remaining.
  @return the default string to show between elements consumed or remaining
}
function TZCharacterAssembly.DefaultDelimiter: string;
begin
  Result := '';
end;

{**
  Returns the number of elements in this assembly.
  @return   the number of elements in this assembly
}
function TZCharacterAssembly.ElementsCount: Integer;
begin
  Result := Length(FBuffer);
end;

{**
  Returns the next character.
  @return the next character from the associated token string
}
function TZCharacterAssembly.NextElement: string;
begin
  ElementIndex := ElementIndex + 1;
  Result :=  FBuffer[ElementIndex];
end;

{**
  Shows the next object in the assembly, without removing it
  @return the next object
}
function TZCharacterAssembly.Peek: IZObject;
begin
  if ElementIndex < ElementsCount then
    Result := TZAnyValue.Create(FBuffer[ElementIndex + 1])
  else Result := nil;
end;

{**
  Returns a textual representation of the amount of this
  characterAssembly that remains to be consumed.

  @param Delimiter the mark to show between consumed elements
  @return a textual description of the amount of this
    assembly that remains to be consumed
}
function TZCharacterAssembly.Remainder(Delimiter: string): string;
var
  I: Integer;
begin
  if Delimiter = '' then
    Result := Copy(FBuffer, ElementsConsumed + 1, Length(FBuffer))
  else begin
    Result := '';
    for I := ElementsConsumed + 1 to Length(FBuffer) do
    begin
      if I > ElementsConsumed + 1 then
        Result := Result + Delimiter;
      Result := Result + FBuffer[I];
    end;
  end;
end;

{ TZChar }

{**
  Returns true every time, since this class assumes it is
  working against a CharacterAssembly.

  @param Value ignored
  @return  true, every time, since this class assumes it is
    working against a CharacterAssembly
}
function TZChar.Qualifies(Value: IZObject): Boolean;
begin
  Result := True;
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return   string   a textual description of this parser
}
function TZChar.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'C';
end;

{ TZSpecificChar }

{**
  Constructs a SpecificChar to match the specified char.
  @param Name a name of this parser.
  @param Character the character to match
  @return a SpecificChar to match a Character constructed
    from the specified char.
}
constructor TZSpecificChar.CreateWithValue(Name: string; Character: Char);
begin
  inherited CreateWithName(Name);
  FCharacter := Character;
end;

{**
  Returns true if an assembly's next element is equal to the
  character this object was constructed with.

  @param Value an element from an assembly
  @return true, if an assembly's next element is equal to
    the character this object was constructed with
}
function TZSpecificChar.Qualifies(Value: IZObject): Boolean;
begin
  if Value.InstanceOf(IZAnyValue) then
    Result := (Value as IZAnyValue).GetString = FCharacter
  else Result := False;
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return   string   a textual description of this parser
}
function TZSpecificChar.UnvisitedString(Visited: IZCollection): string;
begin
  Result := FCharacter;
end;

{ TZLetter }

{**
  Returns true if an assembly's next element is equal to the
  character this object was constructed with.

  @param Value an element from an assembly
  @return true, if an assembly's next element is equal to
    the character this object was constructed with
}
function TZLetter.Qualifies(Value: IZObject): Boolean;
var
  Temp: string;
begin
  if Value.InstanceOf(IZAnyValue) then
  begin
    Temp := (Value as IZAnyValue).GetString;
    if Length(Temp) = 1 then
      Result := Temp[1] in ['a'..'z','A'..'Z']
    else Result := False;
  end else
    Result := False;
end;

{**
  Create a set with one random letter.
}
function TZLetter.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
begin
  Result := TStringList.Create;
  Result.Add(Chr(Ord('a') + Random(26)));
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return   string   a textual description of this parser
}
function TZLetter.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'L';
end;

{ TZDigit }

{**
  Returns true if an assembly's next element is equal to the
  character this object was constructed with.

  @param Value an element from an assembly
  @return true, if an assembly's next element is equal to
    the character this object was constructed with
}
function TZDigit.Qualifies(Value: IZObject): Boolean;
var
  Temp: string;
begin
  if Value.InstanceOf(IZAnyValue) then
  begin
    Temp := (Value as IZAnyValue).GetString;
    if Length(Temp) = 1 then
      Result := Temp[1] in ['0'..'9']
    else Result := False;
  end else
    Result := False;
end;

{**
  Create a set with one random letter.
}
function TZDigit.RandomExpansion(MaxDepth, Depth: Integer): TStrings;
begin
  Result := TStringList.Create;
  Result.Add(Chr(Ord('0') + Random(10)));
end;

{**
  Returns a textual description of this parser.
  @param Visited a list of parsers already printed in this description
  @return   string   a textual description of this parser
}
function TZDigit.UnvisitedString(Visited: IZCollection): string;
begin
  Result := 'D';
end;

{ TZCharacterTester }

{**
  Constructs this test and assignes the related parser object.
  @param Parser a related parser object.
}
constructor TZCharacterTester.Create(Parser: IZParser);
begin
  inherited Create(Parser);
end;

{**
  Gets an assembly object and initializes it with the string.
  @param Str a string buffer for the assembly.
  @return a created assembly object.
}
function TZCharacterTester.GetAssembly(Value: string): IZAssembly;
begin
  Result := TZCharacterAssembly.Create(Value);
end;

{**
  Gets a separator character.
  @return separator character.
}
function TZCharacterTester.GetSeparator: string;
begin
  Result := '';
end;

end.

