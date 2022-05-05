{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Core classes and interfaces                 }
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

unit ZClasses;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  SysUtils, Classes;

type

  {** Replacement for generic interface type. }
  IZInterface = IUnknown;

  {** Represents an interface for all abstract object. }
  IZObject = interface(IZInterface)
    ['{EF46E5F7-00CF-4DDA-BED0-057D6686AEE0}']
    function Equals(const Value: IZInterface): Boolean;
    function Hash: LongInt;
    function Clone: IZInterface;
    function ToString: string;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {** Represents a fake interface for coparable objects. }
  IZComparable = interface(IZObject)
    ['{04112081-F07B-4BBF-A757-817816EB67C1}']
  end;

  {** Represents an interface to clone objects. }
  IZClonnable = interface(IZObject)
    ['{ECB7F3A4-7B2E-4130-BA66-54A2D43C0149}']
  end;

  {** Represents a generic collection iterator interface. }
  IZIterator = interface(IZObject)
    ['{D964DDD0-2308-4D9B-BD36-5810632512F7}']
    function HasNext: Boolean;
    function Next: IZInterface;
  end;

  {** Represents a collection of object interfaces. }
  IZCollection = interface(IZClonnable)
    ['{51417C87-F992-4CAD-BC53-CF3925DD6E4C}']

    function Get(Index: Integer): IZInterface;
    procedure Put(Index: Integer; const Item: IZInterface);
    function IndexOf(const Item: IZInterface): Integer;
    function GetCount: Integer;
    function GetIterator: IZIterator;

    function First: IZInterface;
    function Last: IZInterface;

    function Add(const Item: IZInterface): Integer;
    procedure Insert(Index: Integer; const Item: IZInterface);
    function Remove(const Item: IZInterface): Integer;

    procedure Exchange(Index1, Index2: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Contains(const Item: IZInterface): Boolean;
    function ContainsAll(const Col: IZCollection): Boolean;
    function AddAll(const Col: IZCollection): Boolean;
    function RemoveAll(const Col: IZCollection): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  {** Represents a hash map interface. }
  IZHashMap = interface(IZClonnable)
    ['{782C64F4-AD09-4F56-AF2B-E4193A05BBCE}']

    function Get(const Key: IZInterface): IZInterface;
    procedure Put(const Key: IZInterface; const Value: IZInterface);
    function GetKeys: IZCollection;
    function GetValues: IZCollection;
    function GetCount: Integer;

    function Remove(Key: IZInterface): Boolean;
    procedure Clear;

    property Count: Integer read GetCount;
    property Keys: IZCollection read GetKeys;
    property Values: IZCollection read GetValues;
  end;

  {** Represents a stack interface. }
  IZStack = interface(IZClonnable)
    ['{8FEA0B3F-0C02-4E70-BD8D-FB0F42D4497B}']

    function Peek: IZInterface;
    function Pop: IZInterface;
    procedure Push(Value: IZInterface);
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

  {** Implements an abstract interfaced object. }
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    function Equals(const Value: IZInterface): Boolean; virtual;
    function Hash: LongInt;
    function Clone: IZInterface; virtual;
    function ToString: string; virtual;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {** Represents any value interface. }
  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']

    function GetValue: Variant;
    function GetString: string;
    function GetInt: Integer;
  end;

  {** Implements an any value object. }
  TZAnyValue = class(TZAbstractObject, IZAnyValue, IZComparable)
  private
    FValue: Variant;
  public
    constructor Create(Value: Variant);

    function GetValue: Variant;
    function GetString: string;
    function GetInt: Integer;
    function GetDouble: Double;

    function Equals(const Value: IZInterface): Boolean; override;
    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

implementation

uses ZCompatibility;

{ TZAbstractObject }

{**
  Checks is the specified value equals to this object.
  @param Value an interface to some object.
  @return <code>True</code> if the objects are identical.
}
function TZAbstractObject.Equals(const Value: IZInterface): Boolean;
begin
  if Value <> nil then
  begin
    Result := (IZInterface(Self) = Value)
      or ((Self as IZInterface) = (Value as IZInterface));
  end else
   Result := False;
end;

{**
  Gets a unique hash for this object.
  @return a unique hash for this object.
}
function TZAbstractObject.Hash: LongInt;
begin
  Result := LongInt(Self);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAbstractObject.Clone: IZInterface;
begin
  raise Exception.Create('Clone operation is not supported by this class.');
end;

{**
  Checks is this object implements a specified interface.
  @param IId an interface id.
  @return <code>True</code> if this object support the interface.
}
function TZAbstractObject.InstanceOf(const IId: TGUID): Boolean;
begin
  Result := GetInterfaceEntry(IId) <> nil;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAbstractObject.ToString: string;
begin
  Result := Format('%s <%p>', [ClassName, Pointer(Self)])
end;

{ TZAnyValue }

{**
  Constructs this object and assignes the main properties.
  @param Value an any value.
}
constructor TZAnyValue.Create(Value: Variant);
begin
  FValue := Value;
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAnyValue.Clone: IZInterface;
begin
  Result := TZAnyValue.Create(FValue);
end;

{**
  Compares this and another property.
  @return <code>True</code> is properties are equal.
}
function TZAnyValue.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZAnyValue;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZAnyValue, Temp) = 0 then
    begin
      Result := FValue = Temp.GetValue;
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Gets a stored any value.
  @return a stored any value.
}
function TZAnyValue.GetValue: Variant;
begin
  Result := FValue;
end;

{**
  Gets a stored value converted to double.
  @return a stored value converted to double.
}
function TZAnyValue.GetDouble: Double;
begin
  try
    Result := VarAsType(FValue, varDouble);
  except
    Result := 0;
  end;
end;

{**
  Gets a stored value converted to integer.
  @return a stored value converted to integer.
}
function TZAnyValue.GetInt: Integer;
begin
  try
    Result := VarAsType(FValue, varInteger);
  except
    Result := 0;
  end;
end;

{**
  Gets a stored value converted to string.
  @return a stored value converted to string.
}
function TZAnyValue.GetString: string;
begin
  Result := VarToStrDef(FValue, '');
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAnyValue.ToString: string;
begin
  Result := VarToStrDef(FValue, '[Variant]');
end;

end.

