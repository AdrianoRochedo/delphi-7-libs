{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Component Property Editors                }
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

unit ZPropertyEditor;

interface

{$I ZComponent.inc}

{$IFDEF WITH_PROPERTY_EDITOR}

uses
  Classes, ZClasses, ZCompatibility, ZDbcIntfs,
{$IFNDEF VER130BELOW}
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

type

  {** Implements a property editor for Connection.Protocol property. }
  TZProtocolPropertyEditor = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function  GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{$ENDIF}

implementation

{$IFDEF WITH_PROPERTY_EDITOR}

{ TZProtocolPropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZProtocolPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZProtocolPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZProtocolPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{**
  Processes a list of list items.
  @param Proc a procedure to process the list items.
}
procedure TZProtocolPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
    begin
      Proc(Protocols[J]);
    end;
  end;
end;

{$ENDIF}

end.

