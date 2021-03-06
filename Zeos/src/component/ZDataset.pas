{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Universal Dataset component               }
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

unit ZDataset;

interface

{$I ZComponent.inc}

uses ZAbstractRODataset, ZAbstractDataset;

type

{** Implements an universal SQL query for read/only data access. }
TZReadOnlyQuery = class (TZAbstractRODataSet)
published
  property Active;
{$IFNDEF VER130BELOW}
  property IsUniDirectional;
{$ENDIF}
  property SQL;
  property ParamCheck;
  property Params;
end;

{** Implements an universal SQL query for read/write data access. }
TZQuery = class (TZAbstractDataSet)
published
  property Active;
  property SQL;
  property ParamCheck;
  property Params;
  property ShowRecordTypes;
end;

implementation

end.

