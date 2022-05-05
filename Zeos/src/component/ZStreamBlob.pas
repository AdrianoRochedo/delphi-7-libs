{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Blob streams classes                   }
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

unit ZStreamBlob;

interface

uses Classes, SysUtils, ZDbcIntfs, DB;

type
  {** Implements a class for blobs stream. }
  TZBlobStream = class(TMemoryStream)
  private
    FBlob: IZBlob;
    FMode: TBlobStreamMode;
  protected
    property Blob: IZBlob read FBlob write FBlob;
    property Mode: TBlobStreamMode read FMode write FMode;
  public
    constructor Create(Blob: IZBlob; Mode: TBlobStreamMode);
    destructor Destroy; override;
  end;

implementation

{ TZBlobStream }

{**
  Constructs this object and assignes the main properties.
  @param Blob
}
constructor TZBlobStream.Create(Blob: IZBlob; Mode: TBlobStreamMode);
var
  TempStream: TStream;
begin
  inherited Create;

  FBlob := Blob;
  FMode := Mode;
  if (Mode in [bmRead, bmReadWrite]) and not Blob.IsEmpty then
  begin
    TempStream := Blob.GetStream;
    try
      TempStream.Position := 0;
      CopyFrom(TempStream, TempStream.Size);
      Position := 0;
    finally
      TempStream.Free;
    end;
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
begin
  if Mode in [bmWrite, bmReadWrite] then
    Blob.SetStream(Self);
  inherited Destroy;
end;

end.

