{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclRTF.pas.                                             }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Last modified: August 10, 2000                                               }
{                                                                              }
{******************************************************************************}

unit JclRTF;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, Graphics, SysUtils, Windows;

{$IFNDEF SUPPORTS_DYNAMICARRAYS}
const
  JclRTFProducerArrSize = 100;
{$ENDIF}

type
  TJclRTFProducer = class (TObject)
  private
    FCodePage: Word;
    FFontChanged: Boolean;
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    FColorTable: array of TColor;
    FFontTable: array of string;
    FFontCharsetTable: array of TFontCharset;
    {$ELSE}
    FColorTable: array [0..JclRTFProducerArrSize - 1] of TColor;
    FFontTable: array [0..JclRTFProducerArrSize - 1] of string;
    FFontCharsetTable: array [0..JclRTFProducerArrSize - 1] of TFontCharset;
    FColorCount, FFontCount: Integer;
    {$ENDIF}
    FFont: TFont;
    FStaticStream: TStream;
    FText: string;
    function GetRTFText: string;
    procedure FontChanged(Sender: TObject);
    function RTFHeader: string;
    procedure SetFont(const Value: TFont);
    function GetStaticStream: TStream;
  protected
    function FindColor(Color: TColor): Integer;
    function FindFontNameCharset(const FontName: string; Charset: TFontCharset): Integer;
    procedure WriteChanges;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(const FileName: TFileName);
    procedure SaveToStream(S: TStream);
    procedure WriteLine;
    procedure WriteParagraph(Alignment: TAlignment{$IFDEF SUPPORTS_DEFAULTPARAMS} = taLeftJustify{$ENDIF});
    procedure WriteRaw(const RawText: string);
    procedure WriteText(const Text: string);
    property CodePage: Word read FCodePage write FCodePage;
    property Font: TFont read FFont write SetFont;
    property RTFText: string read GetRTFText;
    property StaticStream: TStream read GetStaticStream;
  end;

implementation

uses
  Consts,
  JclBase, JclStrings, JclSysUtils;

resourcestring
  RsRTFArrayOut = 'Limit of internal array size exceeded.';

type
  TJclRTFStaticStream = class (TCustomMemoryStream)
  private
    FProducer: TJclRTFProducer;
    FText: string;
  public
    constructor Create(AProducer: TJclRTFProducer);
    procedure Refresh;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

//==============================================================================
// TJclRTFProducer
//==============================================================================

procedure TJclRTFProducer.Clear;
begin
  FCodePage := GetACP;
  FFontChanged := True;
  FText := '';
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  FColorTable := nil;
  FFontTable := nil;
  FFontCharsetTable := nil;
  {$ELSE}
  FColorCount := 0;
  FFontCount := 0;
  {$ENDIF}
  FreeAndNil(FStaticStream);
end;

//------------------------------------------------------------------------------

constructor TJclRTFProducer.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  Clear;
end;

//------------------------------------------------------------------------------

destructor TJclRTFProducer.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FStaticStream);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.FindColor(Color: TColor): Integer;
var
  I: Integer;
begin
  Result := -1;
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  for I := Low(FColorTable) to High(FColorTable) do
    if Color = FColorTable[I] then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Result := Length(FColorTable);
    SetLength(FColorTable, Result + 1);
    FColorTable[Result] := Color;
  end;
  {$ELSE}
  for I := 0 to FColorCount - 1 do
    if Color = FColorTable[I] then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Inc(FColorCount);
    if FColorCount > JclRTFProducerArrSize then
      raise EJclError.CreateResRec(@RsRTFArrayOut);
    Result := FColorCount - 1;
    FColorTable[Result] := Color;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.FindFontNameCharset(const FontName: string; Charset: TFontCharset): Integer;
var
  I: Integer;
begin
  Result := -1;
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  for I := Low(FFontTable) to High(FFontTable) do
    if StrSame(FFontTable[I], FontName) and (FFontCharsetTable[I] = Charset) then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Result := Length(FFontTable);
    SetLength(FFontTable, Result + 1);
    SetLength(FFontCharsetTable, Result + 1);
    FFontTable[Result] := FontName;
    FFontCharsetTable[Result] := Charset;
  end;
  {$ELSE}
  for I := 0 to FFontCount - 1 do
    if StrSame(FFontTable[I], FontName) and (FFontCharsetTable[I] = Charset) then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Inc(FFontCount);
    if FFontCount > JclRTFProducerArrSize then
      raise EJclError.CreateResRec(@RsRTFArrayOut);
    Result := FFontCount - 1;
    FFontTable[Result] := FontName;
    FFontCharsetTable[Result] := Charset;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.GetRTFText: string;
begin
  Result := RTFHeader + FText + '\par }';
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.GetStaticStream: TStream;
begin
  if FStaticStream = nil then
    FStaticStream := TJclRTFStaticStream.Create(Self);
  TJclRTFStaticStream(FStaticStream).Refresh;
  Result := FStaticStream;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.RTFHeader: string;
var
  I: Integer;

  function FontString(Index: Integer): string;
  begin
    Result := Format('{\f%d\fcharset%d %s;}', [Index, FFontCharsetTable[Index], FFontTable[Index]]);
  end;

  function ColorString(Index: Integer): string;
  var
    Color: TColorRef;
  begin
    Color := ColorToRGB(FColorTable[Index]);
    Result := Format('\red%d\green%d\blue%d;', [GetRValue(Color), GetGValue(Color), GetBValue(Color)]);
  end;

begin
  Result := Format('{\rtf1\ansi\ansicpg%d\deff0\deftab720', [FCodePage]);
  Result := Result + '{\fonttbl';
  for I := 0 to {$IFDEF SUPPORTS_DYNAMICARRAYS}Length(FFontTable){$ELSE}FFontCount{$ENDIF} - 1 do
    Result := Result + FontString(I);
  Result := Result + '}';
  Result := Result + '{\colortbl';
  for I := 0 to {$IFDEF SUPPORTS_DYNAMICARRAYS}Length(FColorTable){$ELSE}FColorCount{$ENDIF} - 1 do
    Result := Result + ColorString(I);
  Result := Result + '}';
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SaveToFile(const FileName: TFileName);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SaveToStream(S: TStream);
var
  LocalText: string;
begin
  LocalText := RTFText;
  S.WriteBuffer(Pointer(LocalText)^, Length(LocalText) + 1);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteChanges;
const
  FontStyleStrs: array [TFontStyle] of PChar = ('\b', '\i', '\ul', '\strike');
var
  ColorIndex, FontIndex: Integer;
  S: string;
  FS: TFontStyle;
begin
  if FFontChanged then
  begin
    ColorIndex := FindColor(FFont.Color);
    FontIndex := FindFontNameCharset(FFont.Name, FFont.Charset);
    S := Format('\plain\f%d\fs%d\cf%d', [FontIndex, FFont.Size * 2, ColorIndex]);
    for FS := Low(FS) to High(FS) do
      if FS in FFont.Style then
        S := S + FontStyleStrs[FS];
    S := S + ' ';
    FText := FText + S;
    FFontChanged := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteLine;
begin
  WriteText(AnsiLineFeed);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteParagraph(Alignment: TAlignment);
var
  S: string;
begin
  S := '\pard';
  case Alignment of
    taLeftJustify:
      S := S + ' ';
    taRightJustify:
      S := S + '\qr ';
    taCenter:
      S := S + '\qc ';
  end;    
  FText := FText + S;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteRaw(const RawText: string);
begin
  FText := FText + RawText;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteText(const Text: string);
var
  I: Integer;
  S: string;
  C: Char;
begin
  S := '';
  WriteChanges;
  for I := 1 to Length(Text) do
  begin
    C := Text[I];
    case C of
      AnsiLineFeed:
        S := S + '\par ' + AnsiCrLf;
      #32..#127:
        case C of
          '{', '}', '\':
            S := '\' + C;
        else
          S := S + C;
        end;
      #128..#255:
        S := S + LowerCase(Format('\''%.2x', [Byte(C)]));
    end;
  end;
  FText := FText + S;
end;

//==============================================================================
// TJclRTFStaticStream
//==============================================================================

constructor TJclRTFStaticStream.Create(AProducer: TJclRTFProducer);
begin
  inherited Create;
  FProducer := AProducer;
end;

//------------------------------------------------------------------------------

procedure TJclRTFStaticStream.Refresh;
begin
  FText := FProducer.RTFText;
  SetPointer(PChar(FText), Length(FText) + 1);
  Position := 0;
end;

//------------------------------------------------------------------------------

function TJclRTFStaticStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EJclError.CreateResRec(@SCantWriteResourceStreamError);
end;

end.
