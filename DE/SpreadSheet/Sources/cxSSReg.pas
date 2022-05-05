{*******************************************************************}
{                                                                   }
{       Developer Express Cross platform Visual Component Library   }
{       ExpressSpreadSheet registration components unit	            }
{                                                                   }
{       Copyright (c) 2001-2003 Developer Express Inc.              }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by U.S. and       }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES           }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE    }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS   }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET AND ALL           }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE      }
{   PROGRAM ONLY.                                                   }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                      }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}

unit cxSSReg;

{$I cxSSVer.inc}

interface

uses
  Classes, {$IFDEF DELPHI6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  {$IFDEF VCL}
  Dialogs, cxSSheet, cxSSStyles, cxSSTypes, cxSSDesigner,
  cxSSCtrls, cxSSColorBox, cxExcelConst, cxSSRes;
  {$ELSE}
  QDialogs, QcxSSheet, QcxSSStyles, QcxSSTypes, QcxSSDesigner,
  QcxSSCtrls, QcxSSColorBox, QcxExcelConst, QcxSSRes;
  {$ENDIF}


procedure Register;

implementation
type
 { TcxSpreadSheetEditor }
  TcxSpreadSheetEditor = class(TComponentEditor)
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TcxSSCellStyleProperty }
  TcxSSCellStyleProperty = class(TClassProperty)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TcxSSColorProperty }
  TcxSSColorProperty = class(TClassProperty)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TcxSSFontProperty }
  TcxSSFontProperty = class(TClassProperty)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {$IFNDEF DELPHI6}
  {$IFDEF DELPHI5}
  { TSpreadSheetCategory }
  TSpreadSheetCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;
  {$ENDIF}
  {$ENDIF}


{ TcxSpreadSheetEditor }
function TcxSpreadSheetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'ExpressSpreadSheet 1.0.7';
    1: Result := 'Developer Express Inc.';
  end;
end;

function TcxSpreadSheetEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TcxSSCellStyleProperty }
procedure TcxSSCellStyleProperty.Edit;
var
  AStyle: TcxSSCellStyle;
begin
  AStyle := TcxSSCellStyle(GetOrdValue);
  with TcxSSStyleDesigner.Create(nil) do
  try
    Execute(AStyle)
  finally
    Free;
  end;
  inherited;
end;

function TcxSSCellStyleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

{ TcxSSColorProperty }
procedure TcxSSColorProperty.Edit;
var
  AColor: TcxSSColor;
begin
  AColor := TcxSSColor(GetOrdValue);
  if TcxSSColorPropertyEditor.Execute(AColor) then
    SetOrdValue(Integer(AColor));
  inherited;
end;

function TcxSSColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TcxSSFontProperty.Edit;
var
  AFont: TcxSSFont;
begin
  AFont := TcxSSFont(GetOrdValue);
  if AFont <> nil then
  begin
    with TFontDialog.Create(nil) do
    try
      if AFont.FontColor <= 55 then
        Font.Color := cxExcelStdColors[AFont.FontColor]
      else
        Font.Color := 0;
      Font.Name := AFont.Name;
      Font.Style := AFont.Style;
      Font.Charset := AFont.Charset;
      Font.Size := AFont.Size;
      if Execute then
        AFont.AssignInfo(Font.Name, Font.Size, Font.Style, Font.Charset,
          xlsSetColor(Font.Color, @cxExcelStdColors, 56));
    finally
      Free;
    end;
  end;
  inherited;
end;

function TcxSSFontProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog];
end;


{$IFNDEF DELPHI6}
{$IFDEF DELPHI5}
class function TSpreadSheetCategory.Name: string;
begin
  Result := scxSpreadSheet;
end;

class function TSpreadSheetCategory.Description: string;
begin
  Result := scxSpreadSheetDesc;
end;
{$ENDIF}
{$ENDIF}

procedure Register;
const
  VisualCategory: array[0..12] of string =
  ('BufferedPaint', 'ColHeaderHeight', 'DefaultColWidth', 'DefaultRowHeight',
   'DefaultStyle', 'HideSelection', 'PainterType', 'Precision', 'R1C1ReferenceStyle',
   'RowHeaderWidth', 'ShowFormulas', 'ShowGrid', 'ShowHeaders');
  SpreadSheetCategory: array[0..4] of string =
  ('GridColor', 'HeaderColor', 'HeaderFont', 'SelectionColor', 'BackgroundColor');

{$IFDEF DELPHI6}
  AAction: string = sActionCategoryName;
  ASpreadSheet: string = scxSpreadSheet;
  AVisual: string = sVisualCategoryName;
{$ELSE}
{$IFDEF DELPHI5}
  AAction: TPropertyCategoryClass = TActionCategory;
  ASpreadSheet: TPropertyCategoryClass = TSpreadSheetCategory;
  AVisual: TPropertyCategoryClass = TVisualCategory;
{$ENDIF}
{$ENDIF}

begin
  RegisterClasses([TcxPaintPanel, TcxLabelBevel, TcxSSColorComboBox,
    TcxSSColorPanel, TcxSSStyleListBox]);
  {$IFDEF DELPHI5}
  RegisterPropertiesInCategory(AAction, TcxCustomSpreadSheetBook, ['AutoRecalc']);
  RegisterPropertiesInCategory(ASpreadSheet, TcxCustomSpreadSheetBook, SpreadSheetCategory);
  RegisterPropertiesInCategory(AVisual, TcxCustomSpreadSheetBook, VisualCategory);
  {$ENDIF}
  RegisterComponents('Dev Express', [TcxSpreadSheetBook, TcxSpreadSheet]);
  RegisterComponentEditor(TcxCustomSpreadSheetBook, TcxSpreadSheetEditor);
  RegisterPropertyEditor(TypeInfo(TcxSSCellStyle), TcxCustomSpreadSheetBook,
    'DefaultStyle', TcxSSCellStyleProperty);
  RegisterPropertyEditor(TypeInfo(TcxSSColor), nil, '', TcxSSColorProperty);
  RegisterPropertyEditor(TypeInfo(TcxSSFont), nil, '', TcxSSFontProperty);
end;

end.


