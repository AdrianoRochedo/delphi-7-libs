{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fdstdreg;

interface

  procedure Register;

implementation

uses
  DsgnIntf, Classes,
  SysUtils, Graphics, Menus, Forms, Controls, Dialogs, Buttons, StdCtrls,
  ExtCtrls, Grids, Mask, ComCtrls,
  packconst, fPictureEditorDlg, fCollectionEditor, fMenuBuilder;

{$R stdreg.res}

 { TPictureProperty }
type
  TPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureProperty.Edit;
begin
  if EditPictureProperty(TObject(GetOrdValue)) then
    Modified;
end;


{ TCollectionProperty }

type
  TCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TCollectionProperty.Edit;
begin
  ShowCollectionDesigner(TCollectionEditor, Designer, TCollection(GetOrdValue));
end;

function TCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;


{ TMenuEditor }
type

  TMenuEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TMenuEditor.Edit;
begin
  ShowMenuDesigner(Designer, Component as TMenu);
end;

procedure TMenuEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowMenuDesigner(Designer, Component as TMenu);
  end;
end;

function TMenuEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SMDMenuDesigner;
  end;
end;

function TMenuEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


type
  TMenuItemsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TMenuItemsProperty.Edit;
begin
  ShowMenuDesigner(Designer, GetComponent(0) as TMenu);
end;

function TMenuItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;



{ Registration }

const
  sStandard     = 'Standard';
  sAdditional   = 'Additional';
  sWin32        = 'Win32';

procedure Register;
begin
  RegisterComponents(sStandard, [TMainMenu, TPopupMenu, TLabel,
    TEdit, TMemo, TButton, TCheckBox, TRadioButton, TListBox, TComboBox,
    TScrollBar, TGroupBox, TRadioGroup, TPanel]);

  RegisterComponents(sAdditional, [TBitBtn, TSpeedButton,
    TMaskEdit, TStringGrid, TDrawGrid, TImage, TShape, TBevel,
    TScrollBox {, TCheckListBox}, TSplitter, TStaticText]);

  RegisterComponents(sWin32, [TTabControl, TPageControl,
    TImageList, TRichEdit, TTrackBar, TProgressBar, TUpDown,
    THotKey, TAnimate, TTreeView, TListView, THeaderControl,
    TStatusBar, TToolBar, TCoolBar]);

  RegisterClasses([TTabSheet]);

  RegisterNoIcon([TMenuItem]);

  RegisterPropertyEditor(TypeInfo(TPicture), TObject, '', TPictureProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TObject, '', TPictureProperty);
  RegisterPropertyEditor(TypeInfo(TIcon), TObject, '', TPictureProperty);

  RegisterPropertyEditor(TypeInfo(TListColumns), TListView, 'Columns', TCollectionProperty);
  RegisterPropertyEditor(TypeInfo(TStatusPanels), TStatusBar, 'Panels', TCollectionProperty);

  RegisterComponentEditor(TMenu, TMenuEditor);
  RegisterPropertyEditor(TypeInfo(TMenuItem), TMenu, 'Items', TMenuItemsProperty);

{
  RegisterComponentEditor(TMenu, TMenuEditor);
  RegisterComponentEditor(TImage, TGraphicEditor);

  RegisterComponentEditor(TPageControl, TPageControlEditor);
  RegisterComponentEditor(TTabSheet, TPageControlEditor);
  RegisterComponentEditor(TImageList, TImageListEditor);

  RegisterPropertyEditor(TypeInfo(string), TCustomMaskEdit, 'EditMask', TMaskProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomMaskEdit, 'Text', TMaskTextProperty);

  RegisterPropertyEditor(TypeInfo(TTabSheet), TPageControl, 'ActivePage', TActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TStatusPanels), nil, '', TStatusPanelsProperty);
  RegisterPropertyEditor(TypeInfo(THeaderSections), nil, '', THeaderSectionsProperty);
  RegisterPropertyEditor(TypeInfo(TListColumns), nil, '', TListColumnsProperty);
  RegisterPropertyEditor(TypeInfo(TListItems), nil, '', TListItemsProperty);
  RegisterPropertyEditor(TypeInfo(TTreeNodes), nil, '', TTreeNodesProperty);
}
end;

end.
