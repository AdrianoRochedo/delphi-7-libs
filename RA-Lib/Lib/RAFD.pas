{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 192001 Andrei Prygounkov

       description : resource identifers,
                     common global objects

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFD;

interface

uses Windows, SysUtils, Forms, Menus, RARegAuto, RAFDCompat;

type

  ERAFDError = class(Exception)
  public
    ErrCode : integer;
    ErrName : string;
    ErrName2 : string;
    constructor Create(const AErrCode : integer; const AErrName,
      AErrName2 : string);
  end;

  procedure RAFDError(const AErrCode : integer);
  procedure RAFDErrorN(const AErrCode : integer; const AErrName : string);
  procedure RAFDErrorN2(const AErrCode : integer; const AErrName1,
    AErrName2 : string);
  function ResStr(const ID: Integer; const Default: string): string;

  procedure LocaleMenuItem(MenuItem: TMenuItem; const CaptionID, HintID: Integer);
  
const
  deResBase           = $5400;

  deFileNotFound      = 1;
  deNoFormSelected    = 2;
  deCantSaveFile      = 3;
  deFileChanged       = 4;
  deNotYetImplemented = 5;
  deInternal          = 6;
  deRAI2NotFound      = 7;
  deRAI2FuncNotFound  = 8;
  deLoadExpertFail    = 9;
  deErrorCreatingForm = 10;
  deCantChangeIniStrings = 11;

 { source manipualtion error }
  deCantRenameForm       = 12;
  deCantRenameUnit       = 13;
  deCantRenameMethod     = 14;
  deFormDeclNotFound     = 15;
  deClauseIncorrect      = 16;


  deNoHint = 0;

  deStrBase           = deResBase + $200;

  deOk                = deStrBase + 1;
  deCancel            = deStrBase + 2;
  deHelp              = deStrBase + 3;
  deClose             = deStrBase + 4;
  deIgnore            = deStrBase + 5;
  deIgnoreAll         = deStrBase + 6;


 { Form Designer }
  deBringToFront      = deStrBase + 50;
  deSendToBack        = deStrBase + 51;

 { App Builder - main form }
  deFileMenu                   = deStrBase + 100;
  deEditMenu                   = deStrBase + 101;
  deViewsMenu                  = deStrBase + 102;
  deFileNewItem                = deStrBase + 103;
  deFileNewFormItem            = deStrBase + 104;
  deFileOpenItem               = deStrBase + 105;
  deFileSaveItem               = deStrBase + 106;
  deFileSaveAsItem             = deStrBase + 107;
  deFileSaveAllItem            = deStrBase + 108;
  deFileCloseItem              = deStrBase + 109;
  deFileCloseAllItem           = deStrBase + 110;
  deFileExitItem               = deStrBase + 111;
  deEditDeleteItem             = deStrBase + 112;
  deViewObjInspItem            = deStrBase + 113;
  deViewAlignItem              = deStrBase + 114;
  deViewPaletteItem            = deStrBase + 115;
  deRunMenu                    = deStrBase + 116;
  deRunFormItem                = deStrBase + 117;
  deRunReportPreviewItem       = deStrBase + 118;
  deViewToggleFormUnitItem     = deStrBase + 119;
  deOpenHint                   = deStrBase + 120;
  deSaveAllHint                = deStrBase + 121;
  deViewToggleFormUnitHint     = deStrBase + 122;
  deRunFormHint                = deStrBase + 123;
  deRunReportPreviewHint       = deStrBase + 124;
  deComponentMenu              = deStrBase + 125;
  deViewWindowListItem         = deStrBase + 126;
  deSearchMenu                 = deStrBase + 127;
  deProjectMenu                = deStrBase + 128;
  deToolsMenu                  = deStrBase + 129;
  dePrjViewSourceItem          = deStrBase + 130;
  deToolsOptionsItem           = deStrBase + 131;
  deHelpMenu                   = deStrBase + 132;
  deEditCopyItem               = deStrBase + 133;
  deEditPasteItem              = deStrBase + 134;
  deToolsEditorOptionsItem     = deStrBase + 135;
  deRunUnitItem                = deStrBase + 136;
  deViewPrjMngrItem            = deStrBase + 137;
  deRunProjectItem             = deStrBase + 138;
  deViewUnitItem               = deStrBase + 139;
  deViewFormItem               = deStrBase + 140;
  deRunFormItemHint            = deStrBase + 141;
  deRunReportPreviewItemHint   = deStrBase + 142;
  deRunUnitItemHint            = deStrBase + 143;
  deRunProjectItemHint         = deStrBase + 144;
  deInstallPackagesItem        = deStrBase + 145;

 { Alignment Palette }
  deAlign             = deStrBase + 200;
  deAlign10           = deStrBase + 210;

 { ObjectInspector }
  deObjectInspector   = deStrBase + 220;
  deProperties        = deStrBase + 221;
  deEvents            = deStrBase + 222;

 { String List Editor }
  deStringsEditor     = deStrBase + 230;
  deCodeEditor        = deStrBase + 231;

 { New Items }
  deNewItems          = deStrBase + 240;
  deNew               = deStrBase + 241;

 { Project Manager }
  deProjectManager     = deStrBase + 260;
  dePrjMngrUnit        = deStrBase + 261;
  dePrjMngrForm        = deStrBase + 262;
  dePrjMngrPath        = deStrBase + 263;
  dePrjMngrUpdateItem  = deStrBase + 264;

 { View Dialog }
  deViewDlgUnit        = deStrBase + 290;
  deViewDlgForm        = deStrBase + 291;

 { Installed Packages Dialog }
  dePakListDlgTitle      = deStrBase + 310;
  dePakListDlgAddTitle   = deStrBase + 311;
  dePakListDlgAddFilter  = deStrBase + 312;
  dePakListDlgPackages   = deStrBase + 313;
  dePakListDlgAdd        = deStrBase + 314;
  dePakListDlgRemove     = deStrBase + 315;
  dePakListDlgEdit       = deStrBase + 316;
  dePakListDlgComponents = deStrBase + 317;

 { Read Error Dialog }
  deReadErrorDlgCaption  = deStrBase + 320;
  deReadErrorDlgMessage  = deStrBase + 321;

 { Package Components Dialog }
  dePakComponentsDlgCaption   = deStrBase + 330;
  dePakComponentsDlgInstalled = deStrBase + 331;

 { image resources}
  deImageBase         = 30000;

  deDefaultComponentBitmap = deImageBase + 501;

var
  BaseRegKey: string;
  ActiveModule: HModule;

implementation

{$R rafd.res}
{$R rafd2.res}


procedure RAFDError(const AErrCode : integer);
begin
  raise ERAFDError.Create(AErrCode, '', '');
end;    { RAFDError }

procedure RAFDErrorN(const AErrCode : integer; const AErrName : string);
begin
  raise ERAFDError.Create(AErrCode, AErrName, '');
end;    { RAFDErrorN }

procedure RAFDErrorN2(const AErrCode : integer; const AErrName1,
  AErrName2 : string);
begin
  raise ERAFDError.Create(AErrCode, AErrName1, AErrName2);
end;    { RAFDErrorN2 }

constructor ERAFDError.Create(const AErrCode : integer; const AErrName,
  AErrName2 : string);
begin
  inherited Create('');
  ErrCode := AErrCode;
  ErrName := AErrName;
  ErrName2 := AErrName2;
  // S := LoadStr(ErrCode + ResBase);
  { function LoadStr don't work sometimes :-( }
  Message := Format(ResStr(ErrCode + deResBase, ''), [ErrName, ErrName2]);
end;    { Create }

function ResStr(const ID: Integer; const Default: string): string;
begin
  SetLength(Result, 1024);
  SetLength(Result, LoadString(hInstance, ID, PChar(Result), Length(Result)));
  if Length(Result) = 0 then
    Result := Default;
end;    {  }


procedure LocaleMenuItem(MenuItem: TMenuItem; const CaptionID, HintID: Integer);
begin
  MenuItem.Caption := ResStr(CaptionID, MenuItem.Caption);
  if HintID <> deNoHint then
    MenuItem.Hint := ResStr(HintID, MenuItem.Hint);
end;


initialization
  ActiveModule := hInstance;
end.
