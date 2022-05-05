{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 2000 R&A

       class       : TRAFDRunner
       description : runner for forms, reports, units, projects 

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

{.$DEFINE DYNAMICRAI2} // old dynamic package loading

unit RAFDRun;

interface

uses Windows, SysUtils, Classes, Forms,
  RAI2, RAI2Fm;

type

  TRAFDRunner = class(TRAI2Fm)
  private
   {$IFDEF DYNAMICRAI2}
    RAIModule: HModule;
    RAIAModule: HModule;
    Proc: procedure;
   {$ENDIF DYNAMICRAI2}
  protected
   {$IFDEF DYNAMICRAI2}
    procedure DynamicRAI2Load;
   {$ENDIF DYNAMICRAI2}
    function GetUnitSource(UnitName: string; var Source: string): boolean;
      override;
    procedure CreateDfmStream(const UnitName: string; var Stream: TStream); override;
    procedure FreeDfmStream(Stream: TStream); override;
  public
   {$IFDEF DYNAMICRAI2}
    procedure DynamicRAI2RunFormModal(const FileName: TFileName);
    procedure DynamicRAI2RunReportPreview(const FileName: TFileName);
    procedure DynamicRAI2RunUnit(const FileName: TFileName);
   {$ENDIF DYNAMICRAI2}
  end;

implementation

uses RAUtils, RAFD, RAFDPalette, RAFDProjectManager, RAFDEditor, RAFDDesigner;

{$IFDEF DYNAMICRAI2}
const
 {$IFDEF RA_D3}
  RAIPackageFileName = 'rai3.dpl';
  RAIAPackageFileName = 'raia3.dpl';
 {$ENDIF}
 {$IFDEF RA_D4}
  RAIPackageFileName = 'rai4.bpl';
  RAIAPackageFileName = 'raia4.bpl';
 {$ENDIF}
 {$IFDEF RA_D5}
  RAIPackageFileName = 'rai5.bpl';
  RAIAPackageFileName = 'raia5.bpl';
 {$ENDIF}
{$ENDIF DYNAMICRAI2}

{$IFDEF DYNAMICRAI2}

procedure TRAFDRunner.DynamicRAI2Load;
begin
  if RAIModule = 0 then
    try
      DesignPackageList.Add(RAIPackageFileName, True, True, '');
      RAIModule := ActiveModule;
    except
      RAFDErrorN(deRAI2NotFound, RAIPackageFileName);
    end;
  if RAIAModule = 0 then
    try
      DesignPackageList.Add(RAIAPackageFileName, True, True, '');
      RAIAModule := ActiveModule;
    except
      RAFDErrorN(deRAI2NotFound, RAIAPackageFileName);
    end;
 {$IFDEF RA_D3}
  Proc := GetProcAddress(RAIAModule, ('RAI2_all.RAI2_all@00000000'));
 {$ELSE}
  Proc := GetProcAddress(RAIAModule, ('@Rai2_all@initialization$qqrv'));
 {$ENDIF}
  if not Assigned(Proc) then
    RAFDErrorN(deRAI2FuncNotFound, RAIAPackageFileName);
  Proc;
end;    { DynamicRAI2Load }

{ run form using RAI3.dpl package }
procedure TRAFDRunner.DynamicRAI2RunFormModal(const FileName: TFileName);
type
  TRAI2RunFormModal = function (const FileName: TFileName): TModalResult;
begin
  DynamicRAI2Load;
 {$IFDEF RA_D3}
  Proc := GetProcAddress(RAIModule, ('RAI2Fm.RAI2RunFormModal@A8853D1A'));
 {$ELSE}
  Proc := GetProcAddress(RAIModule, ('@Rai2fm@RAI2RunFormModal$qqrx17System@AnsiString'));
 {$ENDIF}
  if not Assigned(Proc) then
    RAFDErrorN(deRAI2FuncNotFound, RAIPackageFileName);
  TRAI2RunFormModal(Proc)(FileName);
end;    { DynamicRAI2RunFormModal }

{ run report using RAI3.dpl package }
procedure TRAFDRunner.DynamicRAI2RunReportPreview(const FileName: TFileName);
type
  TRAI2RunReportPreview = procedure (const FileName: TFileName);
begin
  DynamicRAI2Load;
 {$IFDEF RA_D3}
  Proc := GetProcAddress(RAIAModule, ('RAI2_Quickrpt.RAI2RunReportPreview@D425663E'));
 {$ELSE}
  Proc := GetProcAddress(RAIAModule, ('@Rai2_quickrpt@RAI2RunReportPreview$qqrx17System@AnsiString'));
 {$ENDIF}
  if not Assigned(Proc) then
    RAFDErrorN(deRAI2FuncNotFound, RAIAPackageFileName);
  TRAI2RunReportPreview(Proc)(FileName);
end;    { DynamicRAI2RunReportPreview }

{ run unit using RAI3.dpl package }
procedure TRAFDRunner.DynamicRAI2RunUnit(const FileName: TFileName);
type
  TRAI2RunUnit = function (const FileName: TFileName): Variant;
begin
  DynamicRAI2Load;
 {$IFDEF RA_D3}
  Proc := GetProcAddress(RAIModule, ('RAI2Fm.RAI2RunUnit@A8853D1A')); { not tested ! }
 {$ELSE}
  Proc := GetProcAddress(RAIModule, ('@Rai2fm@RAI2RunUnit$qqrx17System@AnsiString'));
 {$ENDIF}
  if not Assigned(Proc) then
    RAFDErrorN(deRAI2FuncNotFound, RAIPackageFileName);
  TRAI2RunUnit(Proc)(FileName);
end;    { DynamicRAI2RunUnit }

{$ENDIF DYNAMICRAI2}

function TRAFDRunner.GetUnitSource(UnitName: string;
  var Source: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to FileEditorList.Count - 1 do
    if Cmp(FileEditorList[i].FileName, UnitName) or
       ((ProjectManager.IsFileInProject(FileEditorList[i].FileName) or
         not ProjectManager.IsProjectOpen) and
       Cmp(ChangeFileExt(ExtractFileName(FileEditorList[i].FileName), ''),
       UnitName)) then
    begin
      Result := True;
      Source := FileEditorList[i].Lines.Text;
      Exit;
    end;
  Result := inherited GetUnitSource(UnitName, Source);
end;

procedure TRAFDRunner.CreateDfmStream(const UnitName: string; var Stream: TStream);
var
  i: Integer;
begin
  for i := 0 to FileEditorList.Count - 1 do
    if Cmp(FileEditorList[i].FileName, UnitName) or
       ((ProjectManager.IsFileInProject(FileEditorList[i].FileName) or
         not ProjectManager.IsProjectOpen) and
       Cmp(ChangeFileExt(ExtractFileName(FileEditorList[i].FileName), ''),
       UnitName)) then
    begin
      Stream := (FileEditorList[i].Designer as TRAFormDesigner).FormImage;
      Stream.Position := 0;
      Exit;
    end;
  inherited CreateDfmStream(UnitName, Stream);
end;

procedure TRAFDRunner.FreeDfmStream(Stream: TStream);
begin
  if not (Stream is TMemoryStream) then
    inherited FreeDfmStream(Stream);
end;


end.
