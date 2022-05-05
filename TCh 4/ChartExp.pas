{**********************************************}
{   TeeChart Wizard                            }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit ChartExp;

interface

uses
{  ShareMem, }
  Forms,
  Windows,
  ExptIntf,
  ToolIntf,
  VirtIntf,
  SysUtils,
  ExpForm,
  TeeConst;

type
  TTeeChartWizard = class(TIExpert)
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetGlyph: HICON; override;
    function GetStyle: TExpertStyle; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetPage: string; override;
    procedure Execute; override;
    function GetMenuText: string; override;
  end;

Procedure Register;

implementation

{$R CHAEXPER.RES}

procedure HandleException;
begin
  ToolServices.RaiseException(ReleaseException);
end;

{ TTeeChartWizard }
function TTeeChartWizard.GetName: string;
begin
  try
    Result := TeeMsg_TeeChartWizard ;
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetComment: string;
begin
  try
    Result :=  TeeMsg_TeeChartWizard ;
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetGlyph: HICON;
begin
  result:=0;
  try
    Result := LoadIcon(HInstance, 'TEEEXPICON');
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetStyle: TExpertStyle;
begin
  Result := esForm;
  try
    Result := esForm;
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetState: TExpertState;
begin
  try
    Result := [esEnabled];
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetIDString: string;
begin
  try
    Result :=  TeeMsg_TeeMachWizard ;
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetAuthor: string;
begin
  try
    Result := TeeMsg_TeeMachSL ;
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetPage: string;
begin
  try
    Result :=  TeeMsg_WizardTab ;
  except
    HandleException;
  end;
end;

procedure TTeeChartWizard.Execute;
begin
  try
    TeeChartWizard(ToolServices);
  except
    HandleException;
  end;
end;

function TTeeChartWizard.GetMenuText: string;
begin
  result:='';
end;

procedure DoneWizard; export;
begin
  { Put any general destruction code here.  Note that the Delphi IDE
    will destroy any Wizards which have been registered. }
end;

function InitWizard(ToolServices: TIToolServices;
  RegisterProc: TExpertRegisterProc;
  var Terminate: TExpertTerminateProc): Boolean; export; stdcall;
begin
{$IFDEF D3}
  result:=True;
{$ELSE}
  { make sure we are the first and only instance }
  Result := ExptIntf.ToolServices = nil;
  if not Result then Exit;
{$ENDIF}

  ExptIntf.ToolServices := ToolServices;
  if ToolServices <> nil then
    Application.Handle := ToolServices.GetParentHandle;

  Terminate := DoneWizard;

  { register the Wizards }
  RegisterProc(TTeeChartWizard.Create);
end;

Procedure Register;
begin
  RegisterLibraryExpert(TTeeChartWizard.Create);
end;

end.
