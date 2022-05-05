{$I teedefs.inc}
unit EditPro;

interface

Uses WinTypes,WinProcs,Controls,Chart,Teengine;

{ Show the user the dialog associated to the Series }
Procedure EditOneSeries(AOwner:TControl; ASeries:TChartSeries);

{ Show the user a "Save to..." dialog to save it to disk. }
Function SaveChartDialog(AChart:TCustomChart):String;


implementation

{ This unit should be "used" by your applications if showing the
  Chart Editor with "Extended" Series and Functions. }
uses StatChar,CandlEdi,TeeVolEd,SurfEdit,PolarEdi,ErrBarEd,CurvFitt,ContEdit,
     Classes,Forms,SysUtils,TeeConst,TeeProco,Dialogs,TeeStore,TeeBezie,
     Po3DEdit,TeeCount,TeeCumu,EditChar;

{$IFDEF D1}
{$R TeeProBM.R16}
{$ELSE}
{$R TeeProBM.RES}
{$ENDIF}

{ This method shows the "Save As..." dialog to save a Chart }
Function SaveChartDialog(AChart:TCustomChart):String;
begin
  result:='';
  With TSaveDialog.Create(nil) do
  try
    Filter:=TeeMsg_TeeFiles+
          ' (*.' +
          TeeMsg_TeeExtension +
          ')|*.' +
          TeeMsg_TeeExtension;
    Options:=[ofHideReadOnly,ofOverwritePrompt];
    DefaultExt:=TeeMsg_TeeExtension;
    if Execute then
    begin
      SaveChartToFile(AChart,FileName);
      result:=FileName;
    end;
  finally
    Free;
  end;
end;

{ This method shows the Series editor }
Procedure EditOneSeries(AOwner:TControl; ASeries:TChartSeries);
var tmpClass:TFormClass;
    tmp:TForm;
begin
  tmpClass:=TFormClass(GetClass(ASeries.GetEditorClass));
  if Assigned(tmpClass) then
  begin
    tmp:=tmpClass.Create(AOwner);
    With tmp do
    try
      Position:=poScreenCenter;
      BorderStyle:=bsDialog;
      BorderIcons:=[biSystemMenu];
      Scaled:=False;
      Caption:=Format(TeeMsg_Editing,[ASeries.Name]);
      Tag:=Longint(ASeries);
      Height:=Height+GetSystemMetrics(SM_CYDLGFRAME)+GetSystemMetrics(SM_CYCAPTION);
      ShowModal;
    finally
      Free;
    end;
  end
  else raise Exception.CreateFmt(TeeMsg_CannotFindEditor,[ASeries.GetEditorClass]);
end;

end.
