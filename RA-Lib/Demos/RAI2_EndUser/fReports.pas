{$INCLUDE RA.INC}
unit fReports;

interface

{$IFDEF RA_D3H}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RACt, Buttons, RAButtons, RARegAuto, RAI2, RAI2Fm,
  QuickRpt {$IFDEF RA_D6H}, Variants {$ENDIF};
{$ELSE}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RACt, Buttons, RAButtons, RARegAuto, RAI2, RAI2Fm,
  QuickRep;
{$ENDIF}


type
  TReports = class(TForm)
    RAhtLabel1: TRAhtLabel;
    lbReports: TRAhtListBox;
    bReport: TButton;
    RegAuto2: TRegAuto;
    RAI2Fm1: TRAI2Fm;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bReportClick(Sender: TObject);
    procedure RAI2Fm1GetValue(Sender: TObject; Identifer: String;
      var Value: Variant; Args: TArgs; var Done: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure Show;

implementation

uses RAUtils, RAI2_all, RAI2_QuickRpt;

{$R *.DFM}

procedure Show;
begin
  with TReports.Create(Application) do
    try
      ShowModal;
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

procedure TReports.FormCreate(Sender: TObject);
var
  Ss, Ss2: TStringList;
  i: Integer;
begin
  RegAuto2.IniStrings.Clear;
  Ss := TStringList.Create;
  Ss2 := TStringList.Create;
  try
    ReadFolder(ExePath + 'Reports', '*.ini', Ss);
    Ss.Sort;
    for i := 0 to Ss.Count - 1 do    { Iterate }
    begin
      Ss2.LoadFromFile(ExePath + 'Reports\' + Ss[i]);
      RegAuto2.IniStrings.AddStrings(Ss2);
    end;    { for }
  finally { wrap up }
    Ss.Free;
    Ss2.Free;
  end;    { try/finally }

  RegAuto2.ReadSection('Reports', lbReports.Items);
  if lbReports.Items.Count = 0 then
    RAhtLabel1.Caption := 'There are no reports';
//  RAhtLabel1.Caption := '<b><c:Red>ERROR:<c:WindowText></b> Fail reading file <b>' + RegAuto2.IniFile;
end;

procedure RunReport(RAI2FmRunner: TRAI2Fm; const FileName: TFileName);
var
  F1: TFileName;
begin
  F1 := ChangeFileExt(AddPath(FileName, ExePath + 'Reports\'), '.pas');
  if not FileExists(F1) then
		raise Exception.Create('Can''t load report. '#13+
			'File "' + F1 + '" not found.');
  RAI2RunReportPreview(F1);
end;    { RunReport }

procedure TReports.bReportClick(Sender: TObject);
var
  F1: TFileName;
  T1: string;
begin
  if lbReports.ItemIndex < 0 then Exit;
  F1 := Trim(RegAuto2.ReadString(lbReports.Items[lbReports.ItemIndex], 'File', ''));
  T1 := Trim(RegAuto2.ReadString(lbReports.Items[lbReports.ItemIndex], 'Type', 'Report'));
  F1 := ChangeFileExt(AddPath(F1, ExePath + 'Reports\'), '.pas');
  if not FileExists(F1) then
		raise Exception.Create('Can''t load report. '#13+
			'File "' + F1 + '" not found.');
  if Cmp(T1, 'report') then
  begin
    RunReport(RAI2Fm1, F1);
  end
  else
  if Cmp(T1, 'formmodal') then
    RAI2Fm1.RunFormModal(F1)
  else
		raise Exception.Create('Unknown report type.');
end;

procedure TReports.RAI2Fm1GetValue(Sender: TObject; Identifer: String;
  var Value: Variant; Args: TArgs; var Done: Boolean);
var
  RAI2FmRunner: TRAI2Fm;
begin
  if Cmp(Identifer, 'RunReport') then
  begin
    RAI2FmRunner := TRAI2Fm.Create(Self);
    try
      RunReport(RAI2FmRunner, Args.Values[0]);
    finally { wrap up }
      RAI2FmRunner.Free;
    end;    { try/finally }
    Value := Null;
    Done := True;
  end;
end;

end.
