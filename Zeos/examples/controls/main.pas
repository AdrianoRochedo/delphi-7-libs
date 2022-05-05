unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ZAbstractRODataset, ZAbstractDataset, ZDataset, ZConnection,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, Grids, DBGrids, Mask, DBCtrls,
  ExtDlgs;

type
  TMainForm = class(TForm)
    ZConnection: TZConnection;
    ZPeople: TZQuery;
    Panel1: TPanel;
    Label1: TLabel;
    ZProtocol: TComboBox;
    Label2: TLabel;
    ZPassword: TEdit;
    Label3: TLabel;
    ZDatabase: TEdit;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    PageControl1: TPageControl;
    SpeedButton2: TSpeedButton;
    TabSheet1: TTabSheet;
    DBGrid: TDBGrid;
    DSPeople: TDataSource;
    DBNavigator1: TDBNavigator;
    DBId: TDBEdit;
    ZPeoplep_id: TSmallintField;
    ZPeoplep_name: TStringField;
    ZPeoplep_begin_work: TTimeField;
    ZPeoplep_end_work: TTimeField;
    ZPeoplep_picture: TBlobField;
    ZPeoplep_resume: TMemoField;
    ZPeoplep_redundant: TSmallintField;
    DBName: TDBEdit;
    DBBeginWorkTime: TDBEdit;
    DBResume: TDBMemo;
    DBPicture: TDBImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DBEndWorkTime: TDBEdit;
    Label9: TLabel;
    Label10: TLabel;
    LoadImageBtn: TButton;
    LoadResumeBtn: TButton;
    ZDepartment: TZReadOnlyQuery;
    DSDepartment: TDataSource;
    DBDepartment: TDBLookupComboBox;
    Label11: TLabel;
    ZPeoplep_dep_id: TSmallintField;
    ZPort: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    ZUername: TEdit;
    ZPeopledeprtment: TStringField;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure LoadResumeBtnClick(Sender: TObject);
    procedure LoadImageBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.SpeedButton1Click(Sender: TObject);
var
 ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(self);
  try
    ODialog.Filter := 'Interbase Database|*.gdb';
    if ODialog.Execute then
    begin
      ZDatabase.Text := ODialog.FileName;
      if ZConnection.Connected then
        ZConnection.Disconnect;
    end;
  finally
    ODialog.Free;
  end;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  with ZConnection do
  begin
    User := ZUername.Text;
    Password := ZPassword.Text;
    Protocol := ZProtocol.Text;
    if ZPort.Text <> '' then
      Port := StrToInt(ZPort.Text);
    Database := ZDatabase.Text;

    Connect;
    ZDepartment.Active := True;
    ZPeople.Active := True;
  end;
end;

procedure TMainForm.LoadResumeBtnClick(Sender: TObject);
var
 ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(self);
  try
    ODialog.Filter := 'Text documents|*.txt';
    if ODialog.Execute then
      DBResume.Lines.LoadFromFile(ODialog.FileName);
  finally
    ODialog.Free;
  end;
end;

procedure TMainForm.LoadImageBtnClick(Sender: TObject);
var
 ODialog: TOpenPictureDialog;
begin
  ODialog := TOpenPictureDialog.Create(self);
  try
    ODialog.Filter := 'Images|*.bmp';
    if ODialog.Execute then
      DBPicture.Picture.LoadFromFile(ODialog.FileName);
  finally
    ODialog.Free;
  end;
end;

end.
