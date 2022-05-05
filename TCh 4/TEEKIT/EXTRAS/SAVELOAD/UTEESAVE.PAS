unit uteesave;

interface

{ This form demo shows how to SAVE, EDIT and LOAD Chart components
  from *.TEE files.

  It's a mini-editor application.

  WARNING:  Turn OFF "Break on Exception" at "Options->Environment".
}
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, TeEngine, TeeShape, ExtCtrls, TeeProcs, Chart, StdCtrls,
  FileCtrl;

type
  TFormVirtual = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    ButtonSave: TButton;
    ButtonSaveAs: TButton;
    Button3: TButton;
    Chart1: TChart;
    ButtonUnZoom: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonSaveAsClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonUnZoomClick(Sender: TObject);
    procedure Chart1Zoom(Sender: TObject);
    procedure Chart1Scroll(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ChartFileName:String;
  end;

var
  FormVirtual: TFormVirtual;

implementation

{$R *.DFM}
uses teeStore,ChartPro,TeeLoper,EditChar,TeeCumu;

{ This example application can be used to create and edit TeeCharts
  saved on disk as *.tee files.

  In Delphi 3.0 you can add a TSeriesDataSet component and a DBGrid
  component to visually edit the Chart Series values.
}

procedure TFormVirtual.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
  ButtonSave.Enabled:=True;
end;

procedure TFormVirtual.FormCreate(Sender: TObject);
begin
  TeeEraseBack:=False;
  ChartFileName:='';
  DirectoryListBox1.Directory:='';
end;

Function YesNo(Const S:String):Boolean;
Var St:Array[0..255] of Char;
begin
  result:=Application.MessageBox(StrPCopy(St,S),'',MB_YESNO)=IDYES;
end;

{ Load a new Chart from disk... }
procedure TFormVirtual.FileListBox1DblClick(Sender: TObject);
var tmpChart:TCustomChart;
begin
  if (not ButtonSave.Enabled) or
     (YesNo('Chart is modified. Cancel changes?')) then
  begin
    ChartFileName:='';
    Chart1.Free;
    tmpChart:=TChart.Create(Self);
    try
      { WARNING: !!!!!
        If you get here because a "Property does not Exist" EXCEPTION,
        don't worry. Turn off "Break on Exception" at Compiler Options
        and press F9 again.
      }
      LoadChartFromfile(tmpChart,FileListBox1.FileName);
      ChartFileName:=FileListBox1.FileName;
    except
      on E:Exception do ShowMessage(E.Message);
    end;
    Chart1:=tmpChart as TChart;
    With Chart1 do
    begin
      OnZoom:=Chart1Zoom;
      OnScroll:=Chart1Scroll;
      Align:=alClient;
      Parent:=Self;
    end;
    ButtonSaveAs.Enabled:=True;
    ButtonUnZoom.Enabled:=(not Chart1.LeftAxis.Automatic) or
                          (not Chart1.RightAxis.Automatic) or
                          (not Chart1.TopAxis.Automatic) or
                          (not Chart1.BottomAxis.Automatic);
  end;
end;

{ Save the current Chart to disk. }
procedure TFormVirtual.ButtonSaveClick(Sender: TObject);
begin
  SaveChartToFile(Chart1,ChartFileName);
  ButtonSave.Enabled:=False;
end;

{ Save the current Chart as another *.tee file name... }
procedure TFormVirtual.ButtonSaveAsClick(Sender: TObject);
var tmp:String;
begin
  tmp:=SaveChartDialog(Chart1);
  if tmp<>'' then ChartFileName:=tmp;
  FileListBox1.Update;
  FileListBox1.FileName:=ChartFileName;
end;

procedure TFormVirtual.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TFormVirtual.ButtonUnZoomClick(Sender: TObject);
begin
  With Chart1 do
  begin
    LeftAxis.Automatic:=True;
    TopAxis.Automatic:=True;
    RightAxis.Automatic:=True;
    BottomAxis.Automatic:=True;
  end;
end;

procedure TFormVirtual.Chart1Zoom(Sender: TObject);
begin
  ButtonUnZoom.Enabled:=True;
end;

procedure TFormVirtual.Chart1Scroll(Sender: TObject);
begin
  ButtonUnZoom.Enabled:=True;
end;

initialization
  RegisterClasses([TCumulative]);
end.
