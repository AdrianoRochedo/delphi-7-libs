{: Utility to pack on or many DEM sources into a single HTF.<p>

   Note: this is a *basic* tool, error messages are unfriendly and there are
         memory leaks if you do any, I know. So, don't do errors ;)<p> 

   Requires Brad Stowers' BrowseDirectoryDialog component<br>
   (http://www.delphifreestuff.com)
}
unit FMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ValEdit, Grids, Menus, StdCtrls, ComCtrls, ToolWin, ExtCtrls,
  ActnList, ImgList, BrowseDr;

type
   TSrc = record
      fs : TFileStream;
      x, y, w, h : Integer;
      format : Integer;     
      
   end;
   PSrc = ^TSrc;

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    StringGrid: TStringGrid;
    File1: TMenuItem;
    ActionList: TActionList;
    ImageList: TImageList;
    ACOpen: TAction;
    ACSave: TAction;
    ACExit: TAction;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EDHTFName: TEdit;
    EDDEMPath: TEdit;
    BUDEMPath: TButton;
    Button3: TButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    DEMs1: TMenuItem;
    ACNewDEM: TAction;
    ACRemoveDEM: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    AddDEMsource1: TMenuItem;
    RemoveDEMsource1: TMenuItem;
    BDDDEMs: TdfsBrowseDirectoryDlg;
    SDHTF: TSaveDialog;
    PopupMenu: TPopupMenu;
    AddDEMsource2: TMenuItem;
    RemoveDEMsource2: TMenuItem;
    MIAbout: TMenuItem;
    CBType: TComboBox;
    CBFile: TComboBox;
    Label3: TLabel;
    EDSizeX: TEdit;
    EDSizeY: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EDDefaultZ: TEdit;
    ODTerrainPack: TOpenDialog;
    SDTerrainPack: TSaveDialog;
    ToolButton6: TToolButton;
    ACProcess: TAction;
    ToolButton7: TToolButton;
    N2: TMenuItem;
    Process1: TMenuItem;
    Panel2: TPanel;
    ProgressBar: TProgressBar;
    EDTileSize: TEdit;
    Label6: TLabel;
    ToolButton8: TToolButton;
    ACViewer: TAction;
    N3: TMenuItem;
    HTFViewer1: TMenuItem;
    ToolButton9: TToolButton;
    procedure ACExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BUDEMPathClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ACNewDEMExecute(Sender: TObject);
    procedure ACRemoveDEMExecute(Sender: TObject);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure CBTypeChange(Sender: TObject);
    procedure ACSaveExecute(Sender: TObject);
    procedure ACOpenExecute(Sender: TObject);
    procedure EDDEMPathChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDDefaultZChange(Sender: TObject);
    procedure ACProcessExecute(Sender: TObject);
    procedure ACViewerExecute(Sender: TObject);
  private
    { Private declarations }
    sources : array of TSrc;
    defaultZ : SmallInt;

    procedure Parse;
    procedure Cleanup;
    procedure SrcExtract(src : PSrc; relX, relY, len : Integer; dest : PSmallInt);
    procedure WorldExtract(x, y, len : Integer; dest : PSmallInt);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses HeightTileFile, Math, FViewerForm;

procedure TMainForm.FormCreate(Sender: TObject);
var
   i : Integer;
begin
   with ActionList do
      for i:=0 to ActionCount-1 do with TAction(Actions[i]) do
         Hint:=Caption;
   with StringGrid do begin
      Cells[0, 0]:='File Name';
      Cells[1, 0]:='World Offset';
      Cells[2, 0]:='Size';
      Cells[3, 0]:='Data type';
      Row:=0;
   end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
   Cleanup;
end;

procedure TMainForm.ACExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TMainForm.BUDEMPathClick(Sender: TObject);
begin
   BDDDEMs.Selection:=EDDEMPath.Text;
   if BDDDEMs.Execute then
      EDDEMPath.Text:=BDDDEMs.Selection;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
   SDHTF.FileName:=EDHTFName.Text;
   if SDHTF.Execute then
      EDHTFName.Text:=SDHTF.FileName;
end;

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
   ShowMessage(Caption+#13#10#13#10
               +'HTF Generation Utility'#13#10
               +'Part of GLScene library.'#13#10#13#10
               +'http://glscene.org');
end;

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
   ACRemoveDEM.Enabled:=(StringGrid.RowCount>2);
end;

procedure TMainForm.ACNewDEMExecute(Sender: TObject);
begin
   StringGrid.RowCount:=StringGrid.RowCount+1;
end;

procedure TMainForm.ACRemoveDEMExecute(Sender: TObject);
var
   i : Integer;
begin
   with StringGrid do begin
      i:=Row;
      if i<RowCount-1 then begin
         while i<RowCount-1 do begin
            Rows[i]:=Rows[i+1];
            Inc(i);
         end;
      end else Row:=i-1;
      RowCount:=RowCount-1;
   end;
end;

procedure TMainForm.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);

   procedure SetCB(const cb : TComboBox);
   var
      r : TRect;
      i : Integer;
   begin
      r:=StringGrid.CellRect(ACol, ARow);
      cb.Left:=r.Left+StringGrid.Left;
      cb.Top:=r.Top+StringGrid.Top;
      cb.Width:=r.Right+1-r.Left;
      i:=cb.Items.IndexOf(StringGrid.Cells[ACol, ARow]);
      if i>=0 then
         cb.ItemIndex:=i
      else cb.Text:=StringGrid.Cells[ACol, ARow];
      if Visible then
         cb.SetFocus;
   end;

begin
   if ARow>0 then begin
      if ACol=0 then begin
         CBFile.Visible:=True;
         SetCB(CBFile);
      end else CBFile.Visible:=False;
      if ACol=3 then begin
         CBType.Visible:=True;
         SetCB(CBType);
      end else CBType.Visible:=False;
      CanSelect:=True;
   end;
end;

procedure TMainForm.CBTypeChange(Sender: TObject);
begin
   with StringGrid do
      Cells[Col, Row]:=(Sender as TComboBox).Text;
end;

procedure TMainForm.ACSaveExecute(Sender: TObject);
var
   i : Integer;
   sl, sg : TStringList;
begin
   if SDTerrainPack.Execute then begin
      sl:=TStringList.Create;
      with sl do begin
         Values['HTFName']:=EDHTFName.Text;
         Values['WorldSizeX']:=EDSizeX.Text;
         Values['WorldSizeY']:=EDSizeY.Text;
         Values['TileSize']:=EDTileSize.Text;
         Values['DefaultZ']:=EDDefaultZ.Text;
         Values['DEMPath']:=EDDEMPath.Text;
         sg:=TStringList.Create;
         for i:=1 to StringGrid.RowCount-1 do
            sg.Add(StringGrid.Rows[i].CommaText);
         Values['DEMs']:=sg.CommaText;
         sg.Free;
      end;
      sl.SaveToFile(SDTerrainPack.FileName);
      sl.Free;
   end;
end;

procedure TMainForm.ACOpenExecute(Sender: TObject);
var
   i : Integer;
   sl, sg : TStringList;
begin
   if ODTerrainPack.Execute then begin
      sl:=TStringList.Create;
      sl.LoadFromFile(ODTerrainPack.FileName);
      with sl do begin
         EDHTFName.Text:=Values['HTFName'];
         EDSizeX.Text:=Values['WorldSizeX'];
         EDSizeY.Text:=Values['WorldSizeY'];
         EDTileSize.Text:=Values['TileSize'];
         EDDefaultZ.Text:=Values['DefaultZ'];
         EDDEMPath.Text:=Values['DEMPath'];
         sg:=TStringList.Create;
         sg.CommaText:=Values['DEMs'];
         StringGrid.RowCount:=sg.Count+1;
         for i:=0 to sg.Count-1 do
            StringGrid.Rows[i+1].CommaText:=sg[i];
         sg.Free;
      end;
      sl.Free;
      SDTerrainPack.FileName:=ODTerrainPack.FileName;
   end;
end;

procedure TMainForm.EDDEMPathChange(Sender: TObject);
var
   f : TSearchRec;
   r : Integer;
begin
   CBFile.Items.Clear;
   r:=FindFirst(EDDEMPath.Text+'\*.*', faAnyFile, f);
   while r=0 do begin
      if (f.Attr and faDirectory)=0 then
         CBFile.Items.Add(f.Name);
      r:=FindNext(f);
   end;
   FindClose(f);
end;

procedure TMainForm.EDDefaultZChange(Sender: TObject);
begin
   defaultZ:=StrToIntDef(EDDefaultZ.Text, 0);
end;

procedure TMainForm.Parse;
var
   i, p : Integer;
   row : TStrings;
begin
   Cleanup;
   SetLength(sources, StringGrid.RowCount-1);
   for i:=0 to High(sources) do begin
      row:=StringGrid.Rows[i+1];
      sources[i].fs:=TFileStream.Create(EDDEMPath.Text+'\'+row[0], fmOpenRead+fmShareDenyNone);
      p:=Pos(',', row[1]);
      sources[i].x:=StrToInt(Copy(row[1], 1, p-1));
      sources[i].y:=StrToInt(Copy(row[1], p+1, MaxInt));
      p:=Pos('x', row[2]);
      sources[i].w:=StrToInt(Copy(row[2], 1, p-1));
      sources[i].h:=StrToInt(Copy(row[2], p+1, MaxInt));
      if Pos('non-', row[3])>0 then
         sources[i].format:=1
      else if Pos('BT', row[3])>0 then
         sources[i].format:=2
      else sources[i].format:=0;
   end;
end;

procedure TMainForm.Cleanup;
var
   i : Integer;
begin
   for i:=0 to High(sources) do
      sources[i].fs.Free;
   SetLength(sources, 0);
end;

procedure TMainForm.SrcExtract(src : PSrc; relX, relY, len : Integer; dest : PSmallInt);
var
   i : Integer;
   wd : Word;
   buf : array of Single;
begin
   with src^ do begin
      case format of
         0 : begin // 16bits Intel
            fs.Position:=(relX+relY*w)*2;
            fs.Read(dest^, len*2);
         end;
         1 : begin // 16bits non-Intel
            fs.Position:=(relX+relY*w)*2;
            fs.Read(dest^, len*2);
            for i:=0 to len-1 do begin
               wd:=PWord(Integer(dest)+i*2)^;
               PWord(Integer(dest)+i*2)^:=((wd and 255) shl 8)+(wd shr 8);
            end;
         end;
         2 : begin // VTP's BT single
            fs.Position:=(relX+relY*w)*4+256;
            SetLength(buf, len);
            fs.Read(buf[0], len*4);
            for i:=0 to len-1 do
               PSmallInt(Integer(dest)+i*2)^:=Round(buf[i]);
         end;
      end;
   end;
end;

procedure TMainForm.WorldExtract(x, y, len : Integer; dest : PSmallInt);
var
   i, n, rx, ry : Integer;
   src : PSrc;
begin
   while len>0 do begin
      src:=nil;
      for i:=0 to High(sources) do begin
         if (sources[i].x<=x) and (sources[i].y<=y)
               and (x<sources[i].x+sources[i].w)
               and (y<sources[i].y+sources[i].h) then begin
            src:=@sources[i];
            Break;
         end;
      end;
      if Assigned(src) then begin
         rx:=x-src.x;
         ry:=y-src.y;
         n:=len;
         if rx+n>src.w then
            n:=src.w-rx;
         SrcExtract(src, rx, ry, n, dest);
         Dec(len, n);
         Inc(dest, n);
         Inc(x, n);
      end else begin
         dest^:=defaultZ;
         Inc(dest);
         Dec(len);
         Inc(x);
      end;
   end;
end;

procedure TMainForm.ACProcessExecute(Sender: TObject);
var
   x, y, wx, wy, ts, tx, ty, i : Integer;
   n, maxN : Cardinal;
   htf : THeightTileFile;
   buf : array of SmallInt;
   f : file of Byte;
begin
   Screen.Cursor:=crHourGlass;
   
   wx:=StrToInt(EDSizeX.Text);
   wy:=StrToInt(EDSizeY.Text);
   ts:=StrToInt(EDTileSize.Text);
   Parse;
   SetLength(buf, ts*ts);
   htf:=THeightTileFile.CreateNew(EDHTFName.Text, wx, wy, ts);
   htf.DefaultZ:=defaultZ;
   ProgressBar.Max:=1000;
   maxN:=Ceil(wx/ts)*Ceil(wy/ts);
   n:=0;
   ProgressBar.Position:=0;
   y:=0; while y<wy do begin
      ty:=wy-y;
      if ty>ts then ty:=ts;
      x:=0; while x<wx do begin
         tx:=wx-x;
         if tx>ts then tx:=ts;
         Inc(n);
         ProgressBar.Position:=(n*1000) div maxN;
         for i:=0 to ty-1 do
            WorldExtract(x, y+i, tx, @buf[i*tx]);
         htf.CompressTile(x, y, tx, ty, @buf[0]);
         Inc(x, ts);
         if (n and 15)=0 then begin
            Application.ProcessMessages;
         end;
      end;
      Inc(y, ts);
   end;
   htf.Free;
   Cleanup;

   Screen.Cursor:=crDefault;

   AssignFile(f, EDHTFName.Text);
   Reset(f);
   i:=FileSize(f);
   CloseFile(f);

   ShowMessage( 'HTF file created.'#13#10#13#10
               +IntToStr(i)+' bytes in file'#13#10
               +'('+IntToStr(wx*wy*2)+' raw bytes)');

end;

procedure TMainForm.ACViewerExecute(Sender: TObject);
var
   viewer : TViewerForm;
begin
   viewer:=TViewerForm.Create(nil);
   try
      viewer.ShowModal;
   finally
      viewer.Free;
   end;
end;

end.
