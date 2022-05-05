{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : New item dialog

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDNewItem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommCtrl, ComCtrls {$IFDEF RA_D4H}, ImgList {$ENDIF RA_D4H};

type
  TRAFDNewItem = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    ImageList: TImageList;
    TabControl: TTabControl;
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    ExpertList: TList;
    NewCaption: string;
  public
    { Public declarations }
  end;

  procedure Show;

implementation

uses RAUtils, ExptIntf, RAFD, RAFDIDE;

{$R *.DFM}

var
  Expert: TIExpert;
  
procedure Show;
var
  Start: Boolean;
begin
  Expert := nil;
  with TRAFDNewItem.Create(Application) do
    try
      Start := ShowModal = mrOK;
      if Start and Assigned(ListView.Selected) then
        Expert := TIExpert(ListView.Selected.Data);
    finally
      Free;
    end;    { try/finally }
  if Start and Assigned(Expert) then
  begin
    Expert.Execute;
  end;
end;

procedure TRAFDNewItem.FormCreate(Sender: TObject);
var
  Expert: TIExpert;
  i: integer;
  N: string;
begin
  Caption := ResStr(deNewItems, Caption);
  bOK.Caption := ResStr(deOK, bOK.Caption);
  bCancel.Caption := ResStr(deCancel, bCancel.Caption);
  TabControl.Tabs[0] := ResStr(deNew, TabControl.Tabs[0]);
  NewCaption := TabControl.Tabs[0];
  ExpertList := RAFDIDE.GetExpertList;
  for i := 0 to ExpertList.Count - 1 do
  begin
    Expert := TExpertRec(ExpertList[i]).Expert;
    if Assigned(Expert) and (Expert.GetStyle in [esForm, esProject]) then
    begin
      N := Expert.GetPage;
      if not Cmp(N, 'New') and (TabControl.Tabs.IndexOf(N) = -1) then
        TabControl.Tabs.Add(N);
    end;
  end;    { for }
  TabControlChange(nil);
end;

procedure TRAFDNewItem.ListViewDblClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
    ModalResult := mrOk;
end;

procedure TRAFDNewItem.TabControlChange(Sender: TObject);
var
  Expert: TIExpert;
  i: integer;
  Glyph: HIcon;
  Page: string;
  N: string;
begin
  ListView.Items.Clear;
  if TabControl.TabIndex = -1 then Exit;
  Page := TabControl.Tabs[TabControl.TabIndex];
  for i := 0 to ExpertList.Count - 1 do
  begin
    Expert := TExpertRec(ExpertList[i]).Expert;
    if Assigned(Expert) and (Expert.GetStyle in [esForm, esProject]) then
    begin
      N := Expert.GetPage;
      if Cmp(Page, N) or ((TabControl.TabIndex = 0) and Cmp(N, 'New')) then
        with ListView.Items.Add do
        begin
          Caption := Expert.GetName;
          if Expert.GetStyle in [esForm, esProject] then
          begin
            Glyph := Expert.GetGlyph;
            if Glyph <> 0 then
            begin
              ImageIndex := ImageList_AddIcon(ImageList.Handle, Glyph);
            end;
          end;
          Data := Expert;
        end;
    end;
  end;    { for }
  if ListView.Items.Count > 0 then
  begin
    ListView.Items[0].Selected := True;
    ListView.Items[0].Focused := True;
  end;
end;

end.
