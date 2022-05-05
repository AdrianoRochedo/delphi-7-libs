{*********************************************************}
{*                  EXENMFU.PAS 1.05                     *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit Exenmfu;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  AList : TStringList;

implementation

{$R *.DFM}

uses
  StConst,
  StBase,
  StUtils;

function TextFilesOnly(const SR : TSearchRec) : Boolean; far;
var
  S : String;
begin
  Result := False;
  S := SR.Name;
  if (UpperCase(ExtractFileExt(S)) = '.TXT') or
     ((SR.Attr and faDirectory) > 0) then
    Result := True;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  AList := TStringList.Create;
  Edit1.Text := 'C:\';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I : longint;
begin
  AList.Clear;
  ListBox1.Clear;

  Screen.Cursor := crHourGlass;
  if not CheckBox2.Checked then begin
    EnumerateFiles(Edit1.Text, AList, CheckBox1.Checked, nil);
    ListBox1.Items := AList;
  end else begin
    EnumerateFiles(Edit1.Text, AList, CheckBox1.Checked, TextFilesOnly);
    {by its nature, EnumerateFiles also returns directory names}
    {remove those from the list - leaving only the text files}
    for I := 0 to AList.Count-1 do
      if (UpperCase(ExtractFileExt(AList[I])) = '.TXT') then
        ListBox1.Items.Add(AList[I]);
  end;
  Screen.Cursor := crDefault;
  ListBox1.Update;
end;

end.
