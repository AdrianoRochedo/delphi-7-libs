unit Frame_Memo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, Book_Interfaces, StdCtrls;

type
  Tfr_Memo = class(TFrame, IBookPage, IPage, ITextPage)
    Memo: TMemo;
  private
    procedure setReadOnly(b: boolean);
    function getControl(): TWinControl;
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    procedure setFontName(const Name: string);
    procedure setFontColor(const Color: TColor);
    procedure setFontSize(const Size: integer);
    procedure Write(const s: string);
    procedure BeginUpdate();
    procedure EndUpdate();
  public
    { Public declarations }
  end;

  TFactory = class(TInterfacedObject, IPageFactory)
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

implementation
uses Book_Classes;

{$R *.dfm}

{ TTextDocument }

procedure Tfr_Memo.BeginUpdate();
begin
  Memo.Lines.BeginUpdate();
end;

procedure Tfr_Memo.EndUpdate();
begin
  Memo.Lines.EndUpdate();
end;

function Tfr_Memo.getControl(): TWinControl;
begin
  result := self;
end;

procedure Tfr_Memo.LoadFromFile(const Filename: string);
begin
  Memo.Lines.LoadFromFile(Filename);
end;

procedure Tfr_Memo.SaveToFile(const Filename: string);
begin
  Memo.Lines.SaveToFile(Filename);
end;

procedure Tfr_Memo.setFontColor(const Color: TColor);
begin
  Memo.Font.Color := Color;
end;

procedure Tfr_Memo.setFontName(const Name: string);
begin
  Memo.Font.Name := Name;
end;

procedure Tfr_Memo.setFontSize(const Size: integer);
begin
  Memo.Font.Size := Size;
end;

procedure Tfr_Memo.setReadOnly(b: boolean);
begin
  Memo.ReadOnly := b;
end;

procedure Tfr_Memo.Write(const s: string);
begin
  Memo.Lines.Add(s);
end;

{ TTextPageFactory }

function TFactory.CreatePage(aOwner: TComponent): IBookPage;
begin
  result := Tfr_Memo.Create(aOwner);
end;

initialization
  getPagesFactory.RegisterPage('memo', TFactory.Create());

end.
