unit Frame_RTF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Book_Interfaces, StdCtrls, ComCtrls;

type
  Tfr_RTF = class(TFrame, IBookPage, IPage, ITextPage)
    Memo: TRichEdit;
  private
    function getControl(): TWinControl;
  public
    procedure setReadOnly(b: boolean);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    procedure setFontName(const Name: string);
    procedure setFontColor(const Color: TColor);
    procedure setFontSize(const Size: integer);
    procedure Clear();
    procedure BeginUpdate();
    procedure EndUpdate();
    procedure Write(const s: string); overload;
    procedure Write(const s: string; out Line: integer); overload;
  end;

  TFactory = class(TInterfacedObject, IPageFactory)
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

implementation
uses Book_Classes;

{$R *.dfm}

{ TTextDocument }

procedure Tfr_RTF.BeginUpdate();
begin
  Memo.Lines.BeginUpdate();
end;

procedure Tfr_RTF.Clear();
begin
  Memo.Clear();
end;

procedure Tfr_RTF.EndUpdate();
begin
  Memo.Lines.EndUpdate();
end;

function Tfr_RTF.getControl(): TWinControl;
begin
  result := self;
end;

procedure Tfr_RTF.LoadFromFile(const Filename: string);
begin
  Memo.Lines.LoadFromFile(Filename);
end;

procedure Tfr_RTF.SaveToFile(const Filename: string);
begin
  Memo.Lines.SaveToFile(Filename);
end;

procedure Tfr_RTF.setFontColor(const Color: TColor);
begin
  Memo.SelAttributes.Color := Color;
end;

procedure Tfr_RTF.setFontName(const Name: string);
begin
  Memo.SelAttributes.Name := Name;
end;

procedure Tfr_RTF.setFontSize(const Size: integer);
begin
  Memo.SelAttributes.Size := Size;
end;

procedure Tfr_RTF.setReadOnly(b: boolean);
begin
  Memo.ReadOnly := b;
end;

procedure Tfr_RTF.Write(const s: string);
begin
  Memo.Lines.Add(s);
end;

procedure Tfr_RTF.Write(const s: string; out Line: integer);
begin
  Memo.Lines.Add(s);
  Line := Memo.Lines.Count - 1;
end;

{ TTextPageFactory }

function TFactory.CreatePage(aOwner: TComponent): IBookPage;
begin
  result := Tfr_RTF.Create(aOwner);
end;

initialization
  getPagesFactory.RegisterPage('RTF', TFactory.Create());

end.
