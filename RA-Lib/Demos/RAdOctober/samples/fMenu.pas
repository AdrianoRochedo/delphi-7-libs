unit fMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button1: TButton;
    Empty: TPopupMenu;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Reopen1: TMenuItem;
    File2: TMenuItem;
    New2: TMenuItem;
    Open2: TMenuItem;
    Save2: TMenuItem;
    SaveAs2: TMenuItem;
    N7: TMenuItem;
    Print2: TMenuItem;
    PrintSetup2: TMenuItem;
    N6: TMenuItem;
    Exit2: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Repeatcommand1: TMenuItem;
    N10: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    N9: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    N8: TMenuItem;
    Links1: TMenuItem;
    Object1: TMenuItem;
    Window2: TMenuItem;
    NewWindow2: TMenuItem;
    Tile2: TMenuItem;
    Cascade2: TMenuItem;
    ArrangeAll2: TMenuItem;
    N11: TMenuItem;
    Help3: TMenuItem;
    Contents3: TMenuItem;
    SearchforHelpOn3: TMenuItem;
    HowtoUseHelp3: TMenuItem;
    About3: TMenuItem;
    Hide2: TMenuItem;
    Show2: TMenuItem;
    Help2: TMenuItem;
    Contents2: TMenuItem;
    Index2: TMenuItem;
    Commands2: TMenuItem;
    Procedures2: TMenuItem;
    Keyboard2: TMenuItem;
    SearchforHelpOn2: TMenuItem;
    Tutorial2: TMenuItem;
    HowtoUseHelp2: TMenuItem;
    About2: TMenuItem;
    File11: TMenuItem;
    File21: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N4: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    N5: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    NewItem1: TMenuItem;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Open1Click(Sender: TObject);
begin
 {   Open1.Checked := not Open1.Checked; }
  if Open1.Checked then
    Open1.Checked := False
  else
    Open1.Checked := True;
end;


procedure TForm1.File1Click(Sender: TObject);
begin
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Open1.Checked := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Open1.Checked := False;
end;

end.
