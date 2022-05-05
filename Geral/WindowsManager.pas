unit WindowsManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Extpanel, StdCtrls, MessageManager;

type
  Tform_WindowsManager = class(TForm, IMessageReceptor)
    lbJanelas: TListBox;
    Label1: TLabel;
    btnMostrarTodas: TButton;
    btnMostrar: TButton;
    btnRemoverTodas: TButton;
    btnRemover: TButton;
    Label2: TLabel;
    btnFechar: TButton;
    Panel1: TPanel;
    Vis: TImage;
    procedure btnMostrarClick(Sender: TObject);
    procedure btnMostrarTodasClick(Sender: TObject);
    procedure btnRemoverClick(Sender: TObject);
    procedure btnRemoverTodasClick(Sender: TObject);
    procedure btnFecharClick(Sender: TObject);
    procedure lbJanelasClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FList: TList;
    procedure ShowDescriptions;
    function getCount: Integer;
    function getForm(i: Integer): TForm;
    function ReceiveMessage(Const MSG: TadvMessage): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Remove(Window: TForm);
    function  Add(Window: TForm): Integer;

    procedure ShowManager;
    procedure ShowWindows;

    property  Windows[i: Integer]: TForm read getForm; default;
    property  Count: Integer read getCount;
  end;

implementation
uses WindowsManagerMessages;

{$R *.DFM}

{ Tform_WindowsManager }

constructor Tform_WindowsManager.Create;
begin
  inherited Create(nil);
  FList := TList.Create;
  GetMessageManager.RegisterMessage(WINMANAGER_ADD, self);
  GetMessageManager.RegisterMessage(WINMANAGER_REMOVE, self);
end;

destructor Tform_WindowsManager.Destroy;
begin
  Clear;
  FList.Free;
  GetMessageManager.UnRegisterMessage(WINMANAGER_ADD, self);
  GetMessageManager.UnRegisterMessage(WINMANAGER_REMOVE, self);
  inherited;
end;

function Tform_WindowsManager.ReceiveMessage(Const MSG: TadvMessage): Boolean;
begin
  if MSG.ID = WINMANAGER_ADD then
     Add(TForm(MSG.ParamAsObject(0))) else

  if MSG.ID = WINMANAGER_REMOVE then
     Remove(TForm(MSG.ParamAsObject(0)));
end;

function Tform_WindowsManager.Add(Window: TForm): Integer;
begin
  FList.Add(Window);
  if Visible then ShowDescriptions;
end;

procedure Tform_WindowsManager.Clear;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    begin
    TForm(FList[i]).Free;
    FList[i] := nil;
    end;

  FList.Pack;
  if Visible then ShowDescriptions;
end;

function Tform_WindowsManager.getCount: Integer;
begin
  Result := FList.Count;
end;

function Tform_WindowsManager.getForm(i: Integer): TForm;
begin
  Result := TForm(FList[i]);
end;

procedure Tform_WindowsManager.Remove(Window: TForm);
begin
  FList.Remove(Window);
  if Visible then ShowDescriptions;
end;

procedure Tform_WindowsManager.ShowManager;
begin
  Show;
  ShowDescriptions
end;

procedure Tform_WindowsManager.ShowWindows;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    TForm(FList[i]).Show;
end;

procedure Tform_WindowsManager.ShowDescriptions;
var i: Integer;
    s: String;
begin
  Vis.Canvas.Brush.Color := clBtnFace;
  Vis.Canvas.FillRect(Vis.ClientRect);

  lbJanelas.Clear;
  for i := 0 to FList.Count-1 do
    begin
    s := TForm(FList[i]).Caption;
    if s = '' then s := '(' + TForm(FList[i]).ClassName + ')';
    lbJanelas.Items.Add(s);
    end;
end;

procedure Tform_WindowsManager.btnMostrarClick(Sender: TObject);
begin
  if lbJanelas.ItemIndex > -1 then
     TForm(FList[lbJanelas.ItemIndex]).Show;
end;

procedure Tform_WindowsManager.btnMostrarTodasClick(Sender: TObject);
begin
  ShowWindows;
end;

procedure Tform_WindowsManager.btnRemoverClick(Sender: TObject);
var i: Integer;
begin
  i := lbJanelas.ItemIndex;
  if i > -1 then
     begin
     TForm(FList[i]).Free;
     FList.Remove(FList[i]);
     lbJanelas.Items.Delete(i);
     Vis.Canvas.Brush.Color := clBtnFace;
     Vis.Canvas.FillRect(Vis.ClientRect);
     end;
end;

procedure Tform_WindowsManager.btnRemoverTodasClick(Sender: TObject);
begin
  if MessageDLG('Tem certeza ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
     Clear;
end;

procedure Tform_WindowsManager.btnFecharClick(Sender: TObject);
begin
  Close;
end;

procedure Tform_WindowsManager.lbJanelasClick(Sender: TObject);
var b: TBitmap;
    r: TRect;
begin
  if lbJanelas.ItemIndex = -1 then exit;
  b := TForm(FList[lbJanelas.ItemIndex]).getFormImage;
  r := Vis.ClientRect;
  Vis.Canvas.StretchDraw(r, b);
  b.Free;
end;

procedure Tform_WindowsManager.FormActivate(Sender: TObject);
begin
  ShowDescriptions;
end;

end.
