{**********************************************}
{   TeeChart Pro and TeeTree VCL About Form    }
{   Copyright (c) 1995-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeeAbout;

interface

uses
  Wintypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type TTeeProduct=record Description:String; Price,Ship:Integer; end;

  TTeeAboutForm = class(TForm)
    BClose: TButton;
    LabelVersion: TLabel;
    BNext: TButton;
    BPrevious: TButton;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    Notebook1: TNotebook;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    LabelSourceCode: TLabel;
    Labelwww: TLabel;
    LabelVersions: TLabel;
    GroupBox2: TGroupBox;
    Label9: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label16: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    Label17: TLabel;
    EAddress: TEdit;
    ECity: TEdit;
    EState: TEdit;
    EZIP: TEdit;
    ECountry: TEdit;
    EPhone: TEdit;
    EEMail: TEdit;
    ECompany: TEdit;
    EName: TEdit;
    EFAX: TEdit;
    GPCard: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    ECardNumber: TEdit;
    ECardDate: TEdit;
    LabelThanks: TLabel;
    BPrintOrder: TButton;
    BSaveOrder: TButton;
    BCopyOrder: TButton;
    RGLicense: TRadioGroup;
    Panel3: TPanel;
    Label31: TLabel;
    SEQuantity: TEdit;
    Label32: TLabel;
    Panel4: TPanel;
    Label23: TLabel;
    LTotal: TLabel;
    Label18: TLabel;
    ECardHolder: TEdit;
    CBPaymentType: TRadioGroup;
    CBShip: TComboBox;
    LShip: TLabel;
    Label6: TLabel;
    EECTax: TEdit;
    Panel2: TPanel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    Label27: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label22: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label35: TLabel;
    Label38: TLabel;
    Memo1: TMemo;
    Label39: TLabel;
    BOrder: TBitBtn;
    Label40: TLabel;
    Label41: TLabel;
    UDQuantity: TUpDown;
    Image2: TImage;
    procedure BCloseClick(Sender: TObject);
    procedure BOrderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BNextClick(Sender: TObject);
    procedure BPreviousClick(Sender: TObject);
    procedure BPrintOrderClick(Sender: TObject);
    procedure BSaveOrderClick(Sender: TObject);
    procedure BCopyOrderClick(Sender: TObject);
    procedure SEQuantityChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RGLicenseClick(Sender: TObject);
    procedure CBPaymentTypeChange(Sender: TObject);
    procedure CBShipChange(Sender: TObject);
    procedure ENameKeyPress(Sender: TObject; var Key: Char);
    procedure LabelwwwClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OrderProcessed:Boolean;
    ProductCount:Integer;
    TeeProducts:Array[0..10] of TTeeProduct;
    ShowOrderInfo:Boolean;
    procedure CheckButtons;
    Function GetOrderForm:TStringList;
    Function FormatUSD(Value:Longint):String;
    Function ShippingPrice:Longint;
    procedure RefreshTotal;
    Function CalcTotal:Longint;
    Function NumCopies:Longint;
    Procedure ClearProducts;
    Procedure AddProduct(Const ADesc:String; APrice,AShip:Integer);
  end;

{$IFNDEF D1}
Procedure GotoURL(Handle:Integer; Const URL:String);
{$ENDIF}

Const TeeDefProductName:String='TeeChart Pro 4.0 VCL';

{ Displays the TeeChart about-box dialog }
Procedure TeeShowAboutBox(ShowOrder:Boolean);

implementation

{$R *.DFM}
Uses TeeConst,TeeProcs,Printers,PenDlg,ClipBrd
     {$IFNDEF D1}
     ,ShellAPI
     {$ENDIF}
     {$IFDEF TEETRIAL}
     ,TeCanvas
     {$ENDIF}
     ;

Procedure TeeShowAboutBox(ShowOrder:Boolean);
begin
  With TTeeAboutForm.Create(nil) do
  try
    ShowOrderInfo:=ShowOrder;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure TTeeAboutForm.ClearProducts;
begin
  ProductCount:=0;
  RGLicense.Items.Clear;
end;

Procedure TTeeAboutForm.AddProduct(Const ADesc:String; APrice,AShip:Integer);
begin
  if ProductCount<11 then
  begin
    With TeeProducts[ProductCount] do
    begin
      Description:=ADesc;
      Price:=APrice;
      Ship:=AShip;
    end;
    if RGLicense.Items.IndexOf(ADesc)=-1 then
       RGLicense.Items.Add(ADesc);
    Inc(ProductCount);
  end;
end;

procedure TTeeAboutForm.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TTeeAboutForm.BOrderClick(Sender: TObject);
begin
  NoteBook1.PageIndex:=1;
  BPrevious.Visible:=True;
  BNext.Visible:=True;
  CheckButtons;
end;

procedure TTeeAboutForm.FormCreate(Sender: TObject);
begin
  {$IFDEF COMPOSOURCE}
  BPrevious.Visible:=False;
  BNext.Visible:=False;
  {$ENDIF}
  ShowOrderInfo:=True;
  With RGLicense do
  begin   { default products and prices }
    ProductCount:=0;
    AddProduct(Items[0],13900,0);
    AddProduct(Items[1],27900,0);
    AddProduct(Items[2], 3900,0);
    AddProduct(Items[3], 7000,0);
  end;
  {$IFNDEF D1}
  {$IFDEF D2C1}
  LabelWWW.Cursor:=crTeeHand;
  {$ELSE}
  LabelWWW.Cursor:=crHandPoint;
  {$ENDIF}
  {$ENDIF}
  OrderProcessed:=False;
  NoteBook1.PageIndex:=0;
  LabelVersion.Caption:=TeeMsg_Version;
  CBShip.ItemIndex:=0;
  SEQuantityChange(Self);
end;

procedure TTeeAboutForm.CheckButtons;
begin
  NoteBook1.Color:=clBtnFace;
  BPrevious.Enabled:=NoteBook1.PageIndex>1;
  BNext.Enabled:=NoteBook1.PageIndex<4;
  if NoteBook1.PageIndex=1 then RGLicense.SetFocus else
  if NoteBook1.PageIndex=2 then EName.SetFocus else
  if NoteBook1.PageIndex=3 then EECTax.SetFocus else
  if NoteBook1.PageIndex=4 then BPrintOrder.SetFocus;
end;

procedure TTeeAboutForm.BNextClick(Sender: TObject);
begin
  With NoteBook1 do
  if PageIndex<4 then PageIndex:=PageIndex+1;
  CheckButtons;
end;

procedure TTeeAboutForm.BPreviousClick(Sender: TObject);
begin
  With NoteBook1 do
  if PageIndex>1 then PageIndex:=PageIndex-1;
  CheckButtons;
end;

Function TTeeAboutForm.FormatUSD(Value:Longint):String;
begin
  if (Value mod 100)=0 then
     result:=Format('%6.0f US$',[Value/100.0])  { <-- don't translate }
  else
     result:=Format('%6.2f US$',[Value/100.0]);  { <-- don't translate }
end;

Function TTeeAboutForm.CalcTotal:Longint;
begin
  result:=TeeProducts[RGLicense.ItemIndex].Price;
  if NumCopies>1 then result:=result*NumCopies;
  result:=result+ShippingPrice;
end;

Function TTeeAboutForm.NumCopies:Longint;
begin
  result:=UDQuantity.Position;
end;

{ Don't translate "GetOrderForm" method }
Function TTeeAboutForm.GetOrderForm:TStringList;
Const Separator='---------------------------------------------------------------';
Var tmpTotal:Longint;
    tmpSt:String;
begin
  result:=TStringList.Create;
  With result do
  begin
    Add('');
    Add('  *************************************');
    Add('    '+TeeDefProductName+'  -ORDER FORM-');
    Add('  *************************************');
    Add('');
    Add(' WWW: http://www.teemach.com');
    Add('');
    if CBShip.ItemIndex>0 then
       Add('Please mail me '+TeeDefProductName+' to this address:');
    Add('');
    Add('  Customer Information:');
    Add(Separator);
    Add('  Name....: '+EName.Text);
    Add('  Company.: '+ECompany.Text);
    Add('  Address.: '+EAddress.Text);
    Add('  City....: '+ECity.Text);
    Add('  State...: '+EState.Text);
    Add('  ZIP.....: '+EZIP.Text);
    Add('  Country.: '+ECountry.Text);
    Add('  Phone...: '+EPhone.Text);
    Add('  FAX.....: '+EFAX.Text);
    Add('  EMail...: '+EEMail.Text);
    Add('  TAX ID..: '+EECTax.Text);
    Add('');
    Add('Payment Type: '+CBPaymentType.Items[CBPaymentType.ItemIndex]);
    Add('');
    if CBPaymentType.ItemIndex<3 then
    begin
      Add('  Credit Card Information:');
      Add(Separator);
      Add('  Credit Card Type.......: '+CBPaymentType.Items[CBPaymentType.ItemIndex]);
      Add('  Credit CardHolder Name.: '+ECardHolder.Text);
      Add('  Credit Card Number.....: '+ECardNumber.Text);
      Add('  Credit Card Exp. Date..: '+ECardDate.Text);
    end;
    Add('');
    Add('  Order Product:');
    Add(Separator);
    tmpSt:=TeeProducts[RGLicense.ItemIndex].Description;
    tmpTotal:=TeeProducts[RGLicense.ItemIndex].Price;
    if NumCopies=1 then
       Add('      '+tmpSt+': '+FormatUSD(tmpTotal))
    else
    begin
      tmpTotal:=tmpTotal*NumCopies;
      Add('  '+tmpSt+' x '+IntToStr(NumCopies)+': '+FormatUSD(tmpTotal));
    end;
    Add('         Shipping method: '+CBShip.Items[CBShip.ItemIndex]);
    Add('     Shipping & Handling: '+FormatUSD(ShippingPrice));

    Add(Separator);
    Add('          T O T A L     : '+FormatUSD(CalcTotal));
    Add(Separator);
    Case CBPaymentType.ItemIndex of
      6: Add('Warning: EuroCheques must be in Spanish Pesetas. (NOT in US$)');
    end;
    Add('');
    Add('FAX this Order Form to us at FAX NUMBER:  +34 93 423 59 82');
    Add('Or send by Internet EMail to: sales@teemach.com');
    Add('Or send by postal mail to:');
    Add('  teeMach, SL');
    Add('  PO Box 22262');
    Add('  08080 Barcelona, Catalonia');
    Add('  Spain');
    Add(Separator);
    Add('');
    Add('  Please check your email address before sending your order.');
    Add('  You will receive an email with PASSWORD/KEY instructions ');
    Add('  to DOWNLOAD '+TeeDefProductName);
    Add('  Please contact us at sales@teemach.com for any assistance.');
    Add(Separator);
  end;
end;

procedure TTeeAboutForm.BPrintOrderClick(Sender: TObject);
var f:TextFile;
    t:Longint;
begin
  if PrintDialog1.Execute then
  With GetOrderForm do
  try
    AssignPrn(f);
    Rewrite(f);
    Screen.Cursor:=crHourGlass;
    try
      try
        for t:=0 to Count-1 do Writeln(f, Strings[t]);
      finally
        Screen.Cursor:=crDefault;
        System.CloseFile(f);
      end;
      OrderProcessed:=True;
    except
      on Exception do Screen.Cursor:=crDefault;
    end;
  finally
    Free;
  end;
end;

procedure TTeeAboutForm.BSaveOrderClick(Sender: TObject);
Var St:Array[0..255] of char;
begin
  if SaveDialog1.Execute then
  With GetOrderForm do
  try
    SaveToFile(SaveDialog1.FileName);
    WinExec(StrPCopy(St,'notepad '+SaveDialog1.FileName),SW_SHOWMAXIMIZED); { <-- don't translate }
    OrderProcessed:=True;
  finally
    Free;
  end;
end;

procedure TTeeAboutForm.BCopyOrderClick(Sender: TObject);

  {$IFDEF D1}
  function GetTextStr(AStrings:TStrings): PChar;
  var
    I, L, Size, tmpCount: Integer;
    P: PChar;
    S: string;
  begin
    tmpCount := AStrings.Count;
    Size := 1;
    for I := 0 to tmpCount - 1 do Inc(Size, Length(AStrings[I]) + 2);
    GetMem(Result,Size);
    P := Pointer(Result);
    for I := 0 to tmpCount - 1 do
    begin
      S := AStrings[I];
      L := Length(S);
      if L <> 0 then
      begin
        System.Move(S[1], P^, L);
        Inc(P, L);
      end;
      P^ := #13;
      Inc(P);
      P^ := #10;
      Inc(P);
    end;
    P^:=#0;
    Inc(P);
  end;
  {$ENDIF}

Var tmpStrings:TStrings;
    tmp:PChar;
begin
  tmpStrings:=GetOrderForm;
  try
    {$IFDEF D1}
    tmp:=GetTextStr(tmpStrings);
    {$ELSE}
    tmp:=PChar(tmpStrings.Text);
    {$ENDIF}
    Clipboard.SetTextBuf(tmp);
    ShowMessage(TeeMsg_OrderClipboard);
    OrderProcessed:=True;
  finally
    tmpStrings.Free;
  end;
end;

Function TTeeAboutForm.ShippingPrice:Longint;
var tmp:Longint;
begin
  if CBShip.ItemIndex=0 then tmp:=0
                        else tmp:=TeeProducts[RGLicense.ItemIndex].Ship;
  if tmp=0 then
  Case CBShip.ItemIndex of { remember for BCB3 and ActiveX is only $6? }
    0:  tmp:=   0;
    1:  tmp:= 800;
    2:  tmp:=1200;
    3:  tmp:=1800;
  end;
  result:=tmp*UDQuantity.Position; { same for each copy }
end;

procedure TTeeAboutForm.RefreshTotal;
begin
  LShip.Caption :=FormatUSD(ShippingPrice);
  LTotal.Caption:=FormatUSD(CalcTotal);
end;

procedure TTeeAboutForm.SEQuantityChange(Sender: TObject);
begin
  RefreshTotal;
end;

procedure TTeeAboutForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=(NoteBook1.PageIndex=0) or OrderProcessed;
  if not CanClose then CanClose:=TeeYesNo(TeeMsg_CancelOrder);
end;

procedure TTeeAboutForm.RGLicenseClick(Sender: TObject);
begin
  RefreshTotal;
end;

procedure TTeeAboutForm.CBPaymentTypeChange(Sender: TObject);
begin
  GPCard.Visible:=(CBPaymentType.ItemIndex<3);
end;

procedure TTeeAboutForm.CBShipChange(Sender: TObject);
begin
  RefreshTotal;
end;

procedure TTeeAboutForm.ENameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then  { avoid "beep" and allow RETURN to move fields }
  begin
    Key:=#0;
    Perform(WM_NEXTDLGCTL,0,0);
  end;
end;

{$IFNDEF D1}
Procedure GotoURL(Handle:Integer; Const URL:String);
Var St:Array[0..255] of char;
begin
  ShellExecute(Handle,'open',StrPCopy(St,URL),nil,nil,SW_SHOW);  { <-- do not translate }
end;
{$ENDIF}

procedure TTeeAboutForm.LabelwwwClick(Sender: TObject);
begin
{$IFNDEF D1}
  GotoURL(Handle,LabelWWW.Caption);
{$ENDIF}
end;

procedure TTeeAboutForm.FormShow(Sender: TObject);
begin
  RGLicense.ItemIndex:=0;
  {$IFNDEF TEETRIAL}
  if not ShowOrderInfo then
  begin
    LabelVersion.Parent:=Label3.Parent;
    LabelVersion.Top:=Label3.Top+Label3.Height;
    BClose.Parent:=NoteBook1;
    BClose.Top:=LabelVersion.Top+LabelVersion.Height-BClose.Height;
    BOrder.Visible:=False;
    ClientHeight:=LabelVersion.Top+LabelVersion.Height+6;
  end;
  {$ENDIF}
  {$IFDEF COMPOSOURCE}
  Image2.Visible:=True;
  BOrder.Visible:=False;
  {$ENDIF}
end;

{$IFDEF TEETRIAL}
initialization
  TeeTrialShowAboutBox:=TeeShowAboutBox;
{$ENDIF}
end.
