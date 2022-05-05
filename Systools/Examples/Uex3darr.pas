{*********************************************************}
{*                  UEX3DARR.PAS 1.05                    *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit Uex3darr;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, StConst, StBase, StLArr;

type
  TMy3D = class(TStLArray)
  protected
    XMax,
    YMax,
    ZMax    : LongInt;
  public
    constructor Create(X, Y, Z : Cardinal);
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Edit4: TEdit;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  My3D : TMy3D;

implementation

{$R *.DFM}
const
  MaxX = 50;
  MaxY = 50;
  MaxZ = 50;

constructor TMy3D.Create(X, Y, Z : Cardinal);
var
  row,
  col,
  up,
  Value : LongInt;
  A     : TStLMatrix;
begin
  XMax := X;
  YMax := Y;
  ZMax := Z;

  Randomize;

  inherited Create(ZMax, SizeOf(TStLMatrix));
  for up := 0 to ZMax-1 do
  begin
    A := TStLMatrix.Create(XMax, YMax, SizeOf(LongInt));
    for row := 0 to YMax-1 do
      for col := 0 to XMax-1 do begin
        Value := up+100*col+10000*row;
        A.Put(row, col, Value);
      end;
    Put(up, A);
  end;
end;

destructor TMy3D.Destroy;
var
  Up : LongInt;
  A  : TStLMatrix;
begin
  for Up := 0 to ZMax-1 do
  begin
    Get(Up, A);
    if Assigned(A) then
      A.Free;
  end;
  inherited Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  My3D := TMy3D.Create(MaxX, MaxY, MaxZ);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  My3D.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  XV,
  YV,
  ZV,
  Value  : LongInt;
  Z      : TStLMatrix;
begin
  XV := StrToInt(Edit1.Text);
  YV := StrToInt(Edit2.Text);
  ZV := StrToInt(Edit3.Text);
  if (XV < 0) or (XV >= MaxX) then begin
    Edit1.Text := '0';
    XV := StrToInt(Edit1.Text);
  end;
  if (YV < 0) or (YV >= MaxY) then begin
    Edit2.Text := '0';
    YV := StrToInt(Edit2.Text);
  end;
  if (ZV < 0) or (ZV >= MaxZ) then begin
    Edit3.Text := '0';
    ZV := StrToInt(Edit3.Text);
  end;

  My3D.Get(ZV, Z);
  Z.Get(XV, YV, Value);
  Edit4.Text := IntToStr(Value);
end;

end.
