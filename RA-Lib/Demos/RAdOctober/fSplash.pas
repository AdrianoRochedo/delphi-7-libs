unit fSplash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Jpeg;

type
  TSplash = class(TForm)
    Label2: TLabel;
    lblVersion: TLabel;
    Label4: TLabel;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CT: Integer;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

  procedure ShowSplash;
  procedure HideSplash;

implementation

uses RAUtils, RAVersion;

{$R *.DFM}

type

  TForm = class(TSplash);

var
  Splash: TForm;

procedure TSplash.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height) div 3;
  Left := (Screen.Width - Width) div 2;
  CT := ControlCount;
  if HasSwitch('debug') then
    FormStyle := fsNormal;
  lblVersion.Caption := lblVersion.Caption + RALibVersionString;
end;

procedure ShowSplash;
begin
  Application.CreateForm(TForm, Splash);
	Splash.Show;
//	Splash.Update;
{  with Splash do
  begin
    Width := 500; Height := 400;
    Color := clBlack;
    Top := (Screen.Height - Height) div 3;
    Left := (Screen.Width - Width) div 2;
    BorderStyle := bsNone;
    Show;
    Update;
  end; }
end;    { ShowSplash }

procedure HideSplash;
begin
  Splash.Free; Splash := nil;
end;


procedure TSplash.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Canvas.Draw(0, 0, Image2.Picture.Graphic);
  Message.Result := 1;
end;

end.
