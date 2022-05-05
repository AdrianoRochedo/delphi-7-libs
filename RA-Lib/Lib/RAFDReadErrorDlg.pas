unit RAFDReadErrorDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type

  TRAReadErrorDlg = class(TForm)
    Bevel1: TBevel;
    IgnoreButton: TButton;
    CancelButton: TButton;
    IgnoreAllButton: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

  TReadErrorDlg = class(TRAReadErrorDlg);

  function ReadErrorDlg(const Message: string): TModalResult;

implementation

uses RAFD;

{$R *.dfm}

function ReadErrorDlg(const Message: string): TModalResult;
begin
  with TReadErrorDlg.Create(Application) do
    try
      Label1.Caption := Format(Label1.Caption, [Message]);
      Result := ShowModal;
    finally
      Free;
    end;
end;

procedure TRAReadErrorDlg.FormCreate(Sender: TObject);
begin
  { Locale }
  Caption := ResStr(deReadErrorDlgCaption, Caption);
  Label1.Caption := ResStr(deReadErrorDlgMessage, Label1.Caption);
  IgnoreButton.Caption := ResStr(deIgnore, IgnoreButton.Caption);
  CancelButton.Caption := ResStr(deCancel, CancelButton.Caption);
  IgnoreAllButton.Caption := ResStr(deIgnoreAll, IgnoreAllButton.Caption);
end;

end.
