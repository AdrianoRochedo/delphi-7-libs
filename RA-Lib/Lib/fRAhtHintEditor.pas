{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 R&A

       description : Design-time Hint Editor

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm.htm
************************************************************}

{$INCLUDE RA.INC}

unit fRAhtHintEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  {$IFDEF RA_D6H}
   DesignIntf, DesignEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF RA_D6H},
  RAHint;

type
  TRAhtHintEditor = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    bOk: TButton;
    bCancel: TButton;
    Label2: TLabel;
    procedure Memo1Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TRAHintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.DFM}

procedure TRAHintProperty.Edit;
var
  OldHintWindowClass: THintWindowClass;
begin
  OldHintWindowClass := HintWindowClass;
  with TRAhtHintEditor.Create(Application) do
    try
      Memo1.Text := GetValue;
      HintWindowClass := TRAhtHintWindow;
      if ShowModal = mrOk then
        SetValue(Memo1.Text);
    finally 
      Free;
      HintWindowClass := OldHintWindowClass;
     { recreate hint window }
      Application.ShowHint := not Application.ShowHint;
      Application.ShowHint := not Application.ShowHint;
    end;    { try/finally }
end;

function TRAHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;


procedure TRAhtHintEditor.Memo1Change(Sender: TObject);
begin
  Label2.Hint := Memo1.Text;
end;

procedure TRAhtHintEditor.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then bCancel.Click;
end;

end.
