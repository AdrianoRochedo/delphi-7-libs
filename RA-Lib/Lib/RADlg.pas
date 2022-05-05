{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : TProgressForm
       description : dialog components

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RADlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Grids, StdCtrls, ComCtrls;

type

  TRAProgressForm = class(TComponent)
  private
    Form : TForm;
    FProgressBar : TProgressBar;
    Label1 : TLabel;

    FCaption : TCaption;
    FInfoLabel : TCaption;
    FOnShow : TNotifyEvent;
    FCancel : boolean;
    FProgressMin,
    FProgressMax,
    FProgressStep,
    FProgressPosition : integer;
    EE : Exception;
    
    procedure SetCaption(ACaption : TCaption);
    procedure SetInfoLabel(ACaption : TCaption);
    procedure FormOnShow(Sender : TObject);
    procedure FormOnCancel(Sender : TObject);
    procedure SetProgress(index : integer; AValue : integer);
  public
    procedure Execute;
    procedure ProgressStepIt;
    property Cancel : boolean read FCancel;
  published
    property Caption : TCaption read FCaption write SetCaption;
    property InfoLabel : TCaption read FInfoLabel write SetInfoLabel;
    property ProgressMin : integer index 0 read FProgressMin write SetProgress;
    property ProgressMax : integer index 1 read FProgressMax write SetProgress;
    property ProgressStep : integer index 2 read FProgressStep write SetProgress;
    property ProgressPosition : integer index 3 read FProgressPosition write SetProgress;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

uses RAUtils, RACnst;

type

  TRAProgressFormForm = class(TForm)
  private
    procedure WMUser1(var Message : TMessage); message wm_User+1;
  end;

procedure TRAProgressForm.Execute;
begin
 {$IFDEF RA_B}
  Form := TRAProgressFormForm.CreateNew(Self, 1);
 {$ELSE}
  Form := TRAProgressFormForm.CreateNew(Self);
 {$ENDIF RA_B}
  try
    Form.Caption := Caption;
    with Form do
      begin
        ClientWidth := 307;
        ClientHeight := 98;
        BorderStyle := bsDialog;
        Position := poScreenCenter;
        FProgressBar := TProgressBar.Create(Form);
      end;
    with FProgressBar do begin
      Parent := Form;
      SetBounds(8, 38, 292, 18);
      Step := 1;
    end;
    Label1 := TLabel.Create(Form);
    with Label1 do begin
      Parent := Form;
      Caption := '';
      AutoSize := false;
      SetBounds(8, 8, 293, 13);
    end;
    with TButton.Create(Form) do begin
      Parent := Form;
      Caption := SCancel;
      SetBounds(116, 67, 75, 23);
      OnClick := FormOnCancel;
    end;
    Form.OnShow := FormOnShow;
    FCancel := false;
    if Assigned(FOnShow) then
    begin
      EE := nil;
      Form.ShowModal;
      if EE <> nil then raise EE;
    end;
  finally
    Form.Free;
    Form := nil;
  end;
end;

procedure TRAProgressForm.FormOnShow(Sender : TObject);
begin
  PostMessage(Form.Handle, wm_User + 1, 0, 0);
end;

procedure TRAProgressForm.FormOnCancel(Sender : TObject);
begin
  FCancel := true;
end;

procedure TRAProgressFormForm.WMUser1(var Message : TMessage);
begin
  Application.ProcessMessages;
  try
    try
      (Owner as TRAProgressForm).FOnShow(Self);
    except
      on E : Exception do
      begin
        (Owner as TRAProgressForm).EE := E;
        ChangeTopException(nil);
      end;
    end;
  finally
    ModalResult := mrOk;
  end;  
end;

procedure TRAProgressForm.SetCaption(ACaption : TCaption);
begin
  FCaption := ACaption;
  if Form <> nil then Form.Caption := FCaption;
end;

procedure TRAProgressForm.SetInfoLabel(ACaption : TCaption);
begin
  FInfoLabel := ACaption;
  if Form <> nil then Label1.Caption := ACaption;
end;

procedure TRAProgressForm.SetProgress(index : integer; AValue : integer);
begin
  case index of
    0 : FProgressMin := AValue;
    1 : FProgressMax := AValue;
    2 : FProgressStep := AValue;
    3 : FProgressPosition := AValue;
  end;
  if Form <> nil then
  begin
    FProgressBar.Min  := FProgressMin ;
    FProgressBar.Max  := FProgressMax ;
    FProgressBar.Step := FProgressStep;
    FProgressBar.Position := FProgressPosition;
  end;
end;

procedure TRAProgressForm.ProgressStepIt;
begin
  if Form <> nil then
    FProgressBar.StepIt;
end;

end.
