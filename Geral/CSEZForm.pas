unit CSEZForm;
{ Copyright 1995 Classic Software
  All Rights Reserved

  EZForm classes allow you to create forms
  which can be navigated using the Enter,
  Up/Down Arrow and Ctrl+Tab keys in addition
  to the standard Tab key behaviour.

  Individual support for the additional
  navigation keys can be enabled/disabled for
  each form instance.

  EZForm enhanced form navigation can be
  enabled or disabled globally; for example,
  in response to a user setting a system
  option.
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DBCtrls, DBGrids, Mask;

type
  { Forward declarations }
  TcsEZForm = class;
  TcsEZKeys = class;
  TcsEZFormOptions = class;


  TNavigationKey = (nkEnter, nkUpDnArrows, nkCtrlTab);
  TNavigationKeys = set of TNavigationKey;

  { TcsEZForm }

  TcsEZForm = class(TForm)
  private
    FNavigationKeys: TNavigationKeys;
    FOldDefaultBtn: TButton;
    FOldKeyPreview: Boolean;
    FOldOnKeyDown: TKeyEvent;
    FOldOnKeyPress: TKeyPressEvent;
    procedure SetNavigationKeys(Value: TNavigationKeys);
    function FindDefaultBtn: TButton;
    function FindEZKeys: TcsEZKeys;
  protected
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create(AOwner: TComponent); override;
    function CanIntercept(KeyType: TNavigationKey; Ctrl: TControl): Boolean; virtual;
    procedure SelectDefaultBtn; virtual;
    property OldDefaultBtn: TButton read FOldDefaultBtn;
    property OldKeyPreview: Boolean read FOldKeyPreview write FOldKeyPreview;
  published
    property NavigationKeys: TNavigationKeys read FNavigationKeys write SetNavigationKeys
      default [nkEnter, nkUpDnArrows, nkCtrlTab];
  end;

  { TcsEZKeys }

  TcsEZKeys = class(TComponent)
  private
    FNavigationKeys: TNavigationKeys;
    procedure SetNavigationKeys(Value: TNavigationKeys);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property NavigationKeys: TNavigationKeys read FNavigationKeys write SetNavigationKeys
      default [nkEnter, nkUpDnArrows, nkCtrlTab];
  end;

  { TcsEZFormOptions }

  TcsEZFormOptions = class(TObject)
  private
    FEnabled: Boolean;
    FDefaultNavigationKeys: TNavigationKeys;
    procedure Enable(Value: Boolean);
    procedure SetDefaultNavigationKeys(Value: TNavigationKeys);
  public
    constructor Create;
    procedure Init; virtual;
    property Enabled: Boolean read FEnabled write Enable;
    property DefaultNavigationKeys: TNavigationKeys read FDefaultNavigationKeys write SetDefaultNavigationKeys;
  end;

const
  AllNavigationKeys: TNavigationKeys = [nkEnter, nkUpDnArrows, nkCtrlTab];
  K_ENTER = #13;
  K_NULL  = #0;

procedure Register;

{ csEZFormOptions is an access function so that
  the local variable used to store the options
  can be accessed from outside this unit.
}
function csEZFormOptions: TcsEZFormOptions;

implementation

var
  { Define "class" variable for options }
  EZFormOptions: TcsEZFormOptions;

procedure Register;
begin
  RegisterComponents('Samples', [TcsEZKeys]);
end;

function csEZFormOptions: TcsEZFormOptions;
begin
  Result := EZFormOptions;
end;

constructor TcsEZForm.Create(AOwner: TComponent);
var EZKeysComponent: TcsEZKeys;
    CurrentOnKeyDown: TKeyEvent;
    CurrentOnKeyPress: TKeyPressEvent;
begin
  inherited Create(AOwner);

  { Save settings to allow restore }
  FOldKeyPreview := KeyPreview;
  FOldDefaultBtn := FindDefaultBtn;

  { Save the currently assigned event methods
    (if any) for the methods to be handled
    by the EZForm class.
  }
  CurrentOnKeyDown := OnKeyDown;
  if Assigned(CurrentOnKeyDown) then
    FOldOnKeyDown := OnKeyDown;
  CurrentOnKeyPress := OnKeyPress;
  if Assigned(CurrentOnKeyPress) then
    FOldOnKeyPress := OnKeyPress;
  { Re-assign to EZForm's event methods. }
  OnKeyDown := FormKeyDown;
  OnKeyPress := FormKeyPress;

  { Assign default navigation keys }
  EZKeysComponent := FindEZKeys;
  if (EZKeysComponent <> nil) then
    SetNavigationKeys(EZKeysComponent.NavigationKeys)
  else
    { Assign default navigation keys only if
      no value already assigned, e.g. in user's
      own OnCreate event method.
    }
    if (NavigationKeys = []) then
      SetNavigationKeys(EZFormOptions.DefaultNavigationKeys);

  if EZFormOptions.Enabled then
  begin
    KeyPreview := True;
    { Turn off Default button if Enter in keys }
    if (FOldDefaultBtn <> nil) and (nkEnter in NavigationKeys) then
      FOldDefaultBtn.Default := False;
  end;

end;

function TcsEZForm.FindEZKeys: TcsEZKeys;
var AComponent: TComponent;
    I: Integer;
begin
  Result := nil;
  for I := 0 to ComponentCount - 1 do
  begin
    AComponent := Components[I];
    if (AComponent is TcsEZKeys) then
    begin
      Result := AComponent as TcsEZKeys;
      Break;
    end;
  end;
end;

function TcsEZForm.FindDefaultBtn: TButton;
var AComponent: TComponent;
    I: Integer;
begin
  Result := nil;
  for I := 0 to ComponentCount - 1 do
  begin
    AComponent := Components[I];
    if ((AComponent is TButton) and ((AComponent as TButton).Default)) then
    begin
      Result := AComponent as TButton;
      Break;
    end;
  end;
end;

procedure TcsEZForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Perform any user-assigned event method.
    The user-assigned event method can set
    Key to 0 to override the default handling.
  }
  if Assigned(FOldOnKeyDown) then
    FOldOnKeyDown(Sender, Key, Shift);

  if EZFormOptions.Enabled then
  begin
    if not (Key = 0) then
    begin
      if (Key = VK_DOWN) or (Key = VK_UP) then
      begin
        if CanIntercept(nkUpDnArrows, ActiveControl) then
          SelectNext(ActiveControl, (Key = VK_DOWN), True);
      end
      else
      if (ssCtrl in Shift) and (Key = VK_TAB) and
        CanIntercept(nkCtrlTab, ActiveControl) then
      begin
        { Set Key to 0 to stop spurious Tab keystrokes
          going to Memo controls when Ctrl+Tab is held
          down to cycle through all controls.
        }
        Key := 0;
        SelectNext(ActiveControl, (not (ssShift in Shift)), True);
      end;
    end;
  end;
end;

procedure TcsEZForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  { Perform any user-assigned event method.
    The user-assigned event method can set
    Key to #0 to prevent the default handling.
  }
  if Assigned(FOldOnKeyPress) then
    FOldOnKeyPress(Sender, Key);

  if EZFormOptions.Enabled then
  begin
    if not (Key = #0) then
      if ((Key = K_ENTER) and CanIntercept(nkEnter, ActiveControl)) then
      begin
        Key := K_NULL; { consume the key }
        SelectNext(ActiveControl, True, True);
      end;
  end;
end;

function TcsEZForm.CanIntercept(KeyType: TNavigationKey; Ctrl: TControl): Boolean;
begin
  Result := False;
  if EZFormOptions.Enabled and (KeyType in NavigationKeys) then
    case KeyType of
      { Note that in the current version of Delphi the WantReturns
        property is only published for TMemo and not for TDBMemo,
        even though they have the same ancestor.
      }
      nkEnter:      Result :=
                      not (((Ctrl is TMemo) and (Ctrl as TMemo).WantReturns) or
                           (Ctrl is TDBMemo)  or
                           (Ctrl is TCustomDBGrid));
      nkUpDnArrows: Result :=
                      ((Ctrl is TEdit) or
                       (Ctrl is TCustomMaskEdit));
      nkCtrlTab:    Result := True;
    else
      Result := False;
    end;
end;

procedure TcsEZForm.SelectDefaultBtn;
var DefaultBtnOnClick: TNotifyEvent;
begin
  if (OldDefaultBtn <> nil) then
  begin
    DefaultBtnOnClick := OldDefaultBtn.OnClick;
    if Assigned(DefaultBtnOnClick) then
      DefaultBtnOnClick(Self);
  end;
end;

procedure TcsEZForm.SetNavigationKeys(Value: TNavigationKeys);
var HadEnter: Boolean;
    HasEnter: Boolean;
begin
  HadEnter := (nkEnter in FNavigationKeys);
  FNavigationKeys := Value;
  if EZFormOptions.Enabled and (FOldDefaultBtn <> nil) then
  begin
    HasEnter := (nkEnter in FNavigationKeys);
    if (HadEnter <> HasEnter) then
      FOldDefaultBtn.Default := (not HasEnter);
  end;
end;

constructor TcsEZKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetNavigationKeys(EZFormOptions.DefaultNavigationKeys);
end;

procedure TcsEZKeys.SetNavigationKeys(Value: TNavigationKeys);
begin
  FNavigationKeys := Value;
end;

constructor TcsEZFormOptions.Create;
begin
  inherited Create;
  Init;
end;

procedure TcsEZFormOptions.Init;
begin
  FEnabled := True;
  FDefaultNavigationKeys := AllNavigationKeys;
end;

procedure TcsEZFormOptions.Enable(Value: Boolean);
var I: Integer;
begin
  FEnabled := Value;

  { Reflect new status in all open EZForms }
  for I := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.Forms[I] is TcsEZForm) then
      with (Screen.Forms[I] as TcsEZForm) do
      begin
        if FEnabled then
          KeyPreview := True
        else
        begin
          KeyPreview := OldKeyPreview;
        end;
        if (OldDefaultBtn <> nil) then
          OldDefaultBtn.Default := (not FEnabled) or not (nkEnter in NavigationKeys);
      end;
  end;
end;

procedure TcsEZFormOptions.SetDefaultNavigationKeys(Value: TNavigationKeys);
begin
  FDefaultNavigationKeys := Value;
end;

{ Initialisation and cleanup }
procedure FreeEZFormOptions; far;
begin
  EZFormOptions.Free;
end;

initialization
  EZFormOptions := TcsEZFormOptions.Create;
  AddExitProc(FreeEZFormOptions);
end.
