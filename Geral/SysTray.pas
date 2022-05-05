unit Systray;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI, Menus;

const
  WM_SYSTRAY = WM_USER + $400;

  {
     TSystray v1.1, last updated February 1st, 1999. Written in Borland Delphi 3. I have not
     tested it with other versions of Delphi, though I think the code is compatible with
     Delphi 4.

     This component is freeware. You may use it in any kind of development,
     including comercial, and redistribute it freely.

     TSystray is a non-visual component. To use it, simply drop it on a form.
     The following code takes care of the rest.

     TSystray component written by Tobias Ylander. Any corrections, questions or
     comments can be sent to tobias.ylander@swipnet.se
  }

type
  TSystray = class(TComponent)
  private
    { Private declarations }
    FHandle: HWnd;
    FInSysTray: Boolean;
    FEnabled: Boolean;
    FIcon: TIcon;
    FHint: string;
    FPopupMenu: TPopupMenu;
    FNotifyIconData: TNotifyIconData;
    FOnDblClick: TNotifyEvent;
    FOnLeftClick: TNotifyEvent;
    FOnRightClick: TNotifyEvent;
    procedure WndProc(var Message: TMessage);
    procedure CreateIcon;
    procedure UpdateIcon;
    procedure DestroyIcon;
    function GetIconHandle: HIcon;
    procedure SetEnabled(Value: Boolean);
    procedure SetIcon(Value: TIcon);
    procedure SetHint(Value: string);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure Minimize(Sender: TObject); virtual;
    procedure Restore(Sender: TObject); virtual;
    procedure DefaultOnDblClickEvent(Sender: TObject);
    procedure DefaultOnRightClickEvent(Sender: TObject);
  protected
    { Protected declarations }
    procedure DblClick; virtual;
    procedure LeftClick; virtual;
    procedure RightClick; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: HWnd read FHandle;
    property InSystray: Boolean read FInSystray;
  published
    { Published declarations }
    // Properties
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Icon: TIcon read FIcon write SetIcon;
    property Hint: string read FHint write SetHint;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu default nil;
    // Events
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnLeftClick: TNotifyEvent read FOnLeftClick write FOnLeftClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
  end;

procedure Register;

implementation

{ TSystray }

procedure TSystray.WndProc(var Message: TMessage);
begin
  with Message do
    if Msg = WM_SYSTRAY then
    begin
      if FEnabled then
        case LParam of
          WM_LBUTTONDOWN: LeftClick;
          WM_LBUTTONDBLCLK: DblClick;
          WM_RBUTTONDOWN: RightClick;
        end;
    end
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TSystray.CreateIcon;
begin
  if not FInSystray and not (csDesigning in ComponentState) then
  begin
    FillChar(FNotifyIconData, SizeOf(TNotifyIconData), 0);
    with FNotifyIconData do
    begin
      cbSize := SizeOf(TNotifyIconData);
      uID := 1;
      Wnd := FHandle;

      uFlags := uFlags or NIF_MESSAGE or NIF_ICON;
      if Length(FHint) > 0 then
      begin
        uFlags := uFlags or NIF_TIP;
        StrPCopy(szTip, PChar(FHint));
      end;

      uCallbackMessage := WM_SYSTRAY;
      hIcon := GetIconHandle;
    end;
    Shell_NotifyIcon(NIM_ADD, @FNotifyIconData);
    FInSystray := True;
  end;
end;

procedure TSystray.UpdateIcon;
begin
  if FInSystray and not (csDesigning in ComponentState) then
  begin
    with FNotifyIconData do
    begin
      if Length(FHint) > 0 then
      begin
        if uFlags and NIF_TIP = 0 then uFlags := uFlags or NIF_TIP;
        StrPCopy(szTip, PChar(FHint));
      end
      else
        if uFlags and NIF_TIP <> 0 then uFlags := uFlags and not NIF_TIP;

      hIcon := GetIconHandle;
      Shell_NotifyIcon(NIM_MODIFY, @FNotifyIconData);
    end;
  end;
end;

procedure TSystray.DestroyIcon;
begin
  if FInSystray and not (csDesigning in ComponentState) then
  begin
    Shell_NotifyIcon(NIM_DELETE, @FNotifyIconData);
    FInSystray := False;
  end;
end;

function TSystray.GetIconHandle: HIcon;
begin
  Result := FIcon.Handle;
  if Result = 0 then Result := Application.Icon.Handle;
end;

procedure TSystray.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if Application.MainForm.WindowState = wsMinimized then
    begin
      case FEnabled of
        True:
          begin
            CreateIcon;
            ShowWindow(Application.Handle, SW_HIDE);
          end;
        False:
          begin
            DestroyIcon;
            ShowWindow(Application.Handle, SW_SHOW);
          end;
      end;
    end;
  end;
end;

procedure TSystray.SetIcon(Value: TIcon);
begin
  if Value <> FIcon then
  begin
    FIcon.Assign(Value);
    if FEnabled and FInSystray then UpdateIcon;
  end;
end;

procedure TSystray.SetHint(Value: string);
begin
  if Value <> FHint then
  begin
    FHint := Value;
    if FEnabled and FInSystray then UpdateIcon;
  end;
end;

procedure TSystray.SetPopupMenu(Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    FPopupMenu := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TSystray.Minimize(Sender: TObject);
begin
  if FEnabled and not FInSystray then
  begin
    CreateIcon;
    ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TSystray.Restore(Sender: TObject);
var
  Wnd: HWnd;
begin
  if FEnabled and FInSystray then
  begin
    SetForegroundWindow(Application.Handle);
    DestroyIcon;
    ShowWindow(Application.Handle, SW_SHOW);
  end;
end;

procedure TSystray.DefaultOnDblClickEvent(Sender: TObject);
begin
  Application.Restore;
end;

procedure TSystray.DefaultOnRightClickEvent(Sender: TObject);
var
  P: TPoint;
begin
  if Assigned(FPopupMenu) then
  begin
    GetCursorPos(P);
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(P.X, P.Y);
    PostMessage(Application.Handle, WM_NULL, 0, 0);
  end;
end;

procedure TSystray.DblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

procedure TSystray.LeftClick;
begin
  if Assigned(FOnLeftClick) then FOnLeftClick(Self);
end;

procedure TSystray.RightClick;
begin
  if Assigned(FOnRightClick) then FOnRightClick(Self);
end;

constructor TSystray.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEnabled := True;
  FInSystray := False;
  FIcon := TIcon.Create;
  FHint := '';
  FPopupMenu := nil;
  FillChar(FNotifyIconData, SizeOf(TNotifyIconData), 0);

  FOnDblClick := DefaultOnDblClickEvent;
  FOnRightClick := DefaultOnRightClickEvent;

  FHandle := AllocateHWnd(WndProc);

  if not (csDesigning in ComponentState) then
  begin
    Application.OnMinimize := Minimize;
    Application.OnRestore := Restore;
  end;
end;

destructor TSystray.Destroy;
begin
  if FHandle <> 0 then DeallocateHWnd(FHandle);
  if FInSystray then DestroyIcon;
  FIcon.Free;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('AD-2', [TSystray]);
  RegisterClass(TSystray);
end;

end.
