{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       description : Windows hook manager for R&A Controls

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAHook;

interface

uses Windows, Messages, Classes, Forms, Controls;

type

  TWHook = class
  private
    FHandle : HWnd;
    OldWndProc : Pointer;
    NewWndProc : Pointer;
    FWndProc : TWndMethod;
    FEnabled : boolean;
    procedure SetEnabled(Value : boolean);
  public
    constructor Create(AHandle : Hwnd; AWndProc : TWndMethod);
    procedure HookWindow;
    procedure UnhookWindow;
    destructor Destroy; override;
    procedure CallOldProc(var Message: TMessage);
    property Enabled : boolean read FEnabled write SetEnabled;
  end;

implementation

var
  WindowHooks : TList = nil;

constructor TWHook.Create(AHandle : Hwnd; AWndProc : TWndMethod);
begin
  FHandle := AHandle;
  FWndProc := AWndProc;
  HookWindow;
end;

destructor TWHook.Destroy;
begin
  UnhookWindow;
  inherited Destroy;
end;

procedure TWHook.SetEnabled(Value : boolean);
begin
  if FEnabled <> Value then
    if FEnabled then
      HookWindow else
      UnhookWindow;
end;

procedure TWHook.HookWindow;
begin
  if not FEnabled then
  begin
    if WindowHooks = nil then
      WindowHooks := TList.Create;
    WindowHooks.Add(Self);
    OldWndProc := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
    NewWndProc := MakeObjectInstance(TWndMethod(FWndProc));
    SetWindowLong(FHandle, GWL_WNDPROC, LongInt(NewWndProc));
    FEnabled := true;
  end;  
end;

procedure TWHook.UnhookWindow;
var
  i : integer;
  Hook : TWHook;
  HookChanged : boolean;
begin
  if FEnabled then
  begin
    HookChanged := false;
    WindowHooks.Remove(Self);
    for i := 0 to WindowHooks.Count - 1 do
    begin
      Hook := TWHook(WindowHooks[i]);
      if Hook.OldWndProc = NewWndProc then
      begin
        Hook.OldWndProc := OldWndProc;
        SetWindowLong(FHandle, GWL_WNDPROC, LongInt(Hook.NewWndProc));
        HookChanged := true;
        break;
      end;
    end;
    if not HookChanged then
      SetWindowLong(FHandle, GWL_WNDPROC, LongInt(OldWndProc));
    if WindowHooks.Count = 0 then
    begin
      WindowHooks.Free;
      WindowHooks := nil; 
    end;
    if Assigned(NewWndProc) then
      FreeObjectInstance(NewWndProc);
    FEnabled := false;
  end;
end;

procedure TWHook.CallOldProc(var Message: TMessage);
begin
  with Message do
    Result := CallWindowProc(OldWndProc, FHandle, Msg, wParam, lParam);
end;

end.
