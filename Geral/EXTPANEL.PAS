unit Extpanel;

{
  A panel component that allows custom painting (an OnPaint event)
  and can accept files.
  Author: Nikolai Botev Botev
  For details see EXTPANEL.TXT

  DO NOT distribute this component without EXTPANEL.TXT. If you
  make any changes add a description of the work you've done in
  this comments section.

Version 1.0 - 6 June 1997
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus, ShellApi;

Const
  drm_AfterEnter   = wm_User + 100; {Valor aleatorio}
  drm_AfterExit    = wm_User + 101;

type
  TDropFilesEvent = procedure(Files: TStrings; const Pos: TPoint) of object;
  TExtPanel = class(TCustomPanel)
  private
    FDoDefaultPaint : Boolean;

    FAfterEnter : TNotifyEvent;
    FAfterExit  : TNotifyEvent;

    procedure OMAfterEnter(var Msg : TMessage);
      message drm_AfterEnter;
    procedure OMAfterExit(var Msg : TMessage);
      message drm_AfterExit;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    { Protected declarations }
    FAcceptFiles: Boolean;
    FRedraw: array [0..1] of Boolean;
    FOnDropFiles: TDropFilesEvent;
    FOnPaint: TNotifyEvent;
    procedure Paint; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure SetAcceptFiles(Value: Boolean);
    function GetRedraw(Index: Integer): Boolean;
    procedure SetRedraw(Index: Integer; Value: Boolean);
  public
    Constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    { Published declarations }
    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BevelEdges;
    property BevelKind default bkNone;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Caption;
    property Color;
    property Ctl3D;
    property Font;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;

    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles;
    property HorzRedraw: Boolean index 0 read GetRedraw write SetRedraw;
    property VertRedraw: Boolean index 1 read GetRedraw write SetRedraw;
    property DoDefaultPaint: Boolean read FDoDefaultPaint Write FDoDefaultPaint Default True;

    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property AfterEnter : TNotifyEvent
        read FAfterEnter
       write FAfterEnter;

    property AfterExit : TNotifyEvent                                  
        read FAfterExit
       write FAfterExit;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AD-2', [TExtPanel]);
end;

Constructor TExtPanel.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FDoDefaultPaint := True;
end;

procedure TExtPanel.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  SetRedraw(0, FRedraw[0]);
  SetRedraw(1, FRedraw[1]);
end;  { TExtPanel.CreateWindowHandle }

procedure TExtPanel.Paint;
begin
  if DoDefaultPaint Then inherited Paint;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;  { TExtPanel.Paint }

const
  Style: array [0..1] of Integer = (CS_HREDRAW, CS_VREDRAW);

function TExtPanel.GetRedraw(Index: Integer): Boolean;
begin
  if HandleAllocated then
    {$IFDEF WIN32}
      Result := (GetClassLong(Handle,GCL_STYLE) and Style[Index]) <> 0
    {$ELSE}
      Result := (GetClassWord(Handle,GCW_STYLE) and Style[Index]) <> 0
    {$ENDIF}
  else
    Result := FRedraw[Index];
end;  { TExtPanel.GetRedraw }

procedure TExtPanel.SetRedraw(Index: Integer; Value: Boolean);
var
  ClassStyle: Integer;
begin
  FRedraw[Index] := Value;
  if HandleAllocated then begin
    {$IFDEF WIN32}
      ClassStyle := GetClassLong(Handle, GCL_STYLE);
    {$ELSE}
      ClassStyle := GetClassWord(Handle, GCW_STYLE);
    {$ENDIF}
    if FRedraw[Index] then
      {$IFDEF WIN32}
        SetClassLong(Handle, GCL_STYLE, ClassStyle or Style[Index])
      {$ELSE}
        SetClassWord(Handle, GCW_STYLE, ClassStyle or Style[Index])
      {$ENDIF}
    else
      {$IFDEF WIN32}
        SetClassLong(Handle, GCL_STYLE, ClassStyle and not Style[Index]);
      {$ELSE}
        SetClassWord(Handle, GCW_STYLE, ClassStyle and not Style[Index]);
      {$ENDIF}
  end;
end;  { TExtPanel.SetRedraw }

procedure TExtPanel.SetAcceptFiles(Value: Boolean);
begin
  if (Value <> FAcceptFiles) then begin
    FAcceptFiles := Value;
    if not (csDesigning in ComponentState) then
      DragAcceptFiles(Handle, FAcceptFiles);
  end;
end;  { TExtPanel.SetAcceptFiles }

procedure TExtPanel.WMDropFiles(var Msg: TWMDropFiles);
{$IFNDEF WIN32}
type
  UINT = Word;
{$ENDIF}
var
  I, Len, FileCount: Integer;
  FileName: {$IFDEF WIN32} ShortString {$ELSE} string {$ENDIF};
  Files: TStringList;
  Pos: TPoint;
begin
  if not (csDesigning in ComponentState) then begin
    DragQueryPoint(Msg.Drop, Pos);
    Files := TStringList.Create;
    try
      FileCount := DragQueryFile(Msg.Drop, UINT(-1), nil, 0);
      for I := 0 to (FileCount - 1) do begin
        Len := DragQueryFile(Msg.Drop, I, @FileName[1], 255);
        FileName[0] := Char(Len);
        Files.Add(FileName);
      end;
      if (FileCount > 0) and Assigned(FOnDropFiles) then
        FOnDropFiles(Files, Pos);
    finally
      Files.Free;
    end;
  end;
end;  { TExtPanel.WMDropFiles }

procedure TExtPanel.OMAfterEnter(var Msg : TMessage);
begin
  if Assigned(FAfterEnter) then
    FAfterEnter(Self);
end;

procedure TExtPanel.OMAfterExit(var Msg : TMessage);
begin
  if Assigned(FAfterExit) then
    FAfterExit(Self);
end;

procedure TExtPanel.DoExit;
begin
  inherited DoExit;
  PostMessage(Handle, drm_AfterExit, 0, 0);
end;

procedure TExtPanel.DoEnter;
begin
  inherited DoEnter;
  PostMessage(Handle, drm_AfterEnter, 0, 0);
end;

end.
