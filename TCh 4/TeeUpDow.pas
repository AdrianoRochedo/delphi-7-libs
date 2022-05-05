{*****************************************}
{   TeeChart-Pro 4.0                      }
{   Copyright (c) 1995-98 David Berneda   }
{                                         }
{    TUpDown component for Delphi 1       }
{*****************************************}
unit TeeUpDow;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Spin;

type
  TUpDown = class(TSpinButton)
  private
    { Private declarations }
    FMin      :Integer;
    FMax      :Integer;
    FPosition :Integer;
    FIncrement:Integer;
    FAssociate:TControl;
    FWrap     :Boolean;
  protected
    { Protected declarations }
    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    Function GetPosition:Integer;
    Procedure SetPosition(Value:Integer);
    procedure SetAssociate(Value:TControl);
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
  published
    { Published declarations }
    property Associate:TControl read FAssociate write SetAssociate;
    property Increment:Integer read FIncrement write FIncrement default 1;
    property Max:Integer read FMax write FMax default 100;
    property Min:Integer read FMin write FMin default 0;
    property Position:Integer read GetPosition write SetPosition default 0;
    property Wrap:Boolean read FWrap write FWrap default False;
    property DownGlyph stored False;
    property UpGlyph stored False;
  end;

procedure Register;

implementation

Uses TeeConst,StdCtrls;

Constructor TUpDown.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FMin:=0;
  FMax:=100;
  FPosition:=0;
  FIncrement:=1;
  FAssociate:=nil;
  FWrap:=False;
  OnUpClick:=UpClick;
  OnDownClick:=DownClick;
end;

procedure TUpDown.SetAssociate(Value:TControl);
begin
  FAssociate:=Value;
  if Assigned(FAssociate) then
     if FAssociate is TCustomEdit then
        (FAssociate as TCustomEdit).Text:=IntToStr(FPosition);
end;

Function TUpDown.GetPosition:Integer;
begin
  result:=0;
  if Assigned(FAssociate) then
     if FAssociate is TCustomEdit then
        result:=StrToInt((FAssociate as TCustomEdit).Text);
end;

Procedure TUpDown.SetPosition(Value:Integer);
begin
  FPosition:=Value;
  if Assigned(FAssociate) then
     if FAssociate is TCustomEdit then
        (FAssociate as TCustomEdit).Text:=IntToStr(FPosition);
end;

procedure TUpDown.UpClick(Sender: TObject);
begin
  Inc(FPosition,FIncrement);
  if FPosition>FMax then
     if FWrap then FPosition:=FMin
              else FPosition:=FMax;
  if Assigned(FAssociate) then
     if FAssociate is TCustomEdit then
        (FAssociate as TCustomEdit).Text:=IntToStr(FPosition);
end;

procedure TUpDown.DownClick(Sender: TObject);
begin
  Dec(FPosition,FIncrement);
  if FPosition<FMin then
     if FWrap then FPosition:=FMax
              else FPosition:=FMin;
  if Assigned(FAssociate) then
     if FAssociate is TCustomEdit then
        (FAssociate as TCustomEdit).Text:=IntToStr(FPosition);
end;

procedure Register;
begin
  RegisterComponents(TeeMsg_TeeChartPalette, [TUpDown]);
end;

end.
