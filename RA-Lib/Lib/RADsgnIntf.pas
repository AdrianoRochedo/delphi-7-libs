{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       description : interface to design-time routines

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RADsgnIntf;

interface

uses
  Windows, Messages, Forms, Controls, Graphics, SysUtils, Classes;

  { DrawDesignFrame draws frame on the rect, Rect.
    R&A uses this function to drawing frame around controls at design-time }

  procedure DrawDesignFrame(Canvas : TCanvas; Rect : TRect);


  procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
  procedure DesignerModified(Self : TComponent);
  procedure DesignerSelectComponent(Self : TComponent);


var
  DrawDesignFrameProc: procedure (Canvas : TCanvas; Rect : TRect);
  DesignerNotifyProc: procedure (Self, Item: TComponent; Operation: TOperation);
  DesignerModifiedProc: procedure (Self : TComponent);
  DesignerSelectComponentProc: procedure (Self : TComponent);

type
  TGetProjectNameProc = function: string;
  
var
  GetProjectNameProc: TGetProjectNameProc = nil;

implementation

procedure DrawDesignFrame(Canvas : TCanvas; Rect : TRect);
begin
  if Assigned(DrawDesignFrameProc) then
    DrawDesignFrameProc(Canvas, Rect);
end;

procedure DesignerNotify(Self, Item: TComponent; Operation: TOperation);
begin
  if Assigned(DesignerNotifyProc) then
    DesignerNotifyProc(Self, Item, Operation);
end;

procedure DesignerModified(Self : TComponent);
begin
  if Assigned(DesignerModifiedProc) then
    DesignerModifiedProc(Self);
end;

procedure DesignerSelectComponent(Self : TComponent);
begin
  if Assigned(DesignerSelectComponentProc) then
    DesignerSelectComponentProc(Self);
end;

end.
