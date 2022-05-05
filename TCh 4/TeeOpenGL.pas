{******************************************}
{      TeeChart Pro OpenGL Component       }
{   Copyright (c) 1998 by David Berneda    }
{         All Rights Reserved              }
{******************************************}
unit TeeOpenGL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, TeeProcs, StdCtrls,
  ExtCtrls, GLCanvas;

Const TeeOpenGLMsg_OpenGL='Open GL';

type
  TTeeOpenGL=class;

  TGLPosition=class(TPersistent)
  private
    FX,FY,FZ : Double;
    FOwner   : TTeeOpenGL;
    Procedure SetX(Const Value:Double);
    Procedure SetY(Const Value:Double);
    Procedure SetZ(Const Value:Double);
  public
    Procedure Assign(Source:TPersistent); override;
  published
    property X:Double read FX write SetX;
    property Y:Double read FY write SetY;
    property Z:Double read FZ write SetZ;
  end;

  TGLLight=class(TPersistent)
  private
    FColor     : TColor;
    FPosition  : TGLPosition;
    FVisible   : Boolean;
    FOwner     : TTeeOpenGL;
    Procedure SetColor(Value:TColor);
    Procedure SetPosition(Value:TGLPosition);
    procedure SetVisible(Value:Boolean);
  public
    Constructor Create(AOwner:TTeeOpenGL);
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function GLColor:GLMat;
    Function GLPosition:GLMat;
  published
    property Color:TColor read FColor write SetColor default clSilver;
    property Position:TGLPosition read FPosition write SetPosition;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TTeeOpenGL = class(TComponent)
  private
    { Private declarations }
    FActive       : Boolean;
    FLight        : TGLLight;
    FShadeQuality : Boolean;
    FShininess    : Double;
    FTeePanel     : TCustomTeePanel;
    FOnInit       : TNotifyEvent;
    Procedure SetActive(Value:Boolean);
    Procedure SetLight(Value:TGLLight);
    Procedure SetShadeQuality(Value:Boolean);
    Procedure SetTeePanel(Value:TCustomTeePanel);
    Procedure SetShininess(Const Value:Double);
    Procedure Activate;
    Procedure OnCanvasInit(Sender:TObject);
  protected
    { Protected declarations }
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
    Procedure SetDoubleProperty(Var Variable:Double; Const Value:Double);
    Procedure Repaint;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  published
    { Published declarations }
    property Active:Boolean read FActive write SetActive default False;
    property Light:TGLLight read FLight write SetLight;
    property ShadeQuality:Boolean read FShadeQuality write SetShadeQuality default True;
    property Shininess:Double read FShininess write SetShininess;
    property TeePanel:TCustomTeePanel read FTeePanel write SetTeePanel;

    property OnInit:TNotifyEvent read FOnInit write FOnInit;
  end;

Procedure Register;

implementation

{$R TEEGL.RES}

Uses Controls,TeeConst,TeCanvas,OpenGL2;

Procedure SetGLPanelCanvas(APanel:TCustomTeePanel; IsOpenGL:Boolean);
begin
  if IsOpenGL then
  begin
    APanel.Canvas:=TGLCanvas.Create;
    APanel.BevelInner:=bvNone;
    APanel.BevelOuter:=bvNone;
  end
  else
  begin
    APanel.Canvas:=TTeeCanvas3D.Create;
    APanel.BevelInner:=bvNone;
    APanel.BevelOuter:=bvRaised;
  end;
end;

(*
type TGLClass=class
     private
       FPanel:TCustomTeePanel;
       procedure CheckBox1Click(Sender: TObject);
     end;

Var PrivateGLClass:TGLClass;
procedure TGLClass.CheckBox1Click(Sender: TObject);
begin
  if Assigned(FPanel) then
     SetGLPanelCanvas(FPanel,(Sender as TCheckBox).Checked);
end;

Procedure TeeCommanderGLChain(Sender:TTeeCommander);
Var FOpenGL:TCheckBox;
begin
  FOpenGL:=TCheckBox.Create(Sender);
  With FOpenGL do
  begin
    Parent:=Sender;
    Left:=Parent.Width-80;
    Top:=8;
    Caption:=TeeOpenGLMsg_OpenGL;
    Width:=75;
    if Sender.Panel<>nil then
    begin
      Checked:=Sender.Panel.Canvas is TGLCanvas;
      PrivateGLClass:=TGLClass.Create;
      PrivateGLClass.FPanel:=Sender.Panel;
      OnClick:=PrivateGLClass.CheckBox1Click;
    end;
  end;
end;
*)

{ TGLPosition }
Procedure TGLPosition.SetX(Const Value:Double);
begin
  FOwner.SetDoubleProperty(FX,Value);
end;

Procedure TGLPosition.SetY(Const Value:Double);
begin
  FOwner.SetDoubleProperty(FY,Value);
end;

Procedure TGLPosition.SetZ(Const Value:Double);
begin
  FOwner.SetDoubleProperty(FZ,Value);
end;

Procedure TGLPosition.Assign(Source:TPersistent);
begin
  if Source is TGLPosition then
  With TGLPosition(Source) do
  begin
    Self.FX:=X;
    Self.FY:=Y;
    Self.FZ:=Z;
  end
  else inherited Assign(Source);
end;

{ TGLLight }
Constructor TGLLight.Create(AOwner:TTeeOpenGL);
begin
  inherited Create;
  FOwner:=AOwner;
  FColor:=clGray;
  FPosition:=TGLPosition.Create;
  With FPosition do
  begin
    FOwner:=AOwner;
    FX:=0;
    FY:=100;
    FZ:=0;
  end;
  FVisible:=True;
end;

Destructor TGLLight.Destroy;
begin
  FPosition.Free;
  inherited Destroy;
end;

Procedure TGLLight.SetColor(Value:TColor);
begin
  if FColor<>Value then
  begin
    FColor:=Value;
    FOwner.Repaint;
  end;
end;

Procedure TGLLight.SetPosition(Value:TGLPosition);
begin
  FPosition.Assign(Value);
end;

procedure TGLLight.SetVisible(Value:Boolean);
begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    FOwner.Repaint;
  end;
end;

Function TGLLight.GLColor:GLMat;
var AColor:TColor;
begin
  AColor:=ColorToRGB(FColor);
  result[0]:=GetRValue(AColor)/255.0;
  result[1]:=GetGValue(AColor)/255.0;
  result[2]:=GetBValue(AColor)/255.0;
  result[3]:=1;
end;

Function TGLLight.GLPosition:GLMat;
begin
  With FPosition do
  begin
    result[0]:=X;
    result[1]:=Y;
    result[2]:=Z;
  end;
  result[3]:=0;
end;

Procedure TGLLight.Assign(Source:TPersistent);
begin
  if Source is TGLLight then
  With TGLLight(Source) do
  begin
    Self.FColor:=Color;
    Self.FPosition.Assign(Position);
    Self.FVisible:=Visible;
  end
  else inherited Assign(Source);
end;

{ TTeeOpenGL }
Constructor TTeeOpenGL.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FLight:=TGLLight.Create(Self);
  FShininess:=0.8;
  FShadeQuality:=True;
end;

Destructor TTeeOpenGL.Destroy;
begin
  FLight.Free;
  if FActive and Assigned(FTeePanel) then SetGLPanelCanvas(FTeePanel,False);
  inherited Destroy;
end;

Procedure TTeeOpenGL.OnCanvasInit(Sender:TObject);
var tmp:GLMat;
begin
  if FShadeQuality then glShadeModel(GL_SMOOTH)
                   else glShadeModel(GL_FLAT);

  (Sender as TGLCanvas).UseMaterial:=FLight.Visible;

  if FLight.Visible then
  begin
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    tmp:=FLight.GLColor;
{    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,  @tmp);}
    glLightfv(GL_LIGHT0,  GL_AMBIENT, @tmp);
{    glLightfv(GL_LIGHT0,  GL_DIFFUSE, @tmp);}
{    glLightfv(GL_LIGHT0,  GL_SPECULAR, @tmp); }
    tmp:=FLight.GLPosition;
    glLightfv(GL_LIGHT0, GL_POSITION, @tmp);
  end
  else glDisable(GL_LIGHTING);
  glMaterialf(GL_FRONT, GL_SHININESS, 128.0*FShininess);

//  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  if Assigned(FOnInit) then FOnInit(Self);
end;

Procedure TTeeOpenGL.SetShininess(Const Value:Double);
begin
  SetDoubleProperty(FShininess,Value);
end;

Procedure TTeeOpenGL.SetShadeQuality(Value:Boolean);
begin
  if FShadeQuality<>Value then
  begin
    FShadeQuality:=Value;
    Repaint;
  end;
end;

procedure TTeeOpenGL.Notification( AComponent: TComponent;
                                   Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
     if Assigned(FTeePanel) and (AComponent=FTeePanel) then
     begin
       FTeePanel:=nil;
       FActive:=False;
     end;
end;

Procedure TTeeOpenGL.Activate;
begin
  if Assigned(FTeePanel) then
  begin
    SetGLPanelCanvas(FTeePanel,FActive);
    if FActive then (FTeePanel.Canvas as TGLCanvas).OnInit:=OnCanvasInit;
  end;
end;

Procedure TTeeOpenGL.SetLight(Value:TGLLight);
begin
  FLight.Assign(Value);
end;

Procedure TTeeOpenGL.SetActive(Value:Boolean);
begin
  if FActive<>Value then
  begin
    FActive:=Value;
    Activate;
  end;
end;

Procedure TTeeOpenGL.SetDoubleProperty(Var Variable:Double; Const Value:Double);
begin
  if Variable<>Value then
  begin
    Variable:=Value;
    Repaint;
  end;
end;

Procedure TTeeOpenGL.Repaint;
begin
  if Assigned(FTeePanel) then FTeePanel.Repaint;
end;

Procedure TTeeOpenGL.SetTeePanel(Value:TCustomTeePanel);
begin
  FTeePanel:=Value;
  if Assigned(FTeePanel) then
  begin
    FTeePanel.FreeNotification(Self);
    Activate;
  end
  else FActive:=False;
end;

Procedure Register;
begin
  RegisterComponents(TeeMsg_TeeChartPalette, [TTeeOpenGL]);
end;

{initialization
  PrivateGLClass:=nil;
  TeeCommanderChain:=TeeCommanderGLChain;
finalization
  PrivateGLClass.Free;}
end.
