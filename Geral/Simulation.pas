unit Simulation;

interface

uses
  Classes;

type
  TBeforeExecuteEvent = procedure(Sender: TObject) of object;
  TAfterExecuteEvent  = procedure(Sender: TObject) of object;
  TClockEvent         = procedure(Sender: TObject; out EventID: Integer) of object;
  TCallEvent          = procedure(Sender: TObject; const EventID: Integer) of object;
  TProgressEvent      = procedure of Object;

  TSimulationTThread = class(TThread)
  private
    FOnClock         : TClockEvent;
    FOnEvent         : TCallEvent;
    FOnBeforeExecute : TBeforeExecuteEvent;
    FOnAfterExecute  : TAfterExecuteEvent;
    FOnProgress      : TProgressEvent;
  protected
    procedure Execute; override;
    procedure DoBeforeExecute;
    procedure DoAfterExecute;
  Public
    Property OnClock: TClockEvent read FOnClock write FOnClock;
    Property OnEvent: TCallEvent  read FOnEvent write FOnEvent;

    Property OnBeforeExecute: TBeforeExecuteEvent
       read FOnBeforeExecute
       write FOnBeforeExecute;

    Property OnAfterExecute: TAfterExecuteEvent
       read FOnAfterExecute
       write FOnAfterExecute;

   Property OnDoVisual: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TSimulation = class
  private
    FOnClock         : TClockEvent;
    FOnEvent         : TCallEvent;
    FOnBeforeExecute : TBeforeExecuteEvent;
    FOnAfterExecute  : TAfterExecuteEvent;
    FOnProgress      : TProgressEvent;
    FTerminated      : Boolean;
    FFreeOnTerminate : Boolean;
    FPaused          : Boolean;
    FDelay           : Integer;
  protected
    procedure Execute;
  Public
    constructor Create(Suspended: Boolean);
    procedure Terminate;
    procedure Resume;
    procedure Pause;
    procedure Continue;

    property Delay : Integer read FDelay write FDelay;

    Property OnClock: TClockEvent read FOnClock write FOnClock;
    Property OnEvent: TCallEvent  read FOnEvent write FOnEvent;

    Property OnBeforeExecute: TBeforeExecuteEvent
       read FOnBeforeExecute
       write FOnBeforeExecute;

    Property OnAfterExecute: TAfterExecuteEvent
       read FOnAfterExecute
       write FOnAfterExecute;

   Property OnProgress: TProgressEvent read FOnProgress write FOnProgress;

   property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
  end;

implementation
uses Forms, WinUtils;

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TSimulation.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TSimulationTThread }

procedure TSimulationTThread.DoBeforeExecute;
begin
  FOnBeforeExecute(Self);
end;

procedure TSimulationTThread.DoAfterExecute;
begin
  FOnAfterExecute(Self);
end;

// ATENÇÃO: Não utilizar nada de visual nos FOnClock e FOnEvent
procedure TSimulationTThread.Execute;
var EventID: Integer;
begin
  if Assigned(FOnBeforeExecute) Then Synchronize(DoBeforeExecute);
  try
    while not Terminated do
      begin
      if Assigned(FOnProgress) Then Synchronize(FOnProgress);
      if Assigned(FOnClock)    Then FOnClock(Self, EventID);
      if Assigned(FOnEvent)    Then FOnEvent(Self, EventID);
      end;
  finally
    if Assigned(FOnAfterExecute) Then Synchronize(DoAfterExecute);
  end
end;

{ TSimulation }

procedure TSimulation.Continue;
begin
  FPaused := False;
end;

constructor TSimulation.Create(Suspended: Boolean);
begin
  inherited Create;
  if not Suspended then Execute;
end;

procedure TSimulation.Execute;
var EventID: Integer;
begin
  FTerminated := False;
  if Assigned(FOnBeforeExecute) Then FOnBeforeExecute(Self);
  try
    while not FTerminated do
      begin
      if FPaused then repeat Application.ProcessMessages until not FPaused;
      if Assigned(FOnClock) Then FOnClock(Self, EventID);
      if Assigned(FOnEvent) Then FOnEvent(Self, EventID);
      if Assigned(FOnProgress) Then FOnProgress();
      if FDelay = 0 then
         Application.ProcessMessages
      else
         WinUtils.Delay(FDelay);
      end;
  finally
    try
      if Assigned(FOnAfterExecute) Then FOnAfterExecute(Self);
    finally
      if FFreeOnTerminate then Free;
    end;  
  end
end;

procedure TSimulation.Pause;
begin
  FPaused := True;
end;

procedure TSimulation.Resume;
begin
  FPaused := False;
  Execute;
end;

procedure TSimulation.Terminate;
begin
  FTerminated := True;
  FPaused := False;
end;

end.
