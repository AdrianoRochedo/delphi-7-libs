{: GLSMBASS<p>

	BASS based sound-manager (http://www.un4seen.com/music/, free for freeware).<p>

   Unsupported feature(s) :<ul>
      <li>sound source velocity
      <li>looping (sounds are played either once or forever)
      <li>source priorities (not relevant, channels are not limited)
   </ul><p>

	<b>Historique : </b><font size=-1><ul>
      <li>05/02/02 - Egg - BASS 1.4 compatibility
      <li>05/02/01 - Egg - Fixed TGLSMBASS.CPUUsagePercent
	   <li>13/01/01 - Egg - Creation (compat BASS 0.8)
	</ul></font>
}
unit GLSMBASS;

interface

uses Classes, GLSound, GLScene;

type

   // TBASS3DAlgorithm
   //
   TBASS3DAlgorithm = (algDefault, algOff, algFull, algLight);

	// TGLSMBASS
	//
	TGLSMBASS = class (TGLSoundManager)
	   private
	      { Private Declarations }
         FAlgorithm3D : TBASS3DAlgorithm;

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;
         procedure NotifyMasterVolumeChange; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;
         procedure MuteSource(aSource : TGLBaseSoundSource; muted : Boolean); override;
         procedure PauseSource(aSource : TGLBaseSoundSource; paused : Boolean); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function CPUUsagePercent : Single; override;

	   published
	      { Published Declarations }
         property Algorithm3D : TBASS3DAlgorithm read FAlgorithm3D write FAlgorithm3D default algDefault;
	end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses Forms, SysUtils, Bass, Geometry, Dialogs;

type
   TBASSInfo =  record
      channel : HCHANNEL;
      sample : HSAMPLE;
   end;
   PBASSInfo = ^TBASSInfo;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLSMBASS]);
end;

// VectorToBASSVector
//
procedure VectorToBASSVector(const aVector : TVector; var aBASSVector : BASS_3DVECTOR);
begin
   with aBASSVector do begin
      x:=aVector[0];
      y:=aVector[1];
      z:=-aVector[2];
   end;
end;

// ------------------
// ------------------ TGLSMBASS ------------------
// ------------------

// Create
//
constructor TGLSMBASS.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=32;
end;

// Destroy
//
destructor TGLSMBASS.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMBASS.DoActivate : Boolean;
const
   c3DAlgo : array [algDefault..algLight] of Integer =
      (BASS_3DALG_DEFAULT, BASS_3DALG_OFF, BASS_3DALG_FULL, BASS_3DALG_LIGHT);
begin
   if not BASS_Init(-1, OutputFrequency, BASS_DEVICE_3D, Application.Handle) then Assert(False);
   if not BASS_Start then Assert(False);
   BASS_Set3DAlgorithm(c3DAlgo[FAlgorithm3D]);
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMBASS.DoDeActivate;
begin
   BASS_Stop;
   BASS_Free;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMBASS.NotifyMasterVolumeChange;
begin
   BASS_SetVolume(Round(MasterVolume*100));
end;

// KillSource
//
procedure TGLSMBASS.KillSource(aSource : TGLBaseSoundSource);
var
   p : PBASSInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if p.channel<>0 then
         if not BASS_ChannelStop(p.channel) then Assert(False);
      BASS_SampleFree(p.sample);
      FreeMem(p);
      aSource.ManagerTag:=0;
   end;
end;

// UpdateSource
//
procedure TGLSMBASS.UpdateSource(aSource : TGLBaseSoundSource);
var
   i : Integer;
   p : PBASSInfo;
   objPos, objOri, objVel : TVector;
   position, orientation, velocity : BASS_3DVECTOR;
begin
   if (aSource.Sample=nil) or (aSource.Sample.Data.WAVDataSize=0) then Exit;
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if BASS_ChannelIsActive(p.channel)=0 then begin
         aSource.Free;
         Exit;
      end;
   end else begin
      p:=AllocMem(SizeOf(TBASSInfo));
      p.channel:=0;
      i:=BASS_SAMPLE_VAM+BASS_SAMPLE_3D+BASS_SAMPLE_OVER_DIST;
      if aSource.NbLoops>1 then
         i:=i+BASS_SAMPLE_LOOP;
      p.sample:=BASS_SampleLoad(True, aSource.Sample.Data.WAVData, 0,
                                aSource.Sample.Data.WAVDataSize,
                                MaxChannels, i);
      Assert(p.sample<>0, 'BASS Error '+IntToStr(BASS_ErrorGetCode));
      aSource.ManagerTag:=Integer(p);
   end;
   if aSource.Origin<>nil then begin
      objPos:=aSource.Origin.AbsolutePosition;
      objOri:=aSource.Origin.AbsoluteZVector;
      objVel:=NullHmgVector;
   end else begin
      objPos:=NullHmgPoint;
      objOri:=ZHmgVector;
      objVel:=NullHmgVector;
   end;
   VectorToBASSVector(objPos, position);
   VectorToBASSVector(objVel, velocity);
   VectorToBASSVector(objOri, orientation);
   if p.channel=0 then begin
      p.channel:=BASS_SamplePlay3D(p.sample, position, orientation, velocity);
      Assert(p.channel<>0);
      BASS_ChannelSet3DAttributes(p.channel, BASS_3DMODE_NORMAL,
                                  aSource.MinDistance, aSource.MaxDistance,
                                  Round(aSource.InsideConeAngle),
                                  Round(aSource.OutsideConeAngle),
                                  Round(aSource.ConeOutsideVolume*100));
   end else BASS_ChannelSet3DPosition(p.channel, position, orientation, velocity);
   if p.channel<>0 then begin
      if not BASS_ChannelSetAttributes(p.channel, -1, Round(aSource.Volume*100), -101) then
         Assert(False);
   end else aSource.Free;
end;

// MuteSource
//
procedure TGLSMBASS.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
var
   p : PBASSInfo;
   res : Boolean;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if muted then
         res:=BASS_ChannelSetAttributes(p.channel, -1, 0, -101)
      else res:=BASS_ChannelSetAttributes(p.channel, -1, Round(aSource.Volume*100), -101);
      Assert(res);
   end;
end;

// PauseSource
//
procedure TGLSMBASS.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
var
   p : PBASSInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if paused then
         BASS_ChannelPause(p.channel)
      else BASS_ChannelResume(p.channel);
   end;
end;

// UpdateSources
//
procedure TGLSMBASS.UpdateSources;
var
   objPos, objVel, objDir, objUp : TVector;
   position, velocity, fwd, top : BASS_3DVECTOR;
begin
   // update listener
   ListenerCoordinates(objPos, objVel, objDir, objUp);
   VectorToBASSVector(objPos, position);
   VectorToBASSVector(objVel, velocity);
   VectorToBASSVector(objDir, fwd);
   VectorToBASSVector(objUp, top);
   if not BASS_Set3DPosition(position, velocity, fwd, top) then Assert(False);
   // update sources
   inherited;
   if not BASS_Apply3D then Assert(False);
end;

// CPUUsagePercent
//
function TGLSMBASS.CPUUsagePercent : Single;
begin
   Result:=BASS_GetCPU*100;
end;

end.

