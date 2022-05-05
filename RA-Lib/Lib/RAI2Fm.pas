{***********************************************************
                R&A Library
                   RAI2
       Copyright (C) 1999-2000 R&A

       component   : form runner for RAI2
       description : R&A Interpreter version 2

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

{ history (R&A Library versions):
  1.10:
   - first release;
  1.12:
   - more smart interface-part reducementer -
     method MakeCompatibleUnit;
  1.31.3 (R&A Library 1.31 with update 3):
   - support for Delphi5 text DFM files.
  1.52:
   - fixed memory bug;
  1.52.4:
   - previous memory bug fix was moved to rai2.pas unit;
  1.60:
   - forms, placed in used units, are supported;
   - method MakeCompatibleUnit has been removed;
  1.61:
   - fixed bug: local variables in methods overrieded by form memebers;
     this bug prevented MDI forms from "Action := caFree" code to work
     (thanks to Ivan Ravin).
}


unit RAI2Fm;

interface

uses Windows, SysUtils, Classes, Controls, Forms, RAI2, RAI2Parser;

type

  TRAI2GetDfmFileName = procedure(Sender: TObject; UnitName: string;
    var FileName: string; var Done: Boolean) of object;
  TRAI2CreateDfmStream = procedure(Sender: TObject; UnitName: string;
    var Stream: TStream; var Done: Boolean) of object;
  TRAI2FreeDfmStream = procedure(Sender: TObject; Stream: TStream) of object;

  TRAI2Fm = class;

  TRAI2Form = class(TForm)
  private
    FRAI2Fm: TRAI2Fm;
    FMethodList: TList;
    FreeFRAI2Fm: Boolean;
    FClassIdentifer: string;
    FUnitName: string;
    procedure FixupMethods;
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property RAI2Fm: TRAI2Fm read FRAI2Fm;
  end;

  TRAI2Fm = class(TRAI2Program)
  private
    FForm: TRAI2Form;
    FFileName: string;
    FInterfaceUses: Boolean;
    FOnGetDfmFileName: TRAI2GetDfmFileName;
    FOnCreateDfmStream: TRAI2CreateDfmStream;
    FOnFreeDfmStream: TRAI2FreeDfmStream;
    procedure LoadForm(AForm: TRAI2Form);
  protected
    function GetValue(Identifer: string; var Value: Variant; var Args: TArgs)
      : Boolean; override;
    function SetValue(Identifer: string; const Value: Variant; var Args: TArgs)
      : Boolean; override;
    function GetUnitSource(UnitName: string; var Source: string): boolean;
      override;
    procedure CreateDfmStream(const UnitName: string; var Stream: TStream); dynamic;
    procedure FreeDfmStream(Stream: TStream); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
    function MakeForm(const FileName: TFileName): TForm;
    function RunForm(const FileName: TFileName): TForm;
    function RunFormModal(const FileName: TFileName): TModalResult;
    function RunUnit(const FileName: TFileName): Variant;
    procedure RunReportPreview(const FileName: string);
    property Form: TRAI2Form read FForm;
  published
    property OnGetDfmFileName: TRAI2GetDfmFileName read FOnGetDfmFileName write FOnGetDfmFileName;
    property OnCreateDfmStream: TRAI2CreateDfmStream read FOnCreateDfmStream write FOnCreateDfmStream;
    property OnFreeDfmStream: TRAI2FreeDfmStream read FOnFreeDfmStream write FOnFreeDfmStream;
    property InterfaceUses: Boolean read FInterfaceUses write FInterfaceUses default False;
  end;

  function RAI2RunFormModal(const FileName: TFileName): TModalResult;
  function RAI2RunForm(const FileName: TFileName): TForm;
  function RAI2MakeForm(const FileName: TFileName): TForm;
  function RAI2RunUnit(const FileName: TFileName): Variant;
  procedure RAI2RunReportPreview(const FileName: string);
  procedure RAI2RunReportPreview2(const FileName: string; RAI2Program: TRAI2Fm);

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);


const
  ieImplementationNotFound = 401;


var
  RAI2RunReportPreviewProc: procedure (const FileName: string);
  RAI2RunReportPreview2Proc: procedure (const FileName: string; RAI2Program: TRAI2Fm);

implementation

uses Consts, TypInfo, RAUtils, RACnst;

type

  TRAI2Reader = class(TReader)
  protected
    function FindMethod(Root: TComponent; const MethodName: string): Pointer;
      override;
  end;

  THackAdapter = class(TRAI2Adapter);
  

function TRAI2Reader.FindMethod(Root: TComponent; const MethodName: string)
  : Pointer;
begin
  Result := NewStr(MethodName);
  TRAI2Form(Root).FMethodList.Add(Result);
end;

constructor TRAI2Form.Create(AOwner: TComponent);
begin
 {$IFDEF RA_D}
  CreateNew(AOwner);
 {$ELSE}
  CreateNew(AOwner, 1);
 {$ENDIF}
  FMethodList := TList.Create;
end;    { Create }

destructor TRAI2Form.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMethodList.Count - 1 do    { Iterate }
    DisposeStr(FMethodList[i]);
  FMethodList.Free;
  inherited Destroy;
  if FreeFRAI2Fm then
    FRAI2Fm.Free;
end;    { Destroy }

procedure TRAI2Form.FixupMethods;

  procedure ReadProps(Com: TComponent);
  var
    TypeInf  : PTypeInfo;
    TypeData : PTypeData;
    PropList : PPropList;
    NumProps : word;
    i: Integer;
    F: Integer;
    Method: TMethod;
  begin
    TypeInf := Com.ClassInfo;
    TypeData := GetTypeData(TypeInf);
    NumProps := TypeData^.PropCount;
    GetMem(PropList, NumProps*sizeof(pointer));
    try
      GetPropInfos(TypeInf, PropList);
      for i := 0 to NumProps-1 do
        if PropList^[i].PropType^.Kind = tkMethod then
        begin
          Method := GetMethodProp(Com, PropList^[i]);
          if Method.Data = Self then
          begin
            F := FMethodList.IndexOf(Method.Code);
            if F > -1 then
            begin
              SetMethodProp(Com, PropList^[i], TMethod(FRAI2Fm.NewEvent(
                FUnitName,
                PString(FMethodList[F])^, PropList^[i]^.PropType^.Name,
                Self)));
            end;
          end;
        end;
    finally
      FreeMem(PropList, NumProps*sizeof(pointer));
    end;
  end;

var
  i: Integer;
begin
  ReadProps(Self);
  for i := 0 to ComponentCount - 1 do    { Iterate }
    ReadProps(Components[i]);
end;

procedure TRAI2Form.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  FixupMethods;
end;

function RAI2ReadComponentRes(var Stream: TStream; Instance: TComponent)
  : TComponent;
var
  RAI2Reader: TRAI2Reader;
 {$IFDEF RA_D5H}
  TmpStream: TMemoryStream;
 {$ENDIF RA_D5H}
begin
 {$IFDEF RA_D5H}
  if TestStreamFormat(Stream) = sofText then
  begin
    TmpStream := TMemoryStream.Create;
    ObjectTextToResource(Stream, TmpStream);
    Stream.Free;
    Stream := TmpStream;
    Stream.Position := 0;
  end;
 {$ENDIF RA_D5H}

  Stream.ReadResHeader;
  RAI2Reader := TRAI2Reader.Create(Stream, 4096);
  try
    Result := RAI2Reader.ReadRootComponent(Instance);
  finally
    RAI2Reader.Free;
  end;
end;


{ ********************* TRAI2Fm **********************}
constructor TRAI2Fm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;    { Create }

destructor TRAI2Fm.Destroy;
begin
  inherited Destroy;
end;    { Destroy }

function TRAI2Fm.MakeForm(const FileName: TFileName): TForm;
var
  S: string;
  UnitName: string;
begin
  FFileName := FileName;
  UnitName := ChangeFileExt(ExtractFileName(FFileName), '');
  if not (GetUnitSource(FFileName, S) or GetUnitSource(UnitName, S)) then
    RAI2ErrorN(ieUnitNotFound, -1, UnitName);
  Source := S;
  Compile;
  FForm := TRAI2Form.Create(Application);
  FForm.FUnitName := UnitName;
  LoadForm(FForm);
  Result := FForm;
end;    { MakeForm }

procedure TRAI2Fm.CreateDfmStream(const UnitName: string; var Stream: TStream);
var
  Done: Boolean;
  DfmFile: string;
begin
  Done := False;
  if Assigned(FOnCreateDfmStream) then
    FOnCreateDfmStream(Self, UnitName, Stream, Done);
  if not Done then
  begin
    if Assigned(FOnGetDfmFileName) then
      FOnGetDfmFileName(Self, UnitName, DfmFile, Done);
    if not Done then
      DfmFile := FindInPath(ChangeFileExt(UnitName, '.dfm'),
        ExtractFilePath(FFileName));
    Done := FileExists(DfmFile);
    if Done then
      Stream := TFileStream.Create(DfmFile, fmOpenRead);
  end;

  if not Done then
    RAI2ErrorN(ieDfmNotFound, -1, UnitName);
end;

procedure TRAI2Fm.FreeDfmStream(Stream: TStream);
begin
  if Assigned(FOnFreeDfmStream) then
    FOnFreeDfmStream(Self, Stream)
  else
    Stream.Free;
end;

procedure TRAI2Fm.LoadForm(AForm: TRAI2Form);
var
  Stream: TStream;
begin
  FForm := AForm;
  Form.FRAI2Fm := Self;
  CreateDfmStream(FForm.FUnitName, Stream);
  try
    RAI2ReadComponentRes(Stream, Form);
  finally
    FreeDfmStream(Stream);
  end;
  try
    if Assigned(Form.OnCreate) then Form.OnCreate(Form);
  except
    Application.HandleException(Form);
  end;
  if Form.FormStyle <> fsMDIChild then
    Form.Visible := False;
end;

function TRAI2Fm.GetValue(Identifer: string; var Value: Variant;
  var Args: TArgs): Boolean;

  function GetFromForm(Form: TRAI2Form): Boolean;
  var
    Com: TComponent;
  begin
    if Cmp(Identifer, 'Self') then
    begin
      Value := O2V(Form);
      Result := True;
      Exit;
    end;
    Com := Form.FindComponent(Identifer);
    if Com = nil then
    begin
      if (LocalVars <> nil) and (LocalVars.FindVar('', Identifer) <> nil) then
      begin
        Result := LocalVars.GetValue(Identifer, Value, Args);
        Exit;
      end;
     { may be TForm method or published property }
      Args.Obj := Form;
      Args.ObjTyp := varObject;
      try
        Result := inherited GetValue(Identifer, Value, Args);
      finally
        Args.Obj := nil;
        Args.ObjTyp := 0;
      end;
    end
    else
    begin
      Value := O2V(Com);
      Result := True;
    end;
  end;

var
  RAI2SrcClass: TRAI2Identifer;
  RAI2Form: TRAI2Form;
begin
  if (Args.Obj = nil) and (CurInstance is TRAI2Form) then
    Result := GetFromForm(CurInstance as TRAI2Form)
  else
  if (Args.Obj <> nil) and (Args.ObjTyp = varObject) and
     (Args.Obj is TRAI2Form) then
  begin
   { run-time form creation }
    if Cmp(Identifer, 'Create') then
    begin
      RAI2SrcClass := THackAdapter(Adapter).GetSrcClass(
        (Args.Obj as TRAI2Form).FClassIdentifer);
      (Args.Obj as TRAI2Form).FUnitName := RAI2SrcClass.UnitName;  
      LoadForm(Args.Obj as TRAI2Form);
      Value := O2V(Args.Obj);
      Result := True;
      Exit;
    end
    else
      Result := GetFromForm(Args.Obj as TRAI2Form)
  end
  else
    Result := False;

  if Result then Exit;

  { run-time form creation }
  RAI2SrcClass := THackAdapter(Adapter).GetSrcClass(Identifer);
  if RAI2SrcClass <> nil then
  begin
    RAI2Form := TRAI2Form.Create(Application);
    RAI2Form.FClassIdentifer := Identifer;
    Value := O2V(RAI2Form);
    Result := True;
    Exit;
  end;

  Result := Result or inherited GetValue(Identifer, Value, Args);
end;    { GetValue }

function TRAI2Fm.SetValue(Identifer: string; const Value: Variant;
  var Args: TArgs): Boolean;
begin
  if (Args.Obj = nil) and (CurInstance is TRAI2Form) then
  begin
    if (LocalVars <> nil) and (LocalVars.FindVar('', Identifer) <> nil) then
    begin
      Result := LocalVars.SetValue(Identifer, Value, Args);
      Exit;
    end;
   { may be TForm method or published property }
    Args.Obj := CurInstance;
    Args.ObjTyp := varObject;
    try
      Result := inherited SetValue(Identifer, Value, Args);
    finally
      Args.Obj := nil;
      Args.ObjTyp := 0;
    end;
  end
  else
    Result := False;
  Result := Result or inherited SetValue(Identifer, Value, Args);
end;    { SetValue }

function TRAI2Fm.GetUnitSource(UnitName: string; var Source: string): boolean;
var
  FN: TFileName;
begin
  if not FInterfaceUses and (UnitSection = usInterface) then
  begin
    Source := 'unit ' + UnitName + '; end.';
    Result := True;
  end
  else
  begin
    Result := inherited GetUnitSource(UnitName, Source);
    if not Result then
    begin
      if ExtractFileExt(UnitName) = '' then
        UnitName := UnitName + '.pas';
      if FileExists(UnitName) then
        FN := UnitName
      else
        FN := FindInPath(ExtractFileName(UnitName), ExtractFilePath(FFileName));
      Result := FileExists(FN);
      if Result then
        Source := LoadTextFile(FN)
    end;
  end;
end;

procedure TRAI2Fm.Run;
begin
  inherited Run;
end;

function TRAI2Fm.RunForm(const FileName: TFileName): TForm;
begin
  Result := MakeForm(FileName);
  Result.Show;
end;

function TRAI2Fm.RunFormModal(const FileName: TFileName): TModalResult;
begin
  with MakeForm(FileName) do
    try
      Result := ShowModal;
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

function TRAI2Fm.RunUnit(const FileName: TFileName): Variant;
var
  UnitName: string;
  S: string;
begin
  FFileName := FileName;
  try
    UnitName := ChangeFileExt(ExtractFileName(FFileName), '');
    if not (GetUnitSource(FFileName, S) or GetUnitSource(UnitName, S)) then
      RAI2ErrorN(ieUnitNotFound, -1, UnitName);
    Source := S;
  except
    RAI2ErrorN(ieUnitNotFound, -1, FFileName);
  end;
  Run;
end;

procedure TRAI2Fm.RunReportPreview(const FileName: string);
begin
  RAI2RunReportPreview2(FileName, Self);
end;

function RAI2RunFormModal(const FileName: TFileName): TModalResult;
begin
  with TRAI2Fm.Create(Application) do
    try
      Result := RunFormModal(FileName);
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

function RAI2RunForm(const FileName: TFileName): TForm;
begin
  with TRAI2Fm.Create(Application) do
  begin
    Result := RunForm(FileName);
    (Result as TRAI2Form).FreeFRAI2Fm := True;
  end;
end;

function RAI2MakeForm(const FileName: TFileName): TForm;
begin
  with TRAI2Fm.Create(Application) do
  begin
    Result := MakeForm(FileName);
    (Result as TRAI2Form).FreeFRAI2Fm := True;
  end;
end;

function RAI2RunUnit(const FileName: TFileName): Variant;
begin
  with TRAI2Fm.Create(Application) do
    try
      Result := RunUnit(FileName);
    finally { wrap up }
      Free;
    end;    { try/finally }
end;

{ adapter to self }
{ function RAI2RunFormModal(const FileName: TFileName): TModalResult; }
procedure RAI2_RAI2RunFormModal(var Value: Variant; Args: TArgs);
begin
  Value := RAI2RunFormModal(Args.Values[0]);
end;

{ function RAI2RunForm(const FileName: TFileName): TForm; }
procedure RAI2_RAI2RunForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(RAI2RunForm(Args.Values[0]));
end;

{ function RAI2MakeForm(const FileName: TFileName): TForm; }
procedure RAI2_RAI2MakeForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(RAI2MakeForm(Args.Values[0]));
end;

{ function RAI2RunUnit(const FileName: TFileName): Variant }
procedure RAI2_RAI2RunUnit(var Value: Variant; Args: TArgs);
begin
  Value := RAI2RunUnit(Args.Values[0]);
end;

procedure RAI2RunReportPreview(const FileName: string);
begin
  if not Assigned(RAI2RunReportPreviewProc) then
    Error(SNoReportProc);
  RAI2RunReportPreviewProc(FileName);
end;

procedure RAI2RunReportPreview2(const FileName: string; RAI2Program: TRAI2Fm);
begin
  if not Assigned(RAI2RunReportPreview2Proc) then
    Error(SNoReportProc2);
  RAI2RunReportPreview2Proc(FileName, RAI2Program);
end;

procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
    AddFun('RAI2Fm', 'RAI2RunFormModal', RAI2_RAI2RunFormModal, 1, [varString], varEmpty);
    AddFun('RAI2Fm', 'RAI2RunForm', RAI2_RAI2RunForm, 1, [varString], varEmpty);
    AddFun('RAI2Fm', 'RAI2MakeForm', RAI2_RAI2MakeForm, 1, [varString], varEmpty);
    AddFun('RAI2Fm', 'RAI2RunUnit', RAI2_RAI2RunUnit, 1, [varString], varEmpty);
  end;    { with }
end;    { RegisterRAI2Adapter }


end.
