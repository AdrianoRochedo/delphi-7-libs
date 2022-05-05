{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       component   : TRASQLScript, TDBRadioGroupNF
       description : small db-aware components

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RADBCt;

interface

uses
  Windows, Messages, Bde, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, DBTables, DBCtrls, RADBUtil;

type
  TRASQLScript = class;
  TOnScriptProgress = procedure(Sender : TRASQLScript; var Cancel : boolean; Line : integer) of object;

  TRASQLScript = class(TComponent)
  private
    FOnProgress : TOnScriptProgress;
    FScript : TStrings;
    FCommit : TCommit;
    FDatabase : TDatabase;
    procedure SetScript(AValue : TStrings);
    procedure Progress(UserData : integer; var Cancel : boolean; Line : integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    procedure Execute;
    property OnProgress : TOnScriptProgress read FOnProgress write FOnProgress;
    property Script : TStrings read FScript write SetScript;
    property Commit : TCommit read FCommit write FCommit;
    property Database : TDatabase read FDatabase write FDatabase;
  end;

 { non-framed TDBRadioGroup }
  TRADBRadioGroupS = class(TDBRadioGroup)
  private
    FBeveled : boolean;
    procedure SetBeveled(const Value: boolean);
  protected
    procedure Paint; override;
  published
    property Beveled : boolean read FBeveled write SetBeveled;
  end;

implementation

uses RADsgnIntf;

{******************* TRASQLScript ********************}
constructor TRASQLScript.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
end;

destructor TRASQLScript.Destroy;
begin
  FScript.Free;
  inherited Destroy;
end;

procedure TRASQLScript.SetScript(AValue : TStrings);
begin
  FScript.Assign(AValue);
end;

procedure TRASQLScript.Execute;
begin
  ExecuteSQLScript(FDatabase, FScript.Text, FCommit, Progress, 0);
end;

procedure TRASQLScript.Progress(UserData : integer; var Cancel : boolean; Line : integer);
begin
  if Assigned(FOnProgress) then FOnProgress(Self, Cancel, Line);
end;

{************************ TDBRadioGroupNF ************************}
procedure TRADBRadioGroupS.Paint;
begin
  if FBeveled then inherited
  else if (csDesigning in ComponentState) then
    DrawDesignFrame(Canvas, ClientRect);
end;

procedure TRADBRadioGroupS.SetBeveled(const Value: boolean);
begin
  FBeveled := Value;
  Refresh;
end;


end.
