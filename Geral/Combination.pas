unit Combination;

interface

uses
  Classes;

type
  TCombinationEvent = procedure (Sender: TObject; const Elements: array of Integer) of Object;

type
  TCombination = class(TThread)
  private
    FCombVec         : array of Integer;
    FSizeSet         : Integer;
    FNumElements     : Integer;
    FOnCombination   : TCombinationEvent;
    FNumCombinations : Integer;
    FElapse          : Cardinal;
    FInicio          : Cardinal;
    
    procedure DoCombination;
  protected
    procedure Execute; override;
  public
    constructor Create(NumElements, SizeSet: Integer; OnCombination: TCombinationEvent);

    property Elapse          : Cardinal          read FElapse;
    property NumCombinations : Integer           read FNumCombinations;

    property NumElements     : Integer           read FNumElements      write FNumElements;
    property SizeSet         : Integer           read FSizeSet          write FSizeSet;

    property OnCombination   : TCombinationEvent read FOnCombination    write FOnCombination;
  end;

implementation
uses Windows;

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TCombination.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TCombination }

constructor TCombination.Create(NumElements, SizeSet: Integer; OnCombination: TCombinationEvent);
begin
  inherited Create(False);
  FNumElements := NumElements;
  FSizeSet := SizeSet;
  FOnCombination := OnCombination;
  FreeOnTerminate := True;
end;

procedure TCombination.DoCombination;
begin
  inc(FNumCombinations);
  FOnCombination(Self, FCombVec);
end;

procedure TCombination.Execute;

    procedure Comb(const Inicio, Pos: Integer);
    var i: Integer;
    begin
      for i := Inicio to FNumElements-FSizeSet+Pos do
        begin
        if Terminated then Exit;
        FCombVec[Pos] := i;
        if Pos = FSizeSet-1 then
           Synchronize(DoCombination)
        else
           Comb(i + 1, Pos + 1);
        end;
    end;

begin
  FInicio := GetTickCount;
  FNumCombinations := 0;
  SetLength(FCombVec, FSizeSet);
  Comb(0, 0);
  FElapse := GetTickCount - FInicio;
end;

end.
