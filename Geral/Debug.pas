unit Debug;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TDebuger = class(TForm)
    Memo: TMemo;
  private
    { Private declarations }
  public
    Procedure Write(S: String);
    Procedure WriteStrings(SL: TStrings);
    Procedure WriteValues(Const Vals: Array of Const);
    Procedure WriteFmt(Const Sfmt: String; Const Values: Array of Const);
    Procedure Clear();
  end;

var
  Debuger: TDebuger;

implementation

{$R *.DFM}

Procedure TDebuger.Write(S: String);
Begin
  Memo.Lines.Add(S);
  Show();
End;

Procedure TDebuger.WriteStrings(SL: TStrings);
var i: Integer;
Begin
  Show();
  Memo.Lines.Add(Format('[ CLASS: %s  INSTANCE SIZE: %d Bytes]',
  [SL.ClassName, SL.InstanceSize]));

  For i := 0 to SL.Count-1 do
    Memo.Lines.Add('  ' + IntToStr(i) + ') - ' + SL[i]);

  Memo.Lines.Add('[ END CLASS ----------------------------------------------------- ]');
  Memo.Lines.Add('');
End;

Procedure TDebuger.WriteValues(Const Vals: Array of Const);
Var i: Integer;
    O: TObject;
Begin
  Show();
  Memo.Lines.Add(Format('[ VARIABLES: %d ]', [High(Vals)+1]));

  For i := 0 to High(Vals) do
    Case Vals[i].vType of
      vtString  :  Memo.Lines.Add('  ' + Vals[i].vString^);
      vtInteger :  Memo.Lines.Add('  ' + IntToStr(Vals[i].vInteger));
      vtBoolean :  If Vals[i].vBoolean Then Memo.Lines.Add('  True')
                                       else Memo.Lines.Add('  False');

      vtChar    :  Memo.Lines.Add('  ' + Vals[i].vChar);
      vtExtended:  Memo.Lines.Add('  ' + FloatToStr(Vals[i].vExtended^));

      vtPointer :  If (Vals[i].vPointer = Nil) Then
                      Memo.Lines.Add(Format('Variable %d is NIL', [i+1]))
                   Else
                      Memo.Lines.Add(Format('Address: %p', [Vals[i].vPointer]));
      vtObject  :  
        Begin
        If (Vals[i].VObject = Nil) or (Not Assigned(Vals[i].VObject)) Then
           Begin
           Memo.Lines.Add(Format('  Variable %d is NIL', [i+1]));
           End
        Else
           Begin
           O := Vals[i].VObject;

           Memo.Lines.Add(Format('  (CLASS: %s, INSTANCE SIZE: %d Bytes)',
           [O.ClassName, O.InstanceSize]));

           If O is TList Then
              Memo.Lines.Add(Format('    Elements: %d', [TList(O).Count]));

           If O is TStrings Then   
              Memo.Lines.Add(Format('    Strings: %d', [TStringList(O).Count]));

           If O is TComponent Then
              Begin
              Memo.Lines.Add(Format('    Name: %s', [TComponent(O).Name]));
              Memo.Lines.Add(Format('    Owner: %s', [TComponent(O).Owner.Name]));
              Memo.Lines.Add(Format('    Component Count:  %d', [TComponent(O).ComponentCount]));
              Memo.Lines.Add(Format('    Tag: %d', [TComponent(O).Tag]));
              End;

           If O is TControl Then
              Begin
              Memo.Lines.Add(Format('    Top: %d', [TControl(O).Top]));
              Memo.Lines.Add(Format('    Left: %d', [TControl(O).Left]));
              Memo.Lines.Add(Format('    Width:  %d', [TControl(O).Width]));
              Memo.Lines.Add(Format('    Height: %d', [TControl(O).Height]));
              Memo.Lines.Add(Format('    Visible: %d', [Integer(TControl(O).Visible)]));
              Memo.Lines.Add(Format('    Enable: %d', [Integer(TControl(O).Enabled)]));
              Memo.Lines.Add(Format('    Hint: %s', [TControl(O).Hint]));
              End;

           If O is TWinControl Then
              Begin
              Memo.Lines.Add(Format('    Handle: %d', [TWinControl(O).Handle]));
              Memo.Lines.Add(Format('    Control Count: %d', [TWinControl(O).ControlCount]));
              Memo.Lines.Add(Format('    Tab Order:  %d', [TWinControl(O).TabOrder]));
              Memo.Lines.Add(Format('    Tab Stop: %d', [Integer(TWinControl(O).TabStop)]));
              End;
           End;
        End;
      End; {Case}

  Memo.Lines.Add('[ END VARIABLES ----------------------------------------------------- ]');
  Memo.Lines.Add('');
End;

Procedure TDebuger.WriteFmt(Const Sfmt: String; Const Values: Array of Const);
Begin
  Memo.Lines.Add(Format(Sfmt, Values));
  Show();
End;

Procedure TDebuger.Clear();
Begin
  Memo.Clear();
End;

Initialization
  Debuger := TDebuger.Create(nil);

Finalization
  Debuger.Free();  

end.
