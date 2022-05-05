unit Solver.LP_Solver.ResReader;

interface
uses Classes,
     Graphics,
     SysUtils,
     SysUtilsEx,
     Solver.Classes;

type
  TLP_Solver_Res_Reader = class
  private
    FWriter: ITextWriter;
    FBlack, FRed, FBlue: SysUtilsEx.TFontAttributesRec;
    FVars: TVariableList;
    procedure initFont(var Font: TFontAttributesRec; size: integer; color: TColor);
    procedure LoadVars(Text: TStrings; StartPos: integer; PB: IProgressBar);
  public
    constructor Create(Writer: ITextWriter);
    destructor Destroy(); override;

    procedure LoadFromFile(const Filename: string; PB: IProgressBar);

    property Variables : TVariableList read FVars;
  end;

implementation

{ TLP_Solver_Res_Reader }

constructor TLP_Solver_Res_Reader.Create(Writer: ITextWriter);
begin
  inherited Create();
  FWriter := Writer;
  initFont(FBlack, 9, clBlack);
  initFont(FRed, 9, clRed);
  initFont(FBlue, 9, clBlue);
  FVars := TVariableList.Create()
end;

destructor TLP_Solver_Res_Reader.Destroy();
begin
  FVars.Free();
  inherited;
end;

procedure TLP_Solver_Res_Reader.initFont(var Font: TFontAttributesRec; size: integer; color: TColor);
begin
  Font.FontName := 'Courier New';
  Font.Size := size;
  Font.Color := color;
end;

procedure TLP_Solver_Res_Reader.LoadFromFile(const Filename: string; PB: IProgressBar);
var SL: TStrings;
     k: integer;
     i: integer;
     j: integer;
     s: string;
begin
  PB.setMin(0);

  SL := TStringList.Create();
  try
    FWriter.Write('Lendo arquivo de resultados para a memória ...', FBlack);
    SL.LoadFromFile(Filename);
    FWriter.Write('Arquivo contém ' + toString(SL.Count) + ' linhas.', FBlack);
    FWriter.Write('');

    PB.setMessage('Lendo Resultados da otimização');
    PB.setMax(SL.Count-1);

    k := 0;
    s := 'SUBMITTED';
    if SysUtilsEx.Locate(s, SL, k, true) then
       begin
       FWriter.Write(s, FRed);
       for i := k+1 to k+3 do FWriter.Write('  ' + SL[i], FBlack);
       FWriter.Write('');
       end
    else
       k := 0;

    PB.setValue(k);

    s := 'PRESOLVED';
    if SysUtilsEx.Locate(s, SL, k, true) then
       begin
       FWriter.Write(s, FRed);
       for i := k+1 to k+3 do FWriter.Write('  ' + SL[i], FBlack);
       FWriter.Write('');

       inc(k, 5);
       j := k;
       if SysUtilsEx.Locate('Value of objective function:', SL, k, true) then
          begin
          FWriter.Write('OTHERS INFORMATIONS', FRed);
          for i := J to k-2 do FWriter.Write('  ' + SL[i], FBlack);
          FWriter.Write('');
          end
       else
          k := j;
       end
    else
       k := 0;

    PB.setValue(k);

    s := 'Final solution';
    if SysUtilsEx.Locate(s, SL, k, true) then
       begin
       FWriter.Write(UpperCase(s), FRed);
       j := k;
       if SysUtilsEx.Locate('Value of objective function:', SL, k, true) then
          for i := j to k do FWriter.Write('  ' + SL[i], FBlack);

       if SysUtilsEx.Locate('Actual values of the variables:', SL, k, true) then
          LoadVars(SL, k+1, PB);
       end;

    PB.setValue(0);
    PB.setMessage('Progresso da Operação:');

  finally
    SL.Free();
  end;
end;

procedure TLP_Solver_Res_Reader.LoadVars(Text: TStrings; StartPos: integer; PB: IProgressBar);
var i: Integer;
    s: string;
begin
  SysUtilsEx.SaveDecimalSeparator();
  try
    for i := StartPos to Text.Count-1 do
      begin
      s := Text[i];
      FVars.Add({Name}  Copy(s, 1, 12, true),
                {Value} toFloat(Copy(s, 13, 22, true)));
      end;
  finally
    SysUtilsEx.RestoreDecimalSeparator();
  end;

  // Testes ...

  FVars.Sort();
  FWriter.Write('');
  FWriter.Write('VARIABLES (' + toString(FVars.Count) + ')', FRed);
  for i := 0 to FVars.Count-1 do
    begin
    FWriter.Write('  ' + FVars[i].ToString(), FBlack);
    PB.setValue(i);
    end;
end;

end.
