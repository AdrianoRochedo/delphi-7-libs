unit Rosenbrock_Optimizer;

interface
uses Classes,
     SysUtils,
     Graphics,
     Optimizer_Interfaces,
     Optimizer_Base,
     Optimizer_Form_ParsManager,
     Optimizer_Form_ParsManager_Rosen;

type

  TRosenbrock = class(TOptimizer)
  private
    FMTS          : Longword;
    FMS           : Longword;
    NumEstagio    : Integer;
    FO            : Real;
    FIncPasso     : Real;
    FDecPasso     : Real;
    FPassoInicial : Boolean;

    procedure SetDecPasso(const Value: Real);
    procedure SetIncPasso(const Value: Real);
    procedure Execute(); override;

    function CreateParsManager(): TfoParsManager; override;
  Public
    constructor Create();

    property IncStep           : Real           read FIncPasso     write SetIncPasso;
    property DecStep           : Real           read FDecPasso     write SetDecPasso;
    property InitialStep       : Boolean        read FPassoInicial write FPassoInicial;
    property MaxSimulations    : Longword       read FMS           write FMS;
    property MaxTimeSimulation : Longword       read FMTS          write FMTS;
  end;

implementation
uses windows, Messages;

{ TRosenbrock }

constructor TRosenbrock.Create();
begin
  inherited Create();

  FMTS := 3888000000; // 45 dias
  FMS := System.MaxInt;
  FIncPasso := 3.0;
  FDecPasso := 0.5;
end;

type
  TADD    = array of Real;
  TAADD   = array of TADD;
  TACD    = array of Char;
  TABD    = array of Boolean;
  TAID    = array of Byte;

function TRosenbrock.CreateParsManager: TfoParsManager;
begin
  result := TfoParsManager_Rosen.Create(nil);
end;

procedure TRosenbrock.Execute();

var Ciclo, J              : Longword;
    F, FO, FBest, TM      : Real;
    FTol_FO_ABS           : Real;
    B, V, VV              : TAADD;
    Eint, D, AL, H, BX    : TADD;
    FTL_ABS               : TADD;
    PriVez                : TABD;
    Marca                 : TAID;
    UmaSimulacao          : Boolean;
    Melhorou              : Boolean;
    F_Igual_FO            : Boolean;

    procedure Inicializacoes();
    var K, I: Integer;
        Par: TParameter;
    begin
      FStop := False;

      // Dimensiona dinamicamente os Arrays
      I := FPars.Count;

      SetLength(Eint,    I);
      SetLength(D,       I);
      SetLength(AL,      I);
      SetLength(H,       I);
      SetLength(BX,      I);
      SetLength(FTL_ABS, I);
      SetLength(PriVez,  I);
      SetLength(Marca,   I);

      SetLength(B,  I, I);
      SetLength(V,  I, I);
      SetLength(VV, I, I);

      NumEstagio := 0;
      UmaSimulacao := False;
      FPassoInicial := True;

      TfoParsManager_Rosen(FParsManager).RotationCount := 0;

      // AVALIACAO INICIAL DA FUNCAO-OBJETIVO
      FProject.o_CalculateObjetiveFunctions();
      F := self.FOFValues.AsDouble[0];

      FO := F;
      FBEST := FO;

      UpdateOFValue(0, F);

      // ESTABELECE TOLERANCIA DE AJUSTE DA FUNCAO-OBJETIVO
      FTol_FO_ABS := FTolerance * F;

      // ESTABELECE ZONA FRONTEIRICA DA REGIAO VIAVEL E COMPUTA PRECISAO PARA PARAMETROS
      for K := 0 to FPars.Count-1 do
        begin
        Par := FPars[k];
        AL[K] := (Par.Max - Par.Min) * Par.Tolerance;   //??? tratar zeros
        FTL_ABS[K] := Par.Tolerance * ABS(Par.Step);
        end;

      // INICIALIZA COMO INDENTIDADE MATRIZ V DOS VETORES UNIDIRECIONAIS
      for I := 0 to FPars.Count-1 do
        for K := 0 to FPars.Count-1 do
          if I = K then V[I,K] := 1 else V[I,K] := 0;

      // GUARDA VALORES INICIAIS DO PASSO DE VARIACAO DE CADA VARIAVEL
      for K := 0 to FPars.Count-1 do EINT[K] := FPars[k].Step;

      // Obtem o tempo inicial
      TM := GetTickCount();
    end;

    function ParametrosViaveis(): Boolean;
    var K: Integer;
        Par: TParameter;
    begin
      Result := True;
      for K := 0 to FPars.Count-1 do
        begin
        Par := FPars[k];
        if (Par.Value < Par.Min) or (Par.Value > Par.Max) then
           begin
           Result := False;
           Break;
           end;
        end;
    end;

    procedure IncrementarEstagio();
    begin
      inc(NumEstagio);
      TfoParsManager_Rosen(FParsManager).RotationCount := NumEstagio;
    end;

    procedure RotacionaEixo();
    var K, KK, R, C  : Integer;
        SUMVM, SUMAV : Real;
        FNV          : Integer;
    begin
      FNV := FPars.Count;

      for R := 0 to FNV-1 do
        for C := 0 to FNV-1 do
           VV[C,R] := 0.0;

      for R := 0 to FNV-1 do
        for C := 0 to FNV-1 do
          begin
          for K := R to FNV-1 do VV[R,C] := D[K] * V[K,C] + VV[R,C];
          B[R,C] := VV[R,C];
          end;

      BX[0] := 0.0;
      for C := 0 to FNV-1 do
        BX[0] := BX[0] + SQR(B[0,C]);

      BX[0] := SQRT(BX[0]);
      IF BX[0] = 0 then BX[0] := 0.000001;

      for C := 0 to FNV-1 do
        V[0,C] := B[0,C] / BX[0];

      for R := 1 to FNV-1 do  // Aqui começa em 1 mesmo
        for C := 0 to FNV-1 do
          begin
          SUMVM := 0.0;
          for K := 0 to R-2 do  // vai até o penultimo
            begin
            SUMAV := 0.0;
            for KK := 0 to FNV-1 do SUMAV := SUMAV + VV[R,KK]* V[K,KK];
            SUMVM := SUMAV*V[K,C] + SUMVM;
            end;
          B[R,C] := VV[R,C] - SUMVM;
          end;

      for R := 1 to FNV-1 do  // Aqui começa em 1
        begin
        BX[R] :=  0.0;
        for K := 0 to FNV-1 do BX[R] := BX[R] + B[R,K] * B[R,K];
        BX[R] := SQRT(BX[R]);
        IF BX[R] = 0.0 then BX[R] := 0.0000001;
        for C := 0 to FNV-1 do V[R,C] := B[R,C] / BX[R];
        end;

      // FINAL DA ROTACAO - INICIA ESTAGIO SEGUINTE DE COMPUTACOES
      IncrementarEstagio();
      FBEST := FO;
    end;

    function TodosParametrosSaoVale(): Boolean;
    var K: Integer;
    begin
      Result := True;
      for K := 0 to FPars.Count-1 do
        if FPars[K].OF_Status <> 'V' then
           begin
           Result := False;
           Break;
           end;
    end;

    procedure Calcular_Passo_Evitando_Inviabilidade();
    var xx: Real;
        K : Integer;
        Par: TParameter;
    begin
      for K := 0 to FPars.Count-1 do
        begin
        Par := FPars[K];

        xx := FPars[Ciclo].Step * V[Ciclo, K];
        if (xx < 0.0) and (xx < (Par.Min - Par.Value)) then
           FPars[Ciclo].Step := (Par.Min - Par.Value) / V[Ciclo,K];

        xx := FPars[Ciclo].Step * V[Ciclo, K];
        if (xx > 0.0) and (xx > (Par.Max - Par.Value)) then
           FPars[Ciclo].Step := (Par.Max - Par.Value) / V[Ciclo,K];
        end;
    end;

Type
  TipoFalha = (tfIneficiencia, tfInviabilidade);

    // Penaliza Função Objetivo
    function Calcula_Fp(): Real;
    var BW, PW: Real;
        Par: TParameter;
    begin
      Par := FPars[J];

      BW := AL[J];
      if BW = 0 then BW := 10E-10; {Rochedo}
      if (Par.Value < (Par.Min+BW)) or ((Par.Max-BW) < Par.Value) then
         PW := (Par.Min + BW - Par.Value) / BW // PARAMETROS NA REGIAO LIMITROFE INFERIOR
      else
         PW := (Par.Value - Par.Max + BW) / BW; // PARAMETROS NA REGIAO LIMITROFE SUPERIOR

      BW := PW * PW; // Desta maneira reduzo a equação em uma multiplicação
      BW := 3.0 * PW - 4.0 * BW + 2.0 * BW * PW;

      // PENALIZA VALOR DA F-O QDO PARAMETROS SE ACHAM NA ZONA FRONTEIRICA
      Result := F + (H[J] - F) * BW;
    end;

    procedure IniciarNovoEstagio();
    var K: Integer;
    begin
      for K := 0 to FPars.Count-1 do
        begin
        // REINICIALIZA PASSO INICIAL
        if FPassoInicial then FPars[K].Step := EINT[K];

        // INICIALIZA D[k] : ALTERACAO TOTAL DA VARIAVEL NO ESTAGIO CORRENTE
        D[K]                :=  0.0;
        FPars[K].OF_Status  :=  ' ';
        PriVez[K]           :=  True;
        end;
    end;

    Procedure IniciarCicloDeParametros();
    var K: Integer;
        Par: TParameter;
    begin
      Ciclo := 0; // INICIO DE CICLO DE VARIACAO DE PARAMETROS
      FPars.ActiveItem := 0;

      for K := 0 to FPars.Count-1 do
        begin
        Par := FPars[K];
        MARCA[K] := 1;

        // EVITA INSENSIBILIDADE DO PARAMETRO NAO ACEITANDO PASSO NULO
        if Par.Step = 0.0 then Par.Step := FTL_ABS[K];

        // SE VALOR INICIAL É MÁXIMO FAZ PASSO NEGATIVO
        if Abs(Par.Value - Par.Max) < 10E-15 then
           Par.Step := - Par.Step;
        end;
    end;

    Function ParametrosAtingiramZonaDePrecisao(): Boolean;
    var K: Integer;
    begin
      Result := True;
      for K := 0 to FPars.Count-1 do
        if ABS(FPars[K].Step) >= FTL_ABS[K] then
           Begin
           Result := False; // Continua
           Break;
           end;
    end;

    procedure IncremantaParametros();
    var K: Integer;
    begin
      // VERIFICA SE CONDICAO DE VALE FOI OBTIDA APROXIMADAMENTE
      MARCA[Ciclo] := 2;

      for K := 0 to FPars.Count-1 do
        begin
        // ALTERACAO UNIVARIACIONAL DOS PARAMETROS
        FPars[K].Value := FPars[K].Value + FPars[Ciclo].Step * V[Ciclo,K];

        // GUARDA MELHOR VALOR DA F-O  NO ESTAGIO ATUAL
        H[K] := FO;
        end;
    end;

    procedure IncrementarCiclo();
    begin
      inc(Ciclo);
      FPars.ActiveItem := Ciclo;
    end;

    function FimDoCiclo(): Boolean;
    begin
      if TodosParametrosSaoVale() then
         // VALE ENCONTRADO : FINAL DAS COMPUTACOES DESTE ESTAGIO
         // VERIFICA INICIALMENTE SE OTIMO FOI ATINGIDO
         if ABS(FBEST - FO) < FTol_FO_ABS then
            Result := True
         else
            begin
            RotacionaEixo();
            IniciarNovoEstagio();
            IniciarCicloDeParametros();
            Result := ParametrosAtingiramZonaDePrecisao();
            IncremantaParametros();
            end
      else // VERIFICA SE TODOS OS PARÂMETROS FORAM MODIFICADOS
         if Ciclo = FPars.Count-1 then
            begin
            IniciarCicloDeParametros();
            Result := ParametrosAtingiramZonaDePrecisao();
            IncremantaParametros();
            end
         else
            begin
            IncrementarCiclo();
            IncremantaParametros();
            Result := False;
            end
    end; //FimDoCiclo

    function TerminaPorFalha(Tipo: TipoFalha): Boolean;
    var K : Integer;
    begin
      if UmaSimulacao then
         begin
         for K := 0 to FPars.Count-1 do
           FPars[K].Value := FPars[K].Value - FPars[Ciclo].Step * V[Ciclo,K];

         // SOLUCAO PIOROU : MUDA SENTIDO DA VARIACAO E CONTRAI O PASSO PELA METADE
         FPars[Ciclo].Step := - FDecPasso * FPars[Ciclo].Step;

         Calcular_Passo_Evitando_Inviabilidade;
         if Tipo = tfIneficiencia then
            if FPars[Ciclo].OF_Status = 'M' then FPars[Ciclo].OF_Status := 'V';

         Result := FimDoCiclo;
         end
      else
         Result := True;
    end;

    function CanTerminate(): Boolean;
    begin
      Result := (FStop or
                (FSimulationCount > FMS-2) or
                ((GetTickCount - TM) > FMTS));
    end;

var
  Par: TParameter;

begin // Inicio da Rosenbrock.Execute
  Inicializacoes();
  IniciarNovoEstagio();
  IniciarCicloDeParametros();

  if Not ParametrosAtingiramZonaDePrecisao() then
     begin
     IncremantaParametros();
     Repeat
       if ParametrosViaveis() then
          begin
          UmaSimulacao := true;
          UpdateSimulationCount();

          FProject.o_CalculateObjetiveFunctions();
          F := self.FOFValues.AsDouble[0];
          Self.FO := F;

          UpdateOFValue(0, F);
          ProcessMessages(0);

          F_Igual_FO := (Abs(F - FO) < FTol_FO_ABS);
          if (F > FO) or (F_Igual_FO and not PriVez[Ciclo]) then
             if TerminaPorFalha(tfIneficiencia) then Break else Continue
          else
             if F_Igual_FO then PriVez[Ciclo] := False;

          // Verifica Zona Fronteirica e Melhoramento na FO
          J := 0;
          Melhorou := True;
          repeat
            Par := FPars[J];
            if (Par.Value < (Par.Min + AL[J])) or
               (Par.Value > (Par.Max - AL[J])) then
               begin
               F := Calcula_Fp();
               if F >= FO then
                  begin
                  Melhorou := False; {PIOROU !!}
                  Break;
                  end;
               end
            else
               H[J] :=  F; // Armazena novo valor melhor que anterior

            inc(J);
          until J = FPars.Count;

          if Melhorou then
             begin
             Par := FPars[Ciclo];

             D[Ciclo] := D[Ciclo] + Par.Step;
             Par.Step := FIncPasso * Par.Step;

             Calcular_Passo_Evitando_Inviabilidade();

             // MELHOR RESULTADO É REGISTRADO EM FO, SENDO FO GUARDADO ANTES
             // PARA TESTAR SE VALE FOI ENCONTRADO NA APROXIMACAO
             FO := F;
             if Par.OF_Status =  ' ' then Par.OF_Status := 'M';
             if FimDoCiclo() then Break;
             end
          else
             if TerminaPorFalha(tfIneficiencia) then Break;
          end
       else
          if TerminaPorFalha(tfInviabilidade) then Break;

       until CanTerminate();
     end; // if Not ParametrosAtingiramZonaDePrecisao ...
end;

procedure TRosenbrock.SetDecPasso(const Value: Real);
begin
  if (Value < 0) or (Value > 1) then
     FDecPasso := 0.5
  else
     FDecPasso := Value;
end;

procedure TRosenbrock.SetIncPasso(const Value: Real);
begin
  if (Value < 0) then
     FIncPasso := 3.0
  else
     FIncPasso := Value;
end;

end.
