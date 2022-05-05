unit drDataBinding;

interface
uses DB;

type
  TRecordBinding = class
  private
    FDataset: TDataset;
  protected
    // Respons�vel fazer as conec��es otimizadas nos campos.
    procedure Binding; virtual;
  public
    constructor Create(Dataset: TDataset);
    property Dataset: TDataset read FDataset;
  end;

  TDatasetBinding = class
  private
    FDataset: TDataset;
  protected
    FRecordBinding: TRecordBinding;

    // Respons�vel por criar os espec�ficos DataSet e RecordBinding
    procedure Init; virtual; abstract;

    // Disparado antes do Post
    procedure BeforePost(DataSet: TDataSet); virtual;

    // Realiza a valida��o. Como regra geral dever� ser disparado um Exce��o em caso de erro
    procedure DoValidate; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    // Navega��o
    procedure First;
    function Next: Boolean;

    // Edi��o
    procedure Edit;
    procedure Insert;
    procedure Post;

    property Dataset: TDataset read FDataset write FDataset;
    property RecordBinding : TRecordBinding read FRecordBinding write FRecordBinding;
  end;

implementation

{ TDatasetBinding }

constructor TDatasetBinding.Create;
begin
  inherited;
  Init;
  FDataSet.BeforePost := BeforePost;
end;

destructor TDatasetBinding.Destroy;
begin
  if FDataset <> nil then FDataset.Free;
  if FRecordBinding <> nil then FRecordBinding.Free;
  inherited;
end;

procedure TDatasetBinding.BeforePost(DataSet: TDataSet);
begin
  try
    DoValidate;
  except
    FDataset.Cancel;
    raise;
  end;
end;

procedure TDatasetBinding.DoValidate;
begin
  // Nada nesta classe
end;

procedure TDatasetBinding.Close;
begin
  FDataset.Close;
end;

procedure TDatasetBinding.Open;
begin
  FDataset.Open;
  FRecordBinding.Binding;
end;

procedure TDatasetBinding.First;
begin
  FDataset.First;
end;

function TDatasetBinding.Next: Boolean;
begin
  Result := FDataset.FindNext;
end;

procedure TDatasetBinding.Edit;
begin
  FDataset.Edit;
end;

procedure TDatasetBinding.Post;
begin
  FDataset.Post;
end;

procedure TDatasetBinding.Insert;
begin
  FDataset.Insert;
end;

{ TRecordBinding }

constructor TRecordBinding.Create(Dataset: TDataset);
begin
  inherited Create;
  FDataset := Dataset;
end;

procedure TRecordBinding.Binding;
begin
  // Nada nesta classe;
end;

end.
