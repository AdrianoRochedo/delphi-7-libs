unit Form_moCoordinateSystem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MapObjects, StdCtrls;

type
  TForm_moCoordinateSystem = class(TForm)
    rbGeo: TRadioButton;
    rbProj: TRadioButton;
    cbGeo: TComboBox;
    cbProj: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Radio_Click(Sender: TObject);
  private
    FCoordinateSystem: Integer;
  public
    constructor Create;

    // Obtém uma instância do Sistema de Coordenadas,
    // seja ele Geográfico ou Projetado.
    // CS deverá ser atribuído a prop. "CoordinateSystem" de TMap
    class function GetCoordinateSystem(out CS: Variant): Boolean; overload;

    // Obtém a identificação do Sistema de Coordenadas
    //   cType --> 'G' - Sist. Gegráfico    'P' - Sist. Projetado
    //   cName --> Cosntante que identifica o sistema. Ex: moGeoCS_SAD1969
    //   cCod  --> Código da constante para ser utilizado na Prop. "Type" das
    //             classes que implementam os sistemas de coordenadas
    class function GetCoordinateSystem(out cType: Char; out sName, sCod: String): Boolean; overload;
  end;

implementation
uses SysUtilsEx, OleCtrls;

{$R *.dfm}

{ TForm_moCoordinateSystem }

class function TForm_moCoordinateSystem.GetCoordinateSystem(out CS: Variant): Boolean;
var d: TForm_moCoordinateSystem;
    G: IMoGeoCoordSys;
    P: IMoProjCoordSys;
begin
  d := TForm_moCoordinateSystem.Create;
  if d.ShowModal = mrOk then
     begin
     if d.rbGeo.Checked then
        begin
        G := coGeoCoordSys.Create;
        G.type_ := TOleEnum(d.FCoordinateSystem);
        CS := G;
        end
     else
        begin
        P := coProjCoordSys.Create;
        P.type_ := TOleEnum(d.FCoordinateSystem);
        CS := P;
        end;

     Result := true;
     end
  else
     begin
     CS := Variants.NULL;
     Result := false;
     end;

  d.Free;
end;

class function TForm_moCoordinateSystem.GetCoordinateSystem(out cType: Char;
                                            out sName, sCod: String): Boolean;
var d: TForm_moCoordinateSystem;
    s: String;
begin
  d := TForm_moCoordinateSystem.Create;
  if d.ShowModal = mrOk then
     begin
     if d.rbGeo.Checked then
        begin
        s := d.cbGeo.Items[d.cbGeo.ItemIndex];
        cType := 'G';
        end
     else
        begin
        s := d.cbProj.Items[d.cbProj.ItemIndex];
        cType := 'P';
        end;

     sName := LeftStringOf(s, '[', true);
     sCod  := SubString(s, '[', ']');
     Result := true;
     end
  else
     begin
     Result := false;
     end;

  d.Free;
end;

constructor TForm_moCoordinateSystem.Create;
var i: Integer;
    moSL: IMoStrings;
begin
  inherited Create(nil);
  moSL := coStrings.Create;

  moSL.PopulateWithGeographicCoordSys();
  for i := 0 to moSL.Count-1 do
    cbGeo.Items.Add(moSL.Item(i));

  moSL.Clear;
  moSL.PopulateWithProjectedCoordSys();
  for i := 0 to moSL.Count-1 do
    cbProj.Items.Add(moSL.Item(i));
end;

procedure TForm_moCoordinateSystem.Button1Click(Sender: TObject);
begin
  if rbGeo.Checked then
     FCoordinateSystem := StrToIntDef(SubString(
       cbGeo.Items[cbGeo.ItemIndex], '[', ']'), 0)
  else
     FCoordinateSystem := StrToIntDef(SubString(
       cbProj.Items[cbProj.ItemIndex], '[', ']'), 0);

  if FCoordinateSystem = 0 then
     ShowMessage('Selecione um Sistema de Coordenadas')
  else
     ModalResult := mrOK;
end;

procedure TForm_moCoordinateSystem.Radio_Click(Sender: TObject);
begin
  cbGeo.Enabled := rbGeo.Checked;
  cbProj.Enabled := rbProj.Checked;
end;

end.
