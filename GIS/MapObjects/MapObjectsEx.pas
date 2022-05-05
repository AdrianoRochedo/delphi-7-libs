unit MapObjectsEx;

interface
uses Windows,
     Classes,
     Types,
     Contnrs,
     MapObjects;

type
  Tmoec_Layer = class
  private
    FMaxScale: Integer;
    FMinScale: Integer;
    FUseScaleForVisibility: Boolean;
    FmoLayer: IDispatch;
    FMap: TMap;
    function getAsVar: Variant;
    function getAsImageLayer: ImoImageLayer;
    function getAsMapLayer: ImoMapLayer;
    function getOrder: Integer;
  public
    constructor Create(Map: TMap; Layer: IDispatch);

    function IsMapLayer: Boolean;
    procedure Refresh;

    property moLayer : IDispatch read FmoLayer;
    property Map : TMap read FMap;

    property AsVariant   : Variant read getAsVar;
    property AsMapLayer  : ImoMapLayer read getAsMapLayer;
    property AsImageLayer: ImoImageLayer read getAsImageLayer;

    property Order : Integer read getOrder;

    property UseScaleForVisibility : Boolean read FUseScaleForVisibility write FUseScaleForVisibility;
    property MinScale : Integer read FMinScale write FMinScale;
    property MaxScale : Integer read FMaxScale write FMaxScale;
  end;

  // Utilize esta classe para controle das camadas.
  Tmoec_Layers = class
  private
    FMap: TMap;
    FList: TObjectList;
    function getLayer(i: Integer): Tmoec_Layer;
    function getCount: Integer;
    function InternalAdd(const layer: IDispatch): Tmoec_Layer;
  public
    constructor Create(const Map: TMap);
    destructor Destroy; override;

    function Add(const moLayer: IDispatch; out Layer: Tmoec_Layer): WordBool;
    function ByName(const Name: String): IDispatch;

    procedure Remove(i: Integer);
    procedure MoveTo(fromIndex, toIndex: Integer);

    property Layer[i: Integer]: Tmoec_Layer read getLayer; default;
    property Count: Integer read getCount;
  end;

  TMapEx = class(TMap)
  private
    FLayers: Tmoec_Layers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Métodos de conversão TPoint <--> ImoPoint
    function moPointToPoint(p: ImoPoint): TPoint;
    function PointTo_moPoint(p: TPoint): ImoPoint;

    // Formata o ponto para uma string
    function FormatPoint(p: ImoPoint): String;

    // copia uma camada e seus arquivos associados para um diretório
    procedure CopyLayer(const FileName, ToDir: String);

    property Layers : Tmoec_Layers read FLayers;
  end;

  procedure Register;

implementation
uses SysUtils;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TMapEx]);
end;

{ Tmoec_Layers }

constructor Tmoec_Layers.Create(const Map: TMap);
begin
  inherited Create;
  FMap := Map;
  FList := TObjectList.Create(True);
end;

destructor Tmoec_Layers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function Tmoec_Layers.InternalAdd(const Layer: IDispatch): Tmoec_Layer;
begin
  Result := Tmoec_Layer.Create(FMap, Layer);
  FList.Insert(0, Result);
end;

function Tmoec_Layers.Add(const moLayer: IDispatch; out Layer: Tmoec_Layer): WordBool;
begin
  Result := FMap.Layers.Add(moLayer);
  if Result then
     Layer := InternalAdd(moLayer)
  else
     Layer := nil;
end;

procedure Tmoec_Layers.MoveTo(fromIndex, toIndex: Integer);
begin
  FList.Move(fromIndex, toIndex);
  FMap.Layers.MoveTo(fromIndex, toIndex);
end;

procedure Tmoec_Layers.Remove(i: Integer);
begin
  FList.Delete(i);
  FMap.Layers.Remove(i);
end;

function Tmoec_Layers.getLayer(i: Integer): Tmoec_Layer;
begin
  Result := Tmoec_Layer(FList[i]);
end;

function Tmoec_Layers.getCount: Integer;
begin
  Result := FMap.Layers.Count;
end;

function Tmoec_Layers.ByName(const Name: String): IDispatch;
var i: Integer;
    v: Variant;
begin
  Result := nil;
  for i := 0 to getCount-1 do
    begin
    v := getLayer(i).moLayer;
    if CompareText(Name, v.Name) = 0 then
       begin
       Result := v;
       Break;
       end;
    end;
end;

{ TMapEx }

{DONE 5 : Tratar camadas raster}
procedure TMapEx.CopyLayer(const FileName, ToDir: String);
var s, sPath: String;
begin
  if ToDir <> '' then
     begin
     sPath := ExtractFilePath(FileName);
     s := ChangeFileExt(ExtractFileName(FileName), '');
     CopyFile(pChar(sPath + s + '.dbf'), pChar(ToDir + s + '.dbf'), false);
     CopyFile(pChar(sPath + s + '.prj'), pChar(ToDir + s + '.prj'), false);
     CopyFile(pChar(sPath + s + '.shp'), pChar(ToDir + s + '.shp'), false);
     CopyFile(pChar(sPath + s + '.shx'), pChar(ToDir + s + '.shx'), false);
     CopyFile(pChar(sPath + s + '.sbx'), pChar(ToDir + s + '.sbx'), false);
     CopyFile(pChar(sPath + s + '.sbn'), pChar(ToDir + s + '.sbn'), false);
     CopyFile(pChar(sPath + s + '.avl'), pChar(ToDir + s + '.avl'), false);
     end;
end;

constructor TMapEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayers := Tmoec_Layers.Create(Self);
end;

destructor TMapEx.Destroy;
begin
  FLayers.Free;
  inherited;
end;

function TMapEx.FormatPoint(p: ImoPoint): String;
begin

end;

function TMapEx.moPointToPoint(p: ImoPoint): TPoint;
var x, y: Single;
begin
  if p <> nil then
     begin
     FromMapPoint(p, x, y);
     Result.X := Trunc(x);
     Result.Y := Trunc(y);
     end
  else
     Result := Types.Point(0, 0);
end;

function TMapEx.PointTo_moPoint(p: TPoint): ImoPoint;
begin
  Result := ToMapPoint(p.x, p.y);
end;

{ Tmoec_Layer }

constructor Tmoec_Layer.Create(Map: TMap; Layer: IDispatch);
begin
  inherited Create;
  FMap := Map;
  FmoLayer := Layer;
end;

function Tmoec_Layer.getAsImageLayer: ImoImageLayer;
begin
  Result := FmoLayer as ImoImageLayer;
end;

function Tmoec_Layer.getAsMapLayer: ImoMapLayer;
begin
  Result := FmoLayer as ImoMapLayer;
end;

function Tmoec_Layer.getAsVar: Variant;
begin
  Result := FmoLayer;
end;

function Tmoec_Layer.getOrder: Integer;
begin
  for Result := 0 to FMap.Layers.Count-1 do
    if FMap.Layers.Item(Result) = FmoLayer then Exit;
  Result := -1;  
end;

function Tmoec_Layer.IsMapLayer: Boolean;
begin
  Result := (AsVariant.LayerType = moMapLayer);
end;

procedure Tmoec_Layer.Refresh;
begin
  self.Map.RefreshLayer(self.Order);
end;

end.
