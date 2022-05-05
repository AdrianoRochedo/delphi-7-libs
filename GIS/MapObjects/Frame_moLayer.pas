unit Frame_moLayer;

interface

uses
  Windows, Messages, SysUtils, Variants, Forms,
  Dialogs, StdCtrls, ExtCtrls, Classes, Graphics, Controls,
  MapObjectsEx, Menus;

type
  TFrame_moLayer = class(TFrame)
    cbVis: TCheckBox;
    Panel1: TPanel;
    Box: TPaintBox;
    ColorDialog: TColorDialog;
    Menu: TPopupMenu;
    Menu_MostrarRegistros: TMenuItem;
    procedure BoxPaint(Sender: TObject);
    procedure cbVisClick(Sender: TObject);
    procedure BoxDblClick(Sender: TObject);
    procedure FrameDblClick(Sender: TObject);
    procedure Menu_MostrarRegistrosClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
  private
    FSettingData: Boolean;
    FRecordSetFormStyle: TFormStyle;
    function getLayer: Tmoec_Layer;
  public
    constructor Create(AOwner: TComponent; Layer: Tmoec_Layer);

    property Layer : Tmoec_Layer read getLayer;
    property RecordSetFormStyle : TFormStyle read FRecordSetFormStyle write FRecordSetFormStyle;
  end;

implementation
uses MapObjects,
     Form_moLayerProps,
     Form_moRecordSet;

{$R *.dfm}

procedure TFrame_moLayer.BoxPaint(Sender: TObject);
var dx, dy: Integer;
    bmp: TBitmap;
begin
  if Layer.IsMapLayer then
     case Layer.AsVariant.ShapeType of
       moShapeTypePoint:
         begin
         dx := Box.Width div 2;
         dy := Box.Height div 2;
         Box.Canvas.Brush.Color := Layer.AsMapLayer.Symbol.Color;
         Box.Canvas.Ellipse(dx-2, dy-2, dx+2, dy+2);
         end;

       moShapeTypeLine:
         begin
         dx := Box.Width div 3;
         Box.Canvas.Pen.Color := Layer.AsMapLayer.Symbol.Color;
         Box.Canvas.MoveTo(0, 0);
         Box.Canvas.LineTo(dx, Box.Height);
         Box.Canvas.LineTo(dx * 2, 0);
         Box.Canvas.LineTo(Box.Width, Box.Height);
         end;

       moShapeTypePolygon:
         begin
         Box.Canvas.Brush.Color := Layer.AsMapLayer.Symbol.Color;
         Box.Canvas.FillRect(Box.ClientRect);
         end;

       else
         begin
         // Desenha um "X"
         Box.Canvas.Pen.Color := clRED;
         Box.Canvas.MoveTo(0, 0);
         Box.Canvas.LineTo(Box.Width, Box.Height);
         Box.Canvas.MoveTo(Box.Width, 0);
         Box.Canvas.LineTo(0, Box.Height);
         end;

       end // case
  else
     begin
     bmp := TBitmap.Create;
     bmp.Handle := LoadBitmap(hInstance, 'IMAGE_LAYER');
     Box.Canvas.Draw(0, 0, bmp);
     bmp.Free;
     end;
end;

constructor TFrame_moLayer.Create(AOwner: TComponent; Layer: Tmoec_Layer);
begin
  inherited Create(AOwner);
  Tag := Integer(Layer);
  FSettingData := True;
  cbVis.Checked := Layer.AsVariant.Visible;
  FSettingData := False;
end;

function TFrame_moLayer.getLayer: Tmoec_Layer;
begin
  Result := Tmoec_Layer(Tag);
end;

procedure TFrame_moLayer.cbVisClick(Sender: TObject);
var L: Variant;
begin
  if not FSettingData then
     begin
     Layer.AsVariant.Visible := cbVis.Checked;
     Layer.Map.Refresh;
     end;
end;

procedure TFrame_moLayer.BoxDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then
     begin
     Layer.AsVariant.Symbol.Color := ColorDialog.Color;
     Layer.Map.RefreshLayer(Layer.Order);
     Box.Invalidate();
     end;
end;

procedure TFrame_moLayer.FrameDblClick(Sender: TObject);
begin
  TForm_moLayerProps.ShowProps(Layer);
end;

procedure TFrame_moLayer.MenuPopup(Sender: TObject);
begin
  Menu_MostrarRegistros.Enabled := Layer.IsMapLayer;
end;

procedure TFrame_moLayer.Menu_MostrarRegistrosClick(Sender: TObject);
var L : ImoMapLayer;
begin
  L := Layer.AsMapLayer;
  TForm_moRecordSet.ShowRecordSet(L.Name, L.Records, FRecordSetFormStyle);
end;

end.
