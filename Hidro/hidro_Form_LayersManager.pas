unit hidro_Form_LayersManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MessageManager, StdCtrls, ExtCtrls, Frame_PromotionList, Menus,
  MapObjects,
  MapObjectsEx, drEdit;

var
  UM_LM_UPDATE_LAYER: Integer;

type
  THidroForm_LayersManager = class(TForm, IMessageReceptor)
    frPL: TfrPL;
    gbProps: TGroupBox;
    laCor: TLabel;
    Label2: TLabel;
    cbVis: TCheckBox;
    paCor: TPanel;
    ColorDialog: TColorDialog;
    Menu: TPopupMenu;
    Menu_Remover: TMenuItem;
    cbVC: TCheckBox;
    Label1: TLabel;
    edEMin: TdrEdit;
    Label3: TLabel;
    edEMax: TdrEdit;
    procedure FormShow(Sender: TObject);
    procedure Camadas_Click(Sender: TObject);
    procedure cbVisClick(Sender: TObject);
    procedure paCorClick(Sender: TObject);
    procedure Menu_RemoverClick(Sender: TObject);
    procedure cbVCClick(Sender: TObject);
    procedure edEMinExit(Sender: TObject);
    procedure edEMaxExit(Sender: TObject);
    procedure frPLbtnUPClick(Sender: TObject);
  private
    FMap: TMapEx;
    FSettingData: Boolean;

    // Receptor de mensagens enviadas pelo sistema
    function ReceiveMessage(Const MSG: TadvMessage): Boolean;

    procedure ClearFields;
    procedure SelectFirstLayer;
    procedure SetMap(Map: TMapEx; p: Pointer);
    procedure Camadas_ItemMove(Sender: TObject; FromIndex, ToIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses WinUtils,
     pr_Vars;

{$R *.dfm}

{ THidroForm_LayersManager }

constructor THidroForm_LayersManager.Create;
begin
  inherited Create(nil);
  getMessageManager.RegisterMessage(UM_LM_UPDATE_LAYER, Self);
end;

destructor THidroForm_LayersManager.Destroy;
begin
  getMessageManager.UnregisterMessage(UM_LM_UPDATE_LAYER, Self);
  inherited;
end;

function THidroForm_LayersManager.ReceiveMessage(const MSG: TadvMessage): Boolean;
begin
  if MSG.ID = UM_LM_UPDATE_LAYER then
     SetMap(TMapEx(MSG.ParamAsObject(0)), MSG.ParamAsPointer(1));
end;

procedure THidroForm_LayersManager.SelectFirstLayer;
begin
  if frPL.lbList.Items.Count > 0 then
     begin
     gbProps.Enabled := True;
     frPL.lbList.ItemIndex := 0;
     Camadas_Click(frPL.lbList);
     end
  else
     ClearFields();
end;

procedure THidroForm_LayersManager.ClearFields;
begin
  frPL.lbList.Clear;
  cbVis.Checked := False;
  gbProps.Enabled := False;
  paCor.Color := clSilver;
  laCor.Enabled := False;
end;

procedure THidroForm_LayersManager.SetMap(Map: TMapEx; p: Pointer);
var i: Integer;
    L: Variant;
begin
  ClearFields();
  FMap := Map;

  if (Map = FMap) and (p = nil) then
     // nada
  else
     begin
     for i := 0 to FMap.Layers.Count-1 do
       begin
       L := FMap.Layers[i].moLayer;
       frPL.lbList.Items.Add(L.Name);
       end;
     SelectFirstLayer();
     end;
end;

procedure THidroForm_LayersManager.FormShow(Sender: TObject);
begin
  frPL.OnItemMove := Camadas_ItemMove;
end;

procedure THidroForm_LayersManager.Camadas_Click(Sender: TObject);
var L  : Variant;
    LEI: Tmoec_Layer;
    b  : Boolean;
begin
  FSettingData := True;

  LEI := FMap.Layers[frPL.lbList.ItemIndex];
  L := LEI.moLayer;

  cbVis.Checked := L.Visible;
  cbVC.Checked := LEI.UseScaleForVisibility;

  b := (L.LayerType = moMapLayer);
  setEnable([laCor, paCor], b);

  if b then
     begin
     paCor.Color := L.Symbol.Color;
     end
  else
     begin
     paCor.Color := clSilver;
     end;

  edEMin.AsInteger := LEI.MinScale;
  edEMax.AsInteger := LEI.MaxScale;   

  FSettingData := False;
end;

procedure THidroForm_LayersManager.cbVisClick(Sender: TObject);
var L: Variant;
begin
  if (frPL.lbList.ItemIndex >= 0) and not FSettingData then
     begin
     L := FMap.Layers[frPL.lbList.ItemIndex].moLayer;
     L.Visible := cbVis.Checked;
     FMap.Refresh;
     end;
end;

procedure THidroForm_LayersManager.paCorClick(Sender: TObject);
var L: Variant;
begin
  if (frPL.lbList.ItemIndex >= 0) and not FSettingData and ColorDialog.Execute then
     begin
     paCor.Color := ColorDialog.Color;
     L := FMap.Layers[frPL.lbList.ItemIndex].moLayer;
     L.Symbol.Color := paCor.Color;
     FMap.RefreshLayer(frPL.lbList.ItemIndex);
     end;
end;

procedure THidroForm_LayersManager.Camadas_ItemMove(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  FMap.Layers.MoveTo(FromIndex, ToIndex);
  FMap.Refresh;
end;

procedure THidroForm_LayersManager.Menu_RemoverClick(Sender: TObject);
begin
  FMap.Layers.Remove(frPL.lbList.ItemIndex);
  frPL.lbList.Items.Delete(frPL.lbList.ItemIndex);
  SelectFirstLayer;
end;

procedure THidroForm_LayersManager.cbVCClick(Sender: TObject);
var LEI: Tmoec_Layer;
begin
  if (frPL.lbList.ItemIndex >= 0) and not FSettingData then
     begin
     LEI := FMap.Layers[frPL.lbList.ItemIndex];
     LEI.UseScaleForVisibility := cbVC.Checked;
     FMap.Refresh;
     end;
end;

procedure THidroForm_LayersManager.edEMinExit(Sender: TObject);
var LEI: Tmoec_Layer;
begin
  if (frPL.lbList.ItemIndex >= 0) and not FSettingData then
     begin
     LEI := FMap.Layers[frPL.lbList.ItemIndex];
     LEI.MinScale := edEMin.AsInteger;
     FMap.Refresh;
     end;
end;

procedure THidroForm_LayersManager.edEMaxExit(Sender: TObject);
var LEI: Tmoec_Layer;
begin
  if (frPL.lbList.ItemIndex >= 0) and not FSettingData then
     begin
     LEI := FMap.Layers[frPL.lbList.ItemIndex];
     LEI.MaxScale := edEMax.AsInteger;
     FMap.Refresh;
     end;
end;

procedure THidroForm_LayersManager.frPLbtnUPClick(Sender: TObject);
begin
  frPL.btnUPClick(Sender);
end;

initialization
  UM_LM_UPDATE_LAYER := GetMessageManager.RegisterMessageID('UM_LM_UPDATE_LAYER');

end.
