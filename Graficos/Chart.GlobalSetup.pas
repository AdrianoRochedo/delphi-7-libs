unit Chart.GlobalSetup;

interface
uses Classes,
     Graphics,
     GraphicUtils,
     Chart,
     Skin,
     MSXML4,
     XML_Interfaces,
     XML_Utils;

type
  TGlobalSetup = class(TSkin, IToXML, IFromXML)
  private
    // Propriedades gerenciadas

    Chart_Legend_Visible    : boolean;
    Chart_Legend_Font       : TXML_Font;          // Object
    Chart_Legend_Alignment  : TLegendAlignment;

    Chart_Title_Visible     : boolean;
    Chart_Title_Color       : TColor;
    Chart_Title_Font        : TXML_Font;          // Object
    Chart_Title_Text        : TXML_StringList;    // Object
    Chart_Title_Alignment   : TAlignment;
    Chart_Foot_Visible      : boolean;
    Chart_Foot_Color        : TColor;
    Chart_Foot_Font         : TXML_Font;          // Object
    Chart_Foot_Text         : TXML_StringList;    // Object
    Chart_Foot_Alignment    : TAlignment;

    Chart_BackColor         : TColor;
    Chart_Color             : TColor;
    Chart_AxisVisible       : boolean;
    Chart_Chart3DPercent    : integer;
    Chart_Monochrome        : boolean;
    Chart_View3D            : boolean;
    Chart_View3DWalls       : boolean;
    Chart_MarginLeft        : integer;
    Chart_MarginRight       : integer;
    Chart_MarginTop         : integer;
    Chart_MarginBottom      : integer;

    FSetTitleText           : boolean;

    procedure ToXML(Buffer: TStrings; Ident: Integer);
    function GetClassName(): String;
    procedure FromXMLNodeList(NodeList: IXMLDOMNodeList);
    function GetObjectReference(): TObject;
    function ToString(): string;
  protected
    function createDialog(): TDialog; override;
    procedure getDataFromDialog(); override;
    procedure initDialog(); override;
  public
    constructor Create(const Text: string);
    destructor Destroy(); override;

    function Execute(): integer; override;
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);

    // Configura um grafico com as props. armazenadas nesta instancia
    procedure SetupChart(Chart: TChart);
  end;

implementation
uses SysUtilsEx,
     Chart.GlobalSetupForm;

{ TGlobalSetup }

constructor TGlobalSetup.Create(const Text: string);
begin
  inherited Create(Text);

  FSetTitleText := true;

  Chart_Legend_Visible    := true;
  Chart_Legend_Alignment  := laRight;

  Chart_Title_Visible     := true;
  Chart_Title_Color       := clYellow;
  Chart_Title_Alignment   := taCenter;
  Chart_Foot_Visible      := true;
  Chart_Foot_Color        := clYellow;
  Chart_Foot_Alignment    := taCenter;

  Chart_BackColor         := $00C7E6F3;
  Chart_Color             := $00ECEACC;
  Chart_AxisVisible       := true;
  Chart_Chart3DPercent    := 15;
  Chart_Monochrome        := false;
  Chart_View3D            := false;
  Chart_View3DWalls       := true;
  Chart_MarginLeft        := 6;
  Chart_MarginRight       := 6;
  Chart_MarginTop         := 6;
  Chart_MarginBottom      := 6;

  Chart_Legend_Font := TXML_Font.Create();
  Chart_Title_Font  := TXML_Font.Create();
  Chart_Title_Text  := TXML_StringList.Create();
  Chart_Foot_Font   := TXML_Font.Create();
  Chart_Foot_Text   := TXML_StringList.Create();
end;

destructor TGlobalSetup.Destroy();
begin
  Chart_Legend_Font.Free();
  Chart_Title_Font.Free();
  Chart_Title_Text.Free();
  Chart_Foot_Font.Free();
  Chart_Foot_Text.Free();

  inherited Destroy();
end;

function TGlobalSetup.createDialog(): TDialog;
begin
  result := TfoGlobalSetup.Create(nil);
end;

function TGlobalSetup.Execute(): integer;
begin
  inherited Execute();
  result := ShowDialog();
end;

procedure TGlobalSetup.FromXMLNodeList(NodeList: IXMLDOMNodeList);
var no: IXMLDomNode;
    n, s : string;
begin
  no := NodeList.nextNode();
  while (no <> nil) do
    begin
    n := no.nodeName;
    s := no.Text;

    // Legenda

    if n = 'Chart_Legend_Visible'   then Chart_Legend_Visible   := toBoolean(s) else
    if n = 'Chart_Legend_Font'      then Chart_Legend_Font.FromXML(no) else
    if n = 'Chart_Legend_Alignment' then Chart_Legend_Alignment := TLegendAlignment( toInt(s) ) else

    // Titulos

    if n = 'SetTitleText'           then FSetTitleText          := toBoolean(s) else

    if n = 'Chart_Title_Visible'    then Chart_Title_Visible    := toBoolean(s) else
    if n = 'Chart_Title_Color'      then Chart_Title_Color      := toInt(s) else
    if n = 'Chart_Title_Alignment'  then Chart_Title_Alignment  := TAlignment( toInt(s) ) else
    if n = 'Chart_Foot_Visible'     then Chart_Foot_Visible     := toBoolean(s) else
    if n = 'Chart_Foot_Color'       then Chart_Foot_Color       := toInt(s) else
    if n = 'Chart_Foot_Alignment'   then Chart_Foot_Alignment   := TAlignment( toInt(s) ) else

    if n = 'Chart_Title_Font'       then Chart_Title_Font.FromXML(no) else
    if n = 'Chart_Title_Text'       then Chart_Title_Text.FromXML(no) else
    if n = 'Chart_Foot_Font'        then Chart_Foot_Font.FromXML(no)  else
    if n = 'Chart_Foot_Text'        then Chart_Foot_Text.FromXML(no)  else

    // Geral

    if n = 'Chart_BackColor'        then Chart_BackColor        := toInt(s) else
    if n = 'Chart_Color'            then Chart_Color            := toInt(s) else
    if n = 'Chart_AxisVisible'      then Chart_AxisVisible      := toBoolean(s) else
    if n = 'Chart_Chart3DPercent'   then Chart_Chart3DPercent   := toInt(s) else
    if n = 'Chart_Monochrome'       then Chart_Monochrome       := toBoolean(s) else
    if n = 'Chart_View3D'           then Chart_View3D           := toBoolean(s) else
    if n = 'Chart_View3DWalls'      then Chart_View3DWalls      := toBoolean(s) else
    if n = 'Chart_MarginLeft'       then Chart_MarginLeft       := toInt(s) else
    if n = 'Chart_MarginRight'      then Chart_MarginRight      := toInt(s) else
    if n = 'Chart_MarginTop'        then Chart_MarginTop        := toInt(s) else
    if n = 'Chart_MarginBottom'     then Chart_MarginBottom     := toInt(s);

    // go to the next node
    no := NodeList.nextNode();
    end;
end;

function TGlobalSetup.GetClassName(): String;
begin
  result := self.ClassName;
end;

procedure TGlobalSetup.getDataFromDialog();
begin
  inherited getDataFromDialog();

  with TfoGlobalSetup(Dialog) do
    begin
    // Legenda

    Chart_Legend_Visible      :=    Chart.Legend.Visible     ;
    Chart_Legend_Alignment    :=    Chart.Legend.Alignment   ;

    Chart_Legend_Font.Assign ( Chart.Legend.Font )           ;

    // Titulos

    FSetTitleText             :=    TfoGlobalSetup(Dialog).SetTitleText;

    Chart_Title_Visible       :=    Chart.Title.Visible      ;
    Chart_Title_Color         :=    Chart.Title.Color        ;
    Chart_Title_Alignment     :=    Chart.Title.Alignment    ;
    Chart_Foot_Visible        :=    Chart.Foot.Visible       ;
    Chart_Foot_Color          :=    Chart.Foot.Color         ;
    Chart_Foot_Alignment      :=    Chart.Foot.Alignment     ;

    Chart_Title_Font.Assign ( Chart.Title.Font )             ;
    Chart_Foot_Font.Assign  ( Chart.Foot.Font  )             ;

    Chart_Title_Text.Assign ( Chart.Title.Text )             ;
    Chart_Foot_Text.Assign  ( Chart.Foot.Text  )             ;

    // Geral

    Chart_BackColor           :=    Chart.BackColor          ;
    Chart_Color               :=    Chart.Color              ;
    Chart_AxisVisible         :=    Chart.AxisVisible        ;
    Chart_Chart3DPercent      :=    Chart.Chart3DPercent     ;
    Chart_Monochrome          :=    Chart.Monochrome         ;
    Chart_View3D              :=    Chart.View3D             ;
    Chart_View3DWalls         :=    Chart.View3DWalls        ;
    Chart_MarginLeft          :=    Chart.MarginLeft         ;
    Chart_MarginRight         :=    Chart.MarginRight        ;
    Chart_MarginTop           :=    Chart.MarginTop          ;
    Chart_MarginBottom        :=    Chart.MarginBottom       ;
    end;
end;

function TGlobalSetup.GetObjectReference(): TObject;
begin
  result := self;
end;

procedure TGlobalSetup.SetupChart(Chart: TChart);
begin
  // Legenda

  Chart.Legend.Visible     :=     Chart_Legend_Visible    ;
  Chart.Legend.Alignment   :=     Chart_Legend_Alignment  ;

  Chart.Legend.Font.Assign ( Chart_Legend_Font )          ;

  // Titulos

  Chart.Title.Visible      :=     Chart_Title_Visible     ;
  Chart.Title.Color        :=     Chart_Title_Color       ;
  Chart.Title.Alignment    :=     Chart_Title_Alignment   ;
  Chart.Foot.Visible       :=     Chart_Foot_Visible      ;
  Chart.Foot.Color         :=     Chart_Foot_Color        ;
  Chart.Foot.Alignment     :=     Chart_Foot_Alignment    ;

  Chart.Title.Font.Assign ( Chart_Title_Font )            ;
  Chart.Foot.Font.Assign  ( Chart_Foot_Font  )            ;

  if FSetTitleText then
     begin
     Chart.Title.Text.Assign ( Chart_Title_Text ) ;
     Chart.Foot.Text.Assign  ( Chart_Foot_Text  ) ;
     end;

  // Geral

  Chart.BackColor          :=     Chart_BackColor         ;
  Chart.Color              :=     Chart_Color             ;
  Chart.AxisVisible        :=     Chart_AxisVisible       ;
  Chart.Chart3DPercent     :=     Chart_Chart3DPercent    ;
  Chart.Monochrome         :=     Chart_Monochrome        ;
  Chart.View3D             :=     Chart_View3D            ;
  Chart.View3DWalls        :=     Chart_View3DWalls       ;
  Chart.MarginLeft         :=     Chart_MarginLeft        ;
  Chart.MarginRight        :=     Chart_MarginRight       ;
  Chart.MarginTop          :=     Chart_MarginTop         ;
  Chart.MarginBottom       :=     Chart_MarginBottom      ;
end;

procedure TGlobalSetup.initDialog();
begin
  inherited initDialog();
  TfoGlobalSetup(Dialog).SetTitleText := FSetTitleText;
  SetupChart( TfoGlobalSetup(Dialog).Chart);
end;

procedure TGlobalSetup.LoadFromFile(const Filename: string);
begin
  XML_Utils.LoadObjectFromXML(self, Filename);
end;

procedure TGlobalSetup.SaveToFile(const Filename: string);
begin
  XML_Utils.SaveObjectToXML(self, Filename);
end;

function TGlobalSetup.ToString(): string;
begin
  result := 'Chart.GlobalSetup';
end;

procedure TGlobalSetup.ToXML(Buffer: TStrings; Ident: Integer);
var x: TXML_Writer;
begin
  x := TXML_Writer.Create(Buffer, Ident);

  // Legenda

  x.Write('Chart_Legend_Visible'    , Chart_Legend_Visible    );
  x.Write('Chart_Legend_Alignment'  , ord(Chart_Legend_Alignment)  );

  Chart_Legend_Font.ToXML('Chart_Legend_Font', x.Buffer, x.IdentSize);

  // Titulos

  x.Write('SetTitleText' , FSetTitleText);

  x.Write('Chart_Title_Visible'     , Chart_Title_Visible       );
  x.Write('Chart_Title_Color'       , Chart_Title_Color         );
  x.Write('Chart_Title_Alignment'   , ord(Chart_Title_Alignment));
  x.Write('Chart_Foot_Visible'      , Chart_Foot_Visible        );
  x.Write('Chart_Foot_Color'        , Chart_Foot_Color          );
  x.Write('Chart_Foot_Alignment'    , ord(Chart_Foot_Alignment) );

  Chart_Title_Font.ToXML('Chart_Title_Font', x.Buffer, x.IdentSize);
  Chart_Foot_Font.ToXML('Chart_Foot_Font', x.Buffer, x.IdentSize);

  Chart_Title_Text.ToXML('Chart_Title_Text', x.Buffer, x.IdentSize);
  Chart_Foot_Text.ToXML('Chart_Foot_Text', x.Buffer, x.IdentSize);

  // Geral

  x.Write('Chart_BackColor'         , Chart_BackColor         );
  x.Write('Chart_Color'             , Chart_Color             );
  x.Write('Chart_AxisVisible'       , Chart_AxisVisible       );
  x.Write('Chart_Chart3DPercent'    , Chart_Chart3DPercent    );
  x.Write('Chart_Monochrome'        , Chart_Monochrome        );
  x.Write('Chart_View3D'            , Chart_View3D            );
  x.Write('Chart_View3DWalls'       , Chart_View3DWalls       );
  x.Write('Chart_MarginLeft'        , Chart_MarginLeft        );
  x.Write('Chart_MarginRight'       , Chart_MarginRight       );
  x.Write('Chart_MarginTop'         , Chart_MarginTop         );
  x.Write('Chart_MarginBottom'      , Chart_MarginBottom      );

  x.Free();
end;

end.

