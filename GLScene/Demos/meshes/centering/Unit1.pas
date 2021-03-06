{: Illustrates the effects of AutoCentering for FreeForms.<p>

   The same mesh is loaded three times and centered with different options
   (by default, the polyhedron is not centered in its mesh).
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLVectorFileObjects, GLObjects, GLMisc, ComCtrls, StdCtrls,
  GLWin32Viewer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    DummyCube2: TDummyCube;
    DummyCube3: TDummyCube;
    FreeForm1: TFreeForm;
    FreeForm2: TFreeForm;
    FreeForm3: TFreeForm;
    GLLightSource1: TGLLightSource;
    DCCamera: TDummyCube;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
const
   cFileName = '..\..\media\polyhedron.3ds';
begin
   // left one
   FreeForm3.AutoCentering:=[macCenterX, macCenterZ];
   FreeForm3.LoadFromFile(cFileName);
   // central one
   FreeForm2.AutoCentering:=[macCenterY];
   FreeForm2.LoadFromFile(cFileName);
   // right one
   FreeForm1.AutoCentering:=[macCenterX, macCenterY, macCenterZ];
   FreeForm1.LoadFromFile(cFileName); 
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   DCCamera.PitchAngle:=TrackBar1.Position;
end;

end.
