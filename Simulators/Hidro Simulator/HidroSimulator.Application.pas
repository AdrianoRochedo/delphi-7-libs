unit HidroSimulator.Application;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Application_Class, HidroSimulator.MainForm;

type
  THidroApplication = class(TApplication)
  private
    function getMainForm: TMainForm;

  protected
    procedure CreateMainForm(); override;
  public
    property MainForm : TMainForm read getMainForm;
  end;

  function Applic(): THidroApplication;

implementation

{$R *.dfm}

  function Applic(): THidroApplication;
  begin
    result := THidroApplication( TSystem.getAppInstance() );
  end;

{ THidroApplication }

procedure THidroApplication.CreateMainForm();
var MainForm: TMainForm;
begin
  self.CreateForm(TMainForm, MainForm);
end;

function THidroApplication.getMainForm(): TMainForm;
begin
  result := TMainForm(inherited MainForm);
end;

end.
