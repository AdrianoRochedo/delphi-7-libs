unit fDebug;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RARegAuto, ExtCtrls, RAEditor, RAHLEditor;

type
  TDebugLog = class(TForm)
    memDebug: TRAHLEditor;
    RegAuto1: TRegAuto;
    Panel1: TPanel;
    cbDebug: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DebugLog: TDebugLog;

implementation

{$R *.DFM}

end.
