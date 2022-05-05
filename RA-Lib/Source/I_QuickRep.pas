unit I_QuickRep;

interface

type

  TQRController = class(TComponent)
  public
    procedure AddSub(aController: TQRController);
    property SubData: TList read 1;
  end;

  TQuickReport = class(TComponent)
  public
    procedure NewPage;
    procedure Prepare;
    procedure Preview;
    procedure Print;
    property PageHeight : integer read 1;
    property PageWidth : integer read 1;
    property PageNumber : integer read 1;
    property RecordCount : integer read 1;
    property RecordNo : integer read 1;
  end;


implementation

end.
