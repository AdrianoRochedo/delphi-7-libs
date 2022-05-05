unit Book_Interfaces;

interface
uses Classes, Controls, Graphics;

type
  IBookPage = interface
    function getControl(): TWinControl;
  end;

  IPageFactory = interface
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

  IPage = interface
    ['{DF3BA9D9-147B-4D70-A27C-F0A24B3AB8B1}']
    procedure setReadOnly(b: boolean);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
  end;

  IadvPage = interface(IPage)
    ['{064CD948-FD39-4322-B8FD-9F2296A7B953}']
    procedure setFontName(const Name: string);
    procedure setFontColor(const Color: TColor);
    procedure setFontSize(const Size: integer);
  end;

  ITextPage = interface(IadvPage)
    ['{3DE4F9EF-AD50-45B3-809C-06E7789BB6BC}']
    procedure Write(const s: string);
    procedure BeginUpdate();
    procedure EndUpdate();
  end;

  ISheetPage = interface(IadvPage)
    ['{2D3D191B-9B88-4997-9DB0-7FB60BD09C78}']

    // row and col are always based in 1
    procedure Write(row, col: integer; const s: string);
  end;

implementation

end.
