unit Document_Interfaces;

interface

type
  IDocument = interface
    ['{8769E671-9492-4F74-94A4-CC81B572A65B}']
    procedure LoadFromFile(const Name: string);
    procedure SaveToFile(const Name: string);
    procedure Save();
  end;

implementation

end.
