unit TimeUtils;

interface
uses Classes, Windows, SysUtils;

type
  TCronometro = class
  private
    FCount     : Cardinal;
    FStartTime : Cardinal;
    FTime      : Cardinal;
    FLogName   : String;
    FTitle     : String;
  public
    constructor Create(const LogName: String = '');

    procedure Start(const Title: String);
    procedure Stop;

    property Time : Cardinal read FTime;
    property LogName : String read FLogName;
  end;

implementation

{ TCronometro }

constructor TCronometro.Create(const LogName: String);
begin
  inherited Create;
  FLogName := LogName;
end;

procedure TCronometro.Start(const Title: String);
begin
  FTitle := Title;
  inc(FCount);
  FStartTime := GetTickCount;
end;

procedure TCronometro.Stop;
var f: TFileStream;
    s: String;
    m: Word;
begin
  FTime := GetTickCount - FStartTime;
  if FLogName <> '' then
     begin
     s := FTitle + ': <b>' + FloatToStr(FTime/1000) + '</b> segundos <br>'#13#10;
     if FCount = 1 then m := fmCreate else m := fmOpenReadWrite;
     f := TFileStream.Create(FLogName, m);
     f.Seek(F.Size, 0);
     f.WriteBuffer(pointer(s)^, Length(s));
     f.Free;
     end;
end;

end.
