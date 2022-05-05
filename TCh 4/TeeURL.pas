{******************************************}
{   TeeChart Pro  - URL files retrieval    }
{ Copyright (c) 1995-1998 by David Berneda }
{    All Rights Reserved                   }
{******************************************}
{$I teedefs.inc}
unit TeeURL;

interface

Uses Classes, Windows, Chart;

{ Read a Chart from a file ( Chart1,'http://www.teemach.com/demo.tee' ) }
Procedure LoadChartFromURL(Var AChart:TCustomChart; Const URL:String);

Function DownloadURL(URL:PChar; ToStream:TMemoryStream): HResult;
Procedure ShowMessageUser(Const S:String);

implementation

Uses TeeProcs, SysUtils, TeeStore, TeeProCo;

Procedure LoadChartFromURL(Var AChart:TCustomChart; Const URL:String);
var tmp:Integer;
    l,t,w,h:Integer;
    Stream:TMemoryStream;
    tmpURL:String;
begin
  Stream:=TMemoryStream.Create;
  try
    tmpURL:=URL;
    tmp:=DownloadURL(PChar(tmpURL),Stream);
    if tmp=0 then
    begin
      l:=AChart.Left;
      t:=AChart.Top;
      w:=AChart.Width;
      h:=AChart.Height;
      LoadChartFromStream(TCustomChart(AChart),Stream);
      if csDesigning in AChart.ComponentState then
         AChart.SetBounds(l,t,w,h);
    end
    else
      Raise ChartException.CreateFmt(TeeMsg_CannotLoadChartFromURL,[tmp,URL]);
  finally
    Stream.Free;
  end;
end;

Procedure ShowMessageUser(Const S:String);
var St:Array[0..1023] of Char;
begin
  MessageBox(0, StrPCopy(St,S), '', MB_OK or MB_ICONSTOP or MB_TASKMODAL);
end;

Const INTERNET_OPEN_TYPE_PRECONFIG=0;
      INTERNET_FLAG_RAW_DATA = $40000000;               { receive the item as raw data }
      INTERNET_FLAG_NO_CACHE_WRITE        = $04000000;  { don't write this item to the cache }
      INTERNET_FLAG_DONT_CACHE            = INTERNET_FLAG_NO_CACHE_WRITE;


type
  HINTERNET = Pointer;

var WinInetDLL:THandle=0;

   _InternetOpenA:function(lpszAgent: PAnsiChar; dwAccessType: DWORD;
                          lpszProxy, lpszProxyBypass: PAnsiChar;
                          dwFlags: DWORD): HINTERNET; stdcall;

   _InternetOpenURLA:function(hInet: HINTERNET; lpszUrl: PAnsiChar;
                         lpszHeaders: PAnsiChar; dwHeadersLength: DWORD; dwFlags: DWORD;
                         dwContext: DWORD): HINTERNET; stdcall;

  _InternetReadFile:function(hFile: HINTERNET; lpBuffer: Pointer;
                         dwNumberOfBytesToRead: DWORD;
                         var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;

  _InternetCloseHandle:function(hInet: HINTERNET): BOOL; stdcall;

procedure InitWinInet;
var OldError: Longint;
begin
  OldError:=SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    if WinInetDLL = 0 then
    begin
      WinInetDLL:=LoadLibrary('wininet.dll');
      if WinInetDLL <> 0 then
      begin
        @_InternetOpenA      :=GetProcAddress(WinInetDLL,'InternetOpenA');
        @_InternetOpenURLA   :=GetProcAddress(WinInetDLL,'InternetOpenUrlA');
        @_InternetReadFile   :=GetProcAddress(WinInetDLL,'InternetReadFile');
        @_InternetCloseHandle:=GetProcAddress(WinInetDLL,'InternetCloseHandle');
      end;
    end;
  finally
    SetErrorMode(OldError);
  end;
end;

Function DownloadURL(URL:PChar; ToStream:TMemoryStream): HResult;
var H1,H2:HINTERNET;
    Buf:Pointer;
    {$IFDEF D5}
    r:DWord;
    {$ELSE}
    r:Integer;
    {$ENDIF}
    MaxSize:Integer;
begin
  {$IFDEF D5}
  result:=$80000000;
  {$ELSE}
  result:=-1;
  {$ENDIF}
  if WinInetDLL=0 then InitWinInet;
  if WinInetDLL<>0 then
  begin
    h1:=_InternetOpenA('Tee',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,INTERNET_FLAG_DONT_CACHE);
    if h1<>nil then
    try
      h2:=_InternetOpenUrlA(h1,URL,nil,{$IFDEF D5}$80000000{$ELSE}-1{$ENDIF},
                              INTERNET_FLAG_DONT_CACHE {or
                              INTERNET_FLAG_RAW_DATA},
      {INTERNET_FLAG_EXISTING_CONNECT} 0);
      if h2<>nil then
      try
        MaxSize:=64*1024;
        Buf:=AllocMem(MaxSize);
        try
          r := 0;
          if _InternetReadFile( h2,Buf,MaxSize,r) then
          begin
            ToStream.Position:=0;
            ToStream.WriteBuffer(Buf^,r);
            ToStream.Position:=0;
            result:=0;
          end
          else result:=GetLastError;
        finally
          FreeMem(Buf,MaxSize);
        end;
      finally
        if not _InternetCloseHandle(h2) then result:=GetLastError;
      end
      else result:=GetLastError;
    finally
      if not _InternetCloseHandle(h1) then result:=GetLastError;
    end
    else result:=GetLastError;
  end
  else ShowMessageUser('Cannot load WinInet.dll to access TeeChart file: '+URL);
end;

initialization
  {$IFDEF C3}  
  WinInetDLL:=0; { <-- BCB3 crashes in Win95 if we do not do this... }
  {$ENDIF}
finalization
  if WinInetDLL<>0 then FreeLibrary(WinInetDLL);
end.

