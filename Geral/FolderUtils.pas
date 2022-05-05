unit FolderUtils;

interface

  Function WindowsDir() : String;
  Function WindowsTempDir(): String;

const
  CSIDL_DESKTOP                       = $0000;
  CSIDL_INTERNET                      = $0001;
  CSIDL_PROGRAMS                      = $0002;
  CSIDL_CONTROLS                      = $0003;
  CSIDL_PRINTERS                      = $0004;
  CSIDL_PERSONAL                      = $0005;
  CSIDL_FAVORITES                     = $0006;
  CSIDL_STARTUP                       = $0007;
  CSIDL_RECENT                        = $0008;
  CSIDL_SENDTO                        = $0009;
  CSIDL_BITBUCKET                     = $000a;
  CSIDL_STARTMENU                     = $000b;
  CSIDL_DESKTOPDIRECTORY              = $0010;
  CSIDL_DRIVES                        = $0011;
  CSIDL_NETWORK                       = $0012;
  CSIDL_NETHOOD                       = $0013;
  CSIDL_FONTS                         = $0014;
  CSIDL_TEMPLATES                     = $0015;
  CSIDL_COMMON_STARTMENU              = $0016;
  CSIDL_COMMON_PROGRAMS               = $0017;
  CSIDL_COMMON_STARTUP                = $0018;
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;
  CSIDL_APPDATA                       = $001a; { C:\Documents and Settings\rochedo\Application Data }
  CSIDL_PRINTHOOD                     = $001b;
  CSIDL_ALTSTARTUP                    = $001d;
  CSIDL_COMMON_ALTSTARTUP             = $001e;
  CSIDL_COMMON_FAVORITES              = $001f;
  CSIDL_INTERNET_CACHE                = $0020;
  CSIDL_COOKIES                       = $0021;
  CSIDL_HISTORY                       = $0022;
  CSIDL_COMMON_APPDATA                = $0023; { All Users\Application Data }
  CSIDL_WINDOWS                       = $0024; { GetWindowsDirectory() }
  CSIDL_SYSTEM                        = $0025; { GetSystemDirectory() }
  CSIDL_PROGRAM_FILES                 = $0026; { C:\Program Files }
  CSIDL_MYPICTURES                    = $0027; { My Pictures, new for Win2K }
  CSIDL_PROGRAM_FILES_COMMON          = $002b; { C:\Program Files\Common }
  CSIDL_COMMON_DOCUMENTS              = $002e; { All Users\Documents }
  CSIDL_COMMON_ADMINTOOLS             = $002f; { All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS                    = $0030; { <user name>\Start Menu\Programs\Administrative Tools }

  function GetSpecialFolderPath(const FolderID: Integer; Create: Boolean = False): string;
  function GetAppDataFolder(): string;

implementation
uses Windows, SysUtils, ShlObj;

function AddSlash(const s: String): String;
begin
  Result := s;
  if Result <> '' then
     if Result[Length(Result)] <> '\' Then Result := Result  + '\';
end;

Function WindowsDir(): String;
var path: Array[0..200] of Char;
begin
  GetWindowsDirectory(path, sizeOf(path));
  Result := AddSlash(String(path));
end;

Function WindowsTempDir(): String;
var path: Array[0..200] of char;
begin
  GetTempPath(SizeOf(path), path);
  Result := AddSlash(String(path));
end;

function GetSpecialFolderPath(const FolderID: Integer; Create: Boolean = False): String;
var path: Array[0..200] of char;
begin
  path[0] := #0;
  SHGetSpecialFolderPath(0, path, FolderID, Create);
  Result := AddSlash(String(Path));
end;

// retorna algo do tipo:
// C:\Documents and Settings\rochedo\Application Data 
function GetAppDataFolder(): string;
begin
  result := GetSpecialFolderPath(CSIDL_APPDATA, true);
end;

end.
