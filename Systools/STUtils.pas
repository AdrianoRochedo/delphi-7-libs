{$I STDEFINE.INC}

{$A+} {Aligned records}
{$B-} {Incomplete boolean evaluation}
{$I+} {I/O Checking On}                                                {!!.01}
{$W-} {No special Windows stack frames}
{$X+} {Extended syntax}

{$IFNDEF OS32}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   STUTILS.PAS 1.05                    *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit STUtils;
  {-Assorted utility routines}

interface

uses
{$IFDEF OS32}
 {$IFDEF WIN32}
  Windows,
 {$ELSE}
  Os2Def, Os2Base,
 {$ENDIF}
{$ELSE}
  WinTypes,
  WinProcs,
{$ENDIF}
  SysUtils,
  Classes,                                                             {!!.01}
  STConst,
  STBase,
  StDate,                                                              {!!.01}
{$IFDEF Win32}                                                         {!!.01}
 {$IFOPT H+}                                                           {!!.01}
  StStrL;                                                              {!!.01}
 {$ELSE}                                                               {!!.01}
  StStrS;                                                              {!!.01}
 {$ENDIF}                                                              {!!.01}
{$ELSE}                                                                {!!.01}
  StStrS;                                                              {!!.01}
{$ENDIF}                                                               {!!.01}


type
  DiskClass = ( Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy,
    HardDisk, RamDisk, UnknownDisk, InvalidDrive, RemoteDrive, CDRomDisk );
    {This enumerated type defines the nine classes of disks that can be
     identified by GetDiskClass, as well as several types used as error
     indications}

  PMediaIDType = ^MediaIDType;
  MediaIDType = packed record
  {This type describes the information that DOS 4.0 or higher writes
   in the boot sector of a disk when it is formatted}
    InfoLevel : Word;                        {Reserved for future use}
    SerialNumber : LongInt;                  {Disk serial number}
    VolumeLabel : array[0..10] of AnsiChar;  {Disk volume label}
    FileSystemID : array[0..7] of AnsiChar;  {String for internal use by the OS}
  end;

  TIncludeItemFunc = function (const SR : TSearchRec) : Boolean;       {!!.01 Added}

                                                                       {!!.01 Added}
function SignL(L : LongInt) : Integer;
  {-return sign of LongInt value}                                      {!!.01 Added}
function SignF(F : Extended) : Integer;
  {-return sign of floating point value}
                                                                       {!!.01 Added}
function MidWord(W1, W2, W3 : Word) : Word;
  {-return the middle of three Word values}                            {!!.01 Added}
function MidLong(L1, L2, L3 : LongInt) : LongInt;
  {-return the middle of three LongInt values}                         {!!.01 Added}
function MidFloat(F1, F2, F3 : Extended) : Extended;
  {-return the middle of three floating point values}

function MinFloat(F1, F2 : Extended) : Extended;                       {!!.01 Added}
  {-return the lesser of two floating point values}
function MaxFloat(F1, F2 : Extended) : Extended;                       {!!.01 Added}
  {-return the greater of two floating point values}

function IsDirectoryEmpty(const S : string) : Integer;                 {!!.01 Added}
  {-checks if there are any entries in the directory}
function IsFileHidden(const S : string) : Integer;                     {!!.01 Added}
  {-checks if file's hidden attribute is set}
function IsFileSystem(const S : string) : Integer;                     {!!.01 Added}
  {-checks if file's system attribute is set}
function IsFileReadOnly(const S : string) : Integer;                   {!!.01 Added}
  {-checks if file's readonly attribute is set}
function IsFileArchive(const S : string) : Integer;                    {!!.01 Added}
  {-checks if file's archive attribute is set}

procedure EnumerateFiles(StartDir : string; FL : TStrings;             {!!.01 Added}
                         SubDirs : Boolean;
                         IncludeItem : TIncludeItemFunc);

procedure EnumerateDirectories(StartDir : string; FL : TStrings;       {!!.01 Added}
                               SubDirs : Boolean;
                               IncludeItem : TIncludeItemFunc);

function FileTimeToStDateTime(FileTime : LongInt) : TStDateTimeRec;    {!!.01 Added}
function StDateTimeToFileTime(FileTime : TStDateTimeRec) : LongInt;    {!!.01 Added}


{-Assorted utility routines.  16 bit versions are inline }

function MakeInteger16(H, L : Byte): SmallInt;
  {-Construct an integer from two bytes}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax    ;low byte into AL}
    $5B/                   {pop bx    ;high byte into BL}
    $88/$DC);              {mov ah,bl ;high byte into AH}
{$ENDIF}

function MakeWord(H, L : Byte) : Word;
  {-Construct a word from two bytes}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax    ;low byte into AL}
    $5B/                   {pop bx    ;high byte into BL}
    $88/$DC);              {mov ah,bl ;high byte into AH}
{$ENDIF}

function SwapNibble(B : Byte) : Byte;
  {-Swap the high and low nibbles of a byte}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax}
    $B1/$04/               {mov cl,4}
    $D2/$C8);              {ror al,cl}
{$ENDIF}

function SwapWord(L : LongInt) : LongInt;
  {-Swap the low- and high-order words of a long integer}
{$IFNDEF OS32}
  inline(
    $5A/                   {pop dx ;pop low word into DX}
    $58);                  {pop ax ;pop high word into AX}
{$ENDIF}

procedure SetFlag(var Flags : Word; FlagMask : Word);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax        ;FlagMask into AX}
    $5F/                   {pop di}
    $07/                   {pop es        ;ES:DI => Flags}
    $26/$09/$05);          {or es:[di],ax ;Flags := Flags or FlagMask}
{$ENDIF}

procedure ClearFlag(var Flags : Word; FlagMask : Word);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in Flagmask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax         ;FlagMask into AX}
    $5F/                   {pop di}
    $07/                   {pop es         ;ES:DI => Flags}
    $F7/$D0/               {not ax         ;FlagMask := not FlagMask}
    $26/$21/$05);          {and es:[di],ax ;Flags := Flags and not FlagMask}
{$ENDIF}

function FlagIsSet(Flags, FlagMask : Word) : Boolean;
  {-Return True if the bit specified by FlagMask is set in Flags}
{$IFNDEF OS32}
  inline(
    $5A/                   {pop dx    ;FlagMask into DX}
    $58/                   {pop ax    ;Flags into AX}
    $21/$D0/               {and ax,dx ;Mask out everything not in FlagMask}
    $74/$03/               {jz  Exit}
    $B8/$01/$00);          {mov ax,1  ;AX = Ord(True)}
                           {Exit:}
{$ENDIF}

procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax        ;FlagMask into AL}
    $5F/                   {pop di}
    $07/                   {pop es        ;ES:DI => Flags}
    $26/$08/$05);          {or es:[di],al ;Flags := Flags or FlagMask}
{$ENDIF}

procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax         ;FlagMask into AL}
    $5F/                   {pop di}
    $07/                   {pop es         ;ES:DI => Flags}
    $F6/$D0/               {not al         ;AL := not AL}
    $26/$20/$05);          {and es:[di],al ;Flags := Flags and not FlagMask}
{$ENDIF}

function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
  {-Return True if the bit specified by FlagMask is set in the Flags parameter}
{$IFNDEF OS32}
  inline(
    $5A/                   {pop dx    ;FlagMask into DL}
    $58/                   {pop ax    ;Flags into AL}
    $30/$E4/               {xor ah,ah ;Zero out AH}
    $20/$D0/               {and al,dl ;Mask out everything not in FlagMask}
    $74/$02/               {jz  Exit}
    $B0/$01);              {mov al,1  ;AX = Ord(True)}
                           {Exit:}
{$ENDIF}

procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax        ;FlagMask into DX:AX}
    $5A/                   {pop dx}
    $5F/                   {pop di}
    $07/                   {pop es        ;ES:DI => Flags}
    $26/$09/$05/           {or es:[di],ax ;Flags := Flags or FlagMask}
    $26/$09/$55/$02);      {or es:[di+2],dx}
{$ENDIF}

procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax         ;FlagMask into DX:AX}
    $5A/                   {pop dx}
    $5F/                   {pop di}
    $07/                   {pop es         ;ES:DI => Flags}
    $F7/$D0/               {not ax         ;FlagMask := not FlagMask}
    $F7/$D2/               {not dx}
    $26/$21/$05/           {and es:[di],ax ;Flags := Flags and not FlagMask}
    $26/$21/$55/$02);      {and es:[di+2],dx}
{$ENDIF}

function LongFlagIsSet(Flags, FlagMask : LongInt) : Boolean;
  {-Return True if the bit specified by FlagMask is set in Flags}
{$IFNDEF OS32}
  inline(
    $5B/                   {pop bx    ;FlagMask into CX:BX}
    $59/                   {pop cx}
    $58/                   {pop ax    ;Flags into DX:AX}
    $5A/                   {pop dx}
    $21/$D8/               {and ax,bx ;Mask out everything not in FlagMask}
    $21/$CA/               {and dx,cx}
    $09/$D0/               {or  ax,dx}
    $74/$03/               {jz  Exit}
    $B8/$01/$00);          {mov ax,1  ;AX = Ord(True)}
                           {Exit:}
{$ENDIF}

procedure ExchangeBytes(var I, J : Byte);
  {-Exchange the values in two bytes}
{$IFNDEF OS32}
  inline(
    $8C/$DB/               {mov bx,ds       ;save DS}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $8A/$04/               {mov al,[si]     ;AL = J}
    $26/$86/$05/           {xchg al,es:[di] ;I = J, AL = I}
    $88/$04/               {mov [si],al     ;J = I}
    $8E/$DB);              {mov ds,bx       ;restore DS}
{$ENDIF}

procedure ExchangeWords(var I, J : Word);
  {-Exchange the values in two words}
{$IFNDEF OS32}
  inline(
    $8C/$DB/               {mov bx,ds       ;save DS}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $8B/$04/               {mov ax,[si]     ;AX = J}
    $26/$87/$05/           {xchg ax,es:[di] ;I = J, AX = I}
    $89/$04/               {mov [si],ax     ;J = I}
    $8E/$DB);              {mov ds,bx       ;restore DS}
{$ENDIF}

procedure ExchangeLongInts(var I, J : LongInt);
  {-Exchange the values in two long integers}
{$IFNDEF OS32}
  inline(
    $8C/$DB/               {mov bx,ds       ;save DS}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $FC/                   {cld}
    $26/$8B/$05/           {mov ax,es:[di]}
    $A5/                   {movsw}
    $89/$44/$FE/           {mov [si-2],ax}
    $8B/$04/               {mov ax,[si]}
    $26/$87/$05/           {xchg ax,es:[di]}
    $89/$04/               {mov [si],ax}
    $8E/$DB);              {mov ds,bx       ;restore DS}
{$ENDIF}

procedure ExchangeStructs(var I, J; Size : Cardinal);
  {-Exchange the values in two structures}
{$IFNDEF OS32}
  inline(
    $FC/                   {cld       ;go forward}
    $8C/$DA/               {mov dx,ds       ;save DS}
    $59/                   {pop cx          ;CX = Size}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $D1/$E9/               {shr cx,1        ;move by words}
    $E3/$0C/               {jcxz odd}
    $9C/                   {pushf}
                           {start:}
    $89/$F3/               {mov bx,si}
    $26/$8B/$05/           {mov ax,es:[di]  ;exchange words}
    $A5/                   {movsw}
    $89/$07/               {mov [bx],ax}
    $E2/$F6/               {loop start      ;again?}
    $9D/                   {popf}
                           {odd:}
    $73/$07/               {jnc exit}
    $8A/$04/               {mov al,[si]     ;exchange the odd bytes}
    $26/$86/$05/           {xchg al,es:[di]}
    $88/$04/               {mov [si],al}
                           {exit:}
    $8E/$DA);              {mov ds,dx       ;restore DS}
{$ENDIF}

function MinWord(A, B : Word) : Word;
  {-Return the smaller of A and B}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax}
    $5B/                   {pop bx}
    $39/$C3/               {cmp bx,ax}
    $73/$02/               {jae done}
    $89/$D8);              {mov ax,bx}
                           {done:}
{$ENDIF}

function MaxWord(A, B : Word) : Word;
  {-Return the greater of A and B}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax}
    $5B/                   {pop bx}
    $39/$C3/               {cmp bx,ax}
    $76/$02/               {jbe done}
    $89/$D8);              {mov ax,bx}
                           {done:}
{$ENDIF}

function MinLong(A, B : LongInt) : LongInt;
  {-Return the smaller of A and B}
{$IFNDEF OS32}
  inline(
    $5B/                   {pop bx       ;CX:BX = B}
    $59/                   {pop cx}
    $58/                   {pop ax       ;DX:AX = A}
    $5A/                   {pop dx}
    $39/$CA/               {cmp dx,cx    ;compare high byte}
    $7C/$0A/               {jl  done1    ;A < B?}
    $7F/$04/               {jg  greater  ;A > B?}
    $39/$D8/               {cmp ax,bx    ;compare low byte}
    $76/$04/               {jbe done1    ;A <= B?}
                           {greater:}
    $89/$CA/               {mov dx,cx    ;A is greater}
    $89/$D8);              {mov ax,bx}
                           {done1:}
{$ENDIF}

function MaxLong(A, B : LongInt) : LongInt;
  {-Return the greater of A and B}
{$IFNDEF OS32}
  inline(
    $5B/                   {pop bx       ;CX:BX = B}
    $59/                   {pop cx}
    $58/                   {pop ax       ;DX:AX = A}
    $5A/                   {pop dx}
    $39/$CA/               {cmp dx,cx    ;compare high byte}
    $7F/$0A/               {jg  done2    ;A > B?}
    $7C/$04/               {jl  less     ;A < B?}
    $39/$D8/               {cmp ax,bx    ;compare low byte}
    $73/$04/               {jae done2    ;A >= B?}
                           {less:}
    $89/$CA/               {mov dx,cx    ;B is greater}
    $89/$D8);              {mov ax,bx}
                           {done2:}
{$ENDIF}

procedure FillWord(var Dest; Count : Cardinal; Filler : Word);
  {-Fill memory with a word-sized filler}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax    ;AX = Filler}
    $59/                   {pop cx    ;CX = Count}
    $5F/                   {pop di    ;ES:DI => Dest}
    $07/                   {pop es}
    $FC/                   {cld       ;go forward}
    $F2/$AB);              {rep stosw ;fill memory}
{$ENDIF}

procedure FillStruct(var Dest; Count : Cardinal; var Filler; FillerSize : Cardinal);
  {-Fill memory with a variable sized filler}
{$IFNDEF OS32}
  inline(
    $58/                   {pop ax     ;AX = FillerSize}
    $5B/                   {pop bx     ;DX:BX => Filler}
    $5A/                   {pop dx}
    $59/                   {pop cx     ;CX = Count}
    $5F/                   {pop di     ;ES:DI => Dest}
    $07/                   {pop es}
    $E3/$11/               {jcxz done  ;done if Count = 0}
    $FC/                   {cld        ;go forward}
    $1E/                   {push ds    ;save DS}
    $8E/$DA/               {mov ds,dx  ;DS:BX => Filler}
                           {again:}
    $89/$CA/               {mov dx,cx  ;save loop count}
    $89/$DE/               {mov si,bx  ;DS:SI => Filler}
    $89/$C1/               {mov cx,ax  ;CX = FillerSize}
    $F2/$A4/               {rep movsb  ;fill}
    $89/$D1/               {mov cx,dx  ;restore loop count}
    $E2/$F4/               {loop again ;repeat}
    $1F);                  {pop ds     ;restore DS}
                           {done:}
{$ENDIF}

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
  {-Add a word to a pointer.}
{$IFNDEF OS32}
  inline(
    $5B/                   {pop bx     ;bx = W}
    $58/                   {pop ax     ;ax = Ofs(P^)}
    $5A/                   {pop dx     ;dx = Seg(P^)}
    $01/$D8);              {add ax,bx  ;ax = Ofs(P^)+W}
{$ENDIF}

{Assorted operating system routines.}

function GetDiskInfo(Drive : AnsiChar; var ClustersAvailable, TotalClusters,
                     BytesPerSector, SectorsPerCluster : Cardinal) : Boolean;
  {-Return technical information about the specified drive.}

function GetDiskSpace(Drive : AnsiChar;                    {new !!.05}
                  var UserSpaceAvail  : Comp;           {space available to user}
                  var TotalSpaceAvail : Comp;           {total space available}
                  var DiskSize        : Comp) : Boolean;{disk size}
  {-Return space information about the drive.}

function GetDiskClass(Drive : AnsiChar) : DiskClass;
  {-Return the disk class for the specified drive}

function FlushOsBuffers(Handle : Integer) : Boolean;
  {-Flush the OS buffers for the specified file handle.}

function FileHandlesLeft(MaxHandles : Cardinal) : Cardinal;
  {-Return the number of available file handles.}

{$IFNDEF OS32}
function FileHandlesOpen(CountDevices : Boolean) : Cardinal;
  {-Return the number of open files owned by a program.}
{$ENDIF}

function IsDirectory(const FName : string) : Boolean;
  {-Return True if FName is a directory.}

function SameFile(const FilePath1, FilePath2 : string; var ErrorCode : Integer) : Boolean;
  {-Return True if FilePath1 and FilePath2 refer to the same physical file.}

function CopyFile(const SrcPath, DestPath : string) : Cardinal;
  {-Copy a file.}

function ReadVolumeLabel(var VolName : string; Drive : AnsiChar) : Cardinal;
  {-Get the volume label for the specified drive.}

function WriteVolumeLabel(const VolName : string; Drive : AnsiChar) : Cardinal;
  {-Sets the volume label for the specified drive.}

function DeleteVolumeLabel(Drive : AnsiChar) : Cardinal;
  {-Deletes an existing volume label on Drive. Returns 0 for success,
  or OS error code.}

function GetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
  {-Get the media ID record for the specified drive.}

function SetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
  {-Set the media ID record for the specified drive.}

function ValidDrive(Drive : AnsiChar) : Boolean;
  {-Determine if the drive is a valid drive.}

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

const
  ecOutOfMemory = 8;

{$IFDEF OS32}
 {$IFDEF VirtualPascal}
type
  DWORD = Longint;
 {$ENDIF}
const
  FILE_ANY_ACCESS = 0;
  METHOD_BUFFERED = 0;
  IOCTL_DISK_BASE = $00000007;
  VWIN32_DIOC_DOS_IOCTL = 1;
  IOCTL_DISK_GET_MEDIA_TYPES = ((IOCTL_DISK_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0300 shl 2) or METHOD_BUFFERED);

type
  TMediaType =
    ( Unknown,                { Format is unknown }
      F5_1Pt2_512,            { 5.25", 1.2MB,  512 bytes/sector }
      F3_1Pt44_512,           { 3.5",  1.44MB, 512 bytes/sector }
      F3_2Pt88_512,           { 3.5",  2.88MB, 512 bytes/sector }
      F3_20Pt8_512,           { 3.5",  20.8MB, 512 bytes/sector }
      F3_720_512,             { 3.5",  720KB,  512 bytes/sector }
      F5_360_512,             { 5.25", 360KB,  512 bytes/sector }
      F5_320_512,             { 5.25", 320KB,  512 bytes/sector }
      F5_320_1024,            { 5.25", 320KB,  1024 bytes/sector }
      F5_180_512,             { 5.25", 180KB,  512 bytes/sector }
      F5_160_512,             { 5.25", 160KB,  512 bytes/sector }
      RemovableMedia,         { Removable media other than floppy }
      FixedMedia );           { Fixed hard disk media }

  PDiskGeometry = ^TDiskGeometry;
  TDiskGeometry = record
    Cylinders1 : DWORD;
    Cylinders2 : Integer;
    MediaType : TMediaType;
    TracksPerCylinder : DWORD;
    SectorsPerTrack : DWORD;
    BytesPerSector : DWORD;
  end;
{$ELSE}
type
  DescriptorTableEntry = record
    LimitL : Word;
    BaseL  : Word;
    Words : array[0..1] of Word;
  end;

  OS = record
    O, S : Word;
  end;

  XFCBrec = record
    Flag : Byte;                      {should be $FF}
    Reserved0 : array[1..5] of Byte;  {should be all zeroes}
    AttrByte : Byte;                  {should be 8}
    DriveCode : Byte;
    FileSpec : array[1..11] of AnsiChar;
    Reserved1 : array[1..25] of Byte;
  end;

  DTABuf = array[0..63] of AnsiChar;

  DPMIRegisters = record
    DI : LongInt;
    SI : LongInt;
    BP : LongInt;
    Reserved : LongInt;
    case integer of
    1 : ( BX : LongInt;
          DX : LongInt;
          CX : LongInt;
          AX : LongInt;
          Flags : Word;
          ES : Word;
          DS : Word;
          FS : Word;
          GS : Word;
          IP : Word;
          CS : Word;
          SP : Word;
          SS : Word );
    2 : ( BL, BH : Byte; EBXH : Word;
          DL, DH : Byte; EDXH : Word;
          CL, CH : Byte; ECXH : Word;
          AL, AH : Byte; EAXH : Word );
  end;
{$ENDIF}

type
  DevIOCtlRegisters = record
    reg_EBX : LongInt;
    reg_EDX : LongInt;
    reg_ECX : LongInt;
    reg_EAX : LongInt;
    reg_EDI : LongInt;
    reg_ESI : LongInt;
    reg_Flags : LongInt;
  end;

{$IFDEF OS32}   { 32-bit replacements for 16-bit inline }

function MakeInteger16(H, L : Byte): SmallInt;
begin
  Result := (SmallInt(H) shl 8) or L;
end;

function SwapNibble(B : Byte) : Byte;
begin
  Result := (B shr 4) or (B shl 4);
end;

{$IFDEF VirtualPascal}
{&frame-} {&uses none}
function SwapWord(L : LongInt) : LongInt;
asm
  mov eax,l
  ror eax,16;
end;
{$ELSE}
function SwapWord(L : LongInt) : LongInt; register;
asm
  ror eax,16;
end;
{$ENDIF}

procedure SetFlag(var Flags : Word; FlagMask : Word);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearFlag(var Flags : Word; FlagMask : Word);
begin
  Flags := Flags and (not FlagMask);
end;


function FlagIsSet(Flags, FlagMask : Word) : Boolean;
begin
  Result := (FlagMask AND Flags <> 0);
end;

procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
begin
  Flags := Flags and (not FlagMask);
end;

function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
begin
  Result := (FlagMask AND Flags <> 0);
end;

procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt);
begin
  Flags := Flags and (not FlagMask);
end;

function LongFlagIsSet(Flags, FlagMask : LongInt) : Boolean;
begin
  Result := FlagMask = (Flags and FlagMask);
end;

{&frame-} {&uses none}
procedure ExchangeBytes(var I, J : Byte);
{$IFDEF WIN32}
register;
asm
{$ELSE}
asm
  mov  eax,i
  mov  edx,j
{$ENDIF}
  mov  cl, [eax]
  mov  ch, [edx]
  mov  [edx], cl
  mov  [eax], ch
end;

{&frame-} {&uses none}
procedure ExchangeWords(var I, J : Word);
{$IFDEF WIN32}
register;
asm
{$ELSE}
asm
  mov  eax,i
  mov  edx,j
{$ENDIF}
  mov  cx, [eax]
  push ecx
  mov  cx, [edx]
  mov  [eax], cx
  pop  ecx
  mov  [edx], cx
end;

{&frame-} {&uses none}
procedure ExchangeLongInts(var I, J : LongInt);
{$IFDEF WIN32}
register;
asm
{$ELSE}
asm
  mov  eax,i
  mov  edx,j
{$ENDIF}
  mov  ecx, [eax]
  push ecx
  mov  ecx, [edx]
  mov  [eax], ecx
  pop  ecx
  mov  [edx], ecx
end;

{&frame-} {&uses none}
procedure ExchangeStructs(var I, J; Size : Cardinal);
{$IFDEF WIN32}
register;
asm
{$ELSE}
asm
  mov  eax,i
  mov  edx,j
  mov  ecx,Size
{$ENDIF}
  push edi
  push ebx
  push ecx
  shr  ecx, 2
  jz   @@LessThanFour

@@AgainDWords:
  mov  ebx, [eax]
  mov  edi, [edx]
  mov  [edx], ebx
  mov  [eax], edi
  add  eax, 4
  add  edx, 4
  dec  ecx
  jnz  @@AgainDWords

@@LessThanFour:
  pop  ecx
  and  ecx, $3
  jz   @@Done
  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh
  inc  eax
  inc  edx
  dec  ecx
  jz   @@Done

  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh
  inc  eax
  inc  edx
  dec  ecx
  jz   @@Done

  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh

@@Done:
  pop  ebx
  pop  edi
end;

{!!.03  - revised}
{&frame-} {&uses edi}
procedure FillWord(var Dest; Count : Cardinal; Filler : Word);
asm
{$IFDEF WIN32}
  push edi
{$ENDIF}
  mov   edi,Dest
  mov   ax,Filler
  mov   ecx,Count
  cld
  rep  stosw
{$IFDEF WIN32}
  pop   edi
{$ENDIF}
end;

{&frame+} {&uses none}                                                 {!!.03}
procedure FillStruct(var Dest; Count : Cardinal; var Filler;
  FillerSize : Cardinal);
{$IFDEF WIN32}
register;
asm
{$ELSE}
asm
  mov  eax,Dest
  mov  edx,Count
  mov  ecx,Filler
{$ENDIF}
  or   edx, edx
  jz   @@Exit

  push edi
  push esi
  push ebx
  mov  edi, eax
  mov  ebx, ecx

@@NextStruct:
  mov  esi, ebx
  mov  ecx, FillerSize
  shr  ecx, 1
  rep  movsw
  adc  ecx, ecx
  rep  movsb
  dec  edx
  jnz  @@NextStruct

  pop  ebx
  pop  esi
  pop  edi

@@Exit:
end;

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
begin
  Result := Pointer(LongInt(P)+W);
end;

function MakeWord(H, L : Byte) : Word;
begin
  Result := (Word(H) shl 8) or L;
end;

function MinWord(A, B : Word) : Word;
begin
  if A < B then
     Result := A
  else
     Result := B;
end;

function MaxWord(A, B : Word) : Word;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MinLong(A, B : LongInt) : LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxLong(A, B : LongInt) : LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{$ENDIF}     { End of 32-bit replacements for 16-bit inline }

{Operating system utilities}

{$IFNDEF OS32}  { 16 bit utility functions }
function GetCPUFlags : Byte; assembler;
asm
  lahf
  mov     al,ah
end;

function FreeLDTDescriptor(Selector : Word) : Word; assembler;
asm
  mov  bx,Selector
  mov  ax,0001h
  int  31h
  jc   @@ExitPoint

  xor  ax,ax

@@ExitPoint:
end;

function AllocLDTDescriptors(NumOfDesc : Word; var BaseSelector : Word) : Word; assembler;
asm
  mov  cx,NumOfDesc
  xor  ax,ax
  int  31h
  jc   @@ExitPoint

  les  di,BaseSelector
  mov  es:[di],ax
  xor  ax,ax

@@ExitPoint:
end;

function SetSegmentBaseAddr(Selector : Word; BaseAddress : LongInt) : Word; assembler;
asm
  mov  bx,Selector
  mov  dx,word ptr BaseAddress
  mov  cx,word ptr BaseAddress+2
  mov  ax,0007h
  int  31h
  jc   @@ExitPoint

  xor  ax,ax

@@ExitPoint:
end;

function GetDescriptor(Selector : Word;
                       var Descriptor : DescriptorTableEntry) : Word; assembler;
asm
  mov  ax,000Bh
  mov  bx,Selector
  les  di,Descriptor
  int  31h
  jc   @@ExitPoint

  xor  ax,ax

@@ExitPoint:
end;

function GetDTA : Pointer; assembler;
asm
  mov  ah,2Fh
  int  21h
  mov  ax,bx
  mov  dx,es
end;

procedure SetDTA(var DTA); assembler;
asm
  push ds
  lds  dx,DTA
  mov  ah,1Ah
  int  21h
  pop  ds
end;

function SetLimitPrim(Selector : Word; Limit : LongInt) : Word; assembler;
asm
  mov  bx,Selector
  mov  dx,word ptr Limit
  mov  cx,word ptr Limit+2
  mov  ax,0008h
  int  31h
  jc   @@ExitPoint

  xor  ax,ax

@@ExitPoint:
end;

function SetRightsPrim(Selector : Word; Rights : Word) : Word; assembler;
asm
  mov  bx,Selector
  mov  cx,Rights
  mov  ax,0009h
  int  31h
  jc   @@ExitPoint

  xor  ax,ax

@@ExitPoint:
end;

function SimulateRealModeInt(IntNo : Byte;
                             var Regs : DPMIRegisters) : Word; Assembler;
asm
  xor     bx,bx
  mov     bl,IntNo
  xor     cx,cx       {StackWords = 0}
  les     di,Regs
  mov     ax,0300h
  int     31h
  jc      @@ExitPoint

  xor     ax,ax
@@ExitPoint:
end;

function Linear(P : Pointer) : LongInt;
begin
  with OS(P) do
    Result := (LongInt(S) shl 4) + LongInt(O);
end;

function GetSegmentAccessRights(Selector : Word; var Rights : Word) : Word;
var
  Status : Word;
  Descriptor : DescriptorTableEntry;
begin
  Status := GetDescriptor(Selector, Descriptor);
  if Status = 0 then
    with Descriptor do
      Rights := (Words[0] shr 8) or ((Words[1] and $00F0) shl 8);
  Result := Status;
end;

function SetSegmentLimit(Selector : Word; Limit : LongInt) : Word;
var
  Rights : Word;
  Status : Word;
begin
  {Handle limit granularity}
  Status := GetSegmentAccessRights(Selector, Rights);
  if Status <> 0 then begin
    Result := Status;
    Exit;
  end;
  if Limit > $FFFFF then begin
    {Segment larger than 1MB}
    if Limit and $FFF <> $FFF then begin
      {Not page aligned}
      Result := $8021;
      Exit;
    end;
    Rights := Rights or $8000;       {Page-granular}
  end else
    Rights := Rights and not $8000;  {Byte-granular}

   {Assure no overflow when granularity changed}
  Status := SetLimitPrim(Selector, 0);
  if Status = 0 then
    Status := SetRightsPrim(Selector, Rights);
  if Status = 0 then
    Result := SetLimitPrim(Selector, Limit);
  Result := Status;
end;

function GetSelectorForRealMem(RealPtr : Pointer; Limit : LongInt; var Selector : Word) : Word;

  procedure FreeSele;
  begin
    FreeLDTDescriptor(Selector);
  end;

var
  ErrorCode : Word;

begin
  ErrorCode := AllocLDTDescriptors(1, Selector);
  if ErrorCode = 0 then begin
    ErrorCode := SetSegmentBaseAddr(Selector, Linear(RealPtr));
    if ErrorCode = 0 then begin
      ErrorCode := SetSegmentLimit(Selector, Limit);
      if ErrorCode <> 0 then
        FreeSele;
    end else
      FreeSele;
  end;
  Result := ErrorCode;
end;

function DosFCBCreate(var XFCB : XFCBRec) : Byte;
type
  DoubleWord = record LoWord, HiWord : word; end;
var
  Regs : DPMIRegisters;
  L : LongInt;
  RP, PP : ^XFCBRec;
begin
  L := GlobalDosAlloc(SizeOf(XFCBRec));
  if L = 0 then begin
    Result := ecOutOfMemory;
    Exit;
  end;
  RP := Ptr(DoubleWord(L).HiWord, 0);
  PP := Ptr(DoubleWord(L).LoWord, 0);
  FillChar(Regs, SizeOf(Regs), 0);
  Move(XFCB, PP^, SizeOf(XFCBRec));
  with Regs do begin
    DS := OS(RP).S;
    DX := OS(RP).O;
    AX := $1600;
    SimulateRealModeInt($21, Regs);
    Result := Lo(DoubleWord(AX).LoWord);
  end;
  Move(PP^, XFCB, Sizeof(XFCBRec));
  GlobalDosFree(OS(PP).S);
end;

function DosFCBClose(var XFCB : XFCBRec) : Byte;
type
  DoubleWord = record LoWord, HiWord : word; end;
var
  Regs : DPMIRegisters;
  L : LongInt;
  RP, PP : ^XFCBRec;
begin
  L := GlobalDosAlloc(SizeOf(XFCBRec));
  if L = 0 then begin
    Result := ecOutOfMemory;
    Exit;
  end;
  RP := Ptr(DoubleWord(L).HiWord, 0);
  PP := Ptr(DoubleWord(L).LoWord, 0);
  FillChar(Regs, SizeOf(Regs), 0);
  Move(XFCB, PP^, SizeOf(XFCBRec));
  with Regs do begin
    DS := OS(RP).S;
    DX := OS(RP).O;
    AX := $1000;
    SimulateRealModeInt($21, Regs);
    Result := Lo(DoubleWord(AX).LoWord);
  end;
  GlobalDosFree(OS(PP).S);
end;

procedure SelectDrive(Drive : AnsiChar); assembler;
asm
  mov  dl,Drive
  cmp  dl,'z'
  ja   @@ExitPoint

  cmp  dl,'a'
  jb   @@GotDrive

  sub  dl,32

@@GotDrive:
  sub  dl,'A'
  mov  ah,0Eh
  int  21h

@@ExitPoint:
end;

function DefaultDrive : AnsiChar; assembler;
asm
  mov  ah,19h
  int  21h
  add  al,'A'
end;

function CDRomInstalled : Boolean; assembler;
asm
  mov  ax,1500h
  xor  bx,bx
  int  2Fh
  mov  al,0
  or   bx,bx
  jz   @@ExitPoint

  inc  al

@@ExitPoint:
end;

function IsDriveCDROM(Drive : AnsiChar) : Boolean; assembler;
asm
  xor  bx,bx
  xor  cx,cx
  mov  cl,Drive
  cmp  cl,'a'
  jb   @@Upper

  sub  cl,32

@@Upper:
  sub  cl,'A'
  mov  ax,150Bh
  int  2Fh
  mov  cx,ax
  mov  al,0
  cmp  bx,0ADADh
  jne  @@ExitPoint

  or   cx,cx
  jz   @@ExitPoint

  inc  al
@@ExitPoint:
end;

{$ENDIF}  { End 16-bit utility functions }

{$IFDEF OS32}
{$IFDEF VirtualPascal}
function GetDiskInfo(Drive : AnsiChar; var ClustersAvailable, TotalClusters,
                     BytesPerSector, SectorsPerCluster : Cardinal) : Boolean;
Var
  rc        : ApiRet;
  DriveNum  : Word;
  FSInfo    : FSAllocate;

begin
  DriveNum := ord(Drive);
  rc := DosQueryFSInfo( DriveNum, fsil_Alloc, FSInfo, Sizeof( FSInfo ) );
  If rc = 0 then
    begin
      SectorsPerCluster := FSInfo.cSectorUnit;
      ClustersAvailable := FSInfo.cUnitAvail;
      TotalClusters     := FSInfo.cUnit;
      BytesPerSector    := FSInfo.cbSector;
      GetDiskInfo := True;
    end
  else
    GetDiskInfo := False;
end;
{$ELSE}
function GetDiskInfo(Drive : AnsiChar; var ClustersAvailable, TotalClusters,
                     BytesPerSector, SectorsPerCluster : Cardinal) : Boolean;
var
  Root : string;
begin
  if Drive <> ' ' then begin
    Root := AnsiChar(Upcase(Drive)) + ':\';
    Result := GetDiskFreeSpace(PAnsiChar(Root), DWORD(SectorsPerCluster),
      DWORD(BytesPerSector), DWORD(ClustersAvailable), DWORD(TotalClusters));
  end else
    Result := GetDiskFreeSpace(nil, DWORD(SectorsPerCluster),
      DWORD(BytesPerSector), DWORD(ClustersAvailable), DWORD(TotalClusters));
end;
{$ENDIF}
{$ELSE}
function GetDiskInfo(Drive : AnsiChar; var ClustersAvailable, TotalClusters,
           BytesPerSector, SectorsPerCluster : Cardinal) : Boolean; assembler;
asm
  xor  dl,dl
  cmp  Drive,' '
  je   @@1
  mov  al,Drive
  call UpcasePrim
  mov  dl,40h
  sub  al,dl
  mov  dl,al
@@1:
  mov  ah,36h
  int  21h
  cmp  ax,0FFFFh
  je   @@Error

  les  di,SectorsPerCluster
  mov  es:[di],ax
  les  di,BytesPerSector
  mov  es:[di],cx
  les  di,TotalClusters
  mov  es:[di],dx
  les  di,ClustersAvailable
  mov  es:[di],bx
  mov  ax,1
  jmp  @@ExitPoint

@@Error:
  xor  ax,ax

@@ExitPoint:
end;
{$ENDIF}

function GetDiskSpace(Drive : AnsiChar;                    {new !!.05}
                  var UserSpaceAvail  : Comp;           {space available to user}
                  var TotalSpaceAvail : Comp;           {total space available}
                  var DiskSize        : Comp) : Boolean;{disk size}
{$IFDEF Win32}
type
  TGetDiskFreeSpace = function (Drive : PAnsiChar;
                            var UserFreeBytes : Comp;
                            var TotalBytes : Comp;
                            var TotalFreeBytes : Comp) : Bool; stdcall;
  LH = packed record L,H : word; end;
{$ENDIF}
var
  CA, TC, BPS, SPC : Cardinal;
  {$IFDEF Win32}
  VerInfo : TOSVersionInfo;
  LibHandle : THandle;
  GDFS : TGetDiskFreeSpace;
  Root : string;
  {$ENDIF}
begin
  Result := false;
  {$IFDEF Win32}
  {get the version info}
  VerInfo.dwOSVersionInfoSize := sizeof(VerInfo);
  if GetVersionEx(VerInfo) then begin
    with VerInfo do begin
      if ((dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and
          (LH(dwBuildNumber).L <> 1000)) or
         ((dwPlatformId = VER_PLATFORM_WIN32_NT) and
          (dwMajorVersion >= 4)) then begin
        LibHandle := LoadLibrary('KERNEL32.DLL');
        if (LibHandle <> 0) then begin
          @GDFS := GetProcAddress(LibHandle, 'GetDiskFreeSpaceExA');
          if Assigned(GDFS) then begin
            Root := AnsiChar(Upcase(Drive)) + ':\';
            if GDFS(PAnsiChar(Root), UserSpaceAvail, DiskSize, TotalSpaceAvail) then
              Result := true;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
  if not Result then begin
    if GetDiskInfo(Drive, CA, TC, BPS, SPC) then begin
      Result := true;
      DiskSize := BPS;
      DiskSize := DiskSize * SPC * TC;
      TotalSpaceAvail := BPS;
      TotalSpaceAvail := TotalSpaceAvail * SPC * CA;
      UserSpaceAvail := TotalSpaceAvail;
    end;
  end;
end;


{$IFDEF OS32}
 {$IFDEF VirtualPascal}
function GetDiskClass(Drive : AnsiChar) : DiskClass;
  {-Return the disk class for the drive with the specified letter}
{  DiskClass = (
  Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy, Bernoulli,
  HardDisk, RamDisk, SubstDrive, UnknownDisk, InvalidDrive,
  NovellDrive, CDRomDisk);}
Var
  DiskNum   : Word;
  InfoLevel : Word;
  rc        : Word;
  DiskSize  : Word;
  Buffer    : FSAllocate;
  DrvName   : String[3];
  Ordinal   : SmallWord;
  FSQb      : pFSQBuffer2;
  BufLen    : Longint;
  name      : pChar;

begin
  Drive := UpCase( Drive );
  DiskNum   := ord( Drive ) - ord( 'A' ) + 1;
  InfoLevel := 1;

  RC := DosQueryFSInfo( DiskNum, InfoLevel, Buffer, Sizeof( Buffer ) );
  If rc <> No_Error then
    GetDiskClass := InvalidDrive
  else
    begin
      GetDiskClass := UnknownDisk;

      BufLen := 100;
      GetMem( FSQb, BufLen );
      DrvName := Drive+':'#0;
      Ordinal := 0;
      DosQueryFSAttach( @DrvName[1], Ordinal, fsail_QueryName, FSqb, BufLen );
      With FsqB^ do
        begin
          Name := szName + cbName + 1;
          If ( strComp( Name, 'FAT' ) = 0 ) or
             ( strComp( Name, 'HPFS' ) = 0 ) then
            If Drive <= 'B' then
              begin
                DiskSize := ( Buffer.cbSector*Buffer.cUnit ) div 1024;
                if ( DiskSize > 340 ) and ( DiskSize < 370 ) then
                  getDiskClass := Floppy360
                else if ( DiskSize > 700 ) and ( DiskSize < 730 ) then
                  getDiskClass := Floppy720
                else if ( DiskSize > 1180 ) and ( DiskSize < 1210 ) then
                  getDiskClass := Floppy12
                else if ( DiskSize > 1410 ) and ( DiskSize < 1450 ) then
                  getDiskClass := Floppy144
                else
                  getDiskClass := OtherFloppy
              end
            else
              GetDiskClass := HardDisk
          else If StrComp( Name, 'NETWARE' ) = 0 then
            GetDiskClass := RemoteDrive
          else If StrComp( Name, 'CDFS' ) = 0 then
            GetDiskClass := CDRomDisk
          else If StrComp( Name, 'LAN' ) = 0 then
            GetDiskClass := RemoteDrive;
        end;
    end;
end;
 {$ELSE} // Win32 version
function GetDiskClass(Drive : AnsiChar) : DiskClass;
var
  Root : array[0..3] of AnsiChar;                                      {!!.02}
  Root2 : array[0..6] of AnsiChar;                                     {!!.02}
  ReturnedByteCount,
  SectorsPerCluster,
  BytesPerSector,
  NumberOfFreeClusters,
  TotalNumberOfClusters : DWORD;
  SupportedGeometry : array[1..20] of TDiskGeometry;
  HDevice : THandle;
  I : Integer;
  VerInfo : TOSVersionInfo;
  Found : Boolean;
begin
  Result := InvalidDrive;
  Found := False;
  StrCopy(Root, '%:\');                                                {!!.02}
  Root[0] := Drive;                                                    {!!.02}
  case GetDriveType(Root) of                                           {!!.02}
    0 : Result := UnknownDisk;
    1 : Result := InvalidDrive;
    DRIVE_REMOVABLE :
      begin
        GetVersionEx(VerInfo);
        if VerInfo.dwPlatformID = VER_PLATFORM_WIN32_NT then begin
          StrCopy(Root2, '\\.\%:');                                    {!!.02}
          Root2[4] := Drive;                                           {!!.02}
          HDevice := CreateFile(Root2, 0, FILE_SHARE_READ,             {!!.02}
            nil, OPEN_ALWAYS, 0, 0);
          if HDevice = INVALID_HANDLE_VALUE then Exit;
          if not DeviceIoControl(HDevice, IOCTL_DISK_GET_MEDIA_TYPES, nil, 0,
            @SupportedGeometry, SizeOf(SupportedGeometry), ReturnedByteCount, nil)
          then Exit;
          for I := 1 to (ReturnedByteCount div SizeOf(TDiskGeometry)) do begin
            case SupportedGeometry[I].MediaType of
              F5_1Pt2_512 : begin
                Result := Floppy12;
                Exit;
              end;
              F3_1Pt44_512 : begin
                Result := Floppy144;
                Exit;
              end;
              F3_720_512 : begin
                Result := Floppy720;
                Found := True;
              end;
              F5_360_512 : begin
                Result := Floppy360;
                Found := True;
              end;
            end;
          end;
          if Found then Exit;
          Result := OtherFloppy;
        end else begin
          GetDiskFreeSpace(Root, SectorsPerCluster, BytesPerSector,    {!!.02}
            NumberOfFreeClusters, TotalNumberOfClusters);
          case TotalNumberOfClusters of
             354 : Result := Floppy360;
             713,
            1422 : Result := Floppy720;
            2371 : Result := Floppy12;
            2847 : Result := Floppy144;
          else Result := OtherFloppy;
          end;
        end;
      end;
    DRIVE_FIXED : Result := HardDisk;
    DRIVE_REMOTE : Result := RemoteDrive;
    DRIVE_CDROM : Result := CDRomDisk;
    DRIVE_RAMDISK : Result := RamDisk;
  end;
end;
 {$ENDIF}
{$ELSE}
function GetDiskClass(Drive : AnsiChar) : DiskClass;
  {-Return the disk class for the drive with the specified letter}
  {-This routine uses two undocumented DOS functions ($32 and $52).
    Information about these functions was obtained from Terry Dettmann's DOS
    Programmer's Reference (Que, 1988) and Undocumented DOS (Addison-Wesley,
    1990).}
type
  NovellTable = array[1..32] of Byte;
  DriveParameterBlock =
    record
      DriveNum, DeviceDriverUnit : Byte;
      BytesPerSector : Word;
      SectorsPerCluster, ShiftFactor : Byte;
      BootSectors : Word;
      FatCopies : Byte;
      RootDirEntries, FirstDataSector, HighestCluster : Word;
      case Byte of
        0 : (SectorsPerFat23 : Byte;
             FirstDirSector23 : Word;
             DeviceDriver23 : Pointer;
             MediaDescriptor23 : Byte;
             AccessFlag23 : Byte;
             Next23 : Pointer;
             Reserved23 : LongInt);
        1 : (SectorsPerFat45 : Word;
             FirstDirSector45 : Word;
             DeviceDriver45 : Pointer;
             MediaDescriptor45 : Byte;
             AccessFlag45 : Byte;
             Next45 : Pointer;
             Reserved45 : LongInt);
    end;
var
  Ver, DN : Byte;
  Version : LongInt;
  MediaDescriptor : Byte;
  SectorsPerFat : Word;
  Found : Boolean;
  pDPBP, DPBP : ^DriveParameterBlock;
  NTP : ^NovellTable;
  Sele : Word;
  Continue, GotSelector : Boolean;
  CDRI : Boolean;
label
  WalkSFT, Evaluate, ExitPoint;

  function IsCD : Boolean;
  begin
    if CDRI then
      IsCD := IsDriveCDRom(Drive)
    else
      IsCD := False;
  end;

begin
  {assume failure}
  Result := InvalidDrive;
  GotSelector := False;

  {convert drive letter to drive number}
  Drive := Upcase(Drive);
  case Drive of
    'A'..'Z' : DN := Ord(Drive)-Ord('A');
    else Exit;
  end;
  CDRI := CDRomInstalled;
  {get DOS major version number}
  Ver := Hi(HiWord(GetVersion));
  asm
    mov   ah,32h
    mov   dl,DN
    inc   dl
    push  ds
    int   21h
    mov   dx,ds
    pop   ds
    cmp   al,0FFh
    je    WalkSFT

    mov   word ptr pDPBP,bx
    mov   word ptr pDPBP+2,dx
    jmp   Evaluate

  WalkSFT:
    mov   ah,52h
    int   21h
    mov   ax,es:[bx]
    mov   word ptr DPBP,ax
    mov   ax,es:[bx+2]
    mov   word ptr DPBP+2,ax
  end;
  if GetSelectorForRealMem(DPBP, $FFF0, Sele) = 0 then
    GotSelector := True
  else
    Exit;
  Continue := Word(DPBP) <> $FFFF;
  pDPBP := Ptr(Sele, 0);

  Found := False;
  while (Word(DPBP) <> $FFFF) and not Found do begin
    if pDPBP^.DriveNum = DN then
      Found := True
    else if Ver < 4 then
      Move(pDPBP^.Next23, DPBP, SizeOf(DPBP))
    else
      Move(pDPBP^.Next45, DPBP, SizeOf(DPBP));

    Continue := Word(DPBP) <> $FFFF;
    if (not Found) and Continue then begin
      if SetSegmentBaseAddr(Sele, Linear(DPBP)) <> 0 then
        goto ExitPoint;
    end;
  end;
  if not Found then begin
    if IsCD then begin    {moved up}
      Result := CDRomDisk;
      goto ExitPoint;
    end;
    asm
      mov   ax,0EF01h
      xor   si,si
      mov   es,si
      int   21h
      mov   word ptr NTP,si
      mov   word ptr NTP+2,es
    end;
    if NTP <> nil then
      if NTP^[DN+1] and $03 <> 0 then
        Result := RemoteDrive;
    goto ExitPoint;
  end;
Evaluate:
    with pDPBP^ do begin
      if Ver < 4 then begin
        MediaDescriptor := MediaDescriptor23;
        SectorsPerFat := SectorsPerFat23;
      end
      else begin
        MediaDescriptor := MediaDescriptor45;
        SectorsPerFat := SectorsPerFat45;
      end;

      if (FatCopies = 1) then
        { RAM disks have one copy of File Allocation Table }
        Result := RamDisk
      else if (MediaDescriptor = $F8) then
        { MediaDescriptor of $F8 indicates hard disk }
        Result := HardDisk
      else if (MediaDescriptor >= $F9) then
        { media descriptors >= $F9 are for floppy disks }
        case HighestCluster of
           355 : Result := Floppy360;
           714,
          1423 : Result := Floppy720;
          2372 : Result := Floppy12;
          else   Result := OtherFloppy;
        end
      else if (MediaDescriptor = $F0) and (HighestCluster = 2848) then
        { it's a 1.44 meg floppy }
        Result := Floppy144
      else if IsCD then
        Result := CDRomDisk
      else
        {unable to classify disk/drive}
        Result := UnknownDisk;
    end;
  ExitPoint:
    if GotSelector then
      if FreeLDTDescriptor(Sele) = 0 then ;
end;
{$ENDIF}

{$IFDEF OS32}
function FlushOsBuffers(Handle : Integer) : Boolean;
  {-Flush the OS's buffers for the specified file}
begin
{$IFDEF VirtualPascal}
  Result := DosResetBuffer( Handle ) = No_Error;
{$ELSE}
  Result := FlushFileBuffers(Handle);
{$ENDIF}
end;
{$ELSE}
function FlushOsBuffers(Handle : Integer) : Boolean; assembler;
  {-Flush OS's buffers for the specified file}
asm
  mov  ah,45h
  mov  bx,Handle
  int  21h             {DUP the handle}
  mov  dx,0            {assume failure}
  jc   @@ExitPoint

  mov  bx,ax
  mov  ah,3Eh
  int  21h             {close the DUP}
  jc   @@ExitPoint

  inc  dx              {indicate success}

@@ExitPoint:
  mov  ax,dx           {set function result}
end;
{$ENDIF}

{$IFDEF OS32}
{.$HINTS OFF}
function FileHandlesLeft(MaxHandles : Cardinal) : Cardinal;
  {-Returns the number of available file handles. In 32-bit, this can be a
    large number.  Use MaxHandles to limit the number of handles counted.
    The maximum is limited by HandleLimit - you can increase HandleLimit if
    you wish.  A temp file is required because Win95 seems to have some
    limit on the number of times you can open NUL.}
const
  HandleLimit = 1024;
type
  PHandleArray = ^THandleArray;
  THandleArray = array[0..Pred(HandleLimit)] of Integer;
var
  Handles : PHandleArray;
  MaxH, I : Integer;
{$IFDEF VirtualPascal}
  Action : LongInt;
{$ENDIF}
  TempPath, TempFile : PAnsiChar;
begin
  Result := 0;
  MaxH := MinLong(HandleLimit, MaxHandles);
  TempFile := nil;
  TempPath := nil;
  Handles := nil;
  try
{$IFDEF VirtualPascal}
    for I := 0 to Pred(MaxH) do begin
      if DosOpen('NUL', Handles^[i], Action, 0, 0,
           Open_Action_Create_If_New, 0, nil ) = no_error then
        Inc(Result)
      else
        Break;
    end;
{$ELSE}
    TempFile := StrAlloc(256);
    TempPath := StrAlloc(256);
    GetMem(Handles, MaxH * SizeOf(Integer));
    GetTempPath(255, TempPath);
    GetTempFileName(TempPath, 'ST', 0, TempFile);
    for I := 0 to Pred(MaxH) do begin
      Handles^[I] := CreateFile(TempFile, 0, FILE_SHARE_READ, nil,
        OPEN_EXISTING, FILE_FLAG_DELETE_ON_CLOSE, 0);
      if Handles^[I] <> INVALID_HANDLE_VALUE then Inc(Result) else Break;
    end;
{$ENDIF}
    for I := 0 to Pred(Result) do
      FileClose(Handles^[I]);
  finally
    if Assigned(Handles) then
      FreeMem(Handles, MaxH * SizeOf(Integer));
    StrDispose(TempFile);
    StrDispose(TempPath);
  end;
end;
{.$HINTS ON}
{$ELSE}
function FileHandlesLeft(MaxHandles : Cardinal) : Cardinal; assembler;
  {-Returns the number of available file handles}
const
  NullName : array[1..4] of AnsiChar = 'NUL'#0;
  HandleLimit = 255;
var
  Handles : array[1..HandleLimit] of Word;
  N : Byte;
asm
  mov   cx,MaxHandles    {CX = MaxHandles}
  cmp   cx,HandleLimit
  jle   @@1
  mov   cx,HandleLimit

@@1:
  mov   bl,cl            {Save MaxHandles or HandleLimit}
  lea   dx,NullName      {DS:DX => NullName}
  lea   si,Handles       {SI has offset for Handles[1]}

@@Next:
  mov   ax,3D02h         {DOS open file function}
  push  cx
  int   21h              {call DOS}
  pop   cx
  jc    @@Close          {start closing if CF set}

  mov   ss:[si],ax       {save the Handle}
  inc   si               {inc pointer into Handles}
  inc   si
  loop  @@Next           {repeat if CX > 0}

@@Close:
  sub   bl,cl
  mov   N,bl             {save handle count in N}
  jcxz  @@Done           {done if count is 0}

@@CloseOne:
  dec   si               {dec pointer into Handles}
  dec   si
  mov   bx,ss:[si]       {get the handle into BX}
  mov   ax,3E00h         {DOS close file function}
  push  cx
  int   21h              {call DOS, ignore error}
  pop   cx
  loop  @@CloseOne       {do it again}

@@Done:
  xor   ax,ax
  mov   al,N
end;
{$ENDIF}

{$IFNDEF OS32}
function FileHandlesOpen(CountDevices : Boolean) : Cardinal;
  {-Return the number of open files owned by a program}
type
  HandleTable = array[0..254] of Byte;
var
  P : Pointer;
  HandlesPtr : ^HandleTable;
  Sele : Word;
  I, N, Max : Byte;
begin
  Result := 0;
  {pointer to file handles table at PrefixSeg:$34}
  LongInt(P) := MemL[PrefixSeg:$34];
  if not GetSelectorForRealMem(P, $100, Sele) = 0 then
    Exit;
  HandlesPtr := Ptr(Sele, 0);

  {size of file handles table at PrefixSeg:$32}
  Max := Mem[PrefixSeg:$0032]-1;

  N := 0;
  for I := 0 to Max do
    if HandlesPtr^[I] <> $FF then
      case I of
        0..4 : Inc(N, Ord(CountDevices));
        else   Inc(N);
      end;
  if FreeLDTDescriptor(Sele) = 0 then ;
  Result := N;
end;
{$ENDIF}

function IsDirectory(const FName : string) : Boolean;
 {-Return true if FName is a directory}
var
  SLen : Cardinal;
  D, CurDir,
  CurDestDir : string;
  DiffDrive : Boolean;
begin
  Result := False;
  GetDir(0, CurDir);
  SLen := Length(FName);
  if SLen = 0 then Exit;
  if ((SLen > 3) or (FName[2] <> ':')) and (FName[SLen] = '\') then Exit;
  if (SLen >= 2) and (FName[2] = ':') and (FName[1] <> CurDir[1]) then
  begin
    {Checking on a different drive}
    DiffDrive := True;
    D := System.Copy(FName, 1, 2);
    try
      ChDir(D);
    except
      on EInOutError do Exit;
    end;
    GetDir(0, CurDestDir);
  end else
    DiffDrive := False;

  try
    ChDir(FName);
    Result := True;
  except
    on EInOutError do Result := False;
  end;

  if DiffDrive then
    ChDir(CurDestDir);

  ChDir(CurDir);
end;

function SameFile(const FilePath1, FilePath2 : string;
                  var ErrorCode : Integer) : Boolean;
  {-Return true if FilePath1 and FilePath2 refer to the same physical file.
    Error codes:
      0 - Success (no error)
      1 - Invalid FilePath1
      2 - Invalid FilePath2
      3 - Error on FileSetAttr/FileGetAttr }
var
  Attr1, Attr2, NewAttr : Integer;
begin
  Result := False;
  ErrorCode := 0;
  Attr1 := FileGetAttr(FilePath1);
  if Attr1 < 0 then begin
    ErrorCode := 1;
    Exit;
  end;
  Attr2 := FileGetAttr(FilePath2);
  if Attr2 < 0 then begin
    {leave ErrorCode at 0 if file not found but path is valid}
    if Attr2 <> -2 then
      ErrorCode := 2;
    Exit;
  end;
  if Attr1 <> Attr2 then
    Exit;
  if ((Attr1 and faArchive) = 0) then
    NewAttr := Attr1 or faArchive
  else
    NewAttr := Attr1 and (not faArchive);
  if FileSetAttr(FilePath1, NewAttr) <> 0 then begin
    ErrorCode := 3;
    Exit;
  end;
  Attr2 := FileGetAttr(FilePath2);
  if Attr2 < 0 then
    ErrorCode := 3;

  Result := (Attr2 = NewAttr) or (Attr2 = $80);
  { If the attribute is set to $00, Win32 automatically sets it to $80. }

  if FileSetAttr(FilePath1, Attr1) <> 0 then
    ErrorCode := 3;
end;

function CopyFile(const SrcPath, DestPath : string) : Cardinal;
  {-Copy the file specified by SrcPath into DestPath. DestPath must specify
    a complete filename, it may not be the name of a directory without the
    file portion.  This a low level routine, and the input pathnames are not
    checked for validity.}
const
  BufferSize = 4 * 1024;

var
  BytesRead, BytesWritten : Cardinal;
  FileDate : LongInt;
  Src, Dest, Mode, SaveFAttr : Integer;
  Buffer : Pointer;

begin
  Src := 0;
  Dest := 0;
  Buffer := nil;
  Result := 1;
  try
    GetMem(Buffer, BufferSize);
    Mode := FileMode and $F0;
    SaveFAttr := FileGetAttr(SrcPath);
    if SaveFAttr < 0 then begin
      Result := 1;
      Exit;
    end;
    Src := FileOpen(SrcPath, Mode);
    if Src < 0 then begin
      Result := 1;                     {unable to open SrcPath}
      Exit;
    end;
    Dest := FileCreate(DestPath);
    if Dest < 0 then begin
      Result := 2;                     {unable to open DestPath}
      Exit;
    end;
    repeat
      BytesRead := FileRead(Src, Buffer^, BufferSize);
      if (BytesRead = -1) then begin
        Result := 3;                   {error reading SrcPath}
        Exit;
      end;
      BytesWritten := FileWrite(Dest, Buffer^, BytesRead);
      if (BytesWritten = -1) or (BytesWritten <> BytesRead) then begin
        Result := 4;                   {error reading SrcPath}
        Exit;
      end;
    until BytesRead < BufferSize;
    FileDate := FileGetDate(Src);
    if FileDate = -1 then begin
      Result := 5;                     {error getting SrcPath's Date/Time}
      Exit;
    end;
    FileSetDate(Dest, FileDate);
    FileSetAttr(DestPath, SaveFAttr);
    Result := 0;
  finally
    if Assigned(Buffer) then
      FreeMem(Buffer, BufferSize);
    if Src > 0 then FileClose(Src);
    if Dest > 0 then begin
      FileClose(Dest);
      if Result <> 0 then SysUtils.DeleteFile(DestPath);
    end;
  end;
end;

{$IFDEF OS32}
 {$IFDEF VirtualPascal}
function ReadVolumeLabel(var VolName : string; Drive : AnsiChar) : Cardinal;
Var
  mID :MediaIDType;
begin
  Result := GetMediaID( Drive, mID );
  VolName := StrPas(mID.VolumeLabel);
end;
 {$ELSE}
function ReadVolumeLabel(var VolName : string; Drive : AnsiChar) : Cardinal;
var
  Root : string;
  Flags, MaxLength, NameSize : {Integer}DWord;
begin
  NameSize := 0;
  Root := Drive + ':\';
  if Length(VolName) < 12 then
    SetLength(VolName, 12);
  if GetVolumeInformation(PAnsiChar(Root), PAnsiChar(VolName), Length(VolName),
    nil, MaxLength, Flags, nil, NameSize)
  then
    Result := 0
  else
    Result := GetLastError;
end;
 {$ENDIF}
{$ELSE}
function ReadVolumeLabel(var VolName : string; Drive : AnsiChar) : Cardinal;
const
  AllMask = '%:\*.*';
var
  DosError : Integer;
  SearchRec : TSearchRec;
  Temp : string;
  Mask : string;
  P : Cardinal;
begin
  Mask := AllMask;
  Mask[1] := Drive;
  DosError := FindFirst(Mask, faVolumeID, SearchRec);
  if DosError = 18 then begin
    {no Volume ID found}
    Result := 0;
    VolName := '';
  end else begin
    Result := DosError;
    if DosError = 0 then begin
      VolName := SearchRec.Name;
      P := Pos('.', VolName);
      case P of
        0 : ;
        9 : System.Delete(VolName, P, 1);
        else begin
          Temp := System.Copy(VolName, Succ(P), 3);
          VolName := Format('%-8s', [System.Copy(VolName, 1, Pred(P))]) + Temp;
        end;
      end;
    end
    else
      VolName := '';
  end;
  FindClose(SearchRec);
end;
{$ENDIF}

{$IFDEF OS32}
{$IFDEF VirtualPascal}
function WriteVolumeLabel(const VolName : string; Drive : AnsiChar) : Cardinal;
Var
  rc          : ApiRet;
  DriveNumber : Word;
  Os2VolLabel  : String[12];

Begin
  DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;
  Os2VolLabel := VolName;
  Os2VolLabel[ Length( VolName )+1 ] := #0;
  rc := DosSetFSInfo( DriveNumber, fsil_VolSer, Os2VolLabel, Sizeof( Os2VolLabel ));
  If rc = No_Error then
    Result := 0
  else
    Result := rc;
end;
{$ELSE}
function WriteVolumeLabel(const VolName : string; Drive : AnsiChar) : Cardinal;
var
  Temp : string;
  Vol : array[0..11] of AnsiChar;                                      {!!.02}
  Root : array[0..3] of AnsiChar;                                      {!!.02}
begin
  Temp := VolName;
  StrCopy(Root, '%:\');                                                {!!.02}
  Root[0] := Drive;                                                    {!!.02}
  if Length(Temp) > 11 then
    SetLength(Temp, 11);
  StrPCopy(Vol, Temp);
  if Windows.SetVolumeLabel(Root, Vol) then                            {!!.02}
    Result := 0
  else Result := GetLastError;
end;
{$ENDIF}
{$ELSE}
function WriteVolumeLabel(const VolName : string; Drive : AnsiChar) : Cardinal;
var
  XFCB : XFCBRec;
  DTA : DTABuf;
  SaveDTA : Pointer;
  MediaID : MediaIDType;
  DriveNo : Byte;
  VName : array[0..10] of AnsiChar;
  MediaBlockExists : Boolean;
  ErrorCode : Byte;
begin
  SaveDTA := GetDTA;
  SetDTA(DTA);
  FillChar(XFCB, SizeOf(XFCB), 0);
  FillChar(XFCB.FileSpec, 11, '?');
  XFCB.Flag := $FF;
  XFCB.AttrByte := 8;
  DriveNo := Ord(UpCase(Drive)) - (Ord('A') - 1);
  XFCB.DriveCode := DriveNo;
  MediaBlockExists := (GetMediaID(Drive, MediaID) = 0);
  FillChar(VName, SizeOf(VName), ' ');
  Move(VolName[1], VName, MinWord(Length(VolName), SizeOf(VName)));
  asm
    push ds
    mov  ax,ss
    mov  ds,ax
    mov  es,ax
    lea  dx,XFCB
    mov  ah,11h
    int  21h
    or   al,al
    jnz  @@NoLabel

    lea  di,DTA
    mov  dx,di
    add  di,18h
    cld
    mov  cx,ss
    mov  ds,cx
    lea  si,VName
    mov  cx,11
    rep  movsb
    mov  ax,ss
    mov  ds,ax
    mov  ah,17h
    int  21h
    mov  ErrorCode,al
    jmp  @@ExitPoint

  @@NoLabel:
    lea  di,XFCB
    push ss
    push di
    add  di,8
    mov  cx,ss
    mov  ds,cx
    mov  es,cx
    lea  si,VName
    cld
    mov  cx,11
    rep  movsb
    call DosFCBCreate
    mov  ErrorCode,al
    cmp  al,0
    ja   @@ExitPoint

    lea  di,XFCB
    push ss
    push di
    call DosFCBClose
    mov  ErrorCode,al

  @@ExitPoint:
    pop  ds
  end;
  if MediaBlockExists and (ErrorCode = 0) then begin
    Move(VName[0], MediaID.VolumeLabel, SizeOf(MediaID.VolumeLabel));
    ErrorCode := SetMediaID(Drive, MediaID);
  end;
  SetDTA(SaveDTA^);
  Result := ErrorCode;
end;
{$ENDIF}

{$IFDEF OS32}
{$IFDEF VirtualPascal}
function DeleteVolumeLabel(Drive : AnsiChar) : Cardinal;
Var
  rc          : ApiRet;
  DriveNumber : Word;
  Buf         : Record
    SerialNum : Word;
    VolLabel  : String[12];
  end;

Begin
  DeleteVolumeLabel := 8;
  DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;
  fillchar( Buf, Sizeof( Buf ), #0 );
  rc := DosQueryFSInfo( DriveNumber, fsil_VolSer, Buf, Sizeof( Buf ));

  If rc = No_Error then
    begin
      fillchar( Buf.VolLabel, Sizeof( Buf.VolLabel ), 0 );
      rc := DosSetFSInfo( DriveNumber, fsil_VolSer, Buf.VolLabel, Sizeof( Buf.VolLabel ));
      If rc = No_Error then
        DeleteVolumeLabel := 0
    end;
end;
{$ELSE}
function DeleteVolumeLabel(Drive : AnsiChar) : Cardinal;               {!!.02}
var
  Root : array[0..3] of AnsiChar;
begin
  StrCopy(Root, '%:\');                                                {!!.02}
  Root[0] := Drive;                                                    {!!.02}
  if Windows.SetVolumeLabel(Root, '') then                             {!!.02}
    Result := 0
  else Result := GetLastError;
end;
{$ENDIF}
{$ELSE}
function DeleteVolumeLabel(Drive : AnsiChar) : Cardinal;
var
  XFCB : XFCBRec;
  DTA  : DTABuf;
  MediaID : MediaIDType;
  SaveDTA : Pointer;
  ErrorCode : Byte;
  DriveNo : Byte;
  MediaBlockExists : Boolean;
  VolName: array[0..10] of AnsiChar;
begin
  SaveDTA := GetDTA;
  SetDTA(DTA);
  FillChar(VolName, SizeOf(VolName), ' ');
  VolName[10] := #0;
  FillChar(XFCB, SizeOf(XFCB), 0);
  FillChar(XFCB.FileSpec, 11, '?');
  XFCB.Flag := $FF;
  XFCB.AttrByte := faVolumeID;
  DriveNo := Ord(Upcase(Drive)) - (Ord('A') - 1);
  XFCB.DriveCode := DriveNo;
  MediaBlockExists := (GetMediaID(Drive, MediaID) = 0);
  asm
    push     ds
    lea      dx,XFCB
    mov      ax,ss
    mov      ds,ax
    mov      ah,13h
    int      21h
    pop      ds
    mov      ErrorCode,al
  end;
  if MediaBlockExists and (ErrorCode = 0) then begin
    Move(VolName[1], MediaID.VolumeLabel, SizeOf(MediaID.VolumeLabel));
    if SetMediaID(Drive, MediaID) = 0 then ;
  end;
  SetDTA(SaveDTA^);
  Result := ErrorCode;
end;
{$ENDIF}

{$IFDEF OS32}   {!! Does not work for NT}
{$IFDEF VirtualPascal}
function GetMediaID(Drive : AnsiChar;
                    var MediaIDRec : MediaIDType) : cardinal;
Var
  rc          : ApiRet;
  DriveNumber : Word;
  Buf         : Record
    SerialNum : Word;
    VolLabel  : String;
  end;

Begin
  DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;

  rc := DosQueryFSInfo( DriveNumber, fsil_VolSer, Buf, Sizeof( Buf ));
  If rc = No_Error then
    begin
      FillChar( MediaIDRec, Sizeof( MediaIDRec ), 0 );
      MediaIDRec.SerialNumber := Buf.SerialNum;
      Move( Buf.VolLabel[1], MediaIDRec.VolumeLabel, Length( Buf.VolLabel ));

{       Note: FileSystemID uninitialized }
      MediaIDRec.InfoLevel := fsil_VolSer;
      GetMediaID := 0;
    end
  else
    GetMediaID := rc;
end;
{$ELSE}
function GetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
var
  PMid : PMediaIDType;
  Regs : DevIOCtlRegisters;
  CB   : DWord;
  HDevice : THandle;
  SA   : TSecurityAttributes;
begin
  PMid := @MediaIDRec;
  with SA do begin
    nLength := SizeOf(SA);
    lpSecurityDescriptor := nil;
    bInheritHandle := True;
  end;
  with Regs do begin
    reg_EAX := $440D;
    reg_EBX := Ord(UpCase(Drive)) - (Ord('A') - 1);
    reg_ECX := $0866;
    reg_EDX := LongInt(PMid);
  end;
  HDevice := CreateFile('\\.\vwin32', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    Pointer(@SA), OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if HDevice <> INVALID_HANDLE_VALUE then begin
    if DeviceIOControl(HDevice, VWIN32_DIOC_DOS_IOCTL, Pointer(@Regs), SizeOf(Regs),
      Pointer(@Regs), SizeOf(Regs), CB, nil)
    then
      Result := 0
    else
      Result := GetLastError;
    CloseHandle(HDevice);
  end else
    Result := GetLastError;
end;
{$ENDIF}
{$ELSE}
function GetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
type
  DoubleWord = record LoWord, HiWord : word; end;
var
  L : LongInt;
  RP, PP : ^MediaIDType;
  Regs : DPMIRegisters;
begin
  L := GlobalDosAlloc(SizeOf(MediaIDType));
  if L = 0 then begin
    Result := ecOutOfMemory;
    Exit;
  end;
  RP := Ptr(DoubleWord(L).HiWord, 0);
  PP := Ptr(DoubleWord(L).LoWord, 0);
  FillChar(Regs, SizeOf(Regs), 0);
  with Regs do begin
    DS := OS(RP).S;
    DX := OS(RP).O;
    AX := $440D;
    BX := Ord(UpCase(Drive)) - (Ord('A') - 1);
    CX := $0866;
    Flags := GetCPUFlags;
  end;
  SimulateRealModeInt($21, Regs);
  if Odd(Regs.Flags) then
    Result := Regs.AX
  else begin
    MediaIDRec := PP^;
    Result := 0;
  end;
  GlobalDosFree(OS(PP).S);
end;
{$ENDIF}

{$IFDEF OS32}   {!! Does not work for NT}
{$IFDEF VirtualPascal}
function SetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
Var
  rc          : ApiRet;
  DriveNumber : Word;
  Buf         : Record
    SerialNum : Word;
    VolLabel  : String[12];
  end;

Begin
  DriveNumber := Ord( Drive ) - Ord( 'A' ) + 1;
  fillchar( Buf, Sizeof( Buf ), #0 );
  Buf.SerialNum := MediaIDRec.SerialNumber;
  Buf.VolLabel  := MediaIDRec.VolumeLabel;
  Buf.VolLabel[ Length( Buf.VolLabel)+1 ] := #0;

  rc := DosSetFSInfo( DriveNumber, fsil_VolSer, Buf.VolLabel, Sizeof( Buf.VolLabel ));
  If rc = No_Error then
    SetMediaID := 0
  else
    SetMediaID := rc;
end;
{$ELSE}
function SetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
var
  PMid : PMediaIDType;
  Regs : DevIOCtlRegisters;
  CB   : DWord;
  HDevice : THandle;
  SA   : TSecurityAttributes;
begin
  PMid := @MediaIDRec;
  with SA do begin
    nLength := SizeOf(SA);
    lpSecurityDescriptor := nil;
    bInheritHandle := True;
  end;
  with Regs do begin
    reg_EAX := $440D;
    reg_EBX := Ord(UpCase(Drive)) - (Ord('A') - 1);
    reg_ECX := $0846;
    reg_EDX := LongInt(PMid);
  end;
  HDevice := CreateFile('\\.\vwin32', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    Pointer(@SA), OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if HDevice <> INVALID_HANDLE_VALUE then begin
    if DeviceIOControl(HDevice, VWIN32_DIOC_DOS_IOCTL, Pointer(@Regs), SizeOf(Regs),
      Pointer(@Regs), SizeOf(Regs), CB, nil)
    then
      Result := 0
    else
      Result := GetLastError;
    CloseHandle(HDevice);
  end else
    Result := GetLastError;
end;
{$ENDIF}
{$ELSE}
function SetMediaID(Drive : AnsiChar; var MediaIDRec : MediaIDType) : Cardinal;
type
  DoubleWord = record LoWord, HiWord : word; end;
var
  L : LongInt;
  RP, PP : ^MediaIDType;
  Regs : DPMIRegisters;
begin
  L := GlobalDosAlloc(SizeOf(MediaIDType));
  if L = 0 then begin
    Result := ecOutOfMemory;
    Exit;
  end;
  RP := Ptr(DoubleWord(L).HiWord, 0);
  PP := Ptr(DoubleWord(L).LoWord, 0);
  PP^ := MediaIDRec;
  FillChar(Regs, SizeOf(Regs), 0);
  with Regs do begin
    DS := OS(RP).S;
    DX := OS(RP).O;
    AX := $440D;
    BX := Ord(UpCase(Drive)) - (Ord('A') - 1);
    CX := $0846;
    Flags := GetCPUFlags;
  end;
  SimulateRealModeInt($21, Regs);
  if Odd(Regs.Flags) then
    Result := Regs.AX
  else
    Result := 0;
  GlobalDosFree(OS(PP).S);
end;
{$ENDIF}

{$IFDEF OS32}
{$IFDEF VirtualPascal}
function ValidDrive(Drive : AnsiChar) : Boolean;
var
  CurDrive   : Longint;
  Drives     : Set of 'A'..'Z';
  DrivesWord : Longint absolute Drives;
begin
  DosQueryCurrentDisk( CurDrive, DrivesWord );
  DrivesWord := DrivesWord shl 1;
  Result := Drive in Drives;
end;
{$ELSE}
function ValidDrive(Drive : AnsiChar) : Boolean;
var
  DriveBits : LongInt;
  DriveLtr : AnsiChar;
begin
  DriveLtr := UpCase(Drive);
  DriveBits := GetLogicalDrives shr (Ord(DriveLtr)-Ord('A'));
  Result := LongFlagIsSet(DriveBits, $00000001);
end;
{$ENDIF}
{$ELSE}
function ValidDrive(Drive : AnsiChar) : Boolean;
var
  CurDrive : AnsiChar;
begin
  Drive := UpCase(Drive);
  CurDrive := DefaultDrive;
  SelectDrive(Drive);
  Result := (DefaultDrive = Drive);
  SelectDrive(CurDrive);
end;
{$ENDIF}


function SignL(L : LongInt) : Integer;                                 {!!.01 Added}
  {-return sign of LongInt value}
begin
  if L < 0 then
    Result := -1
  else if L = 0 then
    Result := 0
  else
    Result := 1;
end;

function SignF(F : Extended) : Integer;                                {!!.01 Added}
  {-return sign of floating point value}
begin
  if F < 0 then
    Result := -1
  else if F = 0 then
    Result := 0
  else
    Result := 1;
end;

function MidWord(W1, W2, W3 : Word) : Word;                            {!!.01 Added}
  {return the middle of three Word values}
begin
  Result := StUtils.MinWord(StUtils.MinWord(StUtils.MaxWord(W1, W2),
                            StUtils.MaxWord(W2, W3)), StUtils.MaxWord(W1, W3));
end;

function MidLong(L1, L2, L3 : LongInt) : LongInt;                      {!!.01 Added}
  {return the middle of three LongInt values}
begin
  Result := StUtils.MinLong(StUtils.MinLong(StUtils.MaxLong(L1, L2),
                            StUtils.MaxLong(L2, L3)), StUtils.MaxLong(L1, L3));
end;

function MidFloat(F1, F2, F3 : Extended) : Extended;                   {!!.01 Added}
  {return the middle of three floating point values}
begin
  Result := MinFloat(MinFloat(MaxFloat(F1, F2), MaxFloat(F2, F3)), MaxFloat(F1, F3));
end;

function MinFloat(F1, F2 : Extended) : Extended;                       {!!.01 Added}
  {-return the lesser of two floating point values}
begin
  if F1 <= F2 then
    Result := F1
  else
    Result := F2;
end;

function MaxFloat(F1, F2 : Extended) : Extended;                       {!!.01 Added}
  {-return the greater of two floating point values}
begin
  if F1 > F2 then
    Result := F1
  else
    Result := F2;
end;

function IsDirectoryEmpty(const S : string) : Integer;                 {!!.01 Added}
  {-checks if there are any entries in the directory}
var
  SR : TSearchRec;
  R  : Integer;
begin
  Result := 1;
  if IsDirectory(S) then
  begin
{$IFDEF Win32}
 {$IFOPT H+}
    AddBackSlashL(S);
 {$ELSE}
    AddBackSlashS(S);
 {$ENDIF}
{$ELSE}
    AddBackSlashS(S);
{$ENDIF}
    R := FindFirst(S + '*.*', faAnyFile, SR);
    if R <> -18 then
    begin
      if (R = 0) then
      repeat
        if (SR.Attr and faDirectory = faDirectory) then
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            Result := 0;
            break;
          end;
        end else
        begin
          Result := 0;
          break;
        end;
        R := FindNext(SR);
      until R = -18;
    end;
    {$IFDEF WIN32}
    FindClose(SR);
    {$ENDIF}
  end else
    Result := -1;
end;

function IsFileHidden(const S : string) : Integer;                     {!!.01 Added}
  {-checks if file's hidden attribute is set}
begin
  if FileExists(S) then
    Result := Integer((FileGetAttr(S) and faHidden) = faHidden)
  else
    Result := -1;
end;

function IsFileSystem(const S : string) : Integer;                     {!!.01 Added}
  {-checks if file's system attribute is set}
begin
  if FileExists(S) then
    Result := Integer((FileGetAttr(S) and faSysFile) = faSysFile)
  else
    Result := -1;
end;

function IsFileReadOnly(const S : string) : Integer;                   {!!.01 Added}
  {-checks if file's readonly attribute is set}
begin
  if FileExists(S) then
    Result := Integer((FileGetAttr(S) and faReadOnly) = faReadOnly)
  else
    Result := -1;
end;

function IsFileArchive(const S : string) : Integer;                    {!!.01 Added}
  {-checks if file's archive attribute is set}
begin
  if FileExists(S) then
    Result := Integer((FileGetAttr(S) and faArchive) = faArchive)
  else
    Result := -1;
end;

procedure EnumerateFiles(StartDir : string;                            {!!.01 Added}
                         FL : TStrings;
                         SubDirs : Boolean;
                         IncludeItem : TIncludeItemFunc);

    procedure SearchBranch;
    var
      SR    : TSearchRec;
      Error : SmallInt;
      Dir   : string;
    begin
      Error := FindFirst('*.*', faAnyFile, SR);                        {!!.02}
      GetDir(0, Dir);
      if Dir[Length(Dir)] <> '\' then
        Dir := Dir + '\';
      while Error = 0 do
      begin
        try
          if (@IncludeItem = nil) or (IncludeItem(SR)) then
            FL.Add(Dir + SR.Name);
        except
          on EOutOfMemory do
          begin
            raise EOutOfMemory.Create('String list is full');
          end;
        end;
        Error := FindNext(SR);
      end;
      {$IFDEF WIN32}
      FindClose(SR);
      {$ENDIF}

      if SubDirs then
         begin
         Error := FindFirst('*.*', faDirectory, SR);
         while Error = 0 do
           begin
           if ((SR.Attr and faDirectory = faDirectory) and
               (SR.Name <> '.') and (SR.Name <> '..')) then
              begin
              ChDir(SR.Name);
              SearchBranch();
              ChDir('..');
              end;
           Error := FindNext(SR);
           end;
         {$IFDEF WIN32}
         FindClose(SR);
         {$ENDIF}
         end;
    end;

var
  OrgDir : string;

begin
  if IsDirectory(StartDir) then
     begin
     GetDir(0, OrgDir);
     try
       ChDir(StartDir);
       SearchBranch;
       finally
       ChDir(OrgDir);
       end;
     end
  else
    raise Exception.Create('Invalid starting directory');
end;


procedure EnumerateDirectories(StartDir : string; FL : TStrings;       {!!.01 Added}
                               SubDirs : Boolean;
                               IncludeItem : TIncludeItemFunc);

    procedure SearchBranch;
    var
      SR    : TSearchRec;
      Error : SmallInt;
      Dir   : string;
    begin
      Error := FindFirst('*.*', faDirectory, SR);
      GetDir(0, Dir);
      if Dir[Length(Dir)] <> '\' then
        Dir := Dir + '\';
      while Error = 0 do
      begin
        try
          if (@IncludeItem = nil) or (IncludeItem(SR)) then
          begin
            if (SR.Attr and faDirectory = faDirectory) and
               (SR.Name <> '.') and (SR.Name <> '..') then
              FL.Add(Dir + SR.Name);
          end;
        except
          on EOutOfMemory do
          begin
            raise EOutOfMemory.Create('String list is full');
          end;
        end;
        Error := FindNext(SR);
      end;
      {$IFDEF WIN32}
      FindClose(SR);
      {$ENDIF}

      if SubDirs then
      begin
        Error := FindFirst('*.*', faDirectory, SR);
        while Error = 0 do
        begin
          if ((SR.Attr and faDirectory = faDirectory) and
              (SR.Name <> '.') and (SR.Name <> '..')) then
          begin
            if (@IncludeItem = nil) or (IncludeItem(SR)) then
            begin
              ChDir(SR.Name);
              SearchBranch;
              ChDir('..');
            end;
          end;
          Error := FindNext(SR);
        end;
        {$IFDEF WIN32}
        FindClose(SR);
        {$ENDIF}
      end;
    end;

var
  OrgDir : string;

begin
  if IsDirectory(StartDir) then
  begin
    GetDir(0, OrgDir);
    try
      ChDir(StartDir);
      SearchBranch;
    finally
      ChDir(OrgDir);
    end;
  end else
    raise Exception.Create('Invalid starting directory');
end;

function FileTimeToStDateTime(FileTime : LongInt) : TStDateTimeRec;    {!!.01 Added}
var
  DDT : TDateTime;
begin
  DDT := FileDateToDateTime(FileTime);
  Result.D := DateTimeToStDate(DDT);
  Result.T := DateTimeToStTime(DDT);
end;

function StDateTimeToFileTime(FileTime : TStDateTimeRec) : LongInt;    {!!.01 Added}
var
  DDT : TDateTime;
begin
  DDT := Int(StDateToDateTime(FileTime.D)) + Frac(StTimeToDateTime(FileTime.T));
  Result := DateTimeToFileDate(DDT);
end;

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.
