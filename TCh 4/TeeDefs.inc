{ Options }
{$DEFINE NOUSE_BDE} { <-- remove the dot to avoid using BDE (in QRChart,etc) }

{ TeeChart Pro 4.0 & TeeTree 1.0 Include file: TeeDefs.Inc }
{$IFNDEF WIN32}
  {$DEFINE D1}
{$ELSE}
  {$H+}
  {$IF Defined(VER125) or Defined(VER130) or Defined(VER140) or Defined(VER150)}
    {$DEFINE D3}
    {$DEFINE D5}
    {$DEFINE C4}
  {$IFEND}
  {$IFDEF VER120}
    {$DEFINE D3}
    {$DEFINE D5}                                          
    {$DEFINE C3D5}
  {$ENDIF}
  {$IFDEF VER110}
    {$DEFINE D3}
    {$IFDEF BCB}
      {$DEFINE C3}
      {$DEFINE C3D5}
      {$ObjExportAll On}
    {$ELSE}
      {$DEFINE D5}
      {$DEFINE C3D5}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF VER100}
    {$DEFINE D3}
  {$ENDIF}
  {$IFDEF VER90}
    {$DEFINE D2}
    {$DEFINE D2C1}
  {$ENDIF}
  {$IFDEF VER93}
    {$DEFINE C1}
    {$DEFINE D2C1}
  {$ENDIF}
{$ENDIF}

{$IFNDEF D1}
{$IFNDEF D2}
{$IFNDEF C1}
{$DEFINE TEEHELPEDITOR}   { <-- Show Help at TeeChart editor dialog }
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ internal }
{.$DEFINE TEEOCX}

{.$DEFINE COMPOSOURCE}
{.$DEFINE TEETRIAL}
{.$DEFINE TEE5}

{$IFDEF TEEOCX}
{$DEFINE TEEJPEG}
{$ENDIF}

{$IFDEF D3}
{$DEFINE TEENOREGAX}
{$ENDIF}
