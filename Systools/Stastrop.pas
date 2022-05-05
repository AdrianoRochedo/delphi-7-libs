{*********************************************************}
{*                    STASTROP.PAS 1.05                  *}
{*              Planetary Astronomical Routines          *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit Stastrop;

interface

type
  TStPlanetsRec = record
    RA,
    DC,
    Elong  : Double;
  end;
  TStPlanetsArray = array[1..7] of TStPlanetsRec;

const
  radcor  = 57.29577951308232;    {number of degrees in a radian}
  corfac  = 2 * Pi;               {number of radians in a circle}

function PlanetsPos(JD : Double) : TStPlanetsArray;


implementation


var
  Cent1, Cent2, Cent3,
  XCord, YCord, ZCord,
  SolVec,
  SunX, SunY, SunZ,
  OBliquity,
  Longitude, Latitude,
  EAnom,
  TrueAnom,
  TrueLong,
  LongMer, AxisMer, EcMer,
  IncMer, PerMer, NodeMer, AnomMer,
  LongVen, AxisVen, EcVen,
  IncVen, PerVen, NodeVen, AnomVen,
  LongMar, AxisMar, EcMar,
  IncMar, PerMar, NodeMar, AnomMar,
  LongJup, AxisJup, EcJup,
  IncJup, PerJup, NodeJup, AnomJup,
  LongSat, AxisSat, EcSat,
  IncSat, PerSat, NodeSat, AnomSat,
  LongUra, AxisUra, EcUra,
  IncUra, PerUra, NodeUra, AnoMura,
  LongNep, AxisNep, EcNep,
  IncNep, PerNep, NodeNep, AnomNep,
  XEth, YEth, ZEth,
  M1, M2, M3, M4,
  M5, M6, M7, M8,
  SunsRA, SunsDec, SunsElong          : Double;



function Invsin(Value : Double): Double;
begin
  if abs(abs(Value) - 1.0) > 5.0E-12 then
    Result := arctan(Value/sqrt(-Value * Value + 1.0))
  else begin
    if Value < 0 then
      Result := 3.0 * Pi / 2.0
    else
      Result := Pi / 2.0;
  end;
end;

function Invcos(Value : Double): Double;
begin
  if abs(abs(Value) - 1.0) > 5.0E-12 then
    Result := -arctan(Value/sqrt(-Value * Value + 1.0)) + 90.0 / radcor
  else begin
    if Value - Pi/2.0 > 0 then
       Result := 0.0
    else
      Result := Pi;
  end;
end;


function RealAngle(Value2, Value1, Start : Double): Double;
begin
  Result := Start;

  if (Value1 = 0) then
  begin
    if Value2 > 0 then
      Result := Pi / 2.0
    else
      Result := 3.0 * Pi / 2.0;
  end else
  begin
    if (Value2 > 0.0) then
    begin
      if (Value1 < 0.0) then
        Result := Start + Pi
      else
        Result := Start;
    end else
    begin
      if (Value2 = 0) then
      begin
        if Value1 > 0 then
          Result := 0
        else
          Result := Pi;
      end else
      begin
        if (Value2 < 0) then
        begin
          if (Value1 < 0) then
            Result := Start + Pi
          else
            Result := Start + (2.0 * Pi)
        end;
      end;
    end;
  end;
end;


procedure Kepler(MeanAnom, Ectry : Double; var TrueAnom, EAnom : Double);
var
  var1,
  var2 : Double;
begin
  var1 := MeanAnom;
  repeat
    var2 := (MeanAnom + Ectry*sin(var1) - var1)/(1.0 - Ectry*cos(var1));
    var1 := var1 + var2;
  until (abs(var2) < 1.0E-09);
  EAnom := var1;
  TrueAnom := sqrt((1.0 + Ectry)/(1.0 - Ectry))*sin(EAnom/2.0)/cos(EAnom/2.0);
  TrueAnom := 2.0 * arctan(TrueAnom);
  if (TrueAnom < 0.0) then
    TrueAnom := TrueAnom + 360.0/radcor;
end;


procedure conversion (Longitude, Latitude, RadiusVector : Double;
                      var X, Y, Z : Double);
var
  var1, var2, var3 : Double;
begin
  var1 := RadiusVector * cos(Longitude) * cos(Latitude);
  var2 := RadiusVector * sin(Longitude) * cos(Latitude);
  var3 := RadiusVector * sin(Latitude);
  X := var1;
  Y := var2 * cos(Obliquity) - var3 * sin(Obliquity);
  Z := var2 * sin(Obliquity) + var3 * cos(Obliquity);
end;


procedure ecliptic (ArgLat, Incl, Node : Double;
                    var Longitude, Latitude : Double);
var
  var1, var2, var3 : Double;
begin
  var1 := cos(Incl) * sin(ArgLat);
  var2 := cos(ArgLat);
  var3 := arctan(var1/var2);
  if (var1 > 0.0) then
  begin
    if (var2 < 0.0) then
      var3 := var3 + PI
    else
      var3 := var3;
  end else
  begin
    if (var1 < 0.0) then
      if (var2 < 0.0) then
        var3 := var3 + PI
      else
        var3 := var3 + 2.0*PI;
  end;
   Longitude := var3 + Node;
   var1 := sin(ArgLat) * sin(Incl);
   Latitude := arctan(var1/sqrt(-var1*var1 + 1.0));
end;


procedure SunOfDate(JD : Double);
var
  SunLong,
  cor1, cor2,
  cor3, cor4,
  cor5,
  corLong,
  SunAnom,
  EthEc,
  JulCent,
  JulDays,
  Jul2, Jul3     : Double;

begin
  JulCent := (JD - 2415020.0)/36525.0;
  JulDays := (JD -2415020.0);
  Jul2 := sqr(JulCent);
  Jul3 := Jul2 * JulCent;
  Obliquity := 23.45229444 - 1.30125E-02*JulCent - 1.638888E-06*Jul2 + 5.027778E-07*Jul3;
  Obliquity := Obliquity/radcor;

  SunLong := 279.696677777778 + 0.9856473354*JulDays + 3.0250E-04*Jul2;
  SunLong := Frac((SunLong)/360.0) * corfac;

  cor1 := (153.23+22518.7541*JulCent) / radcor;
  cor2 := (216.57+45037.5082*JulCent) / radcor;
  cor3 := (312.69+32964.3577*JulCent) / radcor;
  cor4 := (350.74+445267.1142*JulCent-0.00144*sqr(JulCent)) / radcor;
  cor5 := (231.19+20.20*JulCent) / radcor;

  corLong := 0.00134*cos(cor1) + 0.00154*cos(cor2) + 0.002*cos(cor3);
  corLong := corLong + 0.00179*sin(cor4) + 0.00178*sin(cor5);
  corLong := corLong / radcor;
  SunLong := SunLong + corLong;

  SunAnom := 358.475830 + 0.9856002669*JulDays - 1.50E-04*Jul2 - 3.0E-06*Jul3;
  SunAnom := Frac((SunAnom)/360.0) * corfac;
  EthEc := 0.016751040 - 4.180E-05*JulCent - 1.260E-07*Jul2;

  Kepler(SunAnom, EthEc, TrueAnom, EAnom);
  SolVec := 1.0000002 * (1.0 - EthEc * cos(EAnom));

  TrueLong := SunLong + TrueAnom - SunAnom;

  SunX := SolVec * cos(TrueLong);
  SunY := SolVec * sin(TrueLong) * cos(Obliquity);
  SunZ := SolVec * sin(TrueLong) * sin(Obliquity);
end;


procedure RADec(XPlt,YPlt,ZPlt : Double; var R,D,A  : Double);
var
  var1,
  var2,
  var3,
  var4,
  var5       : Double;
begin
  var1 := XEth + XPlt;
  var2 := YEth + YPlt;
  var3 := ZEth + ZPlt;

  var4 := ARCTAN(var2/var1);
  var4 := RealAngle(var2,var1,var4) * radcor;

  var5 := sqrt(sqr(var1) + sqr(var2) + sqr(var3));
  var3 := Invsin(var3/var5) * radcor;

  R := var4;
  D := var3;

  var4 := R / radcor;
  var3 := D / radcor;

  var1 := sin(SunsDec/radcor) * sin(var3)
        + cos(SunsDec/radcor) * cos(var3) * cos(SunsRA/radcor - var4);
  A := Invcos(var1) * radcor;
end;


procedure Rotation(JD : Double; var RA, DC : Double);
var
  AH,
  DH,
  A,B,C,
  var1,
  Eta,
  Theta,
  Zeta,
  Time1,
  Time2,
  C2,
  C3      : Double;
begin
  Time1 := (JD - 2415020.3135) / 36524.2199;
  Time2 := (2451545.0 - JD) / 36524.2199;
  C2 := sqr(Time2);
  C3 := C2 * Time2;

  Eta := (2304.250 + 1.39656*Time1) * Time2
       + 0.30188 *C2
       + 0.017998 * C3;

  Zeta := Eta
        + 0.791 * C2
        + 0.001 * C3;
  Eta := Eta / 3600 / radcor;
  Zeta := Zeta / 3600 / radcor;

  Theta := (2004.682 - 0.85330*Time1) * Time2
         - 0.426650 * C2
         - 0.041833 * C3;
  Theta := Theta / 3600 / radcor;

  AH := RA/radcor;
  DH := DC/radcor;

  A := cos(DH) * sin(AH + Zeta);
  B := cos(Theta) * cos(DH) * cos(AH+Zeta) - sin(Theta) * sin(DH);
  C := sin(Theta) * cos(DH) * cos(AH+Zeta) + cos(Theta) * sin(DH);

  var1 := ArcTan(A/B);
  var1 := RealAngle(A,B,var1);
  RA := (var1 + Eta) * radcor;

  DC := Invsin(C) * radcor;
end;



procedure MeanElements;
begin
{MERCURY}

  LongMer := Frac((178.179078 + 149474.07078*Cent1 + 3.011E-04*Cent2)
           / 360.0) * corfac;
  AxisMer := 0.3870986;
  EcMer   :=  0.20561421 + 2.0460E-05 * Cent1 - 3.00E-08 * Cent2;
  IncMer  := (7.00288100 + 1.8608E-03 * Cent1 - 1.83E-05 * Cent2)
           / radcor;
  PerMer  := (28.753753 + 0.3702806 * Cent1 + 1.208E-04 * Cent2)
           / radcor;
  NodeMer := (47.145944 + 1.1852083 * Cent1 + 1.739E-04 * Cent2)
           / radcor;
  AnomMer := Frac((102.27938 + 149472.51529 * Cent1 + 7.0E-06 * Cent2)
           / 360.0) * corfac;

{VENUS}

  LongVen := Frac((342.767053 + 58519.21191 * Cent1 + 3.097E-04 * Cent2)
           / 360.0) * corfac;
  AxisVen := 0.7233316;
  EcVen   := 0.00682069 - 4.774E-05 * Cent1 + 9.1E-08 * Cent2;
  IncVen  := (3.393631 + 1.0058E-03 * Cent1 - 1.0E-06 * Cent2)
           / radcor;
  PerVen  := (54.384186 + 0.5081861 * Cent1 - 1.3864E-03 * Cent2)
           / radcor;
  NodeVen := (75.779647 + 0.899850 * Cent1 + 4.1E-04 * Cent2)
           / radcor;
  AnomVen := Frac((212.60322 + 58517.80387 * Cent1 + 1.286E-03 * Cent2)
           / 360.0) * corfac;

{MARS}

  LongMar := Frac((293.737334 + 19141.69551 * Cent1 + 3.107E-04 * Cent2)
           / 360.0) * corfac;
  AxisMar := 1.5236900;
  EcMar   := 0.09331290 + 9.2064E-05 * Cent1 - 7.7E-08 * Cent2;
  IncMar  := (1.850333 - 6.75E-04 * Cent1 + 1.26E-05 * Cent2)
           / radcor;
  PerMar  := (285.431761 + 1.0697667 * Cent1 + 1.313E-04 * Cent2
           + 4.14E-06 * Cent3) / radcor;
  NodeMar := (48.786442 + 0.7709917 * Cent1 - 1.4E-06 * Cent2
           - 5.33E-06 * Cent3) / radcor;
  AnomMar := Frac((319.51913 + 19139.85475 * Cent1 + 1.81E-04 * Cent2)
           / 360.0) * corfac;


{JUPITER}

  LongJup := Frac((238.049257 + 3036.301986 * Cent1 + 3.347E-04 * Cent2
           - 1.65E-06 * Cent3) / 360.0) * corfac;
  AxisJup := 5.202561;
  EcJup   := 0.04833475 - 1.6418E-04 * Cent1 - 4.676E-07 * Cent2
           - 1.7E-09 * Cent3;
  IncJup  := (1.308736 - 5.6961E-03 * Cent1 + 3.9E-06 * Cent2)
           / radcor;
  PerJup  := (273.277558 + 0.5994317 * Cent1 + 7.0405E-04 * Cent2
           + 5.08E-06 * Cent3) / radcor;
  NodeJup := (99.443414 + 1.01053 * Cent1 + 3.5222E-04 * Cent2
           - 8.51E-06 * Cent3) / radcor;
  AnomJup := Frac((225.32833 + 3034.69202 * Cent1 - 7.22E-04 * Cent2)
           / 360.0) * corfac;

{SATURN}

   LongSat := Frac((266.564377 + 1223.509884 * Cent1 + 3.245E-04 * Cent2
            - 5.8E-06 * Cent3) / 360.0) * corfac;
   AxisSat := 9.554747;
   EcSat   := 0.05589232 - 3.455E-04 * Cent1 - 7.28E-07 * Cent2
            + 7.4E-10 * Cent3;
   IncSat  := (2.492519 - 3.9189E-03 * Cent1 - 1.549E-05 * Cent2
            + 4.0E-08 * Cent3) / radcor;
   PerSat  := (338.3078 + 1.0852207 * Cent1 + 9.7854E-04 * Cent2
            + 9.92E-06 * Cent3) / radcor;
   NodeSat := (112.790414 + 0.8731951 * Cent1 - 1.5218E-04 * Cent2
            - 5.31E-06 * Cent3) / radcor;
   AnomSat := Frac((175.46622 + 1221.55147 * Cent1 - 5.02E-04 * Cent2)
            / 360.0) * corfac;

{URANUS}

   LongUra := Frac((244.19747 + 429.863546 * Cent1 + 3.16E-04 * Cent2
            - 6.0E-07 * Cent3) / 360.0) * corfac;
   AxisUra := 19.21814;
   EcUra   := 0.0463444 - 2.658E-05 * Cent1 + 7.7E-08 * Cent2;
   IncUra  := (0.772464 + 6.253E-04 * Cent1 + 3.95E-05 * Cent2)
            / radcor;
   PerUra  := (98.071581 + 0.985765 * Cent1 - 1.0745E-03 * Cent2
            - 6.1E-07 * Cent3) / radcor;
   NodeUra := (73.477111 + 0.4986678 * Cent1 + 1.3117E-03 * Cent2)
            / radcor;
   AnoMura := Frac((72.64878 + 428.37911 * Cent1 + 7.9E-05 * Cent2)
            / 360.0) * corfac;

{NEPTUNE}

   LongNep := Frac((84.457994 + 219.885914 * Cent1 + 3.205E-04 * Cent2
            - 6.0E-07 * Cent3) / 360.0) * corfac;
   AxisNep := 30.10957;
   EcNep   := 8.99704E-03 + 6.330E-06 * Cent1 - 2.0E-09 * Cent2;
   IncNep  := (1.779242 - 9.5436E-03 * Cent1 - 9.1E-06 * Cent2)
            / radcor;
   PerNep  := (276.045975 + 0.3256394 * Cent1 + 1.4095E-04 * Cent2
            + 4.113E-06 * Cent3) / radcor;
   NodeNep := (130.681389 + 1.098935 * Cent1 + 2.4987E-04 * Cent2
            - 4.718E-06 * Cent3) / radcor;
   AnomNep := Frac((37.73063 + 218.46134 * Cent1 - 7.0E-05 * Cent2)
            / 360.0) * corfac;
 end;


 function MercuryPerturbations(JD : Double) : TStPlanetsRec;
 var
   RadiusVector,
   ArgLat,
   Cor1           : Double;

 begin
   Kepler(AnomMer, EcMer, TrueAnom, EAnom);

   RadiusVector := AxisMer * (1.0 - EcMer * cos(EAnom));

   Cor1 := 7.525E-06 * cos(2*M5 - M1 + 53.013/radcor);
   Cor1 := Cor1 + 6.802E-06 * cos(5*M2 - 3*M1 - 259.918/radcor);
   Cor1 := Cor1 + 5.457E-06 * cos(2*(M2-M1) - 71.188/radcor);
   Cor1 := Cor1 + 3.569E-06 * cos(5*M2 - M1 - 77.75/radcor);

   RadiusVector := RadiusVector + Cor1;

   Cor1 := 2.04E-03 * cos(5*M2 - 2*M1 + 12.22/radcor);
   Cor1 := Cor1 + 1.03E-03 * cos(2*M2 - M1 - 160.692/radcor);
   Cor1 := Cor1 + 9.1E-04 * cos(2*M5 - M1 - 37.003/radcor);
   Cor1 := Cor1 + 7.8E-04 * cos(5*M2 - 3*M1 + 10.137/radcor);
   LongMer := LongMer + Cor1/radcor;

   ArgLat := LongMer + TrueAnom - AnomMer - NodeMer;
   if (ArgLat <0.0) then
     ArgLat := ArgLat + 360.0/radcor;

   Ecliptic(ArgLat, IncMer, NodeMer, Longitude, Latitude);

   Conversion(Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

   RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
   Rotation(JD, Result.RA, Result.DC);
 end;


 function VenusPerturbations(JD : Double) : TStPlanetsRec;
 var
   Cor1,
   RadiusVector,
   ArgLat          : Double;

 begin
   Cor1 := 7.7E-04 * sin((237.24 + 150.27*Cent1)/radcor);
   Cor1 := Cor1/radcor;

   LongVen := LongVen + Cor1;
   AnomVen := AnomVen + Cor1;

   Kepler(AnomVen, EcVen, TrueAnom, EAnom);

   RadiusVector := AxisVen * (1.0 - EcVen * cos(EAnom));

   Cor1 := 2.2501E-05 * cos(2*(M3-M2) - 58.208/radcor);
   Cor1 := Cor1 + 1.9045E-05 * cos(3*(M3-M2) + 92.577/radcor);
   Cor1 := Cor1 + 6.887E-06 * cos(M5 - M2 - 118.09/radcor);
   Cor1 := Cor1 + 5.172E-06 * cos(M3 - M2 - 29.11/radcor);

   Cor1 := Cor1 + 3.620E-06 * cos(5*M3 - 4*M2 - 104.208/radcor);
   Cor1 := Cor1 + 3.283E-06 * cos(4*(M3-M2) + 63.513/radcor);
   Cor1 := Cor1 + 3.074E-06 * cos(2*(M5-M2) - 55.167/radcor);

   RadiusVector := RadiusVector + Cor1;

   Cor1 := 3.13E-03 * cos(2*(M3-M2) - 148.225/radcor);
   Cor1 := Cor1 + 1.98E-03 * cos(3*(M3-M2) + 2.565/radcor);
   Cor1 := Cor1 + 1.36E-03 * cos(M3 - M2 - 119.107/radcor);
   Cor1 := Cor1 + 9.60E-04 * cos(3*M3 - 2*M2 - 135.912/radcor);
   Cor1 := Cor1 + 8.20E-04 * cos(M5 - M2 - 208.087/radcor);
   LongVen := LongVen + Cor1/radcor;

   ArgLat := LongVen + TrueAnom - AnomVen - NodeVen;
   if (ArgLat < 0.0) then
     ArgLat := ArgLat + 360.0/radcor;

   Ecliptic(ArgLat, IncVen, NodeVen, Longitude, Latitude);

   Conversion(Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

   RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
   Rotation(JD, Result.RA, Result.DC);
 end;


 function MarsPerturbations(JD : Double) : TStPlanetsRec;
 var
   Cor1,
   RadiusVector,
   ArgLat         : Double;

 begin
   Cor1 := -0.01133 * sin(3*M5 - 8*M4 + 4*M3);
   Cor1 := Cor1 - 0.00933 * cos(3*M5 - 8*M4 + 4*M3);
   Cor1 := Cor1/radcor;

   LongMar := LongMar + Cor1;
   AnomMar := AnomMar + Cor1;

   Kepler(AnomMar, EcMar, TrueAnom, EAnom);

   RadiusVector := AxisMar * (1.0 - EcMar * cos(EAnom));

   Cor1 := 5.3227E-05 * cos(M5 - M4 + 41.1306/radcor);
   Cor1 := Cor1 + 5.0989E-05 * cos(2*(M5-M4) - 101.9847/radcor);
   Cor1 := Cor1 + 3.8278E-05 * cos(2*M5 - M4 - 98.3292/radcor);
   Cor1 := Cor1 + 1.5996E-05 * cos(M3 - M4 - 55.555/radcor);
   Cor1 := Cor1 + 1.4764E-05 * cos(2*M3 - 3*M4 + 68.622/radcor);

   Cor1 := Cor1 + 8.9660E-06 * cos(M5 - 2*M4 + 43.615/radcor);
   Cor1 := Cor1 + 7.9140E-06 * cos(3*M5 - 2*M4 - 139.737/radcor);
   Cor1 := Cor1 + 7.0040E-06 * cos(2*M5 - 3*M4 - 102.888/radcor);
   Cor1 := Cor1 + 6.6200E-06 * cos(M3 - 2*M4 + 113.202/radcor);
   Cor1 := Cor1 + 4.9300E-06 * cos(3*(M5-M4) - 76.243/radcor);

   Cor1 := Cor1 + 4.6930E-06 * cos(3*M3 - 5*M4 + 190.603/radcor);
   Cor1 := Cor1 + 4.5710E-06 * cos(2*M3 - 4*M4 + 244.702/radcor);
   Cor1 := Cor1 + 4.4090E-06 * cos(3*M5 - M4 - 115.828/radcor);

   RadiusVector := RadiusVector + Cor1;

   Cor1 := 7.05E-03 * cos(M5 - M4 - 48.958/radcor);
   Cor1 := Cor1 + 6.07E-03 * cos(2*M5 - M4 - 188.350/radcor);
   Cor1 := Cor1 + 4.45E-03 * cos(2*(M5-M4) - 191.897/radcor);
   Cor1 := Cor1 + 3.88E-03 * cos(M3 - 2*M4 + 20.495/radcor);

   Cor1 := Cor1 + 2.38E-03 * cos(M3 - M4 + 35.097/radcor);
   Cor1 := Cor1 + 2.04E-03 * cos(2*M3 - 3*M4 + 158.638/radcor);
   Cor1 := Cor1 + 1.77E-03 * cos(3*M4 - M2 - 57.602/radcor);
   Cor1 := Cor1 + 1.36E-03 * cos(2*M3 - 4*M4 + 154.093/radcor);
   Cor1 := Cor1 + 1.05E-03 * cos(M5 + 17.618/radcor);

   Longitude := Longitude + Cor1/radcor;

   ArgLat := LongMar + TrueAnom - AnomMar - NodeMar;
   if (ArgLat < 0.0) then
     ArgLat := ArgLat + 360.0/radcor;

   Ecliptic(ArgLat, IncMar, NodeMar, Longitude, Latitude);

   Conversion (Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

   RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
   Rotation(JD, Result.RA, Result.DC);
end;


function JupiterPerturbations(JD : Double) : TStPlanetsRec;
var
  Mu,
  P, Q, S,
  V, W, Z,
  Cor1, cor2,
  RadiusVector,
  ArgLat          : Double;

begin
  Mu := Cent1/5.0 + 0.10;
  P := Frac((237.47555 + 3034.9061*Cent1)/360.0)*corfac;
  Q := Frac((265.91650 + 1222.1139*Cent1)/360.0)*corfac;
  S := Frac((243.51721 + 428.46770*Cent1)/360.0)*corfac;
  V := 5*Q - 2*P;
  W := 2*P - 6*Q + 3*S;
  Z := Q - P;
{  Psi := S - Q;}


  {PERTURBATIONS IN THE MEAN LONGITUDE}

  Cor1 := (0.331364 - 0.010281*Mu - 0.004692*sqr(Mu)) * sin(V);
  Cor1 := Cor1 + (0.003228 - 0.064436*Mu + 0.002075*sqr(Mu)) * cos(V);
  Cor1 := Cor1 - (0.003083 + 0.000275*Mu - 0.000489*sqr(Mu)) * sin(2*V);
  Cor1 := Cor1 + 0.002472 * sin(W) + 0.013619 * sin(Z);
  Cor1 := Cor1 + 0.018472 * sin(2*Z) + 0.006717 * sin(3*Z);

  Cor1 := Cor1 + 0.002775 * sin(4*Z);
  Cor1 := Cor1 + (0.007275 - 0.001253*Mu) * sin(Z) * sin(Q);
  Cor1 := Cor1 + 0.006417 * sin(2*Z) * sin(Q);
  Cor1 := Cor1 + 0.002439 * sin(3*Z) * sin(Q);
  Cor1 := Cor1 - (0.033839 + 0.001125*Mu) * cos(Z) * sin(Q);

  Cor1 := Cor1 - 0.003767 * cos(2*Z) * sin(Q);
  Cor1 := Cor1 - (0.035681 + 0.001208*Mu) * sin(Z) * cos(Q);
  Cor1 := Cor1 - 0.004261 * sin(2*Z) * cos(Q);
  Cor1 := Cor1 + 0.002178 * cos(Q);
  Cor1 := Cor1 + (-0.006333 + 0.001161*Mu) * cos(Z) * cos(Q);

  Cor1 := Cor1 - 0.006675 * cos(2*Z) * cos(Q);
  Cor1 := Cor1 - 0.002664 * cos(3*Z) * cos(Q);
  Cor1 := Cor1 - 0.002572 * cos(Z) * sin(2*Q);
  Cor1 := Cor1 - 0.003567 * sin(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 0.002094 * cos(Z) * cos(2*Q);

  Cor1 := Cor1 + 0.003342 * cos(2*Z) * cos(2*Q);
  Cor1 := Cor1/radcor;

  LongJup := LongJup + Cor1;


  {PERTURBATIONS IN THE PERIHELION}

  cor2 := (0.007192 - 0.003147*Mu) * sin(v);
  cor2 := cor2 + (-0.020428 - 0.000675*Mu + 0.000197*sqr(Mu)) * cos(V);
  cor2 := cor2 + (0.007269 + 0.000672*Mu) * sin(Z) * sin(Q);
  cor2 := cor2 - 0.004344 * sin(Q) + 0.034036 * cos(Z) * sin(Q);
  cor2 := cor2 + 0.005614 * cos(2*Z) * sin(Q);

  cor2 := cor2 + 0.002964 * cos(3*Z) * sin(Q);
  cor2 := cor2 + 0.037761 * sin(Z) * cos(Q);
  cor2 := cor2 + 0.006158 * sin(2*Z) * cos(Q);
  cor2 := cor2 - 0.006603 * cos(Z) * cos(Q);
  cor2 := cor2 - 0.005356 * sin(Z) * sin(2*Q);

  cor2 := cor2 + 0.002722 * sin(2*Z) * sin(2*Q);
  cor2 := cor2 + 0.004483 * cos(Z) * sin(2*Q);
  cor2 := cor2 - 0.002642 * cos(2*Z) * sin(2*Q);
  cor2 := cor2 + 0.004403 * sin(Z) * cos(2*Q);
  cor2 := cor2 - 0.002536 * sin(2*Z) * cos(2*Q);

  cor2 := cor2 + 0.005547 * cos(Z) * cos(2*Q);
  cor2 := cor2 - 0.002689 * cos(2*Z) * cos(2*Q);
  cor2 := cor2/radcor;

  PerJup := PerJup + cor2;

  AnomJup := AnomJup + (Cor1 - cor2/EcJup);


  {PERTURBATIONS IN THE ECCENTRICITY}

  Cor1 := (3606 + 130*Mu - 43 * sqr(Mu)) * sin(V);
  Cor1 := Cor1 + (1289 - 580*Mu) * cos(V);
  Cor1 := Cor1 - 6764 * sin(Z) * sin(Q) - 1110 * sin(2*Z) * sin(Q);
  Cor1 := Cor1 - 224 * sin(3*Z) * sin(Q) - 204 * sin(Q);
  Cor1 := Cor1 + (1284 + 116*Mu) * cos(Z) * sin(Q);

  Cor1 := Cor1 + 188 * cos(2*Z) * sin(Q);
  Cor1 := Cor1 + (1460 + 130*Mu) * sin(Z) * sin(Q);
  Cor1 := Cor1 + 224 * sin(2*Z) * cos(Q) - 817 * cos(Q);
  Cor1 := Cor1 + 6074 * cos(Z) * cos(Q) + 992 * cos(2*Z) * cos(Q);
  Cor1 := Cor1 + 508 * cos(3*Z) * cos(Q) + 230 * cos(4*Z) * cos(Q);

  Cor1 := Cor1 + 108 * cos(5*Z) * cos(Q);
  Cor1 := Cor1 - (956 + 73*Mu) * sin(Z) * sin(2*Q);
  Cor1 := Cor1 + 448 * sin(2*Z) * sin(2*Q) + 137 * sin(3*Z) * sin(2*Q);
  Cor1 := Cor1 + (-997 + 108*Mu) * cos(Z) * sin(2*Q);
  Cor1 := Cor1 + 480 * cos(2*Z) * sin(2*Q) + 148 * cos(3*Z) * sin(2*Q);

  Cor1 := Cor1 + (-956 + 99*Mu) * sin(Z) * cos(2*Q);
  Cor1 := Cor1 + 490 * sin(2*Z) * cos(2*Q) + 158 * sin(3*Z) * cos(2*Q);
  Cor1 := Cor1 + 179 * cos(2*Q) + (1024 + 75*Mu) * cos(Z) * cos(2*Q);
  Cor1 := Cor1 - 437 * cos(2*Z) * cos(2*Q) -132 * cos(3*Z) * cos(2*Q);
  Cor1 := Cor1 * 1.0E-07;

  EcJup := EcJup + Cor1;


  {PERTURBATIONS IN THE SEMI-MAJOR AXIS}

  Cor1 := -263 * cos(V) + 205 * cos(Z) + 693 * cos(2*Z);
  Cor1 := Cor1 + 312 * cos(3*Z) + 147 * cos(4*Z) + 299 * sin(Z) * sin(Q);
  Cor1 := Cor1 + 181 * cos(2*Z) * sin(Q) + 204 * sin(2*Z) * cos(Q);
  Cor1 := Cor1 + 111 * sin(3*Z) * cos(Q) - 337 * cos(Z) * cos(Q);
  Cor1 := Cor1 - 111 * cos(2*Z) * cos(Q);
  Cor1 := Cor1 * 1.0E-06;

  AxisJup := AxisJup + Cor1;

  Kepler(AnomJup, EcJup, TrueAnom, EAnom);

  RadiusVector := AxisJup * (1.0 - EcJup * cos(EAnom));

  ArgLat := LongJup + TrueAnom - AnomJup - NodeJup;
  if (ArgLat < 0.0) then
    ArgLat := ArgLat + 360.0/radcor;

  Ecliptic(ArgLat, IncJup, NodeJup, Longitude, Latitude);

  Conversion(Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

  RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
  Rotation(JD, Result.RA, Result.DC);
end;


function SaturnPerturbations(JD : Double) : TStPlanetsRec;
var
  Mu,
  P, Q, S,
  V, W, Z,
  Psi,
  Cor1, cor2,
  RadiusVector,
  ArgLat          : Double;

begin
  Mu := Cent1/5.0 + 0.10;
  P := Frac((237.47555 + 3034.9061*Cent1)/360.0)*corfac;
  Q := Frac((265.91650 + 1222.1139*Cent1)/360.0)*corfac;
  S := Frac((243.51721 + 428.46770*Cent1)/360.0)*corfac;
  V := 5*Q - 2*P;
  W := 2*P - 6*Q + 3*S;
  Z := Q - P;
  Psi := S - Q;

  Cor1 := (-0.814181 + 0.01815*Mu + 0.016714*sqr(Mu)) * sin(V);
  Cor1 := Cor1 + (-0.010497 + 0.160906*Mu - 0.0041*sqr(Mu)) * cos(V);
  Cor1 := Cor1 + 0.007581 * sin(2*V) - 0.007986 * sin(W)
        - 0.148811 * sin(Z);
  Cor1 := Cor1 - 0.040786 * sin(2*Z) - 0.015208 * sin(3*Z);

  Cor1 := Cor1 - 0.006339 * sin(4*Z) - 0.006244 * sin(Q);
  Cor1 := Cor1 + (0.008931 + 0.002728*Mu) * sin(Z) * sin(Q);
  Cor1 := Cor1 - 0.0165 * sin(2*Z) * sin(Q)
        - 0.005775 * sin(3*Z) * sin(Q);
  Cor1 := Cor1 + (0.081344 + 0.003206*Mu) * cos(Z) * sin(Q);

  Cor1 := Cor1 + 0.015019 * cos(2*Z) * sin(Q);
  Cor1 := Cor1 + (0.085581 + 0.002494*Mu) * sin(Z) * cos(Q);
  Cor1 := Cor1 + (0.025328 - 0.003117*Mu) * cos(Z) * cos(Q);
  Cor1 := Cor1 + 0.014394 * cos(2*Z) * cos(Q)
        + 0.006319 * cos(3*Z) * cos(Q);

  Cor1 := Cor1 + 0.006369 * sin(z) * sin(2*Q)
        + 0.009156 * sin(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 0.007525 * sin(3*Psi) * sin(2*Q);
  Cor1 := Cor1 - 0.005236 * cos(Z) * cos(2*Q)
        - 0.007736 * cos(2*Z) * cos(2*Q);
  Cor1 := Cor1 - 0.007528 * cos(3*Psi) * cos(2*Q);
  Cor1 := Cor1/radcor;

  LongSat := LongSat + Cor1;


  {PERTURBATIONS IN THE PERIHELION}

  cor2 := (0.077108 + 0.007186*Mu - 0.001533*sqr(Mu)) * sin(V);
  cor2 := cor2 + (0.045803 - 0.014766*Mu - 0.000536*sqr(Mu)) * cos(V);
  cor2 := cor2 - 0.007075 * sin(Z) - 0.075825 * sin(Z) * sin(Q);
  cor2 := cor2 - 0.024839 * sin(2*Z) * sin(Q);
  cor2 := cor2 - 0.008631 * sin(3*Z) * sin(Q);

  cor2 := cor2 - 0.072586 * cos(Q) - 0.150383 * cos(Z) * cos(Q);
  cor2 := cor2 + 0.026897 * cos(2*Z) * cos(Q);
  cor2 := cor2 + 0.010053 * cos(3*Z) * cos(Q);
  cor2 := cor2 - (0.013597 + 0.001719*Mu) *sin(Z) * sin(2*Q);
  cor2 := cor2 + (-0.007742 + 0.001517*Mu) * cos(Z) * sin(2*Q);

  cor2 := cor2 + (0.013586 - 0.001375*Mu) * cos(2*Z) * sin(2*Q);
  cor2 := cor2 + (-0.013667 + 0.001239*Mu) * sin(Z) * cos(2*Q);
  cor2 := cor2 + 0.011981 * sin(2*Z) * cos(2*Q);
  cor2 := cor2 + (0.014861 + 0.001136*Mu) * cos(Z) * cos(2*Q);
  cor2 := cor2 - (0.013064 + 0.001628*Mu) * cos(2*Z) * cos(2*Q);
  cor2 := cor2/radcor;

  PerSat := PerSat + cor2;

  AnomSat := AnomSat + (Cor1 - cor2/EcSat);


  {PERTURBATIONS TO THE ECCENTRICITY}

  Cor1 := (-7927 + 2548*Mu + 91*sqr(Mu)) * sin(V);
  Cor1 := Cor1 + (13381 + 1226*Mu - 253*sqr(Mu)) * cos(V);
  Cor1 := Cor1 + (248 - 121*Mu) * sin(2*V);
  Cor1 := Cor1 - (305 + 91*Mu) * cos(2*V);
  Cor1 := Cor1 + 412 * sin(2*Z) + 12415 * sin(Q);

  Cor1 := Cor1 + (390 - 617*Mu) * sin(Z) * sin(Q);
  Cor1 := Cor1 + (165 - 204*Mu) * sin(2*Z) * sin(Q);
  Cor1 := Cor1 + 26599 * cos(Z) * sin(Q);
  Cor1 := Cor1 - 4687 * cos(2*Z) * sin(q);
  Cor1 := Cor1 - 1870 * cos(3*Z) * sin(q);

  Cor1 := Cor1 - 821 * cos(4*Z) * sin(Q);
  Cor1 := Cor1 - 377 * cos(5*Z) * sin(Q);
  Cor1 := Cor1 + 497 * cos(2*Psi) * sin(Q);
  Cor1 := Cor1 + (163 - 611*Mu) * cos(Q);

  Cor1 := Cor1 - 12696 * sin(Z) * cos(Q) - 4200 * sin(2*Z) * cos(Q);
  Cor1 := Cor1 - 1503 * sin(3*Z) * cos(Q) - 619 * sin(4*Z) * cos(Q);
  Cor1 := Cor1 - 268 * sin(5*Z) * cos(Q);
  Cor1 := Cor1 - (282 + 1306*Mu) * cos(Z) * cos(Q);

  Cor1 := Cor1 + (-86 + 230*Mu) * cos(2*Z) * cos(Q);
  Cor1 := Cor1 + 461 * sin(2*Psi) * cos(Q) - 350 * sin(2*Q);
  Cor1 := Cor1 + (2211 - 286*Mu) * sin(Z) * sin(2*Q);
  Cor1 := Cor1 - 2208 * sin(2*Z) * sin(2*Q) - 568 * sin(3*Z) * sin(2*Q);

  Cor1 := Cor1 -346 * sin(4*Z) * sin(2*Q);
  Cor1 := Cor1 - (2780 + 222*Mu) * cos(Z) * sin(2*Q);
  Cor1 := Cor1 + (2022 + 263*Mu) * cos(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 248 * cos(3*Z) * sin(2*Q);

  Cor1 := Cor1 + 242 * sin(3*Psi) * sin(2*Q) + 467 * cos(3*Psi) * sin(2*Q);
  Cor1 := Cor1 - 490 * cos(2*Q) - (2842 + 279*Mu) * sin(Z) * cos(2*Q);
  Cor1 := Cor1 + (128 + 226*Mu) * sin(2*Z) * cos(2*Q);
  Cor1 := Cor1 + 224 * sin(3*Z) * cos(2*Q);

  Cor1 := Cor1 + (-1594 + 282*Mu) * cos(Z) * cos(2*Q);
  Cor1 := Cor1 + (2162 - 207*Mu) * cos(2*Z) * cos(2*Q);
  Cor1 := Cor1 + 561 * cos(3*Z) * cos(2*Q) + 343 * cos(4*Z) * cos(2*Q);
  Cor1 := Cor1 + 469 * sin(3*Psi) * cos(2*Q) - 242 * cos(3*Psi) * cos(2*Q);

  Cor1 := Cor1 - 205 * sin(Z) * sin(3*Q) + 262 * sin(3*Z) * sin(3*Q);
  Cor1 := Cor1 + 208 * cos(Z) * cos(3*Q) - 271 * cos(3*Z) * cos(3*Q);
  Cor1 := Cor1 - 382 * cos(3*Z) * sin(4*Q) - 376 * sin(3*Z) * cos(4*Q);
  Cor1 := Cor1 * 1.0E-07;

  EcSat := EcSat + Cor1;


  {PERTURBATIONS IN THE SEMI-MAJOR AXIS}

  Cor1 := 572*Mu * sin(V) + 2933 * cos(V) + 33629.0 * cos(Z)
        - 3081 * cos(2*Z);
  Cor1 := Cor1 - 1423 * cos(3*Z) - 671 * cos(4*Z) - 320 * cos(5*Z);
  Cor1 := Cor1 + 1098 * sin(Q) - 2812 * sin(Z) * sin(Q)
        + 688 * sin(2*Z) * sin(Q);
  Cor1 := Cor1 - 393 * sin(3*Z) * sin(Q) - 228 * sin(4*Z) * sin(Q);
  Cor1 := Cor1 + 2138 * cos(Z) * sin(Q) - 999 * cos(2*Z) * sin(Q);

  Cor1 := Cor1 - 642 * cos(3*Z) * sin(Q) - 325 * cos(4*Z) * sin(Q);
  Cor1 := Cor1 - 890 * cos(Q) + 2206 * sin(Z) * cos(Q);
  Cor1 := Cor1 - 1590 * sin(2*Z) * cos(Q) - 647 * sin(3*Z) * cos(Q);
  Cor1 := Cor1 - 344 * sin(4*Z) * cos(Q) + 2885 * cos(Z) * cos(Q);
  Cor1 := Cor1 + (2172 + 102*Mu) * cos(2*Z) * cos(Q);

  Cor1 := Cor1 + 296 * cos(3*Z) * cos(Q) - 267 * sin(2*Z) * sin(2*Q);
  Cor1 := Cor1 - 778 * cos(Z) * sin(2*Q) + 495 * cos(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 250 * cos(3*Z) * sin(2*Q) - 856 * sin(Z) * cos(2*Q);
  Cor1 := Cor1 + 441 * sin(2*Z) * cos(2*Q) + 296 * cos(2*Z) * cos(2*Q);
  Cor1 := Cor1 + 211 * cos(3*Z) * cos(2*Q) - 427 * sin(Z) * sin(3*Q);

  Cor1 := Cor1 + 398 * sin(3*Z) * sin(3*Q) + 344 * cos(Z) * cos(3*Q);
  Cor1 := Cor1 - 427 * cos(3*Z) * cos(3*Q);
  Cor1 := Cor1 * 1.0E-06;

  AxisSat := AxisSat + Cor1;

  Kepler(AnomSat, EcSat, TrueAnom, EAnom);

  RadiusVector := AxisSat * (1.0 - EcSat * cos(EAnom));

  ArgLat := LongSat + TrueAnom - AnomSat - NodeSat;
  if (ArgLat < 0.0) then
    ArgLat := ArgLat + 360.0/radcor;

  Ecliptic(ArgLat, IncSat, NodeSat, Longitude, Latitude);

  Cor1 := 0.000747 * cos(Z) * sin(Q) + 0.001069 * cos(Z) * cos(Q);
  Cor1 := Cor1 + 0.002108 * sin(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 0.001261 * cos(2*Z) * sin(2*Q);
  Cor1 := Cor1 + 0.001236 * sin(2*Z) * cos(2*Q);
  Cor1 := Cor1 - 0.002075 * cos(2*Z) * cos(2*Q);

  Latitude := Latitude + Cor1/radcor;

  Conversion (Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

  RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
  Rotation(JD, Result.RA, Result.DC);
end;


function UranusPerturbations(JD : Double) : TStPlanetsRec;
var
  Mu,
  P, Q, S,
  W,
  G, H,
  A, B, C,
  Cor1, cor2,
  RadiusVector,
  ArgLat         : Double;

begin
  Mu := Cent1/5.0 + 0.10;
  P := Frac((237.47555 + 3034.9061*Cent1)/360.0)*corfac;
  Q := Frac((265.91650 + 1222.1139*Cent1)/360.0)*corfac;
  S := Frac((243.51721 + 428.46770*Cent1)/360.0)*corfac;
  W := 2*P - 6*Q + 3*S;

  G := Frac((83.76922 + 218.4901*Cent1)/360.0)*corfac;
  H := 2*G - S;

  A := S - P;
  B := S - Q;
  C := G - S;


  {PERTURBATION IN THE MEAN LONGITUDE}

  Cor1 := (0.864319 - 0.001583*Mu) * sin(H)
        + (0.082222 - 0.006833*Mu) * cos(H);
  Cor1 := Cor1 + 0.036017 * sin(2*H) - 0.003019 * cos(2*H)
        + 0.008122 * sin(W);
  Cor1 := Cor1/radcor;

  LongUra := LongUra + Cor1;


  {PERTURBATIONS IN THE PERIHELION}

  cor2 := 0.120303 * sin(H) + (0.019472 - 0.000947*Mu) * cos(H)
        + 0.006197 * sin(2*H);
  cor2 := cor2/radcor;

  PerUra := PerUra + cor2;

  AnomUra := AnomUra + (Cor1 - cor2/EcUra);


  {PERTURBATIONS IN THE ECCENTRICITY}

  Cor1 := (-3349 + 163*Mu) * sin(H) + 20981 * cos(H) + 1311 * cos(2*H);
  Cor1 := Cor1 * 1.0E-07;

  EcUra := EcUra + Cor1;


  {PERTURBATIONS IN THE SEMI-MAJOR AXIS}

  AxisUra := AxisUra - 0.003825 * cos(H);

  Kepler(AnomUra, EcUra, TrueAnom, EAnom);

  RadiusVector := AxisUra * (1.0 - EcUra * cos(EAnom));

  Cor1 := -25948 + 4985 * cos(A) - 1230 * cos(S) + 3354 * cos(B);
  Cor1 := Cor1 + (5795 * cos(S) - 1165 * sin(S) + 1388 * cos(2*S))
        * sin(B);
  Cor1 := Cor1 + (1351 * cos(S) + 5702 * sin(S) + 1388 * sin(2*S))
        * cos(B);
  Cor1 := Cor1 + 904 * cos(2*C) + 894 * (cos(C) - cos(3*C));
  Cor1 := Cor1 * 1.0E-06;

  RadiusVector := RadiusVector + Cor1;

  ArgLat := LongUra + TrueAnom - AnoMura - NodeUra;
  if (ArgLat < 0.0) then
    ArgLat := ArgLat + 360.0/radcor;

  Ecliptic(ArgLat, IncUra, NodeUra, Longitude, Latitude);

  Cor1 := (0.010122 - 0.000988*Mu) * sin(S+B);
  Cor1 := Cor1 + (-0.038581 + 0.002031 * Mu - 0.00191 * sqr(Mu))
        * cos(S+B);
  Cor1 := Cor1 + (0.034964 - 0.001038 * Mu + 0.000868 * sqr(Mu))
        * cos(2*S+B);

  Cor1 := Cor1 + 0.005594 * sin(S + 3*C) - 0.014808 * sin(A);
  Cor1 := Cor1 - 0.005794 * sin(B) + 0.002347 * cos(B)
        + 0.009872 * sin(C);
  Cor1 := Cor1 + 0.008803 * sin(2*C) - 0.004308 * sin(3*C);

  Longitude := Longitude + Cor1/radcor;

  Cor1 := (0.000458 * sin(B) - 0.000642 * cos(B)
        - 0.000517 * cos(4*C)) * sin(S);
  Cor1 := Cor1 - (0.000347 * sin(B) + 0.000853 * cos(B)
        + 0.000517 * sin(4*B)) * cos(S);
  Cor1 := Cor1 + 0.000403 * (cos(2*C) * sin(2*S)
        + sin(2*C) * cos(2*S));

  Latitude := Latitude + Cor1/radcor;

  Conversion (Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

  RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
  Rotation(JD, Result.RA, Result.DC);
end;


function NeptunePerturbations(JD : Double) : TStPlanetsRec;
var
  Mu,
  P, Q, S,
  G, H,
  A, B, C,
  Cor1, cor2,
  RadiusVector,
  ArgLat         : Double;

begin
   Mu := Cent1/5.0 + 0.10;
   P := Frac((237.47555 + 3034.9061*Cent1)/360.0)*corfac;
   Q := Frac((265.91650 + 1222.1139*Cent1)/360.0)*corfac;
   S := Frac((243.51721 + 428.46770*Cent1)/360.0)*corfac;
   G := Frac((83.76922 + 218.4901*Cent1)/360.0)*corfac;
   H := 2*G - S;


   A := G - P;
   B := G - Q;
   C := G - S;


{PERTURBATIONS IN THE MEAN LONGITUDE}

   Cor1 := (-0.589833 + 0.001089*Mu) * sin(H)
         + (-0.056094 + 0.004658*Mu) * cos(H);
   Cor1 := Cor1 - 0.024286 * sin(2*H);
   Cor1 := Cor1/radcor;

   LongNep := LongNep + Cor1;


{PERTURBATIONS IN THE PERIHELION}

   cor2 := 0.024039 * sin(H) - 0.025303 * cos(H) + 0.006206 * sin(2*H);
   cor2 := cor2 - 0.005992 * cos(2*H);
   cor2 := cor2/radcor;

   PerNep := PerNep + cor2;

   AnomNep := AnomNep + (Cor1 - cor2/EcNep);


{PERTURBATIONS IN THE ECCENTRICITY}

   Cor1 := 4389 * sin(H) + 4262 * cos(H) + 1129 * sin(2*H)
         + 1089 * cos(2*H);

   EcNep := EcNep + Cor1 * 1.0E-07;


   {PERTURBATIONS IN THE SEMI-MAJOR AXIS}

   Cor1 := -817 * sin(H) + 8189 * cos(H) + 781 * cos(2*H);
   AxisNep := AxisNep + Cor1 * 1.0E-06;

   Kepler(AnomNep, EcNep, TrueAnom, EAnom);

   RadiusVector := AxisNep * (1.0 - EcNep * cos(EAnom));

   Cor1 := -40596.0 + 4992 * cos(A) + 2744 * cos(B)
         + 2044 * cos(C) + 1051 * cos(2*C);

   RadiusVector := RadiusVector + Cor1 * 1.0E-06;

   ArgLat := LongNep + TrueAnom - AnomNep - NodeNep;
   if (ArgLat < 0.0) then
     ArgLat := ArgLat + 360.0/radcor;

   Ecliptic(ArgLat, IncNep, NodeNep, Longitude, Latitude);

   Cor1 := -0.009556 * sin(A) - 0.005178 * sin(B) + 0.002572 * sin(2*C);
   Cor1 := Cor1 - 0.002972 * cos(2*C) * sin(G)
         - 0.002833 * sin(2*C) * cos(G);

   Longitude := Longitude + Cor1/radcor;

   Cor1 := 0.000336 * cos(2*C) * sin(G) + 0.000364 * sin(2*C) * cos(G);

   Latitude := Latitude + Cor1/radcor;

   Conversion(Longitude, Latitude, RadiusVector, XCord, YCord, ZCord);

   RADec(XCord, YCord, ZCord, Result.RA, Result.DC, Result.Elong);
   Rotation(JD, Result.RA, Result.DC);
end;


function PlanetsPos(JD : Double) : TStPlanetsArray;
var
  I : Integer;
begin
  Cent1 := (JD - 2415020.0)/36525.0;
  Cent2 := sqr(Cent1);
  Cent3 := Cent1 * Cent2;

  MeanElements;
  M1 := AnomMer;
  M2 := AnomVen;
  M4 := AnomMar;
  M5 := AnomJup;
  M6 := AnomSat;
  M7 := AnomUra;
  M8 := AnomNep;

  SunofDate(JD);
  XEth := SunX;
  YEth := SunY;
  ZEth := SunZ;

  RADec(0,0,0, SunsRA, SunsDec, SunsElong);

  for I := 1 to 7 do
  begin
    case I of
      1 : Result[I] := MercuryPerturbations(JD);
      2 : Result[I] := VenusPerturbations(JD);
      3 : Result[I] := MarsPerturbations(JD);
      4 : Result[I] := JupiterPerturbations(JD);
      5 : Result[I] := SaturnPerturbations(JD);
      6 : Result[I] := UranusPerturbations(JD);
      7 : Result[I] := NeptunePerturbations(JD);
    end;
  end;
end;


end.
