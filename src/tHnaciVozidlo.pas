unit tHnaciVozidlo;

{
  Specifikace hnaciho vozidla.
}

interface

uses Classes, SysUtils, StdCtrls, Generics.Collections, tUltimateLIConst;

const
  _MAX_HV = 128;
  _MAX_FUNC = 28;

type
  THVClass = (parni = 0, diesel = 1, motor = 2, elektro = 3);                   // trida hnaciho vozidla
  TFunkce = array[0.._MAX_FUNC] of boolean;                                     // stav funkci HV
  THVStanoviste = (lichy = 0, sudy = 1);                                        // v jakem smeru se nachazi stanoviste A

  // POM neni pro regulator vubec potreba, je tu jen z uplnosti:
  THVPomCV = record                                                             // jeden zaznam POM se sklada z
    cv:Word;                                                                      // oznaceni CV a
    data:Byte;                                                                    // dat, ktera se maji do CV zapsat.
  end;

  THV = class
   private
     procedure DefaultData();                                                   // nastavi vsechna data na default hodnoty

   public
     Nazev:string;                                                              // nazev HV
     Majitel:string;                                                            // majitel HV
     Oznaceni:string;                                                           // oznaceni HV
     Poznamka:String;                                                           // poznamka k HV
     Adresa:Word;                                                               // digitalni adresa HW (0..9999)
     Trida:THVClass;                                                            // trida hnaciho vozidla - parni, diesel, motor, elektro
     Souprava:string;                                                           // cislo soupravy, na ktere je HV
     StanovisteA:THVStanoviste;                                                 // orientace stanoviste A
     funkce:TFunkce;                                                            // stav funkci
     rychlost_stupne:Word;                                                      // aktualni rychlost ve stupnich
     rychlost_kmph:Word;                                                        // aktualni rychlost v km/h
     smer:Integer;                                                              // aktualni smer
     orid:string;                                                               // id oblasti rizeni, ve ktere se nachazi loko

     POMtake : TList<THVPomCV>;                                                 // seznam POM pri prevzeti do automatu
     POMrelease : TList<THVPomCV>;                                              // seznam POM pri uvolneni to rucniho rizeni

     funcVyznam:array[0.._MAX_FUNC] of string;                                  // seznam popisu funkci hnaciho vozidla

     procedure ParseData(data:string);                                          // parse dat HV ze serveru
     constructor Create(data:string); overload;                                 // vytvoreni HV s daty ze serveru
     destructor Destroy(); override;                                            // zniceni HV

     function GetPanelLokString():string;                                       // vytvoreni stringu obsahujici vsechna data HV pro server
  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor THV.Create(data:string);
begin
 inherited Create();
 Self.POMtake    := TList<THVPomCv>.Create();
 Self.POMrelease := TList<THVPomCv>.Create();
 Self.ParseData(data);
end;//ctor

destructor THV.Destroy();
begin
 Self.POMtake.Free();
 Self.POMrelease.Free();
 inherited;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure THV.ParseData(data:string);
var str, str2, str3:TStrings;
    i:Integer;
    pomCv:THVPomCv;
    tmp:string;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce|rychlost_stupne|rychlost_kmph|smer|or_id|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
 // souprava je bud cislo soupravy, nebo znak '-'
 str  := TStringList.Create();
 str2 := TStringList.Create();
 str3 := TStringList.Create();
 ExtractStringsEx(['|'] , [], data, str);

 Self.DefaultData();

 try
  Self.Nazev        := str[0];
  Self.Majitel      := str[1];
  Self.Oznaceni     := str[2];
  Self.Poznamka     := str[3];
  Self.Adresa       := StrToInt(str[4]);
  Self.Trida        := THvClass(StrToInt(str[5]));
  Self.Souprava     := str[6];
  Self.StanovisteA  := THVStanoviste(StrToInt(str[7]));

  for i := 0 to _MAX_FUNC do
   begin
    if (i < Length(str[8])) then
      if (str[8][i+1] = '1') then
        Self.funkce[i] := true
      else
        Self.funkce[i] := false;
   end;

   Self.rychlost_stupne := StrToInt(str[9]);
   Self.rychlost_kmph   := StrToInt(str[10]);
   Self.smer            := StrToInt(str[11]);
   Self.orid            := str[12];

   if (str.Count > 13) then
    begin
     // pom-take
     ExtractStringsEx([']'] , ['['], str[13], str2);
     for tmp in str2 do
      begin
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv := StrToInt(str3[0]);
       pomCV.cv := StrToInt(str3[1]);
       Self.POMtake.Add(pomCV);
      end;

     // pom-release
     ExtractStringsEx([']'] , ['['], str[14], str2);
     for tmp in str2 do
      begin
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv := StrToInt(str3[0]);
       pomCV.cv := StrToInt(str3[1]);
       Self.POMrelease.Add(pomCV);
      end;
    end;//if str.Count > 12

   // func-vyznam
   if (str.Count > 15) then
    begin
     str2.Clear();
     ExtractStringsEx([';'], [], str[15], str2);
     for i := 0 to _MAX_FUNC do
       if (i < str2.Count) then
        Self.funcVyznam[i] := str2[i]
       else
        Self.funcVyznam[i] := '';
    end else begin
     for i := 0 to _MAX_FUNC do
       Self.funcVyznam[i] := '';
    end;

 except

 end;

 str.Free();
 str2.Free();
 str3.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THV.DefaultData();
var i:Integer;
begin
 Self.Nazev     := '';
 Self.Majitel   := '';
 Self.Oznaceni  := '';
 Self.Poznamka  := '';
 Self.Adresa    := 0;
 Self.Trida     := THvClass.diesel;
 Self.Souprava  := '-';

 Self.POMtake.Clear();
 self.POMrelease.Clear();

 for i := 0 to _MAX_FUNC do
   Self.funkce[i] := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function THV.GetPanelLokString():string;
var i:Integer;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce
 // souprava je bud cislo soupravy, nebo znak '-'
 Result := Self.Nazev + '|' + Self.Majitel + '|' + Self.Oznaceni + '|' + Self.Poznamka + '|' +
           IntToStr(Self.adresa) + '|' + IntToStr(Integer(Self.Trida)) + '|' + Self.souprava + '|' +
           IntToStr(Integer(Self.StanovisteA)) + '|';

 for i := 0 to _MAX_FUNC do
  begin
   if (Self.funkce[i]) then
     Result := Result + '1'
   else
     Result := Result + '0';
  end;

 Result := Result + '||||'+Self.orid+'|';
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
