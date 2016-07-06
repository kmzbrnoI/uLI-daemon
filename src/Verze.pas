unit Verze;

{
  Tato unit implementuje funkce, ktere zjisti verzi aplikace.
}

interface

uses  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ImgList, Buttons,
  ComCtrls,inifiles, ActnList, AppEvnts, Mask, ScktComp,ToolWin,jpeg,
  Spin, ExtDlgs, Grids, Gauges, Registry,
  StrUtils, DateUtils, mmsystem;

 function GetVersion(const FileName: string):string;
 function GetLastBuildDate():string;
 function GetLastBuildTime():string;

implementation

function GetVersion(const FileName: string):string;
var
  size, len: longword;
  handle: THandle;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor, Release: word;
begin
  Result:='Není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if size > 0 then begin
    GetMem(buffer, size);
    if GetFileVersionInfo(Pointer(FileName), 0, size, buffer)
    then
      if VerQueryValue(buffer, '\', pointer(pinfo), len) then begin
        Major   := HiWord(pinfo.dwFileVersionMS);
        Minor   := LoWord(pinfo.dwFileVersionMS);
        Release := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d.%d',[Major, Minor, Release]);
      end;
    FreeMem(buffer);
  end;
end;

function GetLastBuildDate:String;
var lSearchRec: TSearchRec;
 begin
  if (FindFirst(Application.EXEName, faAnyFile, lSearchRec) = 0) then
   begin
    DateTimeToString(Result,'dd.mm.yyyy',FileDateToDateTime(lSearchRec.Time));
    FindClose(lSearchRec);
   end;
 end;

function GetLastBuildTime:String;
var lSearchRec: TSearchRec;
 begin
  if (FindFirst(Application.EXEName, faAnyFile, lSearchRec) = 0) then
   begin
    DateTimeToString(Result,'hh:mm:ss',FileDateToDateTime(lSearchRec.Time));
    FindClose(lSearchRec);
   end;
 end;

end.//unit
