unit Verze;

{
  Tato unit implementuje funkce, ktere zjisti verzi aplikace.
}

interface

uses Windows, SysUtils, Forms, jclPEImage;

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

function GetLastBuildDate():String;
 begin
  DateTimeToString(Result, 'dd.mm.yyyy', jclPEImage.PeReadLinkerTimeStamp(Application.ExeName));
 end;//function

function GetLastBuildTime():String;
 begin
  DateTimeToString(Result, 'hh:mm:ss', jclPEImage.PeReadLinkerTimeStamp(Application.ExeName));
 end;//function

end.//unit
