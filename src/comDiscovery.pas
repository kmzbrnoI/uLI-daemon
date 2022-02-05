unit comDiscovery;

{
  Odhalovani uLI-master v zavislosti na 'Popisu zarizeni hlasenem sbernici'.
  Funkce EnumuLIDevices(list) prida do seznamu \list cisla vsech COM portu,
  na kterych je pripojeno uLI-master.

  Odhalovani COM portu je zalozeno na Windows SetupAPI a faktu, ze
  friendly_name COM portu je ve formatu "description (COMxy)".

  Tento soubor pochazi z http://www.delphipraxis.net/1198221-post7.html.
}

interface

uses Types, Windows, ComObj, StdCtrls, SysUtils, Generics.Collections, StrUtils;

const
  MAX_PATH = 260;
  CR_SUCCESS = $00000000;

type
  //
  // Define type for reference to device information set
  //
  THDEVINFO = Pointer;

  //
  // Device information structure (references a device instance
  // that is a member of a device information set).
  // SP_DEVINFO_DATA je unikatni odkaz na jedno konkretni zarizeni.
  //
  PSP_DevInfo_Data = ^TSP_DevInfo_Data;

  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: ULONG_PTR;
  end;

  TSP_DevInfo_Data = SP_DEVINFO_DATA;

  PDEVPROPKEY = ^TDEVPROPKEY;

  DEVPROPKEY = packed record
    fmtid: TGUID;
    pid: Pointer;
  end;

  TDEVPROPKEY = DEVPROPKEY;

  DEVPROPTYPE = Pointer;
  PCWSTR = PWCHAR;

  TDEVINST = DWORD;

  TPOSVERSIONINFOW = ^TOSVERSIONINFOW;

  TOSVERSIONINFOW = packed record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array [0 .. 127] of wchar;
  end;

procedure EnumuLIDevices(filter: string; res: TList<Integer>);

implementation

function SetupDiGetDeviceProperty(DeviceInfoSet: THDEVINFO;
  DeviceInfoData: PSP_DevInfo_Data; const PropertyKey: PDEVPROPKEY;
  var PropertyType: DEVPROPTYPE; PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD; RequiredSize: PDWORD; Flags: DWORD): BOOL; stdcall;
  external 'Setupapi.DLL' name 'SetupDiGetDevicePropertyW';
function SetupDiGetClassDevsW(const ClassGuid: PGUID; Enumerator: PCWSTR;
  hwndParent: HWND; Flags: DWORD): THDEVINFO; stdcall;
  external 'Setupapi.DLL' name 'SetupDiGetClassDevsW';
function SetupDiEnumDeviceInfo(DeviceInfoSet: THDEVINFO; MemberIndex: DWORD;
  DeviceInfoData: PSP_DevInfo_Data): BOOL; stdcall;
  external 'Setupapi.DLL' name 'SetupDiEnumDeviceInfo';
function CM_Get_Device_IDW(DeviceInstanceHandle: TDEVINST; Buffer: PCWSTR;
  Bufferlen: ULONG; ulFlags: ULONG): DWORD; stdcall;
  external 'Setupapi.DLL' name 'CM_Get_Device_IDW';
function SetupDiGetDeviceRegistryPropertyW(DeviceInfoSet: THDEVINFO;
  const DeviceInfoData: SP_DEVINFO_DATA; Property_: DWORD;
  var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE;
  PropertyBufferSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  external 'Setupapi.DLL' name 'SetupDiGetDeviceRegistryPropertyW';
function GetVersionExW(OsVersion: TPOSVERSIONINFOW): BOOL; stdcall;
  external 'Kernel32.dll' name 'GetVersionExW';

//
// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
//
const // DIGCF_DEFAULT = $00000001; // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_PROFILE = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;
  INVALID_HANDLE_VALUE = DWORD($FFFFFFFF);

  MAX_DEVICE_ID_LEN = 200;

  SPDRP_DEVICEDESC = ($00000000); // DeviceDesc (R/W)
  SPDRP_FRIENDLYNAME = ($0000000C); // FriendlyName (R/W)

  DEVPKEY_Device_BusReportedDeviceDesc
    : TDEVPROPKEY = (fmtid: '{540b947e-8b40-45bc-a8a2-6a0b894cbda2}';
    pid: Pointer(4)); // DEVPROP_TYPE_STRING

function GetComPortId(friendly_string: string): Integer;
var
  start: Integer;
begin
  start := Length(friendly_string) - 1;
  while (friendly_string[start] <> '(') do
    dec(start);
  Result := StrToIntDef(Copy(friendly_string, start + 4, Length(friendly_string)
    - start - 5), 0);
end;

procedure EnumuLIDevices(filter: string; res: TList<Integer>);
var
  GUID_COM: TGUID; // GUID COM portu
  hDevInfo: THDEVINFO; // seznam zarizeni
  DeviceInfoData: TSP_DevInfo_Data; // data o konkretnim zarizeni
  successful: BOOL;
  i, r: DWORD;
  Buffer: array [0 .. MAX_DEVICE_ID_LEN] of wchar;
  dwSize, dwPropertyRegDataType: DWORD;
  szDesc: array [0 .. 1023] of char;
  OsVersion: TOSVERSIONINFOW;
  ulPropertyType: DEVPROPTYPE;
  friendly_description: string;
begin
  GUID_COM := StringToGUID('{4d36e978-e325-11ce-bfc1-08002be10318}');
  // GUID COM portu
  hDevInfo := SetupDiGetClassDevsW(@GUID_COM, nil, 0, DIGCF_PRESENT);
  // ziskame vsechna pripojena zarizeni odpovidajici danemu GUID
  if (DWORD(hDevInfo) <> INVALID_HANDLE_VALUE) then
  begin
    i := 0;
    repeat
      DeviceInfoData.cbSize := sizeof(TSP_DevInfo_Data);
      successful := SetupDiEnumDeviceInfo(hDevInfo, i, @DeviceInfoData);
      // vraci data zarizeni na indexu \i v seznamu zarizeni \hDevInfo
      if successful then
      begin
        r := CM_Get_Device_IDW(DeviceInfoData.DevInst, @Buffer, MAX_PATH, 0);
        // vraci id zarizeni jako string do bufferu \Buffer
        if r = CR_SUCCESS then
        begin
          // zjistujeme DRIENDLYNAME zarizeni
          if (SetupDiGetDeviceRegistryPropertyW(hDevInfo, DeviceInfoData,
            SPDRP_FRIENDLYNAME, dwPropertyRegDataType, @szDesc, sizeof(szDesc),
            dwSize)) then
          begin
            friendly_description := widechartostring(@szDesc);
            OsVersion.dwOSVersionInfoSize := sizeof(TOSVERSIONINFOW);
            if (GetVersionExW(@OsVersion)) then
            begin
              if (OsVersion.dwBuildNumber > 7000) then
              begin // mind. Windows Vista
                if (SetupDiGetDeviceProperty(hDevInfo, @DeviceInfoData,
                  @DEVPKEY_Device_BusReportedDeviceDesc, ulPropertyType,
                  @szDesc, sizeof(szDesc), @dwSize, 0)) then
                begin
                  if (widechartostring(@szDesc) = filter) then
                    res.Add(GetComPortId(friendly_description));
                end;
              end;
            end;
          end;
        end;
        inc(i);
      end;
    until not successful;
  end;
end;

end.// unit
