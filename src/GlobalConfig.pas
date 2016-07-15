unit GlobalConfig;

{
  Globalni konfigurace SW
}

interface

uses IniFiles, SysUtils, Types, Generics.Collections, Classes;

type
  TGlobConfigData = record
    frmPos:TPoint;
    frmSize:TPoint;
  end;

  TGlobConfig = class
    public const
      _DEFAULT_FN = 'config_uLI-daemon.ini';

    private
      filename:string;

    public

      data:TGlobConfigData;

      procedure LoadFile(const filename:string = _DEFAULT_FN);
      procedure SaveFile(const filename:string); overload;
      procedure SaveFile(); overload;

      property fn:string read filename;
  end;

var
  GlobConfig:TGlobConfig;

implementation

uses fMain;

////////////////////////////////////////////////////////////////////////////////

procedure TGlobConfig.LoadFile(const filename:string = _DEFAULT_FN);
var ini:TMemIniFile;
begin
 try
   ini := TMemIniFile.Create(filename);
 except
   Exit();
 end;

 Self.filename := filename;

 Self.data.frmPos.X := ini.ReadInteger('F_Main', 'posX', 100);
 Self.data.frmPos.Y := ini.ReadInteger('F_Main', 'posY', 100);

 Self.data.frmSize.X := ini.ReadInteger('F_Main', 'sizeX', 500);
 Self.data.frmSize.Y := ini.ReadInteger('F_Main', 'sizeY', 300);
end;//function

procedure TGlobConfig.SaveFile(const filename:string);
var ini:TMemIniFile;
begin
 try
   ini := TMemIniFile.Create(filename);
 except
   Exit();
 end;

 ini.WriteInteger('F_Main', 'posX', Self.data.frmPos.X);
 ini.WriteInteger('F_Main', 'posY', Self.data.frmPos.Y);

 ini.WriteInteger('F_Main', 'sizeX', Self.data.frmSize.X);
 ini.WriteInteger('F_Main', 'sizeY', Self.data.frmSize.Y);

 ini.UpdateFile();
end;//function

procedure TGlobConfig.SaveFile();
begin
 Self.SaveFile(Self.filename);
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
  GlobConfig := TGlobConfig.Create();

finalization
  FreeAndNil(GlobConfig);

end.//unit
