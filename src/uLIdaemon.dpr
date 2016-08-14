// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program uLIdaemon;

uses
  Forms,
  Windows,
  SysUtils,
  Generics.Collections,
  fMain in 'fMain.pas' {F_Main},
  Verze in 'Verze.pas',
  tUltimateLI in 'tUltimateLI.pas',
  fDebug in 'fDebug.pas' {F_Debug},
  tUltimateLIConst in 'tUltimateLIConst.pas',
  client in 'client.pas',
  listeningThread in 'listeningThread.pas',
  Hash in 'Hash.pas',
  ORList in 'ORList.pas',
  server in 'server.pas',
  mausSlot in 'mausSlot.pas',
  tHnaciVozidlo in 'tHnaciVozidlo.pas',
  fSlots in 'fSlots.pas' {F_slots},
  GlobalConfig in 'GlobalConfig.pas',
  comDiscovery in 'comDiscovery.pas',
  fConnect in 'fConnect.pas' {F_Connect};

{$R *.res}

var
  Mutex: Cardinal;
  portsList: TList<Integer>;

begin
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
  DecimalSeparator := '.';

  // povolena pouze jedna instance uLI-daemon
  Mutex := CreateMutex(nil, True, 'uLI-daemon');
  if ((Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS)) then halt(0);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'uLI-daemon';
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Debug, F_Debug);
  Application.CreateForm(TF_slots, F_slots);
  Application.CreateForm(TF_Connect, F_Connect);

  try
   TCPServer.Start();
  except
   on E:Exception do
     Application.MessageBox(PChar('Nepodaøilo se nastartovat bridge server'+#13#10+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;

 // pripojovani k uLI-master
 if (GlobConfig.port.port <> '') then
  begin
   // pripojit se k preddefinovanemu portu
   F_Main.Open(GlobConfig.port.port);
  end else begin
   // Kontrola pripojenych zarizeni
   portsList := TList<Integer>.Create();
   EnumuLIDevices(ULI_DEVICE_DESCRIPTION, portsList);
   if (portsList.Count = 1) then
    begin
      // pripojit k portu
      F_Main.Open('COM'+IntToStr(portsList[0]));
    end else begin
      // zobrazit nabidku portu
      F_Main.ShowChild(F_Connect);

      if (portsList.Count = 0) then begin
        F_Connect.GB_Connect.Caption := ' Nenalezeno uLI-master ';
        F_Main.LogMessage('Nenalezeno uLI-master, pøipojte uLI-master, vyberte port a pøipojte se.');
      end else begin
        F_Connect.GB_Connect.Caption := ' Nalezeno více uLI-master, vyberte jedno ';
        F_Main.LogMessage('Nalezeno více uLI-master.');
      end;;
    end;
   portsList.Free();
  end;

  Application.Run;
end.
