// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program uLIdaemon;

{
  ------------------------------ uLI-DAEMON ------------------------------------

  Tento program slouzi jako klientska aplikace k JOP technologii vyvinute
  Janem Horackem, ktera umoznuje rizeni hnacich vozidel pomoci Rocomaus pripojenych
  k pocitaci pomoci uLI-master.

  Funkce:
    - rizeni jizdniho stupne, smeru
    - moznost nouzoveho zastaveni, moznost plynuleho zastaveni
    - multitrakce
    - rozliseni mezi totalnim rucnim rizenim (vhodne napr. pro posun) a
      polorucnim rizenim (vhodne napr. pro rizeni funkci HV v trati)
    - kontrola odpovedi serveru, resp. cetraly, na prikaz
    - spoluprace s ovladaci pripojenymi k XpressNETu primo do centraly
    - kompatibilita s vnejsimi programy akceptovanim argumentu
}

{
 Format argumentu:
    "-u" username
    "-p" password
    "-s" server (ip/dns)
    "-pt" port
    "-l" zobrazit logovaci okno

 napr.
   uLI-daemon.exe -u root -p heslo -s server-tt -p 1234

 Port je nepovinny argument, ostatni argumenty jsou povinne. Pokud jsou predany
 povinne argumenty, uLI-daemon se pokusi pripojit k serveru. Predavani argumentu
 aplikaci je zamysleno predevsim pro DEBUG, v realnem nasazeni uLI-daemon ziskava
 autorizaci od hJOPpanelu.
}

uses
  Forms,
  Windows,
  SysUtils,
  Generics.Collections,
  fMain in 'fMain.pas' {F_Main},
  version in 'version.pas',
  tUltimateLI in 'tUltimateLI.pas',
  fDebug in 'fDebug.pas' {F_Debug},
  tUltimateLIConst in 'tUltimateLIConst.pas',
  client in 'client.pas',
  listeningThread in 'listeningThread.pas',
  ORList in 'ORList.pas',
  server in 'server.pas',
  mausSlot in 'mausSlot.pas',
  tHnaciVozidlo in 'tHnaciVozidlo.pas',
  fSlots in 'fSlots.pas' {F_slots},
  GlobalConfig in 'GlobalConfig.pas',
  comDiscovery in 'comDiscovery.pas',
  fConnect in 'fConnect.pas' {F_Connect};

{$R *.res}

begin
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
  FormatSettings.DecimalSeparator := '.';

  // povolena pouze jedna instance uLI-daemon
  var Mutex := CreateMutex(nil, True, 'uLI-daemon');
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
   var portsList := TList<Integer>.Create();
   try
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
   finally
     portsList.Free();
   end;
  end;

  // Parsovani argumentu
  var server := '';
  var port := _DEFAULT_PORT;
  begin
   var i := 1;
   while (i <= ParamCount) do
    begin
     var arg := ParamStr(i);
     if ((arg = '-u') and (i < ParamCount)) then
      begin
       // parse username
       Inc(i);
       TCPClient.toAuth.username := ParamStr(i);
      end else
     if ((arg = '-p') and (i < ParamCount)) then
      begin
       // parse password
       Inc(i);
       TCPClient.toAuth.password := ParamStr(i);
      end else
     if ((arg = '-s') and (i < ParamCount)) then
      begin
       Inc(i);
       server := ParamStr(i);
      end else
     if ((arg = '-pt') and (i < ParamCount)) then
      begin
       Inc(i);
       port := StrToIntDef(ParamStr(i), _DEFAULT_PORT);
     end;
     if (arg = '-l') then
      begin
       F_Debug.Show();
       F_Debug.CHB_DataLogging.Checked := true;
      end;

     Inc(i);
    end;//while
  end;

  // pripojeni k serveru z argumentu
  if ((server <> '') and (TCPClient.toAuth.username <> '') and (TCPClient.toAuth.password <> '')) then
   begin
    try
      TCPClient.Connect(server, port);
    except
      on E:Exception do
        Application.MessageBox(PChar('Nelze se pøipojit k hJOPserveru:'+#13#10+E.Message), 'uLI-daemon', MB_OK OR MB_ICONWARNING);
    end;
   end;

  Application.Run;
end.
