unit client;

{
  Trida TTCPClient zabezpecuje TCP klient soket, ktery resi komunikaci
  s technologickym serverem.
  Nize je popis komunikacniho protokolu.
}

interface

uses SysUtils, IdTCPClient, listeningThread, IdTCPConnection, IdGlobal,
  Classes, StrUtils, Graphics, Windows, Forms, Controls, ExtCtrls,
  Generics.Collections, Hash, tUltimateLIConst;

const
  _DEFAULT_PORT = 5896;
  _PING_TIMER_PERIOD_MS = 5000;

  // tady jsou vyjmenovane vsechny verze protokoluk pripojeni k serveru, ktere klient podporuje
  protocol_version_accept: array [0 .. 1] of string = ('1.0', '1.1');

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TtoAuth = record
    username: string;
    password: string;
  end;

  TSlotToAuth = record
    slot: Byte;
    ruc: boolean;
  end;

  TTCPClient = class
  private const
    _PROTOCOL_VERSION = '1.1'; // verze protokolu od klienta
    _PROTOCOL_APP_NAME = 'uLIdaemon';

  private
    rthread: TReadingThread;
    // ctecti vlakno (poslouchani probiha v jinem vlakne)
    tcpClient: TIdTCPClient; // objekt TCP klienta
    fstatus: TPanelConnectionStatus;
    // aktualni stav klienta, pro pistup pouzivat \status
    parsed: TStrings;
    // aktualni naparsovana data, pouze pro vnitrni potrebu objektu pri prijmu dat
    data: string; // aktualni prijata data v RAW formatu (jeden radek dat)
    control_disconnect: boolean;
    // true, pokud se odpojuji od serveru na zaklade pozadavku uzivatele, pri nucenem odpojeni false
    fauthorised: boolean;
    // true, pokud strojvedouci autorizovan, pouzivat \authorised
    first_connection: boolean;
    // true, pokud aktualni pripojovani je prvni pripojovani (po startu)
    username: string;
    pingTimer: TTimer;

    procedure OnTcpClientConnected(Sender: TObject);
    // event TCP klienta pripojeni k serveru
    procedure OnTcpClientDisconnected(Sender: TObject);
    // event TCP klienta odpojeni od serveru
    procedure DataReceived(const data: string);
    // event prijatych dat od cteciho vlakna
    procedure Timeout();
    // timeout from socket = broken pipe                // event timeoutu cteciho vlakna (spojeni se serverem rozvbto)

    // data se predavaji v Self.Parsed
    procedure ParseLokGlobal(); // parsing dat s prefixem "-;LOK;G"
    procedure ParseGlobal(); // parsing dat s prefixem "-;"
    procedure ParseLok(); // parsing dat s prefixem "-;LOK;addr"

    procedure SendPing(Sedner: TObject);

  public

    toAuth: TtoAuth;
    lokToSlotMap: TDictionary<Word, TSlotToAuth>;

    constructor Create();
    destructor Destroy(); override;

    function Connect(host: string; port: Word): Integer; // pripojit k serveru
    function Disconnect(): Integer; // odpojit od serveru

    procedure SendLn(str: string); // poslat zpravu (jeden radek)
    procedure LokoPlease(addr: Word; token: string);
    // zadost o lokomotivu tokenem
    procedure Auth();

    class function SlotToAuth(slot: Byte; ruc: boolean): TSlotToAuth;

    property status: TPanelConnectionStatus read fstatus;
    // aktualni stav pripojeni
    property authorised: boolean read fauthorised;
    // true pokud strojvedouci autorizovan
    property user: string read username;

  end; // TPanelTCPClient

var
  tcpClient: TTCPClient;

implementation

{
  Specifikace komunikacniho protokolu:
  Jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem
  prvni parametr je v pripade regulatoru vzdy '-'.
  Komunikace probiha znakovou sadou UTF-8.
  Komunikace JE case-sensitive.


  vynatek z protokolu:
  PRIKAZY PRO REGULATOR:

  ////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// KLIENT -> SERVER ///////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////
  -;OR-LIST;                              - pozadavek na ziskani seznamu oblasti rizeni (stanic)


  -;LOK;G;AUTH;username;passwd            - pozadavek na autorizaci uzivatele
  -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
  -;LOK;G:CANCEL;                         - zruseni pozadavku o loko

  -:LOK;addr;PLEASE;token                 - zadost o rizeni konkretni lokomotivy; token neni potreba pripojovat v pripade, kdy loko uz mame autorizovane a bylo nam ukradeno napriklad mysi
  -;LOK;addr;RELEASE                      - zadost o uvolneni lokomotivy z regulatoru
  -;LOK;addr;SP;sp_km/h                   - nastaveni rychlosti lokomotivy
  -;LOK;addr;SPD;sp_km/h;dir[0,1]         - nastaveni rychlosti a smeru lokomotivy
  -;LOK;addr;D;dir[0,1]                   - nastaveni smeru lokomotivy
  -;LOK;addr;SP-S;sp_stupen[0-28]         - nastaveni rychlosti lokomotivy ve stupnich
  -;LOK;addr;SPD-S;sp_stupen;dir[0,1]     - nastaveni rychlosti a smeru lokomotivy ve stupnich
  -;LOK;addr;F;F_left-F_right;states      - nastaveni funkci lokomotivy
  napr.; or;LOK;F;0-4;00010 nastavi F3 a ostatni F vypne
  -;LOK;addr;STOP;                        - nouzove zastaveni
  -;LOK;addr;TOTAL;[0,1]                  - nastaveni totalniho rucniho rizeni hnaciho vozidla



  ////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// SERVER -> KLIENT ///////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////
  -;OR-LIST;[or1id,or1name][or2id, ...    - zaslani seznamu vsech oblasti rizeni; id je unikatni ID, nazev je nazev pro uzivatele
  dale v protokolu je pouzivano pouze ID

  -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
  -;LOK;G;AUTH;[ok,not];info              - navratove hlaseni o autorizaci uzivatele
  -;LOK;addr;AUTH;[ok,not,stolen,release,total];info;hv_data
  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
  info je string
  hv_data jsou pripojovana k prikazu v pripade, ze doslo uspesne k autorizaci; jedna se o PanelString hnaciho vozdila obsahujici vsechny informace o nem
  -;LOK;addr;F;F_left-F_right;states      - informace o stavu funkci lokomotivy
  napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
  -;LOK;addr;SPD;sp_km/h;sp_stupne;dir    - informace o zmene rychlosti (ci smeru) lokomotivy
  -;LOK;addr;RESP;[ok, err];info;speed_kmph
  - odpoved na prikaz;  speed_kmph je nepovinny argument; info zpravidla obsahuje rozepsani chyby, pokud je odpoved "ok", info je prazdne
  -;LOK;addr;TOTAL;[0,1]                   - zmena rucniho rizeni lokomotivy

  ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

  navazani komunikace:
  1) klient se pripoji
  2) klient posle hanshake ("-;HELLO;verze_protokolu")
  3) klient vycka na odpoved na handshake
  4) klient posle na server pozadavek o autorizaci obecneho rizeni -- login strojvedouciho a vycka na odpoved
  5) klient nacte seznam oblasti rizeni a nabidne uzivateli vybrat oblast rizeni, do ktere provest zadost o LOKO

  jak funguje rizeni z regulatoru:
  a) cesta zadosti z regulatoru
  1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
  2) klient si nacte seznam oblasti rizeni
  3) klient si vybere oblasti rizeni a do ni posle pozadavek
  4) oblasti rizeni priradi regulatoru hnaci vozidlo (vozidla)

  b) cesta primeho prevzeti
  1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
  2) klient si obstara autorizacni token pro dane hnaci vozidlo (napriklad od dispecera -- resp. z panelu)
  3) klient pozada o LOKO a prilozi token, loko je mu prirazeno (pozor: token ma omezenou casovou platnost)

}

uses fDebug, fMain, ORList, tUltimateLI, tHnaciVozidlo, server;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTCPClient.Create();
begin
  inherited;

  // inicializace vlastnosti a objetku
  Self.fauthorised := false;
  Self.parsed := TStringList.Create;
  Self.first_connection := true;

  Self.pingTimer := TTimer.Create(nil);
  Self.pingTimer.Enabled := false;
  Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
  Self.pingTimer.OnTimer := Self.SendPing;

  // vytvoreni TCP klienta
  Self.tcpClient := TIdTCPClient.Create(nil);
  Self.tcpClient.OnConnected := Self.OnTcpClientConnected;
  Self.tcpClient.OnDisconnected := Self.OnTcpClientDisconnected;
  Self.tcpClient.ConnectTimeout := 1500;

  Self.lokToSlotMap := TDictionary<Word, TSlotToAuth>.Create();

  // aktualni status = zavrene spojeni
  Self.fstatus := TPanelConnectionStatus.closed;
  Self.username := '';
end; // ctor

destructor TTCPClient.Destroy();
begin
  try
    if (Assigned(Self.tcpClient)) then
      FreeAndNil(Self.tcpClient);

    if (Assigned(Self.parsed)) then
      FreeAndNil(Self.parsed);

    Self.lokToSlotMap.Free();
    Self.pingTimer.Free();
  finally
    inherited;
  end;
end; // dtor

/// /////////////////////////////////////////////////////////////////////////////

function TTCPClient.Connect(host: string; port: Word): Integer;
begin
  try
    if (Self.tcpClient.Connected) then
      Exit(1);
  except
    try
      Self.tcpClient.Disconnect(false);
    except
    end;
    if (Self.tcpClient.IOHandler <> nil) then
      Self.tcpClient.IOHandler.InputBuffer.Clear;
  end;

  F_Main.ClearMessage();

  Self.tcpClient.host := host;
  Self.tcpClient.port := port;

  Self.fstatus := TPanelConnectionStatus.opening;

  try
    Self.tcpClient.Connect();
  except
    Self.fstatus := TPanelConnectionStatus.closed;
    raise;
  end;

  Self.tcpClient.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
  Self.control_disconnect := false;

  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTCPClient.Disconnect(): Integer;
begin
  try
    if (not Self.tcpClient.Connected) then
      Exit(1);
  except

  end;

  Self.control_disconnect := true;
  if Assigned(Self.rthread) then
    Self.rthread.Terminate;
  try
    Self.tcpClient.Disconnect();
  finally
    if Assigned(Self.rthread) then
    begin
      Self.rthread.WaitFor;
      FreeAndNil(Self.rthread);
    end;
  end;

  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TTCPClient.OnTcpClientConnected(Sender: TObject);
begin
  // klient pripojen -> vytvorime cteci vlakno
  try
    Self.rthread := TReadingThread.Create(TIdTCPClient(Sender));
    Self.rthread.OnData := DataReceived;
    Self.rthread.OnTimeout := Timeout;
    Self.rthread.Start();
  except
    (Sender as TIdTCPClient).Disconnect;
    raise;
  end;

  // update okynka
  F_Main.P_Client.Color := clYellow;
  F_Main.P_Client.Hint := 'Probíhá handshake...';

  Self.fstatus := TPanelConnectionStatus.handshake;
  Self.pingTimer.Enabled := true;

  // odeslat handshake
  Self.SendLn('-;HELLO;' + Self._PROTOCOL_VERSION + ';' + Self._PROTOCOL_APP_NAME);
end;

procedure TTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
  // klient odpojen -> znicit cteci vlakno
  if Assigned(Self.rthread) then
    Self.rthread.Terminate;

  // status klienta na odpojen
  Self.fstatus := TPanelConnectionStatus.closed;
  Self.pingTimer.Enabled := false;
  Self.fauthorised := false;

  // aktualizace okynka
  F_Main.P_Client.Color := clRed;
  F_Main.P_Client.Hint := 'Odpojeno od hJOP serveru';

  // vypnout Rocomaus
  uLI.HardResetSlots();
  TCPServer.BroadcastSlots();
  TCPServer.BroadcastAuth(true);
  uLI.busEnabled := false;

  Self.username := '';
  F_Main.UpdateTitle();

  if ((F_Main.close_app) and (not uLI.Connected)) then
    F_Main.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TTCPClient.DataReceived(const data: string);
begin
  Self.parsed.Clear();

  // vlastni parsovaci funkce
  ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

  Self.data := data;

  // logovani dat
  F_Debug.Log('GET: ' + data);

  try
    if (Self.parsed.Count < 2) then
      Exit();
    Self.parsed[1] := UpperCase(Self.parsed[1]);

    // zakladni rozdeleni podle prefixu
    if (Self.parsed[0] = '-') then
    begin
      if (Self.parsed[1] = 'LOK') then
      begin
        if (UpperCase(Self.parsed[2]) = 'G') then
          Self.ParseLokGlobal()
        else
          Self.ParseLok();
      end
      else
        Self.ParseGlobal();
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.Timeout();
begin
  Self.OnTcpClientDisconnected(Self);
  F_Main.SB_Main.Panels[1].Text := 'Spojení se serverem přerušeno';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.ParseGlobal();
begin
  // parse handhake
  if (Self.parsed[1] = 'HELLO') then
  begin
    // kontrola verze protokolu
    var found := false;
    for var i := 0 to Length(protocol_version_accept) - 1 do
    begin
      if (Self.parsed[2] = protocol_version_accept[i]) then
      begin
        found := true;
        break;
      end;
    end; // for i

    if (not found) then
      Application.MessageBox(PChar('Verze protokolu, kterou používá server (' +
        Self.parsed[2] + ') není podporována'), 'Upozornění',
        MB_OK OR MB_ICONWARNING);

    Self.fstatus := TPanelConnectionStatus.opened;
    F_Main.P_Client.Hint := 'Připojeno k hJOP serveru, probíhá autorizace...';

    // autorizace strojvedouciho
    Self.Auth();
  end

  else if ((parsed[1] = 'PING') and (parsed.Count > 2) and
    (UpperCase(parsed[2]) = 'REQ-RESP')) then
  begin
    if (parsed.Count >= 4) then
      Self.SendLn('-;PONG;' + parsed[3])
    else
      Self.SendLn('-;PONG');
  end

  else if (parsed[1] = 'OR-LIST') then
    ORDb.Parse(parsed[2])

  else if (parsed[1] = 'DCC') then
    uLI.DCC := (UpperCase(parsed[2]) = 'GO');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.ParseLokGlobal();
begin
  if (UpperCase(parsed[3]) = 'AUTH') then
  begin
    // autorizace uzivatele
    var oldAuth := Self.authorised;
    Self.fauthorised := (LowerCase(Self.parsed[4]) = 'ok');
    if (Self.fauthorised) then
    begin
      if (oldAuth) then
        Exit();

      F_Main.P_Client.Hint := 'Připojeno k hJOP serveru, autorizováno';
      F_Main.P_Client.Color := clGreen;

      TCPServer.BroadcastAuth(true);

      // spojedni se serverem uspesne navazano -> zapinam Rocomaus
      if (uLI.Connected) then
      begin
        try
          uLI.busEnabled := true;
        except
          on E: Exception do
            Application.MessageBox
              (PChar('Nelze zapnout napájení sběrnice pro ovladače:' + #13#10 +
              E.Message), 'uLI-daemon', MB_OK OR MB_ICONWARNING);
        end;
      end;

    end
    else
    begin
      F_Main.P_Client.Color := clYellow;
      F_Main.P_Client.Hint := 'Připojeno k hJOP serveru, NEAUTORIZOVÁNO : ' +
        parsed[5];

      if (oldAuth) then
      begin
        if (uLI.busEnabled) then
          uLI.busEnabled := false;
        Application.MessageBox(PChar('Zrušena autorizace uživatele!' + #13#10 +
          parsed[5]), 'Zrušena autorizace', MB_OK OR MB_ICONWARNING);
      end
      else
      begin
        Self.toAuth.username := '';
        Application.MessageBox(PChar('Nepodařilo se autorizovat uživatele!' +
          #13#10 + parsed[5]), 'Autorizace se nezdařila',
          MB_OK OR MB_ICONWARNING);
      end;

      TCPServer.BroadcastAuth();
    end;

    Self.username := Self.toAuth.username;
    Self.toAuth.username := '';
    Self.toAuth.password := '';
    F_Main.UpdateTitle();
  end; // if parsed[3] = AUTH
end;

/// /////////////////////////////////////////////////////////////////////////////

{
  -;LOK;addr;AUTH;[ok,not,stolen,release,total];info;hv_data
  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
  info je string
  hv_data jsou pripojovana k prikazu v pripade, ze doslo uspesne k autorizaci; jedna se o PanelString hnaciho vozdila obsahujici vsechny informace o nem
  -;LOK;addr;F;F_left-F_right;states      - informace o stavu funkci lokomotivy
  napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
  -;LOK;addr;SPD;sp_km/h;sp_stupne;dir    - informace o zmene rychlosti (ci smeru) lokomotivy
  -;LOK;addr;RESP;[ok, err];info;speed_kmph

}

procedure TTCPClient.ParseLok();
var
  addr: Word;
begin
  addr := StrToInt(parsed[2]);
  parsed[3] := UpperCase(parsed[3]);

  if (parsed[3] = 'AUTH') then
  begin
    if (not Self.lokToSlotMap.ContainsKey(addr)) then
      Exit();
    var slot := Self.lokToSlotMap[addr].slot;
    var hvIndex := uLI.sloty[slot].GetHVIndex(addr);

    if (hvIndex = -1) then
    begin
      // prirazeni nove loko do slotu
      if ((parsed[4] = 'ok') or (parsed[4] = 'total')) then
      begin
        var HV := THV.Create(parsed[5]);
        HV.total := (parsed[4] = 'total');
        uLI.sloty[slot].AddLoko(HV);
        uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId + $60), slot);

        if ((not HV.total) and (Self.lokToSlotMap[addr].ruc)) then
        begin
          // loko se chce prevzit do rucniho rizeni
          Self.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';TOTAL;1');
        end;

        // zamezit budoucim prebiranim na rucni rizeni
        Self.lokToSlotMap[addr] := TTCPClient.SlotToAuth(slot, false);

      end
      else if (parsed[4] = 'not') then
      begin
        if (Assigned(uLI.sloty[slot].Sender)) then
        begin
          TCPServer.SendLn(uLI.sloty[slot].Sender, 'LOKO;err;10;' + parsed[5]);
          uLI.sloty[slot].Sender := nil;
        end;
        uLI.sloty[slot].UpdateGUI();
        Application.MessageBox
          (PChar('Lokomotivu ' + parsed[2] + ' se nepodařio autorizovat' +
          #13#10 + parsed[5]), 'Loko neautorizováno', MB_OK OR MB_ICONWARNING)
      end;

    end
    else
    begin
      // aktualizace dat o existujici loko

      if ((parsed[4] = 'ok') or (parsed[4] = 'total')) then
      begin
        uLI.sloty[slot].gui.P_status.Caption := 'OK';
        uLI.sloty[slot].HVs[hvIndex].total := (parsed[4] = 'total');
        uLI.sloty[slot].HVs[hvIndex].ukradeno := false;
        uLI.sloty[slot].UpdateGUI();
        uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId + $60), slot);
      end
      else if (parsed[4] = 'stolen') then
      begin
        uLI.sloty[slot].HVs[hvIndex].ukradeno := true;
        uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId + $60), slot);
        uLI.sloty[slot].UpdateGUI();
      end
      else if (parsed[4] = 'release') then
      begin
        uLI.sloty[slot].RemoveLoko(hvIndex);
        Self.lokToSlotMap.Remove(addr);
        if (uLI.sloty[slot].isMaus) then
          uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId +
            $60), slot);
        TCPServer.BroadcastSlots();
      end;
    end;
  end
  else
  begin
    // vsechny nasledujici prikazy vyzaduji znalost \slot a \hvIndex

    if (not Self.lokToSlotMap.ContainsKey(addr)) then
      Exit();
    var slot := Self.lokToSlotMap[addr].slot;
    if (not uLI.sloty[slot].isLoko) then
      Exit();
    var hvIndex := uLI.sloty[slot].GetHVIndex(addr);
    if (hvIndex < 0) then
      Exit();

    if (parsed[3] = 'F') then
    begin
      var func: TStrings := TStringList.Create();
      ExtractStringsEx(['-'], [], parsed[4], func);
      var left := StrToInt(func[0]);
      var right: Integer;
      if (func.Count > 1) then
        right := StrToInt(func[1])
      else
        right := left;
      func.Free();

      for var i := left to right do
        if (i < _MAX_FUNC) then
          uLI.sloty[slot].HVs[hvIndex].funkce[i] :=
            (parsed[5][i - left + 1] = '1');

      uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId + $60), slot);
    end
    else if (parsed[3] = 'SPD') then
    begin
      uLI.sloty[slot].HVs[hvIndex].rychlost_kmph := StrToInt(parsed[4]);
      uLI.sloty[slot].HVs[hvIndex].rychlost_stupne := StrToInt(parsed[5]);
      uLI.sloty[slot].HVs[hvIndex].smer := StrToInt(parsed[6]);
      uLI.SendLokoStolen(uLI.CalcParity(uLI.sloty[slot].mausId + $60), slot);

      uLI.sloty[slot].gui.L_Speed.Caption :=
        IntToStr(uLI.sloty[slot].HVs[hvIndex].rychlost_kmph) + ' km/h';
    end
    else if (parsed[3] = 'RESP') then
    begin
      if (parsed[4] = 'ok') then
      begin
        if (parsed.Count > 6) then
          uLI.sloty[slot].gui.L_Speed.Caption := parsed[6] + ' km/h';
        uLI.sloty[slot].gui.P_status.Color := clLime;
        uLI.sloty[slot].gui.P_status.Caption := 'OK';
      end
      else
      begin
        uLI.sloty[slot].gui.P_status.Color := clRed;
        uLI.sloty[slot].gui.P_status.Caption := 'ERROR';
        uLI.sloty[slot].gui.P_status.Hint := parsed[5];
      end;
    end
    else if (parsed[3] = 'TOTAL') then
    begin
      uLI.sloty[slot].updating := true;
      uLI.sloty[slot].HVs[hvIndex].total := boolean(StrToInt(parsed[4]));
      uLI.sloty[slot].gui.CHB_Total.Checked := uLI.sloty[slot].total;
      uLI.sloty[slot].updating := false;

      uLI.sloty[slot].gui.P_status.Color := clLime;
      uLI.sloty[slot].gui.P_status.Caption := 'OK';
      uLI.sloty[slot].gui.P_status.Hint := '';
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.SendLn(str: string);
begin
  try
    if (not Self.tcpClient.Connected) then
      Exit;
  except

  end;

  try
    Self.tcpClient.Socket.WriteLn(str);
  except
    if (Self.fstatus = opened) then
      Self.OnTcpClientDisconnected(Self);
  end;

  F_Debug.Log('SEND: ' + str);
end;

procedure TTCPClient.LokoPlease(addr: Word; token: string);
begin
  Self.SendLn('-;LOK;' + IntToStr(addr) + ';PLEASE;' + token);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.SendPing(Sedner: TObject);
begin
  try
    if (Self.tcpClient.Connected) then
      Self.SendLn('-;PING');
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.Auth();
begin
  Self.SendLn('-;LOK;G;AUTH;' + Self.toAuth.username + ';' +
    Self.toAuth.password);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TTCPClient.SlotToAuth(slot: Byte; ruc: boolean): TSlotToAuth;
begin
  Result.slot := slot;
  Result.ruc := ruc;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

tcpClient := TTCPClient.Create;

finalization

FreeAndNil(tcpClient);

end.// unit
