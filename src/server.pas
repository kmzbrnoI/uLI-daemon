unit server;

{
  Bridge TCP server pro komunikaci s panely, ktere daemonovi davaji lokomotivy
  k rizeni.
}

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal, SyncObjs,
  Classes, StrUtils, tUltimateLIConst, Graphics, Windows,
  IdContext, ComCtrls, IdSync, Generics.Collections;

const
  _BRIDGE_DEFAULT_PORT = 5733; // default port, na kterem bezi bridge server

type
  TAuthStatus = (yes, no, cannot);

  TTCPServer = class
  private
    tcpServer: TIdTCPServer; // object serveru
    parsed: TStrings;
    // naparsovana data, implementovano jako globalni promenna pro zrychleni
    data: string; // prijata data v plain-text forme
    fport: Word; // aktualni port serveru
    lastAuth: TAuthStatus; // posledni stav autorizace
    readLock: TCriticalSection;

    procedure OnTcpServerConnect(AContext: TIdContext);
    // event pripojeni klienta z TIdTCPServer
    procedure OnTcpServerDisconnect(AContext: TIdContext);
    // event odpojeni klienta z TIdTCPServer
    procedure OnTcpServerExecute(AContext: TIdContext);
    // event akce klienta z TIdTCPServer

    procedure Parse(AContext: TIdContext);
    // parsinag dat s globalnim prefixem: "-;"

    function IsOpenned(): boolean; // je server zapnut?

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Start(port: Word); overload; // spustit server
    procedure Start(); overload; // spustit server
    procedure Stop(); // zastavit server
    procedure DisconnectClient(conn: TIdContext); // odpojit konkretniho klienta

    procedure BroadcastData(data: string);
    procedure BroadcastSlots();
    procedure BroadcastAuth(onlyChanges: boolean = false);

    procedure SendLn(AContext: TIdContext; str: string);

    property openned: boolean read IsOpenned;
    property port: Word read fport write fport;
  end; // TPanelTCPClient

var
  tcpServer: TTCPServer;

implementation

{
  Jak funguje komunikace ze strany serveru:
  * Klient se pripoji, posila data, server posila data.
  * Neni vyzadovan hanshake.
  * Server neodpojuje klienty pokud to neni nutne.

}
{
  Specifikace komunikacniho protkolu:
  jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem.

  PRIKAZY:

  ////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// KLIENT -> SERVER ///////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

  LOGIN;server;port;username;password         - pozadavek k pripojeni k serveru a autorizaci regulatoru
  LOKO;slot;[addr;token];[addr;token];...     - pozadavek k umisteni lokomotiv do slotu \slot
  LOKO-RUC;slot;[addr;token];[addr;token];... - pozadavek k umisteni lokomotiv do slotu \slot a autorizaci do totalniho rizeni
  SLOTS?                                      - pozadavek na vraceni seznamu slotu a jejich obsahu
  AUTH?                                       - pozadavek na vraceni stavu autorizace vuci hJOPserveru

  ////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// SERVER -> KLIENT ///////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

  LOKO;ok                                  - loko uspesne prevzato
  LOKO;err;err_code;error message          - loko se nepodarilo prevzit
  SLOTS;[F/-/#];[F/-/#];...                  - sloty, ktere ma daemon k dispozici
  '-' je prazdny slot
  '#' je nefunkcni slot
  'F' je plny slot
  pocet slotu je variabilni
  AUTH;[yes/no/cannot]                     - jestli je uLI-daemon autorizovan vuci hJOPserveru

}

/// /////////////////////////////////////////////////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////

uses fMain, fDebug, client, tUltimateLI;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTCPServer.Create();
begin
  inherited;

  Self.fport := _BRIDGE_DEFAULT_PORT;
  Self.parsed := TStringList.Create;
  Self.lastAuth := TAuthStatus.cannot;

  Self.readLock := TCriticalSection.Create();

  Self.tcpServer := TIdTCPServer.Create(nil);
  Self.tcpServer.OnConnect := Self.OnTcpServerConnect;
  Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
  Self.tcpServer.OnExecute := Self.OnTcpServerExecute;
end; // ctor

destructor TTCPServer.Destroy();
begin
  try
    if (Self.tcpServer.Active) then
      Self.tcpServer.Active := false;

    if (Assigned(Self.tcpServer)) then
      FreeAndNil(Self.tcpServer);

    if (Assigned(Self.parsed)) then
      FreeAndNil(Self.parsed);

    Self.readLock.Free();
  finally
    inherited;
  end;
end; // dtor

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Start(port: Word);
begin
  if (Self.tcpServer.Active) then
    Exit();

  F_Main.P_Server.Color := clYellow;
  F_Main.P_Server.Hint := 'Bridge server: spouštění...';
  F_Debug.Log('Bridge server: spouštění...');

  Self.tcpServer.DefaultPort := port;
  Self.fport := port;

  try
    Self.tcpServer.Active := true;
  except
    on E: Exception do
    begin
      F_Main.P_Server.Color := clRed;
      F_Main.P_Server.Hint :=
        'ERR: Panel server: chyba při startování serveru : ' + E.Message;
      F_Debug.Log('ERR: Panel server: chyba při startování serveru : ' +
        E.Message);
      raise;
    end;
  end;

  F_Main.P_Server.Color := clGreen;
  F_Main.P_Server.Hint := 'Bridge server: spuštěn';
  F_Debug.Log('Bridge server: spuštěn');
end;

procedure TTCPServer.Start();
begin
  Self.Start(Self.port);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Stop();
begin
  if (not Self.tcpServer.Active) then
    Exit();

  F_Main.P_Server.Hint := 'Bridge server: vypínám...';
  F_Debug.Log('Bridge server: vypínám...');
  F_Main.P_Server.Color := clGray;

  with Self.tcpServer.Contexts.LockList do
    try
      for var iA := Count - 1 downto 0 do
      begin
        var Context: TIdContext := Items[iA];
        if Context = nil then
          Continue;
        Context.Connection.IOHandler.WriteBufferClear;
        Context.Connection.IOHandler.InputBuffer.Clear;
        Context.Connection.IOHandler.Close;
        if Context.Connection.Connected then
          Context.Connection.Disconnect;
      end;
    finally
      Self.tcpServer.Contexts.UnlockList;
    end;

  Self.tcpServer.Active := false;

  F_Main.P_Server.Color := clRed;
  F_Main.P_Server.Hint := 'Bridge server: vypnut';
end;

/// /////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TTCPServer.OnTcpServerConnect(AContext: TIdContext);
begin
  Self.tcpServer.Contexts.LockList();
  try
    AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;
    F_Debug.Log('Bridge: client connected');
  finally
    Self.tcpServer.Contexts.UnlockList();
  end;
end;

// Udalost vyvolana pri odpojeni klienta
procedure TTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
begin
  Self.tcpServer.Contexts.LockList();
  try
    F_Debug.Log('Bridge: client disconnected');
  finally
    Self.tcpServer.Contexts.UnlockList();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.OnTcpServerExecute(AContext: TIdContext);
begin
  if (not AContext.Connection.Connected) then
    Exit;

  if (AContext.Connection.IOHandler.InputBufferIsEmpty) then
  begin
    IndySleep(1);
    Exit();
  end;

  readLock.Acquire();

  try
    // read data
    // data jsou schvalne globalni, aby se porad nevytvarela a nenicila dokola
    data := AContext.Connection.IOHandler.ReadLn();

    Self.parsed.Clear();
    ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

    if (Self.parsed.Count > 0) then
      Self.parsed[0] := UpperCase(Self.parsed[0])
    else
      Exit();

    try
      Self.Parse(AContext);
    except

    end;
  finally
    readLock.Release();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Parse(AContext: TIdContext);
begin
  if (parsed[0] = 'LOGIN') then
  begin
    if (not TCPClient.authorised) then
    begin
      TCPClient.toAuth.username := parsed[3];
      TCPClient.toAuth.password := parsed[4];

      try
        if (TCPClient.status = TPanelConnectionStatus.opened) then
          TCPClient.Auth()
        else
          TCPClient.Connect(parsed[1], StrToInt(parsed[2]));
      except
        on E: Exception do
          F_Main.LogMessage('Nelze se připojit k hJOPserveru: ' + E.Message);
      end;
    end;
  end
  else if ((parsed[0] = 'LOKO') or (parsed[0] = 'LOKO-RUC')) then
  begin
    // LOKO;slot;[addr;token];[addr;token];...
    if (not TCPClient.authorised) then
    begin
      Self.SendLn(AContext,
        'LOKO;err;1;uLI-daemon neautorizovan vuci hJOPserveru');
      Exit();
    end;
    if (not uLI.Connected) then
    begin
      Self.SendLn(AContext, 'LOKO;err;2;uLI-daemon nepripojen k uLI-master');
      Exit();
    end;
    if (not uLI.status.sense) then
    begin
      Self.SendLn(AContext, 'LOKO;err;3;uLI-master neni napajen');
      Exit();
    end;

    var slot := StrToInt(parsed[1]);
    if ((slot < 0) or (slot > uLI._SLOTS_CNT)) then
    begin
      Self.SendLn(AContext, 'LOKO;err;4;Slot mimo povoleny rozsah slotu');
      Exit();
    end;
    if (not uLI.sloty[slot].isMaus) then
    begin
      Self.SendLn(AContext,
        'LOKO;err;5;Rocomaus s timto cislem slotu neni pripojena k uLI');
      Exit();
    end;
    if (uLI.sloty[slot].isLoko) then
    begin
      Self.SendLn(AContext, 'LOKO;err;6;Slot ' + parsed[1] + ' obsazen');
      Exit();
    end;

    var data: TStrings := TStringList.Create();
    try
      for var i := 2 to parsed.Count - 1 do
      begin
        data.Clear();
        ExtractStringsEx([';'], [#13, #10], parsed[i], data);

        try
          uLI.sloty[slot].gui.P_status.Color := clAqua;
          uLI.sloty[slot].gui.P_status.Caption := '-';
          uLI.sloty[slot].gui.P_status.Hint :=
            'Přišel požadavek na autorizaci LOKO, autorizuji...';
          uLI.sloty[slot].sender := AContext;

          TCPClient.lokToSlotMap.AddOrSetValue(StrToInt(data[0]),
            TCPClient.SlotToAuth(slot, parsed[0] = 'LOKO-RUC'));
          TCPClient.SendLn('-;LOK;' + data[0] + ';PLEASE;' + data[1]);
        except

        end;
      end;
    finally
      data.Free();
    end;

  end
  else if (parsed[0] = 'SLOTS?') then
  begin
    if ((not TCPClient.authorised) or (not uLI.Connected) or
      (not uLI.status.sense)) then
    begin
      // SLOTS;[addr/-/#];[addr/-/#];...
      Self.SendLn(AContext, 'SLOTS;');
    end
    else
    begin
      var tmp := '';
      for var i := 1 to uLI._SLOTS_CNT do
      begin
        if (uLI.sloty[i].isMaus) then
        begin
          if (uLI.sloty[i].isLoko) then
            tmp := tmp + 'F;'
          else
            tmp := tmp + '-;'
        end
        else
          tmp := tmp + '#;';
      end;
      Self.SendLn(AContext, 'SLOTS;' + tmp);
    end; // else no slots

  end
  else if (parsed[0] = 'AUTH?') then
  begin
    if (TCPClient.authorised) then
      Self.SendLn(AContext, 'AUTH;yes')
    else if ((uLI.Connected) and (uLI.statusValid)) then
      Self.SendLn(AContext, 'AUTH;no')
    else
      Self.SendLn(AContext, 'AUTH;cannot');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTCPServer.IsOpenned(): boolean;
begin
  Result := Self.tcpServer.Active;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.SendLn(AContext: TIdContext; str: string);
begin
  // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
  try
    AContext.Connection.IOHandler.WriteLn(str);
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.BroadcastData(data: string);
var
  Context: TIdContext;
begin
  try
    for Context in TList<TIdContext>(Self.tcpServer.Contexts.LockList()) do
      Self.SendLn(Context, data);
  finally
    Self.tcpServer.Contexts.UnlockList();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.DisconnectClient(conn: TIdContext);
begin
  conn.Connection.Disconnect();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.BroadcastSlots();
var
  str: string;
begin
  if (not Self.tcpServer.Active) then
    Exit();

  str := 'SLOTS;';

  if ((TCPClient.authorised) and (uLI.Connected) and (uLI.status.sense)) then
  begin
    for var i := 1 to uLI._SLOTS_CNT do
    begin
      if (uLI.sloty[i].isMaus) then
      begin
        if (uLI.sloty[i].isLoko) then
          str := str + 'F;'
        else
          str := str + '-;'
      end
      else
        str := str + '#;';
    end;
  end; // else no slots

  Self.BroadcastData(str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.BroadcastAuth(onlyChanges: boolean = false);
var
  newAuth: TAuthStatus;
begin
  if (TCPClient.authorised) then
    newAuth := yes
  else if ((uLI.Connected) and (uLI.statusValid)) then
    newAuth := no
  else
    newAuth := cannot;

  if ((newAuth = Self.lastAuth) and (onlyChanges)) then
    Exit();
  Self.lastAuth := newAuth;

  case (newAuth) of
    yes:
      Self.BroadcastData('AUTH;yes');
    no:
      Self.BroadcastData('AUTH;no');
    cannot:
      Self.BroadcastData('AUTH;cannot');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

tcpServer := TTCPServer.Create;

finalization

FreeAndNil(tcpServer);

end.// unit
