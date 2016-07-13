unit server;

{
  Bridge TCP server pro komunikaci s panely, ktere daemonovi davaji lokomotivy
  k rizeni.
}

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal,
     Classes, StrUtils, tUltimateLIConst, Graphics, Windows,
     IdContext, ComCtrls, IdSync;

const
  _BRIDGE_DEFAULT_PORT = 5733;                                                  // default port, na ktere bezi bridge server
  _MAX_BRIDGE_CLIENTS = 32;                                                     // maximalni pocet klientu

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  EServerAlreadyStarted = class(Exception);

  // jeden klient:
  TBridgeClient = class
    conn:TIdContext;                                                            // fyzicke spojeni
    status:TPanelConnectionStatus;                                              // stav spojeni
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane
  end;

  TTCPServer = class
   private const
    _PROTOCOL_VERSION = '1.0';

   private
    clients:array[0.._MAX_BRIDGE_CLIENTS] of TBridgeClient;                     // databaze klientu
    tcpServer: TIdTCPServer;                                                    // object serveru
    parsed: TStrings;                                                           // naparsovana data, implementovano jako globalni promenna pro zrychleni
    data:string;                                                                // prijata data v plain-text forme
    fport:Word;                                                                 // aktualni port serveru

     procedure OnTcpServerConnect(AContext: TIdContext);                        // event pripojeni klienta z TIdTCPServer
     procedure OnTcpServerDisconnect(AContext: TIdContext);                     // event odpojeni klienta z TIdTCPServer
     procedure OnTcpServerExecute(AContext: TIdContext);                        // event akce klienta z TIdTCPServer

     procedure Parse(AContext: TIdContext);                                     // parsinag dat s globalnim prefixem: "-;"

     function IsOpenned():boolean;                                              // je server zapnut?

   public

     constructor Create();
     destructor Destroy(); override;

     procedure Start(port:Word); overload;                                      // spustit server
     procedure Start(); overload;                                               // spustit server
     procedure Stop();                                                          // zastavit server
     procedure DisconnectClient(conn:TIdContext);                               // odpojit konkretniho klienta

     procedure BroadcastData(data:string);

     procedure SendLn(AContext:TIDContext; str:string);

     function GetClient(index:Integer):TBridgeClient;

      property openned:boolean read IsOpenned;
      property port:Word read fport write fport;
  end;//TPanelTCPClient

var
  TCPServer : TTCPServer;

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

LOGIN;server;port;username;password      - pozadavek k pripojeni k serveru a autorizaci regulatoru
LOKO;addr;token;slot                     - pozadavek k umisteni loko \addr do slotu \slot
SLOTS?                                   - pozadavek na vraceni seznamu slotu a jejich obsahu

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// SERVER -> KLIENT ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

LOKO;addr;ok                             - loko uspesne prevzato
LOKO;addr;err_code;error message         - loko se nepodarilo prevzit
SLOTS;[addr/-/#];[addr/-/#];...          - sloty, ktere ma daemon k dispozici
                                           '-' je prazdny slot
                                           '#' je nefunkcni slot
                                           \addr je adresa v plnem slotu
                                           pocet slotu je variabilni

}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{
 Navazani komunikace:
  TODO
}

uses fMain, fDebug, client, tUltimateLI;

////////////////////////////////////////////////////////////////////////////////

constructor TTCPServer.Create();
var i:Integer;
begin
 inherited;

 Self.fport := _BRIDGE_DEFAULT_PORT;

 for i := 0 to _MAX_BRIDGE_CLIENTS-1 do
  Self.clients[i] := nil;

 Self.parsed := TStringList.Create;

 Self.tcpServer := TIdTCPServer.Create(nil);
 Self.tcpServer.OnConnect    := Self.OnTcpServerConnect;
 Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
 Self.tcpServer.OnExecute    := Self.OnTcpServerExecute;
end;//ctor

destructor TTCPServer.Destroy();
begin
 if (Self.tcpServer.Active) then
  Self.tcpServer.Active := false;

 if (Assigned(Self.tcpServer)) then
   FreeAndNil(Self.tcpServer);

 if (Assigned(Self.parsed)) then
   FreeAndNil(Self.parsed);

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Start(port:Word);
begin
 if (Self.tcpServer.Active) then Exit();

 F_Main.S_Server.Brush.Color := clYellow;
 F_Main.S_Server.Hint := 'Bridge server: spouštìní...';
 F_Debug.Log('Bridge server: spouštìní...');

 Self.tcpServer.DefaultPort := port;
 Self.fport := port;

 try
  Self.tcpServer.Active := true;
 except
  on E:Exception do
   begin
    F_Main.S_Server.Brush.Color := clRed;
    F_Main.S_Server.Hint := 'ERR: Panel server: chyba pøi startování serveru : '+E.Message;
    F_Debug.Log('ERR: Panel server: chyba pøi startování serveru : '+E.Message);
    raise;
   end;
 end;

 F_Main.S_Server.Brush.Color := clLime;
 F_Main.S_Server.Hint := 'Bridge server: spuštìn';
 F_Debug.Log('Bridge server: spuštìn');
end;

procedure TTCPServer.Start();
begin
 Self.Start(Self.port);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Stop();
begin
 if (not Self.tcpServer.Active) then Exit();

 F_Main.S_Server.Hint := 'Bridge server: vypínám...';
 F_Debug.Log('Bridge server: vypínám...');
 F_Main.S_Server.Brush.Color := clGray;

 Self.tcpServer.Active := false;

 F_Main.S_Server.Brush.Color := clRed;
 F_Main.S_Server.Hint := 'Bridge server: vypnut';
end;

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TTCPServer.OnTcpServerConnect(AContext: TIdContext);
var i:Integer;
begin
 AContext.Connection.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;

 for i := 0 to _MAX_BRIDGE_CLIENTS-1 do
  if (Self.clients[i] = nil) then
   break;

 // na serveru neni misto -> odpojit klienta
 if (i = _MAX_BRIDGE_CLIENTS) then
  begin
   // tady bych mohl napsat chybovou hlasku
   AContext.Connection.Disconnect();
   Exit();
  end;

 Self.clients[i]        := TBridgeClient.Create();
 Self.clients[i].conn   := AContext;
 Self.clients[i].status := TPanelConnectionStatus.handshake;

 F_Debug.Log('Bridge: client connected');
end;//procedure

// Udalost vyvolana pri odpojeni klienta
procedure TTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
var i:Integer;
begin
 // vymazeme klienta z databaze klientu
 for i := 0 to _MAX_BRIDGE_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].conn)) then
   begin
    FreeAndNil(Self.clients[i]);
    break;
   end;

 F_Debug.Log('Bridge: client disconnected');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.OnTcpServerExecute(AContext: TIdContext);
begin
 if (not AContext.Connection.Connected) then Exit;

 if (AContext.Connection.IOHandler.InputBufferIsEmpty) then
  begin
   IndySleep(1);
   Exit();
  end;

 //read data
 // data jsou schvalne globalni, aby se porad nevytvarela a nenicila dokola
 data := AContext.Connection.IOHandler.ReadLn();

 Self.parsed.Clear();
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 try
   Self.Parse(AContext);
 except

 end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Parse(AContext: TIdContext);
var slot, i:Integer;
    tmp:string;
begin
 if (parsed[0] = 'LOGIN') then begin
   if (not TCPClient.authorised) then
    begin
     TCPClient.toAuth.username := parsed[3];
     TCPClient.toAuth.password := parsed[4];

     TCPClient.Connect(parsed[1], StrToInt(parsed[2]));
    end;
 end else if (parsed[0] = 'LOKO') then begin
   // LOKO;addr;token;slot
   if (not TCPClient.authorised) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';1;uLI-daemon neautorizovan vuci hJOPserveru');
     Exit();
    end;
   if (not uLI.connected) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';2;uLI-daemon nepripojen k uLI-master');
     Exit();
    end;
   if (not uLI.status.sense) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';3;uLI-master neni napajen');
     Exit();
    end;

   slot := StrToInt(parsed[3]);
   if ((slot < 0) or (slot > uLI._SLOTS_CNT)) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';4;Slot mimo povoleny rozsah slotu');
     Exit();
    end;
   if (not uLI.sloty[slot].isMaus) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';5;Rocomaus s timto cislem slotu neni pripojena k uLI');
     Exit();
    end;
   if (uLI.sloty[slot].isLoko) then
    begin
     Self.SendLn(AContext, 'LOKO;'+parsed[1]+';6;Slot '+parsed[3]+' obsazen');
     Exit();
    end;

   TCPClient.lokToSlotMap.AddOrSetValue(StrToInt(parsed[1]), slot);
   TCPClient.SendLn('-;LOK;'+parsed[1]+';PLEASE;'+parsed[2]);

 end else if (parsed[0] = 'SLOTS?') then begin
   if ((not TCPClient.authorised) or (not uLI.connected) or (not uLI.status.sense)) then
    begin
     // SLOTS;[addr/-/#];[addr/-/#];...
     Self.SendLn(AContext, 'SLOTS;');
    end else begin
     tmp := '';
     for i := 1 to uLI._SLOTS_CNT do
      begin
       if (uLI.sloty[i].isMaus) then begin
         if (uLI.sloty[i].isLoko) then
           tmp := tmp + IntToStr(uLI.sloty[i].HV.Adresa) + ';'
         else
           tmp := tmp + '-;'
       end else
         tmp := tmp + '#;';
      end;
     Self.SendLn(AContext, 'SLOTS;' + tmp);
    end;//else no slots
 end;//"SLOTS?"
end;

////////////////////////////////////////////////////////////////////////////////

function TTCPServer.IsOpenned():boolean;
begin
 Result := Self.tcpServer.Active;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.SendLn(AContext:TIDContext; str:string);
begin
 // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
 try
   AContext.Connection.IOHandler.WriteLn(str);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.BroadcastData(data:string);
var i:Integer;
begin
 for i := 0 to _MAX_BRIDGE_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.SendLn(Self.clients[i].conn, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.DisconnectClient(conn:TIdContext);
begin
 conn.Connection.Disconnect();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TTCPServer.GetClient(index:Integer):TBridgeClient;
begin
 if (index < _MAX_BRIDGE_CLIENTS) then
   Result := Self.clients[index]
 else
   Result := nil;
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
 TCPServer := TTCPServer.Create;

finalization
 FreeAndNil(TCPServer);

end.//unit
