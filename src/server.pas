unit server;

{
  TCP server pro komunikaci s panely.
}

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal,
     Classes, StrUtils, tUltimateLIConst, Graphics, Windows,
     IdContext, ComCtrls, IdSync;

const
  _PANEL_DEFAULT_PORT = 5896;                                                   // default port, na ktere bezi server
  _MAX_OR_CLIENTS = 32;                                                         // maximalni pocet klientu

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  EServerAlreadyStarted = class(Exception);

  // jeden klient:
  TPanelClient = class
    conn:TIdContext;                                                            // fyzicke spojeni
    status:TPanelConnectionStatus;                                              // stav spojeni
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane
  end;

  TTCPServer = class
   private const
    _PROTOCOL_VERSION = '1.0';

   private
    clients:array[0.._MAX_OR_CLIENTS] of TPanelClient;                          // databaze klientu
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

     function GetClient(index:Integer):TPanelClient;

      property openned:boolean read IsOpenned;
      property port:Word read fport write fport;
  end;//TPanelTCPClient

var
  TCPServer : TTCPServer;

implementation

{
 Jak funguje komunikace ze strany serveru:
  TODO

}
{
 Specifikace komunikacniho protkolu:
  jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem
  prvni parametr je vzdy id oblasti rizeni, popr. '-' pokud se jedna o rezijni prikaz

 PRIKAZY:

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// KLIENT -> SERVER ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
/////////////////////////// SERVER -> KLIENT ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{
 Navazani komunikace:
  TODO
}

uses fMain, fDebug;

////////////////////////////////////////////////////////////////////////////////

constructor TTCPServer.Create();
var i:Integer;
begin
 inherited;

 Self.fport := _PANEL_DEFAULT_PORT;

 for i := 0 to _MAX_OR_CLIENTS-1 do
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
 F_Main.S_Server.Hint := 'Panel server: spouštìní...';
 F_Debug.Log('Panel server: spouštìní...');

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
 F_Main.S_Server.Hint := 'Panel server: spuštìn';
 F_Debug.Log('Panel server: spuštìn');
end;

procedure TTCPServer.Start();
begin
 Self.Start(Self.port);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Stop();
begin
 if (not Self.tcpServer.Active) then Exit();

 F_Main.S_Server.Hint := 'Panel server: vypínám...';
 F_Debug.Log('Panel server: vypínám...');
 F_Main.S_Server.Brush.Color := clGray;

 Self.tcpServer.Active := false;

 F_Main.S_Server.Brush.Color := clRed;
 F_Main.S_Server.Hint := 'Panel server: vypnut';

end;

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TTCPServer.OnTcpServerConnect(AContext: TIdContext);
var i:Integer;
begin
 AContext.Connection.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;

 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Self.clients[i] = nil) then
   break;

 // na serveru neni misto -> odpojit klienta
 if (i = _MAX_OR_CLIENTS) then
  begin
   // tady bych mohl napsat chybovou hlasku
   AContext.Connection.Disconnect();
   Exit();
  end;

 Self.clients[i]        := TPanelClient.Create();
 Self.clients[i].conn   := AContext;
 Self.clients[i].status := TPanelConnectionStatus.handshake;
end;//procedure

// Udalost vyvolana pri odpojeni klienta
procedure TTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
var i:Integer;
begin
 // vymazeme klienta z databaze klientu
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].conn)) then
   begin
    FreeAndNil(Self.clients[i]);
    break;
   end;
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

 if (Self.parsed.Count > 1) then Self.parsed[1] := UpperCase(Self.parsed[1]); 

 try
   Self.Parse(AContext);
 except

 end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.Parse(AContext: TIdContext);
begin
 { if (parsed[1] = 'STIT') then }
end;//procedure

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
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.SendLn(Self.clients[i].conn, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPServer.DisconnectClient(conn:TIdContext);
begin
 conn.Connection.Disconnect();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TTCPServer.GetClient(index:Integer):TPanelClient;
begin
 if (index < _MAX_OR_CLIENTS) then
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
