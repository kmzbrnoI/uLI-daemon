unit tUltimateLI;

{
  Trida TuLI je rozhranim k uLI.
}

interface

uses SysUtils, CPort, Forms, tUltimateLIConst, Classes, Registry, Windows,
     ExtCtrls, mausSlot;

type
  TBuffer = record
   data:array [0..255] of Byte;
   Count:Integer;
  end;

  TuLIStatus = record
   sense: boolean;
   transistor: boolean;
   aliveReceiving: boolean;
   aliveSending: boolean;
  end;

  EuLIStatusInvalid = class(Exception);
  EPowerTurnedOff = class(Exception);

  TuLI = class
    private const
      _DEF_ULI_STATUS : TuLIStatus = (
        sense: false;
        transistor: false;
        aliveReceiving: true;
        aliveSending: true;
      );

      _CPORT_BAUDRATE    = br19200;
      _CPORT_STOPBITS    = sbOneStopBit;
      _CPORT_DATABITS    = dbEight;
      _CPORT_FLOWCONTROL = fcNone;

      _BUF_IN_TIMEOUT_MS = 300;                                                 // timeout vstupniho bufferu v ms (po uplynuti timeoutu dojde k vymazani bufferu) - DULEZITY SAMOOPRAVNY MECHANISMUS!
                                                                                // pro spravnou funkcnost musi byt < _TIMEOUT_MSEC

      _DEVICE_DESCRIPTION = 'uLI - master';

      _KA_SEND_PERIOD_MS = 1000;
      _KA_RECEIVE_TIMEOUT_TICKS = 6;
      _KA_RECEIVE_PERIOD_MS = 500;

      _SLOTS_CNT = 6;
      _DEFAULT_DCC = true;

      _BROADCAST_HEADER : ShortString = #$60;
      _CMD_DCC_ON : ShortString = #$61#$01;
      _CMD_DCC_OFF : ShortString = #$61#$00;

    private
     ComPort: TComPort;

     tKASendTimer: TTimer;
     tKAReceiveTimer: TTimer;

     KAreceiveTimeout: Integer;

     Fbuf_in: TBuffer;
     Fbuf_in_timeout: TDateTime;

     uLIStatusValid: boolean;
     uLIStatus: TuLIStatus;

     fLogLevel: TuLILogLevel;
     fDCC : boolean;

     sloty: array [1.._SLOTS_CNT] of TSlot;

     // events
     fOnLog: TuLILogEvent;

      procedure OntKASendTimer(Sender:TObject);
      procedure OntKAReceiveTimer(Sender:TObject);

      procedure OnComError(Sender: TObject; Errors: TComErrors);
      procedure OnComException(Sender: TObject;
         TComException: TComExceptions; ComportMessage: string; WinError: Int64;
         WinMessage: string);
      procedure ComBeforeOpen(Sender:TObject);
      procedure ComAfterOpen(Sender:TObject);
      procedure ComBeforeClose(Sender:TObject);
      procedure ComAfterClose(Sender:TObject);
      procedure ComRxChar(Sender: TObject; Count: Integer);

      procedure CheckFbufInTimeout();
      procedure ParseComMsg(var msg: TBuffer);
      procedure ParseDeviceMsg(deviceAddr:Byte; var msg: TBuffer);
      procedure ParseuLIMsg(var msg: TBuffer);
      procedure ParseuLIStatus(var msg:TBuffer);

      procedure WriteLog(lvl:TuLILogLevel; msg:string);

      procedure Send(data:TBuffer);
      procedure SendKeepAlive();
      procedure SendStatusRequest();

      procedure SetLogLevel(new:TuLILogLevel);

      function CreateBuf(str:ShortString):TBuffer;

      procedure SetBusActive(new:boolean);
      procedure SetDCC(new:boolean);

      function LokAddrEncode(addr: Integer): Word; inline;                        // ctyrmistna adresa lokomotivy do dvou bytu
      function LokAddrDecode(ah, al: byte): Integer; inline;                      // ctyrmistna adresa lokomotivy ze dvou bajtu do klasickeho cisla
      function LokAddrToBuf(addr: Integer): ShortString;                          // adresa to bufferu

    public

      constructor Create();
      destructor Destroy(); override;

      procedure Open(port:string);
      procedure Close();

      procedure EnumDevices(const Ports: TStringList);

      procedure SetStatus(new:TuLIStatus);

      property OnLog: TuLILogEvent read fOnLog write fOnLog;
      property logLevel: TuLILogLevel read fLogLevel write SetLogLevel;
      property busEnabled: boolean read uLIStatus.transistor write SetBusActive;
      property DCC : boolean read fDCC write SetDCC;

  end;

var
  uLI : TuLI;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TuLI.Create();
var i:Integer;
begin
 inherited;

 Self.uLIStatus := _DEF_ULI_STATUS;

 Self.ComPort := TComPort.Create(nil);
 Self.ComPort.BaudRate                := _CPORT_BAUDRATE;
 Self.ComPort.StopBits                := _CPORT_STOPBITS;
 Self.ComPort.DataBits                := _CPORT_DATABITS;
 Self.ComPort.FlowControl.FlowControl := _CPORT_FLOWCONTROL;

 Self.ComPort.OnError       := Self.OnComError;
 Self.ComPort.OnException   := Self.OnComException;
 Self.ComPort.OnBeforeOpen  := Self.ComBeforeOpen;
 Self.ComPort.OnAfterOpen   := Self.ComAfterOpen;
 Self.ComPort.OnBeforeClose := Self.ComBeforeClose;
 Self.ComPort.OnAfterClose  := Self.ComAfterClose;
 Self.ComPort.OnRxChar      := Self.ComRxChar;

 Self.tKASendTimer          := TTimer.Create(nil);
 Self.tKASendTimer.Interval := _KA_SEND_PERIOD_MS;
 Self.tKASendTimer.Enabled  := false;
 Self.tKASendTimer.OnTimer  := Self.OntKASendTimer;

 Self.tKAReceiveTimer          := TTimer.Create(nil);
 Self.tKAReceiveTimer.Interval := _KA_RECEIVE_PERIOD_MS;
 Self.tKAReceiveTimer.Enabled  := false;
 Self.tKAReceiveTimer.OnTimer  := Self.OntKAReceiveTimer;

 Self.uLIStatusValid := false;
 Self.fDCC := _DEFAULT_DCC;

 for i := 1 to _SLOTS_CNT do Self.sloty[i] := TSlot.Create(i);
end;

destructor TuLI.Destroy();
var i:Integer;
begin
 for i := 1 to _SLOTS_CNT do FreeAndNil(Self.sloty[i]);

 Self.tKASendTimer.Free();
 Self.tKAReceiveTimer.Free();
 Self.ComPort.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.WriteLog(lvl:TuLILogLevel; msg:string);
begin
 if ((lvl <= Self.logLevel) and (Assigned(Self.OnLog))) then
   Self.OnLog(self, lvl, msg);
end;

////////////////////////////////////////////////////////////////////////////////
// COM port events:

procedure TuLI.OnComError(Sender: TObject; Errors: TComErrors);
begin
 Self.WriteLog(tllErrors, 'ERR: COM PORT ERROR');
end;

procedure TuLI.OnComException(Sender: TObject;
   TComException: TComExceptions; ComportMessage: string; WinError: Int64;
   WinMessage: string);
begin
 Self.WriteLog(tllErrors, 'ERR: COM PORT EXCEPTION: '+ComportMessage+'; '+WinMessage);
 raise Exception.Create(ComportMessage);
end;

procedure TuLI.ComBeforeOpen(Sender:TObject);
begin
 Self.uLIStatusValid := false;
end;

procedure TuLI.ComAfterOpen(Sender:TObject);
begin
 Self.WriteLog(tllCommands, 'OPEN OK');

 // reset uLI status
 Self.uLIStatus := _DEF_ULI_STATUS;
 Self.SetStatus(Self.uLIStatus);
end;

procedure TuLI.ComBeforeClose(Sender:TObject);
begin
 Self.tKASendTimer.Enabled    := false;
 Self.tKAReceiveTimer.Enabled := false;
 Self.uLIStatusValid := false;
 Self.uLIStatus := _DEF_ULI_STATUS;
end;

procedure TuLI.ComAfterClose(Sender:TObject);
begin
 Self.WriteLog(tllCommands, 'CLOSE OK');
 Self.uLIStatusValid := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.Open(port:string);
begin
 if (Self.ComPort.Connected) then Exit();
 Self.ComPort.Port := port;

 Self.WriteLog(tllCommands, 'OPENING port='+port+' br='+BaudRateToStr(Self.ComPort.BaudRate)+' sb='+StopBitsToStr(Self.ComPort.StopBits)+' db='+DataBitsToStr(Self.ComPort.DataBits)+' fc='+FlowControlToStr(Self.ComPort.FlowControl.FlowControl));

 try
   Self.ComPort.Open();
 except
   on E : Exception do
    begin
     Self.ComPort.Close();
     Self.ComAfterClose(Self);
     // TODO: log exception
    end;
 end;
end;

procedure TuLI.Close();
begin
 if (not Self.ComPort.Connected) then Exit();

 Self.WriteLog(tllCommands, 'CLOSING');

 try
   Self.ComPort.Close();
 except
   // TODO: log exception
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.CheckFbufInTimeout();
begin
 if ((Self.Fbuf_in_timeout < Now) and (Self.Fbuf_in.Count > 0)) then
  begin
   WriteLog(tllErrors, 'INPUT BUFFER TIMEOUT, removing buffer');
   Self.Fbuf_in.Count := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ComRxChar(Sender: TObject; Count: Integer);
var
  ok, parity: Boolean;
  msg_len: Integer;
  x: byte;
  i,tmp: Integer;
  Buf:array [0..255] of Byte;
  s:string;
begin
  // check timeout
  Self.CheckFbufInTimeout();

  Self.ComPort.Read(buf, Count);

  for i := 0 to Count-1 do Fbuf_in.data[Fbuf_in.Count+i] := Buf[i];
  Fbuf_in.Count := Fbuf_in.Count + Count;
  Fbuf_in_timeout := Now + EncodeTime(0, 0, _BUF_IN_TIMEOUT_MS div 1000, _BUF_IN_TIMEOUT_MS mod 1000);

  // TODO: no not construct data for log when loglevel is low

  s := 'BUF: ';
  for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
  WriteLog(tllDetail, s);

  ok := true;
  while (ok) do
   begin
    if (Fbuf_in.Count >= 2) then
     begin
      // msg_len is length with all bytes (call, header, data, xor)
      msg_len := (Fbuf_in.data[1] AND $0F) + 3;
      if (msg_len <= Fbuf_in.Count) then
       begin
        // check first byte parity
        x := Fbuf_in.data[0];
        parity := false;
        for i := 0 to 7 do
         begin
          if ((x AND 1) = 1) then parity := not parity;
          x := x shr 1;
         end;

        if (not parity) then
         begin
          // parity ok -> check xor
          x := 0;
          for i := 1 to msg_len-2 do x := x xor Fbuf_in.data[i];

          if (x = Fbuf_in.data[msg_len-1]) then
           begin
            // parse one message
            tmp := Fbuf_in.Count;
            Fbuf_in.Count := msg_len;
            Self.ParseComMsg(Fbuf_in);
            Fbuf_in.Count := tmp;
           end else begin
            // xor error
            s := '';
            for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
            WriteLog(tllErrors, 'GET: XOR ERROR, removing buffer : '+s);
           end;
         end else begin
          // parity error
          s := '';
          for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
          WriteLog(tllErrors, 'GET: PARITY ERROR, removing buffer : '+s);
         end;

        // TODO: send "transfer errors" ???

        // remove message from buffer
        for i := 0 to Fbuf_in.Count-msg_len-1 do Fbuf_in.data[i] := Fbuf_in.data[i+msg_len];
        Fbuf_in.Count := Fbuf_in.Count - msg_len;

        s := 'BUF: ';
        for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
        if (Fbuf_in.Count > 0) then WriteLog(tllDetail, s);

       end else ok := false;
     end else ok := false;
   end;//while
end;//procedur

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseComMsg(var msg: TBuffer);
var target:Byte;
    s:string;
    i:Integer;
begin
 s := '';
 for i := 0 to msg.Count-1 do s := s + IntToHex(msg.data[i], 2) + ' ';
 Self.WriteLog(tllData, 'GET: '+s);

 try
   target := (msg.data[0] shr 5) AND 3;
   if (target = 3) then
     Self.ParseDeviceMsg((msg.data[0] AND $1F), msg)
   else if (target = 1) then
     Self.ParseuLIMsg(msg)
   else begin
     // TODO: unknown message
   end;

 except
   // TODO
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseDeviceMsg(deviceAddr:Byte; var msg: TBuffer);
var toSend: ShortString;
    tmp, tmp2: Byte;
    addr: Word;
    i: Integer;
begin
 case (msg.data[1]) of
   $21: begin
     case (msg.data[2]) of
       $21: begin
         Self.WriteLog(tllCommands, 'GET: command station software version request');
         Self.WriteLog(tllCommands, 'SEND: command station software version');
         Self.Send(CreateBuf(ShortString(chr(msg.data[0])+#$63+#$21+#$36+#$00)));
       end;

       $24: begin
         Self.WriteLog(tllCommands, 'GET: command station status request');
         Self.WriteLog(tllCommands, 'SEND: command station staus');
         Self.Send(CreateBuf(ShortString(chr(msg.data[0])+#$62+#$22+(char(not Self.DCC)))));
       end;

       $81: begin
         Self.WriteLog(tllCommands, 'GET: resume operations request');
         Self.WriteLog(tllCommands, 'PUT: GO');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
       end;

       $80: begin
         Self.WriteLog(tllCommands, 'GET: STOP operations request');

         // TODO: zastavit hnaci vozidlo

         Self.WriteLog(tllCommands, 'PUT: STOP');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
         Self.WriteLog(tllCommands, 'PUT: GO');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
       end;
     end;// case msg.data[2]
   end;//$21

   $E3: begin
     case (msg.data[2]) of
       00: begin
         Self.WriteLog(tllCommands, 'GET: locomotive information request');

         toSend := AnsiChar(msg.data[0]) + #$E4;

         addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);
         if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko) or
             (Self.sloty[addr].HV.ukradeno)) then
          begin
           // lokomotiva neni rizena ovladacem
           toSend := toSend + #$A + #$80 + #0 + #0;
          end else begin
           // lokomotiva je rizena ovladacem
           toSend := toSend + #2;

           // rychlost + smer
           case (Self.sloty[addr].HV.rychlost_stupne) of
              0: tmp2 := 0;
              1..28: tmp2 := Self.sloty[addr].HV.rychlost_stupne+3;
              else tmp2 := 0;
           end;

           tmp := ((Self.sloty[addr].HV.smer AND $1) shl 7) +
                  ((tmp2 AND $1e) shr 1) +
                  ((tmp2 AND $01) shl 4);

           toSend := toSend + AnsiChar(tmp);

           // F0 - F4
           tmp := (byte(Self.sloty[addr].HV.funkce[0]) shl 4);
           for i := 1 to 4 do tmp := tmp + (byte(Self.sloty[addr].HV.funkce[i]) shl (i-1));
           toSend := toSend + AnsiChar(tmp);

           // F5 - F12
           for i := 5 to 12 do tmp := tmp + (byte(Self.sloty[addr].HV.funkce[i]) shl (i-5));
           toSend := toSend + AnsiChar(tmp);
          end;

         Self.WriteLog(tllCommands, 'PUT: locomotive information');
         Self.Send(CreateBuf(toSend));
       end;
     end;
   end;
 end;//case msg.data[1]
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseuLIMsg(var msg: TBuffer);
begin
 case (msg.data[1]) of
  $01: begin
    // informative messages
    case (msg.data[2]) of
     $01: Self.WriteLog(tllErrors, 'ERR: GET: USB incoming data timeout');
     $02: Self.WriteLog(tllErrors, 'ERR: GET: USART incoming data timeout');
     $03: Self.WriteLog(tllErrors, 'ERR: GET: Unknown command');
     $04: Self.WriteLog(tllCommands, 'GET: OK');
     $05: Self.WriteLog(tllChanges, 'GET: keep-alive');
     $06: Self.WriteLog(tllErrors, 'ERR: GET: USB>USART buffer overflow');
     $07: Self.WriteLog(tllErrors, 'ERR: GET: USB XOR error');
     $08: Self.WriteLog(tllErrors, 'ERR: GET: USB parity error');
     $09: Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power source turned off');
     $0A: Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power transistor closed');
    end;
  end;

  $11: begin
    // uLI-master status response
    Self.WriteLog(tllCommands, 'GET: master status');
    Self.ParseuLIStatus(msg);
  end;
 end;//case msg.data[1]
end;//procedure

procedure TuLI.ParseuLIStatus(var msg:TBuffer);
var new:TuLIStatus;
begin
 new.transistor     := boolean(msg.data[2] and 1);
 new.sense          := boolean((msg.data[2] shr 1) and 1);
 new.aliveReceiving := boolean((msg.data[2] shr 2) and 1);
 new.aliveSending   := boolean((msg.data[2] shr 3) and 1);

 // prijimani a odesilani schvalne obraceno
 // (v recordu jsou data z pohledu uLI-master, timery jsou z pohledu SW v pocitaci)
 Self.tKASendTimer.Enabled    := new.aliveReceiving;
 Self.tKAReceiveTimer.Enabled := new.aliveSending;
 Self.KAreceiveTimeout := 0;

 Self.uLIStatus      := new;
 Self.uLIStatusValid := true;
end;

////////////////////////////////////////////////////////////////////////////////

// Tato funkce funguje jako blokujici.
// Z funkce je vyskoceno ven az po odeslani dat (nebo vyjimce).
// Tato funckce ocekava vstupni data bez XORu na konci (prida ho sama).
procedure TuLI.Send(data:TBuffer);
var
  x: byte;
  i: integer;
  log:string;
  asp: PAsync;
begin
  if (not Self.ComPort.Connected) then
   begin
    Self.WriteLog(tllErrors, 'PUT ERR: uLI not connected');
    Exit;
   end;
  if (data.Count > 18) then
   begin
    Self.WriteLog(tllErrors, 'PUT ERR: Message too long');
    Exit;
   end;

  //xor
  x := 0;
  for i := 1 to data.Count-1 do x := x xor data.data[i];
  Inc(data.Count);
  data.data[data.Count-1] := x;

  //get string for log
  log := '';
  for i := 0 to data.Count-1 do log := log + IntToHex(data.data[i],2) + ' ';
  Self.WriteLog(tllData, 'PUT: '+log);

  try
    InitAsync(asp);
    Self.ComPort.WriteAsync(data.data, data.Count, asp);
    while (not Self.ComPort.IsAsyncCompleted(asp)) do
     begin
      Application.ProcessMessages();
      Sleep(1);
     end;
  except
   on E : Exception do
    begin
     Self.WriteLog(tllErrors, 'PUT ERR: com object error : '+E.Message);
     if (Self.ComPort.Connected) then Self.Close();
    end;
  end;

 DoneAsync(asp);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetLogLevel(new:TuLILogLevel);
begin
 Self.fLogLevel := new;
 Self.WriteLog(tllCommands, 'NEW LOGLEVEL: '+IntToStr(Integer(new)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.EnumDevices(const Ports: TStringList);
var
  nInd:  Integer;
begin  { EnumComPorts }
  with  TRegistry.Create(KEY_READ)  do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if  OpenKey('hardware\devicemap\serialcomm', False)  then
        try
          Ports.BeginUpdate();
          try
            GetValueNames(Ports);
            for  nInd := Ports.Count - 1  downto  0  do
              Ports.Strings[nInd] := ReadString(Ports.Strings[nInd]);
            Ports.Sort()
          finally
            Ports.EndUpdate()
          end { try-finally }
        finally
          CloseKey()
        end { try-finally }
      else
        Ports.Clear()
    finally
      Free()
    end { try-finally }
end { EnumComPorts };

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetStatus(new:TuLIStatus);
var data:Byte;
begin
 Self.WriteLog(tllCommands, 'PUT: status');
 data := $A0 + Integer(new.transistor) + (Integer(new.aliveReceiving) shl 2) +
          (Integer(new.aliveSending) shl 3);
 Self.Send(CreateBuf(ShortString(#$A0+#$11+AnsiChar(data))));
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.CreateBuf(str:ShortString):TBuffer;
var i:Integer;
begin
 Result.Count := Length(str);
 for i := 0 to Result.Count-1 do
   Result.data[i] := ord(str[i+1]);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.OntKASendTimer(Sender:TObject);
begin
 Self.SendKeepAlive();
end;

procedure TuLI.OntKAReceiveTimer(Sender:TObject);
begin
 Inc(Self.KAreceiveTimeout);
 if (Self.KAreceiveTimeout > _KA_RECEIVE_TIMEOUT_TICKS) then
  begin
   // uLI-master neodpovedelo -> co delat?
   // TODO
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendKeepAlive();
begin
 Self.WriteLog(tllChanges, 'SEND: keep-alive');
 Self.Send(CreateBuf(#$A0+#$01+#$05));
end;

procedure TuLI.SendStatusRequest();
begin
 Self.WriteLog(tllChanges, 'SEND: status request');
 Self.Send(CreateBuf(#$A0+#$11+#$A2));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetBusActive(new:boolean);
var newStatus:TuLIStatus;
begin
 if (not Self.uLIStatusValid) then
  begin
   Self.SendStatusRequest();
   raise EuLIStatusInvalid.Create('Program nemá validní data o stavu uLI');
  end;

 if (not self.uLIStatus.sense) then
   raise EPowerTurnedOff.Create('Napájení sbìrnice je vypnuto');

 newStatus := Self.uLIStatus;
 newStatus.transistor := new;
 Self.SetStatus(newStatus);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetDCC(new:boolean);
begin
 if (Self.fDCC = new) then Exit();
 Self.fDCC := new;

 if (Self.ComPort.Connected) then
  begin
   if (new) then
    begin
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_ON + _BROADCAST_HEADER + _CMD_DCC_ON));
    end else begin
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_OFF + _BROADCAST_HEADER + _CMD_DCC_OFF));
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.LokAddrEncode(addr: Integer): Word;
begin
  if (addr > 99) then begin
    Result := (addr + $C000);
   end else begin
    Result := addr;
  end;
end;

function TuLI.LokAddrToBuf(addr: Integer):ShortString;
var encoded:Word;
begin
  encoded := Self.LokAddrEncode(addr);
  Result := AnsiChar(Hi(encoded)) + AnsiChar(Lo(encoded));
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.LokAddrDecode(ah, al: byte): Integer;
begin
  Result := al or ((ah AND $3F) shl 8);
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
  uLI := TuLI.Create();
finalization
  FreeAndNil(uLI);

end.
