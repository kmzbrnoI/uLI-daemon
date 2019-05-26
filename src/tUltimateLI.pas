unit tUltimateLI;

{
  Trida TuLI je rozhranim k uLI.
}

interface

uses SysUtils, CPort, Forms, tUltimateLIConst, Classes, Registry, Windows,
     ExtCtrls, mausSlot, Graphics;

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

  TuLIVersion = record
    sw:string;
    hw:string;
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

      _DEF_ULI_VERSION : TuLIVersion = (
        sw: '';
        hw: '';
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

      _DEFAULT_DCC = true;

      _CMD_DCC_ON : ShortString = #$61#$01;
      _CMD_DCC_OFF : ShortString = #$61#$00;

      _KEEP_ALIVE : array[0..3] of Byte = ($A0, $01, $05, $04);

    public const
      _SLOTS_CNT = 6;
      _BROADCAST_HEADER : ShortString = #$60;
      _BROADCAST_CODE   : Byte = $60;

    private
     ComPort: TComPort;

     tKASendTimer: TTimer;
     tKAReceiveTimer: TTimer;

     KAreceiveTimeout: Integer;

     Fbuf_in: TBuffer;
     Fbuf_in_timeout: TDateTime;

     uLIStatusValid: boolean;
     uLIStatus: TuLIStatus;
     uLIVersion: TuLIVersion;

     fLogLevel: TuLILogLevel;
     fDCC : boolean;

     ffusartMsgTotalCnt: Cardinal;
     ffusartMsgTimeoutCnt: Cardinal;

     // events
     fOnLog: TuLILogEvent;
     fOnUsartMsgCntChange: TNotifyEvent;

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

      function FindSlot(mausId:Byte):Integer;
      function GetConnected():boolean;
      function GetBusActive():boolean;

      procedure SetUsartMsgTotalCnt(new:Cardinal);
      procedure SetUsartMsgTimeoutCnt(new:Cardinal);

      property fusartMsgTotalCnt: Cardinal read ffusartMsgTotalCnt write SetUsartMsgTotalCnt;
      property fusartMsgTimeoutCnt: Cardinal read ffusartMsgTimeoutCnt write SetUsartMsgTimeoutCnt;

    public

     sloty: array [1.._SLOTS_CNT] of TSlot;
     ignoreKeepAliveLogging : boolean;

      constructor Create();
      destructor Destroy(); override;

      procedure Open(port:string);
      procedure Close();

      procedure EnumDevices(const Ports: TStringList);

      procedure SendLokoStolen(callByte:Byte; addrHi:Byte; addrLo:Byte); overload;
      procedure SendLokoStolen(callByte:Byte; addr:Word); overload;

      procedure SetStatus(new:TuLIStatus);

      function CalcParity(data:Byte):Byte;

      procedure RepaintSlots(form:TForm);
      procedure HardResetSlots();
      procedure ReleaseAllLoko();

      procedure ResetUsartCounters();

      property OnLog: TuLILogEvent read fOnLog write fOnLog;
      property logLevel: TuLILogLevel read fLogLevel write SetLogLevel;
      property busEnabled: boolean read GetBusActive write SetBusActive;
      property DCC : boolean read fDCC write SetDCC;
      property status : TuLIStatus read uLIStatus;
      property version : TuLIVersion read uLIVersion;
      property connected : boolean read GetConnected;
      property statusValid : boolean read uLIStatusValid;

      property usartMsgTotalCnt: Cardinal read ffusartMsgTotalCnt;
      property usartMsgTimeoutCnt: Cardinal read ffusartMsgTimeoutCnt;

      property OnUsartMsgCntChange: TNotifyEvent read fOnUsartMsgCntChange write fOnUsartMsgCntChange;

  end;

var
  uLI : TuLI;

implementation

uses client, tHnaciVozidlo, fMain, server, fSlots, fConnect;

////////////////////////////////////////////////////////////////////////////////

constructor TuLI.Create();
var i:Integer;
begin
 inherited;

 Self.uLIStatus := _DEF_ULI_STATUS;
 Self.uLIVersion := _DEF_ULI_VERSION;

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
 Self.ignoreKeepAliveLogging := true;

 Self.ffusartMsgTotalCnt := 0;
 Self.ffusartMsgTimeoutCnt := 0;

 for i := 1 to _SLOTS_CNT do Self.sloty[i] := TSlot.Create(i);
end;

destructor TuLI.Destroy();
var i:Integer;
begin
 Self.tKASendTimer.Free();
 Self.tKAReceiveTimer.Free();
 Self.ComPort.Free();

 for i := 1 to _SLOTS_CNT do FreeAndNil(Self.sloty[i]);

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

 Self.fusartMsgTotalCnt := 0;
 Self.fusartMsgTimeoutCnt := 0;

 F_Main.P_ULI.Color := clYellow;
 F_Main.P_ULI.Hint := 'Pøipojuji se k uLI-master...';
end;

procedure TuLI.ComAfterOpen(Sender:TObject);
begin
 Self.WriteLog(tllCommands, 'OPEN OK');
 F_Main.P_ULI.Color := clYellow;
 F_Main.P_ULI.Hint := 'Pøipojeno k uLI-master, èekám na stav...';

 // close if uLI does not respond in a few seconds
 Self.tKAReceiveTimer.Enabled := true;
 Self.KAreceiveTimeout := 0;

 // reset uLI status
 Self.uLIStatus := _DEF_ULI_STATUS;
 Self.SetStatus(Self.uLIStatus);

 // uLI version request
 Self.WriteLog(tllCommands, 'SEND: version request');
 Self.Send(CreateBuf(ShortString(#$A0+#$11+#$80)));
end;

procedure TuLI.ComBeforeClose(Sender:TObject);
var i:Integer;
begin
 Self.tKASendTimer.Enabled    := false;
 Self.tKAReceiveTimer.Enabled := false;
 Self.uLIStatusValid := false;
 Self.uLIStatus := _DEF_ULI_STATUS;
 Self.uLIVersion := _DEF_ULI_VERSION;

 for i := 1 to _SLOTS_CNT do
   if (Self.sloty[i].isLoko) then
     Self.sloty[i].ReleaseLoko();

 F_Main.P_ULI.Color := clYellow;
 F_Main.P_ULI.Hint := 'Odpojuji se od uLI-master...';
end;

procedure TuLI.ComAfterClose(Sender:TObject);
var i:Integer;
begin
 Self.WriteLog(tllCommands, 'CLOSE OK');
 Self.uLIStatusValid := false;

 for i := 1 to _SLOTS_CNT do Self.sloty[i].mausId := TSlot._MAUS_NULL;

 F_Main.P_ULI.Color := clRed;
 F_Main.P_ULI.Hint := 'Odpojeno od uLI-master';

 Self.RepaintSlots(F_Slots);
 TCPServer.BroadcastSlots();
 TCPServer.BroadcastAuth(true);

 if (F_Main.close_app) then
  begin
   if (TCPClient.status = client.TPanelConnectionStatus.closed) then F_Main.Close();
  end else begin
   F_Main.ShowChild(F_Connect);
   F_Connect.GB_Connect.Caption := ' Odpojeno od uLI-master ';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.Open(port:string);
begin
 if (Self.ComPort.Connected) then Exit();
 Self.ComPort.Port := port;

 F_Main.ClearMessage();
 Self.WriteLog(tllCommands, 'OPENING port='+port+' br='+BaudRateToStr(Self.ComPort.BaudRate)+' sb='+StopBitsToStr(Self.ComPort.StopBits)+' db='+DataBitsToStr(Self.ComPort.DataBits)+' fc='+FlowControlToStr(Self.ComPort.FlowControl.FlowControl));

 try
   Self.ComPort.Open();
 except
   on E : Exception do
    begin
     Self.ComPort.Close();
     Self.ComAfterClose(Self);
     raise;
    end;
 end;
end;

procedure TuLI.Close();
begin
 if (not Self.ComPort.Connected) then Exit();

 Self.WriteLog(tllCommands, 'CLOSING');

 if (Self.busEnabled) then Self.busEnabled := false; 

 try
   Self.ComPort.Close();
 except

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

  if (Self.logLevel >= tllDetail) then begin
    s := 'BUF: ';
    for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
    WriteLog(tllDetail, s);
   end;

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

        if (Self.logLevel >= tllDetail) then begin
          s := 'BUF: ';
          for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
          if (Fbuf_in.Count > 0) then WriteLog(tllDetail, s);
         end;

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
 if ((not Self.ignoreKeepAliveLogging) or (msg.Count <> 4) or (not CompareMem(@msg.data, @_KEEP_ALIVE, 4))) then
  begin
   s := '';
   for i := 0 to msg.Count-1 do s := s + IntToHex(msg.data[i], 2) + ' ';
   Self.WriteLog(tllData, 'GET: '+s);
  end;

 try
   target := (msg.data[0] shr 5) AND 3;
   if (target = 3) then
     Self.ParseDeviceMsg((msg.data[0] AND $1F), msg)
   else if (target = 1) then
     Self.ParseuLIMsg(msg)
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseDeviceMsg(deviceAddr:Byte; var msg: TBuffer);
var toSend: ShortString;
    tmp, tmp2, tmpSmer: Byte;
    addr: Word;
    i, addrOld: Integer;
    funkce: TFunkce;
    changed: boolean;
    maxsp, speed:Integer;
    emergencyStop:Boolean;
begin
 Self.fusartMsgTotalCnt := Self.fusartMsgTotalCnt + 1;

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
         Self.WriteLog(tllCommands, 'SEND: command station status');
         Self.Send(CreateBuf(ShortString(chr(msg.data[0])+#$62+#$22+(char(not Self.DCC)))));
       end;

       $81: begin
         Self.WriteLog(tllCommands, 'GET: resume operations request');

         Self.WriteLog(tllCommands, 'PUT: GO');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));

         Self.WriteLog(tllCommands, 'PUT: STOP');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
       end;

       $80: begin
         Self.WriteLog(tllCommands, 'GET: STOP operations request');

         // zastavit hnaci vozidlo
         i := Self.FindSlot(msg.data[0] AND $1F);
         if ((i > -1) and (Self.sloty[i].isLoko)) then
           Self.sloty[i].STOPloko();

         Self.WriteLog(tllCommands, 'PUT: STOP');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_OFF));
         Self.WriteLog(tllCommands, 'PUT: GO');
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
         Self.Send(CreateBuf(AnsiChar(msg.data[0]) + _CMD_DCC_ON));
       end;
     end;// case msg.data[2]
   end;//$21

   $42: begin
     // Accessory Decoder information request
     Self.WriteLog(tllCommands, 'GET: Accessory Decoder information request');
     Self.WriteLog(tllCommands, 'PUT: Default Accessory Decoder information');
     msg.data[3] := $20 + ((msg.data[3] and 1) shl 4);
     msg.Count := 4;
     Self.Send(msg);
   end;//$42

   $E3: begin
     case (msg.data[2]) of
       00: begin
         Self.WriteLog(tllCommands, 'GET: locomotive information request');

         toSend := AnsiChar(msg.data[0]) + #$E4;

         addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);

         changed := false;
         addrOld := Self.FindSlot(msg.data[0] AND $1F);
         if ((addrOld > -1) and (addrOld <> addr)) then
          begin
           // na mysi doslo ke zmene adresy z addrOld na addr
           // -> odstranit slot addrOld
           Self.sloty[addrOld].ReleaseLoko();
           Self.sloty[addrOld].mausId := TSlot._MAUS_NULL;
           changed := true;
          end;

         if (((addr >= 1) and (addr <= _SLOTS_CNT)) and (not Self.sloty[addr].isMaus)) then
          begin
           // obsazujeme slot adresou
           Self.sloty[addr].mausId := (msg.data[0] AND $1F);
           changed := true;
          end;

         if (changed) then
          begin
           Self.RepaintSlots(F_Slots);
           TCPServer.BroadcastSlots();
          end;

         if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko) or
             (Self.sloty[addr].ukradeno)) then
          begin
           // lokomotiva neni rizena ovladacem
           toSend := toSend + #$A + #$80 + #0 + #0;
          end else begin
           // lokomotiva je rizena ovladacem
           toSend := toSend + AnsiChar(2 + (Byte(Self.sloty[addr].mausId <> (msg.data[0] AND $1F)) shl 3));

           // rychlost + smer
           case (Self.sloty[addr].rychlost_stupne) of
              0: tmp2 := 0;
              1..28: tmp2 := Self.sloty[addr].rychlost_stupne+3;
              else tmp2 := 0;
           end;

           tmp := (((1-Self.sloty[addr].smer) AND $1) shl 7) +
                  ((tmp2 AND $1e) shr 1) +
                  ((tmp2 AND $01) shl 4);

           toSend := toSend + AnsiChar(tmp);

           // F0 - F4
           tmp := (byte(Self.sloty[addr].funkce[0]) shl 4);
           for i := 1 to 4 do tmp := tmp + (byte(Self.sloty[addr].funkce[i]) shl (i-1));
           toSend := toSend + AnsiChar(tmp);

           // F5 - F12
           tmp := 0;
           for i := 5 to 12 do tmp := tmp + (byte(Self.sloty[addr].funkce[i]) shl (i-5));
           toSend := toSend + AnsiChar(tmp);
          end;

         Self.WriteLog(tllCommands, 'PUT: locomotive information');
         Self.Send(CreateBuf(toSend));
       end;
     end;
   end;

   $E4: begin
     case (msg.data[2]) of
      $10..$13: begin
        Self.WriteLog(tllCommands, 'GET: locomotive set speed');

        addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);

        case (msg.data[2]) of
          $10: maxsp :=  14;
          $11: maxsp :=  27;
          $12: maxsp :=  28;
          $13: maxsp := 128;
        else
          maxsp := 128;
        end;

        emergencyStop := false;
        speed := 0;
        case (maxsp) of
         14: begin
              speed := (msg.data[5] AND $0F);
              if (speed = 1) then
               begin
                emergencyStop := true;
                speed := 0;
               end;
              if (speed > 0) then Dec(speed);              
              speed := (speed * 2);               // normovani rychlosti (28/14)=2
         end;

         27, 28: begin
              speed := ((msg.data[5] AND $0F) shl 1) OR ((msg.data[5] AND $10) shr 4);
              if (speed = 2) then emergencyStop := true;              
              if ((speed >= 1) and (speed <= 3)) then speed := 0;
              if (speed >= 4) then speed := speed-3;
         end;

         128:begin
              speed := (msg.data[5] AND $7F);
              if (speed = 1) then
               begin
                speed := 0;
                emergencyStop := true;
               end;
              speed := Round(speed * (28/128));   // normovani rychlosti
         end;
        end;

        if (((addr >= 1) and (addr <= _SLOTS_CNT)) and ((msg.data[0] AND $1F) <> Self.sloty[addr].mausId)) then
         begin
          if (Self.sloty[addr].isMaus) then Self.SendLokoStolen(CalcParity(Self.sloty[addr].mausId + $60), addr);
          Self.sloty[addr].mausId := (msg.data[0] AND $1F);
         end;

        if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
         begin
          // lokomotiva neni rizena ovladacem
          // -> odeslat "locomotive is being operated by another device"
          Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end else begin
          // lokomotiva je rizena ovladacem -> nastavit rychlost a smer

          tmpSmer := 1 - ((Byte(msg.data[5]) shr 7) and $1);
          if (emergencyStop) then
           begin
            // emergency stop -> uvolnit HV ze slotu
            Self.sloty[addr].ReleaseLoko();
           end else begin
            // normal stop
            if (Self.sloty[addr].total) then Self.sloty[addr].SetRychlostSmer(speed, tmpSmer);
           end;
          if (not Self.sloty[addr].total) then Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end;

      end;

      $20: begin
        Self.WriteLog(tllCommands, 'GET: set F0-F4');

        addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);
        if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
         begin
          // lokomotiva neni rizena ovladacem
          // -> odeslat "locomotive is being operated by another device"
          Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end else begin
          // lokomotiva je rizena ovladacem -> nastavit funkce
          funkce[0] := boolean((msg.data[5] shr 4) and $1);
          for i := 0 to 3 do funkce[i+1] := boolean((msg.data[5] shr i) and $1);
          Self.sloty[addr].SetFunctions(0, 4, funkce);
         end;
      end;

      $21: begin
        Self.WriteLog(tllCommands, 'GET: set F5-F8');

        addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);
        if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
         begin
          // lokomotiva neni rizena ovladacem
          // -> odeslat "locomotive is being operated by another device"
          Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end else begin
          // lokomotiva je rizena ovladacem -> nastavit funkce
          for i := 0 to 3 do funkce[i+5] := boolean((msg.data[5] shr i) and $1);
          Self.sloty[addr].SetFunctions(5, 8, funkce);
         end;
      end;

      $22: begin
        Self.WriteLog(tllCommands, 'GET: set F9-F12');

        addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);
        if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
         begin
          // lokomotiva neni rizena ovladacem
          // -> odeslat "locomotive is being operated by another device"
          Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end else begin
          // lokomotiva je rizena ovladacem -> nastavit funkce
          for i := 0 to 3 do funkce[i+9] := boolean((msg.data[5] shr i) and $1);
          Self.sloty[addr].SetFunctions(9, 12, funkce);
         end;
      end;

      $F3: begin
        Self.WriteLog(tllCommands, 'GET: set F13-F20');

        addr := Self.LokAddrDecode(msg.data[3], msg.data[4]);
        if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
         begin
          // lokomotiva neni rizena ovladacem
          // -> odeslat "locomotive is being operated by another device"
          Self.SendLokoStolen(Byte(msg.data[0]), Byte(msg.data[3]), Byte(msg.data[4]));
         end else begin
          // lokomotiva je rizena ovladacem -> nastavit funkce
          for i := 0 to 7 do funkce[i+13] := boolean((msg.data[5] shr i) and $1);
          Self.sloty[addr].SetFunctions(13, 20, funkce);
         end;
      end;

     end;// case msg.data[2]
   end;//$E4
 end;//case msg.data[1]
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseuLIMsg(var msg: TBuffer);
begin
 case (msg.data[1]) of
  $01: begin
    // informative messages
    case (msg.data[2]) of
     $01: begin
        Self.WriteLog(tllErrors, 'ERR: GET: USB incoming data timeout');
        F_Main.LogMessage('uLI-ERR: GET: USB incoming data timeout');
     end;
     $02: begin
        Self.WriteLog(tllErrors, 'ERR: GET: USART incoming data timeout');

        // report the error quite silently
        if ((F_Main.P_ULI.Color = clGreen) or (F_Main.P_ULI.Color = clLime)) then
          F_Main.P_ULI.Color := clTeal;

        Inc(Self.ffusartMsgTotalCnt); // will not cause event to fire (ff)
        Self.fusartMsgTimeoutCnt := Self.fusartMsgTimeoutCnt + 1; // will cause event to fire (f)
     end;
     $03: begin
        Self.WriteLog(tllErrors, 'ERR: GET: Unknown command');
        F_Main.LogMessage('uLI-ERR: GET: Unknown command');
     end;
     $04: Self.WriteLog(tllCommands, 'GET: OK');
     $05: begin
       Self.WriteLog(tllChanges, 'GET: keep-alive');
       Self.KAreceiveTimeout := 0;

       if ((F_Main.P_ULI.Color = clGreen) or (F_Main.P_ULI.Color = clTeal)) then F_Main.P_ULI.Color := clLime
       else if (F_Main.P_ULI.Color = clLime) then F_Main.P_ULI.Color := clGreen;
     end;
     $06: begin
        Self.WriteLog(tllErrors, 'ERR: GET: USB>USART buffer overflow');
        F_Main.LogMessage('uLI-ERR: GET: USB>USART buffer overflow');
     end;
     $07: begin
        Self.WriteLog(tllErrors, 'ERR: GET: USB XOR error');
        F_Main.LogMessage('uLI-ERR: GET: USB XOR error');
     end;
     $08: begin
        Self.WriteLog(tllErrors, 'ERR: GET: USB parity error');
        F_Main.LogMessage('uLI-ERR: GET: USB parity error');
     end;
     $09: begin
        Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power source turned off');
        F_Main.LogMessage('uLI-ERR: GET: XpressNET power source turned off');
     end;
     $0A: begin
        Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power transistor closed');
        F_Main.LogMessage('uLI-ERR: GET: XpressNET power transistor closed');
     end;
    end;
  end;

  $11: begin
    // uLI-master status response
    Self.WriteLog(tllCommands, 'GET: master status');
    Self.ParseuLIStatus(msg);

    F_Main.P_ULI.Color := clGreen;
    F_Main.P_ULI.Hint := 'Pøipojeno k uLI-master, stav vyèten';
  end;

  $13: begin
    if (msg.data[2] = $80) then
     begin
      Self.uLIVersion.hw := IntToStr((msg.data[3] shr 4) AND $F) + '.' + IntToStr(msg.data[3] AND $F);
      Self.uLIVersion.sw := IntToStr((msg.data[4] shr 4) AND $F) + '.' + IntToStr(msg.data[4] AND $F);
      Self.WriteLog(tllCommands, 'GET: uLI version hw:'+Self.uLIVersion.hw+', sw:'+Self.uLIVersion.sw);
     end;

  end;//case msg.data[1]
 end;//case
end;//procedure

procedure TuLI.ParseuLIStatus(var msg:TBuffer);
var new:TuLIStatus;
    blackout, turnon:boolean;
    i:Integer;
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

 blackout := ((Self.status.sense) and (not new.sense));
 turnon   := ((not Self.status.sense) and (new.sense) and (not new.transistor));

 if ((not Self.status.sense) and (new.sense)) then
    F_Main.ClearMessage();

 if (not Self.uLIStatusValid) then
  begin
   // these variables must be changed before BroadcastAuth calling
   Self.uLIStatus      := new;
   Self.uLIStatusValid := true;
   TCPServer.BroadcastAuth(true);
  end else begin
   Self.uLIStatus      := new;
   Self.uLIStatusValid := true;
  end;

 if (blackout) then
  begin
   // vypadek napajeni multiMaus
   Self.ReleaseAllLoko();
   for i := 1 to _SLOTS_CNT do Self.sloty[i].mausId := TSlot._MAUS_NULL;
   Self.RepaintSlots(F_Slots);
   TCPServer.BroadcastSlots();
   F_Main.LogMessage('Výpadek napájení uLI-master!');
  end else if (turnon) then
   begin
    // obnoveni napajeni sbernice -> zapnout ovladace
    try
      if (TCPClient.authorised) then Self.busEnabled := true;
    except

    end;
   end;
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
  if ((not Self.ignoreKeepAliveLogging) or (data.Count <> 4) or (not CompareMem(@data.data, @_KEEP_ALIVE, 4))) then
   begin
    log := '';
    for i := 0 to data.Count-1 do log := log + IntToHex(data.data[i],2) + ' ';
    Self.WriteLog(tllData, 'PUT: '+log);
   end;

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
     F_Main.LogMessage('uLI-master PUT ERR : '+E.Message);
     Self.WriteLog(tllErrors, 'PUT ERR: com object error : '+E.Message);
     if (Self.ComPort.Connected) then Self.ComPort.Close();
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
   Self.KAreceiveTimeout := 0;
   Self.WriteLog(tllErrors, 'uLI neodpovìdìlo na keep-alive!');
   F_Main.LogMessage('uLI neodpovìdìlo na keep-alive!');
   Self.Close();
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
    i:Integer;
begin
 if ((new) and (not Self.uLIStatusValid)) then
  begin
   Self.SendStatusRequest();
   raise EuLIStatusInvalid.Create('Program nemá validní data o stavu uLI!');
  end;

 if ((new) and (not self.uLIStatus.sense)) then
   raise EPowerTurnedOff.Create('Zaøízení není napájeno!');

 if (not new) then
  begin
   for i := 1 to _SLOTS_CNT do Self.sloty[i].mausId := TSlot._MAUS_NULL;
   Self.RepaintSlots(F_Slots);
  end;

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
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_ON));
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_ON));
    end else begin
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_OFF));
     Self.Send(CreateBuf(_BROADCAST_HEADER + _CMD_DCC_OFF));
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

////////////////////////////////////////////////////////////////////////////////

function TuLI.LokAddrDecode(ah, al: byte): Integer;
begin
  Result := al or ((ah AND $3F) shl 8);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLokoStolen(callByte:Byte; addrHi:Byte; addrLo:Byte);
begin
 Self.WriteLog(tllCommands, 'PUT: locomotive is being operated by another device');
 Self.Send(CreateBuf(AnsiChar(callByte) + #$E3 + #$40 + AnsiChar(addrHi) + AnsiChar(addrLo)));
end;

procedure TuLI.SendLokoStolen(callByte:Byte; addr:Word);
var encoded:Word;
begin
 encoded := Self.LokAddrEncode(addr);
 Self.SendLokoStolen(callByte, (encoded shr 8) and $FF, encoded AND $FF);
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.FindSlot(mausId:Byte):Integer;
var i:Integer;
begin
 for i := 1 to _SLOTS_CNT do
   if (Self.sloty[i].mausId = mausId) then
     Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.CalcParity(data:Byte):Byte;
var i:Integer;
    parity:boolean;
    tmp:Byte;
begin
 tmp := data;
 parity := false;
 for i := 0 to 7 do
  begin
   if ((tmp AND $1) > 0) then parity := not parity;
   tmp := tmp shr 1;
  end;
 if (parity) then
   Result := data + $80
 else
   Result := data;
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.GetConnected():boolean;
begin
 Result := Self.ComPort.Connected;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.RepaintSlots(form:TForm);
var i, j, cnt:Integer;
begin
 cnt := 0;
 for i := 1 to _SLOTS_CNT do
   if (Self.sloty[i].isMaus) then
     Inc(cnt);

 j := 0;
 for i := 1 to _SLOTS_CNT do
  begin
   if ((Self.sloty[i].isMaus) and (Self.busEnabled) and (TCPClient.authorised)) then
    begin
     Self.sloty[i].Show(form, j, cnt);
     Inc(j);
    end else
     Self.sloty[i].HideGUI();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.HardResetSlots();
var i:Integer;
begin
 for i := 1 to _SLOTS_CNT do
   Self.sloty[i].HardResetSlot();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ReleaseAllLoko();
var i:Integer;
begin
 for i := 1 to _SLOTS_CNT do
   Self.sloty[i].ReleaseLoko();
end;

////////////////////////////////////////////////////////////////////////////////

function TuLI.GetBusActive():boolean;
begin
 Result := ((uLIStatusValid) and (uLIStatus.transistor) and (uLIStatus.sense));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetUsartMsgTotalCnt(new:Cardinal);
begin
 if (Self.ffusartMsgTotalCnt <> new) then begin
   Self.ffusartMsgTotalCnt := new;
   if (Assigned(Self.fOnUsartMsgCntChange)) then Self.fOnUsartMsgCntChange(Self);
 end else begin
   Self.ffusartMsgTotalCnt := new;
 end;
end;

procedure TuLI.SetUsartMsgTimeoutCnt(new:Cardinal);
begin
 if (Self.ffusartMsgTimeoutCnt <> new) then begin
   Self.ffusartMsgTimeoutCnt := new;
   if (Assigned(Self.fOnUsartMsgCntChange)) then Self.fOnUsartMsgCntChange(Self);
 end else begin
   Self.ffusartMsgTimeoutCnt := new;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.ResetUsartCounters();
begin
 Self.ffusartMsgTotalCnt := 0; // will not cause an event to fire
 Self.fusartMsgTimeoutCnt := 0; // will cause an event to fire
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  uLI := TuLI.Create();
finalization
  FreeAndNil(uLI);

end.
