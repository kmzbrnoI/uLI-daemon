unit tUltimateLI;

{
  Trida TuLI je rozhranim k uLI.
}

interface

uses SysUtils, CPort, Forms;

type
  TTrkLogLevel = (tllNo = 0, tllErrors = 1, tllCommands = 2, tllData = 3, tllChanges = 4, tllDetail = 5);

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

  TuLI = class
    private const
      _CPORT_BAUDRATE    = br19200;
      _CPORT_STOPBITS    = sbOneStopBit;
      _CPORT_DATABITS    = dbEight;
      _CPORT_FLOWCONTROL = fcNone;

      _BUF_IN_TIMEOUT_MS = 300;                                                 // timeout vstupniho bufferu v ms (po uplynuti timeoutu dojde k vymazani bufferu) - DULEZITY SAMOOPRAVNY MECHANISMUS!
                                                                                // pro spravnou funkcnost musi byt < _TIMEOUT_MSEC

    private
     ComPort:TComPort;

     Fbuf_in: TBuffer;
     Fbuf_in_timeout:TDateTime;

     uLIStatusValid: boolean;
     uLIStatus: TuLIStatus;

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

      procedure WriteLog(lvl:TTrkLogLevel; msg:string);

      procedure Send(data:RawByteString);

    public

      constructor Create();
      destructor Destroy(); override;

      procedure Open(port:string);
      procedure Close();

  end;

var
  uLI : TuLI;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TuLI.Create();
begin
 inherited;

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

 Self.uLIStatusValid := false;
end;

destructor TuLI.Destroy();
begin
 Self.ComPort.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.WriteLog(lvl:TTrkLogLevel; msg:string);
begin
 // TODO
end;

////////////////////////////////////////////////////////////////////////////////
// COM port events:

procedure TuLI.OnComError(Sender: TObject; Errors: TComErrors);
begin

end;

procedure TuLI.OnComException(Sender: TObject;
   TComException: TComExceptions; ComportMessage: string; WinError: Int64;
   WinMessage: string);
begin

end;

procedure TuLI.ComBeforeOpen(Sender:TObject);
begin
 Self.uLIStatusValid := false;
end;

procedure TuLI.ComAfterOpen(Sender:TObject);
begin

end;

procedure TuLI.ComBeforeClose(Sender:TObject);
begin

end;

procedure TuLI.ComAfterClose(Sender:TObject);
begin
 Self.uLIStatusValid := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TuLI.Open(port:string);
begin
 if (Self.ComPort.Connected) then Exit();
 Self.ComPort.Port := port;

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
begin
 case (msg.data[1]) of
   $21: begin
     case (msg.data[2]) of
       $21: begin
         // command station software-version request
       end;//$21

       $24: begin
         // command station status request
       end;
     end;// case msg.data[2]
   end;//$21
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
     $05: Self.WriteLog(tllCommands, 'GET: keep-alive');
     $06: Self.WriteLog(tllErrors, 'ERR: GET: USB>USART buffer overflow');
     $07: Self.WriteLog(tllErrors, 'ERR: GET: USB XOR error');
     $08: Self.WriteLog(tllErrors, 'ERR: GET: USB parity error');
     $09: Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power source turned off');
     $0A: Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power transistor closed');
    end;
  end;

  $11: begin
    // uLI-master status response

  end;
 end;//case msg.data[1]
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// Tato funkce funguje jako blokujici.
// Z funkce je vyskoceno ven az po odeslani dat (nebo vyjimce).
// Tato funckce ocekava vstupni data bez XORu na konci (prida ho sama).
procedure TuLI.Send(data:RawByteString);
var
  x: byte;
  i: integer;
  log:string;
  asp: PAsync;
begin
  if (not Self.ComPort.Connected) then
   begin
    Self.WriteLog(tllErrors, 'PUT ERR: XpressNet not connected');
    Exit;
   end;
  if (Length(data) > 18) then
   begin
    Self.WriteLog(tllErrors, 'PUT ERR: Message too long');
    Exit;
   end;

  //xor
  x := 0;
  for i := 1 to Length(data)-1 do x := x xor ord(data[i]);
  data := data + RawByteString(chr(x));

  //get string for log
  log := '';
  for i := 0 to Length(data)-1 do log := log + IntToHex(ord(data[i]),2) + ' ';
  Self.WriteLog(tllData, 'PUT: '+log);

  try
    InitAsync(asp);
    Self.ComPort.WriteAsync(data, Length(data), asp);
    while (not Self.ComPort.IsAsyncCompleted(asp)) do
     begin
      Application.ProcessMessages();
      Sleep(1);
     end;
  except
   on E : Exception do
    begin
     Self.WriteLog(tllErrors, 'PUT ERR: com object error : '+E.Message);
    end;
  end;

 DoneAsync(asp);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
  uLI := TuLI.Create();
finalization
  FreeAndNil(uLI);

end.
