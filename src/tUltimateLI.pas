unit tUltimateLI;

{
  Trida TuLI je rozhranim k uLI.
}

interface

uses SysUtils, CPort, Forms, tUltimateLIConst, Classes, Registry, Windows,
  ExtCtrls, mausSlot, Graphics;

type
  TBuffer = record
    data: array [0 .. 255] of Byte;
    Count: Integer;
  end;

  TuLIStatus = record
    sense: boolean;
    transistor: boolean;
    aliveReceiving: boolean;
    aliveSending: boolean;
  end;

  TuLIVersion = record
    sw: string;
    hw: string;
  end;

  EuLIStatusInvalid = class(Exception);
  EPowerTurnedOff = class(Exception);

  TuLI = class
  private const
    _DEF_ULI_STATUS: TuLIStatus = (sense: false; transistor: false;
      aliveReceiving: true; aliveSending: true;);

    _DEF_ULI_VERSION: TuLIVersion = (sw: ''; hw: '';);

    _CPORT_BAUDRATE = br19200;
    _CPORT_STOPBITS = sbOneStopBit;
    _CPORT_DATABITS = dbEight;
    _CPORT_FLOWCONTROL = fcNone;

    _BUF_IN_TIMEOUT_MS = 300;
    // timeout vstupniho bufferu v ms (po uplynuti timeoutu dojde k vymazani bufferu) - DULEZITY SAMOOPRAVNY MECHANISMUS!
    // pro spravnou funkcnost musi byt < _TIMEOUT_MSEC

    _DEVICE_DESCRIPTION = 'uLI - master';

    _KA_SEND_PERIOD_MS = 1000;
    _KA_RECEIVE_TIMEOUT_TICKS = 6;
    _KA_RECEIVE_PERIOD_MS = 500;

    _DEFAULT_DCC = true;

    _CMD_DCC_ON: ShortString = #$61#$01;
    _CMD_DCC_OFF: ShortString = #$61#$00;
    _CMD_DCC_STOP: ShortString = #$81#$00;

    _KEEP_ALIVE: Byte = $05;

  public const
    _SLOTS_CNT = 6;
    _BROADCAST_HEADER: ShortString = #$60;
    _BROADCAST_CODE: Byte = $60;

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
    fDCC: boolean;

    ffusartMsgTotalCnt: Cardinal;
    ffusartMsgTimeoutCnt: Cardinal;

    // events
    fOnLog: TuLILogEvent;
    fOnUsartMsgCntChange: TNotifyEvent;

    procedure OntKASendTimer(Sender: TObject);
    procedure OntKAReceiveTimer(Sender: TObject);

    procedure OnComError(Sender: TObject; Errors: TComErrors);
    procedure OnComException(Sender: TObject; TComException: TComExceptions;
      ComportMessage: string; WinError: Int64; WinMessage: string);
    procedure ComBeforeOpen(Sender: TObject);
    procedure ComAfterOpen(Sender: TObject);
    procedure ComBeforeClose(Sender: TObject);
    procedure ComAfterClose(Sender: TObject);
    procedure ComRxChar(Sender: TObject; Count: Integer);

    procedure CheckFbufInTimeout();
    procedure ParseComMsg(callByte: Byte; headerByte: Byte; msg: PByte; msgLen: Cardinal);
    procedure ParseDeviceMsg(deviceAddr: Byte; headerByte: Byte; msg: PByte; msgLen: Cardinal);
    procedure ParseuLIMsg(headerByte: Byte; msg: PByte; msgLen: Cardinal);
    procedure ParseuLIStatus(msg: PByte; msgLen: Cardinal);

    procedure WriteLog(lvl: TuLILogLevel; msg: string);

    procedure Send(callByte: Byte; data: ShortString);
    procedure SendXN(device: Byte; data: ShortString);
    procedure SenduLI(data: ShortString);
    procedure SendKeepAlive();
    procedure SendStatusRequest();
    function  CheckAddrChangeOK(deviceAddr: Byte; addr: Integer): Boolean;
    procedure SendLocoData(deviceAddr: Byte; addr: Integer);
    procedure SendLocoFunc13(deviceAddr: Byte; addr: Integer);
    procedure SendLocoFuncType(deviceAddr: Byte; addr: Integer);
    procedure SendLocoFunc13Type(deviceAddr: Byte; addr: Integer);
    procedure SendNotSupported(deviceAddr: Byte);

    procedure SetLogLevel(new: TuLILogLevel);

    function CreateBuf(str: ShortString): TBuffer;

    procedure SetBusActive(new: boolean);
    procedure SetDCC(new: boolean);

    function LokAddrEncode(addr: Integer): Word; inline;
    // ctyrmistna adresa lokomotivy do dvou bytu
    function LokAddrDecode(ah, al: Byte): Integer; inline;
    // ctyrmistna adresa lokomotivy ze dvou bajtu do klasickeho cisla

    function FindSlot(mausId: Byte): Integer;
    function GetConnected(): boolean;
    function GetBusActive(): boolean;

    procedure SetUsartMsgTotalCnt(new: Cardinal);
    procedure SetUsartMsgTimeoutCnt(new: Cardinal);

    function Parity(b: Byte): Boolean;
    function Xorxor(data: array of Byte; from: Cardinal; len: Cardinal): Byte; overload;
    function Xorxor(data: ShortString): Byte; overload;
    function BufToStr(data: ShortString): string; overload;
    function BufToStr(data: PByte; len: Cardinal): string; overload;
    function BufToStr(data: PByte; from: Cardinal; len: Cardinal): string; overload;

    property fusartMsgTotalCnt: Cardinal read ffusartMsgTotalCnt
      write SetUsartMsgTotalCnt;
    property fusartMsgTimeoutCnt: Cardinal read ffusartMsgTimeoutCnt
      write SetUsartMsgTimeoutCnt;

  public

    sloty: array [1 .. _SLOTS_CNT] of TSlot;
    ignoreKeepAliveLogging: boolean;

    constructor Create();
    destructor Destroy(); override;

    procedure Open(port: string);
    procedure Close();

    procedure EnumDevices(const Ports: TStringList);

    procedure SendLokoStolen(deviceAddr: Byte; addrHi: Byte;
      addrLo: Byte); overload;
    procedure SendLokoStolen(deviceAddr: Byte; addr: Word); overload;

    procedure SetStatus(new: TuLIStatus);

    function CalcParity(data: Byte): Byte;

    procedure RepaintSlots(form: TForm);
    procedure HardResetSlots();
    procedure ReleaseAllLoko();

    procedure ResetUsartCounters();

    property OnLog: TuLILogEvent read fOnLog write fOnLog;
    property logLevel: TuLILogLevel read fLogLevel write SetLogLevel;
    property busEnabled: boolean read GetBusActive write SetBusActive;
    property DCC: boolean read fDCC write SetDCC;
    property status: TuLIStatus read uLIStatus;
    property version: TuLIVersion read uLIVersion;
    property connected: boolean read GetConnected;
    property statusValid: boolean read uLIStatusValid;

    property usartMsgTotalCnt: Cardinal read ffusartMsgTotalCnt;
    property usartMsgTimeoutCnt: Cardinal read ffusartMsgTimeoutCnt;

    property OnUsartMsgCntChange: TNotifyEvent read fOnUsartMsgCntChange
      write fOnUsartMsgCntChange;

  end;

var
  uLI: TuLI;

implementation

uses client, tHnaciVozidlo, fMain, server, fSlots, fConnect, System.Math;

/// /////////////////////////////////////////////////////////////////////////////

constructor TuLI.Create();
begin
  inherited;

  Self.uLIStatus := _DEF_ULI_STATUS;
  Self.uLIVersion := _DEF_ULI_VERSION;

  Self.ComPort := TComPort.Create(nil);
  Self.ComPort.BaudRate := _CPORT_BAUDRATE;
  Self.ComPort.StopBits := _CPORT_STOPBITS;
  Self.ComPort.DataBits := _CPORT_DATABITS;
  Self.ComPort.FlowControl.FlowControl := _CPORT_FLOWCONTROL;

  Self.ComPort.OnError := Self.OnComError;
  Self.ComPort.OnException := Self.OnComException;
  Self.ComPort.OnBeforeOpen := Self.ComBeforeOpen;
  Self.ComPort.OnAfterOpen := Self.ComAfterOpen;
  Self.ComPort.OnBeforeClose := Self.ComBeforeClose;
  Self.ComPort.OnAfterClose := Self.ComAfterClose;
  Self.ComPort.OnRxChar := Self.ComRxChar;

  Self.tKASendTimer := TTimer.Create(nil);
  Self.tKASendTimer.Interval := _KA_SEND_PERIOD_MS;
  Self.tKASendTimer.Enabled := false;
  Self.tKASendTimer.OnTimer := Self.OntKASendTimer;

  Self.tKAReceiveTimer := TTimer.Create(nil);
  Self.tKAReceiveTimer.Interval := _KA_RECEIVE_PERIOD_MS;
  Self.tKAReceiveTimer.Enabled := false;
  Self.tKAReceiveTimer.OnTimer := Self.OntKAReceiveTimer;

  Self.uLIStatusValid := false;
  Self.fDCC := _DEFAULT_DCC;
  Self.ignoreKeepAliveLogging := true;

  Self.ffusartMsgTotalCnt := 0;
  Self.ffusartMsgTimeoutCnt := 0;

  for var i := 1 to _SLOTS_CNT do
    Self.sloty[i] := TSlot.Create(i);
end;

destructor TuLI.Destroy();
begin
  Self.tKASendTimer.Free();
  Self.tKAReceiveTimer.Free();
  Self.ComPort.Free();

  for var i := 1 to _SLOTS_CNT do
    FreeAndNil(Self.sloty[i]);

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.WriteLog(lvl: TuLILogLevel; msg: string);
begin
  if ((lvl <= Self.logLevel) and (Assigned(Self.OnLog))) then
    Self.OnLog(Self, lvl, msg);
end;

/// /////////////////////////////////////////////////////////////////////////////
// COM port events:

procedure TuLI.OnComError(Sender: TObject; Errors: TComErrors);
begin
  Self.WriteLog(tllErrors, 'ERR: COM PORT ERROR');
end;

procedure TuLI.OnComException(Sender: TObject; TComException: TComExceptions;
  ComportMessage: string; WinError: Int64; WinMessage: string);
begin
  Self.WriteLog(tllErrors, 'ERR: COM PORT EXCEPTION: ' + ComportMessage + '; ' +
    WinMessage);
  raise Exception.Create(ComportMessage);
end;

procedure TuLI.ComBeforeOpen(Sender: TObject);
begin
  Self.uLIStatusValid := false;

  Self.fusartMsgTotalCnt := 0;
  Self.fusartMsgTimeoutCnt := 0;

  F_Main.P_ULI.Color := clYellow;
  F_Main.P_ULI.Hint := 'Připojuji se k uLI-master...';
end;

procedure TuLI.ComAfterOpen(Sender: TObject);
begin
  Self.WriteLog(tllCommands, 'OPEN OK');
  F_Main.P_ULI.Color := clYellow;
  F_Main.P_ULI.Hint := 'Připojeno k uLI-master, čekám na stav...';

  // close if uLI does not respond in a few seconds
  Self.tKAReceiveTimer.Enabled := true;
  Self.KAreceiveTimeout := 0;

  // reset uLI status
  Self.uLIStatus := _DEF_ULI_STATUS;
  Self.SetStatus(Self.uLIStatus);

  // uLI version request
  Self.WriteLog(tllCommands, 'SEND: version request');
  Self.SenduLI(#$11 + #$80);
end;

procedure TuLI.ComBeforeClose(Sender: TObject);
begin
  Self.tKASendTimer.Enabled := false;
  Self.tKAReceiveTimer.Enabled := false;
  Self.uLIStatusValid := false;
  Self.uLIStatus := _DEF_ULI_STATUS;
  Self.uLIVersion := _DEF_ULI_VERSION;

  for var i := 1 to _SLOTS_CNT do
    if (Self.sloty[i].isLoko) then
      Self.sloty[i].ReleaseLoko();

  F_Main.P_ULI.Color := clYellow;
  F_Main.P_ULI.Hint := 'Odpojuji se od uLI-master...';
end;

procedure TuLI.ComAfterClose(Sender: TObject);
begin
  Self.WriteLog(tllCommands, 'CLOSE OK');
  Self.uLIStatusValid := false;

  for var i := 1 to _SLOTS_CNT do
    Self.sloty[i].mausId := TSlot._MAUS_NULL;

  F_Main.P_ULI.Color := clRed;
  F_Main.P_ULI.Hint := 'Odpojeno od uLI-master';

  Self.RepaintSlots(F_Slots);
  TCPServer.BroadcastSlots();
  TCPServer.BroadcastAuth(true);

  if (F_Main.close_app) then
  begin
    if (TCPClient.status = client.TPanelConnectionStatus.closed) then
      F_Main.Close();
  end
  else
  begin
    F_Main.ShowChild(F_Connect);
    F_Connect.GB_Connect.Caption := ' Odpojeno od uLI-master ';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.Open(port: string);
begin
  if (Self.ComPort.connected) then
    Exit();
  Self.ComPort.port := port;

  F_Main.ClearMessage();
  Self.WriteLog(tllCommands, 'OPENING port=' + port + ' br=' +
    BaudRateToStr(Self.ComPort.BaudRate) + ' sb=' +
    StopBitsToStr(Self.ComPort.StopBits) + ' db=' +
    DataBitsToStr(Self.ComPort.DataBits) + ' fc=' +
    FlowControlToStr(Self.ComPort.FlowControl.FlowControl));

  try
    Self.ComPort.Open();
  except
    on E: Exception do
    begin
      Self.ComPort.Close();
      Self.ComAfterClose(Self);
      raise;
    end;
  end;
end;

procedure TuLI.Close();
begin
  if (not Self.ComPort.connected) then
    Exit();

  Self.WriteLog(tllCommands, 'CLOSING');

  if (Self.busEnabled) then
    Self.busEnabled := false;

  try
    Self.ComPort.Close();
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.CheckFbufInTimeout();
begin
  if ((Self.Fbuf_in_timeout < Now) and (Self.Fbuf_in.Count > 0)) then
  begin
    WriteLog(tllErrors, 'INPUT BUFFER TIMEOUT, removing buffer');
    Self.Fbuf_in.Count := 0;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ComRxChar(Sender: TObject; Count: Integer);
begin
  // check timeout
  Self.CheckFbufInTimeout();

  const freeSpace: Integer = Length(Fbuf_in.data)-Fbuf_in.Count;
  var readBytes: Integer := Self.ComPort.Read(Fbuf_in.data[Fbuf_in.Count], Min(Count, freeSpace));

  Fbuf_in.Count := Fbuf_in.Count + readBytes;
  Fbuf_in_timeout := Now + EncodeTime(0, 0, _BUF_IN_TIMEOUT_MS div 1000, _BUF_IN_TIMEOUT_MS mod 1000);

  if (Self.logLevel >= tllDetail) then
    WriteLog(tllDetail, 'BUF: '+Self.BufToStr(@Self.Fbuf_in.data, 0, Self.Fbuf_in.Count));

  var msgStartI: Integer := 0;

  while (msgStartI < Self.Fbuf_in.Count) do
  begin
    const msgAvailableBytes: Integer = Self.Fbuf_in.Count-msgStartI;

    if (Self.Fbuf_in.data[msgStartI] <> $51) then
    begin
      msgStartI := msgStartI + 1;
      continue;
    end;

    if (msgAvailableBytes < 2) then
      break; // wait for next data

    if (Self.Fbuf_in.data[msgStartI+1] <> $15) then
    begin
      msgStartI := msgStartI + 1;
      continue;
    end;

    if (msgAvailableBytes < 3) then
      break; // wait for next data

    // check parity of Call byte
    const callByte: Byte = Self.Fbuf_in.data[msgStartI+2];
    if (Self.Parity(callByte)) then
    begin
      msgStartI := msgStartI + 1;
      WriteLog(tllErrors, 'GET: PARITY ERROR');
      continue;
    end;

    if (msgAvailableBytes < 4) then
      break; // wait for next data

    const headerByte: Byte = Self.Fbuf_in.data[msgStartI+3];
    const msgLen = (headerByte AND $0F) + 5;

    if (msgAvailableBytes < msgLen) then
      break; // wait for next data

    // check xor of whole message
    var rxor: Byte := Self.Xorxor(Self.Fbuf_in.data, msgStartI+3, msgLen-3);
    if (rxor <> 0) then
    begin
      WriteLog(tllErrors, 'GET: XOR ERROR: ' + Self.BufToStr(@Self.Fbuf_in.data, msgStartI, msgLen));
      msgStartI := msgStartI + msgLen; // ignore whole message
      break;
    end;

    // message ok -> parse
    Self.ParseComMsg(callByte, headerByte, @Self.Fbuf_in.data[msgStartI+4], msgLen-5);

    msgStartI := msgStartI + msgLen;
  end; // while

  // remove processed data from Fbuf_in
  if (msgStartI > 0) then
  begin
    for var i := 0 to Fbuf_in.Count - msgStartI - 1 do
      Fbuf_in.data[i] := Fbuf_in.data[i + msgStartI];
    Fbuf_in.Count := Fbuf_in.Count - msgStartI;
  end;

  if ((Self.logLevel >= tllDetail) and (Fbuf_in.Count > 0)) then
    WriteLog(tllDetail, 'BUF: '+Self.BufToStr(@Self.Fbuf_in.data, 0, Self.Fbuf_in.Count));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseComMsg(callByte: Byte; headerByte: Byte; msg: PByte; msgLen: Cardinal);
begin
  if ((not Self.ignoreKeepAliveLogging) or (msgLen <> 1) or (callByte <> $A0) or (msg[0] <> _KEEP_ALIVE)) then
    Self.WriteLog(tllData, 'GET: 51 15 ' + IntToHex(ord(callByte), 2) + ' ' + IntToHex(ord(headerByte), 2) + ' ' + Self.BufToStr(msg, msgLen));

  try
    var target := (callByte shr 5) AND 3;
    if (target = 3) then
      Self.ParseDeviceMsg((callByte AND $1F), headerByte, msg, msgLen)
    else if (target = 1) then
      Self.ParseuLIMsg(headerByte, msg, msgLen)
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseDeviceMsg(deviceAddr: Byte; headerByte: Byte; msg: PByte; msgLen: Cardinal);
begin
  Self.fusartMsgTotalCnt := Self.fusartMsgTotalCnt + 1;

  case (headerByte) of
    $21:
      begin
        case (msg[0]) of
          $21:
            begin
              Self.WriteLog(tllCommands,
                'GET: command station software version request');
              Self.WriteLog(tllCommands,
                'SEND: command station software version');
              Self.SendXN(deviceAddr, #$63 + #$21 + #$36 + #$00);
            end;

          $24:
            begin
              Self.WriteLog(tllCommands, 'GET: command station status request');
              Self.WriteLog(tllCommands, 'SEND: command station status');
              Self.SendXN(deviceAddr, #$62 + #$22 + (char(not Self.DCC)));
            end;

          $81:
            begin
              Self.WriteLog(tllCommands, 'GET: resume operations request');

              Self.WriteLog(tllCommands, 'PUT: GO');
              Self.SendXN(deviceAddr, _CMD_DCC_ON);
              Self.SendXN(deviceAddr, _CMD_DCC_ON);

              Self.WriteLog(tllCommands, 'PUT: STOP');
              Self.SendXN(deviceAddr, _CMD_DCC_OFF);
              Self.SendXN(deviceAddr, _CMD_DCC_OFF);
            end;

          $80:
            begin
              Self.WriteLog(tllCommands, 'GET: STOP operations request');

              // zastavit hnaci vozidlo
              var i := Self.FindSlot(deviceAddr);
              if ((i > -1) and (Self.sloty[i].isLoko)) then
                Self.sloty[i].STOPloko();

              Self.WriteLog(tllCommands, 'PUT: STOP');
              Self.SendXN(deviceAddr, _CMD_DCC_OFF);
              Self.SendXN(deviceAddr, _CMD_DCC_OFF);
              Self.WriteLog(tllCommands, 'PUT: GO');
              Self.SendXN(deviceAddr, _CMD_DCC_ON);
              Self.SendXN(deviceAddr, _CMD_DCC_ON);
            end;
        else
          Self.SendNotSupported(deviceAddr);
        end; // case msg.data[2]
      end; // $21

    $42:
      begin
        // Accessory Decoder information request
        Self.WriteLog(tllCommands,
          'GET: Accessory Decoder information request');
        Self.WriteLog(tllCommands,
          'PUT: Default Accessory Decoder information');
        var data2: Byte := $20 + ((msg[1] and 1) shl 4);
        Self.SendXN(deviceAddr, #$42 + AnsiChar(msg[0]) + AnsiChar(data2));
      end; // $42

    $80:
      begin
        // stop all (power on)
        Self.WriteLog(tllCommands, 'GET: STOP ALL LOKS');
        var slot := Self.FindSlot(deviceAddr);
        if (slot > 0) then
          Self.sloty[slot].ReleaseLoko();

        Self.WriteLog(tllCommands, 'PUT: 3x STOP');
        Self.SendXN(0, _CMD_DCC_STOP);
        Self.SendXN(0, _CMD_DCC_STOP);
        Self.SendXN(0, _CMD_DCC_STOP);

        Self.WriteLog(tllCommands, 'PUT: 3x GO');
        Self.SendXN(0, _CMD_DCC_ON);
        Self.SendXN(0, _CMD_DCC_ON);
        Self.SendXN(0, _CMD_DCC_ON);
      end;
    $92:
      begin
        // e-stop one loco
        var addr := Self.LokAddrDecode(msg[0], msg[1]);
        if (((addr >= 1) and (addr <= _SLOTS_CNT)) and
          (deviceAddr <> Self.sloty[addr].mausId)) then
        begin
          if (Self.sloty[addr].isMaus) then
            Self.SendLokoStolen(deviceAddr, addr);
          Self.sloty[addr].mausId := deviceAddr;
        end;
        if ((addr > 0) or (addr <= _SLOTS_CNT) or (Self.sloty[addr].isLoko)) then
          if (Self.sloty[addr].total) then
            Self.sloty[addr].STOPloko();
      end;

    $E3:
      begin
        case (msg[0]) of
          00:
            begin
              Self.WriteLog(tllCommands, 'GET: locomotive information request');
              Self.SendLocoData(deviceAddr, Self.LokAddrDecode(msg[1], msg[2]));
            end;
          07:
            begin
              Self.WriteLog(tllCommands, 'GET: function status F0-F12 request (>=3.0)');
              Self.SendLocoFuncType(deviceAddr, Self.LokAddrDecode(msg[1], msg[2]));
            end;
          08:
            begin
              Self.WriteLog(tllCommands, 'GET: function status F13-F28 request (>=3.6)');
              Self.SendLocoFunc13Type(deviceAddr, Self.LokAddrDecode(msg[1], msg[2]));
            end;
          09:
            begin
              Self.WriteLog(tllCommands, 'GET: function status F13-F28 request (>=3.6)');
              Self.SendLocoFunc13(deviceAddr, Self.LokAddrDecode(msg[1], msg[2]));
            end;
          else
            Self.SendNotSupported(deviceAddr);
        end;
      end;

    $E4:
      begin
        case (msg[0]) of
          $10 .. $13:
            begin
              Self.WriteLog(tllCommands, 'GET: locomotive set speed');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              var maxsp: Integer;

              case (msg[0]) of
                $10:
                  maxsp := 14;
                $11:
                  maxsp := 27;
                $12:
                  maxsp := 28;
                $13:
                  maxsp := 128;
              else
                maxsp := 128;
              end;

              var emergencyStop := false;
              var speed := 0;
              case (maxsp) of
                14:
                  begin
                    speed := (msg[3] AND $0F);
                    if (speed = 1) then
                    begin
                      emergencyStop := true;
                      speed := 0;
                    end;
                    if (speed > 0) then
                      Dec(speed);
                    speed := (speed * 2); // normovani rychlosti (28/14)=2
                  end;

                27, 28:
                  begin
                    speed := ((msg[3] AND $0F) shl 1) OR
                      ((msg[3] AND $10) shr 4);
                    if (speed = 2) then
                      emergencyStop := true;
                    if ((speed >= 1) and (speed <= 3)) then
                      speed := 0;
                    if (speed >= 4) then
                      speed := speed - 3;
                  end;

                128:
                  begin
                    speed := (msg[3] AND $7F);
                    if (speed = 1) then
                    begin
                      speed := 0;
                      emergencyStop := true;
                    end;
                    speed := Round(speed * (28 / 128)); // normovani rychlosti
                  end;
              end;

              if (((addr >= 1) and (addr <= _SLOTS_CNT)) and (deviceAddr <> Self.sloty[addr].mausId)) then
              begin
                if (Self.sloty[addr].isMaus) then
                  Self.SendLokoStolen(deviceAddr, addr);
                Self.sloty[addr].mausId := deviceAddr;
              end;

              if ((addr = 0) or (addr > _SLOTS_CNT) or
                (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, Byte(msg[1]), Byte(msg[2]));
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit rychlost a smer

                var tmpSmer := 1 - ((Byte(msg[3]) shr 7) and $1);
                if (emergencyStop) then
                begin
                  // emergency stop -> uvolnit HV ze slotu
                  Self.sloty[addr].ReleaseLoko();
                end
                else
                begin
                  // normal stop
                  if (Self.sloty[addr].total) then
                    Self.sloty[addr].SetRychlostSmer(speed, tmpSmer);
                end;
                if (not Self.sloty[addr].total) then
                  Self.SendLokoStolen(deviceAddr, msg[1], msg[2]);
              end;

            end;

          $20:
            begin
              Self.WriteLog(tllCommands, 'GET: set F0-F4');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or
                (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, msg[1], msg[2]);
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce
                var funkce: TFunkce;
                funkce[0] := boolean((msg[3] shr 4) and $1);
                for var i := 0 to 3 do
                  funkce[i + 1] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(0, 4, funkce);
              end;
            end;

          $21:
            begin
              Self.WriteLog(tllCommands, 'GET: set F5-F8');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or
                (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, Byte(msg[1]), Byte(msg[2]));
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce
                var funkce: TFunkce;
                for var i := 0 to 3 do
                  funkce[i + 5] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(5, 8, funkce);
              end;
            end;

          $22:
            begin
              Self.WriteLog(tllCommands, 'GET: set F9-F12');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or
                (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, msg[1], msg[2]);
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce\
                var funkce: TFunkce;
                for var i := 0 to 3 do
                  funkce[i + 9] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(9, 12, funkce);
              end;
            end;

          $23:
            begin
              Self.WriteLog(tllCommands, 'GET: set F13-F20 (>=3.6)');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, msg[1], msg[2]);
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce\
                var funkce: TFunkce;
                for var i := 0 to 7 do
                  funkce[i + 13] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(13, 20, funkce);
              end;
            end;

          $28:
            begin
              Self.WriteLog(tllCommands, 'GET: set F21-F28 (>=3.6)');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, msg[1], msg[2]);
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce\
                var funkce: TFunkce;
                for var i := 0 to 7 do
                  funkce[i + 21] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(21, 28, funkce);
              end;
            end;

          $F3:
            begin
              Self.WriteLog(tllCommands, 'GET: set F13-F20');

              var addr := Self.LokAddrDecode(msg[1], msg[2]);
              if ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko)) then
              begin
                // lokomotiva neni rizena ovladacem
                // -> odeslat "locomotive is being operated by another device"
                Self.SendLokoStolen(deviceAddr, Byte(msg[1]), Byte(msg[2]));
              end
              else
              begin
                // lokomotiva je rizena ovladacem -> nastavit funkce
                var funkce: TFunkce;
                for var i := 0 to 7 do
                  funkce[i + 13] := boolean((msg[3] shr i) and $1);
                Self.sloty[addr].SetFunctions(13, 20, funkce);
              end;
            end;
        else
          Self.SendNotSupported(deviceAddr);
        end; // case msg.data[2]
      end; // $E4
  else
    Self.SendNotSupported(deviceAddr);
  end; // case msg.data[1]
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ParseuLIMsg(headerByte: Byte; msg: PByte; msgLen: Cardinal);
begin
  case (headerByte) of
    $01:
      begin
        // informative messages
        case (msg[0]) of
          $01:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: USB incoming data timeout');
              F_Main.LogMessage('uLI-ERR: GET: USB incoming data timeout');
            end;
          $02:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: USART incoming data timeout');

              // report the error quite silently
              if ((F_Main.P_ULI.Color = clGreen) or
                (F_Main.P_ULI.Color = clLime)) then
                F_Main.P_ULI.Color := clTeal;

              Inc(Self.ffusartMsgTotalCnt); // will not cause event to fire (ff)
              Self.fusartMsgTimeoutCnt := Self.fusartMsgTimeoutCnt + 1;
              // will cause event to fire (f)
            end;
          $03:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: Unknown command');
              F_Main.LogMessage('uLI-ERR: GET: Unknown command');
            end;
          $04:
            Self.WriteLog(tllCommands, 'GET: OK');
          $05:
            begin
              if (not Self.ignoreKeepAliveLogging) then
                Self.WriteLog(tllChanges, 'GET: keep-alive');
              Self.KAreceiveTimeout := 0;

              if ((F_Main.P_ULI.Color = clGreen) or
                (F_Main.P_ULI.Color = clTeal)) then
                F_Main.P_ULI.Color := clLime
              else if (F_Main.P_ULI.Color = clLime) then
                F_Main.P_ULI.Color := clGreen;
            end;
          $06:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: USB>USART buffer overflow');
              F_Main.LogMessage('uLI-ERR: GET: USB>USART buffer overflow');
            end;
          $07:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: USB XOR error');
              F_Main.LogMessage('uLI-ERR: GET: USB XOR error');
            end;
          $08:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: USB parity error');
              F_Main.LogMessage('uLI-ERR: GET: USB parity error');
            end;
          $09:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power source turned off');
              F_Main.LogMessage('uLI-ERR: GET: XpressNET power source turned off');
            end;
          $0A:
            begin
              Self.WriteLog(tllErrors, 'ERR: GET: XpressNET power transistor closed');
              F_Main.LogMessage('uLI-ERR: GET: XpressNET power transistor closed');
            end;
          $0B:
            begin
              Self.WriteLog(tllErrors, 'WARN: GET: Missed timer');
            end;
          $0C:
            begin
              Self.WriteLog(tllErrors, 'WARN: GET: USART RX Framing error');
            end;
        end;
      end;

    $11:
      begin
        // uLI-master status response
        Self.WriteLog(tllCommands, 'GET: master status');
        Self.ParseuLIStatus(msg, msgLen);

        F_Main.P_ULI.Color := clGreen;
        F_Main.P_ULI.Hint := 'Připojeno k uLI-master, stav vyčten';
      end;

    $13:
      begin
        if (msg[0] = $80) then
        begin
          Self.uLIVersion.hw := IntToStr((msg[1] shr 4) AND $F) + '.' +
            IntToStr(msg[1] AND $F);
          Self.uLIVersion.sw := IntToStr((msg[2] shr 4) AND $F) + '.' +
            IntToStr(msg[2] AND $F);
          Self.WriteLog(tllCommands, 'GET: uLI version hw:' + Self.uLIVersion.hw
            + ', sw:' + Self.uLIVersion.sw);

          F_Main.P_ULI.Hint := 'Připojeno k uLI-master HW='+Self.uLIVersion.hw + ', SW='+Self.uLIVersion.sw;
        end;

      end; // case msg.data[1]
  end; // case
end;

procedure TuLI.ParseuLIStatus(msg: PByte; msgLen: Cardinal);
var
  new: TuLIStatus;
begin
  new.transistor := boolean(msg[0] and 1);
  new.sense := boolean((msg[0] shr 1) and 1);
  new.aliveReceiving := boolean((msg[0] shr 2) and 1);
  new.aliveSending := boolean((msg[0] shr 3) and 1);

  // prijimani a odesilani schvalne obraceno
  // (v recordu jsou data z pohledu uLI-master, timery jsou z pohledu SW v pocitaci)
  Self.tKASendTimer.Enabled := new.aliveReceiving;
  Self.tKAReceiveTimer.Enabled := new.aliveSending;
  Self.KAreceiveTimeout := 0;

  var blackout := ((Self.status.sense) and (not new.sense));
  var turnon := ((not Self.status.sense) and (new.sense) and (not new.transistor));

  if ((not Self.status.sense) and (new.sense)) then
    F_Main.ClearMessage();

  if (not Self.uLIStatusValid) then
  begin
    // these variables must be changed before BroadcastAuth calling
    Self.uLIStatus := new;
    Self.uLIStatusValid := true;
    TCPServer.BroadcastAuth(true);
  end
  else
  begin
    Self.uLIStatus := new;
    Self.uLIStatusValid := true;
  end;

  if (blackout) then
  begin
    // vypadek napajeni multiMaus
    Self.ReleaseAllLoko();
    for var i := 1 to _SLOTS_CNT do
      Self.sloty[i].mausId := TSlot._MAUS_NULL;
    Self.RepaintSlots(F_Slots);
    TCPServer.BroadcastSlots();
    F_Main.LogMessage('Výpadek napájení uLI-master!');
  end
  else if (turnon) then
  begin
    // obnoveni napajeni sbernice -> zapnout ovladace
    try
      if (TCPClient.authorised) then
        Self.busEnabled := true;
    except

    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// Tato funkce funguje jako blokujici.
// Z funkce je vyskoceno ven az po odeslani dat (nebo vyjimce).
// Tato funckce ocekava vstupni data bez xoru na konci
procedure TuLI.Send(callByte: Byte; data: ShortString);
begin
  if (not Self.ComPort.connected) then
  begin
    Self.WriteLog(tllErrors, 'PUT ERR: uLI not connected');
    Exit();
  end;
  if (Length(data) > 18) then
  begin
    Self.WriteLog(tllErrors, 'PUT ERR: Message too long');
    Exit();
  end;

  // xor
  var rawData: ShortString := #$51 + #$15 + AnsiChar(callByte) + data + AnsiChar(Self.Xorxor(data));

  // log
  if ((not Self.ignoreKeepAliveLogging) or (callByte <> $A0) or (Length(data) <> 2) or (ord(data[2]) <> _KEEP_ALIVE)) then
    Self.WriteLog(tllData, 'PUT: ' + Self.BufToStr(rawData));

  var asp: PAsync;
  InitAsync(asp);
  try
    Self.ComPort.WriteAsync(rawData[1], Length(rawData), asp);
    while (not Self.ComPort.IsAsyncCompleted(asp)) do
    begin
      Application.ProcessMessages();
      Sleep(1);
    end;
  except
    on E: Exception do
    begin
      F_Main.LogMessage('uLI-master PUT ERR : ' + E.Message);
      Self.WriteLog(tllErrors, 'PUT ERR: com object error : ' + E.Message);
      if (Self.ComPort.connected) then
        Self.ComPort.Close();
    end;
  end;

  DoneAsync(asp);
end;

procedure TuLI.SendXN(device: Byte; data: ShortString);
begin
  var callByte: Byte := $60 OR (device AND $1F);
  if (Self.Parity(callByte)) then
    callByte := callByte OR $80;
  Self.Send(callByte, data);
end;

procedure TuLI.SenduLI(data: ShortString);
begin
  Self.Send($A0, data);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetLogLevel(new: TuLILogLevel);
begin
  Self.fLogLevel := new;
  Self.WriteLog(tllCommands, 'NEW LOGLEVEL: ' + IntToStr(Integer(new)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.EnumDevices(const Ports: TStringList);
begin { EnumComPorts }
  with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('hardware\devicemap\serialcomm', false) then
        try
          Ports.BeginUpdate();
          try
            GetValueNames(Ports);
            for var nInd := Ports.Count - 1 downto 0 do
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

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetStatus(new: TuLIStatus);
begin
  Self.WriteLog(tllCommands, 'PUT: status');
  var data: Byte := $A0 + Integer(new.transistor) + (Integer(new.aliveReceiving) shl 2) +
    (Integer(new.aliveSending) shl 3);
  Self.SenduLI(#$11 + AnsiChar(data));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.CreateBuf(str: ShortString): TBuffer;
begin
  Result.Count := Length(str);
  for var i := 0 to Result.Count - 1 do
    Result.data[i] := ord(str[i + 1]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.OntKASendTimer(Sender: TObject);
begin
  Self.SendKeepAlive();
end;

procedure TuLI.OntKAReceiveTimer(Sender: TObject);
begin
  Inc(Self.KAreceiveTimeout);
  if (Self.KAreceiveTimeout > _KA_RECEIVE_TIMEOUT_TICKS) then
  begin
    Self.KAreceiveTimeout := 0;
    Self.WriteLog(tllErrors, 'uLI neodpovědělo na keep-alive!');
    F_Main.LogMessage('uLI neodpovědělo na keep-alive!');
    Self.Close();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendKeepAlive();
begin
  if (not Self.ignoreKeepAliveLogging) then
    Self.WriteLog(tllChanges, 'SEND: keep-alive');
  Self.SenduLI(#$01 + #$05);
end;

procedure TuLI.SendStatusRequest();
begin
  Self.WriteLog(tllChanges, 'SEND: status request');
  Self.SenduLI(#$11 + #$A2);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLi.CheckAddrChangeOK(deviceAddr: Byte; addr: Integer): Boolean;
var
  changed: boolean;
begin
  changed := false;

  // kontrola adresdy loko/slotu na danem ovladaci
  var addrOld := Self.FindSlot(deviceAddr);
  if ((addrOld > -1) and (addrOld <> addr)) then
  begin
    // na ovladaci doslo ke zmene adresy z addrOld na addr
    // -> odstranit slot addrOld
    Self.sloty[addrOld].ReleaseLoko();
    Self.sloty[addrOld].mausId := TSlot._MAUS_NULL;
    changed := true;
  end;

  // kontrola, zda adresa loko/slotu je v platnem rozsahu
  if (((addr >= 1) and (addr <= _SLOTS_CNT)) and (not Self.sloty[addr].isMaus))
  then
  begin
    // obsazujeme slot adresou
    Self.sloty[addr].mausId := deviceAddr;
    changed := true;
  end;

  if (changed) then
  begin
    Self.RepaintSlots(F_Slots);
    TCPServer.BroadcastSlots();
  end;

  Result := not ((addr = 0) or (addr > _SLOTS_CNT) or (not Self.sloty[addr].isLoko) or
    (Self.sloty[addr].ukradeno));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLocoData(deviceAddr: Byte; addr: Integer);
var
  toSend: ShortString;
begin
  toSend := #$E4;

  if (CheckAddrChangeOK(deviceAddr, addr)) then
  begin
    // lokomotiva je rizena ovladacem
    toSend := toSend + AnsiChar
      (2 + (Byte(Self.sloty[addr].mausId <> deviceAddr) shl 3));

    // rychlost + smer
    begin
      var tmp2: Integer;
      case (Self.sloty[addr].rychlost_stupne) of
        0:
          tmp2 := 0;
        1 .. 28:
          tmp2 := Self.sloty[addr].rychlost_stupne + 3;
      else
        tmp2 := 0;
      end;

      var tmp := (((1 - Self.sloty[addr].smer) AND $1) shl 7) + ((tmp2 AND $1E) shr 1)
          + ((tmp2 AND $01) shl 4);
      toSend := toSend + AnsiChar(tmp);
    end;

    // F0 - F4
    begin
      var tmp: Integer := (Byte(Self.sloty[addr].funkce[0]) shl 4);
      for var i := 1 to 4 do
        tmp := tmp + (Byte(Self.sloty[addr].funkce[i]) shl (i - 1));
      toSend := toSend + AnsiChar(tmp);
    end;

    // F5 - F12
    begin
      var tmp: Integer := 0;
      for var i := 5 to 12 do
        tmp := tmp + (Byte(Self.sloty[addr].funkce[i]) shl (i - 5));
      toSend := toSend + AnsiChar(tmp);
    end;

    Self.sloty[addr].mausFunkce := Self.sloty[addr].funkce;
    Self.WriteLog(tllCommands, 'PUT: locomotive information');

  end else begin
    // lokomotiva neni rizena ovladacem
    if ((addr > 0) and (addr < _SLOTS_CNT)) then
      for var i := 0 to _MAX_FUNC do
        Self.sloty[addr].mausFunkce[i] := false;
    Self.WriteLog(tllCommands, 'PUT: locomotive is busy - empty slot');
    toSend := toSend + #$A + #$80 + #0 + #0;
  end;

  Self.SendXN(deviceAddr, toSend);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLocoFunc13(deviceAddr: Byte; addr: Integer);
var
  toSend: ShortString;
begin
  toSend := #$E4;
  // Kennung (static)
  toSend := toSend + #$52;

  if (CheckAddrChangeOK(deviceAddr, addr)) then
  begin
    // lokomotiva je rizena ovladacem

    // F13 - F20
    begin
      var tmp: Integer := 0;
      for var i := 13 to 20 do
        tmp := tmp + (Byte(Self.sloty[addr].funkce[i]) shl (i - 13));
      toSend := toSend + AnsiChar(tmp);
    end;

    // F21 - F28
    begin
      var tmp: Integer := 0;
      for var i := 21 to 28 do
        tmp := tmp + (Byte(Self.sloty[addr].funkce[i]) shl (i - 21));
      toSend := toSend + AnsiChar(tmp);
    end;

    // R (emulated)
    toSend := toSend + #$03; // 28 speed steps

    Self.sloty[addr].mausFunkce := Self.sloty[addr].funkce;
    Self.WriteLog(tllCommands, 'PUT: function F13-F28 information');
  end else begin
    // no good addres or slot, send empty response
    toSend := toSend + #$00 + #$00 + #$03;
    Self.WriteLog(tllCommands, 'PUT: function F13-F28 information empty');
  end;

  Self.SendXN(deviceAddr, toSend);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLocoFuncType(deviceAddr: Byte; addr: Integer);
var
  toSend: ShortString;
begin
  toSend := #$E3 + #$50;

  if (CheckAddrChangeOK(deviceAddr, addr)) then
  begin
    // lokomotiva je rizena ovladacem

    // F0 - F4
    begin
      var tmp: Integer := (Byte(Self.sloty[addr].funkceType[0]) shl 4);
      for var i := 1 to 4 do
        tmp := tmp + (Byte(Self.sloty[addr].funkceType[i]) shl (i - 1));
      toSend := toSend + AnsiChar(tmp);
    end;

    // F5 - F12
    begin
      var tmp: Integer := 0;
      for var i := 5 to 12 do
        tmp := tmp + (Byte(Self.sloty[addr].funkceType[i]) shl (i - 5));
      toSend := toSend + AnsiChar(tmp);
    end;

    Self.sloty[addr].mausFunkce := Self.sloty[addr].funkce;
    Self.WriteLog(tllCommands, 'PUT: function F0-F12 momentary information');
  end else begin
    // no good addres or slot, send empty response
    toSend := toSend + #$00 + #$00;
    Self.WriteLog(tllCommands, 'PUT: function F0-F12 momentary information empty');
  end;

  Self.SendXN(deviceAddr, toSend);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLocoFunc13Type(deviceAddr: Byte; addr: Integer);
var
  toSend: ShortString;
begin
  toSend := #$E4 + #$51;

  if (CheckAddrChangeOK(deviceAddr, addr)) then
  begin
    // lokomotiva je rizena ovladacem

    // F13 - F20
    begin
      var tmp: Integer := 0;
      for var i := 13 to 20 do
        tmp := tmp + (Byte(Self.sloty[addr].funkceType[i]) shl (i - 13));
      toSend := toSend + AnsiChar(tmp);
    end;

    // F21 - F28
    begin
      var tmp: Integer := 0;
      for var i := 21 to 28 do
        tmp := tmp + (Byte(Self.sloty[addr].funkceType[i]) shl (i - 21));
      toSend := toSend + AnsiChar(tmp);
    end;

    // R (emulated)
    toSend := toSend + #$03; // 28 speed steps

    Self.sloty[addr].mausFunkce := Self.sloty[addr].funkce;
    Self.WriteLog(tllCommands, 'PUT: function F13-F28 momentary information');
  end else begin
    // no good addres or slot, send empty response
    Self.WriteLog(tllCommands, 'PUT: function F13-F28 momentary information empty');
    toSend := toSend + #$00 + #$00 + #$03;
  end;

  Self.SendXN(deviceAddr, toSend);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendNotSupported(deviceAddr: Byte);
begin
  Self.WriteLog(tllCommands, 'PUT: command not supported');
  Self.SendXN(deviceAddr, #$61 + #$82);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetBusActive(new: boolean);
begin
  if ((new) and (not Self.uLIStatusValid)) then
  begin
    Self.SendStatusRequest();
    raise EuLIStatusInvalid.Create('Program nemá validní data o stavu uLI!');
  end;

  if ((new) and (not Self.uLIStatus.sense)) then
    raise EPowerTurnedOff.Create('Zařízení není napájeno!');

  if (not new) then
  begin
    for var i := 1 to _SLOTS_CNT do
      Self.sloty[i].mausId := TSlot._MAUS_NULL;
    Self.RepaintSlots(F_Slots);
  end;

  var newStatus := Self.uLIStatus;
  newStatus.transistor := new;
  Self.SetStatus(newStatus);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetDCC(new: boolean);
begin
  if (Self.fDCC = new) then
    Exit();
  Self.fDCC := new;

  if (Self.ComPort.connected) then
  begin
    if (new) then
    begin
      Self.SendXN(0, _CMD_DCC_ON);
      Self.SendXN(0, _CMD_DCC_ON);
    end
    else
    begin
      Self.SendXN(0, _CMD_DCC_OFF);
      Self.SendXN(0, _CMD_DCC_OFF);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.LokAddrEncode(addr: Integer): Word;
begin
  if (addr > 99) then
  begin
    Result := (addr + $C000);
  end
  else
  begin
    Result := addr;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.LokAddrDecode(ah, al: Byte): Integer;
begin
  Result := al or ((ah AND $3F) shl 8);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SendLokoStolen(deviceAddr: Byte; addrHi: Byte; addrLo: Byte);
begin
  Self.WriteLog(tllCommands, 'PUT: locomotive is being operated by another device');
  Self.SendXN(deviceAddr, #$E3 + #$40 + AnsiChar(addrHi) + AnsiChar(addrLo));
end;

procedure TuLI.SendLokoStolen(deviceAddr: Byte; addr: Word);
begin
  var encoded: Word := Self.LokAddrEncode(addr);
  Self.SendLokoStolen(deviceAddr, (encoded shr 8) and $FF, encoded AND $FF);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.FindSlot(mausId: Byte): Integer;
begin
  for var i := 1 to _SLOTS_CNT do
    if (Self.sloty[i].mausId = mausId) then
      Exit(i);
  Exit(-1);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.CalcParity(data: Byte): Byte;
var
  parity: boolean;
  tmp: Byte;
begin
  tmp := data;
  parity := false;
  for var i := 0 to 7 do
  begin
    if ((tmp AND $1) > 0) then
      parity := not parity;
    tmp := tmp shr 1;
  end;
  if (parity) then
    Result := data + $80
  else
    Result := data;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.GetConnected(): boolean;
begin
  Result := Self.ComPort.connected;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.RepaintSlots(form: TForm);
var
  cnt: Integer;
begin
  cnt := 0;
  for var i := 1 to _SLOTS_CNT do
    if (Self.sloty[i].isMaus) then
      Inc(cnt);

  var j := 0;
  for var i := 1 to _SLOTS_CNT do
  begin
    if ((Self.sloty[i].isMaus) and (Self.busEnabled) and (TCPClient.authorised))
    then
    begin
      Self.sloty[i].Show(form, j, cnt);
      Inc(j);
    end
    else
      Self.sloty[i].HideGUI();
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.HardResetSlots();
begin
  for var i := 1 to _SLOTS_CNT do
    Self.sloty[i].HardResetSlot();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ReleaseAllLoko();
begin
  for var i := 1 to _SLOTS_CNT do
    Self.sloty[i].ReleaseLoko();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.GetBusActive(): boolean;
begin
  Result := ((uLIStatusValid) and (uLIStatus.transistor) and (uLIStatus.sense));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.SetUsartMsgTotalCnt(new: Cardinal);
begin
  if (Self.ffusartMsgTotalCnt <> new) then
  begin
    Self.ffusartMsgTotalCnt := new;
    if (Assigned(Self.fOnUsartMsgCntChange)) then
      Self.fOnUsartMsgCntChange(Self);
  end
  else
  begin
    Self.ffusartMsgTotalCnt := new;
  end;
end;

procedure TuLI.SetUsartMsgTimeoutCnt(new: Cardinal);
begin
  if (Self.ffusartMsgTimeoutCnt <> new) then
  begin
    Self.ffusartMsgTimeoutCnt := new;
    if (Assigned(Self.fOnUsartMsgCntChange)) then
      Self.fOnUsartMsgCntChange(Self);
  end
  else
  begin
    Self.ffusartMsgTimeoutCnt := new;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TuLI.ResetUsartCounters();
begin
  Self.ffusartMsgTotalCnt := 0; // will not cause an event to fire
  Self.fusartMsgTimeoutCnt := 0; // will cause an event to fire
end;

/// /////////////////////////////////////////////////////////////////////////////

function TuLI.Parity(b: Byte): Boolean;
begin
  Result := False;
  for var i := 0 to 7 do
  begin
    if ((b AND 1) = 1) then
      Result := not Result;
    b := b shr 1;
  end;
end;

function TuLI.Xorxor(data: array of Byte; from: Cardinal; len: Cardinal): Byte;
begin
  Result := 0;
  for var i: Cardinal := from to from+len-1 do
    Result := Result xor data[i];
end;

function TuLI.Xorxor(data: ShortString): Byte;
begin
  Result := 0;
  for var i: Cardinal := 1 to Length(data) do
    Result := Result xor ord(data[i]);
end;

function TuLI.BufToStr(data: PByte; from: Cardinal; len: Cardinal): string;
begin
  Result := '';
  for var i := from to from+len-1 do
  begin
    Result := Result + IntToHex(Fbuf_in.data[i], 2);
    if (i < (from+len-1)) then
      Result := Result + ' ';
  end;
end;

function TuLI.BufToStr(data: PByte; len: Cardinal): string;
begin
  Result := Self.BufToStr(data, 0, len);
end;

function TuLI.BufToStr(data: ShortString): string;
begin
  Result := '';
  for var i := 1 to Length(data) do
  begin
    Result := Result + IntToHex(ord(data[i]), 2);
    if (i < Length(data)) then
      Result := Result + ' ';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

uLI := TuLI.Create();

finalization

FreeAndNil(uLI);

end.
