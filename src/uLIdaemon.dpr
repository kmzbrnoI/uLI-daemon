// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program uLIdaemon;

uses
  Forms,
  Windows,
  SysUtils,
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
  fSlots in 'fSlots.pas' {F_slots};

{$R *.res}

var
  Mutex: Cardinal;

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

  try
   TCPServer.Start();
  except
   on E:Exception do
     Application.MessageBox(PChar('Nepodaøilo se nastartovat bridge server'+#13#10+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;

  Application.Run;
end.
