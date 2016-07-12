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
  ORList in 'ORList.pas';

{$R *.res}

var
  Mutex: Cardinal;

begin
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
  DecimalSeparator := '.';

  // povolena pouze jedna instance uLI-daemon
  Mutex := CreateMutex(nil, True, 'uLI-daemon');
  if ((Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS)) then
   begin
    Application.MessageBox('uLI-daemon již spuštìn, povolena pouze jedna instance.', 'Již spuštìn', MB_ICONWARNING OR MB_OK);
    halt(0);
   end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'uLI-daemon';
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Debug, F_Debug);
  Application.Run;
end.
