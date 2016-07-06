// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program uLIdaemon;

uses
  Forms,
  Windows,
  SysUtils,
  fMain in 'fMain.pas' {F_Main},
  Verze in 'Verze.pas';

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
  Application.Run;
end.
