// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program uLIdaemon;

uses
  Forms,
  fMain in 'fMain.pas' {F_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'uLI-daemon';
  Application.CreateForm(TF_Main, F_Main);
  Application.Run;
end.
