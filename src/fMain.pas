unit fMain;

{
  Unit hlavniho okynka, tady se resi predevsim interakce s GUI.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tUltimateLIConst, ActnList, StdCtrls, ComCtrls, ExtCtrls,
  Generics.Collections;

type
  TF_Main = class(TForm)
    AL_Main: TActionList;
    A_Debug: TAction;
    SB_Main: TStatusBar;
    A_ServerConnect: TAction;
    A_ServerDisconnect: TAction;
    procedure FormShow(Sender: TObject);
    procedure A_DebugExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SB_MainDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure PuLIOnDblClick(Sender:TObject);

  public

    P_Server : TPanel;
    P_Client : TPanel;
    P_ULI    : TPanel;
    close_app : boolean;

    activeMDIform : TForm;

     procedure OnuLILog(Sender:TObject; lvl:TuLILogLevel; msg:string);
     procedure CreateShapes();
     procedure LogMessage(msg:string);
     procedure UpdateTitle();
     procedure ShowChild(form:TForm);
     procedure Open(port:string);

  end;

var
  F_Main: TF_Main;

implementation

uses Verze, fDebug, tUltimateLI, WbemScripting_TLB, ActiveX, client, fSlots,
      GlobalConfig, comDiscovery, fConnect;

{$R *.dfm}

procedure TF_Main.A_DebugExecute(Sender: TObject);
begin
 F_Debug.Show();
end;

procedure TF_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 GlobConfig.data.frmPos.X := Self.Left;
 GlobConfig.data.frmPos.Y := Self.Top;
 GlobConfig.data.frmSize.X := Self.Width;
 GlobConfig.data.frmSize.Y := Self.Height;
 GlobConfig.SaveFile();
end;

procedure TF_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if (not Self.close_app) then
  begin
   if (TCPClient.status <> TPanelConnectionStatus.closed) then
    begin
     Self.close_app := true;  // informujeme OnDisconnect, ze ma zavrit okno
     TCPClient.Disconnect();
     CanClose := false;       // okno zatim nezavirame, zavre se az pri OnDisconnect
    end;

   if (uLI.connected) then
    begin
     Self.close_app := true;
     uLI.Close();
     CanClose := false;
    end;
  end;
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
 Self.activeMDIform := nil;

 Self.CreateShapes();

 uLI.logLevel := tllNo;
 uLI.OnLog    := Self.OnuLILog;

 GlobConfig.LoadFile();

 Self.Width  := GlobConfig.data.frmSize.X;
 Self.Height := GlobConfig.data.frmSize.Y;

 if ((GlobConfig.data.frmPos.X >= 0) and (GlobConfig.data.frmPos.Y >= 0) and
   (GlobConfig.data.frmPos.X+100 < Screen.Width) and (GlobConfig.data.frmPos.Y+100 < Screen.Height)) then
  begin
   Self.Left := GlobConfig.data.frmPos.X;
   Self.Top := GlobConfig.data.frmPos.Y;
  end;
end;

procedure TF_Main.FormResize(Sender: TObject);
begin
 if (Assigned(Self.activeMDIform) and (Assigned(Self.activeMDIform.OnResize))) then Self.activeMDIform.OnResize(Self);
end;

procedure TF_Main.FormShow(Sender: TObject);
begin
 Self.UpdateTitle();
end;

procedure TF_Main.OnuLILog(Sender:TObject; lvl:TuLILogLevel; msg:string);
begin
 if (Assigned(F_Debug)) then F_Debug.Log('uLI: '+msg);
end;

procedure TF_Main.SB_MainDblClick(Sender: TObject);
begin
 Self.SB_Main.Panels[1].Text := '';
 Self.SB_Main.Hint := '';
end;

procedure TF_Main.CreateShapes();
begin
 P_Server := TPanel.Create(SB_Main);
 with (P_Server) do
  begin
   Parent := SB_Main;
   ParentBackground := false;
   Left := 1;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   BevelOuter := bvNone;
   Hint := 'Bridge server: vypnut';
   Color := clRed;
   Caption := '';
  end;

 P_ULI := TPanel.Create(SB_Main);
 with (P_ULI) do
  begin
   Parent := SB_Main;
   ParentBackground := false;
   Left := 32;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   BevelOuter := bvNone;
   Hint := 'Odpojeno od uLI-master';
   Color := clRed;
   Caption := '';
   OnDblClick := Self.PuLIOnDblClick;
  end;

 P_Client := TPanel.Create(SB_Main);
 with (P_Client) do
  begin
   Parent := SB_Main;
   ParentBackground := false;
   Left := 63;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   BevelOuter := bvNone;
   Hint := 'Odpojeno od hJOP serveru';
   Color := clRed;
   Caption := '';
  end;
end;

procedure TF_Main.LogMessage(msg:string);
begin
 Self.SB_Main.Panels[1].Text := msg;
 Self.SB_Main.Hint := msg;
end;

procedure TF_Main.UpdateTitle();
begin
 Self.Caption := 'uLI-daemon v'+GetVersion(Application.ExeName)+' (build '+GetLastBuildDate()+')';

 if (TCPClient.user <> '') then
   Self.Caption := Self.Caption + ' (' + TCPClient.user + ')';
end;

procedure TF_Main.ShowChild(form:TForm);
begin
 if (Assigned(Self.activeMDIform)) then Self.activeMDIform.Close();
 form.Parent := Self;
 form.Show();
 Self.activeMDIform := form;
end;

procedure TF_Main.Open(port:string);
begin
 try
   uLI.Open(port);
   Self.ShowChild(F_Slots);
 except
   on E:Exception do
    begin
     Application.MessageBox(PChar('Nepodaøilo se otevøít COM port '+port+'.'+#13#10+E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
     // okno pripojeni k COM portu se zobrazi automaticky - v tom ComAfterClose
    end;
 end;
end;

procedure TF_Main.PuLIOnDblClick(Sender:TObject);
begin
 if (uLI.connected) then
  begin
   Application.MessageBox(PChar('uLI-master verze HW:'+uLI.version.hw+', SW:'+uLI.version.sw), 'Info', MB_OK OR MB_ICONINFORMATION);
  end;
end;

end.//unit
