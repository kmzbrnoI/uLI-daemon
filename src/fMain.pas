unit fMain;

{
  Unit hlavniho okynka, tady se resi predevsim interakce s GUI.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tUltimateLIConst, ActnList, StdCtrls, ComCtrls, ExtCtrls;

type
  TF_Main = class(TForm)
    AL_Main: TActionList;
    A_Debug: TAction;
    SB_Main: TStatusBar;
    A_ServerConnect: TAction;
    A_ServerDisconnect: TAction;
    procedure FormShow(Sender: TObject);
    procedure A_DebugExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SB_MainDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }

  public

    S_Server : TShape;
    S_Client : TShape;
    S_ULI    : TShape;
    close_app : boolean;

    activeMDIform : TForm;

     procedure OnuLILog(Sender:TObject; lvl:TuLILogLevel; msg:string);
     procedure CreateShapes();
     procedure LogMessage(msg:string);

  end;

var
  F_Main: TF_Main;

implementation

uses Verze, fDebug, tUltimateLI, WbemScripting_TLB, ActiveX, client, fSlots;

{$R *.dfm}

procedure TF_Main.A_DebugExecute(Sender: TObject);
begin
 F_Debug.Show();
end;

procedure TF_Main.Button1Click(Sender: TObject);
var ports:TStringList;
//   i:Integer;
begin
 ports := TStringList.Create();
 uLI.EnumDevices(ports);

{ Self.ListBox1.Clear();
 for i := 0 to ports.Count-1 do
   Self.ListBox1.Items.Add(ports[i]); }

 ports.Free();
end;

procedure TF_Main.Button2Click(Sender: TObject);
var
  Locator:  ISWbemLocator;
  Services: ISWbemServices;
  ObjSet:   ISWbemObjectSet;
  SObject:  ISWbemObject;
  PropSet:  ISWbemPropertySet;
  SProp:    ISWbemProperty;
  sValue:   String;
  Enum:     IEnumVariant;
  Value:    Cardinal;
  TempObj:  OleVariant;

begin

//  Memo1.Lines.Clear;
  Locator:= CoSWbemLocator.Create;
  Services:=  Locator.ConnectServer('.', 'root\cimv2', '', '', '','', 0, nil);
  ObjSet:= Services.InstancesOf('Win32_PnPEntity', wbemFlagReturnWhenComplete, nil);
  Enum:= (ObjSet._NewEnum) as IEnumVariant;
  while (Enum.Next(1, tempObj, Value) = S_OK) do begin
    SObject:= IUnknown(tempObj) as SWBemObject;
    PropSet := SObject.Properties_;
    SProp  := PropSet.Item('Description', 0);
    sValue := SProp.Get_Value;
//    Memo1.Lines.Add(sValue);

    SProp  := PropSet.Item('Caption',0);
    sValue := SProp.Get_Value;
//    Memo1.Lines.Add('- '+sValue);

    SProp  := PropSet.Item('DeviceID',0);
    sValue := SProp.Get_Value;
//    Memo1.Lines.Add('- '+sValue);
  end;
end;

procedure TF_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if (not Self.close_app) then
  begin
   if (uLI.connected) then
    begin
     Self.close_app := true;
     uLI.Close();
     CanClose := false;
    end;

   if (TCPClient.status <> TPanelConnectionStatus.closed) then
    begin
     Self.close_app := true;  // informujeme OnDisconnect, ze ma zavrit okno
     TCPClient.Disconnect();
     CanClose := false;       // okno zatim nezavirame, zavre se az pri OnDisconnect
    end;
  end;
end;

procedure TF_Main.FormCreate(Sender: TObject);
var fSlots:TF_Slots;
begin
 Self.activeMDIform := nil;

 Self.CreateShapes();

 uLI.logLevel := tllData;
 uLI.OnLog    := Self.OnuLILog;

 try
   uLI.Open('COM4');
 except
   // TODO
 end;

 fSlots := TF_slots.Create(Self);
 fSlots.Parent := Self;
 fSlots.Show();
 Self.activeMDIform := fSlots;
end;

procedure TF_Main.FormResize(Sender: TObject);
begin
 if (Assigned(Self.activeMDIform) and (Assigned(Self.activeMDIform.OnResize))) then Self.activeMDIform.OnResize(Self);
end;

procedure TF_Main.FormShow(Sender: TObject);
begin
 Self.Caption := 'uLI-daemon v'+GetVersion(Application.ExeName)+' (build '+GetLastBuildDate()+')';
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
 S_Server := TShape.Create(SB_Main);
 with (S_Server) do
  begin
   Parent := SB_Main;
   Left := 1;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   Hint := 'Bridge server: vyppnut';
   Brush.Color := clRed;
  end;

 S_ULI := TShape.Create(SB_Main);
 with (S_ULI) do
  begin
   Parent := SB_Main;
   Left := 32;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   Hint := 'Odpojeno od uLI-master';
   Brush.Color := clRed;
  end;

 S_Client := TShape.Create(SB_Main);
 with (S_Client) do
  begin
   Parent := SB_Main;
   Left := 63;
   Top  := 2;
   Height := 16;
   Width := 30;
   ShowHint := true;
   Hint := 'Odpojeno od serveru';
   Brush.Color := clRed;
  end;
end;

procedure TF_Main.LogMessage(msg:string);
begin
 Self.SB_Main.Panels[1].Text := msg;
 Self.SB_Main.Hint := msg;
end;

end.//unit
