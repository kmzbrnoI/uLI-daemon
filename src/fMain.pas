unit fMain;

{
  Unit hlavniho okynka, tady se resi predevsim interakce s GUI.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tUltimateLIConst, ActnList, StdCtrls;

type
  TF_Main = class(TForm)
    AL_Main: TActionList;
    A_Debug: TAction;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure FormShow(Sender: TObject);
    procedure A_DebugExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }

  public

     procedure OnuLILog(Sender:TObject; lvl:TuLILogLevel; msg:string);

  end;

var
  F_Main: TF_Main;

implementation

uses Verze, fDebug, tUltimateLI, WbemScripting_TLB, ActiveX;

{$R *.dfm}

procedure TF_Main.A_DebugExecute(Sender: TObject);
begin
 F_Debug.Show();
end;

procedure TF_Main.Button1Click(Sender: TObject);
var ports:TStringList;
    i:Integer;
begin
 ports := TStringList.Create();
 uLI.EnumDevices(ports);

 Self.ListBox1.Clear();
 for i := 0 to ports.Count-1 do
   Self.ListBox1.Items.Add(ports[i]);

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

  Memo1.Lines.Clear;
  Locator:= CoSWbemLocator.Create;
  Services:=  Locator.ConnectServer('.', 'root\cimv2', '', '', '','', 0, nil);
  ObjSet:= Services.InstancesOf('Win32_PnPEntity', wbemFlagReturnWhenComplete, nil);
  Enum:= (ObjSet._NewEnum) as IEnumVariant;
  while (Enum.Next(1, tempObj, Value) = S_OK) do begin
    SObject:= IUnknown(tempObj) as SWBemObject;
    PropSet := SObject.Properties_;
    SProp  := PropSet.Item('Description', 0);
    sValue := SProp.Get_Value;
    Memo1.Lines.Add(sValue);

    SProp  := PropSet.Item('Caption',0);
    sValue := SProp.Get_Value;
    Memo1.Lines.Add('- '+sValue);

    SProp  := PropSet.Item('DeviceID',0);
    sValue := SProp.Get_Value;
    Memo1.Lines.Add('- '+sValue);
  end;
end;

procedure TF_Main.Button3Click(Sender: TObject);
begin
 uLI.Open('COM4');
end;

procedure TF_Main.Button4Click(Sender: TObject);
var new: TuLIStatus;
begin
 new.transistor := true;
 new.aliveReceiving := true;
 new.aliveSending := true;
 uLI.SetStatus(new);
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
 uLI.logLevel := tllCommands;
 uLI.OnLog    := Self.OnuLILog;
end;

procedure TF_Main.FormShow(Sender: TObject);
begin
 Self.Caption := 'uLI-daemon v'+GetVersion(Application.ExeName)+' (build '+GetLastBuildDate()+')';
end;

procedure TF_Main.OnuLILog(Sender:TObject; lvl:TuLILogLevel; msg:string);
begin
 if (Assigned(F_Debug)) then F_Debug.Log('uLI: '+msg);
end;

end.//unit
