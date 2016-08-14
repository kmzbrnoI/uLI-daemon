unit fConnect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generics.Collections;

const
  ULI_DEVICE_DESCRIPTION = 'uLI - master';

type
  TF_Connect = class(TForm)
    GB_Connect: TGroupBox;
    Label1: TLabel;
    LB_Ports: TListBox;
    CHB_Remember: TCheckBox;
    B_Connect: TButton;
    B_Update: TButton;
    procedure FormResize(Sender: TObject);
    procedure B_UpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure B_ConnectClick(Sender: TObject);
  private
    procedure UpdateList();
  public
    { Public declarations }
  end;

var
  F_Connect: TF_Connect;

implementation

{$R *.dfm}

uses comDiscovery, GlobalConfig, tUltimateLI, fMain, fSlots;

procedure TF_Connect.B_ConnectClick(Sender: TObject);
var comPort:string;
begin
 if (Self.LB_Ports.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte COM port!', 'Vyber COM port', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 comPort := Self.LB_Ports.Items[Self.LB_Ports.ItemIndex];
 if (Self.CHB_Remember.Checked) then
   GlobConfig.port.port := comPort
 else
   GlobConfig.port.port := '';

 try
   Self.B_Connect.Enabled := false;
   uLI.Open(comPort);
   F_Main.ShowChild(F_Slots);
 except
   on E:Exception do
     Application.MessageBox(PChar('Nepodaøilo se otevøít COM port '+comPort+'.'+#13#10+E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
 end;

 Self.B_Connect.Enabled := true;
end;

procedure TF_Connect.B_UpdateClick(Sender: TObject);
begin
 Self.UpdateList();
end;

procedure TF_Connect.FormResize(Sender: TObject);
begin
 // vycentrovat GB_Connect
 Self.GB_Connect.Left := (Self.ClientWidth div 2) - (Self.GB_Connect.Width div 2);
 Self.GB_Connect.Top  := (Self.ClientHeight div 2) - (Self.GB_Connect.Height div 2);
end;

procedure TF_Connect.FormShow(Sender: TObject);
begin
 Self.UpdateList();
 Self.CHB_Remember.Checked := false;  // TODO settings
end;

procedure TF_Connect.UpdateList();
var portsList:TList<Integer>;
    num:Integer;
begin
 try
   Self.B_Update.Enabled := false;
   portsList := TList<Integer>.Create();
   Self.LB_Ports.Clear();

   EnumuLIDevices(ULI_DEVICE_DESCRIPTION, portsList);

   for num in portsList do
     Self.LB_Ports.Items.Add('COM'+IntToStr(num));

   if (Self.LB_Ports.Count = 1) then
    Self.LB_Ports.ItemIndex := 0
   else
    Self.LB_Ports.ItemIndex := -1;

   portsList.Clear();
 finally
   Self.B_Update.Enabled := true;
 end;
end;

end.//unit
