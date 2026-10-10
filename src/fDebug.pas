unit fDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrUtils, tUltimateLIConst;

type
  TF_Debug = class(TForm)
    LV_Log: TListView;
    M_Data: TMemo;
    B_ClearLog: TButton;
    Label1: TLabel;
    L_len: TLabel;
    Label2: TLabel;
    CHB_KeepAlive: TCheckBox;
    CHB_PingLogging: TCheckBox;
    Label3: TLabel;
    CB_Loglevel: TComboBox;
    procedure B_ClearLogClick(Sender: TObject);
    procedure LV_LogChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_LogCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure M_DataChange(Sender: TObject);
    procedure CHB_KeepAliveClick(Sender: TObject);
    procedure CB_LoglevelChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Log(msg: string; lvl: TuLILogLevel);
  end;

var
  F_Debug: TF_Debug;

implementation

uses tUltimateLI;

{$R *.dfm}

procedure TF_Debug.B_ClearLogClick(Sender: TObject);
begin
  Self.M_Data.Clear();
  Self.LV_Log.Clear();
end;

procedure TF_Debug.LV_LogChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (Assigned(Self.LV_Log.Selected)) then
    Self.M_Data.Text := Self.LV_Log.Selected.SubItems.Strings[0]
  else
    Self.M_Data.Text := '';
end;

procedure TF_Debug.LV_LogCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if (Item.SubItems.Count < 1) then
    Exit();

  if (LeftStr(Item.SubItems.Strings[0], 3) = 'GET') then
    Self.LV_Log.Canvas.Brush.Color := $FFEEEE;
  if (LeftStr(Item.SubItems.Strings[0], 4) = 'SEND') then
    Self.LV_Log.Canvas.Brush.Color := $EEFFEE;
end;

procedure TF_Debug.M_DataChange(Sender: TObject);
var
  len: Cardinal;
begin
  if (Length(Self.M_Data.Text) >= 5) then
    len := Length(Self.M_Data.Text) - 5
  else
    len := Length(Self.M_Data.Text);

  Self.L_len.Caption := IntToStr(len div 1000) + ' ' + IntToStr(len mod 1000);
end;

procedure TF_Debug.CB_LoglevelChange(Sender: TObject);
begin
  uLI.logLevel := TuLILogLevel(Self.CB_Loglevel.ItemIndex);
end;

procedure TF_Debug.CHB_KeepAliveClick(Sender: TObject);
begin
  uLI.ignoreKeepAliveLogging := not Self.CHB_KeepAlive.Checked;
end;

procedure TF_Debug.FormDestroy(Sender: TObject);
begin
  F_Debug := nil;
end;

procedure TF_Debug.Log(msg: string; lvl: TuLILogLevel);
begin
  if (F_Debug = nil) then
    Exit();

  if (lvl > TuLILogLevel(Self.CB_Loglevel.ItemIndex)) then
    Exit();

  if ((not Self.CHB_PingLogging.Checked) and ((ContainsStr(msg, '-;PING')) or
    (ContainsStr(msg, '-;PONG')))) then
    Exit();

  if (Self.LV_Log.Items.Count > 200) then
    Self.LV_Log.Clear();

  var LI := Self.LV_Log.Items.Insert(0);
  LI.Caption := FormatDateTime('hh:nn:ss,zzz', Now);
  LI.SubItems.Add(msg);
end;

end.// unit
