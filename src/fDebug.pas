unit fDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrUtils, tUltimateLIConst, Vcl.ExtCtrls;

type
  TF_Debug = class(TForm)
    LV_Log: TListView;
    P_Header: TPanel;
    B_ClearLog: TButton;
    CHB_PingLogging: TCheckBox;
    CHB_KeepAlive: TCheckBox;
    CB_Loglevel: TComboBox;
    Label3: TLabel;
    procedure B_ClearLogClick(Sender: TObject);
    procedure LV_LogCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CHB_KeepAliveClick(Sender: TObject);
    procedure CB_LoglevelChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Log(msg: string; source: string; lvl: TuLILogLevel);
  end;

var
  F_Debug: TF_Debug;

implementation

uses tUltimateLI;

{$R *.dfm}

procedure TF_Debug.B_ClearLogClick(Sender: TObject);
begin
  Self.LV_Log.Clear();
end;

procedure TF_Debug.LV_LogCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  const loglevel: TuLILogLevel = TuLILogLevel(Item.Data);

  if (loglevel = tllErrors) then
    Self.LV_Log.Canvas.Brush.Color := $E0E0FF
  else if (loglevel = tllWarnings) then
    Self.LV_Log.Canvas.Brush.Color := $D0FFFF
  else if (Item.SubItems.Count >= 3) and (LeftStr(Item.SubItems.Strings[2], 3) = 'GET') then
    Self.LV_Log.Canvas.Brush.Color := $FFEEEE
  else if (Item.SubItems.Count >= 3) and (LeftStr(Item.SubItems.Strings[2], 3) = 'PUT') then
    Self.LV_Log.Canvas.Brush.Color := $EEFFEE;
end;

procedure TF_Debug.CB_LoglevelChange(Sender: TObject);
begin
  uLI.logLevel := TuLILogLevel(Self.CB_Loglevel.ItemIndex);
end;

procedure TF_Debug.CHB_KeepAliveClick(Sender: TObject);
begin
  uLI.ignoreKeepAliveLogging := not Self.CHB_KeepAlive.Checked;
end;

procedure TF_Debug.FormCreate(Sender: TObject);
begin
  Self.ActiveControl := Self.CB_Loglevel;
end;

procedure TF_Debug.FormDestroy(Sender: TObject);
begin
  F_Debug := nil;
end;

procedure TF_Debug.Log(msg: string; source: string; lvl: TuLILogLevel);
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
  LI.SubItems.Add(source);
  LI.SubItems.Add(ULILogLevelToStr(lvl));
  LI.SubItems.Add(msg);
  LI.Data := Pointer(lvl);
end;

end.// unit
