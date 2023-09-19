unit mausSlot;

{
  Tato unit implemenuje tridu TSlot. TSlot je slot na multiMaus (napr. 1..3).
}

interface

uses tHnaciVozidlo, SysUtils, Generics.Collections, Forms, ExtCtrls, Controls,
  StdCtrls, Graphics, IdContext;

type
  TMomRelease = record
    f: Integer;
    shutdownTime: TDateTime;
  end;

  TSlot = class
  public const
    _MAUS_NULL = -1;
    _MOM_KEEP_ON_MS = 750;

  private

    imagSmer: Integer;
    q_mom_release: TQueue<TMomRelease>; // fronta momentary funkci k vypnuti
    T_Mom_Release: TTimer;

    function fIsLoko(): boolean;
    function fIsMaus(): boolean;

    function GetSmer(): Integer;
    function GetRychlostStupne(): Word;
    function GetRychlostKmph(): Word;
    function GetFunkce(): TFunkce;
    function GetFunkceType(): TFunkceType;
    function GetUkradeno(): boolean;
    function GetTotal(): boolean;

    procedure CreateGUI();
    procedure UpdateLokString();

    procedure OnBReleaseClick(Sender: TObject);
    procedure ONBTakeClick(Sender: TObject);
    procedure OnCHBTotalClick(Sender: TObject);

    procedure T_Mom_ReleaseTimer(Sender: TObject);
    procedure MomRelease(mr: TMomRelease);
    function CreateMomRelease(f: Integer; shutdownTime: TDateTime)
      : TMomRelease; overload;
    function CreateMomRelease(f: Integer): TMomRelease; overload;

  public
    mausAddr: Integer; // primarni klic
    mausId: Integer; // XpressNet adresa ovladace
    HVs: TObjectList<THV>;
    Sender: TIdContext;
    updating: boolean;
    mausFunkce: TFunkce;
    // aktualni funkce zobrazene na mysi (pozor: momentary funkce!)

    gui: record
      panel: TPanel;
      L_slotId: TLabel;
      P_status: TPanel;
      L_Addrs: TLabel;
      L_Speed: TLabel;
      CHB_Total: TCheckBox;
      B_Release: TButton;
      B_Take: TButton;
    end;

    constructor Create(mausAddr: Integer);
    destructor Destroy(); override;

    procedure ReleaseLoko();
    procedure AddLoko(HV: THV);
    procedure RemoveLoko(index: Integer);
    procedure STOPloko();
    procedure SetRychlostSmer(stupne: Word; smer: Integer);
    procedure SetFunctions(start, fin: Integer; new: TFunkce);

    procedure HardResetSlot();
    function GetHVIndex(lokoAddr: Word): Integer;

    procedure Show(form: TForm; activeIndex, activeCount: Integer);
    procedure HideGUI();
    procedure UpdateGUI();

    property isLoko: boolean read fIsLoko; // zda slot ma alespon nejake HV
    property isMaus: boolean read fIsMaus; // zda existuje ovladac pro dany slot

    property smer: Integer read GetSmer;
    property rychlost_stupne: Word read GetRychlostStupne;
    property rychlost_kmph: Word read GetRychlostKmph;
    property funkce: TFunkce read GetFunkce;
    property funkceType: TFunkceType read GetFunkceType;
    property ukradeno: boolean read GetUkradeno;
    property total: boolean read GetTotal;

  end;

implementation

uses client, fSlots, server;

/// /////////////////////////////////////////////////////////////////////////////

constructor TSlot.Create(mausAddr: Integer);
begin
  inherited Create();
  Self.mausAddr := mausAddr;
  Self.mausId := _MAUS_NULL;
  Self.HVs := TObjectList<THV>.Create();
  Self.imagSmer := 0;
  Self.CreateGUI();
  Self.Sender := nil;
  Self.updating := false;
  Self.q_mom_release := TQueue<TMomRelease>.Create();

  Self.T_Mom_Release := TTimer.Create(nil);
  Self.T_Mom_Release.OnTimer := Self.T_Mom_ReleaseTimer;
  Self.T_Mom_Release.Interval := 200;
  Self.T_Mom_Release.Enabled := false; // enabled with first loco
end;

destructor TSlot.Destroy();
begin
  Self.T_Mom_Release.Enabled := false;

  if (Self.HVs.Count > 0) then
    Self.ReleaseLoko();
  Self.HVs.Free();
  Self.q_mom_release.Free();
  Self.T_Mom_Release.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TSlot.fIsLoko(): boolean;
begin
  Result := (Self.HVs.Count > 0);
end;

function TSlot.fIsMaus(): boolean;
begin
  Result := (Self.mausId <> _MAUS_NULL);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.ReleaseLoko();
var
  HV: THV;
begin
  if (Self.HVs.Count > 0) then
  begin
    Self.gui.P_status.Color := clAqua;
    Self.gui.P_status.Caption := '...';
    Self.gui.P_status.Hint :=
      'Odeslán požadavek na odhlášení lokomotiv, čekám na odpověď...';
  end;

  for HV in Self.HVs do
    TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';RELEASE');
end;

/// /////////////////////////////////////////////////////////////////////////////

function TSlot.GetSmer(): Integer;
begin
  if (Self.HVs.Count = 0) then
    Result := 0
  else if (Self.HVs.Count = 1) then
    Result := Self.HVs[0].smer
  else
    Result := Self.imagSmer;
end;

function TSlot.GetRychlostStupne(): Word;
begin
  if (Self.HVs.Count = 0) then
    Result := 0
  else
    Result := Self.HVs[0].rychlost_stupne;
end;

function TSlot.GetRychlostKmph(): Word;
begin
  if (Self.HVs.Count = 0) then
    Result := 0
  else
    Result := Self.HVs[0].rychlost_kmph;
end;

function TSlot.GetFunkce(): TFunkce;
var
  f: TFunkce;
  i: Integer;
  HV: THV;
begin
  if (Self.HVs.Count = 0) then
  begin
    for i := 0 to _MAX_FUNC do
      f[i] := false;
    Result := f
  end
  else if (Self.HVs.Count = 1) then
    Result := Self.HVs[0].funkce
  else
  begin
    for i := 0 to _MAX_FUNC do
      f[i] := true;
    for HV in Self.HVs do
      for i := 0 to _MAX_FUNC do
        f[i] := f[i] AND HV.funkce[i];
    Result := f;
  end;
end;

function TSlot.GetFunkceType(): TFunkceType;
var
  f: TFunkceType;
  i: Integer;
  HV: THV;
begin

  if (Self.HVs.Count = 0) then
  begin
    for i := 0 to _MAX_FUNC do
      f[i] := permanent;
    Result := f
  end
  else if (Self.HVs.Count = 1) then
    Result := Self.HVs[0].funcType
  else
  begin
    for i := 0 to _MAX_FUNC do
      f[i] := permanent;
    for HV in Self.HVs do
      for i := 0 to _MAX_FUNC do
        if HV.funcType[i] = momentary then
          f[i] := momentary;
    Result := f;
  end;
end;

function TSlot.GetUkradeno(): boolean;
begin
  Result := false;
  for var HV in Self.HVs do
    Result := Result OR HV.ukradeno;
end;

function TSlot.GetTotal(): boolean;
var
  HV: THV;
begin
  Result := true;
  for HV in Self.HVs do
    Result := Result AND HV.total;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.SetRychlostSmer(stupne: Word; smer: Integer);
begin
  if (Self.HVs.Count > 1) then
    Self.imagSmer := smer
  else if (Self.HVs.Count = 1) then
    Self.HVs[0].smer := smer;

  for var HV in Self.HVs do
  begin
    HV.rychlost_stupne := stupne;
    TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';SPD-S;' +
      IntToStr(HV.rychlost_stupne) + ';' + IntToStr(HV.smer XOR imagSmer));
  end;
end;

procedure TSlot.STOPloko();
begin
  for var HV in Self.HVs do
  begin
    HV.rychlost_stupne := 0;
    TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';STOP');
  end;
end;

procedure TSlot.SetFunctions(start, fin: Integer; new: TFunkce);
var
  momTurnOff: TFunkce;
begin
  for var i := 0 to _MAX_FUNC do
    momTurnOff[i] := ((i >= start) and (i <= fin));
  // set temporary true everywhere

  for var HV in Self.HVs do
  begin
    for var i := start to fin do
    begin
      if (new[i] <> Self.mausFunkce[i]) then
      begin
        if (HV.funcType[i] = THVFuncType.momentary) then
        begin
          if (HV.funkce[i]) then
            momTurnOff[i] := false;
          HV.funkce[i] := (not HV.funkce[i]);
        end
        else
        begin
          momTurnOff[i] := false;
          HV.funkce[i] := new[i];
        end;
      end
      else
        momTurnOff[i] := false;
    end;
    TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';F;' + IntToStr(start) +
      '-' + IntToStr(fin) + ';' + HV.SerializeFunctions(start, fin));
  end;

  for var i := start to fin do
    Self.mausFunkce[i] := new[i];

  for var i := 0 to _MAX_FUNC do
    if (momTurnOff[i]) then
      Self.q_mom_release.Enqueue(Self.CreateMomRelease(i));
  // enqueue for release
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.HardResetSlot();
begin
  Self.mausId := _MAUS_NULL;
  Self.HVs.Clear();

  Self.gui.L_Speed.Caption := '-';
  Self.gui.CHB_Total.Checked := false;
  Self.gui.CHB_Total.Enabled := false;
  Self.gui.B_Release.Enabled := false;
  Self.gui.L_Addrs.Caption := '-';

  Self.gui.P_status.Color := clSilver;
  Self.gui.P_status.Caption := '-';
  Self.gui.P_status.Hint := '';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TSlot.GetHVIndex(lokoAddr: Word): Integer;
begin
  Result := -1;
  for var i := 0 to Self.HVs.Count - 1 do
    if (Self.HVs[i].Adresa = lokoAddr) then
      Exit(i);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.AddLoko(HV: THV);
begin
  try
    Self.updating := true;

    if (Self.HVs.Count = 0) then
    begin
      Self.imagSmer := 0;
      Self.T_Mom_Release.Enabled := true;
    end;
    Self.HVs.Add(HV);
    Self.UpdateLokString();

    if (Self.HVs.Count >= 1) then
    begin
      Self.gui.L_Speed.Caption := IntToStr(Self.HVs[0].rychlost_kmph) + ' km/h';
      Self.gui.CHB_Total.Enabled := true;
      Self.gui.CHB_Total.Checked := Self.total;
      Self.gui.B_Release.Enabled := true;
      Self.gui.P_status.Caption := 'OK';
      Self.gui.P_status.Hint := '';
      Self.gui.P_status.Color := clLime;
    end;

    if (Self.HVs.Count = 1) then
      TCPServer.BroadcastSlots();

    if (Assigned(Self.Sender)) then
    begin
      TCPServer.SendLn(Self.Sender, 'LOKO;ok');
      Self.Sender := nil;
    end;
  finally
    Self.updating := false;
  end;
end;

procedure TSlot.RemoveLoko(index: Integer);
begin
  try
    Self.updating := true;
    Self.HVs.Delete(index);

    Self.UpdateLokString();
    if (Self.HVs.Count = 0) then
    begin
      Self.q_mom_release.Clear();
      Self.T_Mom_Release.Enabled := false;

      Self.gui.L_Speed.Caption := '-';
      Self.gui.CHB_Total.Checked := false;
      Self.gui.CHB_Total.Enabled := false;
      Self.gui.B_Release.Enabled := false;
      Self.gui.L_Addrs.Caption := '-';

      Self.gui.P_status.Color := clSilver;
      Self.gui.P_status.Caption := '-';
      Self.gui.P_status.Hint := '';
    end;
  finally
    Self.updating := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
/// ///////////////////////       G  U  I        ////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.CreateGUI();
begin
  Self.gui.panel := TPanel.Create(nil);
  with (Self.gui.panel) do
  begin
    Left := 5;
    Top := 5;
    Height := 40;
  end;

  Self.gui.L_slotId := TLabel.Create(Self.gui.panel);
  with (Self.gui.L_slotId) do
  begin
    Parent := Self.gui.panel;
    Left := 1;
    Caption := IntToStr(Self.mausAddr);
    Font.Size := 24;
  end;

  Self.gui.P_status := TPanel.Create(Self.gui.panel);
  with (Self.gui.P_status) do
  begin
    Parent := Self.gui.panel;
    BevelOuter := bvLowered;
    ParentBackground := false;
    Color := clSilver;
    Caption := '-';
    Left := 30;
    Width := 40;
    Top := 10;
    Height := 20;
    ShowHint := true;
  end;

  Self.gui.B_Take := TButton.Create(Self.gui.panel);
  with (Self.gui.B_Take) do
  begin
    Parent := Self.gui.panel;
    Left := 80;
    Caption := 'Převzít';
    Enabled := false;
    Top := 5;
    Width := 50;
    Height := 30;
    OnClick := Self.ONBTakeClick;
  end;

  Self.gui.L_Addrs := TLabel.Create(Self.gui.panel);
  with (Self.gui.L_Addrs) do
  begin
    Parent := Self.gui.panel;
    Left := 80;
    Font.Size := 16;
    Caption := '-';
    Top := 7;
    AutoSize := false;
  end;

  Self.gui.L_Speed := TLabel.Create(Self.gui.panel);
  with (Self.gui.L_Speed) do
  begin
    Parent := Self.gui.panel;
    Left := 210;
    Top := 14;
    Caption := '-';
    AutoSize := false;
    Width := 50;
  end;

  Self.gui.CHB_Total := TCheckBox.Create(Self.gui.panel);
  with (Self.gui.CHB_Total) do
  begin
    Parent := Self.gui.panel;
    Left := 300;
    Caption := 'Ruční řízení';
    Enabled := false;
    Top := 11;
    OnClick := Self.OnCHBTotalClick;
    Width := 75;
  end;

  Self.gui.B_Release := TButton.Create(Self.gui.panel);
  with (Self.gui.B_Release) do
  begin
    Parent := Self.gui.panel;
    Left := 380;
    Caption := 'Uvolnit';
    Enabled := false;
    Top := 5;
    Width := 50;
    Height := 30;
    OnClick := Self.OnBReleaseClick;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.Show(form: TForm; activeIndex, activeCount: Integer);
begin
  Self.gui.panel.Parent := form;
  with (Self.gui.panel) do
  begin
    Top := ((form.Height * activeIndex) div activeCount) +
      (((form.Height div activeCount) - Height) div 2);
    Width := form.Width - 10;
    Visible := true;
  end;

  with (Self.gui) do
  begin
    B_Release.Left := panel.ClientWidth - B_Release.Width - 10;
    CHB_Total.Left := B_Release.Left - CHB_Total.Width - 10;
    L_Speed.Left := CHB_Total.Left - L_Speed.Width - 10;
    B_Take.Visible := Self.ukradeno;
    B_Take.Enabled := Self.ukradeno;
    if (Self.ukradeno) then
      L_Addrs.Left := B_Take.Left + B_Take.Width + 10
    else
      L_Addrs.Left := P_status.Left + P_status.Width + 10;
    L_Addrs.Width := L_Speed.Left - L_Addrs.Left - 10;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.HideGUI();
begin
  Self.gui.panel.Visible := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

// Aktualizace textu popisujici lokomotivy.
procedure TSlot.UpdateLokString();
begin
  if (Self.HVs.Count = 0) then
    Self.gui.L_Addrs.Caption := '-'
  else
  begin
    Self.gui.L_Addrs.Caption := '';
    for var i := 0 to Self.HVs.Count - 2 do
      Self.gui.L_Addrs.Caption := Self.gui.L_Addrs.Caption + Self.HVs[i].Nazev +
        ' (' + IntToStr(Self.HVs[i].Adresa) + '), ';
    Self.gui.L_Addrs.Caption := Self.gui.L_Addrs.Caption +
      Self.HVs[Self.HVs.Count - 1].Nazev + ' (' +
      IntToStr(Self.HVs[Self.HVs.Count - 1].Adresa) + ')';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Eventy GUI.

// Uvolnit lokomotivu ze slotu.
procedure TSlot.OnBReleaseClick(Sender: TObject);
begin
  Self.ReleaseLoko();
end;

// Prevzit / odhlasit lokomotivu z rucniho rizeni.
procedure TSlot.OnCHBTotalClick(Sender: TObject);
begin
  if (Self.updating) then
    Exit();

  Self.gui.P_status.Color := clAqua;
  Self.gui.P_status.Caption := '...';
  Self.gui.P_status.Hint :=
    'Odeslán požadavek na totální řízení, čekám na odpověď...';

  for var HV in Self.HVs do
    TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';TOTAL;' +
      IntToStr(Integer(TCheckBox(Sender).Checked)));
end;

// Prevzit ukradenou lokomotivu.
procedure TSlot.ONBTakeClick(Sender: TObject);
begin
  Self.gui.B_Take.Enabled := false;
  Self.gui.P_status.Color := clAqua;
  Self.gui.P_status.Caption := '...';
  Self.gui.P_status.Hint :=
    'Odeslán požadavek na totální řízení, čekám na odpověď...';

  for var HV in Self.HVs do
    if (HV.ukradeno) then
      TCPClient.LokoPlease(HV.Adresa, '');
end;

/// /////////////////////////////////////////////////////////////////////////////

// Aktualizace GUI dle aktualniho stavu programu.
procedure TSlot.UpdateGUI();
begin
  try
    Self.updating := true;
    Self.gui.CHB_Total.Checked := Self.total;
    if (Self.ukradeno) then
    begin
      Self.gui.P_status.Color := clYellow;
      Self.gui.P_status.Caption := 'ukrad.';
      Self.gui.P_status.Hint := 'Lokomotiva ukradena';
      Self.gui.L_Speed.Caption := '? km/h';
      Self.gui.CHB_Total.Enabled := false;
    end
    else if (Self.HVs.Count = 0) then
      Self.gui.P_status.Color := clSilver
    else
    begin
      Self.gui.P_status.Color := clLime;
      Self.gui.CHB_Total.Enabled := true;
      Self.gui.L_Speed.Caption := IntToStr(Self.rychlost_kmph) + ' km/h';
    end;

    with (Self.gui) do
    begin
      B_Take.Visible := Self.ukradeno;
      B_Take.Enabled := Self.ukradeno;
      if (Self.ukradeno) then
        L_Addrs.Left := B_Take.Left + B_Take.Width + 10
      else
        L_Addrs.Left := P_status.Left + P_status.Width + 10;
      L_Addrs.Width := L_Speed.Left - L_Addrs.Left - 10;
    end;

  finally
    Self.updating := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSlot.T_Mom_ReleaseTimer(Sender: TObject);
begin
  if (Self.q_mom_release.Count > 0) then
    if (Self.q_mom_release.Peek().shutdownTime <= Now) then
      Self.MomRelease(Self.q_mom_release.Dequeue());
end;

procedure TSlot.MomRelease(mr: TMomRelease);
begin
  for var HV in Self.HVs do
  begin
    if (HV.funkce[mr.f]) then
    begin
      HV.funkce[mr.f] := false;
      TCPClient.SendLn('-;LOK;' + IntToStr(HV.Adresa) + ';F;' +
        IntToStr(mr.f) + ';0');
    end;
  end;
end;

function TSlot.CreateMomRelease(f: Integer; shutdownTime: TDateTime)
  : TMomRelease;
begin
  Result.f := f;
  Result.shutdownTime := shutdownTime;
end;

function TSlot.CreateMomRelease(f: Integer): TMomRelease;
begin
  Result := Self.CreateMomRelease(f, Now + EncodeTime(0, 0,
    _MOM_KEEP_ON_MS div 1000, _MOM_KEEP_ON_MS mod 1000));
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
