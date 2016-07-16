unit mausSlot;

{
  Tato unit implemenuje tridu TSlot. TSlot je slot na multiMaus (napr. 1..3).
}

interface

uses tHnaciVozidlo, SysUtils, Generics.Collections, Forms, ExtCtrls, Controls,
      StdCtrls, Graphics, IdContext;

type
  TSlot = class
    public const
      _MAUS_NULL = -1;

    private

      imagSmer : Integer;

       function fIsLoko():boolean;
       function fIsMaus():boolean;

       function GetSmer():Integer;
       function GetRychlostStupne():Word;
       function GetRychlostKmph():Word;
       function GetFunkce():TFunkce;
       function GetUkradeno():boolean;
       function GetTotal():boolean;

       procedure CreateGUI();
       procedure UpdateLokString();

       procedure OnBReleaseClick(Sender:TObject);
       procedure OnCHBTotalClick(Sender:TObject);

    public
      mausAddr : Integer;  // primarni klic
      mausId : Integer;
      HVs : TList<THV>;
      sender : TIdContext;

      gui : record
        panel : TPanel;
        L_slotId : TLabel;
        P_status : TPanel;
        L_Addrs : TLabel;
        L_Speed : TLabel;
        CHB_Total : TCheckBox;
        B_Release : TButton;
      end;

       constructor Create(mausAddr:Integer);
       destructor Destroy(); override;

       procedure ReleaseLoko();
       procedure AddLoko(HV:THV);
       procedure RemoveLoko(index:Integer);
       procedure STOPloko();
       procedure SetRychlostSmer(stupne:Word; smer:Integer);
       procedure SetFunctions(start, fin:Integer; new:TFunkce);

       procedure HardResetSlot();
       function GetHVIndex(lokoAddr:Word):Integer;

       procedure Show(form:TForm; activeIndex, activeCount:Integer);
       procedure HideGUI();
       procedure UpdateGUI();

       property isLoko : boolean read fIsLoko;
       property isMaus : boolean read fIsMaus;

       property smer : Integer read GetSmer;
       property rychlost_stupne : Word read GetRychlostStupne;
       property rychlost_kmph : Word read GetRychlostKmph;
       property funkce : TFunkce read GetFunkce;
       property ukradeno : boolean read GetUkradeno;
       property total : boolean read GetTotal;

  end;

implementation

uses client, fSlots, server;

////////////////////////////////////////////////////////////////////////////////

constructor TSlot.Create(mausAddr:Integer);
begin
 inherited Create();
 Self.mausAddr := mausAddr;
 Self.mausId   := _MAUS_NULL;
 Self.HVs      := TList<THV>.Create();
 Self.imagSmer := 0;
 Self.CreateGUI();
 Self.sender   := nil;
end;

destructor TSlot.Destroy();
var i:Integer;
begin
 if (Self.HVs.Count > 0) then
  begin
   Self.ReleaseLoko();
   for i := 0 to Self.HVs.Count-1 do Self.HVs[i].Free();
  end;
 Self.HVs.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TSlot.fIsLoko():boolean;
begin
 Result := (Self.HVs.Count > 0);
end;

function TSlot.fIsMaus():boolean;
begin
 Result := (Self.mausId <> _MAUS_NULL);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.ReleaseLoko();
var HV:THV;
begin
 if (Self.HVs.Count > 0) then
  begin
   Self.gui.P_status.Color   := clAqua;
   Self.gui.P_status.Caption := '...';
   Self.gui.P_status.Hint    := 'Odeslán požadavek na odhlášení lokomotiv, èekám na odpovìï...';
  end;

 for HV in Self.HVs do
   TCPClient.SendLn('-;LOK;'+IntToStr(HV.Adresa)+';RELEASE');
end;

////////////////////////////////////////////////////////////////////////////////

function TSlot.GetSmer():Integer;
begin
 if (Self.HVs.Count = 0) then
   Result := 0
 else if (Self.HVs.Count = 1) then
   Result := Self.HVs[0].smer
 else
   Result := Self.imagSmer;
end;

function TSlot.GetRychlostStupne():Word;
begin
 if (Self.HVs.Count = 0) then
   Result := 0
 else
   Result := Self.HVs[0].rychlost_stupne;
end;

function TSlot.GetRychlostKmph():Word;
begin
 if (Self.HVs.Count = 0) then
   Result := 0
 else
   Result := Self.HVs[0].rychlost_kmph;
end;

function TSlot.GetFunkce():TFunkce;
var f:TFunkce;
    i:Integer;
    HV:THV;
begin
 if (Self.HVs.Count = 0) then begin
   for i := 0 to _MAX_FUNC do f[i] := false;
   Result := f
 end else if (Self.HVs.Count = 1) then
   Result := Self.HVs[0].funkce
 else begin
   for i := 0 to _MAX_FUNC do f[i] := true;
   for HV in Self.HVs do
     for i := 0 to _MAX_FUNC do f[i] := f[i] AND HV.funkce[i];
   Result := f;
 end;
end;

function TSlot.GetUkradeno():boolean;
var HV:THV;
begin
 Result := false;
 for HV in Self.HVs do Result := Result OR HV.ukradeno;
end;

function TSlot.GetTotal():boolean;
var HV:THV;
begin
 Result := true;
 for HV in Self.HVs do Result := Result AND HV.total;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.SetRychlostSmer(stupne:Word; smer:Integer);
var HV:THV;
begin
 if (Self.HVs.Count > 1) then Self.imagSmer := smer
 else if (Self.HVs.Count = 1) then Self.HVs[0].smer := smer;

 for HV in Self.HVs do
  begin
   HV.rychlost_stupne := stupne;
   TCPClient.SendLn('-;LOK;'+IntToStr(HV.Adresa)+';SPD-S;'+
     IntToStr(HV.rychlost_stupne)+';'+IntToStr(HV.smer XOR imagSmer));
  end;
end;

procedure TSlot.STOPloko();
var HV:THV;
begin
 for HV in Self.HVs do
  begin
   HV.rychlost_stupne := 0;
   TCPClient.SendLn('-;LOK;'+IntToStr(HV.Adresa)+';STOP');
  end;
end;

procedure TSlot.SetFunctions(start, fin:Integer; new:TFunkce);
var HV:THV;
    i:Integer;
begin
 for HV in Self.HVs do
  begin
   for i := start to fin do HV.funkce[i] := new[i];
   TCPClient.SendLn('-;LOK;'+IntToStr(HV.Adresa)+';F;'+IntToStr(start)+'-'+IntToStr(fin)+';'+
      HV.SerializeFunctions(start, fin));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.HardResetSlot();
var HV:THV;
begin
 Self.mausId := _MAUS_NULL;
 for HV in Self.HVs do HV.Free();
 Self.HVs.Clear();

 Self.gui.L_Speed.Caption := '-';
 Self.gui.CHB_Total.Checked := false;
 Self.gui.CHB_Total.Enabled := false;
 Self.gui.B_Release.Enabled := false;
 Self.gui.L_Addrs.Caption   := '-';

 Self.gui.P_status.Color   := clSilver;
 Self.gui.P_status.Caption := '-';
 Self.gui.P_status.Hint    := '';
end;

////////////////////////////////////////////////////////////////////////////////

function TSlot.GetHVIndex(lokoAddr:Word):Integer;
var i:Integer;
begin
 Result := -1;
 for i := 0 to Self.HVs.Count-1 do
   if (Self.HVs[i].Adresa = lokoAddr) then Exit(i);                                                 
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.AddLoko(HV:THV);
begin
 if (Self.HVs.Count = 0) then Self.imagSmer := 0;
 Self.HVs.Add(HV);
 Self.UpdateLokString();

 if (Self.HVs.Count = 1) then
  begin
   Self.gui.L_Speed.Caption := IntToStr(Self.HVs[0].rychlost_kmph) + ' km/h';
   Self.gui.CHB_Total.Enabled := true;
   Self.gui.CHB_Total.Checked := Self.total;
   Self.gui.B_Release.Enabled := true;
   Self.gui.P_status.Caption := 'OK';
   Self.gui.P_status.Hint    := '';
   Self.gui.P_status.Color   := clLime;
  end;

 if (Assigned(Self.sender)) then
  begin
   TCPServer.SendLn(Self.sender, 'LOKO;ok');
   Self.sender := nil;
  end;
end;

procedure TSlot.RemoveLoko(index:Integer);
begin
 Self.HVs[index].Free();
 Self.HVs.Delete(index);

 Self.UpdateLokString();
 if (Self.HVs.Count = 0) then
  begin
   Self.gui.L_Speed.Caption := '-';
   Self.gui.CHB_Total.Checked := false;
   Self.gui.CHB_Total.Enabled := false;
   Self.gui.B_Release.Enabled := false;
   Self.gui.L_Addrs.Caption   := '-';

   Self.gui.P_status.Color   := clSilver;
   Self.gui.P_status.Caption := '-';
   Self.gui.P_status.Hint    := '';
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//////////////////////////       G  U  I        ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TSlot.CreateGUI();
begin
 Self.gui.panel := TPanel.Create(nil);
 with (Self.gui.panel) do
  begin
   Left := 5;
   Top := 5;
   Height := 40;
//   BevelOuter := bvNone;
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
   Left := 220;
   Top := 14;
   Caption := '-';
   AutoSize := false;
   Width := 40;
  end;

 Self.gui.CHB_Total := TCheckBox.Create(Self.gui.panel);
 with (Self.gui.CHB_Total) do
  begin
   Parent := Self.gui.panel;
   Left := 300;
   Caption := 'Ruèní øízení';
   Enabled := false;
   Top := 11;
   OnClick := Self.OnCHBTotalClick;
   Width := 70;
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

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.Show(form:TForm; activeIndex, activeCount:Integer);
begin
 Self.gui.panel.Parent := form;
 with (Self.gui.panel) do
  begin
   Top := ((form.Height*activeIndex) div activeCount) + (((form.Height div activeCount)-Height) div 2);
   Width := form.Width - 10;
   Visible := true;
  end;

 with (Self.gui) do
  begin
   B_Release.Left := panel.ClientWidth - B_Release.Width - 10;
   CHB_Total.Left := B_Release.Left - CHB_Total.Width - 10;
   L_Speed.Left   := CHB_Total.Left - L_Speed.Width - 10;
   L_Addrs.Width  := L_Speed.Left - P_status.Left - P_status.Width - 20;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.HideGUI();
begin
 Self.gui.panel.Visible := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.UpdateLokString();
var i:Integer;
begin
 if (Self.HVs.Count = 0) then
  Self.gui.L_Addrs.Caption := '-'
 else begin
  Self.gui.L_Addrs.Caption := '';
  for i := 0 to Self.HVs.Count-2 do
    Self.gui.L_Addrs.Caption := Self.gui.L_Addrs.Caption + Self.HVs[i].Nazev +
                                ' (' + IntToStr(Self.HVs[i].Adresa) + '), ';
  Self.gui.L_Addrs.Caption := Self.gui.L_Addrs.Caption + Self.HVs[Self.HVs.Count-1].Nazev +
                              ' (' + IntToStr(Self.HVs[Self.HVs.Count-1].Adresa) + ')';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.OnBReleaseClick(Sender:TObject);
begin
 Self.ReleaseLoko();
end;

procedure TSlot.OnCHBTotalClick(Sender:TObject);
var HV:THV;
begin
 Self.gui.P_status.Color   := clAqua;
 Self.gui.P_status.Caption := '...';
 Self.gui.P_status.Hint    := 'Odeslán požadavek na totální øízení, èekám na odpovìï...';

 for HV in Self.HVs do
   TCPClient.SendLn('-;LOK;'+IntToStr(HV.Adresa)+';TOTAL;'+IntToStr(Integer(TCheckBox(Sender).Checked)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.UpdateGUI();
begin
 Self.gui.CHB_Total.Checked := Self.total;
 if (Self.ukradeno) then begin
   Self.gui.P_status.Color    := clYellow;
   Self.gui.P_status.Caption  := 'ukradena';
   Self.gui.P_status.Hint     := 'Lokomotiva ukradena';
   Self.gui.L_Speed.Caption   := '? km/h';
   Self.gui.CHB_Total.Enabled := false;
 end else if (Self.HVs.Count = 0) then
   Self.gui.P_status.Color := clSilver
 else begin
   Self.gui.P_status.Color := clLime;
   Self.gui.CHB_Total.Enabled := true;
   Self.gui.L_Speed.Caption := IntToStr(Self.rychlost_kmph) + ' km/h';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

end.
