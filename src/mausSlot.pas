unit mausSlot;

{
  Tato unit implemenuje tridu TSlot. TSlot je slot na multiMaus (napr. 1..3).
}

interface

uses tHnaciVozidlo, SysUtils, Generics.Collections, Forms, ExtCtrls, Controls,
      StdCtrls, Graphics;

type
  TSlot = class
    private const
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

    public
      mausAddr : Integer;  // primarni klic
      mausId : Integer;
      HVs : TList<THV>;

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

       procedure ResetSlot();
       function GetHVIndex(lokoAddr:Word):Integer;

       procedure Show(form:TForm; activeIndex, activeCount:Integer);
       procedure HideGUI();

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

uses client, fSlots;

////////////////////////////////////////////////////////////////////////////////

constructor TSlot.Create(mausAddr:Integer);
begin
 inherited Create();
 Self.mausAddr := mausAddr;
 Self.mausId   := _MAUS_NULL;
 Self.HVs      := TList<THV>.Create();
 Self.ResetSlot();
 Self.CreateGUI();
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
 Result := true;
 for HV in Self.HVs do Result := Result AND HV.ukradeno;
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

procedure TSlot.ResetSlot();
begin
 Self.imagSmer := 0;
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
 if (Self.HVs.Count = 0) then Self.ResetSlot();
 Self.HVs.Add(HV);
end;

procedure TSlot.RemoveLoko(index:Integer);
begin
 Self.HVs[index].Free();
 Self.HVs.Delete(index);
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
   Caption := '?';
   Left := 30;
   Width := 40;
   Top := 10;
   Height := 20;
  end;

 Self.gui.L_Addrs := TLabel.Create(Self.gui.panel);
 with (Self.gui.L_Addrs) do
  begin
   Parent := Self.gui.panel;
   Left := 80;
   Font.Size := 16;
   Caption := '1234';
   Top := 7;
  end;

 Self.gui.L_Speed := TLabel.Create(Self.gui.panel);
 with (Self.gui.L_Speed) do
  begin
   Parent := Self.gui.panel;
   Left := 220;
   Top := 14;
   Caption := '80 km/h';
  end;

 Self.gui.CHB_Total := TCheckBox.Create(Self.gui.panel);
 with (Self.gui.CHB_Total) do
  begin

  end;

 Self.gui.B_Release := TButton.Create(Self.gui.panel);
 with (Self.gui.B_Release) do
  begin

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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.HideGUI();
begin
 Self.gui.panel.Visible := false;
end;

////////////////////////////////////////////////////////////////////////////////

end.
