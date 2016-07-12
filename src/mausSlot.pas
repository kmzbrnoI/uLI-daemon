unit mausSlot;

{
  Tato unit implemenuje tridu TSlot. TSlot je slot na multiMaus (napr. 1..3).
}

interface

uses tHnaciVozidlo;

type
  TSlot = class
    private const
      _MAUS_NULL = -1;

    private

       function fIsLoko():boolean;
       function fIsMaus():boolean;

    public
      mausAddr : Integer;  // primarni klic
      mausId : Integer;
      HV : THV;

       constructor Create(mausAddr:Integer);
       destructor Destroy(); override;

       procedure ReleaseLoko();

       property isLoko : boolean read fIsLoko;
       property isMaus : boolean read fIsMaus;

  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TSlot.Create(mausAddr:Integer);
begin
 inherited Create();
 Self.mausAddr := mausAddr;
 Self.mausId   := _MAUS_NULL;
 Self.HV       := nil;
end;

destructor TSlot.Destroy();
begin
 if (Assigned(Self.HV)) then Self.ReleaseLoko();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TSlot.fIsLoko():boolean;
begin
 Result := Assigned(Self.HV);
end;

function TSlot.fIsMaus():boolean;
begin
 Result := (Self.mausId <> _MAUS_NULL);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSlot.ReleaseLoko();
begin

end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

end.
