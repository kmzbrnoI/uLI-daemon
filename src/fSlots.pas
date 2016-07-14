unit fSlots;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_slots = class(TForm)
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_slots: TF_slots;

implementation

uses tUltimateLI;

{$R *.dfm}

procedure TF_slots.FormResize(Sender: TObject);
begin
 uLI.RepaintSlots(self);
end;

end.
