unit fMain;

{
  Unit hlavniho okynka, tady se resi predevsim interakce s GUI.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TF_Main = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Main: TF_Main;

implementation

uses Verze;

{$R *.dfm}

procedure TF_Main.FormShow(Sender: TObject);
begin
 Self.Caption := 'uLI-daemon v'+GetVersion(Application.ExeName)+' (build '+GetLastBuildDate()+')';
end;

end.//unit
