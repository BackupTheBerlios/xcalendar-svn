unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, Grids, xcalClass, xcalAstroPersian, xcalPersian,
  ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    AstroPersianCalendar1: TAstroPersianCalendar;
    PersianCalendar1: TPersianCalendar;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    GroupBox1: TGroupBox;
    edtDegrees: TEdit;
    edtMinutes: TEdit;
    edtSeconds: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Row: Integer;
  y: Word;
  pl, apl: Boolean;
  DiffCount: Integer;

    function BoolToStr(const b: Boolean): string;
    begin
      Result := '';
      if b then
        Result := 'X';
    end;

begin
  AstroPersianCalendar1.Longitude :=
    StrToInt(edtDegrees.Text) +
    StrToInt(edtMinutes.Text) / 60 +
    StrToFloat(edtSeconds.Text) / (60 * 60);

  StringGrid1.RowCount := 2;

  StringGrid1.Cells[0, 0] := 'Year';
  StringGrid1.Cells[1, 0] := 'Persian Leap';
  StringGrid1.Cells[2, 0] := 'AstroPersian Leap';
  StringGrid1.Cells[0, 1] := '';
  StringGrid1.Cells[1, 2] := '';
  StringGrid1.Cells[2, 3] := '';

  Row := 1;
  DiffCount := 0;
  for y := UpDown1.Position to UpDown2.Position do
  begin
    pl := PersianCalendar1.IsLeapYear(y);
    apl := AstroPersianCalendar1.IsLeapYear(y);

    if pl <> apl then
    begin
      Inc(DiffCount);

      if StringGrid1.RowCount < Row + 1 then
        StringGrid1.RowCount := Row + 1;

      StringGrid1.Cells[0, Row] := IntToStr(y);
      StringGrid1.Cells[1, Row] := BoolToStr(pl);
      StringGrid1.Cells[2, Row] := BoolToStr(apl);
      Inc(Row);
    end;
  end;

  if DiffCount = 0 then
    ShowMessage('No difference found')
  else    
    ShowMessage(Format('%d differences found', [DiffCount]));
end;

end.
