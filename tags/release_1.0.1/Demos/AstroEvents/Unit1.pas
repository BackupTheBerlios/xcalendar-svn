unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, xcalEvents, xcalAstroEvents;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    XCalendarAstroEvents1: TXCalendarAstroEvents;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    edtTimeZone: TEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
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
  dtStart, dtEnd: TDateTime;
  EOL: TXCalEventOccurenceList;
  i: Integer;

begin
  XCalendarAstroEvents1.TimeZone := StrToFloat(edtTimeZone.Text);

  dtStart := EncodeDate(UpDown1.Position, 1, 1);
  dtEnd := EncodeDate(UpDown1.Position, 12, 31);

  EOL := TXCalEventOccurenceList.Create;
  try
    XCalendarAstroEvents1.FindIntervalEvents(dtStart, dtEnd, EOL);
    EOL.SortByDate;

    ListBox1.Items.Clear;
    for i := 0 to EOL.Count - 1 do
      ListBox1.Items.Add(DateToStr(EOL[i].Date) + ': ' + EOL[i].DisplayText);

  finally
    EOL.Free;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  XCalendarAstroEvents1.Equinoxes.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  XCalendarAstroEvents1.MoonPhases.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  XCalendarAstroEvents1.Eclipses.Enabled := (Sender as TCheckBox).Checked;
end;

end.
