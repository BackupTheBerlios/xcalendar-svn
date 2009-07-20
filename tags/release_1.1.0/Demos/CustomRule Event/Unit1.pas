unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, xcalEvents, xcalGregorian, xcalClass, ComCtrls;

type
  TForm1 = class(TForm)
    XCalendarEvents1: TXCalendarEvents;
    Button1: TButton;
    GregorianCalendar1: TGregorianCalendar;
    ListBox1: TListBox;
    Edit1: TEdit;
    Edit2: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    procedure XCalendarEvents1CustomRule(Sender: TXCalEventItem;
      AYear: Word; var OccurenceDate: TDateTime; var Occurs: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  AAEaster;

{$R *.dfm}

procedure TForm1.XCalendarEvents1CustomRule(Sender: TXCalEventItem;
  AYear: Word; var OccurenceDate: TDateTime; var Occurs: Boolean);
var
  ED: TAAEasterDetails;
  
begin
  if Sender.Tag = 1 then
  begin
    ED := Easter_Calculate(AYear, True);
    if not GregorianCalendar1.TryEncodeDate(AYear, ED.Month, ED.Day, OccurenceDate) then
      Occurs := False; { Occurs is True by default }
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  dtStart, dtEnd: TDateTime;
  EOL: TXCalEventOccurenceList;
  i: Integer;

begin
  dtStart := GregorianCalendar1.StartOfAYear(UpDown1.Position);
  dtEnd := GregorianCalendar1.EndOfAYear(UpDown2.Position);

  EOL := TXCalEventOccurenceList.Create;
  try
    XCalendarEvents1.FindIntervalEvents(dtStart, dtEnd, EOL);
    EOL.SortByDate;

    ListBox1.Items.Clear;
    for i := 0 to EOL.Count - 1 do
      ListBox1.Items.Add(GregorianCalendar1.FormatDateTime('dddd mmmm d, yyyy', EOL[i].Date));

  finally
    EOL.Free;
  end;
end;

end.
