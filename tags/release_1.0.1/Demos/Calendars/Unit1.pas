unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, xcalClass, StdCtrls, xcalHijri, xcalPersian, xcalGregorian;

type
  TForm1 = class(TForm)
    GregorianCalendar1: TGregorianCalendar;
    PersianCalendar1: TPersianCalendar;
    HijriCalendar1: THijriCalendar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    ListBox1: TListBox;
    edtDate: TEdit;
    btnUpdateDate: TButton;
    edtFormat: TEdit;
    cbWeekStartDay: TComboBox;
    cbWeekRule: TComboBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edtTDateTime: TEdit;
    edtJulianDay: TEdit;
    edtDayOfWeek: TEdit;
    edtYear: TEdit;
    edtMonth: TEdit;
    edtDay: TEdit;
    edtLeap: TEdit;
    edtStartOfTheYear: TEdit;
    edtWeekOfTheYear: TEdit;
    edtStartOfTheMonth: TEdit;
    edtWeekOfTheMonth: TEdit;
    edtDaySpan1: TEdit;
    edtDaySpan2: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    edtLastWed: TEdit;
    edtNearestFri: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnUpdateDateClick(Sender: TObject);
    procedure edtFormatChange(Sender: TObject);
    procedure cbWeekStartDayChange(Sender: TObject);
    procedure cbWeekRuleChange(Sender: TObject);
  private
    { Private declarations }
    FXCalendar: TXCalendar;
    FDate: TDateTime;
    procedure Update;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  dt: TDateTime;
begin
  ListBox1.Items.Clear;
  for I := 0 to ComponentCount - 1 do
  begin
    if (Components[I] is TXCalendar) then
      ListBox1.Items.AddObject(Components[I].Name, Components[I]);
  end;

  FDate := Date;
  ListBox1.ItemIndex := 0;
  ListBox1Click(nil);
end;

procedure TForm1.Update;
var
  Y, M, D, W, WY, WM: Word;
begin
  FXCalendar.FormatSettings.ShortDateFormat := edtFormat.Text;

  edtDate.Text := FXCalendar.DateToStr(FDate);
  
  edtTDateTime.Text := FloatToStr(FDate);
  edtJulianDay.Text := FloatToStr(DateTimeToJulianDay(FDate));

  FXCalendar.DecodeDate(FDate, Y, M, D);
  edtYear.Text := IntToStr(Y);
  edtMonth.Text := IntToStr(M);
  edtDay.Text := IntToStr(D);
  edtDayOfWeek.Text := FXCalendar.FormatDateTime('dddd', FDate);
  if FXCalendar.IsLeapYear(Y) then
    edtLeap.Text := 'Leap'
  else
    edtLeap.Text := 'Common';

  edtStartOfTheYear.Text := FXCalendar.FormatDateTime(FXCalendar.FormatSettings.LongDateFormat,
    FXCalendar.StartOfAYear(Y));
  W := FXCalendar.WeekOfTheYear(FDate, WY);
  edtWeekOfTheYear.Text := Format('Week %d of Year %d', [W, WY]);

  edtStartOfTheMonth.Text := FXCalendar.FormatDateTime(FXCalendar.FormatSettings.LongDateFormat,
    FXCalendar.StartOfAMonth(Y, M));
  W := FXCalendar.WeekOfTheMonth(FDate, WY, WM);
  edtWeekOfTheMonth.Text := Format('Week %d of Month %d of Year %d', [W, WM, WY]);
  
  edtDaySpan1.Text := IntToStr(FXCalendar.DaysBetween(FDate, FXCalendar.EndOfAMonth(Y, M)));
  edtDaySpan2.Text := IntToStr(FXCalendar.DaysBetween(FDate, FXCalendar.EndOfAYear(Y)));

  edtLastWed.Text := FXCalendar.FormatDateTime(FXCalendar.FormatSettings.LongDateFormat,
    FXCalendar.EncodeDayOfWeekInMonth(Y, FXCalendar.MonthsInAYear(Y), -1, xcalWednesday));
  edtNearestFri.Text := FXCalendar.FormatDateTime(FXCalendar.FormatSettings.LongDateFormat,
    FXCalendar.EncodeNearestDayOfWeekToADate(FDate, xcalFriday));
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  FXCalendar := TXCalendar(ListBox1.Items.Objects[ListBox1.ItemIndex]);

  cbWeekStartDay.Items.Assign(FXCalendar.FormatSettings.LongDayNames);
  if FXCalendar.WeekStartDay - 1 < cbWeekStartDay.Items.Count then
    cbWeekStartDay.ItemIndex := FXCalendar.WeekStartDay - 1;

  cbWeekRule.ItemIndex := Ord(FXCalendar.WeekRule);

  Update;
end;

procedure TForm1.btnUpdateDateClick(Sender: TObject);
begin
  FDate := FXCalendar.StrToDate(edtDate.Text);
  Update;
end;

procedure TForm1.edtFormatChange(Sender: TObject);
begin
  Update;
end;

procedure TForm1.cbWeekStartDayChange(Sender: TObject);
begin
  FXCalendar.WeekStartDay := cbWeekStartDay.ItemIndex + 1;
  Update;
end;

procedure TForm1.cbWeekRuleChange(Sender: TObject);
begin
  FXCalendar.WeekRule := TXCalWeekRule(cbWeekRule.ItemIndex);
  Update; 
end;

end.
