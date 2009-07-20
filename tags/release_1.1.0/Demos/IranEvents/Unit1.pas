unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  xcalGregorian, xcalHijri, xcalObservedHijri, xcalPersian, xcalClass,
  xcalEvents, StdCtrls, Grids, ComCtrls, ExtCtrls, IniFiles;

type
  TForm1 = class(TForm)
    IranEventsPersian: TXCalendarEvents;
    IranEventsMisc: TXCalendarEvents;
    IranEventsHijri: TXCalendarEvents;
    IranAggregateEvents: TXCalendarAggregateEvents;
    PersianCalendar: TPersianCalendar;
    GregorianCalendar: TGregorianCalendar;
    DrawGrid1: TDrawGrid;
    IranEventsGregorian: TXCalendarEvents;
    Panel1: TPanel;
    btnSave: TButton;
    btnSaveVacations: TButton;
    btnGetEvents: TButton;
    UpDown1: TUpDown;
    Edit1: TEdit;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ComboBox1: TComboBox;
    btnLoad: TButton;
    ObservedHijriCalendar: TObservedHijriCalendar;
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveVacationsClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetEventsClick(Sender: TObject);
    procedure ObservedHijriCalendarGetDaysInAMonth(const Year, Month: Word; const Leap: Boolean;
      var NDays: Word);
    procedure btnLoadClick(Sender: TObject);
    procedure ObservedHijriCalendarFindLeapYears(const Year: Word;
      var IsLeap: Boolean);
  private
    { Private declarations }
    EOL: TXCalEventOccurenceList;
    dtStart, dtEnd: TDateTime;
    Ini: TMemIniFile;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Path: string;
begin
  Path := ExtractFilePath(Application.ExeName);
  
  Ini := TMemIniFile.Create( Path + 'Hijri.ini' );

  { Thanks to Mohsen Rokkaei (mabna2001@yahoo.com) for providing much of the events }
  IranEventsPersian.LoadFromFile(Path + 'IranEventsPersian.xml');
  IranEventsHijri.LoadFromFile(Path + 'IranEventsHijri.xml');
  IranEventsGregorian.LoadFromFile(Path + 'IranEventsGregorian.xml');
  IranEventsMisc.LoadFromFile(Path + 'IranEventsMisc.xml');

  btnGetEventsClick(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  EOL.Free;
  Ini.Free;
end;

procedure TForm1.btnGetEventsClick(Sender: TObject);
var
  Y: Word;

begin
  Y := StrToInt(Edit1.Text);
  dtStart := PersianCalendar.StartOfAYear(Y);
  dtEnd := PersianCalendar.EndOfAYear(Y);

  if Assigned(EOL) then
    EOL.Free;
  EOL := TXCalEventOccurenceList.Create;
  IranAggregateEvents.FindIntervalEvents(dtStart, dtEnd, EOL);
  EOL.SortByDate;

  DrawGrid1.RowCount := Trunc(dtEnd) - Trunc(dtStart) + 2;
  DrawGrid1.ColWidths[0] := 110;
  DrawGrid1.ColWidths[1] := 110;
  DrawGrid1.ColWidths[2] := 110;
  DrawGrid1.ColWidths[3] := 800;
  DrawGrid1.Invalidate;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  dt: TDateTime;
  s: string;
  n: Integer;
  AddDash: Boolean;
  x: Integer;


  procedure AddText(Text: string; Vacation: Boolean = False);
  var
    R: TRect;
  begin
    if Vacation then
    begin
      DrawGrid1.Canvas.Font.Color := clRed;
      Text := Text + ' (ÊÚØíá)';
    end
    else
      DrawGrid1.Canvas.Font.Color := clWindowText;

    R := Rect;
    Dec(R.Right, x); // right to left
    DrawText(DrawGrid1.Canvas.Handle, PChar(Text), Length(Text), R,
      DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_RTLREADING or DT_NOPREFIX);

    x := x + DrawGrid1.Canvas.TextWidth(Text);
  end;

begin
  s := '';
  if Arow = 0 then
  begin
    case ACol of
      0: s := 'åÌÑí ÔãÓí';
      1: s := 'åÌÑí ÞãÑí';
      2: s := 'ãíáÇÏí';
      3: s := 'ãäÇÓÈÊ åÇ';
    end;

    DrawGrid1.Canvas.Font.Style := [fsBold];
    DrawText(DrawGrid1.Canvas.Handle, PChar(s), Length(s), Rect,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_RTLREADING or DT_NOPREFIX);
  end
  else
  begin
    dt := dtStart + (ARow - DrawGrid1.FixedRows);
    case ACol of
      0..2:
      begin
        case ACol of
          0: s := PersianCalendar.FormatDateTime('dddd d mmmm', dt);
          1: s := ObservedHijriCalendar.FormatDateTime('d mmmm yyyy', dt);
          2: s := GregorianCalendar.FormatDateTime('d mmmm yyyy', dt);
        end;

        Dec(Rect.Right, 2); // right to left
        DrawText(DrawGrid1.Canvas.Handle, PChar(s), Length(s), Rect,
          DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_RTLREADING or DT_NOPREFIX);
      end;

      3:
      begin
        if DayOfWeek(dt) = xcalFriday then
        begin
          DrawGrid1.Canvas.Brush.Color := $E0E0E0;
          DrawGrid1.Canvas.FillRect(Rect);
        end;

        AddDash := False;
        x := 2;
        n := EOL.IndexOf(dt);
        while n >= 0 do
        begin
          if AddDash then
            AddText(' - ');
          AddText(EOL[n].DisplayText, EOL[n].IsVacation);
          AddDash := True;
          n := EOL.IndexOf(dt, n + 1);
        end;
      end;
    end;
  end;
end;

procedure TForm1.ObservedHijriCalendarGetDaysInAMonth(const Year, Month: Word; const Leap: Boolean;
  var NDays: Word);
var
  Section: string;
  ReadDays: Integer;

begin
  { Hijri calendar in Iran is based on observations of the moon, but the
    predicted lengths of the Hijri months are announced before each Persian
    year for calendar event calculations. Thanks to Fatemeh Taherian,
    (http://radcom.ir) for providing the ini file containing observed month
    lengths. }
  Section := IntToStr(Year);
  if Ini.SectionExists(Section) then
  begin
    ReadDays := Ini.ReadInteger(Section, IntToStr(Month), 0);
    if ReadDays <> 0 then
      NDays := ReadDays;
  end;
end;

procedure TForm1.ObservedHijriCalendarFindLeapYears(const Year: Word;
  var IsLeap: Boolean);
begin
  if Ini.ValueExists('leap', IntToStr(Year)) then
    IsLeap := True
  else
    IsLeap := False;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  if ComboBox1.ItemIndex < 0 then Exit;
  if SaveDialog1.Execute then
    case ComboBox1.ItemIndex of
      0: IranEventsPersian.SaveToFile(SaveDialog1.FileName);
      1: IranEventsHijri.SaveToFile(SaveDialog1.FileName);
      2: IranEventsGregorian.SaveToFile(SaveDialog1.FileName);
      3: IranEventsMisc.SaveToFile(SaveDialog1.FileName);
    end;
end;

procedure TForm1.btnSaveVacationsClick(Sender: TObject);
var
  xe: TXCalendarEvents;
  i: Integer;
begin
  if SaveDialog1.Execute then
  begin
    xe := TXCalendarEvents.Create(Self);
    try
      with IranEventsPersian.Events do
        for i := 0 to Count - 1 do
          if Items[i].IsVacation then
            xe.Events.Add.Assign(Items[i]);

      with IranEventsHijri.Events do
        for i := 0 to Count - 1 do
          if Items[i].IsVacation then
            xe.Events.Add.Assign(Items[i]);

      with IranEventsGregorian.Events do
        for i := 0 to Count - 1 do
          if Items[i].IsVacation then
            xe.Events.Add.Assign(Items[i]);

      with IranEventsMisc.Events do
        for i := 0 to Count - 1 do
          if Items[i].IsVacation then
            xe.Events.Add.Assign(Items[i]);

      xe.SaveToFile(SaveDialog1.FileName);
    finally
      xe.Free;
    end;
  end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var
  xe: TXCalendarEvents;
begin
  if OpenDialog1.Execute then
  begin
    xe := TXCalendarEvents.Create(Self);
    try
      xe.LoadFromFile(OpenDialog1.FileName);
      EOL.Clear;
      xe.FindIntervalEvents(dtStart, dtEnd, EOL);
      EOL.SortByDate;
      DrawGrid1.Invalidate;
    finally
      xe.Free;
    end;
  end;
end;

end.
