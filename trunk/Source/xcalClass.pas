{

    XCalendar - General Calendar Utilities for Delphi
    by adgteq

    Project website: http://xcalendar.sourceforge.net
    Author's e-mail: adgteq@yahoo.co.uk

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

}


unit xcalClass;

{$I xcalDefs.inc}

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  Libc,
{$ENDIF}
  SysConst,
  xcalXML;

  
const
  XCAL_MAJ_VER = 1; // Major version nr.
  XCAL_MIN_VER = 1; // Minor version nr.
  XCAL_REL_VER = 0; // Release nr.


const
  { Days of week are compliant with SysUtils.DayOfWeek }
  xcalSunday = 1;
  xcalMonday = 2;
  xcalTuesday = 3;
  xcalWednesday = 4;
  xcalThursday = 5;
  xcalFriday = 6;
  xcalSaturday = 7;

  { Used in RecodeDate, RecodeTime and RecodeDateTime for those datetime
    fields you want to leave alone }
  RecodeLeaveFieldAsIs = High(Word);


type

  TXCalStringList = class(TStringlist)
  private
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
  public
    property Strings[Index: Integer]: string read GetItem write SetItem; default;
  end;

  TXCalFormatSettings = class;

  TXCalWeekRule = (wrFullWeek, wrFourDayWeek, wrOneDayWeek);
  TXCalDateOrder = (doMDY, doDMY, doYMD);

  TXCalendar = class(TComponent)
  private
    FVersion: string;
    FFormatSettings: TXCalFormatSettings;
    FMinWeekDays: Word; { minimum in-range days that a week must have to be counted - automatically calculated from WeekRule }
    FWeekRule: TXCalWeekRule; { calendar rule for counting weeks }
    FWeekStartDay: Word; { calendar first day of week }

    FMinYear, FMaxYear: Word; { minimum and maximum allowed years in the calendar }

    { Approximate values used to calculate year and month spans }
    FApproxDaysPerMonth: Double;
    FApproxDaysPerYear: Double;

    FUpdateCount: Integer; { Used when batch updating the settings }

    procedure FormatSettingsChanged(Sender: TObject);

    procedure ScanBlanks(const S: string; var Pos: Integer);
    function ScanNumber(const S: string; var Pos: Integer; var Number: Word; var CharCount: Byte): Boolean;
    function ScanString(const S: string; var Pos: Integer; const Symbol: string): Boolean;
    function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
    function GetDateOrder(const DateFormat: string): TXCalDateOrder;
    procedure ScanToNumber(const S: string; var Pos: Integer);
// No eras yet    function GetEraYearOffset(const Name: string): Integer;
    function ScanDate(const S: string; var Pos: Integer; var Date: TDateTime): Boolean;
    function ScanTime(const S: string; var Pos: Integer; var Time: TDateTime): Boolean;


    { Error reporting }
    procedure ConvertError(ResString: PResStringRec);
    procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
    procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute,
      ASecond, AMilliSecond: Word; const ABaseDate: TDateTime = 0);
    procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
    procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
    procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
    procedure InvalidDayOfWeekInMonthError(const AYear, AMonth: Word;
      const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word);


    procedure SetWeekRule(const Value: TXCalWeekRule);
    procedure SetWeekStartDay(const Value: Word);
    procedure SetFormatSettings(FormatSettings: TXCalFormatSettings);
    procedure SetVersion(const Value: string);

  protected
    procedure Changed;

    { override these to save/load additional settings }
    procedure SaveSettingsToXMLItemChilds(XMLItem: TxcalXMLItem); virtual;
    procedure LoadSettingsFromXMLItemChilds(XMLItem: TxcalXMLItem); virtual;

    property MinWeekDays: Word read FMinWeekDays;
    property UpdateCount: Integer read FUpdateCount;

    property MinYear: Word read FMinYear write FMinYear;
    property MaxYear: Word read FMaxYear write FMaxYear;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdateSettings; { Call it before batch updates to the settings }
    procedure EndUpdateSettings; { Call it after batch updates to the settings }

    procedure SaveSettingsToStream(Stream: TStream);
    procedure LoadSettingsFromStream(Stream: TStream);
    procedure SaveSettingsToFile(const FileName: string);
    procedure LoadSettingsFromFile(const FileName: string);
    procedure LoadSettingsFromResourceName(Instance: THandle; const ResName: string);

    { SysUtils }
    function EncodeDate(Year, Month, Day: Word): TDateTime;
    function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; virtual; abstract; { override this }
    function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
    function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
    procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
    function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean; virtual; abstract; { override this }
    {$IFDEF LINUX}
    function InternalDecodeDate(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
    {$ENDIF}
    procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
    function DayOfWeek(const DateTime: TDateTime): Word; overload;
    function DayOfWeek(const AYear, AMonth, ADay: Word): Word; overload; { --- my addition --- }
    function CurrentYear: Word; virtual;
    function IsLeapYear(Year: Word): Boolean; virtual; abstract; { override this }
    function DateToStr(const DateTime: TDateTime): string;
    function TimeToStr(const DateTime: TDateTime): string;
    function DateTimeToStr(const DateTime: TDateTime): string;

    function StrToDate(const S: string): TDateTime;
    function StrToDateDef(const S: string; const Default: TDateTime): TDateTime;
    function TryStrToDate(const S: string; out Value: TDateTime): Boolean;
    function StrToTime(const S: string): TDateTime;
    function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime;
    function TryStrToTime(const S: string; out Value: TDateTime): Boolean;
    function StrToDateTime(const S: string): TDateTime;
    function StrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime;
    function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
    function FormatDateTime(const Format: string; DateTime: TDateTime): string;
    procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime);


    { DateUtils }
    { Differences from DateUtils: 1. DayOfWeek is like SysUtils; 2. We got a WeekRule }

    { Simple trimming functions }
    function DateOf(const AValue: TDateTime): TDateTime;
    function TimeOf(const AValue: TDateTime): TDateTime;
    { Misc functions }
    function IsInLeapYear(const AValue: TDateTime): Boolean;
    function IsPM(const AValue: TDateTime): Boolean;
    function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
    function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
    function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
      AMilliSecond: Word): Boolean;
    function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
    function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean; { Uses WeekRule }
    function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean; { Uses WeekRule }
    function WeeksInYear(const AValue: TDateTime): Word; { Uses WeekRule }
    function WeeksInAYear(const AYear: Word): Word; { Uses WeekRule }
    function DaysInYear(const AValue: TDateTime): Word;
    function DaysInAYear(const AYear: Word): Word; virtual; abstract; { override this }
    function DaysInMonth(const AValue: TDateTime): Word;
    function DaysInAMonth(const AYear, AMonth: Word): Word; virtual; abstract; { override this }
    function Today: TDateTime;
    function Yesterday: TDateTime;
    function Tomorrow: TDateTime;
    function IsToday(const AValue: TDateTime): Boolean;
    function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
    function MonthsInYear(const AValue: TDateTime): Word; { --- my addition --- }
    function MonthsInAYear(const AYear: Word): Word; virtual; { --- my addition --- } { override this for lunisolar calendars }
    function WeeksInMonth(const AValue: TDateTime): Word; { --- my addition --- }     { Uses WeekRule }
    function WeeksInAMonth(const AYear, AMonth: Word): Word; { --- my addition --- }  { Uses WeekRule }
    { Pick-a-field functions }
    function YearOf(const AValue: TDateTime): Word;
    function MonthOf(const AValue: TDateTime): Word;
    function WeekOf(const AValue: TDateTime): Word; { Uses WeekRule }
    function DayOf(const AValue: TDateTime): Word;
    function HourOf(const AValue: TDateTime): Word;
    function MinuteOf(const AValue: TDateTime): Word;
    function SecondOf(const AValue: TDateTime): Word;
    function MilliSecondOf(const AValue: TDateTime): Word;
    { Start/End functions }
    function StartOfTheYear(const AValue: TDateTime): TDateTime;
    function EndOfTheYear(const AValue: TDateTime): TDateTime;
    function StartOfAYear(const AYear: Word): TDateTime;
    function EndOfAYear(const AYear: Word): TDateTime;
    function StartOfTheMonth(const AValue: TDateTime): TDateTime;
    function EndOfTheMonth(const AValue: TDateTime): TDateTime;
    function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
    function EndOfAMonth(const AYear, AMonth: Word): TDateTime;
    function StartOfTheWeek(const AValue: TDateTime): TDateTime; { Uses WeekStartDay }
    function EndOfTheWeek(const AValue: TDateTime): TDateTime;   { Uses WeekStartDay }
    function StartOfAWeek(const AYear, AWeekOfYear: Word;        { Uses WeekStartDay }
      const ADayOfWeek: Word = 1): TDateTime;
    function EndOfAWeek(const AYear, AWeekOfYear: Word;          { Uses WeekStartDay }
      const ADayOfWeek: Word = 7): TDateTime;
    function StartOfTheDay(const AValue: TDateTime): TDateTime;
    function EndOfTheDay(const AValue: TDateTime): TDateTime;
    function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
    function StartOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;
    function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
    function EndOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;
    { This of that functions }
    function MonthOfTheYear(const AValue: TDateTime): Word;
    function WeekOfTheYear(const AValue: TDateTime): Word; overload;      { Uses WeekRule }
    function WeekOfTheYear(const AValue: TDateTime;                       { Uses WeekRule }
      var AYear: Word): Word; overload;
    function DayOfTheYear(const AValue: TDateTime): Word;
    function HourOfTheYear(const AValue: TDateTime): Word;
    function MinuteOfTheYear(const AValue: TDateTime): LongWord;
    function SecondOfTheYear(const AValue: TDateTime): LongWord;
    function MilliSecondOfTheYear(const AValue: TDateTime): Int64;
    function WeekOfTheMonth(const AValue: TDateTime): Word; overload;    { Uses WeekRule }
    function WeekOfTheMonth(const AValue: TDateTime; var AYear,          { Uses WeekRule }
      AMonth: Word): Word; overload;
    function DayOfTheMonth(const AValue: TDateTime): Word;
    function HourOfTheMonth(const AValue: TDateTime): Word;
    function MinuteOfTheMonth(const AValue: TDateTime): Word;
    function SecondOfTheMonth(const AValue: TDateTime): LongWord;
    function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;
    function HourOfTheWeek(const AValue: TDateTime): Word;                { Uses WeekRule }
    function MinuteOfTheWeek(const AValue: TDateTime): Word;              { Uses WeekRule }
    function SecondOfTheWeek(const AValue: TDateTime): LongWord;          { Uses WeekRule }
    function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;     { Uses WeekRule }
    function HourOfTheDay(const AValue: TDateTime): Word;
    function MinuteOfTheDay(const AValue: TDateTime): Word;
    function SecondOfTheDay(const AValue: TDateTime): LongWord;
    function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
    function MinuteOfTheHour(const AValue: TDateTime): Word;
    function SecondOfTheHour(const AValue: TDateTime): Word;
    function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;
    function SecondOfTheMinute(const AValue: TDateTime): Word;
    function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;
    function MilliSecondOfTheSecond(const AValue: TDateTime): Word;
    { Range checking functions }
    function WithinPastYears(const ANow, AThen: TDateTime;
      const AYears: Integer): Boolean;
    function WithinPastMonths(const ANow, AThen: TDateTime;
      const AMonths: Integer): Boolean;
    function WithinPastWeeks(const ANow, AThen: TDateTime;
      const AWeeks: Integer): Boolean;
    function WithinPastDays(const ANow, AThen: TDateTime;
      const ADays: Integer): Boolean;
    function WithinPastHours(const ANow, AThen: TDateTime;
      const AHours: Int64): Boolean;
    function WithinPastMinutes(const ANow, AThen: TDateTime;
      const AMinutes: Int64): Boolean;
    function WithinPastSeconds(const ANow, AThen: TDateTime;
      const ASeconds: Int64): Boolean;
    function WithinPastMilliSeconds(const ANow, AThen: TDateTime;
      const AMilliSeconds: Int64): Boolean;
    { Range query functions }
    function YearsBetween(const ANow, AThen: TDateTime): Integer;
    function MonthsBetween(const ANow, AThen: TDateTime): Integer;
    function WeeksBetween(const ANow, AThen: TDateTime): Integer;
    function DaysBetween(const ANow, AThen: TDateTime): Integer;
    function HoursBetween(const ANow, AThen: TDateTime): Int64;
    function MinutesBetween(const ANow, AThen: TDateTime): Int64;
    function SecondsBetween(const ANow, AThen: TDateTime): Int64;
    function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
    { Range spanning functions }
    { YearSpan and MonthSpan are approximates, not exact but pretty darn close }
    function YearSpan(const ANow, AThen: TDateTime): Double;
    function MonthSpan(const ANow, AThen: TDateTime): Double;
    function WeekSpan(const ANow, AThen: TDateTime): Double;
    function DaySpan(const ANow, AThen: TDateTime): Double;
    function HourSpan(const ANow, AThen: TDateTime): Double;
    function MinuteSpan(const ANow, AThen: TDateTime): Double;
    function SecondSpan(const ANow, AThen: TDateTime): Double;
    function MilliSecondSpan(const ANow, AThen: TDateTime): Double;
    { Increment/decrement datetime fields }
    function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer = 1): TDateTime;
    procedure IncAYear(var Year, Month, Day: Word; NumberOfYears: Integer = 1); { --- my addition --- }
    function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer = 1): TDateTime;
    procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
    function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer = 1): TDateTime;
    function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer = 1): TDateTime;
    function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime;
    function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64 = 1): TDateTime;
    function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64 = 1): TDateTime;
    function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64 = 1): TDateTime;
    { Unified encode/decode functions that deal with all datetime fields at once }
    function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
    procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
      AHour, AMinute, ASecond, AMilliSecond: Word);
    { Encode/decode functions that work with week of year and day of week }
    function EncodeDateWeek(const AYear, AWeekOfYear: Word;               { Uses WeekRule }
      const ADayOfWeek: Word = 1): TDateTime;
    procedure DecodeDateWeek(const AValue: TDateTime; out AYear,          { Uses WeekRule }
      AWeekOfYear, ADayOfWeek: Word);
    { Encode/decode functions that work with day of year }
    function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
    procedure DecodeDateDay(const AValue: TDateTime; out AYear, ADayOfYear: Word);
    { Encode/decode functions that work with week of month }
    function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth,      { Uses WeekRule }
      ADayOfWeek: Word): TDateTime;
    procedure DecodeDateMonthWeek(const AValue: TDateTime;               { Uses WeekRule }
      out AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
    { The following functions are similar to the above ones except these don't
      generated exceptions on failure, they return false instead }
    function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
      AMilliSecond: Word; out AValue: TDateTime): Boolean;
    function TryEncodeDateWeek(const AYear, AWeekOfYear: Word;            { Uses WeekRule }
      out AValue: TDateTime; const ADayOfWeek: Word = 1): Boolean;
    function TryEncodeDateDay(const AYear, ADayOfYear: Word;
      out AValue: TDateTime): Boolean;
    function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth,   { Uses WeekRule }
      ADayOfWeek: Word; var AValue: TDateTime): Boolean;
    { Recode functions for datetime fields }
    function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
    function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
    function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
    function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
    function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
    function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
    function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
    function RecodeDate(const AValue: TDateTime; const AYear, AMonth,
      ADay: Word): TDateTime;
    function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond,
      AMilliSecond: Word): TDateTime;
    function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay,
      AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
    { The following function is similar to the above one except it doesn't
      generated an exception on failure, it return false instead }
    function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay,
      AHour, AMinute, ASecond, AMilliSecond: Word; out AResult: TDateTime): Boolean;
    { Fuzzy comparison }
    function CompareDateTime(const A, B: TDateTime): Integer;
    function SameDateTime(const A, B: TDateTime): Boolean;
    function CompareDate(const A, B: TDateTime): Integer;
    function SameDate(const A, B: TDateTime): Boolean;
    function CompareTime(const A, B: TDateTime): Integer;
    function SameTime(const A, B: TDateTime): Boolean;
    { For a given date these functions tell you the which day of the week in the
      month.  If its a Thursday, they will tell you if its the first,
      second, etc Thursday of the month (or year). }
    function NthDayOfWeek(const AValue: TDateTime): Word;
    procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; out AYear, AMonth,
      ANthDayOfWeek, ADayOfWeek: Word);

    { --- my improvement --- }
    { If ANthDayOfWeek is negative, counting is started from the end of the month downwards. }
    { e.g. EncodeDayOfWeekInMonth(Y, M, -1, xcalSunday) returns the last Sunday of the month }
    function EncodeDayOfWeekInMonth(const AYear, AMonth: Word;
      const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
    function TryEncodeDayOfWeekInMonth(const AYear, AMonth: Word;
      const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word; out AValue: TDateTime): Boolean;

    { --- my additions --- }
    { If ANthDayOfWeek is positive, returns the Nth date having the day of week ADayOfWeek after or equal AFromDate;
      If ANthDayOfWeek is negative, returns the Nth date having the day of week ADayOfWeek before or equal AFromDate.
      (N = ANthDayOfWeek) }
    function EncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
      const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
    function TryEncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
      const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word; out AValue: TDateTime): Boolean;
    { Returns the nearest date to ADate having the day of week ADayOfWeek }
    function EncodeNearestDayOfWeekToADate(const ADate: TDateTime;
      const ADayOfWeek: Word): TDateTime;
    function TryEncodeNearestDayOfWeekToADate(const ADate: TDateTime;
      const ADayOfWeek: Word; out AValue: TDateTime): Boolean;

    { --- my additions --- }
    function OffsetToDayOfWeek(const AOffset: Word): Word; { returns the DayOfWeek value corresponding to the offset AOffset from the first day of week (WeekStartDay). Normal values for AOffset are 0..6 }
    function DayOfWeekToOffset(const ADayOfWeek: Word): Word; { returns the offset from the first day of week (WeekStartDay) to ADayOfWeek. Result value is in 0..6 }


    property ApproxDaysPerYear: Double read FApproxDaysPerYear write FApproxDaysPerYear;
    property ApproxDaysPerMonth: Double read FApproxDaysPerMonth write FApproxDaysPerMonth;
  published
    property Version: string read FVersion write SetVersion stored False;
    property FormatSettings: TXCalFormatSettings read FFormatSettings write SetFormatSettings;
    property WeekRule: TXCalWeekRule read FWeekRule write SetWeekRule default wrFullWeek;
    property WeekStartDay: Word read FWeekStartDay write SetWeekStartDay default xcalSunday;
  end;


  { TXCalendar class reference type }
  TXCalendarClass = class of TXCalendar;


  TXCalFormatSettings = class(TPersistent)
  private
    FDecimalSeparator: Char;
    FDateSeparator: Char;
    FTimeSeparator: Char;
    FShortDateFormat: String;
    FLongDateFormat: String;
    FTimeAMString: String;
    FTimePMString: String;
    FShortTimeFormat: String;
    FLongTimeFormat: String;
    FShortMonthNames: TXCalStringList;
    FLongMonthNames: TXCalStringList;
    FShortDayNames: TXCalStringList;
    FLongDayNames: TXCalStringList;
    FTwoDigitYearCenturyWindow: Word;

    FOwner: TXCalendar;
    FOnChange: TNotifyEvent;
    procedure Changed;
    procedure StringListsChanged(Sender: TObject);
    procedure SetDateSeparator(const Value: Char);
    procedure SetDecimalSeparator(const Value: Char);
    procedure SetLongDateFormat(const Value: String);
    procedure SetLongTimeFormat(const Value: String);
    procedure SetShortDateFormat(const Value: String);
    procedure SetShortTimeFormat(const Value: String);
    procedure SetTimeAMString(const Value: String);
    procedure SetTimePMString(const Value: String);
    procedure SetTimeSeparator(const Value: Char);
    procedure SetLongDayNames(const Value: TXCalStringList);
    procedure SetLongMonthNames(const Value: TXCalStringList);
    procedure SetShortDayNames(const Value: TXCalStringList);
    procedure SetShortMonthNames(const Value: TXCalStringList);
    procedure SetTwoDigitYearCenturyWindow(const Value: Word);

    function IsPropertyStored(Index: Integer): Boolean;
    function IsDecimalSeparatorStored: Boolean;
    function IsDateSeparatorStored: Boolean;
    function IsTimeSeparatorStored: Boolean;
    function IsShortDateFormatStored: Boolean;
    function IsLongDateFormatStored: Boolean;
    function IsTimeAMStringStored: Boolean;
    function IsTimePMStringStored: Boolean;
    function IsShortTimeFormatStored: Boolean;
    function IsLongTimeFormatStored: Boolean;
    function IsShortMonthNamesStored: Boolean;
    function IsLongMonthNamesStored: Boolean;
    function IsShortDayNamesStored: Boolean;
    function IsLongDayNamesStored: Boolean;
    function IsTwoDigitYearCenturyWindowStored: Boolean;
  public
    constructor Create(AOwner: TXCalendar);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromGlobalVariables;
    procedure CopyToGlobalVariables;
{$IFDEF Delphi7}
    procedure LoadFromRecord(const FormatSettings: TFormatSettings);
    procedure CopyToRecord(var FormatSettings: TFormatSettings);
{$ENDIF}

    property OnChange: TNotifyEvent read FOnChange write FOnChange; { property change notifier }
  published
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator stored IsDecimalSeparatorStored;
    property DateSeparator: Char read FDateSeparator write SetDateSeparator stored IsDateSeparatorStored;
    property TimeSeparator: Char read FTimeSeparator write SetTimeSeparator stored IsTimeSeparatorStored;
    property ShortDateFormat: String read FShortDateFormat write SetShortDateFormat stored IsShortDateFormatStored;
    property LongDateFormat: String read FLongDateFormat write SetLongDateFormat stored IsLongDateFormatStored;
    property TimeAMString: String read FTimeAMString write SetTimeAMString stored IsTimeAMStringStored;
    property TimePMString: String read FTimePMString write SetTimePMString stored IsTimePMStringStored;
    property ShortTimeFormat: String read FShortTimeFormat write SetShortTimeFormat stored IsShortTimeFormatStored;
    property LongTimeFormat: String read FLongTimeFormat write SetLongTimeFormat stored IsLongTimeFormatStored;
    property ShortMonthNames: TXCalStringList read FShortMonthNames write SetShortMonthNames stored IsShortMonthNamesStored;
    property LongMonthNames: TXCalStringList read FLongMonthNames write SetLongMonthNames stored IsLongMonthNamesStored;
    property ShortDayNames: TXCalStringList read FShortDayNames write SetShortDayNames stored IsShortDayNamesStored;
    property LongDayNames: TXCalStringList read FLongDayNames write SetLongDayNames stored IsLongDayNamesStored;
    property TwoDigitYearCenturyWindow: Word read FTwoDigitYearCenturyWindow write SetTwoDigitYearCenturyWindow stored IsTwoDigitYearCenturyWindowStored;
  end;



{ Helper Functions }

{ Just to avoid using the Math unit }
procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);

{ --- my additions --- }
{ Algebraic DateTime is a TDateTime value that can be used in additions or
  subtractions without respect to its sign.
  e.g. the TDateTime value for 12/29/1899 6:00AM is -1.25 but it's
  algebraic DateTime is -0.75 }
function DateTimeToAlgebraicDateTime(const AValue: TDateTime): Double;
function AlgebraicDateTimeToDateTime(const AValue: Double): TDateTime;

{ Julian and Modified Julian Day conversion support }
{ Be aware that not all Julian Days (or MJD) are encodable as a TDateTime }

function DateTimeToJulianDay(const AValue: TDateTime): Double;
function JulianDayToDateTime(const AValue: Double): TDateTime;
function TryJulianDayToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;

function DateTimeToModifiedJulianDay(const AValue: TDateTime): Double;
function ModifiedJulianDayToDateTime(const AValue: Double): TDateTime;
function TryModifiedJulianDayToDateTime(const AValue: Double;
  out ADateTime: TDateTime): Boolean;

{ Unix date conversion support }

function DateTimeToUnix(const AValue: TDateTime): Int64;
function UnixToDateTime(const AValue: Int64): TDateTime;





implementation

uses
{$IFNDEF Delphi6}
  xcalD5Utils,
{$ENDIF}
  xcalConsts;

const
  XCalendarSettingsXMLRootName: string = 'XCalendarSettings'; // XML Root Name


procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

function DateTimeToAlgebraicDateTime(const AValue: TDateTime): Double;
begin
  if AValue < 0 then
    Result := Int(AValue) + Abs(Frac(AValue))
  else
    Result := AValue;
end;

function AlgebraicDateTimeToDateTime(const AValue: Double): TDateTime;
var
  F: Double;
begin
  Result := AValue;
  F := Frac(Result);
  if F < 0 then
    Result := Int(Result) - 2 - F;
end;

const
  JDDelta: Double = 2415018.5;

function DateTimeToJulianDay(const AValue: TDateTime): Double;
begin
  Result := DateTimeToAlgebraicDateTime(AValue) + JDDelta;
end;

function JulianDayToDateTime(const AValue: Double): TDateTime;
begin
  if not TryJulianDayToDateTime(AValue, Result) then
    raise EConvertError.CreateFmt(SXCalInvalidJulianDay, [AValue]);
end;

{$IFNDEF Delphi7}
const
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }

function TryFloatToDateTime(const Value: Extended; out AResult: TDateTime): Boolean;
begin
  Result := not ((Value < MinDateTime) or (Value >= Int(MaxDateTime) + 1.0));
  if Result then
    AResult := Value;
end;
{$ENDIF}

function TryJulianDayToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;
begin
  Result := TryFloatToDateTime(AlgebraicDateTimeToDateTime(AValue - JDDelta), ADateTime);
end;

const
  CJDToMJDOffset: Double = 2400000.5;

function DateTimeToModifiedJulianDay(const AValue: TDateTime): Double;
begin
  Result := DateTimeToJulianDay(AValue) - CJDToMJDOffset;
end;

function ModifiedJulianDayToDateTime(const AValue: Double): TDateTime;
begin
  Result := JulianDayToDateTime(AValue + CJDToMJDOffset);
end;

function TryModifiedJulianDayToDateTime(const AValue: Double;
  out ADateTime: TDateTime): Boolean;
begin
  Result := TryJulianDayToDateTime(AValue + CJDToMJDOffset, ADateTime);
end;

{$IFNDEF Delphi7}
const
{ Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970) }
  UnixDateDelta = 25569;
{$ENDIF}

function DateTimeToUnix(const AValue: TDateTime): Int64;
begin
  Result := Round((DateTimeToAlgebraicDateTime(AValue) - UnixDateDelta) * SecsPerDay);
end;

function UnixToDateTime(const AValue: Int64): TDateTime;
begin
  Result := AlgebraicDateTimeToDateTime(AValue / SecsPerDay + UnixDateDelta);
end;

function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
var
  LNow, LThen: Double;
begin
  LNow := DateTimeToAlgebraicDateTime(ANow);
  LThen := DateTimeToAlgebraicDateTime(AThen);
  if LNow < LThen then
    Result := LThen - LNow
  else
    Result := LNow - LThen;
end;

{ TXCalStringList }

function TXCalStringList.GetItem(Index: Integer): string;
begin
  if (Index >= Count) or (Index < 0) then
    result := ''
  else
    result := inherited Strings[Index];
end;

procedure TXCalStringList.SetItem(Index: Integer; const Value: string);
begin
  inherited Strings[Index] := Value;
end;

{ TXCalendar }

constructor TXCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVersion := IntToStr(XCAL_MAJ_VER) + '.' +
              IntToStr(XCAL_MIN_VER) + '.' +
              IntToStr(XCAL_REL_VER);

  FFormatSettings := TXCalFormatSettings.Create(Self);
  FFormatSettings.OnChange := FormatSettingsChanged;

  FMinYear := 1;
  FMaxYear := 9999;
  FWeekRule := wrFullWeek;
  FMinWeekDays := 7;
  FWeekStartDay := xcalSunday;
  FApproxDaysPerMonth := 0;
  FApproxDaysPerYear := 0;
end;

destructor TXCalendar.Destroy;
begin
  FFormatSettings.Free;

  inherited Destroy;
end;

procedure TXCalendar.Changed;
begin
  //Removed
end;

procedure TXCalendar.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TXCalendar.ConvertError(ResString: PResStringRec);
begin
  raise EConvertError.CreateRes(ResString);
end;

procedure TXCalendar.ConvertErrorFmt(ResString: PResStringRec;
  const Args: array of const);
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

function TXCalendar.EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  if not TryEncodeDate(Year, Month, Day, Result) then
    ConvertError(@SDateEncodeError);
end;

function TXCalendar.EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  Result := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

function TXCalendar.TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
begin
{$IFDEF Delphi6}
  Result := SysUtils.TryEncodeTime(Hour, Min, Sec, MSec, Time);
{$ELSE}
  Result := False;
  try
    Time := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
    Result := True;
  except
    on EConvertError do;
  end;
{$ENDIF}
end;

procedure TXCalendar.ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function TXCalendar.ScanNumber(const S: string; var Pos: Integer;
  var Number: Word; var CharCount: Byte): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function TXCalendar.ScanString(const S: string; var Pos: Integer;
  const Symbol: string): Boolean;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ScanBlanks(S, Pos);
    if AnsiCompareText(Symbol, Copy(S, Pos, Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

function TXCalendar.ScanChar(const S: string; var Pos: Integer;
  Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

function TXCalendar.GetDateOrder(const DateFormat: string): TXCalDateOrder;
var
  I: Integer;
begin
  Result := doYMD;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
// No eras yet      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
  Result := doYMD;
end;

procedure TXCalendar.ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not (S[Pos] in ['0'..'9']) do
  begin
{$IFDEF Delphi6}
{$IFDEF Delphi12}
    if IsLeadChar(S[Pos]) then
{$ELSE}
    if S[Pos] in LeadBytes then
{$ENDIF}
      Pos := NextCharIndex(S, Pos)
    else
      Inc(Pos);
{$ELSE}
    if S[Pos] in LeadBytes then Inc(Pos);
    Inc(Pos);
{$ENDIF}
  end;
end;

(* // No eras yet
function TXCalendar.GetEraYearOffset(const Name: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EraNames) to High(EraNames) do
  begin
    if EraNames[I] = '' then Break;
    if AnsiStrPos(PChar(EraNames[I]), PChar(Name)) <> nil then
    begin
      Result := EraYearOffsets[I];
      Exit;
    end;
  end;
end;*)

function TXCalendar.ScanDate(const S: string; var Pos: Integer; var Date: TDateTime): Boolean;
var
  DateOrder: TXCalDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  CenturyBase: Integer;
//  EraName : string;
//  EraYearOffset: Integer;

(* // No eras yet
  function EraToYear(Year: Integer): Integer;
  begin
{$IFDEF MSWINDOWS}
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
{$ENDIF}
    Result := Year + EraYearOffset;
  end;*)

begin
  Y := 0;
  M := 0;
  D := 0;
  YearLen := 0;
  Result := False;
  DateOrder := GetDateOrder(FormatSettings.ShortDateFormat);
(* // No eras yet
  EraYearOffset := 0;
  if FormatSettings.ShortDateFormat[1] = 'g' then  // skip over prefix text
  begin
    ScanToNumber(S, Pos);
    EraName := Trim(Copy(S, 1, Pos-1));
    EraYearOffset := GetEraYearOffset(EraName);
  end
  else
    if AnsiPos('e', FormatSettings.ShortDateFormat) > 0 then
      EraYearOffset := EraYearOffsets[1];*)
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, FormatSettings.DateSeparator) and
    ScanNumber(S, Pos, N2, L2)) then Exit;
  if ScanChar(S, Pos, FormatSettings.DateSeparator) then
  begin
    if not ScanNumber(S, Pos, N3, L3) then Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      doYMD: begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
(* // No eras yet
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else*)
    if (YearLen <= 2) then
    begin
      CenturyBase := CurrentYear - FormatSettings.TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (FormatSettings.TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1; M := N2;
    end else
    begin
      M := N1; D := N2;
    end;
  end;
  ScanChar(S, Pos, FormatSettings.DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', FormatSettings.ShortDateFormat) <> 0) then
  begin     // ignore trailing text
    if FormatSettings.ShortTimeFormat[1] in ['0'..'9'] then  // stop at time digit
      ScanToNumber(S, Pos)
    else  // stop at time prefix
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do Inc(Pos);
        ScanBlanks(S, Pos);
      until (Pos > Length(S)) or
        (AnsiCompareText(FormatSettings.TimeAMString,
         Copy(S, Pos, Length(FormatSettings.TimeAMString))) = 0) or
        (AnsiCompareText(FormatSettings.TimePMString,
         Copy(S, Pos, Length(FormatSettings.TimePMString))) = 0);
  end;
  Result := TryEncodeDate(Y, M, D, Date);
end;

function TXCalendar.ScanTime(const S: string; var Pos: Integer; var Time: TDateTime): Boolean;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
begin
  Result := False;
  BaseHour := -1;
  if ScanString(S, Pos, FormatSettings.TimeAMString) or ScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ScanString(S, Pos, FormatSettings.TimePMString) or ScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ScanBlanks(S, Pos);
  if not ScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  Sec := 0;
  MSec := 0;
  if ScanChar(S, Pos, FormatSettings.TimeSeparator) then
  begin
    if not ScanNumber(S, Pos, Min, Junk) then Exit;
    if ScanChar(S, Pos, FormatSettings.TimeSeparator) then
    begin
      if not ScanNumber(S, Pos, Sec, Junk) then Exit;
      if ScanChar(S, Pos, FormatSettings.DecimalSeparator) then
        if not ScanNumber(S, Pos, MSec, Junk) then Exit;
    end;
  end;
  if BaseHour < 0 then
    if ScanString(S, Pos, FormatSettings.TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ScanString(S, Pos, FormatSettings.TimePMString) or ScanString(S, Pos, 'PM') then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ScanBlanks(S, Pos);
  Result := TryEncodeTime(Hour, Min, Sec, MSec, Time);
end;

procedure TXCalendar.DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
var
  Dummy: Word;
begin
  DecodeDateFully(DateTime, Year, Month, Day, Dummy);
end;

{$IFDEF LINUX}
function TXCalendar.InternalDecodeDate(const DateTime: TDateTime; var Year,
  Month, Day, DOW: Word): Boolean;
begin
  Result := DecodeDateFully(DateTime, Year, Month, Day, DOW);
  Dec(DOW);
end;
{$ENDIF}

procedure TXCalendar.DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
  SysUtils.DecodeTime(DateTime, Hour, Min, Sec, MSec);
end;

function TXCalendar.DayOfWeek(const DateTime: TDateTime): Word;
begin
  Result := DateTimeToTimeStamp(DateTime).Date mod 7 + 1;
end;

function TXCalendar.DayOfWeek(const AYear, AMonth, ADay: Word): Word;
begin
  Result := DayOfWeek(EncodeDate(AYear, AMonth, ADay));
end;

function TXCalendar.CurrentYear: Word;
{$IFDEF MSWINDOWS}
var
  SystemTime: TSystemTime;
  M, D: Word;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    DecodeDate(SysUtils.EncodeDate(wYear, wMonth, wDay), Result, M, D);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  T: TTime_T;
  UT: TUnixTime;
  M, D: Word;
begin
  __time(@T);
  localtime_r(@T, UT);
  Result := DecodeDate(SysUtils.EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday), Result, M, D);
end;
{$ENDIF}

function TXCalendar.DateToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, FormatSettings.ShortDateFormat, DateTime);
end;

function TXCalendar.TimeToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, FormatSettings.LongTimeFormat, DateTime);
end;

function TXCalendar.DateTimeToStr(const DateTime: TDateTime): string;
begin
  DateTimeToString(Result, '', DateTime);
end;

function TXCalendar.StrToDate(const S: string): TDateTime;
begin
  if not TryStrToDate(S, Result) then
    ConvertErrorFmt(@SInvalidDate, [S]);
end;

function TXCalendar.StrToDateDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  if not TryStrToDate(S, Result) then
    Result := Default;
end;

function TXCalendar.TryStrToDate(const S: string;
  out Value: TDateTime): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanDate(S, Pos, Value) and (Pos > Length(S));
end;

function TXCalendar.StrToTime(const S: string): TDateTime;
begin
  if not TryStrToTime(S, Result) then
    ConvertErrorFmt(@SInvalidTime, [S]);
end;

function TXCalendar.StrToTimeDef(const S: string; const Default: TDateTime): TDateTime;
begin
  if not TryStrToTime(S, Result) then
    Result := Default;
end;

function TXCalendar.TryStrToTime(const S: string; out Value: TDateTime): Boolean;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ScanTime(S, Pos, Value) and (Pos > Length(S));
end;

function TXCalendar.StrToDateTime(const S: string): TDateTime;
begin
  if not TryStrToDateTime(S, Result) then
    ConvertErrorFmt(@SInvalidDateTime, [S]);
end;

function TXCalendar.StrToDateTimeDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(S, Result) then
    Result := Default;
end;

function TXCalendar.TryStrToDateTime(const S: string;
  out Value: TDateTime): Boolean;
var
  Pos: Integer;
  Date, Time: TDateTime;
begin
  Result := True;
  Pos := 1;
  Time := 0;
  if not ScanDate(S, Pos, Date) or
     not ((Pos > Length(S)) or ScanTime(S, Pos, Time)) then

    // Try time only
    Result := TryStrToTime(S, Value)
  else
    if Date >= 0 then
      Value := Date + Time
    else
      Value := Date - Time;
end;

function TXCalendar.FormatDateTime(const Format: string;
  DateTime: TDateTime): string;
begin
  DateTimeToString(Result, Format, DateTime);
end;

procedure TXCalendar.DateTimeToString(var Result: string;
  const Format: string; DateTime: TDateTime);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := Length(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N * SizeOf(Char));
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, Length(NumBuf), Format,
      Length(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

(* // No eras yet
{$IFDEF MSWINDOWS}
    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, Length(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, Length(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;
{$ENDIF}

{$IFDEF LINUX}
    function FindEra(Date: Integer): Byte;
    var
      I : Byte;
    begin
      Result := 0;
      for I := 1 to EraCount do
      begin
        if (EraRanges[I].StartDate <= Date) and
          (EraRanges[I].EndDate >= Date) then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;

    function ConvertEraString(const Count: Integer) : String;
    var
      I : Byte;
    begin
      Result := '';
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        Result := EraNames[I];
    end;

    function ConvertYearString(const Count: Integer) : String;
    var
      I : Byte;
      S : string;
    begin
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        S := IntToStr(Year - EraYearOffsets[I])
      else
        S := IntToStr(Year);
      while Length(S) < Count do
        S := '0' + S;
      if Length(S) > Count then
        S := Copy(S, Length(S) - (Count - 1), Count);
      Result := S;
    end;
{$ENDIF}*)

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
{$IFNDEF Delphi6}
        Inc(Format);
{$ENDIF}
{$IFDEF Delphi12}
        if IsLeadChar(Starter) then
{$ELSE}
        if Starter in LeadBytes then
{$ENDIF}
        begin
{$IFDEF Delphi6}
          AppendChars(Format, StrCharLength(Format) div SizeOf(Char));
          Format := StrNextChar(Format);
{$ELSE}
          if Format^ = #0 then Break;
          Inc(Format);
{$ENDIF}
          LastToken := ' ';
          Continue;
        end;
{$IFDEF Delphi6}
        Format := StrNextChar(Format);
{$ENDIF}
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
(* // No eras yet
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;*)
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(FormatSettings.ShortMonthNames[Month - 1]);
              else
                AppendString(FormatSettings.LongMonthNames[Month - 1]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime) - 1]);
                4: AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime) - 1]);
                5: AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              else
                AppendFormat(Pointer(FormatSettings.LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
{$IFDEF Delphi12}
                if IsLeadChar(P^) then
{$ELSE}
                if P^ in LeadBytes then
{$ENDIF}
                begin
{$IFDEF Delphi6}
                  P := StrNextChar(P);
{$ELSE}
                  Inc(P);
                  if P^ = #0 then Break;
                  Inc(P);
{$ENDIF}
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(FormatSettings.ShortTimeFormat)) else
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(FormatSettings.TimeAMString) else
                  AppendString(FormatSettings.TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime) - 1]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime) - 1]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
              end;
            end;
          '/':
            if DateSeparator <> #0 then
              AppendChars(@FormatSettings.DateSeparator, 1);
          ':':
            if TimeSeparator <> #0 then
              AppendChars(@FormatSettings.TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
{$IFDEF Delphi12}
                if IsLeadChar(Format^) then
{$ELSE}
                if Format^ in LeadBytes then
{$ENDIF}
{$IFDEF Delphi6}
                  Format := StrNextChar(Format)
                else
{$ELSE}
                begin
                  Inc(Format);
                  if Format^ = #0 then Break;
                end;
{$ENDIF}
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  SetString(Result, Buffer, BufPos);
end;

function TXCalendar.IsInLeapYear(const AValue: TDateTime): Boolean;
begin
  Result := IsLeapYear(YearOf(AValue));
end;

function TXCalendar.IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
begin
  Result := (AYear >= FMinYear) and (AYear <= FMaxYear) and
            (AMonth >= 1) and (AMonth <= MonthsInAYear(AYear)) and
            (ADay >= 1) and (ADay <= DaysInAMonth(AYear, AMonth));
end;

function TXCalendar.IsValidDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  Result := IsValidDate(AYear, AMonth, ADay) and
            IsValidTime(AHour, AMinute, ASecond, AMilliSecond);
end;

function TXCalendar.IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
begin
  Result := (AYear >= FMinYear) and (AYear <= FMaxYear) and
            (ADayOfYear >= 1) and (ADayOfYear <= DaysInAYear(AYear));
end;

function TXCalendar.IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
begin
  Result := (AYear >= FMinYear) and (AYear <= FMaxYear) and
            (AWeekOfYear >= 1) and (AWeekOfYear <= WeeksInAYear(AYear)) and
            (ADayOfWeek >= xcalSunday) and (ADayOfWeek <= xcalSaturday);
end;

function TXCalendar.IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth,
  ADayOfWeek: Word): Boolean;
begin
  Result := (AYear >= FMinYear) and (AYear <= FMaxYear) and
            (AMonth >= 1) and (AMonth <= MonthsInAYear(AYear)) and
            (AWeekOfMonth >= 1) and (AWeekOfMonth <= WeeksInAMonth(AYear, AMonth)) and
            (ADayOfWeek >= xcalSunday) and (ADayOfWeek <= xcalSaturday);
end;

function TXCalendar.WeeksInYear(const AValue: TDateTime): Word;
begin
  Result := WeeksInAYear(YearOf(AValue));
end;

function TXCalendar.WeeksInAYear(const AYear: Word): Word;
var
  LDaysInYear: Word;
  LDaysInFirstWeek, LDaysInLastWeek: Word;
begin
  LDaysInYear := DaysInAYear(AYear);
  LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(EncodeDate(AYear, 1, 1)));
  if LDaysInYear - LDaysInFirstWeek < FMinWeekDays then { just in case! }
  begin
    Result := 0;
    if FMinWeekDays = 7 then
      Inc(Result);
    Exit;
  end;
  if LDaysInFirstWeek <> 7 then
  begin
    if LDaysInFirstWeek >= FMinWeekDays then
      { The incomplete first week should be counted }
      Inc(LDaysInYear, 7 - LDaysInFirstWeek)
    else
      { The incomplete first week should NOT be counted }
      Dec(LDaysInYear, LDaysInFirstWeek);
  end;
  DivMod(LDaysInYear, 7, Result, LDaysInLastWeek);
  { Check if the incomplete last week should be counted. In case of full week rule,
    we count the week spanning between two years as being the last of the first. }
  if (LDaysInLastWeek >= FMinWeekDays) or
     ((FMinWeekDays = 7) and (LDaysInLastWeek > 0)) then
    Inc(Result);
end;

function TXCalendar.DaysInYear(const AValue: TDateTime): Word;
begin
  Result := DaysInAYear(YearOf(AValue));
end;

function TXCalendar.DaysInMonth(const AValue: TDateTime): Word;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := DaysInAMonth(LYear, LMonth);
end;

function TXCalendar.YearOf(const AValue: TDateTime): Word;
var
  LMonth, LDay: Word;
begin
  DecodeDate(AValue, Result, LMonth, LDay);
end;

function TXCalendar.MonthOf(const AValue: TDateTime): Word;
var
  LYear, LDay: Word;
begin
  DecodeDate(AValue, LYear, Result, LDay);
end;

function TXCalendar.WeekOf(const AValue: TDateTime): Word;
begin
  Result := WeekOfTheYear(AValue);
end;

function TXCalendar.DayOf(const AValue: TDateTime): Word;
var
  LYear, LMonth: Word;
begin
  DecodeDate(AValue, LYear, LMonth, Result);
end;

function TXCalendar.StartOfTheYear(const AValue: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(AValue), 1, 1);
end;

function TXCalendar.EndOfTheYear(const AValue: TDateTime): TDateTime;
var
  Y, M: Word;
begin
  Y := YearOf(AValue);
  M := MonthsInAYear(Y);
  Result := EndOfTheDay(EncodeDate(Y, M, DaysInAMonth(Y, M)));
end;

function TXCalendar.StartOfAYear(const AYear: Word): TDateTime;
begin
  Result := EncodeDate(AYear, 1, 1);
end;

function TXCalendar.EndOfAYear(const AYear: Word): TDateTime;
begin
  Result := EndOfTheDay(EncodeDateDay(AYear, DaysInAYear(AYear)));
end;

function TXCalendar.StartOfTheMonth(const AValue: TDateTime): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := EncodeDate(LYear, LMonth, 1);
end;

function TXCalendar.EndOfTheMonth(const AValue: TDateTime): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := EndOfTheDay(EncodeDate(LYear, LMonth, DaysInAMonth(LYear, LMonth)));
end;

function TXCalendar.StartOfAMonth(const AYear, AMonth: Word): TDateTime;
begin
  Result := EncodeDate(AYear, AMonth, 1);
end;

function TXCalendar.EndOfAMonth(const AYear, AMonth: Word): TDateTime;
begin
  Result := EndOfTheDay(EncodeDate(AYear, AMonth, DaysInAMonth(AYear, AMonth)));
end;

function TXCalendar.StartOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  Result := Trunc(AValue) - DayOfWeekToOffset(DayOfWeek(AValue));
end;

function TXCalendar.EndOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheDay(StartOfTheWeek(AValue) + 6);
end;

function TXCalendar.StartOfAWeek(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  Result := EncodeDateWeek(AYear, AWeekOfYear, ADayOfWeek);
end;

function TXCalendar.EndOfAWeek(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  Result := EndOfTheDay(EncodeDateWeek(AYear, AWeekOfYear, ADayOfWeek));
end;

function TXCalendar.StartOfADay(const AYear, AMonth,
  ADay: Word): TDateTime;
begin
  Result := StartOfAMonth(AYear, AMonth) + ADay - 1;
end;

function TXCalendar.StartOfADay(const AYear, ADayOfYear: Word): TDateTime;
begin
  Result := EncodeDateDay(AYear, ADayOfYear);
end;

function TXCalendar.EndOfADay(const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result := EndOfAMonth(AYear, AMonth) + ADay - 1;
end;

function TXCalendar.EndOfADay(const AYear, ADayOfYear: Word): TDateTime;
begin
  Result := EndOfTheDay(EncodeDateDay(AYear, ADayOfYear));
end;

function TXCalendar.MonthOfTheYear(const AValue: TDateTime): Word;
begin
  Result := MonthOf(AValue);
end;

function TXCalendar.WeekOfTheYear(const AValue: TDateTime): Word;
var
  LYear, LDOW: Word;
begin
  DecodeDateWeek(AValue, LYear, Result, LDOW);
end;

function TXCalendar.WeekOfTheYear(const AValue: TDateTime;
  var AYear: Word): Word;
var
  LDOW: Word;
begin
  DecodeDateWeek(AValue, AYear, Result, LDOW);
end;

function TXCalendar.DayOfTheYear(const AValue: TDateTime): Word;
begin
  Result := Trunc(AValue - StartOfTheYear(AValue)) + 1;
end;

{$IFNDEF Delphi7}
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
{$ENDIF}

function TXCalendar.HourOfTheYear(const AValue: TDateTime): Word;
begin
  Result := HourOf(AValue) + (DayOfTheYear(AValue) - 1) * HoursPerDay;
end;

function TXCalendar.MinuteOfTheYear(const AValue: TDateTime): LongWord;
begin
  Result := MinuteOf(AValue) + HourOfTheYear(AValue) * MinsPerHour;
end;

function TXCalendar.SecondOfTheYear(const AValue: TDateTime): LongWord;
begin
  Result := SecondOf(AValue) + MinuteOfTheYear(AValue) * SecsPerMin;
end;

function TXCalendar.MilliSecondOfTheYear(const AValue: TDateTime): Int64;
begin
  Result := MilliSecondOf(AValue) + SecondOfTheYear(AValue) * MSecsPerSec;
end;

function TXCalendar.WeekOfTheMonth(const AValue: TDateTime): Word;
var
  LYear, LMonth, LDayOfWeek: Word;
begin
  DecodeDateMonthWeek(AValue, LYear, LMonth, Result, LDayOfWeek);
end;

function TXCalendar.WeekOfTheMonth(const AValue: TDateTime; var AYear,
  AMonth: Word): Word;
var
  LDayOfWeek: Word;
begin
  DecodeDateMonthWeek(AValue, AYear, AMonth, Result, LDayOfWeek);
end;

function TXCalendar.DayOfTheMonth(const AValue: TDateTime): Word;
begin
  Result := DayOf(AValue);
end;

function TXCalendar.HourOfTheMonth(const AValue: TDateTime): Word;
begin
  Result := HourOf(AValue) + (DayOfTheMonth(AValue) - 1) * HoursPerDay;
end;

function TXCalendar.MinuteOfTheMonth(const AValue: TDateTime): Word;
begin
  Result := MinuteOf(AValue) + HourOfTheMonth(AValue) * MinsPerHour;
end;

function TXCalendar.SecondOfTheMonth(const AValue: TDateTime): LongWord;
begin
  Result := SecondOf(AValue) + MinuteOfTheMonth(AValue) * SecsPerMin;
end;

function TXCalendar.MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;
begin
  Result := MilliSecondOf(AValue) + SecondOfTheMonth(AValue) * MSecsPerSec;
end;

function TXCalendar.HourOfTheWeek(const AValue: TDateTime): Word;
begin
  Result := HourOf(AValue) + DayOfWeekToOffset(DayOfWeek(AValue)) * HoursPerDay;
end;

function TXCalendar.MinuteOfTheWeek(const AValue: TDateTime): Word;
begin
  Result := MinuteOf(AValue) + HourOfTheWeek(AValue) * MinsPerHour;
end;

function TXCalendar.SecondOfTheWeek(const AValue: TDateTime): LongWord;
begin
  Result := SecondOf(AValue) + MinuteOfTheWeek(AValue) * SecsPerMin;
end;

function TXCalendar.MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;
begin
  Result := MilliSecondOf(AValue) + SecondOfTheWeek(AValue) * MSecsPerSec;
end;

function TXCalendar.HourOfTheDay(const AValue: TDateTime): Word;
begin
  Result := HourOf(AValue);
end;

function TXCalendar.MinuteOfTheDay(const AValue: TDateTime): Word;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LMinutes + LHours * MinsPerHour;
end;

function TXCalendar.SecondOfTheDay(const AValue: TDateTime): LongWord;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LSeconds + (LMinutes + LHours * MinsPerHour) * SecsPerMin;
end;

function TXCalendar.MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LMilliSeconds + (LSeconds + (LMinutes + LHours * MinsPerHour) * SecsPerMin) * MSecsPerSec;
end;

function TXCalendar.WithinPastYears(const ANow, AThen: TDateTime;
  const AYears: Integer): Boolean;
begin
  Result := YearsBetween(ANow, AThen) <= AYears;
end;

function TXCalendar.WithinPastMonths(const ANow, AThen: TDateTime;
  const AMonths: Integer): Boolean;
begin
  Result := MonthsBetween(ANow, AThen) <= AMonths;
end;

function TXCalendar.WithinPastWeeks(const ANow, AThen: TDateTime;
  const AWeeks: Integer): Boolean;
begin
  Result := WeeksBetween(ANow, AThen) <= AWeeks;
end;

function TXCalendar.WithinPastDays(const ANow, AThen: TDateTime;
  const ADays: Integer): Boolean;
begin
  Result := DaysBetween(ANow, AThen) <= ADays;
end;

function TXCalendar.YearsBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result := Trunc(YearSpan(ANow, AThen));
end;

function TXCalendar.MonthsBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result := Trunc(MonthSpan(ANow, AThen));
end;

function TXCalendar.WeeksBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result := Trunc(WeekSpan(ANow, AThen));
end;

function TXCalendar.DaysBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result := Trunc(DaySpan(ANow, AThen));
end;

function TXCalendar.HoursBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(HourSpan(ANow, AThen));
end;

function TXCalendar.MinutesBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(MinuteSpan(ANow, AThen));
end;

function TXCalendar.SecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(SecondSpan(ANow, AThen));
end;

function TXCalendar.MilliSecondsBetween(const ANow,
  AThen: TDateTime): Int64;
begin
  Result := Trunc(MilliSecondSpan(ANow, AThen));
end;

function TXCalendar.YearSpan(const ANow, AThen: TDateTime): Double;
begin
  if FApproxDaysPerYear = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := DaySpan(ANow, AThen) / FApproxDaysPerYear;
end;

function TXCalendar.MonthSpan(const ANow, AThen: TDateTime): Double;
begin
  if FApproxDaysPerMonth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := DaySpan(ANow, AThen) / FApproxDaysPerMonth;
end;

function TXCalendar.WeekSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := DaySpan(ANow, AThen) / 7;
end;

function TXCalendar.DaySpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SpanOfNowAndThen(ANow, AThen);
end;

function TXCalendar.HourSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := HoursPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function TXCalendar.MinuteSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := MinsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function TXCalendar.SecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SecsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function TXCalendar.MilliSecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := MSecsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function TXCalendar.IncYear(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(AValue, Year, Month, Day);
  IncAYear(Year, Month, Day, ANumberOfYears);
  Result := EncodeDate(Year, Month, Day);
  ReplaceTime(Result, AValue);
end;

procedure TXCalendar.IncAYear(var Year, Month, Day: Word;
  NumberOfYears: Integer);
var
  LMonthsInYear, LDaysInMonth: Word;
begin
  Inc(Year, NumberOfYears);
  LMonthsInYear := MonthsInAYear(Year);
  if Month > LMonthsInYear then Month := LMonthsInYear;
  LDaysInMonth := DaysInAMonth(Year, Month);
  if Day > LDaysInMonth then Day := LDaysInMonth;
end;

function TXCalendar.IncMonth(const DateTime: TDateTime;
  NumberOfMonths: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  IncAMonth(Year, Month, Day, NumberOfMonths);
  Result := EncodeDate(Year, Month, Day);
  ReplaceTime(Result, DateTime);
end;

procedure TXCalendar.IncAMonth(var Year, Month, Day: Word;
  NumberOfMonths: Integer);
var
  LMonthsInYear, LDaysInMonth: Word;
  Sign: Integer;
begin
  if NumberOfMonths >= 0 then Sign := 1 else Sign := -1;
  LMonthsInYear := MonthsInAYear(Year);
  while Word(Month + NumberOfMonths - 1) > LMonthsInYear - 1 do { works for negative NumberOfMonths also }
  begin                                                         
    Inc(Year, Sign);
    Dec(NumberOfMonths, Sign * LMonthsInYear);
    LMonthsInYear := MonthsInAYear(Year);
  end;
  Inc(Month, NumberOfMonths);
  LDaysInMonth := DaysInAMonth(Year, Month);
  if Day > LDaysInMonth then Day := LDaysInMonth;
end;

function TXCalendar.IncWeek(const AValue: TDateTime;
  const ANumberOfWeeks: Integer): TDateTime;
begin
  Result := AValue + ANumberOfWeeks * 7;
end;

function TXCalendar.IncDay(const AValue: TDateTime;
  const ANumberOfDays: Integer): TDateTime;
begin
  Result := AValue + ANumberOfDays;
end;

function TXCalendar.IncHour(const AValue: TDateTime;
  const ANumberOfHours: Int64): TDateTime;
begin
  Result := AlgebraicDateTimeToDateTime(
    ((DateTimeToAlgebraicDateTime(AValue) * HoursPerDay) + ANumberOfHours) / HoursPerDay);
end;

function TXCalendar.IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64): TDateTime;
begin
  Result := AlgebraicDateTimeToDateTime(
    ((DateTimeToAlgebraicDateTime(AValue) * MinsPerDay) + ANumberOfMinutes) / MinsPerDay);
end;

function TXCalendar.IncSecond(const AValue: TDateTime;
  const ANumberOfSeconds: Int64): TDateTime;
begin
  Result := AlgebraicDateTimeToDateTime(
    ((DateTimeToAlgebraicDateTime(AValue) * SecsPerDay) + ANumberOfSeconds) / SecsPerDay);
end;

function TXCalendar.IncMilliSecond(const AValue: TDateTime;
  const ANumberOfMilliSeconds: Int64): TDateTime;
begin
  Result := AlgebraicDateTimeToDateTime(
    ((DateTimeToAlgebraicDateTime(AValue) * MSecsPerDay) + ANumberOfMilliSeconds) / MSecsPerDay);
end;

function TXCalendar.EncodeDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  if not TryEncodeDateTime(AYear, AMonth, ADay,
                           AHour, AMinute, ASecond, AMilliSecond, Result) then
    InvalidDateTimeError(AYear, AMonth, ADay,
                         AHour, AMinute, ASecond, AMilliSecond);
end;

procedure TXCalendar.DecodeDateTime(const AValue: TDateTime; out AYear,
  AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  DecodeTime(AValue, AHour, AMinute, ASecond, AMilliSecond);
end;

function TXCalendar.EncodeDateWeek(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  if not TryEncodeDateWeek(AYear, AWeekOfYear, Result, ADayOfWeek) then
    InvalidDateWeekError(AYear, AWeekOfYear, ADayOfWeek);
end;

function TXCalendar.EncodeDateDay(const AYear,
  ADayOfYear: Word): TDateTime;
begin
  if not TryEncodeDateDay(AYear, ADayOfYear, Result) then
    InvalidDateDayError(AYear, ADayOfYear);
end;

procedure TXCalendar.DecodeDateDay(const AValue: TDateTime; out AYear,
  ADayOfYear: Word);
begin
  AYear := YearOf(AValue);
  ADayOfYear := DayOfTheYear(AValue);
end;

function TXCalendar.EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth,
  ADayOfWeek: Word): TDateTime;
begin
  if not TryEncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek,
                                Result) then
    InvalidDateMonthWeekError(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
end;

function TXCalendar.TryEncodeDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean;
var
  LTime: TDateTime;
begin
  Result := TryEncodeDate(AYear, AMonth, ADay, AValue);
  if Result then
  begin
    Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
    if Result then
      if AValue >= 0 then
        AValue := AValue + LTime
      else
        AValue := AValue - LTime
  end;
end;

function TXCalendar.TryEncodeDateWeek(const AYear, AWeekOfYear: Word;
  out AValue: TDateTime; const ADayOfWeek: Word): Boolean;
var
  LDayOfYear: Integer;
  LDaysInFirstWeek: Word;
begin
  Result := IsValidDateWeek(AYear, AWeekOfYear, ADayOfWeek);
  if Result then
  begin
    AValue := EncodeDate(AYear, 1, 1);
    LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(AValue));
    LDayOfYear := (AWeekOfYear - 1) * 7 + DayOfWeekToOffset(ADayOfWeek); { + 1 }
    if LDaysInFirstWeek <> 7 then
    begin
      if LDaysInFirstWeek < FMinWeekDays then
        { The counted weeks in AWeekOfYear do not contain some first days of the year. So add them }
        Inc(LDayOfYear, LDaysInFirstWeek)
      else
        { The first counted week in AWeekOfYear is started in the previous year. So subtract the extra days }
        Dec(LDayOfYear, 7 - LDaysInFirstWeek);
    end;
    AValue := AValue + LDayOfYear; { - 1 }
  end;
end;

function TXCalendar.TryEncodeDateMonthWeek(const AYear, AMonth,
  AWeekOfMonth, ADayOfWeek: Word; var AValue: TDateTime): Boolean;
var
  LDayOfMonth: Integer;
  LDaysInFirstWeek: Word;
begin
  Result := IsValidDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
  if Result then
  begin
    AValue := EncodeDate(AYear, AMonth, 1);
    LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(AValue));
    LDayOfMonth := (AWeekOfMonth - 1) * 7 + DayOfWeekToOffset(ADayOfWeek); { + 1 }
    if LDaysInFirstWeek <> 7 then
    begin
      if LDaysInFirstWeek < FMinWeekDays then
        { The counted weeks in AWeekOfMonth do not contain some first days of the month. So add them }
        Inc(LDayOfMonth, LDaysInFirstWeek)
      else
        { The first counted week in AWeekOfMonth is started in the previous month. So subtract the extra days }
        Dec(LDayOfMonth, 7 - LDaysInFirstWeek);
    end;
    AValue := AValue + LDayOfMonth; { - 1 }
  end;
end;

function TXCalendar.TryEncodeDateDay(const AYear, ADayOfYear: Word;
  out AValue: TDateTime): Boolean;
begin
  Result := IsValidDateDay(AYear, ADayOfYear);
  if Result then
    AValue := StartOfAYear(AYear) + ADayOfYear - 1;
end;

function TXCalendar.RecodeYear(const AValue: TDateTime;
  const AYear: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, AYear, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeMonth(const AValue: TDateTime;
  const AMonth: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, AMonth,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeDay(const AValue: TDateTime;
  const ADay: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    ADay, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, AHour, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, AMinute, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, ASecond,
    RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeMilliSecond(const AValue: TDateTime;
  const AMilliSecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, AMilliSecond);
end;

function TXCalendar.RecodeDate(const AValue: TDateTime; const AYear,
  AMonth, ADay: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, AYear, AMonth, ADay, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs);
end;

function TXCalendar.RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue, RecodeLeaveFieldAsIs, RecodeLeaveFieldAsIs,
    RecodeLeaveFieldAsIs, AHour, AMinute, ASecond, AMilliSecond);
end;

function TXCalendar.RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  if not TryRecodeDateTime(AValue, AYear, AMonth, ADay,
                           AHour, AMinute, ASecond, AMilliSecond, Result) then
    InvalidDateTimeError(AYear, AMonth, ADay,
                         AHour, AMinute, ASecond, AMilliSecond,
                         AValue);
end;

function TXCalendar.TryRecodeDateTime(const AValue: TDateTime; const AYear,
  AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  out AResult: TDateTime): Boolean;
var
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond: Word;
begin
  DecodeDateTime(AValue, LYear, LMonth, LDay,
                         LHour, LMinute, LSecond, LMilliSecond);
  if AYear <> RecodeLeaveFieldAsIs then LYear := AYear;
  if AMonth <> RecodeLeaveFieldAsIs then LMonth := AMonth;
  if ADay <> RecodeLeaveFieldAsIs then LDay := ADay;
  if AHour <> RecodeLeaveFieldAsIs then LHour := AHour;
  if AMinute <> RecodeLeaveFieldAsIs then LMinute := AMinute;
  if ASecond <> RecodeLeaveFieldAsIs then LSecond := ASecond;
  if AMilliSecond <> RecodeLeaveFieldAsIs then LMilliSecond := AMilliSecond;
  Result := TryEncodeDateTime(LYear, LMonth, LDay,
                              LHour, LMinute, LSecond, LMilliSecond, AResult);
end;

const
  OneMillisecond = 1 / MSecsPerDay;

function TXCalendar.CompareDateTime(const A, B: TDateTime): Integer;
var
  LA, LB: Double;
begin
  LA := DateTimeToAlgebraicDateTime(A);
  LB := DateTimeToAlgebraicDateTime(B);
  if Abs(LA - LB) < OneMillisecond then
    Result := 0
  else if LA < LB then
    Result := -1
  else
    Result := 1;
end;

function TXCalendar.SameDateTime(const A, B: TDateTime): Boolean;
begin
  Result := Abs(DateTimeToAlgebraicDateTime(A) - DateTimeToAlgebraicDateTime(B)) < OneMillisecond;
end;

function TXCalendar.CompareDate(const A, B: TDateTime): Integer;
begin
  if Trunc(A) = Trunc(B) then
    Result := 0
  else if A < B then
    Result := -1
  else
    Result := 1;
end;

function TXCalendar.SameDate(const A, B: TDateTime): Boolean;
begin
  Result := Trunc(A) = Trunc(B);
end;

function TXCalendar.CompareTime(const A, B: TDateTime): Integer;
var
  AF, BF: Double;
begin
  AF := Abs(Frac(A));
  BF := Abs(Frac(B));
  if Abs(AF - BF) < OneMillisecond then
    Result := 0
  else if AF < BF then
    Result := -1
  else
    Result := 1;
end;

function TXCalendar.SameTime(const A, B: TDateTime): Boolean;
begin
  Result := Abs(Abs(Frac(A)) - Abs(Frac(B))) < OneMillisecond;
end;

function TXCalendar.NthDayOfWeek(const AValue: TDateTime): Word;
begin
  Result := (DayOfTheMonth(AValue) - 1) div 7 + 1;
end;

procedure TXCalendar.DecodeDayOfWeekInMonth(const AValue: TDateTime;
  out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);
var
  ADay: Word;
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  ANthDayOfWeek := (ADay - 1) div 7 + 1;
  ADayOfWeek := DayOfWeek(AValue);
end;

function TXCalendar.EncodeDayOfWeekInMonth(const AYear, AMonth: Word;
  const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
begin
  if not TryEncodeDayOfWeekInMonth(AYear, AMonth, ANthDayOfWeek, ADayOfWeek, Result) then
    InvalidDayOfWeekInMonthError(AYear, AMonth, ANthDayOfWeek, ADayOfWeek);
end;

function TXCalendar.TryEncodeDayOfWeekInMonth(const AYear, AMonth: Word;
  const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word; out AValue: TDateTime): Boolean;
var
  LDOW, LDay: Word;
begin
  if (ADayOfWeek < xcalSunday) or (ADayOfWeek > xcalSaturday) then
  begin
    Result := False;
    Exit;
  end;

  { Works with any WeekStartDay }

  if ANthDayOfWeek >= 0 then
  begin
    LDOW := DayOfWeek(EncodeDate(AYear, AMonth, 1));
    if LDOW <= ADayOfWeek then
      LDay := (ADayOfWeek - LDOW + 1) + 7 * (ANthDayOfWeek - 1)
    else
      LDay := (8 - LDOW) + ADayOfWeek + 7 * (ANthDayOfWeek - 1);
  end
  else
  begin
    LDay := DaysInAMonth(AYear, AMonth);
    LDOW := DayOfWeek(EncodeDate(AYear, AMonth, LDay));
    if LDOW >= ADayOfWeek then
      Dec(LDay, (LDOW - ADayOfWeek) - 7 * (ANthDayOfWeek + 1))
    else
      Dec(LDay, LDOW - 1 + (8 - ADayOfWeek) - 7 * (ANthDayOfWeek + 1));
  end;

  Result := TryEncodeDate(AYear, AMonth, LDay, AValue);
end;

function TXCalendar.EncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
  const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
begin
  if not TryEncodeDayOfWeekAfterADate(AFromDate, ANthDayOfWeek, ADayOfWeek, Result) then
    ConvertErrorFmt(@SXCalInvalidDayOfWeek, [ADayOfWeek]);
end;

function TXCalendar.TryEncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
  const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word; out AValue: TDateTime): Boolean;
var
  LOffset, LAddDays: Integer;
begin
  if (ADayOfWeek < xcalSunday) or (ADayOfWeek > xcalSaturday) then
  begin
    Result := False;
    Exit;
  end;

  LOffset := DayOfWeek(AFromDate) - ADayOfWeek;
  if LOffset < 0 then
    Inc(LOffset, 7);

  if ANthDayOfWeek >= 0 then
  begin
    LAddDays := 7 - LOffset;
    if LAddDays = 7 then
      LAddDays := 0;
    AValue := Trunc(AFromDate) + LAddDays + 7 * (ANthDayOfWeek - 1);
  end
  else
  begin
    LAddDays := LOffset;
    AValue := Trunc(AFromDate) - LAddDays - 7 * (ANthDayOfWeek + 1);
  end;
  Result := True;
end;

function TXCalendar.EncodeNearestDayOfWeekToADate(const ADate: TDateTime; const ADayOfWeek: Word): TDateTime;
begin
  if not TryEncodeNearestDayOfWeekToADate(ADate, ADayOfWeek, Result) then
    ConvertErrorFmt(@SXCalInvalidDayOfWeek, [ADayOfWeek]);
end;

function TXCalendar.TryEncodeNearestDayOfWeekToADate(const ADate: TDateTime;
  const ADayOfWeek: Word; out AValue: TDateTime): Boolean;
var
  LDate: TDateTime;
  LOffset: Integer;
begin
  if (ADayOfWeek < xcalSunday) or (ADayOfWeek > xcalSaturday) then
  begin
    Result := False;
    Exit;
  end;

  LDate := Trunc(ADate) + 3;
  LOffset := DayOfWeek(LDate) - ADayOfWeek;
  if LOffset < 0 then
    Inc(LOffset, 7);
  AValue := LDate - LOffset;
  Result := True;
end;

procedure TXCalendar.DecodeDateWeek(const AValue: TDateTime; out AYear,
  AWeekOfYear, ADayOfWeek: Word);
var
  LDaysInYear: Word;
  LDayOfYear, LSaveDayOfYear: Integer;
  LMonth, LDay: Word;
  LStart: TDateTime;
  LDaysInFirstWeek, LDaysInLastWeek: Word;
begin
  DecodeDateFully(AValue, AYear, LMonth, LDay, ADayOfWeek);
  LStart := EncodeDate(AYear, 1, 1);
  LDayOfYear := Trunc(AValue - LStart + 1);
  LSaveDayOfYear := LDayOfYear;
  LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(LStart));
  if LDaysInFirstWeek <> 7 then
  begin
    if LDaysInFirstWeek >= FMinWeekDays then
      { The incomplete first week should be counted }
      Inc(LDayOfYear, 7 - LDaysInFirstWeek)
    else
      { The incomplete first week should NOT be counted }
      Dec(LDayOfYear, LDaysInFirstWeek);
  end;
  if LDayOfYear <= 0 then
    DecodeDateWeek(LStart - 1, AYear, AWeekOfYear, LDay)
  else
  begin
    AWeekOfYear := LDayOfYear div 7;
    if LDayOfYear mod 7 <> 0 then
      Inc(AWeekOfYear);

    if (FMinWeekDays <> 7) then
    begin
      { Check if the date falls on the first week of the next year }
      LDaysInYear := DaysInAYear(AYear);
      LDaysInLastWeek := DayOfWeekToOffset(DayOfWeek(LStart + LDaysInYear - 1)) + 1;
      if (LDaysInLastWeek < FMinWeekDays) and
         (LSaveDayOfYear > LDaysInYear - LDaysInLastWeek) then
      begin
        Inc(AYear);
        AWeekOfYear := 1;
      end;
    end;
  end;
end;

procedure TXCalendar.DecodeDateMonthWeek(const AValue: TDateTime;
  out AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
var
  LDay, LDaysInMonth: Word;
  LDayOfMonth: Integer;
  LStart: TDateTime;
  LDaysInFirstWeek, LDaysInLastWeek: Word;
begin
  DecodeDateFully(AValue, AYear, AMonth, LDay, ADayOfWeek);
  LStart := EncodeDate(AYear, AMonth, 1);
  LDayOfMonth := LDay;
  LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(LStart));
  if LDaysInFirstWeek <> 7 then
  begin
    if LDaysInFirstWeek >= FMinWeekDays then
      { The incomplete first week should be counted }
      Inc(LDayOfMonth, 7 - LDaysInFirstWeek)
    else
      { The incomplete first week should NOT be counted }
      Dec(LDayOfMonth, LDaysInFirstWeek);
  end;
  if LDayOfMonth <= 0 then
    DecodeDateMonthWeek(LStart - 1, AYear, AMonth, AWeekOfMonth, LDay)
  else
  begin
    AWeekOfMonth := LDayOfMonth div 7;
    if LDayOfMonth mod 7 <> 0 then
      Inc(AWeekOfMonth);

    if (FMinWeekDays <> 7) then
    begin
      { Check if the date falls on the first week of the next month }
      LDaysInMonth := DaysInAMonth(AYear, AMonth);
      LDaysInLastWeek := DayOfWeekToOffset(DayOfWeek(LStart + LDaysInMonth - 1)) + 1;
      if (LDaysInLastWeek < FMinWeekDays) and
         (LDay > LDaysInMonth - LDaysInLastWeek) then
      begin
        Inc(AMonth);
        if AMonth = MonthsInAYear(AYear) + 1 then
        begin
          AMonth := 1;
          Inc(AYear);
        end;
        AWeekOfMonth := 1;
      end;
    end;
  end;
end;

function TXCalendar.MonthsInYear(const AValue: TDateTime): Word;
begin
  Result := MonthsInAYear(YearOf(AValue));
end;

function TXCalendar.MonthsInAYear(const AYear: Word): Word;
begin
  Result := 12;
end;

function TXCalendar.WeeksInMonth(const AValue: TDateTime): Word;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := WeeksInAMonth(LYear, LMonth);
end;

function TXCalendar.WeeksInAMonth(const AYear, AMonth: Word): Word;
var
  LDaysInMonth: Word;
  LDaysInFirstWeek, LDaysInLastWeek: Word;
begin
  LDaysInMonth := DaysInAMonth(AYear, AMonth);
  LDaysInFirstWeek := 7 - DayOfWeekToOffset(DayOfWeek(EncodeDate(AYear, AMonth, 1)));
  if LDaysInMonth - LDaysInFirstWeek < FMinWeekDays then { just in case! }
  begin
    Result := 0;
    if FMinWeekDays = 7 then
      Inc(Result);
    Exit;
  end;
  if LDaysInFirstWeek <> 7 then
  begin
    if LDaysInFirstWeek >= FMinWeekDays then
      { The incomplete first week should be counted }
      Inc(LDaysInMonth, 7 - LDaysInFirstWeek)
    else
      { The incomplete first week should NOT be counted }
      Dec(LDaysInMonth, LDaysInFirstWeek);
  end;
  DivMod(LDaysInMonth, 7, Result, LDaysInLastWeek);
  { Check if the incomplete last week should be counted. In case of full week rule,
    we count the week spanning between two months as being the last of the first. }
  if (LDaysInLastWeek >= FMinWeekDays) or
     ((FMinWeekDays = 7) and (LDaysInLastWeek > 0)) then
    Inc(Result);
end;

procedure TXCalendar.SetWeekRule(const Value: TXCalWeekRule);
begin
  if Value <> FWeekRule then
  begin
    FWeekRule := Value;
    case FWeekRule of
      wrFullWeek:    FMinWeekDays := 7;
      wrFourDayWeek: FMinWeekDays := 4;
      wrOneDayWeek:  FMinWeekDays := 1;
    end;
    Changed;
  end;
end;

procedure TXCalendar.SetWeekStartDay(const Value: Word);
begin
  if (Value <> FWeekStartDay) and (xcalSunday <= Value) and (Value <= xcalSaturday) then
  begin
    FWeekStartDay := Value;
    Changed;
  end;
end;

procedure TXCalendar.SetFormatSettings(FormatSettings: TXCalFormatSettings);
begin
  FFormatSettings.Assign(FormatSettings);
end;

procedure TXCalendar.SetVersion(const Value: string);
begin
  { This is needed for the property to be shown in the object inspector }
end;

function TXCalendar.OffsetToDayOfWeek(const AOffset: Word): Word;
begin
  Result := FWeekStartDay + AOffset;
end;

function TXCalendar.DayOfWeekToOffset(const ADayOfWeek: Word): Word;
var
  Offs: SmallInt;
begin
  Offs := ADayOfWeek - FWeekStartDay;
  if Offs < 0 then
    Inc(Offs, 7);
  Result := Offs;
end;

function TXCalendar.DateOf(const AValue: TDateTime): TDateTime;
begin
  Result := Trunc(AValue);
end;

function TXCalendar.TimeOf(const AValue: TDateTime): TDateTime;
begin
  Result := Frac(AValue);
end;

function TXCalendar.IsPM(const AValue: TDateTime): Boolean;
begin
  Result := HourOf(AValue) >= 12;
end;

function TXCalendar.IsValidTime(const AHour, AMinute, ASecond,
  AMilliSecond: Word): Boolean;
begin
  Result := ((AHour < HoursPerDay) and (AMinute < MinsPerHour) and
             (ASecond < SecsPerMin) and (AMilliSecond < MSecsPerSec)) or
            ((AHour = 24) and (AMinute = 0) and // midnight early next day
             (ASecond = 0) and (AMilliSecond = 0));
end;

function TXCalendar.Today: TDateTime;
begin
  Result := Date;
end;

function TXCalendar.Yesterday: TDateTime;
begin
  Result := Date - 1;
end;

function TXCalendar.Tomorrow: TDateTime;
begin
  Result := Date + 1;
end;

function TXCalendar.IsToday(const AValue: TDateTime): Boolean;
begin
  Result := IsSameDay(AValue, Date);
end;

function TXCalendar.IsSameDay(const AValue, ABasis: TDateTime): Boolean;
begin
  Result := (Trunc(AValue) = Trunc(ABasis));
end;

function TXCalendar.HourOf(const AValue: TDateTime): Word;
var
  LMinute, LSecond, LMilliSecond: Word;
begin
  DecodeTime(AValue, Result, LMinute, LSecond, LMilliSecond);
end;

function TXCalendar.MinuteOf(const AValue: TDateTime): Word;
var
  LHour, LSecond, LMilliSecond: Word;
begin
  DecodeTime(AValue, LHour, Result, LSecond, LMilliSecond);
end;

function TXCalendar.SecondOf(const AValue: TDateTime): Word;
var
  LHour, LMinute, LMilliSecond: Word;
begin
  DecodeTime(AValue, LHour, LMinute, Result, LMilliSecond);
end;

function TXCalendar.MilliSecondOf(const AValue: TDateTime): Word;
var
  LHour, LMinute, LSecond: Word;
begin
  DecodeTime(AValue, LHour, LMinute, LSecond, Result);
end;

function TXCalendar.StartOfTheDay(const AValue: TDateTime): TDateTime;
begin
  Result := Trunc(AValue);
end;

function TXCalendar.EndOfTheDay(const AValue: TDateTime): TDateTime;
begin
  Result := RecodeTime(AValue, HoursPerDay - 1, MinsPerHour - 1, SecsPerMin - 1, MSecsPerSec - 1);
end;

function TXCalendar.MinuteOfTheHour(const AValue: TDateTime): Word;
begin
  Result := MinuteOf(AValue);
end;

function TXCalendar.SecondOfTheHour(const AValue: TDateTime): Word;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LSeconds + (LMinutes * SecsPerMin);
end;

function TXCalendar.MilliSecondOfTheHour(const AValue: TDateTime): LongWord;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LMilliSeconds + (LSeconds + LMinutes * SecsPerMin) * MSecsPerSec;
end;

function TXCalendar.SecondOfTheMinute(const AValue: TDateTime): Word;
begin
  Result := SecondOf(AValue);
end;

function TXCalendar.MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LMilliSeconds + LSeconds * MSecsPerSec;
end;

function TXCalendar.MilliSecondOfTheSecond(const AValue: TDateTime): Word;
begin
  Result := MilliSecondOf(AValue);
end;

function TXCalendar.WithinPastHours(const ANow, AThen: TDateTime;
  const AHours: Int64): Boolean;
begin
  Result := HoursBetween(ANow, AThen) <= AHours;
end;

function TXCalendar.WithinPastMinutes(const ANow, AThen: TDateTime;
  const AMinutes: Int64): Boolean;
begin
  Result := MinutesBetween(ANow, AThen) <= AMinutes;
end;

function TXCalendar.WithinPastSeconds(const ANow, AThen: TDateTime;
  const ASeconds: Int64): Boolean;
begin
  Result := SecondsBetween(ANow, AThen) <= ASeconds;
end;

function TXCalendar.WithinPastMilliSeconds(const ANow, AThen: TDateTime;
  const AMilliSeconds: Int64): Boolean;
begin
  Result := MilliSecondsBetween(ANow, AThen) <= AMilliSeconds;
end;

procedure TXCalendar.SaveSettingsToXMLItemChilds(XMLItem: TxcalXMLItem);
var
  xi, xi2, xi3: TxcalXMLItem;
  I: Integer;
begin

  xi := XMLItem.Add;
  xi.Name := 'FormatSettings';

    xi2 := xi.Add;
    xi2.Name := 'DecimalSeparator';
    xi2.Prop['value'] := FFormatSettings.DecimalSeparator;

    xi2 := xi.Add;
    xi2.Name := 'DateSeparator';
    xi2.Prop['value'] := FFormatSettings.DateSeparator;

    xi2 := xi.Add;
    xi2.Name := 'TimeSeparator';
    xi2.Prop['value'] := FFormatSettings.TimeSeparator;

    xi2 := xi.Add;
    xi2.Name := 'ShortDateFormat';
    xi2.Prop['value'] := FFormatSettings.ShortDateFormat;

    xi2 := xi.Add;
    xi2.Name := 'LongDateFormat';
    xi2.Prop['value'] := FFormatSettings.LongDateFormat;

    xi2 := xi.Add;
    xi2.Name := 'TimeAMString';
    xi2.Prop['value'] := FFormatSettings.TimeAMString;

    xi2 := xi.Add;
    xi2.Name := 'TimePMString';
    xi2.Prop['value'] := FFormatSettings.TimePMString;

    xi2 := xi.Add;
    xi2.Name := 'ShortTimeFormat';
    xi2.Prop['value'] := FFormatSettings.ShortTimeFormat;

    xi2 := xi.Add;
    xi2.Name := 'LongTimeFormat';
    xi2.Prop['value'] := FFormatSettings.LongTimeFormat;

    xi2 := xi.Add;
    xi2.Name := 'ShortMonthNames';

      for I := 0 to FFormatSettings.ShortMonthNames.Count - 1 do
      begin
        xi3 := xi2.Add;
        xi3.Name := 'Item';
        xi3.Prop['value'] := FFormatSettings.ShortMonthNames[I];
      end;

    xi2 := xi.Add;
    xi2.Name := 'LongMonthNames';

      for I := 0 to FFormatSettings.LongMonthNames.Count - 1 do
      begin
        xi3 := xi2.Add;
        xi3.Name := 'Item';
        xi3.Prop['value'] := FFormatSettings.LongMonthNames[I];
      end;

    xi2 := xi.Add;
    xi2.Name := 'ShortDayNames';

      for I := 0 to FFormatSettings.ShortDayNames.Count - 1 do
      begin
        xi3 := xi2.Add;
        xi3.Name := 'Item';
        xi3.Prop['value'] := FFormatSettings.ShortDayNames[I];
      end;

    xi2 := xi.Add;
    xi2.Name := 'LongDayNames';

      for I := 0 to FFormatSettings.LongDayNames.Count - 1 do
      begin
        xi3 := xi2.Add;
        xi3.Name := 'Item';
        xi3.Prop['value'] := FFormatSettings.LongDayNames[I];
      end;

    xi2 := xi.Add;
    xi2.Name := 'TwoDigitYearCenturyWindow';
    xi2.Prop['value'] := IntToStr(FFormatSettings.TwoDigitYearCenturyWindow);

  xi := XMLItem.Add;
  xi.Name := 'WeekRule';
  xi.Prop['value'] := IntToStr(Ord(FWeekRule));

  xi := XMLItem.Add;
  xi.Name := 'WeekStartDay';
  xi.Prop['value'] := IntToStr(FWeekStartDay);
end;

procedure TXCalendar.LoadSettingsFromXMLItemChilds(XMLItem: TxcalXMLItem);
var
  I, N, C: Integer;
  xi, xi2: TxcalXMLItem;
begin

  I := XMLItem.Find('FormatSettings');
  if I >= 0 then
  begin
    xi := XMLItem.Items[I];

    I := xi.Find('DecimalSeparator');
    if I >= 0 then
      FFormatSettings.DecimalSeparator := xi.Items[I].Prop['value'][1];

    I := xi.Find('DateSeparator');
    if I >= 0 then
      FFormatSettings.DateSeparator := xi.Items[I].Prop['value'][1];

    I := xi.Find('TimeSeparator');
    if I >= 0 then
      FFormatSettings.TimeSeparator := xi.Items[I].Prop['value'][1];

    I := xi.Find('ShortDateFormat');
    if I >= 0 then
      FFormatSettings.ShortDateFormat := xi.Items[I].Prop['value'];

    I := xi.Find('LongDateFormat');
    if I >= 0 then
      FFormatSettings.LongDateFormat := xi.Items[I].Prop['value'];

    I := xi.Find('TimeAMString');
    if I >= 0 then
      FFormatSettings.TimeAMString := xi.Items[I].Prop['value'];

    I := xi.Find('TimePMString');
    if I >= 0 then
      FFormatSettings.TimePMString := xi.Items[I].Prop['value'];

    I := xi.Find('ShortTimeFormat');
    if I >= 0 then
      FFormatSettings.ShortTimeFormat := xi.Items[I].Prop['value'];

    I := xi.Find('LongTimeFormat');
    if I >= 0 then
      FFormatSettings.LongTimeFormat := xi.Items[I].Prop['value'];

    FFormatSettings.ShortMonthNames.Clear;
    I := xi.Find('ShortMonthNames');
    if I >= 0 then
    begin
      xi2 := xi.Items[I];
      C := xi2.Count;
      FFormatSettings.ShortMonthNames.BeginUpdate;
      try
        for I := 0 to C - 1 do
          FFormatSettings.ShortMonthNames.Add(xi2.Items[I].Prop['value']);
      finally
        FFormatSettings.ShortMonthNames.EndUpdate;
      end;
    end;

    FFormatSettings.LongMonthNames.Clear;
    I := xi.Find('LongMonthNames');
    if I >= 0 then
    begin
      xi2 := xi.Items[I];
      C := xi2.Count;
      FFormatSettings.LongMonthNames.BeginUpdate;
      try
        for I := 0 to C - 1 do
          FFormatSettings.LongMonthNames.Add(xi2.Items[I].Prop['value']);
      finally
        FFormatSettings.LongMonthNames.EndUpdate;
      end;
    end;

    FFormatSettings.ShortDayNames.Clear;
    I := xi.Find('ShortDayNames');
    if I >= 0 then
    begin
      xi2 := xi.Items[I];
      C := xi2.Count;
      FFormatSettings.ShortDayNames.BeginUpdate;
      try
        for I := 0 to C - 1 do
          FFormatSettings.ShortDayNames.Add(xi2.Items[I].Prop['value']);
      finally
        FFormatSettings.ShortDayNames.EndUpdate;
      end;
    end;

    FFormatSettings.LongDayNames.Clear;
    I := xi.Find('LongDayNames');
    if I >= 0 then
    begin
      xi2 := xi.Items[I];
      C := xi2.Count;
      FFormatSettings.LongDayNames.BeginUpdate;
      try
        for I := 0 to C - 1 do
          FFormatSettings.LongDayNames.Add(xi2.Items[I].Prop['value']);
      finally
        FFormatSettings.LongDayNames.EndUpdate;
      end;
    end;

    I := xi.Find('TwoDigitYearCenturyWindow');
    if (I >= 0) and TryStrToInt(xi.Items[I].Prop['value'], N) and
       (N >= 0) then
      FFormatSettings.TwoDigitYearCenturyWindow := N;

  end;

  I := XMLItem.Find('WeekRule');
  if (I >= 0) and TryStrToInt(XMLItem.Items[I].Prop['value'], N) and
    (Ord(wrFullWeek) <= N) and (N <= Ord(wrOneDayWeek)) then
    FWeekRule := TXCalWeekRule(N);

  I := XMLItem.Find('WeekStartDay');
  if (I >= 0) and TryStrToInt(XMLItem.Items[I].Prop['value'], N) and
    (xcalSunday <= N) and (N <= xcalSaturday) then
    FWeekStartDay := N;

end;

procedure TXCalendar.BeginUpdateSettings;
begin
  Inc(FUpdateCount);
end;

procedure TXCalendar.EndUpdateSettings;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

procedure TXCalendar.SaveSettingsToStream(Stream: TStream);
var
  XMLDoc: TxcalXMLDocument;
begin
  XMLDoc := TxcalXMLDocument.Create;
  try
    XMLDoc.AutoIndent := True;
    XMLDoc.Root.Name := XCalendarSettingsXMLRootName;
    XMLDoc.Root.Prop['Version'] := FVersion;
    XMLDoc.Root.Prop['ClassName'] := ClassName;
    SaveSettingsToXMLItemChilds(XMLDoc.Root);
    XMLDoc.SaveToStream(Stream);
  finally
    XMLDoc.Free;
  end;
end;

procedure TXCalendar.LoadSettingsFromStream(Stream: TStream);
var
  XMLDoc: TxcalXMLDocument;
begin
  XMLDoc := TxcalXMLDocument.Create;
  try
    XMLDoc.LoadFromStream(Stream);
    if XMLDoc.Root.Name = XCalendarSettingsXMLRootName then
    begin
      BeginUpdateSettings;
      try
        LoadSettingsFromXMLItemChilds(XMLDoc.Root);
      finally
        EndUpdateSettings;
      end;
    end;
  finally
    XMLDoc.Free;
  end;
end;

procedure TXCalendar.SaveSettingsToFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveSettingsToStream(f);
  finally
    f.Free;
  end;
end;

procedure TXCalendar.LoadSettingsFromFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadSettingsFromStream(f);
  finally
    f.Free;
  end;
end;

procedure TXCalendar.LoadSettingsFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadSettingsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXCalendar.InvalidDateDayError(const AYear, ADayOfYear: Word);
begin
  raise EConvertError.CreateFmt(SXCalInvalidDateDay, [AYear, ADayOfYear]);
end;

procedure TXCalendar.InvalidDateMonthWeekError(const AYear, AMonth,
  AWeekOfMonth, ADayOfWeek: Word);
begin
  raise EConvertError.CreateFmt(SXCalInvalidDateMonthWeek, [AYear, AMonth,
    AWeekOfMonth, ADayOfWeek]);
end;

procedure TXCalendar.InvalidDateTimeError(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word; const ABaseDate: TDateTime);

  function Translate(AOrig, AValue: Word): string;
  begin
    if AValue = RecodeLeaveFieldAsIs then
      if ABaseDate = 0 then
        Result := SXCalMissingDateTimeField
      else
        Result := IntToStr(AOrig)
    else
      Result := IntToStr(AValue);
  end;

var
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond: Word;
begin
  DecodeDate(ABaseDate, LYear, LMonth, LDay);
  DecodeTime(ABaseDate, LHour, LMinute, LSecond, LMilliSecond);
  raise EConvertError.CreateFmt(SInvalidDateTime,
                                [Translate(LYear, AYear) + DateSeparator +
                                 Translate(LMonth, AMonth) + DateSeparator +
                                 Translate(LDay, ADay) + ' ' +
                                 Translate(LHour, AHour) + TimeSeparator +
                                 Translate(LMinute, AMinute) + TimeSeparator +
                                 Translate(LSecond, ASecond) + DecimalSeparator +
                                 Translate(LMilliSecond, AMilliSecond)]);
end;

procedure TXCalendar.InvalidDateWeekError(const AYear, AWeekOfYear,
  ADayOfWeek: Word);
begin
  raise EConvertError.CreateFmt(SXCalInvalidDateWeek, [AYear, AWeekOfYear, ADayOfWeek]);
end;

procedure TXCalendar.InvalidDayOfWeekInMonthError(const AYear,
  AMonth: Word; const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word);
begin
  raise EConvertError.CreateFmt(SXCalInvalidDayOfWeekInMonth, [AYear, AMonth,
    ANthDayOfWeek, ADayOfWeek]);
end;

{ TXCalFormatSettings }

constructor TXCalFormatSettings.Create(AOwner: TXCalendar);
begin
  FOwner := AOwner;

  FShortMonthNames := TXCalStringList.Create;
  FShortMonthNames.OnChange := StringListsChanged;
  FLongMonthNames := TXCalStringList.Create;
  FLongMonthNames.OnChange := StringListsChanged;
  FShortDayNames := TXCalStringList.Create;
  FShortDayNames.OnChange := StringListsChanged;
  FLongDayNames := TXCalStringList.Create;
  FLongDayNames.OnChange := StringListsChanged;
end;

destructor TXCalFormatSettings.Destroy;
begin
  FShortMonthNames.Free;
  FLongMonthNames.Free;
  FShortDayNames.Free;
  FLongDayNames.Free;

  inherited Destroy;
end;

procedure TXCalFormatSettings.Assign(Source: TPersistent);
begin
  if (Source is TXCalFormatSettings) then
  begin
    FDecimalSeparator := TXCalFormatSettings(Source).FDecimalSeparator;
    FDateSeparator := TXCalFormatSettings(Source).FDateSeparator;
    FTimeSeparator := TXCalFormatSettings(Source).FTimeSeparator;
    FShortDateFormat := TXCalFormatSettings(Source).FShortDateFormat;
    FLongDateFormat := TXCalFormatSettings(Source).FLongDateFormat;
    FTimeAMString := TXCalFormatSettings(Source).FTimeAMString;
    FTimePMString := TXCalFormatSettings(Source).FTimePMString;
    FShortTimeFormat := TXCalFormatSettings(Source).FShortTimeFormat;
    FLongTimeFormat := TXCalFormatSettings(Source).FLongTimeFormat;
    FShortMonthNames.Assign(TXCalFormatSettings(Source).FShortMonthNames);
    FLongMonthNames.Assign(TXCalFormatSettings(Source).FLongMonthNames);
    FShortDayNames.Assign(TXCalFormatSettings(Source).FShortDayNames);
    FLongDayNames.Assign(TXCalFormatSettings(Source).FLongDayNames);
    FTwoDigitYearCenturyWindow := TXCalFormatSettings(Source).FTwoDigitYearCenturyWindow;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TXCalFormatSettings.LoadFromGlobalVariables;
var
  I: Integer;
begin
  FDecimalSeparator := SysUtils.DecimalSeparator;
  FDateSeparator := SysUtils.DateSeparator;
  FTimeSeparator := SysUtils.TimeSeparator;
  FShortDateFormat := SysUtils.ShortDateFormat;
  FLongDateFormat := SysUtils.LongDateFormat;
  FTimeAMString := SysUtils.TimeAMString;
  FTimePMString := SysUtils.TimePMString;
  FShortTimeFormat := SysUtils.ShortTimeFormat;
  FLongTimeFormat := SysUtils.LongTimeFormat;
  FShortMonthNames.Clear;
  FLongMonthNames.Clear;
  for I := 1 to 12 do
  begin
    FShortMonthNames.Add(SysUtils.ShortMonthNames[I]);
    FLongMonthNames.Add(SysUtils.LongMonthNames[I]);
  end;
  FShortDayNames.Clear;
  FLongDayNames.Clear;
  for I := 1 to 7 do
  begin
    FShortDayNames.Add(SysUtils.ShortDayNames[I]);
    FLongDayNames.Add(SysUtils.LongDayNames[I]);
  end;
  FTwoDigitYearCenturyWindow := SysUtils.TwoDigitYearCenturyWindow;
  Changed;
end;

procedure TXCalFormatSettings.CopyToGlobalVariables;
var
  I: Integer;
begin
  SysUtils.DecimalSeparator := FDecimalSeparator;
  SysUtils.DateSeparator := FDateSeparator;
  SysUtils.TimeSeparator := FTimeSeparator;
  SysUtils.ShortDateFormat := FShortDateFormat;
  SysUtils.LongDateFormat := FLongDateFormat;
  SysUtils.TimeAMString := FTimeAMString;
  SysUtils.TimePMString := FTimePMString;
  SysUtils.ShortTimeFormat := FShortTimeFormat;
  SysUtils.LongTimeFormat := FLongTimeFormat;
  for I := 1 to 12 do
  begin
    if I > FShortMonthNames.Count then
      SysUtils.ShortMonthNames[I] := ''
    else
      SysUtils.ShortMonthNames[I] := FShortMonthNames[I - 1];
    if I > FLongMonthNames.Count then
      SysUtils.LongMonthNames[I] := ''
    else
      SysUtils.LongMonthNames[I] := FLongMonthNames[I - 1];
  end;
  for I := 1 to 7 do
  begin
    if I > FShortDayNames.Count then
      SysUtils.ShortDayNames[I] := ''
    else
      SysUtils.ShortDayNames[I] := FShortDayNames[I - 1];
    if I > FLongDayNames.Count then
      SysUtils.LongDayNames[I] := ''
    else
      SysUtils.LongDayNames[I] := FLongDayNames[I - 1];
  end;
  SysUtils.TwoDigitYearCenturyWindow := FTwoDigitYearCenturyWindow;
end;

{$IFDEF Delphi7}
procedure TXCalFormatSettings.LoadFromRecord(const FormatSettings: TFormatSettings);
var
  I: Integer;
begin
  FDecimalSeparator := FormatSettings.DecimalSeparator;
  FDateSeparator := FormatSettings.DateSeparator;
  FTimeSeparator := FormatSettings.TimeSeparator;
  FShortDateFormat := FormatSettings.ShortDateFormat;
  FLongDateFormat := FormatSettings.LongDateFormat;
  FTimeAMString := FormatSettings.TimeAMString;
  FTimePMString := FormatSettings.TimePMString;
  FShortTimeFormat := FormatSettings.ShortTimeFormat;
  FLongTimeFormat := FormatSettings.LongTimeFormat;
  FShortMonthNames.Clear;
  FLongMonthNames.Clear;
  for I := 1 to 12 do
  begin
    FShortMonthNames.Add(FormatSettings.ShortMonthNames[I]);
    FLongMonthNames.Add(FormatSettings.LongMonthNames[I]);
  end;
  FShortDayNames.Clear;
  FLongDayNames.Clear;
  for I := 1 to 7 do
  begin
    FShortDayNames.Add(FormatSettings.ShortDayNames[I]);
    FLongDayNames.Add(FormatSettings.LongDayNames[I]);
  end;
  FTwoDigitYearCenturyWindow := FormatSettings.TwoDigitYearCenturyWindow;
  Changed;
end;

procedure TXCalFormatSettings.CopyToRecord(var FormatSettings: TFormatSettings);
var
  I: Integer;
begin
  FormatSettings.DecimalSeparator := FDecimalSeparator;
  FormatSettings.DateSeparator := FDateSeparator;
  FormatSettings.TimeSeparator := FTimeSeparator;
  FormatSettings.ShortDateFormat := FShortDateFormat;
  FormatSettings.LongDateFormat := FLongDateFormat;
  FormatSettings.TimeAMString := FTimeAMString;
  FormatSettings.TimePMString := FTimePMString;
  FormatSettings.ShortTimeFormat := FShortTimeFormat;
  FormatSettings.LongTimeFormat := FLongTimeFormat;
  for I := 1 to 12 do
  begin
    if I > FShortMonthNames.Count then
      FormatSettings.ShortMonthNames[I] := ''
    else
      FormatSettings.ShortMonthNames[I] := FShortMonthNames[I - 1];
    if I > FLongMonthNames.Count then
      FormatSettings.LongMonthNames[I] := ''
    else
      FormatSettings.LongMonthNames[I] := FLongMonthNames[I - 1];
  end;
  for I := 1 to 7 do
  begin
    if I > FShortDayNames.Count then
      FormatSettings.ShortDayNames[I] := ''
    else
      FormatSettings.ShortDayNames[I] := FShortDayNames[I - 1];
    if I > FLongDayNames.Count then
      FormatSettings.LongDayNames[I] := ''
    else
      FormatSettings.LongDayNames[I] := FLongDayNames[I - 1];
  end;
  FormatSettings.TwoDigitYearCenturyWindow := FTwoDigitYearCenturyWindow;
end;
{$ENDIF}

procedure TXCalFormatSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TXCalFormatSettings.StringListsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TXCalFormatSettings.SetDateSeparator(const Value: Char);
begin
  if Value <> FDateSeparator then
  begin
    FDateSeparator := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetDecimalSeparator(const Value: Char);
begin
  if Value <> FDecimalSeparator then
  begin
    FDecimalSeparator := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetLongDateFormat(const Value: String);
begin
  if Value <> FLongDateFormat then
  begin
    FLongDateFormat := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetLongTimeFormat(const Value: String);
begin
  if Value <> FLongTimeFormat then
  begin
    FLongTimeFormat := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetShortDateFormat(const Value: String);
begin
  if Value <> FShortDateFormat then
  begin
    FShortDateFormat := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetShortTimeFormat(const Value: String);
begin
  if Value <> FShortTimeFormat then
  begin
    FShortTimeFormat := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetTimeAMString(const Value: String);
begin
  if Value <> FTimeAMString then
  begin
    FTimeAMString := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetTimePMString(const Value: String);
begin
  if Value <> FTimePMString then
  begin
    FTimePMString := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetTimeSeparator(const Value: Char);
begin
  if Value <> FTimeSeparator then
  begin
    FTimeSeparator := Value;
    Changed;
  end;
end;

procedure TXCalFormatSettings.SetLongDayNames(const Value: TXCalStringList);
begin
  FLongDayNames.Assign(Value);
end;

procedure TXCalFormatSettings.SetLongMonthNames(const Value: TXCalStringList);
begin
  FLongMonthNames.Assign(Value);
end;

procedure TXCalFormatSettings.SetShortDayNames(const Value: TXCalStringList);
begin
  FShortDayNames.Assign(Value);
end;

procedure TXCalFormatSettings.SetShortMonthNames(const Value: TXCalStringList);
begin
  FShortMonthNames.Assign(Value);
end;

procedure TXCalFormatSettings.SetTwoDigitYearCenturyWindow(const Value: Word);
begin
  if Value <> FTwoDigitYearCenturyWindow then
  begin
    FTwoDigitYearCenturyWindow := Value;
    Changed;
  end;
end;

function TXCalFormatSettings.IsPropertyStored(Index: Integer): Boolean;
var
  XCal: TXCalendar;
begin
  Result := True;
  XCal := TXCalendar(FOwner.NewInstance);
  XCal.Create(nil);
  try
    case Index of
       1: Result := (XCal.FormatSettings.FDecimalSeparator <> FDecimalSeparator);
       2: Result := (XCal.FormatSettings.FDateSeparator <> FDateSeparator);
       3: Result := (XCal.FormatSettings.FTimeSeparator <> FTimeSeparator);
       4: Result := (XCal.FormatSettings.FShortDateFormat <> FShortDateFormat);
       5: Result := (XCal.FormatSettings.FLongDateFormat <> FLongDateFormat);
       6: Result := (XCal.FormatSettings.FTimeAMString <> FTimeAMString);
       7: Result := (XCal.FormatSettings.FTimePMString <> FTimePMString);
       8: Result := (XCal.FormatSettings.FShortTimeFormat <> FShortTimeFormat);
       9: Result := (XCal.FormatSettings.FLongTimeFormat <> FLongTimeFormat);
      10: Result := (XCal.FormatSettings.FShortMonthNames.Text <> FShortMonthNames.Text);
      11: Result := (XCal.FormatSettings.FLongMonthNames.Text <> FLongMonthNames.Text);
      12: Result := (XCal.FormatSettings.FShortDayNames.Text <> FShortDayNames.Text);
      13: Result := (XCal.FormatSettings.FLongDayNames.Text <> FLongDayNames.Text);
      14: Result := (XCal.FormatSettings.FTwoDigitYearCenturyWindow <> FTwoDigitYearCenturyWindow);
    end;
  finally
    XCal.Free;
  end;
end;

function TXCalFormatSettings.IsDecimalSeparatorStored: Boolean;
begin
  Result := IsPropertyStored(1);
end;

function TXCalFormatSettings.IsDateSeparatorStored: Boolean;
begin
  Result := IsPropertyStored(2);
end;

function TXCalFormatSettings.IsTimeSeparatorStored: Boolean;
begin
  Result := IsPropertyStored(3);
end;

function TXCalFormatSettings.IsShortDateFormatStored: Boolean;
begin
  Result := IsPropertyStored(4);
end;

function TXCalFormatSettings.IsLongDateFormatStored: Boolean;
begin
  Result := IsPropertyStored(5);
end;

function TXCalFormatSettings.IsTimeAMStringStored: Boolean;
begin
  Result := IsPropertyStored(6);
end;

function TXCalFormatSettings.IsTimePMStringStored: Boolean;
begin
  Result := IsPropertyStored(7);
end;

function TXCalFormatSettings.IsShortTimeFormatStored: Boolean;
begin
  Result := IsPropertyStored(8);
end;

function TXCalFormatSettings.IsLongTimeFormatStored: Boolean;
begin
  Result := IsPropertyStored(9);
end;

function TXCalFormatSettings.IsShortMonthNamesStored: Boolean;
begin
  Result := IsPropertyStored(10);
end;

function TXCalFormatSettings.IsLongMonthNamesStored: Boolean;
begin
  Result := IsPropertyStored(11);
end;

function TXCalFormatSettings.IsShortDayNamesStored: Boolean;
begin
  Result := IsPropertyStored(12);
end;

function TXCalFormatSettings.IsLongDayNamesStored: Boolean;
begin
  Result := IsPropertyStored(13);
end;

function TXCalFormatSettings.IsTwoDigitYearCenturyWindowStored: Boolean;
begin
  Result := IsPropertyStored(14);
end;

end.
