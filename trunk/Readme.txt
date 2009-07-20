
    XCalendar - General Calendar Utilities for Delphi
    by adgteq

    Project website: http://xcalendar.sourceforge.net
    Author's e-mail: adgteq@yahoo.co.uk


    
INTRODUCTION
============
XCalendar is a collection of calendar utilities for Delphi.

As you know the Delphi TDateTime type stores date/times as a floating point
value representing the time passed from a specific time, or more precisely:
The integral part of a Delphi TDateTime value is the number of days that
have passed since 12/30/1899. The fractional part of the TDateTime value is
fraction of a 24 hour day that has elapsed.
A calendar divides time into units, such as weeks, months, and years. The
number, length, and start of the divisions vary in each calendar.
So any TDateTime value can be decoded to units of an arbitrary calendar and
values expressed in units of a calendar can be encoded to TDateTime values.

XCalendar contains calendars, database date/time field types, tools to find
calendar events and some astronomical stuff.

For installation instructions see Install.txt



1. Calendars
============
Calendars like Gregorian calendar, Persian calendar, etc are derived from
the base class TXCalendar which is implemented as a descendant of TComponent
(to be put on forms for example) and presents methods named identically as
Delphi Date/Time routines of SysUtils and DateUtils which provie the same
functionalities generalized for arbitrary calendars.
The derived calendars implement TXCalendar's abstract methods to encode/
decode between year/month/day and TDateTime, etc.

Please note:
  - All the day of week values are compliant with SysUtils DayOfWeek:
      const
        xcalSunday = 1;
        xcalMonday = 2;
        xcalTuesday = 3;
        xcalWednesday = 4;
        xcalThursday = 5;
        xcalFriday = 6;
        xcalSaturday = 7;

  - There is a WeekRule property that determines the rule for counting
    weeks:

      wrFullWeek: The first week of the year/month is the one having
        all it's days inside the year/month.
      wrFourDayWeek: The first week of the year/month is the one having
        at least 4 of it's days inside the year/month.
      wrOneDayWeek: The first week of the year/month is the one including
        the first day of the year/month.

    Setting WeekRule to wrFourDayWeek makes the methods act like DateUtils
    routines that use the ISO 8601 week rule.
    
  - Start of the week is set by the WeekStartDay property.

  - Format settings such as month names, etc are set by the
    FormatSettings property.

  - Eras are not implemented yet.
  
  - No unicode support yet.

Examples:

  dt := GregoianCalendar1.EncodeDate(2000, 1, 1);
  w := HijriCalendar1.WeekOfTheYear(dt);
  ShowMessage(
    PersianCalendar1.FormatDateTime('dddd d mmmm yyyy hh:nn AM/PM', Now));


Special methods:

  procedure SaveSettingsToStream(Stream: TStream);
  procedure LoadSettingsFromStream(Stream: TStream);
  procedure SaveSettingsToFile(const FileName: string);
  procedure LoadSettingsFromFile(const FileName: string);
  procedure LoadSettingsFromResourceName(Instance: THandle; const ResName: string);
  {
  These methods are used to save/load the settings of the calendar (such as
  format settings, week rule, etc) to/from files/streams in XML format.
  (RTTI is not uses yet)
  You can also save/load the component using Delphi TStream/TWriter/TReader
  utilities.
  }

  procedure AddNotify(NotifyEvent: TNotifyEvent);
  procedure RemoveNotify(NotifyEvent: TNotifyEvent);
  {
  Setting-change notifying. These methods act like an OnChange event, but
  alow for more than one object to register interest in change notification.
  These can be used in components using XCalendar calendars.
  }

  
Date/time methods in addition to / differing from SysUtils/DateUtils routines:

  function DayOfWeek(const AYear, AMonth, ADay: Word): Word; overload;
  { Equals to DayOfWeek(EncodeDate(AYear, AMonth, ADay)) }

  function MonthsInYear(const AValue: TDateTime): Word;
  { Equals to MonthsInAYear(YearOf(AValue)) }

  function MonthsInAYear(const AYear: Word): Word; virtual;
  {
  Returns the number of months in the specified year. Normally it returns 12,
  but lunisolar calendars can override this.
  }

  function WeeksInMonth(const AValue: TDateTime): Word;
  { Equals to WeeksInAMonth(YearOf(AValue), MonthOf(AValue)) }

  function WeeksInAMonth(const AYear, AMonth: Word): Word;
  {
  Returns the number of weeks in the specified month of the specified year.
  }

  procedure IncAYear(var Year, Month, Day: Word; NumberOfYears: Integer = 1);
  {
  Increments date data by NumberOfYears years
  }

  function EncodeDayOfWeekInMonth(const AYear, AMonth: Word;
    const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
  {
  Acts like DateUtils.EncodeDayOfWeekInMonth, with the improvement that
  if ANthDayOfWeek is negative, counting is started from the end of the
  month downwards. e.g. EncodeDayOfWeekInMonth(Y, M, -1, xcalSunday)
  returns the last Sunday of the month.

  TryEncodeDayOfWeekInMonth acts similarly
  }

  function EncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
    const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word): TDateTime;
  {
  If ANthDayOfWeek is positive, returns the Nth date having the day of
  week ADayOfWeek after or equal AFromDate;
  If ANthDayOfWeek is negative, returns the Nth date having the day of
  week ADayOfWeek before or equal AFromDate.
  (N = ANthDayOfWeek)
  }

  function TryEncodeDayOfWeekAfterADate(const AFromDate: TDateTime;
    const ANthDayOfWeek: SmallInt; const ADayOfWeek: Word; out AValue: TDateTime): Boolean;
  { Similar to the above function, but doesn't raise an exception }

  function OffsetToDayOfWeek(const AOffset: Word): Word;
  {
  Returns the DayOfWeek value corresponding to the offset AOffset from
  the first day of week (WeekStartDay). Normal values for AOffset are 0..6
  }
  
  function DayOfWeekToOffset(const ADayOfWeek: Word): Word;
  {
  Returns the offset from the first day of week (WeekStartDay) to ADayOfWeek.
  Result value is in 0..6
  }


Routines available in the xcalClass unit:

  function DateTimeToAlgebraicDateTime(const AValue: TDateTime): Double;
  function AlgebraicDateTimeToDateTime(const AValue: Double): TDateTime;
  {
  Algebraic DateTime is a TDateTime value that can be used in additions or
  subtractions without respect to its sign.
  e.g. the TDateTime value for 12/29/1899 6:00AM is -1.25 but it's
  algebraic DateTime is -0.75

  These functions convert between DateTimes and algebraic DateTimes.
  }

  function DateTimeToJulianDay(const AValue: TDateTime): Double;
  function JulianDayToDateTime(const AValue: Double): TDateTime;
  function TryJulianDayToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;
  function DateTimeToModifiedJulianDay(const AValue: TDateTime): Double;
  function ModifiedJulianDayToDateTime(const AValue: Double): TDateTime;
  function TryModifiedJulianDayToDateTime(const AValue: Double;
    out ADateTime: TDateTime): Boolean;
  {
  These functions act like their DateUtils counterparts (with the naming
  change of JulianDate -> JulianDay) operating faster.
  }



The calendars currently implemented are:

  1.1. TGregorianCalendar
  =======================
    TGregorianCalendar is just a wrapper for the already-available functions
      of SysUtils.

  1.2. TPersianCalendar
  =====================
    The Persian calendar using Jalali 33-year cycles to determine common and
      leap years.

  1.3. TAstroPersianCalendar
  ==========================
    The Persian calendar using the astronomical rule to determine common
      and leap years: If the vernal equinox occurs before true solar noon,
      then the day becomes the first day of the new year, but if it happens
      after the noon, then the next day becomes the first day of the new year.

    Use the Longitude property to set the longitude in degrees to calculate
      the noon time for.
      
    The CacheSize property sets the size of the cache of calculated years.

  1.4. THijriCalendar
  ===================
    The Hijri (Islamic lunar) calendar
    
    The LeapYearsKind property determines the kind of leap year calculation:

      lyk16: Years 2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29 in each 30-year
        cycle are leap years
      lyk15: Years 2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29 in each 30-year
        cycle are leap years

    The value of HijriAdjustment property (positive or negative) is added to
      the calculated dates to accommodate the variances in the start and the
      end of Ramadan and to accommodate the date difference between
      countries/regions.

    You can use the OnGetDaysInAMonth event to provide individual month
      lengths for the calendar according to the year. Please note that for
      each year, the length of the months should sum to either 354 (for
      common years) or 355 (for leap years). Otherwise the calendar might
      malfunction. If you need to adjust years also, use TObservedHijriCalendar.

  1.5. TObservedHijriCalendar
  ===========================
    The Hijri calendar with additional supplements that you can use to provide
      observational data to adjust years/months.

    It works by adjusting the conventional years to observed years. You should
      give a base adjustment by the BaseYear and BaseYearAdjustment properties
      and provide the observed common and leap years in a range specified by
      the MinObservedYear and MaxObservedYear properties by assigning the
      OnFindLeapYears event. And, of course, you should also assign the
      OnGetDaysInAMonth event to provide the observed month lengths (see
      THijriCalendar). 

    BaseYear: The base year for calculating year adjustments

    BaseYearAdjustment: Specifies the offset of the conventional year start,
      relative to the observed year start for the year specified by BaseYear.
      For example if for the base year, the first day of the conventional
      year is calculated to occur on March 15, but the observed new moon makes
      the year start on March 16, this adjustment would be -1.

    MinObservedYear: Minimum year that you have observational information for
    
    MaxObservedYear: Maximum year that you have observational information for

    OnFindLeapYears: You should use this event to specify the observed common
      and leap years. This event is only called for the years between
      MinObservedYear and MaxObservedYear inclusive, and your code should
      specify every common and leap year in the range by setting the IsLeap
      parameter to False or True respectively. Years outside the range are
      automatically adjusted until they coincide with the conventional years.



2. Database Date/Time Field Types
=================================
XcalDateTimeField, XcalDateField, XCalSQLTimeStampField are available from
the New Field dialog box to allow for data-aware controls to set/get the
date/time fields as strings in the calendar specified by the XCalendar
property of the field.

In order to use these field types, after adding all the fields in the
Fields Editor, remove the date/time fields and right click in the Fields
Editor window and choose "New Field...". In the Name box, type the actual
name of the desired date/time field. Choose XcalDateTimeField (or the
appropriate type of field) from the Type ComboBox. Choose Data for the
Field type and click OK.

Now You can set the XCalendar property of the created field component to a
calendar component you have put on your form/datamodule and see the results
in your data-aware controls.



3. Calendar Events
==================
XCalendar event components are derived from TCustomXCalendarEvents which
presents methods to find the occurences of the events implemented in
descendant components between two times:

  procedure FindIntervalEvents(const FromDate, ToDate: TDateTime;
    var EOL: TXCalEventOccurenceList); virtual; abstract;
  {
  Finds the occurences of the events represented by the event component
  from FromDate to ToDate inclusive and puts them into EOL.
  EOL should be an instance of TXCalEventOccurenceList.
  Note: EOL is not cleared by the method.

  TXCalEventOccurenceList is a TList descendant containing
  TXCalEventOccurence objects:

  TXCalEventOccurence = class(TObject)
    Date: TDateTime;
    DisplayText: string;
    IsVacation: Boolean;
    Obj: TObject;
  end;

  Date is the occurence date (might include time).
  DisplayText is the title to be displayed for this occurence.
  IsVacation tells if this event occurence makes a day off.
  Obj is as arbitrary object. For TXCalendarEvents, it points to the
    TXCalEventItem that this event occurence belongs to.

  Useful methods in TXCalEventOccurenceList:

    function Add(const Date: TDateTime; const DisplayText: string;
      const IsVacation: Boolean; const Obj: TObject): Integer;
    { Adds a new item having the provided values }

    function IndexOf(ADate: TDateTime; StartIndex: Integer = 0): Integer;
    {
    Searches the list for the first item having the same date as ADate,
    starting from StartIndex.
    If it finds one returns the index of the found item, otherwise
    returns -1.
    }

    procedure SortByDate;
    {
    Sorts the items according to their occurence dates in ascending order.
    For two occurences on the same date, if one is a vacation, it is put first.
    }

  Code example:

    var
      EOL: TXCalEventOccurenceList;
    ...
      EOL := TXCalEventOccurenceList.Create;
      try
        XCalendarEvents1.FindIntervalEvents(..., ..., EOL);
        EOL.SortByDate;
        ...
      finally
        EOL.Free;
      end;
  }

  procedure FindDateEvents(const ADate: TDateTime; var EOL: TXCalEventOccurenceList);
  { Equals to FindIntervalEvents(ADate, ADate, EOL) }



These event components are implemented:

  3.1. XCalendarEvents
  ====================
    Used for calculating the occurences of general annual events.

    The Events property is a Collection of TXCalEventItem items each
    representing a single annual event. The properties of the items that you
    should set are as follows:

      1. XCalendar: The calendar that this event is calculated in as a
        calendar component.

      2. RuleType: Type of the rule to calculate the event. These types of
        events are supported:

          rtExactDay: Exact day of a month in each year.
          rtNthDayOfWeekInMonth: Rules like 1st Sunday in February.
          rtNthDayOfWeekAfterADay: Rules like 1st Wednesday before March 19.
          rtNearestDayOfWeek: Rules like the nearest Monday to March 19.
          rtCustomRule: Custom Rule defined in the OnCustomRule event of
            the related XCalendarEvents component.
            
      3. The properties related to the type of the rule, namely:

          for rtExactDay: Month, Day
          for rtNthDayOfWeekInMonth: Nth, DayOfWeek, Month
            Note: Negative values for Nth mean counting from the end of the
              month. e.g. -1 means "the last ..."
          for rtNthDayOfWeekAfterADay: Nth, DayOfWeek, Month, Day
            Note: Negative values for Nth mean searching for Nth day of week
              before a day
          for rtNearestDayOfWeek: DayOfWeek, Month, Day
          for rtCustomRule: Don't forget to implement the OnCustomRule event
            of the related XCalendarEvents component.

      4.(Optional) Corrections: You can use this if some day offset should
        be added to the calculated date or if the event must not occur on
        some day(s) of week.

      5. Title: A title for the event.

      6. FormatTitle: Set to True if the Title includes date/time format
         strings to be formatted according to the occurence date.
      
      7. IsVacation: Set to True if this event makes a day off.


  Special methods:
  
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    {
    These methods are used to save/load the events to/from files/streams in
    XML format. (RTTI is not uses yet)
    Please note: If you want to use these methods, all the XCalendars used
    in the events should have the same owner specified by the XCalendarsOwner
    property of the XCalendarEvents object.
    You can also save/load the component using Delphi TStream/TWriter/TReader
    utilities which doesn't have this limitation.
    }



  3.2. XCalendarAstroEvents
  =========================
    Used for calculating the occurences of some astronomical events, namely,
    equinoxes, moon phases and eclipses. You can set the titles for each
    event.

    The TimeZone property should be set (in hours) to determine on which
    day the event occurs.

    If the TimeFormat property is not empty, the time of the events (in the
    specified TimeZone) are added to the titles in the specified format.


  3.3. XCalendarAggregateEvents
  =============================
    You can use this component to gather other event components into a group.
    The occurences of the contained event components are accumulated to make
    the occurences of this event component. 



4. Astronomical Stuff
=====================
XCalendar contains a Delphi port of some part of the AA+ project (v1.30) by
PJ Naughter (http://www.naughter.com) for use in it's astronomical
calculations.
the related units are characterized by their name starting with "AA".
You can use them if you know how to.






LICENSE INFORMATION
===================
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


ACKNOWLEDGEMENTS
================
My special thanks go to:

Borland/CodeGear developers for their vcl sources
Microsoft .net developers for the nice calendar idea
PJ Naughter (http://www.naughter.com) for his nice AA+ library
NoneForce (http://forum.p30world.com) for many nice source codes
Amir Rahimi F. (http://www.farsicomponents.com) for the nice database fields idea
and to everyone that helps in improving this library

adgteq
Aug-2008