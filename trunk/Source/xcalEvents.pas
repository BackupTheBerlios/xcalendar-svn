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

unit xcalEvents;

{$I xcalDefs.inc}

interface

uses
  Classes, SysUtils, xcalClass, xcalXML;

type

  TXCalEventOccurence = class(TObject)
    Date: TDateTime;
    DisplayText: string;
    IsVacation: Boolean;
    Obj: TObject; { Arbitrary object; For TXCalendarEvents, points to the TXCalEventItem that this event occurence belongs to }
  end;

  TXCalEventOccurenceList = class(TList)
  private
    function Get(Index: Integer): TXCalEventOccurence;
    procedure Put(Index: Integer; const Value: TXCalEventOccurence);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    { Adds a new item having the provided values }
    function Add(const Date: TDateTime; const DisplayText: string;
      const IsVacation: Boolean; const Obj: TObject): Integer;
    { Searches the list for the first item having the same date as ADate, starting from StartIndex.
      If it finds one returns the index of the found item, otherwise returns -1 }
    function IndexOf(ADate: TDateTime; StartIndex: Integer = 0): Integer;
    function First: TXCalEventOccurence;
    function Last: TXCalEventOccurence;
    property Items[Index: Integer]: TXCalEventOccurence read Get write Put; default;
    { Sorts the items according to their occurence dates in ascending order.
      For two occurences on the same date, if one is a vacation, it is put first. }
    procedure SortByDate;
  end;

  TCustomXCalendarEvents = class(TComponent)
  public
    { Note: The methods don't clear the input list }
    procedure FindIntervalEvents(const FromDate, ToDate: TDateTime;
      var EOL: TXCalEventOccurenceList); virtual; abstract;
    procedure FindDateEvents(const ADate: TDateTime; var EOL: TXCalEventOccurenceList);
  end;







{ TXCalendarEvents }

  TDayOfWeekCorrectionType = (dcNoCorrection,
    dcToNextSunday, dcToNextMonday, dcToNextTuesday, dcToNextWednesday, dcToNextThursday, dcToNextFriday, dcToNextSaturday,
    dcToPrevSunday, dcToPrevMonday, dcToPrevTuesday, dcToPrevWednesday, dcToPrevThursday, dcToPrevFriday, dcToPrevSaturday);

  TDayOfWeekCorrections = class(TPersistent)
  private
    FIfSunday: TDayOfWeekCorrectionType;
    FIfMonday: TDayOfWeekCorrectionType;
    FIfTuesday: TDayOfWeekCorrectionType;
    FIfWednesday: TDayOfWeekCorrectionType;
    FIfThursday: TDayOfWeekCorrectionType;
    FIfFriday: TDayOfWeekCorrectionType;
    FIfSaturday: TDayOfWeekCorrectionType;
  public
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
  published
    property IfSunday: TDayOfWeekCorrectionType read FIfSunday write FIfSunday default dcNoCorrection;
    property IfMonday: TDayOfWeekCorrectionType read FIfMonday write FIfMonday default dcNoCorrection;
    property IfTuesday: TDayOfWeekCorrectionType read FIfTuesday write FIfTuesday default dcNoCorrection;
    property IfWednesday: TDayOfWeekCorrectionType read FIfWednesday write FIfWednesday default dcNoCorrection;
    property IfThursday: TDayOfWeekCorrectionType read FIfThursday write FIfThursday default dcNoCorrection;
    property IfFriday: TDayOfWeekCorrectionType read FIfFriday write FIfFriday default dcNoCorrection;
    property IfSaturday: TDayOfWeekCorrectionType read FIfSaturday write FIfSaturday default dcNoCorrection;
  end;

  TXCalEventCorrections = class(TPersistent)
  private
    FDayCorrection: SmallInt; { Offset to be added to the calculated date }
    FDayOfWeekCorrection: TDayOfWeekCorrections; { Corrections to be made according to the day-of-week of the calculated date }

    procedure SetDayOfWeekCorrection(const Value: TDayOfWeekCorrections);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
  published
    property DayCorrection: SmallInt read FDayCorrection write FDayCorrection default 0;
    property DayOfWeekCorrection: TDayOfWeekCorrections read FDayOfWeekCorrection write SetDayOfWeekCorrection;
  end;

  PXCalEventOccurenceCacheItem = ^TXCalEventOccurenceCacheItem;
  TXCalEventOccurenceCacheItem = record
    Year: Word;
    Occurs: Boolean;
    OccurenceDate: TDateTime;
    DisplayText: string;
    IsVacation: Boolean;
  end;

  TXCalEventOccurenceCache = class(TList)
  private
    FSize: Integer;
    procedure SetSize(const Value: Integer);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(const AYear: Word; const AOccurs: Boolean; const AOccurenceDate: TDateTime;
      const ADisplayText: string; const AIsVacation: Boolean): Integer;
    function FindValue(const AYear: Word; out Occurs: Boolean;
      out OccurenceDate: TDateTime; out DisplayText: string; out IsVacation: Boolean): Integer;
    property Size: Integer read FSize write SetSize;
  end;

  TXCalEventRuleType = (rtExactDay, rtNthDayOfWeekInMonth, rtNthDayOfWeekAfterADay, rtNearestDayOfWeek, rtCustomRule);

  TXCalEventItem = class;

  TXCalEventCustomRuleProc = procedure(Sender: TXCalEventItem; AYear: Word;
    var OccurenceDate: TDateTime; var Occurs: Boolean) of object;

  TXCalEventOccurEvent = procedure(Sender: TXCalEventItem; YearFoundFor: Word;
    var OccurenceDate: TDateTime; var DisplayText: string; var IsVacation: Boolean;
    var AllowOccurence: Boolean) of Object;

  TXCalEventItem = class(TCollectionItem)
  private
    FXCalendar: TXCalendar; { Calendar }
    FRuleType: TXCalEventRuleType;
    FDay, FMonth: Word;                           
    FDayOfWeek: Word;
    FNth: SmallInt; { used as N in Nth day of week after/before some day }
    FTitle: string; { event title }
    FFormatTitle: Boolean; { indicates if the title should be passed to FormatDateTime }
    FIsVacation: Boolean; { True for vacations }
    FTag: Integer; { Custom Tag }

    FCorrections: TXCalEventCorrections; { Correction Rules }
    //If adding fields, update: Assign and SaveToXMLItem/LoadFromXMLItem

    FCache: TXCalEventOccurenceCache;

    { functions to determines if there are correction rules }
    function DayCorrectionExists: Boolean;
    function DayOfWeekCorrectionExists: Boolean;
    function CorrectionRulesExist: Boolean;

    function GetCacheSize: Integer;
    procedure SetCacheSize(const Value: Integer);
    procedure SetCorrections(const Value: TXCalEventCorrections);
  protected
    function GetDisplayName: string; override;
    procedure LoadDefaults;
    { Makes the corrections on ADate according to the correction rules of the event. }
    procedure DoCorrections(var ADate: TDateTime);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { Sets OccurenceDate to the occurence of this event for the year AYear in the specified calendar.
      Returns False if no occurence for the year. }
    function FindOccurenceForAYear(const AYear: Word; out OccurenceDate: TDateTime;
      out DisplayText: string; out IsVacation: Boolean): Boolean;

    { Finds occurences of this event from FromDate to ToDate inclusive. }
    { Note: Doesn't clear OccurenceList }
    procedure FindOccurencesInAnInterval(const FromDate, ToDate: TDateTime;
      var OccurenceList: TXCalEventOccurenceList);

    { Returns True if this event occurs on the date ADate.
      Sets DisplayText to the Text to be displayed as the event title. }
    function OccursOn(const ADate: TDateTime; out DisplayText: string): Boolean;

    procedure SaveToXMLItem(XMLItem: TxcalXMLItem);
    procedure LoadFromXMLItem(XMLItem: TxcalXMLItem);

    property CacheSize: Integer read GetCacheSize write SetCacheSize;
  published
    property XCalendar: TXCalendar read FXCalendar write FXCalendar;
    property RuleType: TXCalEventRuleType read FRuleType write FRuleType default rtExactDay;
    property Day: Word read FDay write FDay default 0;
    property Month: Word read FMonth write FMonth default 0;
    property DayOfWeek: Word read FDayOfWeek write FDayOfWeek default 0;
    property Nth: SmallInt read FNth write FNth default 0;
    property Title: string read FTitle write FTitle;
    property FormatTitle: Boolean read FFormatTitle write FFormatTitle default False;
    property Corrections: TXCalEventCorrections read FCorrections write SetCorrections;
    property IsVacation: Boolean read FIsVacation write FIsVacation default False;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TXCalEventCollection = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TXCalEventItem;
    procedure SetItem(Index: Integer; const Value: TXCalEventItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TXCalEventItem;
    function Insert(Index: Integer): TXCalEventItem;
    property Items[Index: Integer]: TXCalEventItem read GetItem write SetItem; default;
{$IFNDEF Delphi6}
    function Owner: TPersistent;
{$ENDIF}
  end;


  TXCalendarEvents = class(TCustomXCalendarEvents)
  private
    FVersion: string;
    FEvents: TXCalEventCollection;
    FOnCustomRule: TXCalEventCustomRuleProc;
    FOnOccur: TXCalEventOccurEvent;
    FXCalendarsOwner: TComponent;
    procedure SetEvents(const Value: TXCalEventCollection);
    function IsXCalendarsOwnerStored: Boolean;
    procedure SetVersion(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FindIntervalEvents(const FromDate, ToDate: TDateTime;
      var EOL: TXCalEventOccurenceList); override;

    procedure SaveToXMLItemChilds(XMLItem: TxcalXMLItem);
    procedure LoadFromXMLItemChilds(XMLItem: TxcalXMLItem;
      StartIndex: Integer = 0; Count: Integer = 0);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  published
    property Version: string read FVersion write SetVersion stored False;
    property Events: TXCalEventCollection read FEvents write SetEvents;

    { To store the XCalendar property of the events in xml in a simple way, all
      the XCalendars used in the events should have the same owner set here. }
    property XCalendarsOwner: TComponent read FXCalendarsOwner write FXCalendarsOwner stored IsXCalendarsOwnerStored; 

    property OnCustomRule: TXCalEventCustomRuleProc read FOnCustomRule write FOnCustomRule;
    property OnOccur: TXCalEventOccurEvent read FOnOccur write FOnOccur;
  end;






{ TXCalendarAggregateEvents }

  TXCalAggregateEventsCollectionItem = class(TCollectionItem)
  private
    FXCalendarEvents: TCustomXCalendarEvents;
    procedure SetXCalendarEvents(const Value: TCustomXCalendarEvents);
  protected
    function GetDisplayName: string; override;
  published
    property XCalendarEvents: TCustomXCalendarEvents read FXCalendarEvents write SetXCalendarEvents;
  end;

  TXCalAggregateEventsCollection = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TXCalAggregateEventsCollectionItem;
    procedure SetItem(Index: Integer;
      const Value: TXCalAggregateEventsCollectionItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TXCalAggregateEventsCollectionItem;
    function Insert(Index: Integer): TXCalAggregateEventsCollectionItem;
    property Items[Index: Integer]: TXCalAggregateEventsCollectionItem read GetItem write SetItem; default;
  end;

  TXCalendarAggregateEvents = class(TCustomXCalendarEvents)
  private
    FObjects: TXCalAggregateEventsCollection;
    procedure SetObjects(const Value: TXCalAggregateEventsCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FindIntervalEvents(const FromDate, ToDate: TDateTime;
      var OccurenceList: TXCalEventOccurenceList); override;
  published
    property Objects: TXCalAggregateEventsCollection read FObjects write SetObjects;
  end;


var

{ XML strings }

  XCalendarEventsXMLRootName: string = 'XCalendarEvents';
  XCalendarEventsXMLEventName: string = 'Event';




implementation

{$IFNDEF Delphi6}
uses
  xcalD5Utils;
{$ENDIF}

{ TXCalEventItem }

procedure TXCalEventItem.Assign(Source: TPersistent);
begin
  if (Source is TXCalEventItem) then
  begin
    FXCalendar := TXCalEventItem(Source).FXCalendar;
    FRuleType := TXCalEventItem(Source).FRuleType;
    FDay := TXCalEventItem(Source).FDay;
    FMonth := TXCalEventItem(Source).FMonth;
    FDayOfWeek := TXCalEventItem(Source).FDayOfWeek;
    FNth := TXCalEventItem(Source).FNth;
    FTitle := TXCalEventItem(Source).FTitle;
    FFormatTitle := TXCalEventItem(Source).FFormatTitle;
    FCorrections.Assign(TXCalEventItem(Source).FCorrections);
    FIsVacation := TXCalEventItem(Source).FIsVacation;
    FTag := TXCalEventItem(Source).FTag;
  end
  else
    inherited Assign(Source);
end;

function TXCalEventItem.GetDisplayName: string;
begin
  Result := FTitle;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TXCalEventItem.SaveToXMLItem(XMLItem: TxcalXMLItem);
begin

  with XMLItem do
  begin
    if FTitle <> '' then
      Prop['Title'] := AnsiToUtf8(FTitle);
    if FFormatTitle then
      Prop['FormatTitle'] := '1';
    if FIsVacation then
      Prop['IsVacation'] := '1';
    if FTag <> 0 then
      Prop['Tag'] := IntToStr(FTag);

    if Assigned(FXCalendar) then
      Prop['XCalendar'] := FXCalendar.Name;
    if FRuleType <> rtExactDay then
      Prop['RuleType'] := IntToStr(Ord(FRuleType));
    if FDay <> 0 then
      Prop['Day'] := IntToStr(FDay);
    if FMonth <> 0 then
      Prop['Month'] := IntToStr(FMonth);
    if FDayOfWeek <> 0 then
      Prop['DayOfWeek'] := IntToStr(FDayOfWeek);
    if FNth <> 0 then
      Prop['Nth'] := IntToStr(FNth);

    if FCorrections.DayCorrection <> 0 then
      Prop['Corrections.DayCorrection'] := IntToStr(FCorrections.DayCorrection);

    if FCorrections.DayOfWeekCorrection.IfSunday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfSunday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfSunday));
    if FCorrections.DayOfWeekCorrection.IfMonday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfMonday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfMonday));
    if FCorrections.DayOfWeekCorrection.IfTuesday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfTuesday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfTuesday));
    if FCorrections.DayOfWeekCorrection.IfWednesday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfWednesday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfWednesday));
    if FCorrections.DayOfWeekCorrection.IfThursday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfThursday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfThursday));
    if FCorrections.DayOfWeekCorrection.IfFriday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfFriday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfFriday));
    if FCorrections.DayOfWeekCorrection.IfSaturday <> dcNoCorrection then
      Prop['Corrections.DayOfWeekCorrection.IfSaturday'] := IntToStr(Ord(FCorrections.DayOfWeekCorrection.IfSaturday));
  end;
end;

procedure TXCalEventItem.LoadFromXMLItem(XMLItem: TxcalXMLItem);
var
  N: Integer;
  XCalendarsOwner, Comp: TComponent;
begin
  LoadDefaults;

  with XMLItem do
  begin
    if PropExists('Title') then
      FTitle := Utf8ToAnsi(Prop['Title']);
    if PropExists('FormatTitle') and TryStrToInt(Prop['FormatTitle'], N) then
      FFormatTitle := (N = 1);
    if PropExists('IsVacation') and TryStrToInt(Prop['IsVacation'], N) then
      FIsVacation := (N = 1);
    if PropExists('Tag') and TryStrToInt(Prop['Tag'], N) then
      FTag := N;

    if PropExists('XCalendar') then
    begin
{$IFDEF Delphi6}
      if (Collection.Owner is TXCalendarEvents) then
      begin
        XCalendarsOwner := TXCalendarEvents(Collection.Owner).FXCalendarsOwner;
{$ELSE}
      if (Collection is TXCalEventCollection) and
         (TXCalEventCollection(Collection).Owner is TXCalendarEvents) then
      begin
        XCalendarsOwner := TXCalendarEvents(TXCalEventCollection(Collection).Owner).FXCalendarsOwner;
{$ENDIF}
        if Assigned(XCalendarsOwner) then
        begin
          Comp := XCalendarsOwner.FindComponent(Prop['XCalendar']);
          if (Comp is TXCalendar) then
            FXCalendar := TXCalendar(Comp);
        end;
      end;
    end;
    if PropExists('RuleType') and TryStrToInt(Prop['RuleType'], N) and
      (Ord(rtExactDay) <= N) and (N <= Ord(rtCustomRule)) then
      FRuleType := TXCalEventRuleType(N);
    if PropExists('Day') and TryStrToInt(Prop['Day'], N) and (N >= 0) then
      FDay := N;
    if PropExists('Month') and TryStrToInt(Prop['Month'], N) and (N >= 0) then
      FMonth := N;
    if PropExists('DayOfWeek') and TryStrToInt(Prop['DayOfWeek'], N) and (N >= 0) then
      FDayOfWeek := N;
    if PropExists('Nth') and TryStrToInt(Prop['Nth'], N) then
      FNth := N;

    if PropExists('Corrections.DayCorrection') and TryStrToInt(Prop['Corrections.DayCorrection'], N) then
      FCorrections.DayCorrection := N;

    if PropExists('Corrections.DayOfWeekCorrection.IfSunday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfSunday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfSunday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfMonday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfMonday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfMonday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfTuesday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfTuesday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfTuesday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfWednesday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfWednesday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfWednesday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfThursday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfThursday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfThursday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfFriday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfFriday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfFriday := TDayOfWeekCorrectionType(N);
    if PropExists('Corrections.DayOfWeekCorrection.IfSaturday') and
       TryStrToInt(Prop['Corrections.DayOfWeekCorrection.IfSaturday'], N) and
       (Ord(dcNoCorrection) <= N) and (N <= Ord(dcToPrevSaturday)) then
      FCorrections.DayOfWeekCorrection.IfSaturday := TDayOfWeekCorrectionType(N);
  end;
end;

procedure TXCalEventItem.DoCorrections(var ADate: TDateTime);

      procedure DoDayOfWeekCorrection(CorrectionType: TDayOfWeekCorrectionType);
      begin
        case CorrectionType of
          dcToNextSunday..dcToNextSaturday:
            ADate := FXCalendar.EncodeDayOfWeekAfterADate(ADate,  1, Ord(CorrectionType) - Ord(dcToNextSunday) + 1);

          dcToPrevSunday..dcToPrevSaturday:
            ADate := FXCalendar.EncodeDayOfWeekAfterADate(ADate, -1, Ord(CorrectionType) - Ord(dcToPrevSunday) + 1);
        end;
      end;

begin
  if DayCorrectionExists then
    ADate := ADate + FCorrections.DayCorrection;

  if DayOfWeekCorrectionExists and Assigned(FXCalendar) then
  begin
    case FXCalendar.DayOfWeek(ADate) of
      xcalSunday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfSunday);
      xcalMonday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfMonday);
      xcalTuesday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfTuesday);
      xcalWednesday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfWednesday);
      xcalThursday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfThursday);
      xcalFriday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfFriday);
      xcalSaturday: DoDayOfWeekCorrection(FCorrections.DayOfWeekCorrection.IfSaturday);
    end;
  end;
end;

function TXCalEventItem.DayCorrectionExists: Boolean;
begin
  Result := (FCorrections.DayCorrection <> 0);
end;

function TXCalEventItem.DayOfWeekCorrectionExists: Boolean;
begin
  Result :=
   (FCorrections.DayOfWeekCorrection.IfSunday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfMonday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfTuesday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfWednesday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfThursday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfFriday <> dcNoCorrection) or
   (FCorrections.DayOfWeekCorrection.IfSaturday <> dcNoCorrection);
end;

function TXCalEventItem.CorrectionRulesExist: Boolean;
begin
  Result := DayCorrectionExists or
            DayOfWeekCorrectionExists;
end;

constructor TXCalEventItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FCorrections := TXCalEventCorrections.Create;
  FCache := TXCalEventOccurenceCache.Create;
  FCache.Size := 3;
end;

destructor TXCalEventItem.Destroy;
begin
  FCorrections.Free;
  FCache.Free;
  
  inherited Destroy;
end;

function TXCalEventItem.GetCacheSize: Integer;
begin
  Result := FCache.Size;
end;

procedure TXCalEventItem.SetCacheSize(const Value: Integer);
begin
  FCache.Size := Value;
end;

procedure TXCalEventItem.SetCorrections(const Value: TXCalEventCorrections);
begin
  FCorrections.Assign(Value);
end;

procedure TXCalEventItem.LoadDefaults;
begin
  FTitle := '';
  IsVacation := False;
  Tag := 0;
  FXCalendar := nil;
  FRuleType := rtExactDay;
  FDay := 0;
  FMonth := 0;
  FDayOfWeek := 0;
  FNth := 0;
  FCorrections.Clear;
end;

procedure TXCalEventItem.FindOccurencesInAnInterval(const FromDate,
  ToDate: TDateTime; var OccurenceList: TXCalEventOccurenceList);
var
  Y, Y1, Y2: Word;
  MinDT, MaxDT, OccurenceDate: TDateTime;
  LDisplayText: string;
  LIsVacation: Boolean;
begin
  if not Assigned(OccurenceList) then
    Exit;

  if not Assigned(FXCalendar) then
    Exit;

  if (FRuleType = rtExactDay) and (not CorrectionRulesExist) then
  begin
    Y1 := FXCalendar.YearOf(FromDate);
    Y2 := FXCalendar.YearOf(ToDate);
  end
  else
  begin
    Y1 := FXCalendar.YearOf(FromDate) - 1;
    Y2 := FXCalendar.YearOf(ToDate) + 1;
  end;

  MinDT := FXCalendar.StartOfTheDay(FromDate);
  MaxDT := FXCalendar.EndOfTheDay(ToDate);

  for Y := Y1 to Y2 do
    if FindOccurenceForAYear(Y, OccurenceDate, LDisplayText, LIsVacation) and
       (MinDT <= OccurenceDate) and (OccurenceDate <= MaxDT) then
      OccurenceList.Add(OccurenceDate, LDisplayText, LIsVacation, Self);
end;

function TXCalEventItem.OccursOn(const ADate: TDateTime; out DisplayText: string): Boolean;
var
  OccurenceList: TXCalEventOccurenceList;
begin
  Result := False;
  OccurenceList := TXCalEventOccurenceList.Create;
  FindOccurencesInAnInterval(ADate, ADate, OccurenceList);
  if OccurenceList.Count > 0 then
  begin
    Result := True;
    DisplayText := OccurenceList[0].DisplayText;
  end;
end;

function TXCalEventItem.FindOccurenceForAYear(const AYear: Word;
  out OccurenceDate: TDateTime; out DisplayText: string; out IsVacation: Boolean): Boolean;
begin
  if not Assigned(FXCalendar) then
    Exit;

  if FCache.FindValue(AYear, Result, OccurenceDate, DisplayText, IsVacation) >= 0 then
    Exit;

  Result := False;
  case FRuleType of
    rtExactDay: Result := FXCalendar.TryEncodeDate(AYear, FMonth, FDay, OccurenceDate);
    rtNthDayOfWeekInMonth: Result := FXCalendar.TryEncodeDayOfWeekInMonth(AYear, FMonth, FNth, FDayOfWeek, OccurenceDate);
    rtNthDayOfWeekAfterADay:
      begin
        Result := FXCalendar.TryEncodeDate(AYear, FMonth, FDay, OccurenceDate);
        if Result then
          Result := FXCalendar.TryEncodeDayOfWeekAfterADate(OccurenceDate, FNth, FDayOfWeek, OccurenceDate);
      end;
    rtNearestDayOfWeek:
      begin
        Result := FXCalendar.TryEncodeDate(AYear, FMonth, FDay, OccurenceDate);
        if Result then
          Result := FXCalendar.TryEncodeNearestDayOfWeekToADate(OccurenceDate, FDayOfWeek, OccurenceDate);
      end;
    rtCustomRule: 
{$IFDEF Delphi6}
      if (Collection.Owner is TXCalendarEvents) and
          Assigned(TXCalendarEvents(Collection.Owner).FOnCustomRule) then
      begin
        Result := True;
        TXCalendarEvents(Collection.Owner).FOnCustomRule(Self, AYear, OccurenceDate, Result);
      end;
{$ELSE}
      if (Collection is TXCalEventCollection) and
         (TXCalEventCollection(Collection).Owner is TXCalendarEvents) and
          Assigned(TXCalendarEvents(TXCalEventCollection(Collection).Owner).FOnCustomRule) then
      begin
        Result := True;
        TXCalendarEvents(TXCalEventCollection(Collection).Owner).FOnCustomRule(Self, AYear, OccurenceDate, Result);
      end;
{$ENDIF}
  end;

  if not Result then
    Exit;

  if CorrectionRulesExist then
    DoCorrections(OccurenceDate);

  if FFormatTitle then
    DisplayText := FXCalendar.FormatDateTime(FTitle, OccurenceDate)
  else
    DisplayText := FTitle;
  IsVacation := FIsVacation;

{$IFDEF Delphi6}
  if (Collection.Owner is TXCalendarEvents) and
      Assigned(TXCalendarEvents(Collection.Owner).FOnOccur) then
        TXCalendarEvents(Collection.Owner).FOnOccur(Self, AYear, OccurenceDate, DisplayText, IsVacation, Result);
{$ELSE}
  if (Collection is TXCalEventCollection) and
     (TXCalEventCollection(Collection).Owner is TXCalendarEvents) and
      Assigned(TXCalendarEvents(TXCalEventCollection(Collection).Owner).FOnOccur) then
        TXCalendarEvents(TXCalEventCollection(Collection).Owner).FOnOccur(Self, AYear, OccurenceDate, DisplayText, IsVacation, Result);
{$ENDIF}

  FCache.Add(AYear, Result, OccurenceDate, DisplayText, IsVacation);
end;

{ TXCalEventCollection }

constructor TXCalEventCollection.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TXCalEventItem);
end;

function TXCalEventCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TXCalEventCollection.Add: TXCalEventItem;
begin
  Result := TXCalEventItem(inherited Add);
end;

function TXCalEventCollection.GetItem(Index: Integer): TXCalEventItem;
begin
  Result := TXCalEventItem(inherited GetItem(Index));
end;

function TXCalEventCollection.Insert(Index: Integer): TXCalEventItem;
begin
  Result := TXCalEventItem(inherited Insert(Index));
end;

procedure TXCalEventCollection.SetItem(Index: Integer; const Value: TXCalEventItem);
begin
  inherited SetItem(Index, Value);
end;

{$IFNDEF Delphi6}
function TXCalEventCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;
{$ENDIF}

{ TXCalendarEvents }

constructor TXCalendarEvents.Create(AOwner: TComponent);
begin
  inherited;

  FVersion := IntToStr(XCAL_MAJ_VER) + '.' +
              IntToStr(XCAL_MIN_VER) + '.' +
              IntToStr(XCAL_REL_VER);
  FXCalendarsOwner := Owner;
  FEvents := TXCalEventCollection.Create(Self);
end;

destructor TXCalendarEvents.Destroy;
begin
  FEvents.Free;

  inherited;
end;

procedure TXCalendarEvents.FindIntervalEvents(const FromDate,
  ToDate: TDateTime; var EOL: TXCalEventOccurenceList);
var
  I: Integer;
  XCalEventItem: TXCalEventItem;
begin
  if not Assigned(EOL) then
    Exit;

  for I := 0 to FEvents.Count - 1 do
  begin
    XCalEventItem := FEvents[I];
    XCalEventItem.FindOccurencesInAnInterval(FromDate, ToDate, EOL);
  end;
end;

function TXCalendarEvents.IsXCalendarsOwnerStored: Boolean;
begin
  Result := (FXCalendarsOwner <> Owner);
end;

procedure TXCalendarEvents.LoadFromFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(f);
  finally
    f.Free;
  end;
end;

procedure TXCalendarEvents.LoadFromStream(Stream: TStream);
var
  XMLDoc: TxcalXMLDocument;
begin
  XMLDoc := TxcalXMLDocument.Create;
  try
    XMLDoc.LoadFromStream(Stream);
    if CompareText(XMLDoc.Root.Name, XCalendarEventsXMLRootName) = 0 then
      LoadFromXMLItemChilds(XMLDoc.Root);
  finally
    XMLDoc.Free;
  end;
end;

procedure TXCalendarEvents.LoadFromXMLItemChilds(XMLItem: TxcalXMLItem;
  StartIndex, Count: Integer);
var
  I: Integer;
  xi: TxcalXMLItem;
begin
  I := StartIndex;
  if I < 0 then
    I := 0;

  if Count = 0 then
    Count := XMLItem.Count
  else if XMLItem.Count - StartIndex < Count then
    Count := XMLItem.Count - StartIndex;

  FEvents.BeginUpdate;
  try
    while I < Count do
    begin
      xi := XMLItem.Items[I];
      if CompareText(xi.Name, XCalendarEventsXMLEventName) = 0 then
        FEvents.Add.LoadFromXMLItem(xi);
      Inc(I);
    end;
  finally
    FEvents.EndUpdate;
  end;
end;

procedure TXCalendarEvents.SaveToFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

procedure TXCalendarEvents.SaveToStream(Stream: TStream);
var
  XMLDoc: TxcalXMLDocument;
begin
  XMLDoc := TxcalXMLDocument.Create;
  try
    XMLDoc.AutoIndent := True;
    XMLDoc.Root.Name := XCalendarEventsXMLRootName;
    XMLDoc.Root.Prop['Version'] := FVersion;
    SaveToXMLItemChilds(XMLDoc.Root);
    XMLDoc.SaveToStream(Stream);
  finally
    XMLDoc.Free;
  end;
end;

procedure TXCalendarEvents.SaveToXMLItemChilds(XMLItem: TxcalXMLItem);
var
  I: Integer;
  xi: TxcalXMLItem;
begin
  for I := 0 to FEvents.Count - 1 do
  begin
    xi := XMLItem.Add;
    xi.Name := XCalendarEventsXMLEventName;
    FEvents[I].SaveToXMLItem(xi);
  end;
end;

procedure TXCalendarEvents.SetEvents(const Value: TXCalEventCollection);
begin
  FEvents.Assign(Value);
end;

procedure TXCalendarEvents.SetVersion(const Value: string);
begin
end;

{ TDayOfWeekCorrections }

procedure TDayOfWeekCorrections.Assign(Source: TPersistent);
begin
  if (Source is TDayOfWeekCorrections) then
  begin
    FIfSunday := TDayOfWeekCorrections(Source).FIfSunday;
    FIfMonday := TDayOfWeekCorrections(Source).FIfMonday;
    FIfTuesday := TDayOfWeekCorrections(Source).FIfTuesday;
    FIfWednesday := TDayOfWeekCorrections(Source).FIfWednesday;
    FIfThursday := TDayOfWeekCorrections(Source).FIfThursday;
    FIfFriday := TDayOfWeekCorrections(Source).FIfFriday;
    FIfSaturday := TDayOfWeekCorrections(Source).FIfSaturday;
  end
  else
    inherited Assign(Source);
end;

procedure TDayOfWeekCorrections.Clear;
begin
  FIfSunday := dcNoCorrection;
  FIfMonday := dcNoCorrection;
  FIfTuesday := dcNoCorrection;
  FIfWednesday := dcNoCorrection;
  FIfThursday := dcNoCorrection;
  FIfFriday := dcNoCorrection;
  FIfSaturday := dcNoCorrection;
end;

{ TXCalEventOccurenceCache }

function TXCalEventOccurenceCache.Add(const AYear: Word; const AOccurs: Boolean;
  const AOccurenceDate: TDateTime; const ADisplayText: string; const AIsVacation: Boolean): Integer;
var
  p: PXCalEventOccurenceCacheItem;
begin
  if (FSize <= 0) then
  begin
    Result := -1;
    Exit;
  end;

  New(p);
  p^.Year := AYear;
  p^.Occurs := AOccurs;
  p^.OccurenceDate := AOccurenceDate;
  p^.DisplayText := ADisplayText;
  p^.IsVacation := AIsVacation;

  if (Count = FSize) then
    Delete(0);
  Result := inherited Add(p);
end;

function TXCalEventOccurenceCache.FindValue(const AYear: Word; out Occurs: Boolean;
  out OccurenceDate: TDateTime; out DisplayText: string; out IsVacation: Boolean): Integer;
var
  p: PXCalEventOccurenceCacheItem;
begin
  Result := Count - 1;
  while (Result >= 0) do
  begin
    p := Items[Result];
    if Assigned(p) and (p^.Year = AYear) then
    begin
      Occurs := p^.Occurs;
      OccurenceDate := p^.OccurenceDate;
      DisplayText := p^.DisplayText;
      IsVacation := p^.IsVacation;
      Break;
    end;
    Dec(Result);
  end;
end;

procedure TXCalEventOccurenceCache.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    Dispose(PXCalEventOccurenceCacheItem(Ptr));
end;

procedure TXCalEventOccurenceCache.SetSize(const Value: Integer);
begin
  FSize := Value;
  if FSize < 0 then
    FSize := 0;
  while FSize < Count do
    Delete(0);
  Capacity := FSize;
end;

{ TXCalEventCorrections }

constructor TXCalEventCorrections.Create;
begin
  inherited;
  
  FDayOfWeekCorrection := TDayOfWeekCorrections.Create;
end;

destructor TXCalEventCorrections.Destroy;
begin
  FDayOfWeekCorrection.Free;

  inherited;
end;

procedure TXCalEventCorrections.Assign(Source: TPersistent);
begin
  if (Source is TXCalEventCorrections) then
  begin
    FDayCorrection := TXCalEventCorrections(Source).FDayCorrection;
    FDayOfWeekCorrection.Assign(TXCalEventCorrections(Source).FDayOfWeekCorrection);
  end
  else
    inherited Assign(Source);
end;

procedure TXCalEventCorrections.SetDayOfWeekCorrection(const Value: TDayOfWeekCorrections);
begin
  FDayOfWeekCorrection.Assign(Value);
end;

procedure TXCalEventCorrections.Clear;
begin
  FDayCorrection := 0;
  FDayOfWeekCorrection.Clear;
end;

{ TXCalEventOccurenceList }

function TXCalEventOccurenceList.Add(const Date: TDateTime;
  const DisplayText: string; const IsVacation: Boolean; const Obj: TObject): Integer;
var
  EO: TXCalEventOccurence;
begin
  EO := TXCalEventOccurence.Create;
  EO.Date := Date;
  EO.DisplayText := DisplayText;
  EO.IsVacation := IsVacation;
  EO.Obj := Obj;
  Result := inherited Add(EO);
end;

function TXCalEventOccurenceList.Get(Index: Integer): TXCalEventOccurence;
begin
  Result := TXCalEventOccurence(inherited Items[Index]);
end;

procedure TXCalEventOccurenceList.Put(Index: Integer;
  const Value: TXCalEventOccurence);
begin
  inherited Items[Index] := Value;
end;

function TXCalEventOccurenceList.IndexOf(ADate: TDateTime; StartIndex: Integer): Integer;
var
  ADateInt: Integer;
begin
  Result := StartIndex;
  if Result < 0 then
    Result := 0;
  ADateInt := Trunc(ADate);
  while (Result < Count) and
    (Trunc(Items[Result].Date) <> ADateInt) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

procedure TXCalEventOccurenceList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    TXCalEventOccurence(Ptr).Free;
end;

function CompareDates(Item1, Item2: Pointer): Integer;
var
  DT1, DT2: TDateTime;
begin
  DT1 := TXCalEventOccurence(Item1).Date;
  DT2 := TXCalEventOccurence(Item2).Date;

  if DT1 = DT2 then
  begin
    if (TXCalEventOccurence(Item1).IsVacation) and (not TXCalEventOccurence(Item2).IsVacation) then
      Result := -1
    else if (not TXCalEventOccurence(Item1).IsVacation) and (TXCalEventOccurence(Item2).IsVacation) then
      Result := 1
    else
      Result := 0;
  end
  else if DT1 < DT2 then
    Result := -1
  else
    Result := 1;
end;

procedure TXCalEventOccurenceList.SortByDate;
begin
  inherited Sort(@CompareDates);
end;

function TXCalEventOccurenceList.Last: TXCalEventOccurence;
begin
  Result := TXCalEventOccurence(inherited Last);
end;

function TXCalEventOccurenceList.First: TXCalEventOccurence;
begin
  Result := TXCalEventOccurence(inherited First);
end;

{ TXCalAggregateEventsCollectionItem }

function TXCalAggregateEventsCollectionItem.GetDisplayName: string;
begin
  if Assigned(FXCalendarEvents) then
    Result := FXCalendarEvents.GetNamePath
  else
    Result := inherited GetDisplayName;    
end;

procedure TXCalAggregateEventsCollectionItem.SetXCalendarEvents(
  const Value: TCustomXCalendarEvents);
begin
  { Aggregate events cannot refer to aggregate events }
  if (Value is TXCalendarAggregateEvents) then
    Exit;

  FXCalendarEvents := Value;
end;

{ TXCalAggregateEventsCollection }

function TXCalAggregateEventsCollection.Add: TXCalAggregateEventsCollectionItem;
begin
  Result := TXCalAggregateEventsCollectionItem(inherited Add);
end;

constructor TXCalAggregateEventsCollection.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TXCalAggregateEventsCollectionItem);
end;

function TXCalAggregateEventsCollection.GetItem(
  Index: Integer): TXCalAggregateEventsCollectionItem;
begin
  Result := TXCalAggregateEventsCollectionItem(inherited GetItem(Index));
end;

function TXCalAggregateEventsCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TXCalAggregateEventsCollection.Insert(
  Index: Integer): TXCalAggregateEventsCollectionItem;
begin
  Result := TXCalAggregateEventsCollectionItem(inherited Insert(Index));
end;

procedure TXCalAggregateEventsCollection.SetItem(Index: Integer;
  const Value: TXCalAggregateEventsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TXCalendarAggregateEvents }

constructor TXCalendarAggregateEvents.Create(AOwner: TComponent);
begin
  inherited;

  FObjects := TXCalAggregateEventsCollection.Create(Self);
end;

destructor TXCalendarAggregateEvents.Destroy;
begin
  FObjects.Free;
  
  inherited;
end;

procedure TXCalendarAggregateEvents.FindIntervalEvents(const FromDate,
  ToDate: TDateTime; var OccurenceList: TXCalEventOccurenceList);
var
  I: Integer;
  EventComponent: TCustomXCalendarEvents;
begin
  for I := 0 to FObjects.Count - 1 do
  begin
    EventComponent := FObjects[I].XCalendarEvents;
    if Assigned(EventComponent) then
      EventComponent.FindIntervalEvents(FromDate, ToDate, OccurenceList);
  end;
end;

procedure TXCalendarAggregateEvents.SetObjects(
  const Value: TXCalAggregateEventsCollection);
begin
  FObjects.Assign(Value);
end;

{ TCustomXCalendarEvents }

procedure TCustomXCalendarEvents.FindDateEvents(const ADate: TDateTime;
  var EOL: TXCalEventOccurenceList);
begin
  FindIntervalEvents(ADate, ADate, EOL);
end;

end.
