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

unit xcalReg;

{$I xcalDefs.inc}

interface

procedure Register;


implementation

uses
  Classes, SysUtils, Dialogs,
{$IFDEF Delphi6}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  xcalDB, xcalClass, xcalGregorian, xcalPersian, xcalAstroPersian, xcalHijri,
  xcalObservedHijri, xcalEvents, xcalAstroEvents;

type

  TXCalendarEventsComponentEditor = class(TDefaultEditor)
  protected
  {$IFNDEF Delphi6}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TXCalendarAggregateEventsComponentEditor = class(TDefaultEditor)
  protected
  {$IFNDEF Delphi6}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


const
  XCAL_PALETTE_PAGE = 'XCalendar';


procedure Register;
begin
  { Components }
  RegisterComponents(XCAL_PALETTE_PAGE, [ TGregorianCalendar,
                                          TPersianCalendar,
                                          TAstroPersianCalendar,
                                          THijriCalendar,
                                          TObservedHijriCalendar,
                                          TXCalendarEvents,
                                          TXCalendarAstroEvents,
                                          TXCalendarAggregateEvents
                                        ]);

  { Component Editors }
  RegisterComponentEditor(TXCalendarEvents, TXCalendarEventsComponentEditor);
  RegisterComponentEditor(TXCalendarAggregateEvents, TXCalendarAggregateEventsComponentEditor);

  
  { Fields }
  RegisterXCalFields;
end;

{ TXCalendarEventsComponentEditor }

{$IFNDEF Delphi6}
procedure TXCalendarEventsComponentEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TXCalendarEventsComponentEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'EVENTS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TXCalendarEventsComponentEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
begin
  case Index of
    0: Edit;
    1:
      begin
        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
          if OpenDialog.Execute then
            (Component as TXCalendarEvents).LoadFromFile(OpenDialog.FileName);
        finally
          OpenDialog.Free;
        end;
      end;
    2:
      begin
        SaveDialog := TSaveDialog.Create(nil);
        try
          SaveDialog.DefaultExt := 'xml';
          SaveDialog.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
          if SaveDialog.Execute then
            (Component as TXCalendarEvents).SaveToFile(SaveDialog.FileName);
        finally
          SaveDialog.Free;
        end;
      end;
  end;
end;

function TXCalendarEventsComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := '&Edit Events...';
    1: Result := '&Load Events From File...';
    2: Result := '&Save Events To File...';
  end;
end;

function TXCalendarEventsComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TXCalendarAggregateEventsComponentEditor }

{$IFNDEF Delphi6}
procedure TXCalendarAggregateEventsComponentEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TXCalendarAggregateEventsComponentEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'OBJECTS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TXCalendarAggregateEventsComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TXCalendarAggregateEventsComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := '&Edit...';
  end;
end;

function TXCalendarAggregateEventsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
