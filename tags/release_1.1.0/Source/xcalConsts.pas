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

unit xcalConsts;

{$I xcalDefs.inc}

interface

resourcestring

{ Exception message strings }

  SXCalInvalidDateDay = '(%d, %d) is not a valid DateDay pair';
  SXCalInvalidDateWeek = '(%d, %d, %d) is not a valid DateWeek triplet';
  SXCalInvalidDateMonthWeek = '(%d, %d, %d, %d) is not a valid DateMonthWeek quad';
  SXCalInvalidDayOfWeekInMonth = '(%d, %d, %d, %d) is not a valid DayOfWeekInMonth quad';
  SXCalInvalidJulianDay = '%f Julian cannot be represented as a DateTime';
  SXCalMissingDateTimeField = '?';
  SXCalInvalidDayOfWeek = '%d is not a valid DayOfWeek value';

implementation

end.

