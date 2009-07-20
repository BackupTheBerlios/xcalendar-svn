object Form1: TForm1
  Left = 346
  Top = 199
  Width = 833
  Height = 640
  BiDiMode = bdRightToLeft
  Caption = 'Events Demo'
  Color = clBtnFace
  Font.Charset = ARABIC_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DrawGrid1: TDrawGrid
    Left = 0
    Top = 137
    Width = 825
    Height = 469
    Align = alClient
    ColCount = 4
    Ctl3D = True
    FixedCols = 3
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking]
    ParentCtl3D = False
    TabOrder = 0
    OnDrawCell = DrawGrid1DrawCell
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 825
    Height = 137
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      825
      137)
    object btnSave: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnSaveVacations: TButton
      Left = 16
      Top = 56
      Width = 233
      Height = 25
      Caption = 'Save all vacations'
      TabOrder = 1
      OnClick = btnSaveVacationsClick
    end
    object btnGetEvents: TButton
      Left = 512
      Top = 96
      Width = 187
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Get events for the Persian year'
      Default = True
      TabOrder = 2
      OnClick = btnGetEventsClick
    end
    object UpDown1: TUpDown
      Left = 777
      Top = 98
      Width = 16
      Height = 21
      Anchors = [akTop, akRight]
      Associate = Edit1
      Min = 1
      Max = 9999
      Position = 1388
      TabOrder = 3
      Thousands = False
    end
    object Edit1: TEdit
      Left = 704
      Top = 98
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      BiDiMode = bdRightToLeftNoAlign
      ParentBiDiMode = False
      TabOrder = 4
      Text = '1388'
    end
    object ComboBox1: TComboBox
      Left = 96
      Top = 18
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      Items.Strings = (
        'Iran Events Persian'
        'Iran Events Hijri'
        'Iran Events Gregorian'
        'Iran Events Miscellaneous')
    end
    object btnLoad: TButton
      Left = 16
      Top = 96
      Width = 233
      Height = 25
      Caption = 'Load'
      TabOrder = 6
      OnClick = btnLoadClick
    end
  end
  object IranEventsPersian: TXCalendarEvents
    Events = <>
    Left = 56
    Top = 216
  end
  object IranEventsMisc: TXCalendarEvents
    Events = <>
    Left = 176
    Top = 216
  end
  object IranEventsHijri: TXCalendarEvents
    Events = <>
    Left = 96
    Top = 216
  end
  object IranAggregateEvents: TXCalendarAggregateEvents
    Objects = <
      item
        XCalendarEvents = IranEventsPersian
      end
      item
        XCalendarEvents = IranEventsHijri
      end
      item
        XCalendarEvents = IranEventsGregorian
      end
      item
        XCalendarEvents = IranEventsMisc
      end>
    Left = 472
    Top = 96
  end
  object PersianCalendar: TPersianCalendar
    Left = 56
    Top = 248
  end
  object GregorianCalendar: TGregorianCalendar
    Left = 136
    Top = 248
  end
  object IranEventsGregorian: TXCalendarEvents
    Events = <>
    Left = 136
    Top = 216
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml'
    Left = 296
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Left = 336
    Top = 16
  end
  object ObservedHijriCalendar: TObservedHijriCalendar
    OnGetDaysInAMonth = ObservedHijriCalendarGetDaysInAMonth
    BaseYear = 1429
    MinObservedYear = 1346
    MaxObservedYear = 1430
    OnFindLeapYears = ObservedHijriCalendarFindLeapYears
    Left = 96
    Top = 248
  end
end
