object Form1: TForm1
  Left = 325
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Calendars Demo'
  ClientHeight = 570
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 569
    Height = 153
    TabOrder = 0
    object Label1: TLabel
      Left = 336
      Top = 88
      Width = 23
      Height = 13
      Caption = 'Date'
    end
    object Label2: TLabel
      Left = 336
      Top = 24
      Width = 80
      Height = 13
      Caption = 'ShortDateFormat'
    end
    object Label14: TLabel
      Left = 176
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Week Start Day'
    end
    object Label15: TLabel
      Left = 176
      Top = 88
      Width = 54
      Height = 13
      Caption = 'Week Rule'
    end
    object Label18: TLabel
      Left = 16
      Top = 24
      Width = 42
      Height = 13
      Caption = 'Calendar'
    end
    object ListBox1: TListBox
      Left = 16
      Top = 40
      Width = 145
      Height = 89
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object edtDate: TEdit
      Left = 336
      Top = 104
      Width = 137
      Height = 21
      TabOrder = 1
    end
    object btnUpdateDate: TButton
      Left = 480
      Top = 102
      Width = 75
      Height = 25
      Caption = 'Update'
      Default = True
      TabOrder = 2
      OnClick = btnUpdateDateClick
    end
    object edtFormat: TEdit
      Left = 336
      Top = 40
      Width = 137
      Height = 21
      TabOrder = 3
      Text = 'yyyy/mm/dd'
      OnChange = edtFormatChange
    end
    object cbWeekStartDay: TComboBox
      Left = 176
      Top = 40
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = cbWeekStartDayChange
    end
    object cbWeekRule: TComboBox
      Left = 176
      Top = 104
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = cbWeekRuleChange
      Items.Strings = (
        'Full Weeks'
        'Four Day Weeks'
        'One Day Weeks')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 168
    Width = 569
    Height = 393
    TabOrder = 1
    object Label3: TLabel
      Left = 48
      Top = 16
      Width = 83
      Height = 13
      Caption = 'TDateTime Value'
    end
    object Label4: TLabel
      Left = 208
      Top = 16
      Width = 49
      Height = 13
      Caption = 'Julian Day'
    end
    object Label5: TLabel
      Left = 376
      Top = 128
      Width = 65
      Height = 13
      Caption = 'Day Of Week'
    end
    object Label6: TLabel
      Left = 48
      Top = 72
      Width = 22
      Height = 13
      Caption = 'Year'
    end
    object Label7: TLabel
      Left = 208
      Top = 72
      Width = 30
      Height = 13
      Caption = 'Month'
    end
    object Label8: TLabel
      Left = 376
      Top = 72
      Width = 19
      Height = 13
      Caption = 'Day'
    end
    object Label9: TLabel
      Left = 48
      Top = 128
      Width = 105
      Height = 13
      Caption = 'Leap or Common Year'
    end
    object Label10: TLabel
      Left = 48
      Top = 184
      Width = 83
      Height = 13
      Caption = 'Start Of The Year'
    end
    object Label11: TLabel
      Left = 288
      Top = 184
      Width = 90
      Height = 13
      Caption = 'Week Of The Year'
    end
    object Label12: TLabel
      Left = 48
      Top = 232
      Width = 91
      Height = 13
      Caption = 'Start Of The Month'
    end
    object Label13: TLabel
      Left = 288
      Top = 232
      Width = 98
      Height = 13
      Caption = 'Week Of The Month'
    end
    object Label16: TLabel
      Left = 48
      Top = 288
      Width = 121
      Height = 13
      Caption = 'Days to End of the Month'
    end
    object Label17: TLabel
      Left = 288
      Top = 288
      Width = 113
      Height = 13
      Caption = 'Days to End of the Year'
    end
    object Label19: TLabel
      Left = 48
      Top = 336
      Width = 133
      Height = 13
      Caption = 'Last Wednesday of the year'
    end
    object Label20: TLabel
      Left = 288
      Top = 336
      Width = 68
      Height = 13
      Caption = 'Nearest Friday'
    end
    object edtTDateTime: TEdit
      Left = 48
      Top = 32
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 0
    end
    object edtJulianDay: TEdit
      Left = 208
      Top = 32
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 1
    end
    object edtDayOfWeek: TEdit
      Left = 376
      Top = 144
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 2
    end
    object edtYear: TEdit
      Left = 48
      Top = 88
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 3
    end
    object edtMonth: TEdit
      Left = 208
      Top = 88
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 4
    end
    object edtDay: TEdit
      Left = 376
      Top = 88
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 5
    end
    object edtLeap: TEdit
      Left = 48
      Top = 144
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 6
    end
    object edtStartOfTheYear: TEdit
      Left = 48
      Top = 200
      Width = 217
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 7
    end
    object edtWeekOfTheYear: TEdit
      Left = 288
      Top = 200
      Width = 225
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 8
    end
    object edtStartOfTheMonth: TEdit
      Left = 48
      Top = 248
      Width = 217
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 9
    end
    object edtWeekOfTheMonth: TEdit
      Left = 288
      Top = 248
      Width = 225
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 10
    end
    object edtDaySpan1: TEdit
      Left = 48
      Top = 304
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 11
    end
    object edtDaySpan2: TEdit
      Left = 288
      Top = 304
      Width = 137
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 12
    end
    object edtLastWed: TEdit
      Left = 48
      Top = 352
      Width = 217
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 13
    end
    object edtNearestFri: TEdit
      Left = 288
      Top = 352
      Width = 225
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 14
    end
  end
  object GregorianCalendar1: TGregorianCalendar
    Top = 56
  end
  object PersianCalendar1: TPersianCalendar
    Top = 88
  end
  object HijriCalendar1: THijriCalendar
    Top = 120
  end
end
