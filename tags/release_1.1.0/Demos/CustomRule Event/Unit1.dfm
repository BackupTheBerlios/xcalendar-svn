object Form1: TForm1
  Left = 325
  Top = 114
  Width = 435
  Height = 403
  Caption = 'Custom Rule Events Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 32
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 144
    Top = 32
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Button1: TButton
    Left = 256
    Top = 32
    Width = 147
    Height = 25
    Caption = 'Find Easter Sundays'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 24
    Top = 72
    Width = 377
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 56
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '2000'
  end
  object Edit2: TEdit
    Left = 168
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 3
    Text = '2010'
  end
  object UpDown1: TUpDown
    Left = 113
    Top = 32
    Width = 17
    Height = 21
    Associate = Edit1
    Min = 1
    Max = 9999
    Position = 2000
    TabOrder = 4
    Thousands = False
    Wrap = False
  end
  object UpDown2: TUpDown
    Left = 225
    Top = 32
    Width = 17
    Height = 21
    Associate = Edit2
    Min = 1
    Max = 9999
    Position = 2010
    TabOrder = 5
    Thousands = False
    Wrap = False
  end
  object XCalendarEvents1: TXCalendarEvents
    Events = <
      item
        XCalendar = GregorianCalendar1
        RuleType = rtCustomRule
        Title = 'Easter Sunday'
        Tag = 1
      end>
    OnCustomRule = XCalendarEvents1CustomRule
    Left = 128
    Top = 144
  end
  object GregorianCalendar1: TGregorianCalendar
    Left = 184
    Top = 144
  end
end
