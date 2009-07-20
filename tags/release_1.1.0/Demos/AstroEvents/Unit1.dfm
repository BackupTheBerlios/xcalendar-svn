object Form1: TForm1
  Left = 325
  Top = 114
  Width = 508
  Height = 578
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 200
    Top = 76
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label2: TLabel
    Left = 200
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Time Zone'
  end
  object ListBox1: TListBox
    Left = 24
    Top = 136
    Width = 450
    Height = 385
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 312
    Top = 94
    Width = 161
    Height = 25
    Caption = 'Find Astronomical Events'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 200
    Top = 96
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '2008'
  end
  object UpDown1: TUpDown
    Left = 273
    Top = 96
    Width = 16
    Height = 21
    Associate = Edit1
    Min = 1
    Max = 9999
    Position = 2008
    TabOrder = 3
    Thousands = False
    Wrap = False
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 16
    Width = 153
    Height = 105
    Caption = 'Event Types'
    TabOrder = 4
    object CheckBox1: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Equinoxes'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Moon Phases'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Eclipses'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox3Click
    end
  end
  object edtTimeZone: TEdit
    Left = 200
    Top = 32
    Width = 89
    Height = 21
    TabOrder = 5
    Text = '0.0'
  end
  object XCalendarAstroEvents1: TXCalendarAstroEvents
    Left = 184
    Top = 200
  end
end
