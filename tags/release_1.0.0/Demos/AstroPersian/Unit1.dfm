object Form1: TForm1
  Left = 339
  Top = 189
  Width = 533
  Height = 533
  ActiveControl = Edit1
  Caption = 'AstroPersian Calendar Leap Years Compare Demo'
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
    Left = 16
    Top = 16
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Button1: TButton
    Left = 352
    Top = 70
    Width = 161
    Height = 25
    Caption = 'Compare Leap Years'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 104
    Width = 496
    Height = 382
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    DefaultColWidth = 100
    RowCount = 2
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 16
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '1300'
  end
  object Edit2: TEdit
    Left = 16
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '1500'
  end
  object UpDown1: TUpDown
    Left = 73
    Top = 32
    Width = 16
    Height = 21
    Associate = Edit1
    Min = 1
    Max = 9999
    Position = 1300
    TabOrder = 1
    Thousands = False
    Wrap = False
  end
  object UpDown2: TUpDown
    Left = 73
    Top = 72
    Width = 16
    Height = 21
    Associate = Edit2
    Min = 1
    Max = 9999
    Position = 1500
    TabOrder = 3
    Thousands = False
    Wrap = False
  end
  object GroupBox1: TGroupBox
    Left = 120
    Top = 16
    Width = 217
    Height = 81
    Caption = 'Longitude for AstroPersian Calendar'
    TabOrder = 4
    object Label3: TLabel
      Left = 16
      Top = 32
      Width = 40
      Height = 13
      Caption = 'Degrees'
    end
    object Label4: TLabel
      Left = 80
      Top = 32
      Width = 37
      Height = 13
      Caption = 'Minutes'
    end
    object Label5: TLabel
      Left = 144
      Top = 32
      Width = 42
      Height = 13
      Caption = 'Seconds'
    end
    object edtDegrees: TEdit
      Left = 16
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 0
      Text = '52'
    end
    object edtMinutes: TEdit
      Left = 80
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '30'
    end
    object edtSeconds: TEdit
      Left = 144
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 2
      Text = '0'
    end
  end
  object AstroPersianCalendar1: TAstroPersianCalendar
    Longitude = 52.5
    CacheSize = 0
    Left = 256
    Top = 160
  end
  object PersianCalendar1: TPersianCalendar
    Left = 152
    Top = 160
  end
end
