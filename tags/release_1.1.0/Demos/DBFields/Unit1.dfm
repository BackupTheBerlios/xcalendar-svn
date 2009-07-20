object Form1: TForm1
  Left = 322
  Top = 121
  Width = 738
  Height = 594
  Caption = 'Database Fields Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 177
    Width = 730
    Height = 383
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 730
    Height = 177
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 120
      Width = 47
      Height = 13
      Caption = 'Sale Date'
    end
    object Label2: TLabel
      Left = 240
      Top = 120
      Width = 47
      Height = 13
      Caption = 'Ship Date'
    end
    object DBEdit1: TDBEdit
      Left = 24
      Top = 136
      Width = 185
      Height = 21
      DataField = 'SaleDate'
      DataSource = DataSource1
      TabOrder = 0
    end
    object DBEdit2: TDBEdit
      Left = 240
      Top = 136
      Width = 185
      Height = 21
      DataField = 'ShipDate'
      DataSource = DataSource1
      TabOrder = 1
    end
    object RadioGroup1: TRadioGroup
      Left = 24
      Top = 16
      Width = 185
      Height = 81
      Caption = 'Show Sale Date in'
      ItemIndex = 0
      Items.Strings = (
        'Gregorian'
        'Persian'
        'Hijri')
      TabOrder = 2
      OnClick = RadioGroup1Click
    end
    object RadioGroup2: TRadioGroup
      Left = 240
      Top = 16
      Width = 185
      Height = 81
      Caption = 'Show Ship Date in'
      ItemIndex = 1
      Items.Strings = (
        'Gregorian'
        'Persian'
        'Hijri')
      TabOrder = 3
      OnClick = RadioGroup2Click
    end
    object DBNavigator1: TDBNavigator
      Left = 456
      Top = 136
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 4
    end
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'orders.db'
    Left = 472
    Top = 48
    object Table1SaleDate: TXcalDateTimeField
      DisplayWidth = 30
      FieldName = 'SaleDate'
      DisplayFormat = 'dddd yyyy/mm/dd'
      XCalendar = GregorianCalendar1
    end
    object Table1ShipDate: TXcalDateTimeField
      DisplayWidth = 30
      FieldName = 'ShipDate'
      DisplayFormat = 'dddd d mmmm yyyy'
      XCalendar = PersianCalendar1
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 504
    Top = 48
  end
  object GregorianCalendar1: TGregorianCalendar
    Left = 560
    Top = 48
  end
  object PersianCalendar1: TPersianCalendar
    Left = 592
    Top = 48
  end
  object HijriCalendar1: THijriCalendar
    Left = 624
    Top = 48
  end
end
