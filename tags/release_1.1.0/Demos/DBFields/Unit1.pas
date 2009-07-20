unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, DBCtrls, ExtCtrls, Grids, DBGrids, DB, DBTables,
  xcalDB, xcalGregorian, xcalHijri, xcalPersian, xcalClass;

type
  TForm1 = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Table1SaleDate: TXcalDateTimeField;
    Table1ShipDate: TXcalDateTimeField;
    GregorianCalendar1: TGregorianCalendar;
    PersianCalendar1: TPersianCalendar;
    HijriCalendar1: THijriCalendar;
    Panel1: TPanel;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    DBNavigator1: TDBNavigator;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: Table1SaleDate.XCalendar := GregorianCalendar1;
    1: Table1SaleDate.XCalendar := PersianCalendar1;
    2: Table1SaleDate.XCalendar := HijriCalendar1;
  end;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  case RadioGroup2.ItemIndex of
    0: Table1ShipDate.XCalendar := GregorianCalendar1;
    1: Table1ShipDate.XCalendar := PersianCalendar1;
    2: Table1ShipDate.XCalendar := HijriCalendar1;
  end;
end;

end.
