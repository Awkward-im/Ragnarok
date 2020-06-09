unit ROHelpConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls;

type

  { TRODictConstForm }

  TRODictConstForm = class(TForm)
    lblConstList: TLabel;
    lblConstPart: TLabel;
    lbConstPart: TListBox;
    lbConstList: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lbConstPartSelectionChange(Sender: TObject; User: boolean);
  private
    procedure FillPartList;
  public

  end;

var
  RODictConstForm: TRODictConstForm=nil;

implementation

uses
  rodatatypes,rodataio;

{$R *.lfm}

{ TRODictConstForm }

procedure TRODictConstForm.FillPartList;
var
  i:integer;
begin
  lbConstPart.Clear;
  for i:=0 to ROConstParts.Count-1 do
    lbConstPart.Items.Add(tROConstPart(ROConstParts[i]^).name);
  if ROConstParts.Count>0 then
    lbConstPart.ItemIndex:=0;
end;

procedure TRODictConstForm.lbConstPartSelectionChange(Sender: TObject; User: boolean);
var
  idx,i:integer;
begin
   idx:=lbConstPart.ItemIndex;
   lbConstList.Clear;
   for i:=tROConstPart(ROConstParts[idx]^).cFrom
       to tROConstPart(ROConstParts[idx]^).cTo do
     lbConstList.Items.Add(PAnsiChar(ROConstValues[i]^));
end;

procedure TRODictConstForm.FormCreate(Sender: TObject);
begin
  FillPartList;
end;

end.
