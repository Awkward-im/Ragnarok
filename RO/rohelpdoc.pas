unit ROHelpDoc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, Dialogs, StdCtrls,
  rodoc;

type

  { TROHelpDocForm }

  TROHelpDocForm = class(TForm)
    cbROPart: TComboBox;
    cbROCard: TComboBox;
    lPart: TLabel;
    lblValue: TLabel;
    memCard: TMemo;

    procedure cbROCardChange(Sender: TObject);
    procedure cbROPartChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRODoc:pRODocument;
    FMustFree:boolean;

    procedure FillPartList;
    procedure FillCardList;
  public
    procedure SelectWord(const aword:AnsiString);
    constructor Create(AOwner: TComponent;
        const aRODoc:tRODocument; aCardLabel:String; amustFree:boolean=false); overload;
  end;


implementation

uses
  rodatatypes;

{$R *.lfm}

{ TROHelpDocForm }

procedure TROHelpDocForm.FillPartList;
var
  ls:AnsiString;
  i,num:integer;
begin
  cbROPart.Clear;

  if FRODoc^.DocParts>0 then
    cbROPart.Items.Add('Common things');

  for i:=0 to FRODoc^.PartCount-1 do
  begin
    num:=FRODoc^.PartNumber[i];
    if num>0 then
    begin
      ls:=IntToStr(num mod 1000);
      if num>1000 then
        ls:=ls + '.'+IntToStr(num div 1000);
      ls:=ls+'. '+AnsiString(FRODoc^.Parts[i]);
    end
    else
      ls:=AnsiString(FRODoc^.Parts[i]);
    cbROPart.Items.Add(ls);
  end;

  cbROPart.ItemIndex:=0;
  FillCardList;
end;

procedure TROHelpDocForm.FillCardList;
var
  l_part,i:integer;
begin
  cbROCard.Clear;

  l_part:=cbROPart.ItemIndex;
  if FRODoc^.DocParts>0 then
    dec(l_part);

  if l_part<0 then
  begin
    for i:=0 to FRODoc^.DocParts-1 do
    begin
      cbROCard.Items.Add(FRODoc^.Doc[i]);
    end;
  end
  else
  begin
    for i:=0 to FRODoc^.CardCount-1 do
    begin
      if (l_part=0) or (FRODoc^.CardPart[i]=l_part) then
        cbROCard.Items.Add(FRODoc^.Cards[i]);
    end;
  end;

  cbROCard.ItemIndex:=0;
  cbROCardChange(nil);
end;

procedure TROHelpDocForm.FormCreate(Sender: TObject);
begin
  if (FRODoc^.CardCount>0) or (FRODoc^.Load) then
    FillPartList;
end;

constructor TROHelpDocForm.Create(AOwner: TComponent;
    const aRODoc:tRODocument; aCardLabel:String; amustFree:boolean=false);
begin
  inherited Create(AOwner);

  lblValue.Caption:=aCardLabel;
  Caption:='Doc: '+aCardLabel;
  FRODoc:=@aRODoc;
  FMustFree:=amustfree;
end;

procedure TROHelpDocForm.FormDestroy(Sender: TObject);
begin
  if FMustFree then
  begin
    FRODoc^.Free;
    FreeMem(FRODoc);
  end;
end;

procedure TROHelpDocForm.cbROPartChange(Sender: TObject);
begin
  FillCardList;
end;

procedure TROHelpDocForm.cbROCardChange(Sender: TObject);
begin
  memCard.Lines.Clear;
  if (FRODoc^.DocParts>0) and (cbROPart.ItemIndex=0) then
    memCard.Lines.Text:=FRODoc^.DocDescr[PAnsiChar(cbROCard.Text)]
  else
    memCard.Lines.Text:=FRODoc^.Descr[PAnsiChar(cbROCard.Text)];
end;

procedure TROHelpDocForm.SelectWord(const aword:AnsiString);
var
  i:integer;
begin
  if aword='' then exit;

  for i:=0 to FRODoc^.CardCount-1 do
  begin
    if aword=FRODoc^.Cards[i] then
    begin
      cbROPart.ItemIndex:=FRODoc^.CardPart[i]+1;
      FillCardList;
      cbROCard.Text:=aword;
      cbROCardChange(nil);
      exit;
    end;
  end;

  if FRODoc^.DocParts>0 then
    cbROPart.ItemIndex:=1
  else
    cbROPart.ItemIndex:=0;
  FillCardList;
  cbROCard.ItemIndex:=0;
  cbROCardChange(nil);
end;

end.
