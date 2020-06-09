unit ROHelpCardMob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TROHelpCardMobForm }

  TROHelpCardMobForm = class(TForm)
    bbOk: TBitBtn;
    btnFullInfo: TButton;
    btnMap: TButton;
    cbStretch: TCheckBox;
    edAGI: TEdit;
    edATK1: TEdit;
    edATK2: TEdit;
    edDEF: TEdit;
    edDEX: TEdit;
    edElementLevel: TEdit;
    edINT: TEdit;
    edHP: TEdit;
    edSP: TEdit;
    edLUK: TEdit;
    edMDEF: TEdit;
    edName: TEdit;
    edLevel: TEdit;
    edBaseEXP: TEdit;
    edBaseJEXP: TEdit;
    edScale: TEdit;
    edRace: TEdit;
    edElement: TEdit;
    edSTR: TEdit;
    edVIT: TEdit;
    gbStats: TGroupBox;
    gbDEFATK: TGroupBox;
    ImgMobSprite: TImage;
    lblAGI: TLabel;
    lblATK1: TLabel;
    lblATK2: TLabel;
    lblDEF: TLabel;
    lblDEX: TLabel;
    lblINT: TLabel;
    lblHP: TLabel;
    lblSP: TLabel;
    lblLUK: TLabel;
    lblMDEF: TLabel;
    lblName: TLabel;
    lblScale: TLabel;
    lblRace: TLabel;
    lblElement: TLabel;
    lblSTR: TLabel;
    lblVIT: TLabel;
    lblLevel: TLabel;
    lblBaseEXP: TLabel;
    lblBaseJEXP: TLabel;
    tbOFF: TToggleBox;
    procedure bbOkClick(Sender: TObject);
    procedure btnFullInfoClick(Sender: TObject);
    procedure btnMapClick(Sender: TObject);
    procedure cbStretchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    procedure FillForm(anIdx:integer);
  public
    constructor Create(AOwner:TComponent; anIdx:integer); overload;
  end;

var
  ROHelpCardMobForm: TROHelpCardMobForm;

implementation

uses
  ROFullCardMob,RORespList,
  rodatatypes,rodata;

{$R *.lfm}

{ TROHelpCardMobForm }

constructor TROHelpCardMobForm.Create(AOwner:TComponent; anIdx:integer);
begin
  inherited Create(AOwner);
  Caption:=RODB_GetTitle(anIdx,rosmMonster);
  FillForm(anIdx);
end;

procedure TROHelpCardMobForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TROHelpCardMobForm.cbStretchChange(Sender: TObject);
begin
  imgMobSprite.Stretch:=cbStretch.Checked;
end;

procedure TROHelpCardMobForm.btnFullInfoClick(Sender: TObject);
begin
  with TROFullCardMobForm.Create(Self.Owner,Tag) do Show;
  Close;
end;

procedure TROHelpCardMobForm.bbOkClick(Sender: TObject);
begin
  ModalResult:=mrOk; //!!
  Close; // for non-modal form
end;

procedure TROHelpCardMobForm.FillForm(anIdx:integer);
begin
  if anIdx<0 then
    exit;

  Tag:=anIdx;
  edName.Text:=RODB_GetText(anIdx,rosmMonster);
  with ROMobData[anIdx] do
  begin
    tbOFF.Checked:=Disabled;
    edLevel       .Text:=IntToStr(LVL);
    edHP          .Text:=IntToStr(HP);
    edSP          .Text:=IntToStr(SP);
    edBaseEXP     .Text:=IntToStr(EXP);
    edBaseJEXP    .Text:=IntToStr(JEXP);
    edScale       .Text:=ROSizes   [Scale];
    edRace        .Text:=RORaces   [Race];
    edElement     .Text:=ROElements[Element mod 20];
    edElementLevel.Text:=IntToStr(Element div 20);
    edSTR         .Text:=IntToStr(STR);
    edAGI         .Text:=IntToStr(AGI);
    edVIT         .Text:=IntToStr(VIT);
    edINT         .Text:=IntToStr(INT);
    edDEX         .Text:=IntToStr(DEX);
    edLUK         .Text:=IntToStr(LUK);
    edATK1        .Text:=IntToStr(ATK1);
    edATK2        .Text:=IntToStr(ATK2);
    edDEF         .Text:=IntToStr(DEF);
    edMDEF        .Text:=IntToStr(MDEF);
  end;

  Caption:=RODB_GetTitle(anIdx,rosmMonster);

  try
    imgMobSprite.Picture.LoadFromFile(RODB_GetSprite(anIdx,rosmMonster));
  except
  end;
end;

procedure TROHelpCardMobForm.btnMapClick(Sender: TObject);
begin
  with TRoRespListForm.Create(Self,rosmMonster,Tag) do Show;
end;

end.

