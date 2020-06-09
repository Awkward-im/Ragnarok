unit ROFullCardMob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Buttons,
  rodatatypes, Types;

type

  { TROFullCardMobForm }

  TROFullCardMobForm = class(TForm)
    bbOK: TBitBtn;
    bbClose: TBitBtn;
    btnJobMode: TButton;
    btnMap: TButton;
    cbScale: TComboBox;
    cbElement: TComboBox;
    cbRace: TComboBox;
    cbElementLevel: TComboBox;
    edAGI: TEdit;
    edATK1: TEdit;
    edATK2: TEdit;
    edBaseEXP: TEdit;
    edBaseJEXP: TEdit;
    edDEF: TEdit;
    edDEX: TEdit;
    edMode: TEdit;
    edHP: TEdit;
    edINT: TEdit;
    edAEName: TEdit;
    edID: TEdit;
    edLevel: TEdit;
    edLUK: TEdit;
    edMDEF: TEdit;
    edIROName: TEdit;
    edKROName: TEdit;
    edRange1: TEdit;
    edRange2: TEdit;
    edRange3: TEdit;
    edSP: TEdit;
    edMVPEXP: TEdit;
    edSpeed: TEdit;
    edaDelay: TEdit;
    edaMotion: TEdit;
    eddMotion: TEdit;
    edSTR: TEdit;
    edVIT: TEdit;
    gbDEFATK: TGroupBox;
    gbStats: TGroupBox;
    gbRange: TGroupBox;
    ImgMobSprite: TImage;
    lblRange: TLabel;
    lblID: TLabel;
    lblAGI: TLabel;
    lblATK1: TLabel;
    lblATK2: TLabel;
    lblBaseEXP: TLabel;
    lblBaseJEXP: TLabel;
    lblDEF: TLabel;
    lblDEX: TLabel;
    lblElement: TLabel;
    lblHP: TLabel;
    lblINT: TLabel;
    lblKROName1: TLabel;
    lblLevel: TLabel;
    lblLUK: TLabel;
    lblMDEF: TLabel;
    lblIROName: TLabel;
    lblKROName: TLabel;
    lblRace: TLabel;
    lblRange1: TLabel;
    lblRange2: TLabel;
    lblRange3: TLabel;
    lblScale: TLabel;
    lblSP: TLabel;
    lblSpeed: TLabel;
    lblaDelay: TLabel;
    lblaMotion: TLabel;
    lbldMotion: TLabel;
    lblMode: TLabel;
    lvlMVPEXP: TLabel;
    lblSTR: TLabel;
    lblVIT: TLabel;
    sgDrop: TStringGrid;
    tbOFF: TToggleBox;
    tbReadOnly: TToggleBox;
    procedure bbModeClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure btnMapClick(Sender: TObject);
    procedure edModeChange(Sender: TObject);
    procedure edRange1Change(Sender: TObject);
    procedure sgDropDblClick(Sender: TObject);
    procedure sgDropDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SomethingChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sgDropEditingDone(Sender: TObject);
    procedure tbReadOnlyClick(Sender: TObject);
  private
    pics: array [1..13] of TPicture;
    procedure FillDropLine(const aDrop:tRODropData; aline:integer);
    procedure ClearGrid;
    procedure PrepareForm;
    procedure FillForm(idx:integer);
    procedure SaveForm;
    procedure SetROMode(arg:boolean);
    procedure SetID(arg:integer);

  public
    constructor Create(AOwner:TComponent; anIdx:integer; areadonly:boolean=true); overload;

    property ID:integer write SetID;
    property ReadOnly:boolean write SetROMode;
  end;

var
  ROFullCardMobForm: TROFullCardMobForm;

implementation

uses
  ROFullCardItem, RORespList,
  rodata,rodatagui;

{$R *.lfm}

resourcestring
  sMobMode = 'Monster mode';

{ TROFullCardMobForm }

constructor TROFullCardMobForm.Create(AOwner:TComponent; anIdx:integer; areadonly:boolean=true);
begin
  inherited Create(AOwner);

  Caption:=RODB_GetTitle(anIdx,rosmMonster);

  PrepareForm;
  FillForm(anIdx);

  bbOk.Caption:='&OK';
  bbClose.Visible:=false;
  ReadOnly:=areadonly;
  tbReadOnly.Checked:=areadonly;
  tbReadOnly.Visible:=ROAllowEditCard;
end;

procedure TROFullCardMobForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i:integer;
begin
  for i:=1 to 13 do
    if (pics[i]<>nil) and (pics[i]<>TPicture(-1)) then
      pics[i].Free;

  CloseAction:=caFree;
end;

procedure TROFullCardMobForm.ClearGrid;
begin
  sgDrop.Clear;
  sgDrop.RowCount:=13+1;
  with sgDrop do
  begin
    Cells[0, 1]:='1';
    Cells[0, 2]:='2';
    Cells[0, 3]:='3';
    Cells[0, 4]:='4';
    Cells[0, 5]:='5';
    Cells[0, 6]:='6';
    Cells[0, 7]:='7';
    Cells[0, 8]:='8';
    Cells[0, 9]:='9';
    Cells[0,10]:='Card';
    Cells[0,11]:='MVP 1';
    Cells[0,12]:='MVP 2';
    Cells[0,13]:='MVP 3';
  end;
end;

procedure TROFullCardMobForm.PrepareForm;
var
  i:integer;
begin
  ClearGrid;

  cbElementLevel.Clear; for i:=1 to  4 do cbElementLevel.Items.Add(IntToStr(i));
  cbElement     .Clear; for i:=0 to  9 do cbElement     .Items.Add(ROElements[i]);
  cbScale       .Clear; for i:=0 to  2 do cbScale       .Items.Add(ROSizes[i]);
  cbRace        .Clear; for i:=0 to 10 do cbRace        .Items.Add(RORaces[i]);
end;

procedure TROFullCardMobForm.FillDropLine(const aDrop:tRODropData; aline:integer);
begin
  if aDrop.id<>0 then
  begin
    sgDrop.Cells[1,aline]:=IntToStr(aDrop.id);
    sgDrop.Cells[2,aline]:=
      RODB_GetText(
      RODB_GetIndex(aDrop.id,rosmItem),
      rosmItem);
    sgDrop.Cells[3,aline]:=IntToStr(aDrop.rate);
  end;
end;

procedure TROFullCardMobForm.FillForm(idx:integer);
var
  j:integer;
begin
  if idx<0 then exit;

  Tag:=idx;
  with ROMobData[idx] do
  begin
    tbOFF.Checked:=Disabled;
    edAEName .Text:=StrCache[name]{RODB_Text[anIdx,rosmMonster]};
    edID     .Text:=IntToStr(id);
    edIROName.Text:=descr;
    if kro='' then
      edKROName.Text:=descr
    else
      edKROName.Text:=kro;

    edLevel   .Text:=IntToStr(LVL);
    edHP      .Text:=IntToStr(HP);
    edSP      .Text:=IntToStr(SP);
    edBaseEXP .Text:=IntToStr(EXP);
    edBaseJEXP.Text:=IntToStr(JEXP);
    edRange1  .Text:=IntToStr(Range1);
    edATK1    .Text:=IntToStr(ATK1);
    edATK2    .Text:=IntToStr(ATK2);
    edDEF     .Text:=IntToStr(DEF);
    edMDEF    .Text:=IntToStr(MDEF);
    edSTR     .Text:=IntToStr(STR);
    edAGI     .Text:=IntToStr(AGI);
    edVIT     .Text:=IntToStr(VIT);
    edINT     .Text:=IntToStr(INT);
    edDEX     .Text:=IntToStr(DEX);
    edLUK     .Text:=IntToStr(LUK);
    edRange2  .Text:=IntToStr(Range2);
    edRange3  .Text:=IntToStr(Range3);
    cbScale       .ItemIndex:=Scale;
    cbRace        .ItemIndex:=Race;
    cbElement     .ItemIndex:=Element mod 20;
    cbElementLevel.ItemIndex:=(Element div 20)-1;
    edMode    .Text:=IntToHex(Mode,8);
    edSpeed   .Text:=IntToStr(Speed);
    edaDelay  .Text:=IntToStr(aDelay);
    edaMotion .Text:=IntToStr(aMotion);
    eddMotion .Text:=IntToStr(dMotion);
    edMVPEXP  .Text:=IntToStr(MEXP);
    edMVPEXP.Enabled:=(StrToInt('$'+edMode.Text) and MD_MVP)<>0;

    for j:=0 to 8 do
    begin
      FillDropLine(Drop[j],j+1);
    end;
    FillDropLine(Card,10);
    if (StrToInt('$'+edMode.Text) and MD_MVP)<>0 then
    begin
      for j:=0 to 2 do
      begin
        FillDropLine(MVPDrop[j],j+11);
      end;
    end;
  end;

  try
    imgMobSprite.Picture.LoadFromFile(RODB_GetSprite(idx,rosmMonster));
  except
  end;
end;

procedure TROFullCardMobForm.SaveForm;
var
  i,j:integer;
begin
  j:=StrToInt(edID.Text);
  i:=RODB_GetIndex(j,rosmMonster);

  if i<0 then exit; //!!!!

  with ROMobData[i] do
  begin
    Modified:=true;

    id   :=j;
    StrCache[name]:=pointer(edAEName.Text); //!!!!
    descr:=edIROName.Text;
    if edKROName.Text=descr then
      kro:=''
    else
      kro:=edKROName.Text;

    LVL    :=StrToInt(edLevel   .Text);
    HP     :=StrToInt(edHP      .Text);
    SP     :=StrToInt(edSP      .Text);
    EXP    :=StrToInt(edBaseEXP .Text);
    JEXP   :=StrToInt(edBaseJEXP.Text);
    Range1 :=StrToInt(edRange1  .Text);
    ATK1   :=StrToInt(edATK1    .Text);
    ATK2   :=StrToInt(edATK2    .Text);
    DEF    :=StrToInt(edDEF     .Text);
    MDEF   :=StrToInt(edMDEF    .Text);
    STR    :=StrToInt(edSTR     .Text);
    AGI    :=StrToInt(edAGI     .Text);
    VIT    :=StrToInt(edVIT     .Text);
    INT    :=StrToInt(edINT     .Text);
    DEX    :=StrToInt(edDEX     .Text);
    LUK    :=StrToInt(edLUK     .Text);
    Range2 :=StrToInt(edRange2  .Text);
    Range3 :=StrToInt(edRange3  .Text);
    Scale  :=cbScale.ItemIndex;
    Race   :=cbRace.ItemIndex;
    // (cbElementLevel.ItemIndex+1) or IntToStr(cbElementLevel.Text)
    Element:=cbElement.ItemIndex+(cbElementLevel.ItemIndex+1)*20;
    Mode   :=StrToInt('$'+edMode.Text);
    Speed  :=StrToInt(edSpeed   .Text);
    aDelay :=StrToInt(edaDelay  .Text);
    aMotion:=StrToInt(edaMotion .Text);
    dMotion:=StrToInt(eddMotion .Text);
    MEXP   :=StrToInt(edMVPEXP  .Text);
  end;

  with ROMobData[i] do
  begin
    if (StrToInt('$'+edMode.Text) and MD_MVP)<>0 then
    begin
      for j:=0 to 2 do
      begin
        MVPDrop[j].id  :=StrToIntDef(sgDrop.Cells[1,11+j],0);
        MVPDrop[j].rate:=StrToIntDef(sgDrop.Cells[3,11+j],0);
      end;
    end;
    for j:=0 to 8 do
    begin
      Drop[j].id  :=StrToIntDef(sgDrop.Cells[1,1+j],0);
      Drop[j].rate:=StrToIntDef(sgDrop.Cells[3,1+j],0);
    end;
    Card.id  :=StrToIntDef(sgDrop.Cells[1,10],0);
    Card.rate:=StrToIntDef(sgDrop.Cells[3,10],0);
  end;
end;

procedure TROFullCardMobForm.bbOKClick(Sender: TObject);
begin
  if bbClose.Visible then
  begin
    SaveForm;
  end;
  ModalResult:=mrOk; //!!
  Close; // for non-modal form
end;

procedure TROFullCardMobForm.SetROMode(arg:boolean);
begin
  tbOFF     .Enabled:=not arg;
  edID      .Enabled:=not arg;
  edIROName .ReadOnly:=arg;
  edKROName .ReadOnly:=arg;
  edAEName  .ReadOnly:=arg;
  edLevel   .Enabled:=not arg;
  edHP      .Enabled:=not arg;
  edSP      .Enabled:=not arg;
  edBaseEXP .Enabled:=not arg;
  edBaseJEXP.Enabled:=not arg;
  edRange1  .Enabled:=not arg;
  edATK1    .Enabled:=not arg;
  edATK2    .Enabled:=not arg;
  edDEF     .Enabled:=not arg;
  edMDEF    .Enabled:=not arg;
  edSTR     .Enabled:=not arg;
  edAGI     .Enabled:=not arg;
  edVIT     .Enabled:=not arg;
  edINT     .Enabled:=not arg;
  edDEX     .Enabled:=not arg;
  edLUK     .Enabled:=not arg;
  edRange2  .Enabled:=not arg;
  edRange3  .Enabled:=not arg;
  cbScale   .Enabled:=not arg;
  cbRace    .Enabled:=not arg;
  cbElement .Enabled:=not arg;
  cbElementLevel.Enabled:=not arg;
  edMode    .Enabled:=not arg;
  edSpeed   .Enabled:=not arg;
  edaDelay  .Enabled:=not arg;
  edaMotion .Enabled:=not arg;
  eddMotion .Enabled:=not arg;
  edMVPEXP  .Enabled:=(not arg) and ((StrToInt('$'+edMode.Text) and MD_MVP)<>0);
  if arg then
    sgDrop.Options:=sgDrop.Options-[goEditing]
  else
    sgDrop.Options:=sgDrop.Options+[goEditing];
end;

procedure TROFullCardMobForm.tbReadOnlyClick(Sender: TObject);
begin
  ReadOnly:=tbReadOnly.Checked;
end;

procedure TROFullCardMobForm.bbModeClick(Sender: TObject);
begin
  with CreateMaskForm(edMode,ROMobModes,sMobMode) do ShowModal;
{
  with tROMonsterModeForm.Create(self,StrToInt('$'+edMode.Text)) do
  begin
    if ShowModal=mrOk then
    begin
      edMode.Text:=IntToHex(Self.Tag,8);
    end;
  end;
}
end;

procedure TROFullCardMobForm.SetID(arg:integer);
begin
  if StrToInt(edID.Text)<>arg then
  begin
    ClearGrid;
    FillForm(arg);
  end;
end;

procedure TROFullCardMobForm.edRange1Change(Sender: TObject);
begin
  lblRange.Caption:=RORange[StrToInt(edRange1.Text)>2];
  SomethingChanged(Sender);
end;

procedure TROFullCardMobForm.sgDropDblClick(Sender: TObject);
begin
  if sgDrop.Col=2 then
  begin
    if sgDrop.Cells[1,sgDrop.Row]<>'' then
      with TROFullCardItemForm.Create(Self,
        RODB_GetIndex(StrToInt(sgDrop.Cells[1,sgDrop.Row]),rosmItem)) do Show;
  end;
end;

procedure TROFullCardMobForm.sgDropDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  ls:AnsiString;
  idx:integer;
begin
  if (aCol=2) and (aRow in [1..13]) and
    (UIntPtr(pics[aRow])<>UIntPtr(-1)) and
    (sgDrop.Cells[1,aRow]<>'') then
  begin
    if pics[aRow]=nil then
    begin
      idx:=RODB_GetIndex(StrToInt(sgDrop.Cells[1,aRow]),rosmItem);
      ls:=RODB_GetSprite(idx,1,rosmItem);
      if ls='' then exit;

      pics[aRow]:=TPicture.Create;
      try
        pics[aRow].LoadFromFile(ls);
      except
        pics[aRow]:=TPicture(-1);
        exit;
      end;
      sgDrop.RowHeights[aRow]:=pics[aRow].Height;
    end;
    sgDrop.Canvas.Draw(aRect.Left,aRect.Top,pics[aRow].Bitmap);
  end;
end;

procedure TROFullCardMobForm.edModeChange(Sender: TObject);
var
  isMVP:boolean;
begin
  isMVP:=(StrToInt('$'+edMode.Text) and MD_MVP)<>0;
  edMVPEXP.Enabled:=isMVP and edBaseEXP.Enabled;
  SomethingChanged(Sender);
end;

procedure TROFullCardMobForm.SomethingChanged(Sender: TObject);
begin
  bbOK.Caption:='&Save';
  bbClose.Visible:=true;
end;

procedure TROFullCardMobForm.sgDropEditingDone(Sender: TObject);
var
  i:integer;
begin
  if sgDrop.Col=3 then
  begin
    if sgDrop.Cells[3,sgDrop.Row]<>'' then
    begin
      i:=StrToIntDef(sgDrop.Cells[3,sgDrop.Row],0);
      if i>10000 then i:=10000;
      sgDrop.Cells[3,sgDrop.Row]:=IntToStr(i);
    end;
  end
  else if sgDrop.Col=1 then
  begin
    if sgDrop.Cells[1,sgDrop.Row]<>'' then
    begin
      i:=StrToIntDef(sgDrop.Cells[1,sgDrop.Row],0);
      sgDrop.Cells[1,sgDrop.Row]:=IntToStr(i);

      sgDrop.Cells[2,sgDrop.Row]:=
          RODB_GetText(
          RODB_GetIndex(i,rosmItem),rosmItem);

      if (pics[sgDrop.Row]<>nil) then
      begin
        if pics[sgDrop.Row]<>TPicture(-1) then
          pics[sgDrop.Row].Free;
        pics[sgDrop.Row]:=nil;
      end;
    end;
  end;
  SomethingChanged(Sender);
end;

procedure TROFullCardMobForm.btnMapClick(Sender: TObject);
begin
  with TRoRespListForm.Create(Self,rosmMonster,Tag) do Show;
end;

end.

