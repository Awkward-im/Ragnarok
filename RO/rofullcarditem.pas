unit ROFullCardItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Buttons,
  rodatatypes;

type

  { TROFullCardItemForm }

  TROFullCardItemForm = class(TForm)
    bbClose: TBitBtn;
    bbOK: TBitBtn;
    btnJobMode: TButton;
    btnClassMode: TButton;
    btnTradeMode: TButton;
    btnLocMode: TButton;
    cbRefineable: TCheckBox;
    cbItemType: TComboBox;
    cbGender: TComboBox;
    cbSlots: TComboBox;
    cbView: TComboBox;
    edATK: TEdit;
    edBuy: TEdit;
    edClass: TEdit;
    edeLevel: TEdit;
    edTrade: TEdit;
    edMATK: TEdit;
    edMaxLevel: TEdit;
    edRange: TEdit;
    edScript: TEdit;
    edLocation: TEdit;
    edScriptEquip: TEdit;
    edScriptUnequip: TEdit;
    edSell: TEdit;
    edWeight: TEdit;
    edName: TEdit;
    edDEF: TEdit;
    edJob: TEdit;
    edAEName: TEdit;
    edID: TEdit;
    edwLevel: TEdit;
    gbLevel: TGroupBox;
    gbPrice: TGroupBox;
    gbWeapon: TGroupBox;
    imgSubItemSprite: TImage;
    imgItemSprite: TImage;
    lblTrade: TLabel;
    lblATK: TLabel;
    lblBuy: TLabel;
    lbleLevel: TLabel;
    lblMATK: TLabel;
    lblMaxLevel: TLabel;
    lblName: TLabel;
    lblDroppedBy: TLabel;
    lblRange: TLabel;
    lblScriptEquip: TLabel;
    lblScriptUnequip: TLabel;
    lblSell: TLabel;
    lblView: TLabel;
    lblScript: TLabel;
    lblUpper: TLabel;
    lblGender: TLabel;
    lblLocation: TLabel;
    lblWeight: TLabel;
    lblDEF: TLabel;
    lblSlots: TLabel;
    lblJob: TLabel;
    lblAEName: TLabel;
    lblID: TLabel;
    lblType: TLabel;
    lblwLevel: TLabel;
    sgDrop: TStringGrid;
    tbOFF: TToggleBox;
    tbReadOnly: TToggleBox;
    procedure bbOKClick(Sender: TObject);
    procedure btnClassModeClick(Sender: TObject);
    procedure btnJobModeClick(Sender: TObject);
    procedure btnLocModeClick(Sender: TObject);
    procedure btnTradeModeClick(Sender: TObject);
    procedure cbItemTypeChange(Sender: TObject);
    procedure edTradeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sgDropDblClick(Sender: TObject);
    procedure SomethingChanged(Sender: TObject);
    procedure tbReadOnlyClick(Sender: TObject);
  private
    ChangedMask:cardinal;

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
  ROFullCardItemForm: TROFullCardItemForm;

implementation

uses
  ROFullCardMob,
  rodatagui,rodata;

{$R *.lfm}

resourcestring
  sLocation = 'Equipment location';
  sJob      = 'Jobs';
  sClass    = 'Classes';
  sTrade    = 'Trade mode';

{ TROFullCardItemForm }

procedure TROFullCardItemForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

constructor TROFullCardItemForm.Create(AOwner: TComponent; anIdx: integer; areadonly: boolean);
begin
  inherited Create(AOwner);

  Caption:=RODB_GetTitle(anIdx,rosmItem);

  PrepareForm;
  FillForm(anIdx);

  AddScriptButton(edScript);
  if edScriptEquip  .Visible then AddScriptButton(edScriptEquip);
  if edScriptUnEquip.Visible then AddScriptButton(edScriptUnEquip);

  ReadOnly:=areadonly;
  bbOk.Caption:='&OK';
  bbClose.Visible:=false;
  tbReadOnly.Checked:=areadonly;
  tbReadOnly.Visible:=ROAllowEditCard;

  ChangedMask:=0;
end;

procedure TROFullCardItemForm.PrepareForm;
var
  i:integer;
begin
  cbItemType.Clear;
  for i:=0 to High(ROItemType) do
    cbItemType.Items.Add(ROItemType[i]);

  cbGender.Clear;
  for i:=0 to High(ROGender) do
    cbGender.Items.Add(ROGender[i]);
end;

procedure TROFullCardItemForm.FillForm(idx:integer);
var
  j,ldata:integer;
begin
  if idx<0 then
  begin
    cbItemType.ItemIndex:=0;
    cbItemTypeChange(cbItemType);
    exit;
  end;
  with ROItemData[idx] do
  begin
    tbOFF.Checked:=Disabled;
    edID    .Text:=IntToStr(id);
{
    if (Length(name)>3) and (name[Length(name)-2]='[') then
      edName.Text:=Copy(name,1,Length(name)-3)
    else
}
      edName.Text:=descr;
    edAEName.Text:=StrCache[name]{RODB_Text[anIdx,rosmItem]};

    cbItemType.ItemIndex:=ItemType;
    cbItemTypeChange(cbItemType);
{
    if (Buy=0) and (Sell<>0) then
      ldata:=Sell * 2
    else
    }
      ldata:=Buy;
    edBuy.Text:=IntToStr(ldata);

    if (Sell=0) and (Buy<>0) then
      ldata:=Buy div 2
    else
      ldata:=Sell;
    edSell    .Text     :=IntToStr(ldata);
    edWeight  .Text     :=IntToStr(Weight);
    edATK     .Text     :=IntToStr(ATK);
    edMATK    .Text     :=IntToStr(MATK);
    edDEF     .Text     :=IntToStr(DEF);
    edRange   .Text     :=IntToStr(Range);
    cbSlots   .ItemIndex:=Slots;
    edJob     .Text     :=IntToHex(Job,8);
    edClass   .Text     :=IntToHex(CharClass,8);
    cbGender  .ItemIndex:=Gender;
    edLocation.Text     :=IntToHex(Loc,8);
    edwLevel  .Text     :=IntToStr(wLevel);
    edeLevel  .Text     :=IntToStr(eLevel);
    edMaxLevel.Text     :=IntToStr(MaxLevel);
    cbRefineable.Checked:=Refineable;
    edTrade   .Text     :=IntToHex(Trade,8);

    if cbView.Items.Count=0 then
      cbView.Text:=IntToStr(View)
    else
      cbView.ItemIndex:=View;

    edScript       .Text:=Script;
    edScriptEquip  .Text:=OnEquip_Script;
    edScriptUnEquip.Text:=OnUnEquip_Script;
  end;

  // Fill drop grid
  sgDrop.Clear;
  j:=-1;
  sgDrop.RowCount:=1;
  repeat
    j:=GetNextDropMonster(ROItemData[idx].id,j,ldata);
    if j<0 then break;
    sgDrop.RowCount:=sgDrop.RowCount+1;
    sgDrop.Cells[0,sgDrop.RowCount-1]:=RODB_GetID  (j,rosmMonster);
    sgDrop.Cells[1,sgDrop.RowCount-1]:=RODB_GetText(j,rosmMonster);
    sgDrop.Cells[2,sgDrop.RowCount-1]:=IntToStr(ldata);
    sgDrop.Cells[3,sgDrop.RowCount-1]:=IntToStr(j);
  until false;

  try
    imgItemSprite.Picture.LoadFromFile(RODB_GetSprite(idx,0,rosmItem));
  except
  end;
  try
    imgSubItemSprite.Picture.LoadFromFile(RODB_GetSprite(idx,1,rosmItem));
  except
  end;
end;

procedure TROFullCardItemForm.SaveForm;
var
  i:integer;
begin
  i:=RODB_GetIndex(StrToIntDef(edID.Text,0),rosmItem);
  if i<0 then exit; //!!!!

  if (ChangedMask and 2)<>0 then
  begin
    ROItemData[i].Trade:=StrToIntDef('$'+edTrade.Text,0);
    ROItemData[i].Modified:=ROItemData[i].Modified or 2;
  end;

  if (ChangedMask and 1)<>0 then
  begin
    with ROItemData[i] do
    begin
      Modified:=Modified or 1;

      id   :=StrToInt(edID.Text);
      StrCache[name]:=pointer(edAEName.Text);
      descr:=edName.Text;
{
      if cbSlots.ItemIndex>0 then
        descr:=descr+'['+cbSlots.Text+']';
}
      ItemType  :=cbItemType.ItemIndex;
      Buy       :=StrToIntDef(edBuy     .Text,0);
      Sell      :=StrToIntDef(edSell    .Text,0);
      if Sell=Buy div 2 then Sell:=0;

      Weight    :=StrToIntDef(edWeight  .Text,0);
      ATK       :=StrToIntDef(edATK     .Text,0);
      MATK      :=StrToIntDef(edMATK    .Text,0);
      DEF       :=StrToIntDef(edDEF     .Text,0);
      Range     :=StrToIntDef(edRange   .Text,0);
      if cbSlots.ItemIndex>=0 then
        Slots:=cbSlots.ItemIndex
      else
        Slots:=0;
      Job       :=Cardinal(StrToIntDef('$'+edJob .Text,0));
      CharClass :=StrToIntDef(edClass   .Text,0);
      Gender    :=cbGender.ItemIndex;
      Loc       :=StrToIntDef(edLocation.Text,0);
      wLevel    :=StrToIntDef(edwLevel  .Text,0);
      eLevel    :=StrToIntDef(edeLevel  .Text,0);
      MaxLevel  :=StrToIntDef(edMaxLevel.Text,0);
      Refineable:=cbRefineable.Checked;

      if cbView.Items.Count=0 then
        View:=StrToIntDef(cbView.Text,0)
      else if cbView.ItemIndex>=0 then
        View:=cbView.ItemIndex
      else
        View:=0;

      Script          :=edScript       .Text;
      OnEquip_Script  :=edScriptEquip  .Text;
      OnUnEquip_Script:=edScriptUnEquip.Text;
    end;
  end;
end;

procedure TROFullCardItemForm.bbOKClick(Sender: TObject);
begin
  if bbClose.Visible then
  begin
    SaveForm;
  end;
  ModalResult:=mrOk; //!!
  Close; // for non-modal form
end;

procedure TROFullCardItemForm.sgDropDblClick(Sender: TObject);
begin
  with TROFullCardMobForm.Create(Self,
    StrToInt(sgDrop.Cells[3,sgDrop.Row])) do Show;
end;

procedure TROFullCardItemForm.SetROMode(arg:boolean);
begin
  tbOFF          .Enabled:=not arg;
  edID           .Enabled:=not arg;
  edName         .ReadOnly:=arg;
  edAEName       .ReadOnly:=arg;
  cbItemType     .Enabled:=not arg;
  edBuy          .Enabled:=not arg;
  edSell         .Enabled:=not arg;
  edWeight       .Enabled:=not arg;
  edATK          .Enabled:=not arg;
  edMATK         .Enabled:=not arg;
  edDEF          .Enabled:=not arg;
  edRange        .Enabled:=not arg;
  cbSlots        .Enabled:=not arg;
  edJob          .Enabled:=not arg;
  edClass        .Enabled:=not arg;
  cbGender       .Enabled:=not arg;
  edLocation     .Enabled:=not arg;
  edwLevel       .Enabled:=not arg;
  edeLevel       .Enabled:=not arg;
  edMaxLevel     .Enabled:=not arg;
  cbRefineable   .Enabled:=not arg;
  cbView         .Enabled:=not arg;
  edScript       .ReadOnly:=arg;
  edScriptEquip  .ReadOnly:=arg;
  edScriptUnequip.ReadOnly:=arg;
  edTrade        .Enabled:=not arg;
end;

procedure TROFullCardItemForm.tbReadOnlyClick(Sender: TObject);
begin
  ReadOnly:=tbReadOnly.Checked;
end;

procedure TROFullCardItemForm.SetID(arg:integer);
begin
  if StrToInt(edID.Text)<>arg then
  begin
//    ClearGrid;
    FillForm(arg);
  end;
end;

procedure TROFullCardItemForm.SomethingChanged(Sender: TObject);
begin
  bbOK.Caption:='&Save';
  bbClose.Visible:=true;
  ChangedMask:=ChangedMask or 1;
end;

procedure TROFullCardItemForm.edTradeChange(Sender: TObject);
begin
  bbOK.Caption:='&Save';
  bbClose.Visible:=true;
  ChangedMask:=ChangedMask or 2;
end;

procedure TROFullCardItemForm.cbItemTypeChange(Sender: TObject);
var
  i:integer;
  showadd:boolean;
begin
  case cbItemType.ItemIndex of
    4: begin // equip
      cbSlots.Clear;
      for i:=0 to 2 do
        cbSlots.Items.Add(IntToStr(i));
    end;

    5: begin // weapon
      cbSlots.Clear;
      for i:=0 to 4 do
        cbSlots.Items.Add(IntToStr(i));
      cbView.Clear;
      for i:=0 to High(ROWeaponView) do
        cbView.Items.Add(ROWeaponView[i]);
    end;

    10: begin // ammo
      cbView.Clear;
      for i:=0 to High(ROAmmoView) do
        cbView.Items.Add(ROAmmoView[i]);
    end;
  end;

  showadd:=cbItemType.ItemIndex in [4,5];
  lblSlots    .Visible:=showadd;
  cbSlots     .Visible:=showadd;
  cbRefineable.Visible:=showadd;

  gbWeapon.Visible:=cbItemType.ItemIndex in [5,10];
  lblDEF  .Visible:=cbItemType.ItemIndex=4;
  edDEF   .Visible:=cbItemType.ItemIndex=4;

  lblRange .Visible:=cbItemType.ItemIndex<>10;
  edRange  .Visible:=cbItemType.ItemIndex<>10;
  lblwLevel.Visible:=cbItemType.ItemIndex<>10;
  edwLevel .Visible:=cbItemType.ItemIndex<>10;

  showadd:=cbItemType.ItemIndex in [4,5,6,10,18];
  lblScriptEquip  .Visible:=showadd;
  edScriptEquip   .Visible:=showadd;
  lblScriptUnEquip.Visible:=showadd;
  edScriptUnEquip .Visible:=showadd;
  lblLocation     .Visible:=showadd;
  edLocation      .Visible:=showadd;
  btnLocMode      .Visible:=showadd;

  SomethingChanged(Sender);
end;

procedure TROFullCardItemForm.btnLocModeClick(Sender: TObject);
begin
  with CreateMaskForm(edLocation,ROLocations,sLocation) do ShowModal;
end;

procedure TROFullCardItemForm.btnJobModeClick(Sender: TObject);
begin
  with CreateMaskForm(edJob,ROJobs,sJob) do ShowModal;
end;

procedure TROFullCardItemForm.btnClassModeClick(Sender: TObject);
begin
  with CreateMaskForm(edClass,ROClasses,sClass) do ShowModal;
end;

procedure TROFullCardItemForm.btnTradeModeClick(Sender: TObject);
begin
  with CreateMaskForm(edTrade,ROTrade,sTrade) do ShowModal;
end;

end.

