unit ROSearchItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, Grids, Types;

type

  { TROSearchItemForm }

  TROSearchItemForm = class(TForm)
    btnSearch: TButton;
    cbItemType: TComboBox;
    cbItemName: TComboBox;
    cbDropped: TCheckBox;
    cbHeadgear: TCheckBox;
    cbCostume: TCheckBox;
    clbHeadgear: TCheckListBox;
    clbEquipType: TCheckListBox;
    clbWeaponType: TCheckListBox;
    clbJob: TCheckListBox;
    edEffect: TEdit;
    edId: TEdit;
    lblId: TLabel;
    lblFound: TLabel;
    lblEffect: TLabel;
    lblItemName: TLabel;
    lblTo: TLabel;
    lblFrom: TLabel;
    lblItemType: TLabel;
    sgSearchResult: TStringGrid;
    procedure btnSearchClick(Sender: TObject);
    procedure cbHeadgearClick(Sender: TObject);
    procedure cbItemTypeChange(Sender: TObject);
    procedure clbClickCheck(Sender: TObject);
    procedure clbWeaponClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgSearchResultCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure sgSearchResultDblClick(Sender: TObject);
    procedure sgSearchResultDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    pnlLevel,
    pnlWeight,
    pnlBuyPrice,
    pnlSellPrice,
    pnlSlots,
    pnlDef,
    pnlMDef,
    pnlAtk,
    pnlMAtk,
    pnlWLevel:TPanel;
    procedure FillItemTypeList;
    procedure FillWeaponList;
    procedure FillEquipList;
    procedure FillJobList;
    procedure FillItemNames;
    procedure PrepareForm;
    procedure Reposition;
    function GetJobType:cardinal;
    function GetWeaponType:cardinal;
    function CheckWeaponType(amask:cardinal; aview:cardinal):boolean;
    function GetEquipType:cardinal;
    function GetHeadMask():cardinal;
    function CheckEquipType(amask:cardinal; aview:cardinal; aheadmask:cardinal):boolean;
    procedure SetCell(acol,aval:integer);

  public

  end;

var
  ROSearchItemForm: TROSearchItemForm;

implementation

uses
  common,
  ROFullCardItem,
  rodata,rodatagui,rodatatypes;

{$R *.lfm}

resourcestring
  sAllItemTypes = 'All Item types';
  sLevel       = 'Level';
  sWeight      = 'Weight';
  sBuyPrice    = 'Buy Price';
  sSellPrice   = 'Sell Price';
  sSlots       = 'Slots';
  sDef         = 'Def';
  sMDef        = 'MDef';
  sAtk         = 'Atk';
  sMAtk        = 'MAtk';
  sWeaponLevel = 'Weapon Level';
  sFound       = 'Search: found items = ';
  sTitle       = 'Title';
  sSeparate    = 'Separate';
  sAll         = 'All';
  sSome        = 'Some';

const
  DiaHeight = 26;
  DiaLeft   = 520;
const
  ResColumns: array [0..10] of AnsiString = (
    sTitle,
    sSlots,
    sLevel,
    sWeight,
    sAtk,
    sMatk,
    sDef,
    sMDef,
    sWeaponLevel,
    sSellPrice,
    sBuyPrice
  );

{ TROSearchItemForm }

function TROSearchItemForm.GetJobType:cardinal;
var
  i:integer;
begin
  result:=0;
  for i:=0 to clbJob.Items.Count-1 do
  begin
    if clbJob.Checked[i] then
      result:=result or cardinal(1 shl i);
  end;
end;

function TROSearchItemForm.GetEquipType:cardinal;
begin
  result:=0;
  if cbItemType.ItemIndex in [0,4+1,12+1] then
  begin
    // common equip data
    if clbEquipType.Checked[0] then result:=result or EQP_HAND_R  or EQP_SHADOW_WEAPON;
    if clbEquipType.Checked[1] then result:=result or EQP_ARMOR   or EQP_SHADOW_ARMOR;
    if clbEquipType.Checked[2] then result:=result or EQP_HAND_L  or EQP_SHADOW_SHIELD;
    if clbEquipType.Checked[3] then result:=result or EQP_GARMENT;
    if clbEquipType.Checked[4] then result:=result or EQP_SHOES   or EQP_SHADOW_SHOES;
    if clbEquipType.Checked[5] then result:=result or EQP_ACC_RL  or EQP_SHADOW_ACC_RL;

    if clbEquipType.Checked[6] then
    begin
      // headgear data
      if clbHeadgear.Checked[0] then result:=result or EQP_HEAD_TOP or EQP_COSTUME_HEAD_TOP;
      if clbHeadgear.Checked[1] then result:=result or EQP_HEAD_MID or EQP_COSTUME_HEAD_MID;
      if clbHeadgear.Checked[2] then result:=result or EQP_HEAD_LOW or EQP_COSTUME_HEAD_LOW;
    end;
  end
  else if cbItemType.ItemIndex=(6+1) then
  begin
    // common equip data
    if clbEquipType.Checked[0] then result:=result or EQP_HAND_R  {or EQP_SHADOW_WEAPON};
    if clbEquipType.Checked[1] then result:=result or EQP_ARMOR   {or EQP_SHADOW_ARMOR};
    if clbEquipType.Checked[2] then result:=result or EQP_HAND_L  {or EQP_SHADOW_SHIELD};
    if clbEquipType.Checked[3] then result:=result or EQP_GARMENT;
    if clbEquipType.Checked[4] then result:=result or EQP_SHOES   {or EQP_SHADOW_SHOES};
    if clbEquipType.Checked[5] then result:=result or EQP_ACC_RL  {or EQP_SHADOW_ACC_RL};
    if clbEquipType.Checked[6] then result:=result or EQP_HEAD;
  end;
end;

function TROSearchItemForm.GetHeadMask():cardinal;
begin
  case cbCostume.State of
    cbGrayed   : result:=EQP_HEAD or EQP_COSTUME_HEAD;
    cbChecked  : result:=EQP_COSTUME_HEAD;
    cbUnChecked: result:=EQP_HEAD;
  end;
end;

function TROSearchItemForm.CheckEquipType(amask:cardinal; aview:cardinal;
         aheadmask:cardinal):boolean;
begin

  // Headgear
  if ((amask and aheadmask)<>0) and ((aview and aheadmask)<>0) then
  begin
    case cbHeadgear.State of
      cbGrayed: begin
        result:=((amask or aview) and aheadmask)=(amask and aheadmask);
      end;
      cbChecked: begin
        result:=(amask and aheadmask)=(aview and aheadmask);
      end;
      cbUnChecked: begin
        result:=(amask and aheadmask and aview)<>0;
      end;
    end;
    if result then exit;
  end;
  // the rest
  result:=(amask and aview and not (EQP_HEAD or EQP_COSTUME_HEAD))<>0;
end;

function TROSearchItemForm.GetWeaponType:cardinal;
var
  i:integer;
begin
  result:=0;
  if cbItemType.ItemIndex in [0,5+1] then
  begin
    for i:=0 to clbWeaponType.Items.Count-1 do
    begin
      if clbWeaponType.Checked[i] then
        result:=result or cardinal(1 shl i);
    end;
  end;
end;

function TROSearchItemForm.CheckWeaponType(amask:cardinal; aview:cardinal):boolean;
var
  lmask,i:cardinal;
begin
  lmask:=1;
  for i:=1 to ROWeaponMaxType do
  begin
    if (aview=i) and ((amask and lmask)<>0) then exit(true);
    lmask:=lmask shl 1;
  end;
  result:=false;
end;

procedure TROSearchItemForm.SetCell(acol,aval:integer);
begin
  if aval>0 then
    sgSearchResult.Cells[acol,sgSearchResult.RowCount-1]:=IntToStr(aval);
end;

procedure TROSearchItemForm.btnSearchClick(Sender: TObject);
var
  ldata:pROItemData;
  lLevelFrom    ,lLevelTo,
  lWeightFrom   ,lWeightTo,
  lBuyPriceFrom ,lBuyPriceTo,
  lSellPriceFrom,lSellPriceTo,
  lSlotsFrom    ,lSlotsTo,
  lDefFrom      ,lDefTo,
  lMDefFrom     ,lMDefTo,
  lAtkFrom      ,lAtkTo,
  lMAtkFrom     ,lMAtkTo,
  lWLevelFrom   ,lWLevelTo:integer;
  lSell,lMDef,idx:integer;
  lJobMask, lHeadMask: cardinal;
  lWeaponView,lEquipView:integer;
  lName,lEffect:AnsiString;
  lItemType:integer;
  lId:integer;
  i,j,lrow:integer;
begin
  // get values
  if edId.Text<>'' then
    lId:=StrToInt(edId.Text)
  else
    lId:=0;

  // search
  sgSearchResult.Clear;
  sgSearchResult.BeginUpdate;
  sgSearchResult.RowCount:=1;
  sgSearchResult.Columns[1].Width:=200;

  if lId=0 then
  begin
    lItemType:=cbItemType.ItemIndex-1;
    lName  :=AnsiUpperCase(cbItemName.Text);
    lEffect:=AnsiUpperCase(edEffect.Text);

    lLevelTo    :=GetDiapazoneValue(pnlLevel    ,lLevelFrom);
    lWeightTo   :=GetDiapazoneValue(pnlWeight   ,lWeightFrom);
    lBuyPriceTo :=GetDiapazoneValue(pnlBuyPrice ,lBuyPriceFrom);
    lSellPriceTo:=GetDiapazoneValue(pnlSellPrice,lSellPriceFrom);
    lSlotsTo    :=GetDiapazoneValue(pnlSlots    ,lSlotsFrom);
    lDefTo      :=GetDiapazoneValue(pnlDef      ,lDefFrom);
    lMDefTo     :=GetDiapazoneValue(pnlMDef     ,lMDefFrom);
    lAtkTo      :=GetDiapazoneValue(pnlAtk      ,lAtkFrom);
    lMAtkTo     :=GetDiapazoneValue(pnlMatk     ,lMAtkFrom);
    lWLevelTo   :=GetDiapazoneValue(pnlWLevel   ,lWLevelFrom);

    lWeaponView:=GetWeaponType();
    lEquipView :=GetEquipType();
    lJobMask   :=GetJobType();
    lHeadMask  :=GetHeadMask();
  end;
  for i:=0 to High(ROItemData) do
  begin
    ldata:=@ROItemData[i];

    if lId<>0 then
    begin
      if ldata^.id<>lId then continue;

      idx:=Pos('bonus bMdef,',ldata^.Script);
      if idx>0 then
        lMDef:=common.StrToInt(PAnsiChar(ldata^.Script)+idx+12-1)
      else
        lMDef:=0;

      if ldata^.Sell=0 then
        lSell:=ldata^.Buy div 2
      else
        lSell:=ldata^.Sell;
    end
    ///////
    else
    begin
      // Item type
      if (lItemType>=0) and (lItemType<>ldata^.ItemType) then continue;
      // Weapon View
      if lWeaponView<>0 then
      begin
        if ldata^.ItemType<>5 then continue;
        if not CheckWeaponType(lWeaponView,ldata^.View) then continue;
      end;
      // Equip type
      if lEquipView<>0 then
      begin
        if not (ldata^.ItemType in [4,6,12]) then continue;
        if not CheckEquipType(lEquipView,ldata^.Loc,lHeadMask) then continue;
      end;
      // Job+Class
      if lJobMask<>0 then
      begin
        if (lJobMask and ldata^.Job)=0 then continue;
      end;
      // Item name
      if lName<>'' then
      begin
        if (Pos(lName,AnsiUpperCase(ldata^.descr))=0) and
           (Pos(lName,AnsiUpperCase(ldata^.local))=0) then continue;
      end;
      // Script
      if lEffect<>'' then
      begin
        if (Pos(lEffect,AnsiUpperCase(ldata^.Script          ))=0) and
           (Pos(lEffect,AnsiUpperCase(ldata^.OnEquip_Script  ))=0) and
           (Pos(lEffect,AnsiUpperCase(ldata^.OnUnEquip_Script))=0) then continue;
      end;
      // Dropped
      if cbDropped.State<>cbGrayed then
      begin
        if GetNextDropMonster(ldata^.id,-1,j)>0 then // droppable
        begin
          if cbDropped.State=cbUnChecked then continue;
        end
        else if cbDropped.State=cbChecked then continue;
      end;
      // level
      if (lLevelTo  >=0) then if lLevelTo  <ldata^.eLevel then continue;
      if (lLevelFrom>=0) then if lLevelFrom>ldata^.eLevel then continue
      else if ldata^.MaxLevel>0 then if lLevelFrom>ldata^.MaxLevel then continue;
      // Weight
      if (lWeightFrom>=0) then if lWeightFrom>ldata^.Weight then continue;
      if (lWeightTo  >=0) then if lWeightTo  <ldata^.Weight then continue;
      // Price
      if (lBuyPriceFrom>=0) then if cardinal(lBuyPriceFrom)>ldata^.Buy then continue;
      if (lBuyPriceTo  >=0) then if cardinal(lBuyPriceTo  )<ldata^.Buy then continue;
      if ldata^.Sell=0 then
        lSell:=ldata^.Buy div 2
      else
        lSell:=ldata^.Sell;
      if (lSellPriceFrom>=0) then if lSellPriceFrom>lSell then continue;
      if (lSellPriceTo  >=0) then if lSellPriceTo  <lSell then continue;
      // Slots
      if (lSlotsFrom>=0) then if lSlotsFrom>ldata^.Slots then continue;
      if (lSlotsTo  >=0) then if lSlotsTo  <ldata^.Slots then continue;
      // Def
      if (lDefFrom>=0) then if lDefFrom>ldata^.Def then continue;
      if (lDefTo  >=0) then if lDefTo  <ldata^.Def then continue;
      // MDef (check script and save value)
      idx:=Pos('bonus bMdef,',ldata^.Script);
      if idx>0 then
        lMDef:=common.StrToInt(PAnsiChar(ldata^.Script)+idx+12-1)
      else
        lMDef:=0;
      if (lMDefFrom>=0) or (lMDefTo>=0) then
      begin
        if (lMDefFrom>=0) then if lMDefFrom>lMDef then continue;
        if (lMDefTo  >=0) then if lMDefTo  <lMDef then continue;
      end;
      // Atk
      if (lAtkFrom>=0) then if lAtkFrom>ldata^.Atk then continue;
      if (lAtkTo  >=0) then if lAtkTo  <ldata^.Atk then continue;
      // MAtk
      if (lMAtkFrom>=0) then if lMAtkFrom>ldata^.MAtk then continue;
      if (lMAtkTo  >=0) then if lMAtkTo  <ldata^.MAtk then continue;
      // Weapon Level
      if (lWLevelFrom>=0) then if lWLevelFrom>ldata^.wLevel then continue;
      if (lWLevelTo  >=0) then if lWLevelTo  <ldata^.wLevel then continue;
    end;

    lrow:=sgSearchResult.RowCount;
    sgSearchResult.RowCount:=sgSearchResult.RowCount+1;

    // set height for subitems
    sgSearchResult.Cells[sgSearchResult.ColCount-1,lrow]:=IntToStr(i);
    sgSearchResult.Cells[1,lrow]:=RODB_GetText(i,rosmItem); //ldata^.Descr;
    SetCell( 2, ldata^.Slots);
    SetCell( 3, ldata^.eLevel);
    SetCell( 4, ldata^.Weight);
    SetCell( 5, ldata^.Atk);
    SetCell( 6, ldata^.MAtk);
    SetCell( 7, ldata^.Def);
    SetCell( 8, lMDef);
    SetCell( 9, ldata^.wLevel);
    SetCell(10, lSell);
    SetCell(11, ldata^.Buy);

    if lId<>0 then break;
  end;

  sgSearchResult.EndUpdate;
  lblFound.Caption:=sFound+IntToStr(sgSearchResult.RowCount-1);
end;

procedure TROSearchItemForm.cbHeadgearClick(Sender: TObject);
begin
  case cbHeadgear.State of
    cbGrayed   : cbHeadgear.Caption:=sSeparate;
    cbChecked  : cbHeadgear.Caption:=sAll;
    cbUnChecked: cbHeadgear.Caption:=sSome;
  end;
  FillItemNames;
end;

procedure TROSearchItemForm.cbItemTypeChange(Sender: TObject);
begin
  case cbItemType.ItemIndex of
    0: begin // All items
      pnlSlots .Visible:=true;
      pnlDef   .Visible:=true;
      pnlMDef  .Visible:=true;
      pnlAtk   .Visible:=true;
      pnlMAtk  .Visible:=true;
      pnlWLevel.Visible:=true;
      clbEquipType .Visible:=true;
      clbHeadgear  .Visible:=true;
      cbHeadgear   .Visible:=true;
      cbCostume    .Visible:=true;
      clbWeaponType.Visible:=true;
    end;

    4+1,12+1: begin // equip+shadow
      pnlSlots .Visible:=true;
      pnlDef   .Visible:=true;
      pnlMDef  .Visible:=true;
      pnlAtk   .Visible:=false;
      pnlMAtk  .Visible:=false;
      pnlWLevel.Visible:=false;
      clbEquipType .Visible:=true;
      clbHeadgear  .Visible:=true;
      cbHeadgear   .Visible:=true;
      cbCostume    .Visible:=true;
      clbWeaponType.Visible:=false;
    end;

    5+1: begin // weapon
      pnlSlots .Visible:=true;
      pnlDef   .Visible:=false;
      pnlMDef  .Visible:=false;
      pnlAtk   .Visible:=true;
      pnlMAtk  .Visible:=true;
      pnlWLevel.Visible:=true;
      clbEquipType .Visible:=false;
      clbHeadgear  .Visible:=false;
      cbHeadgear   .Visible:=false;
      cbCostume    .Visible:=false;
      clbWeaponType.Visible:=true;
    end;

    6+1: begin // cards
      pnlSlots .Visible:=false;
      pnlDef   .Visible:=false;
      pnlMDef  .Visible:=false;
      pnlAtk   .Visible:=false;
      pnlMAtk  .Visible:=false;
      pnlWLevel.Visible:=false;
      clbEquipType .Visible:=true;
      clbHeadgear  .Visible:=false;
      cbHeadgear   .Visible:=false;
      cbCostume    .Visible:=false;
      clbWeaponType.Visible:=false;
    end
  else
    pnlSlots .Visible:=false;
    pnlDef   .Visible:=false;
    pnlMDef  .Visible:=false;
    pnlAtk   .Visible:=false;
    pnlMAtk  .Visible:=false;
    pnlWLevel.Visible:=false;
    clbEquipType .Visible:=false;
    clbHeadgear  .Visible:=false;
    cbHeadgear   .Visible:=false;
    cbCostume    .Visible:=false;
    clbWeaponType.Visible:=false;
  end;

  Reposition;
  FillItemNames;
end;

procedure TROSearchItemForm.clbClickCheck(Sender: TObject);
begin
  if cbItemType.ItemIndex in [4+1,12+1,6+1] then FillItemNames;
end;

procedure TROSearchItemForm.clbWeaponClickCheck(Sender: TObject);
begin
  if cbItemType.ItemIndex in [5+1] then FillItemNames;
end;

procedure TROSearchItemForm.FillItemNames;
var
  i,lItemType,lWeaponType,lEquipType:integer;
begin
  lItemType:=cbItemType.ItemIndex-1;
  lWeaponType:=GetWeaponType;
  lEquipType :=GetEquipType;
  cbItemName.Items.BeginUpdate;
  cbItemName.Items.Clear;
  cbItemName.Items.Capacity:=Length(ROItemData)*2;
  for i:=0 to High(ROItemData) do
  begin
    if (lItemType>=0) and (lItemType<>ROItemData[i].ItemType) then continue;
    case lItemType of
      4,12: begin
        if (lEquipType<>0) and
           not CheckEquipType(lEquipType,ROItemData[i].Loc,
               EQP_HEAD or EQP_COSTUME_HEAD) then continue;
      end;
      5: begin
        if (lWeaponType<>0) and
            not CheckWeaponType(lWeaponType,ROItemData[i].View) then continue;
      end;
    end;
    cbItemName.Items.Add(ROItemData[i].descr);
    if ROItemData[i].local<>'' then
      cbItemName.Items.Add(ROItemData[i].local);
  end;
  cbItemName.Items.EndUpdate;
end;

procedure TROSearchItemForm.FillItemTypeList;
var
  i:integer;
begin
  cbItemType.Clear;
  cbItemType.Items.Add(sAllItemTypes);
  for i:=0 to High(ROItemType) do
    cbItemType.Items.Add(ROItemType[i]);
  cbItemType.ItemIndex:=0;
end;

procedure TROSearchItemForm.FillWeaponList;
var
  i:integer;
begin
  clbWeaponType.Items.Clear;
  // without barehand and combos
  for i:=1 to ROWeaponMaxType-1 do
    clbWeaponType.Items.Add(ROWeaponView[i]);
end;

procedure TROSearchItemForm.FillEquipList;
begin
  clbEquipType.Items.Clear;
  clbEquipType.Items.Add(sLocWeapon);
  clbEquipType.Items.Add(sLocArmor);
  clbEquipType.Items.Add(sLocShield);
  clbEquipType.Items.Add(sLocGarment);
  clbEquipType.Items.Add(sLocFootgear);
  clbEquipType.Items.Add(sAccess);
  clbEquipType.Items.Add(sHeadgear);

  clbHeadgear.Items.Clear;
  clbHeadgear.Items.Add(sUpper);
  clbHeadgear.Items.Add(sMiddle);
  clbHeadgear.Items.Add(sLower);
  cbHeadgearClick(cbHeadgear);
end;

procedure TROSearchItemForm.FillJobList;
var
  i:integer;
begin
  clbJob.Items.Clear;
  for i:=0 to High(ROJobs) do
    clbJob.Items.Add(ROJobs[i].name);
end;

procedure TROSearchItemForm.PrepareForm;
var
  col:TGridColumn;
  i,ltop:integer;
begin
  cbDropped.State:=cbGrayed;
  // Result Grid
  sgSearchResult.BeginUpdate;
  // for sprite
  with sgSearchResult.Columns.Add do
  begin
    ReadOnly     :=true;
    SizePriority :=0;
    Width        :=28;
    Title.Caption:='';
  end;
  // main data
  for i:=0 to High(ResColumns) do
  begin
    col:=sgSearchResult.Columns.Add;
    with col do
    begin
      ReadOnly:=true;
      if i=0 then
      begin
        SizePriority:=0;
      end;
      Title.Caption     :=ResColumns[i];
//      Title.Alignment   :=taCenter;
      Title.PrefixOption:=poHeaderClick;
      Title.Multiline   :=true;
      Width:=80;
    end;
  end;
  // for index
  with sgSearchResult.Columns.Add do
  begin
    Visible:=false;
  end;
  sgSearchResult.EndUpdate;

  // Diapazone values
  ltop:=20;
  pnlLevel:=CreateDiapazonePanel(Self,sLevel);
  with pnlLevel do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlWeight:=CreateDiapazonePanel(Self,sWeight);
  with pnlWeight do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlBuyPrice:=CreateDiapazonePanel(Self,sBuyPrice);
  with pnlBuyPrice do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlSellPrice:=CreateDiapazonePanel(Self,sSellPrice);
  with pnlSellPrice do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  // equip+weapon
  pnlSlots :=CreateDiapazonePanel(Self,sSlots      ); pnlSlots .Left:=DiaLeft;
  // equip
  pnlDef   :=CreateDiapazonePanel(Self,sDef        ); pnlDef   .Left:=DiaLeft;
  pnlMDef  :=CreateDiapazonePanel(Self,sMDef       ); pnlMDef  .Left:=DiaLeft;
  // weapon
  pnlAtk   :=CreateDiapazonePanel(Self,sAtk        ); pnlAtk   .Left:=DiaLeft;
  pnlMAtk  :=CreateDiapazonePanel(Self,sMAtk       ); pnlMAtk  .Left:=DiaLeft;
  pnlWLevel:=CreateDiapazonePanel(Self,sWeaponLevel); pnlWLevel.Left:=DiaLeft;
end;

procedure TROSearchItemForm.Reposition;
var
  lleft,ltop:integer;
begin
  //-- Check box lists
  lleft:=clbJob.Left+clbJob.Width+8;
  if clbWeaponType.Visible then
  begin
    clbWeaponType.Left:=lleft;
    inc(lleft,clbWeaponType.Width+8);
  end;
  if clbEquipType.Visible then
  begin
    clbEquipType.Left:=lleft;
    inc(lleft,clbEquipType.Width+8);

    cbCostume  .Left:=lleft;
    cbHeadgear .Left:=lleft;
    clbHeadgear.Left:=lleft;
  end;

  //-- Diapazones
  ltop:=20+4*DiaHeight; // skip default panels;
  // equip+weapon
  if pnlSlots.Visible then
  begin
    pnlSlots.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
  // equip
  if pnlDef.Visible then
  begin
    pnlDef.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
  if pnlMDef.Visible then
  begin
    pnlMDef.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
  // weapon
  if pnlAtk.Visible then
  begin
    pnlAtk.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
  if pnlMAtk.Visible then
  begin
    pnlMAtk.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
  if pnlWLevel.Visible then
  begin
    pnlWLevel.Top:=ltop;
    inc(ltop,DiaHeight);
  end;
end;

procedure TROSearchItemForm.FormCreate(Sender: TObject);
begin
  PrepareForm;
  FillWeaponList;
  FillEquipList;
  FillJobList;
  FillItemTypeList;
  cbItemTypeChange(cbItemType);
end;

procedure TROSearchItemForm.sgSearchResultCompareCells(Sender: TObject; ACol,
  ARow, BCol, BRow: Integer; var Result: integer);
var
  s1,s2:ansistring;
begin
  s1:=(Sender as TStringGrid).Cells[ACol,ARow];
  s2:=(Sender as TStringGrid).Cells[BCol,BRow];
  if (ACol<>1) and (BCol<>1) then
  begin
    result:=StrToIntDef(s1,0)-
            StrToIntDef(s2,0);
  end
  else
    result:=CompareStr(s1,s2);

  if (Sender as TStringGrid).SortOrder=soDescending then
    result:=-result;
end;

procedure TROSearchItemForm.sgSearchResultDblClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=StrToInt(sgSearchResult.Cells[sgSearchResult.ColCount-1,sgSearchResult.Row]);
  with TROFullCardItemForm.Create(Self,idx) do Show;
end;

procedure TROSearchItemForm.sgSearchResultDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  ls:String;
  lp:TPicture;
  idx:integer;
begin
  if (aRow>0) and (aCol=0) then
  begin
    idx:=StrToInt(sgSearchResult.Cells[sgSearchResult.ColCount-1,aRow]);
    ls:=RODB_GetSprite(idx,1,rosmItem);
    if ls='' then exit;

    lp:=TPicture.Create;
    try
      try
        lp.LoadFromFile(ls);
      except
        exit;
      end;
      sgSearchResult.RowHeights[aRow]:=lp.Height+4;
      sgSearchResult.Canvas.Draw(aRect.Left+2,aRect.Top+2,lp.Bitmap);
    finally
      lp.Free;
    end;
  end;
end;

end.

