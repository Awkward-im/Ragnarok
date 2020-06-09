unit ROWizShop;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls,
  ROWizNPCBase;

type

  { TNPCShopForm }

  TNPCShopForm = class(TForm)
    btnDelete: TButton;
    btnAdd: TButton;
    btnInsert: TButton;
    cbItemName: TComboBox;
    cbShopType: TComboBox;
    edQuantity: TEdit;
    edShopPrice: TEdit;
    lQuantity: TLabel;
    lShopType: TLabel;
    lvShopItemList: TListView;
    lItemID: TLabel;
    lShopPrice: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure cbShopTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    NPCForm:TNPCBase;

    procedure FillItemList;
    procedure FillShopStyleList;
  public

  end;

var
  NPCShopForm: TNPCShopForm=nil;

implementation

uses
  rodatagui,
  rodatatypes;

{$R *.lfm}

resourcestring
  sQuantity = 'Quantity';
  sDiscount = 'Discount';

{ TNPCShopForm }

procedure TNPCShopForm.FillItemList;
var
  s:String;
  i,cnt:integer;
begin
  cbItemName.Clear;
  cnt:=RODB_GetCount(rosmItem);
  cbItemName.Items.BeginUpdate;
  cbItemName.Items.Capacity:=cnt*2;
  for i:=0 to cnt-1 do
  begin
    cbItemName.Items.AddObject(RODB_GetText(i,0,rosmItem),TObject(i));
    s:=RODB_GetText(i,-1,rosmItem);
    if s<>'' then
      cbItemName.Items.AddObject(s,TObject(i));
  end;
  cbItemName.Items.EndUpdate;
end;

procedure TNPCShopForm.FillShopStyleList;
var
  i:integer;
begin
  cbShopType.Clear;
  for i:=0 to ROShopTypeCount-1 do
    cbShopType.Items.Add(cRoShopType[i]);
  cbShopType.ItemIndex:=0;
end;

procedure TNPCShopForm.FormCreate(Sender: TObject);
begin
  AddListButton(cbItemName,rosmItem);

  NPCForm:=TNPCBase.Create(Self);
  Self.Height:=Self.Height+NPCForm.Height;
  Self.Constraints.MinHeight:=Self.Height;

  NPCForm.Parent :=Self;
  NPCForm.Visible:=true;
  NPCForm.Left   :=0;
  NPCForm.Top    :=0;
  NPCForm.Anchors:=[akLeft,akRight,akTop,akBottom];
  NPCForm.Align  :=alTop;

  FillItemList;
  FillShopStyleList;
end;

procedure TNPCShopForm.cbShopTypeChange(Sender: TObject);
begin
  case cbShopType.ItemIndex of
    0,1: begin
      edQuantity.Visible:=false;
      lQuantity .Visible:=false;
      lvShopItemList.Columns[2].Visible:=false;
    end;
    2,3: begin
      edQuantity.Visible:=true;
      lQuantity .Visible:=true;
      lQuantity .Caption:=sDiscount;
      lvShopItemList.Columns[2].Visible:=false;
    end;
    4: begin
      edQuantity.Visible:=true;
      lQuantity .Visible:=true;
      lQuantity .Caption:=sQuantity;
      lvShopItemList.Columns[2].Visible:=true;
    end;
  end;
end;

procedure TNPCShopForm.btnAddClick(Sender: TObject);
var
  li:TListItem;
  idx:integer;
begin
  idx:=GetListIndex(cbItemName,rosmItem);
  if idx<0 then exit;

  li:=lvShopItemList.Items.Add;
  li.Data   :=pointer(RODB_GetIDNum(idx,rosmItem));
  li.Caption:=        RODB_GetTitle(idx,rosmItem);

  if edShopPrice.Text='' then
    li.SubItems.Add('-1')
  else
    li.SubItems.Add(edShopPrice.Text);
  if (edQuantity.Visible) and (edQuantity.Text<>'') then
    li.SubItems.Add(edQuantity.Text)
  else
    li.SubItems.Add('1');

  lvShopItemList.ItemIndex:=li.Index;

  btnDelete.Enabled:=true;
  btnInsert.Enabled:=true;
end;

procedure TNPCShopForm.btnDeleteClick(Sender: TObject);
begin
  lvShopItemList.Items.Delete(lvShopItemList.ItemIndex);

  btnDelete.Enabled:=lvShopItemList.Items.Count>0;
  btnInsert.Enabled:=lvShopItemList.Items.Count>0;
end;

procedure TNPCShopForm.btnInsertClick(Sender: TObject);
var
  data:TRONPCdata;
  li:TListItem;
  res:AnsiString;
  i:integer;
begin
  NPCForm.SaveNPC(data);
  res:=GetNPCCode(cbShopType.Text,data);
  case cbShopType.ItemIndex of
    // shop, cashshop
    0,1: ;
    // itemshop  - adds <costitemid>:<discount>
    // pointshop - adds <costvariable>:<discount>
    2,3: begin
      res:=res+cbItemName.Text+':'+edQuantity.Text+',';
    end;
    // marketshop
    4: ;
  end;

  for i:=0 to lvShopItemList.Items.Count-1 do
  begin
    if i>0 then
      res:=res+',';
    li:=lvShopItemList.Items[i];
    res:=res+IntToStr(UIntPtr(li.data));

    if li.SubItems[0]='' then
      res:=res+':-1'
    else
      res:=res+':'+li.SubItems[0];

    if cbShopType.ItemIndex=4 then
    begin
      if li.SubItems[1]='' then
        res:=res+':1'
      else
        res:=res+':'+li.SubItems[1];
    end;
  end;
  res:=res+#13#10;

  ActiveEditor.InsertTextAtCaret(res);
end;

end.
