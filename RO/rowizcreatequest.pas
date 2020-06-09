unit ROWizCreateQuest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Arrow,
  ComCtrls,
  ROWizNPCBase;

type

  { TNPCCreateForm }

  TNPCCreateForm = class(TForm)
    arResultUp: TArrow;
    arResultDown: TArrow;
    btnAddResult: TButton;
    btnAddIngredient: TButton;
    btnDeleteResult: TButton;
    btnDeleteIngredient: TButton;
    btnInsert: TButton;
    cbResultItem: TComboBox;
    cbItemName: TComboBox;
    cbUseNames: TCheckBox;
    edQuantity: TEdit;
    Label1: TLabel;
    lIngredient: TLabel;
    lbResult: TListBox;
    lvItemList: TListView;
    lQuantity: TLabel;
    lResultItem: TLabel;
    procedure arResultDownClick(Sender: TObject);
    procedure arResultUpClick(Sender: TObject);
    procedure btnAddIngredientClick(Sender: TObject);
    procedure btnAddResultClick(Sender: TObject);
    procedure btnDeleteIngredientClick(Sender: TObject);
    procedure btnDeleteResultClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure cbItemNameChange(Sender: TObject);
    procedure cbResultItemChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbResultSelectionChange(Sender: TObject; User: boolean);
  private
    type
      tCraftData = record
        idx     :integer;
        quantity:integer;
      end;
      tResultData = record
        idx:integer;
        src:array of tCraftData;
      end;
  private
    NPCForm:TNPCBase;
    CraftData:array of tResultData;

    procedure FillItemList;
  public

  end;

var
  NPCCreateForm: TNPCCreateForm=nil;

implementation

uses
  rodatagui,rodatatypes;

{$R *.lfm}

{ TNPCCreateForm }

procedure TNPCCreateForm.FillItemList;
var
  s,ls:String;
  i,cnt:integer;
begin
  cbResultItem.Clear;
  cbItemName  .Clear;

  cnt:=RODB_GetCount(rosmItem);
  cbResultItem.Items.Capacity:=cnt*2;
  cbItemName  .Items.Capacity:=cnt*2;

  cbResultItem.Items.BeginUpdate;
  for i:=0 to cnt-1 do
  begin
    ls:=' ('+RODB_GetID(i,rosmItem)+')';
    cbResultItem.Items.AddObject(RODB_GetText(i,0,rosmItem)+ls,TObject(i));
    s:=RODB_GetText(i,-1,rosmItem);
    if s<>'' then
      cbResultItem.Items.AddObject(s+ls,TObject(i));
  end;
  cbResultItem.Items.EndUpdate;

  cbItemName.Items.Assign(cbResultItem.Items);

  cbResultItem.ItemIndex:=0;
  cbItemName  .ItemIndex:=0;
end;

procedure TNPCCreateForm.FormCreate(Sender: TObject);
begin
  AddListButton(cbResultItem,rosmItem);
  AddListButton(cbItemName  ,rosmItem);

  NPCForm:=TNPCBase.Create(Self);

  Self.Height:=Self.Height+NPCForm.Height;
  Self.Constraints.MinHeight:=Self.Height;

  NPCForm.Parent:=Self;
  NPCForm.Visible:=true;
  NPCForm.Left:=0;
  NPCForm.Top:=0;
  NPCForm.Anchors:=[akLeft,akRight,akTop,akBottom];
  NPCForm.Align  :=alTop;

  FillItemList;
end;

procedure TNPCCreateForm.FormResize(Sender: TObject);
begin
  lbResult  .Width:=(Self.Width-38) div 2;
  lvItemList.Width:=(Self.Width-38) div 2;
  lvItemList.Left :=Self.Width-6-lvItemList.Width;
end;

procedure TNPCCreateForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i:integer;
begin
  for i:=0 to High(CraftData) do
    SetLength(CraftData[i].src,0);
  SetLength(CraftData,0);
end;

procedure TNPCCreateForm.lbResultSelectionChange(Sender: TObject; User: boolean);
var
  li:TListItem;
  cdata:^tCraftData;
  idx,i:integer;
begin
  // Fill lvItemList by current result ingredients
  lvItemList.Items.Clear;
  idx:=integer(lbResult.Items.Objects[lbResult.ItemIndex]);
  for i:=0 to High(CraftData[idx].src) do
  begin
    cdata:=@CraftData[idx].src[i];
    if cdata^.idx>=0 then
    begin
      li:=lvItemList.Items.Add;
      li.Caption:=IntToStr(cdata^.quantity);
      li.SubItems.Add(RODB_GetTitle(cdata^.idx,rosmItem));
      li.Data:=pointer(i);
    end;
  end;
end;

procedure TNPCCreateForm.btnAddResultClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=GetListIndex(cbResultItem,rosmItem);
  if idx<0 then exit;

  SetLength(CraftData,Length(CraftData)+1);
  FillChar(CraftData[High(CraftData)],SizeOf(tResultData),0);

  CraftData[High(CraftData)].idx:=idx;
  lbResult.ItemIndex:=lbResult.Items.AddObject(
      RODB_GetTitle(idx,rosmItem),TObject(High(CraftData)));

  btnDeleteResult .Enabled:=true;
  btnAddIngredient.Enabled:=true;
  btnInsert       .Enabled:=true;
end;

procedure TNPCCreateForm.btnDeleteResultClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=lbResult.ItemIndex;
  lbResult.Items.Delete(lbResult.ItemIndex);
  if idx>=lbResult.Items.Count then
    dec(idx);
  lbResult.ItemIndex:=idx;
  if idx<0 then
  begin
    lvItemList.Clear;
    btnDeleteIngredient.Enabled:=false;
  end;

  btnDeleteResult .Enabled:=lbResult.Items.Count>0;
  btnAddIngredient.Enabled:=lbResult.Items.Count>0;
  btnInsert       .Enabled:=lbResult.Items.Count>0;

  // will keep array til form close
end;

procedure TNPCCreateForm.arResultUpClick(Sender: TObject);
var
  i:integer;
begin
  if lbResult.ItemIndex>0 then
  begin
    i:=lbResult.ItemIndex;
    lbResult.Items.Move(lbResult.ItemIndex,lbResult.ItemIndex-1);
    lbResult.ItemIndex:=i-1;
  end;
end;

procedure TNPCCreateForm.arResultDownClick(Sender: TObject);
var
  i:integer;
begin
  if (lbResult.ItemIndex>=0) and
     (lbResult.ItemIndex<(lbResult.Items.Count-1)) then
  begin
    i:=lbResult.ItemIndex;
    lbResult.Items.Move(lbResult.ItemIndex,lbResult.ItemIndex+1);
    lbResult.ItemIndex:=i+1;
  end;
end;

procedure TNPCCreateForm.cbResultItemChange(Sender: TObject);
begin
  btnAddResult.Enabled:=cbResultItem.Text<>'';
end;

procedure TNPCCreateForm.cbItemNameChange(Sender: TObject);
begin
  btnAddIngredient.Enabled:=(cbItemName.Text<>'') and (lbResult.Items.Count>0);
end;

procedure TNPCCreateForm.btnDeleteIngredientClick(Sender: TObject);
var
  li:TListItem;
  i,idx:integer;
begin
  // clear ingredient id (not delete)
  idx:=integer(lbResult.Items.Objects[lbResult.ItemIndex]);
  i :=lvItemList.ItemIndex;
  li:=lvItemList.Items[i];
  CraftData[idx].src[UIntPtr(li.data)].idx:=-1;

  lvItemList.Items.Delete(i);
  if i>=lvItemList.Items.Count then
    dec(i);
  lvItemList.ItemIndex:=i;

  btnDeleteIngredient.Enabled:=lvItemList.Items.Count>0;
end;

procedure TNPCCreateForm.btnAddIngredientClick(Sender: TObject);
var
  li:TListItem;
  cdata:^tCraftData;
  idx,iidx,i:integer;
begin
  iidx:=GetListIndex(cbItemName,rosmItem);
  if iidx<0 then exit;

  idx:=integer(lbResult.Items.Objects[lbResult.ItemIndex]);
  i:=Length(CraftData[idx].src);
  SetLength(CraftData[idx].src,i+1);
  cdata:=@CraftData[idx].src[i];
  FillChar(cdata^,SizeOf(tCraftData),0);

  li:=lvItemList.Items.Add;
  if edQuantity.Text='' then
    li.Caption:='1'
  else
    li.Caption:=edQuantity.Text;
  cdata^.quantity:=StrToInt(li.Caption);
  cdata^.idx     :=iidx;
  li.SubItems.Add(RODB_GetTitle(iidx,rosmItem));
  li.Data:=pointer(i);

  lvItemList.ItemIndex:=li.Index;

  btnDeleteIngredient.Enabled:=true;
end;

procedure TNPCCreateForm.btnInsertClick(Sender: TObject);
var
  data:TRONPCdata;
  res,ls:AnsiString;
  cdata:^tCraftData;
  idx,i,j,flg:integer;
begin
  NPCForm.SaveNPC(data);
  res:=GetNPCCode('script',data)+'{'#13#10;

  res:=res+
    '  mes "Добро пожаловать!";'#13#10'  mes "Что желаете?";'#13#10'  next;'#13#10;

  // resulting item choosing
  res:=res+'  switch(select(""';
  for i:=0 to lbResult.Count-1 do
  begin
    if i>0 then
      res:=res+'+"","" ';
    res:=res+'+ getitemname('+
        RODB_GetID(CraftData[integer(lbResult.Items.Objects[i])].idx,rosmItem)+')';
  end;
  res:=res+'+"")) {'#13#10;

  // cycle through all crafting items
  for i:=0 to lbResult.Count-1 do
  begin
    idx:=integer(lbResult.Items.Objects[i]);
    res:=res+#13#10'    case '+IntToStr(i+1)+':'#13#10+
             '    mes "Ты хочешь сделать "+getitemname('+
             RODB_GetID(CraftData[idx].idx,rosmItem)+')+"";'#13#10+
             '    mes "Для этого тебе надо следующее:";'#13#10;
{}
    // notes about ingredients
    for j:=0 to High(CraftData[idx].src) do
    begin
      cdata:=@CraftData[idx].src[j];
      if cdata^.idx>=0 then
      begin
        res:=res+'    mes "'+
            IntToStr(cdata^.quantity)+'x "+getitemname('+
            RODB_GetID(cdata^.idx,rosmItem)+')+"";'#13#10;
      end;
    end;

    // Choosing for crafting or not
    res:=res+
        '    if (select("Да", "Нет") == 2) close;'#13#10+
        '    else{'#13#10+
        '      if ';
    flg:=0;
    for j:=0 to High(CraftData[idx].src) do
    begin
      cdata:=@CraftData[idx].src[j];
      if cdata^.idx>=0 then
      begin
        if flg>0 then
          res:=res+' || ';

        flg:=1;
        if cbUseNames.checked then
          ls:=RODB_GetName(cdata^.idx,rosmItem);
        if cbUseNames.checked and (ls<>'') then
          res:=res+'(countitem("' +ls+'") < '+
              IntToStr(cdata^.quantity)+')'
        else
          res:=res+'(countitem('+
              RODB_GetID(cdata^.idx,rosmItem)+') < '+
              IntToStr(cdata^.quantity)+')';
      end;
    end;
    res:=res+'close;'#13#10;

    // removing used ingredients
    for j:=0 to High(CraftData[idx].src) do
    begin
      cdata:=@CraftData[idx].src[j];
      if cdata^.idx>=0 then
      begin
        if cbUseNames.checked then
          ls:=RODB_GetName(cdata^.idx,rosmItem);
        if cbUseNames.checked and (ls<>'') then
          res:=res+'      delitem "'+ls+'",'+
              IntToStr(cdata^.quantity)+';'#13#10
        else
          res:=res+'      delitem '+
            RODB_GetID(cdata^.idx,rosmItem)+','+
            IntToStr(cdata^.quantity)+
            '; // ' +RODB_GetText(cdata^.idx,rosmItem)+#13#10;
      end;
    end;

    // adding resulting item
    if cbUseNames.checked then
      ls:=RODB_GetName(CraftData[idx].idx,rosmItem);
    if cbUseNames.checked and (ls<>'') then
       res:=res+'      getitem "'+ls+'",1;'#13#10
     else
       res:=res+'      getitem '+
          RODB_GetID  (CraftData[idx].idx,rosmItem)+',1; // '+
          RODB_GetText(CraftData[idx].idx,rosmItem)+#13#10;
{}
    res:=res+'      close;'#13#10'    }'#13#10'    break;'#13#10#13#10;
  end;
  res:=res+'  }'#13#10'}'#13#10;

  ActiveEditor.InsertTextAtCaret(res);
end;

end.
