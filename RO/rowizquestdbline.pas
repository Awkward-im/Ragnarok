unit ROWizQuestDBLine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  rodatatypes, Classes;

type

  { TQuestDBLineForm }

  TQuestDBLineForm = class(TForm)
    btnGetCode: TButton;
    btnSave: TButton;
    btnHelp: TButton;
    cbNameID1: TComboBox;
    cbNameID2: TComboBox;
    cbNameID3: TComboBox;
    cbMobID1: TComboBox;
    cbMobID2: TComboBox;
    cbMobID3: TComboBox;
    cbTarget1: TComboBox;
    cbTarget2: TComboBox;
    cbTarget3: TComboBox;
    edVal1: TEdit;
    edQuestID: TEdit;
    edQuestTitle: TEdit;
    edRate1: TEdit;
    edRate2: TEdit;
    edRate3: TEdit;
    edTimeLimit: TEdit;
    edVal2: TEdit;
    edVal3: TEdit;
    gbTarget: TGroupBox;
    GroupBox1: TGroupBox;
    lQuestID: TLabel;
    lQuestTitle: TLabel;
    lMobID1: TLabel;
    lMobID2: TLabel;
    lMobID3: TLabel;
    lTarget1: TLabel;
    lTarget2: TLabel;
    lTimeLimit: TLabel;
    procedure btnGetCodeClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure AddListButtons;
    procedure FillItemList;
    procedure FillMobList;
    procedure FillAllData(const val:TROQuestData);
  public
    constructor Create(aOwner:TComponent; const astr:String); overload;
    constructor Create(aOwner:TComponent; idx:integer); overload;
  end;

var
  QuestDBLineForm: TQuestDBLineForm=nil;

implementation

uses
  rodataio,
  rodatagui;

{$R *.lfm}

{ TQuestDBLineForm }

constructor TQuestDBLineForm.Create(aOwner:TComponent; const astr:String);
var
  lqdata:TROQuestData;
begin
  inherited Create(AOwner);

  FillItemList;
  FillMobList;
  AddListButtons;

  if astr<>'' then
  begin
    if GetQuestLine(astr,lqdata) then
      FillAllData(lqdata);
    btnGetCode.Visible:=false;
  end
  else
  begin
    btnGetCode.Visible:=true;
  end;
end;

constructor TQuestDBLineForm.Create(aOwner:TComponent; idx:integer);
begin
  inherited Create(AOwner);

  FillItemList;
  FillMobList;
  AddListButtons;

  FillAllData(ROQuestData[idx]);
  btnGetCode.Visible:=false;
end;

procedure TQuestDBLineForm.AddListButtons;
begin
  AddListButton(cbTarget1,rosmMonster);
  AddListButton(cbTarget2,rosmMonster);
  AddListButton(cbTarget3,rosmMonster);
  AddListButton(cbMobID1 ,rosmMonster);
  AddListButton(cbMobID2 ,rosmMonster);
  AddListButton(cbMobID3 ,rosmMonster);
  AddListButton(cbNameID1,rosmItem);
  AddListButton(cbNameID2,rosmItem);
  AddListButton(cbNameID3,rosmItem);
end;

procedure TQuestDBLineForm.FillItemList;
var
  s:String;
  i,cnt:integer;
begin
  cbNameID1.Clear;
  cbNameID2.Clear;
  cbNameID3.Clear;

  // not including localization
  cnt:=RODB_GetCount(rosmItem);
  cbNameID1.Items.Capacity:=cnt*2; // english + localization
  cbNameID2.Items.Capacity:=cnt*2;
  cbNameID3.Items.Capacity:=cnt*2;

  cbNameID1.Items.BeginUpdate;
  for i:=0 to cnt-1 do
  begin
    s:=RODB_GetText(i,0,rosmItem);
    cbNameID1.Items.AddObject(s,TObject(i));
    s:=RODB_GetText(i,-1,rosmItem);
    if s<>'' then
      cbNameID1.Items.AddObject(s,TObject(i));
  end;
  cbNameID1.Items.EndUpdate;

  cbNameID2.Items.Assign(cbNameID1.Items);
  cbNameID3.Items.Assign(cbNameID1.Items);
end;

procedure TQuestDBLineForm.FillMobList;
var
  s:AnsiString;
  i,cnt:integer;
begin
  cbTarget1.Clear;
  cbTarget2.Clear;
  cbTarget3.Clear;
  cbMobID1.Clear;
  cbMobID2.Clear;
  cbMobID3.Clear;

  // not including localization
  cnt:=RODB_GetCount(rosmMonster);
  cbTarget1.Items.Capacity:=cnt*3; // iro+kro+localization
  cbTarget2.Items.Capacity:=cnt*3;
  cbTarget3.Items.Capacity:=cnt*3;
  cbMobID1 .Items.Capacity:=cnt*3;
  cbMobID2 .Items.Capacity:=cnt*3;
  cbMobID3 .Items.Capacity:=cnt*3;

  cbMobID1.Items.BeginUpdate;
  for i:=0 to cnt-1 do
  begin
    s:=RODB_GetText(i, 0,rosmMonster); if s<>'' then cbMobID1.Items.AddObject(s,TObject(i));
    s:=RODB_GetText(i, 1,rosmMonster); if s<>'' then cbMobID1.Items.AddObject(s,TObject(i));
    s:=RODB_GetText(i,-1,rosmMonster); if s<>'' then cbMobID1.Items.AddObject(s,TObject(i));
  end;
  cbMobID1.Items.EndUpdate;

  cbMobID2 .Items.Assign(cbMobID1.Items);
  cbMobID3 .Items.Assign(cbMobID1.Items);
  cbTarget1.Items.Assign(cbMobID1.Items);
  cbTarget2.Items.Assign(cbMobID1.Items);
  cbTarget3.Items.Assign(cbMobID1.Items);
end;

procedure TQuestDBLineForm.FormResize(Sender: TObject);
var
  dx:integer;
begin
  dx:=(lMobID2.Width+lMobID1.Width) div 2;
  dx:=((lMobID1.Width-dx) div 2)*2;

  lMobID1.Width :=lMobID1.Width-dx;
  lMobID2.Left  :=lMobID2.Left +dx;
  lMobID2.Width :=lMobID2.Width+dx;

  cbMobID1.Width :=cbMobID1.Width+dx;
  cbMobID2.Width :=cbMobID2.Width+dx;
  cbMobID3.Width :=cbMobID3.Width+dx;

  cbNameID1.Left :=cbNameID1.Left +dx;
  cbNameID1.Width:=cbNameID1.Width+dx;
  cbNameID2.Left :=cbNameID2.Left +dx;
  cbNameID2.Width:=cbNameID2.Width+dx;
  cbNameID3.Left :=cbNameID3.Left +dx;
  cbNameID3.Width:=cbNameID3.Width+dx;
end;

function GetStrValue(const s:String; var apos:integer):String;
var
  start:integer;
begin
  if apos>Length(s) then
  begin
    result:='';
    exit;
  end;
  start:=apos;
  while (apos<=Length(s)) and (s[apos]<>',') do inc(apos);
  result:=Copy(s,start,apos-start);
  inc(apos);
end;

function IfNotEmpty(val:integer):String; inline;
begin
  if val=0 then result:=''
  else          result:=IntToStr(val);
end;

procedure TQuestDBLineForm.btnHelpClick(Sender: TObject);
begin
  with CreateQuestHelpForm(Self,StrToIntDef(edQuestID.Text,0)) do Show;
end;

procedure TQuestDBLineForm.FillAllData(const val:TROQuestData);
begin
  edQuestID   .Text :=IntToStr(val.id);
  edTimeLimit .Text :=IfNotEmpty(val.time);

  SetListIndex(cbTarget1 ,val.hunt_mob1,rosmMonster);
  edVal1.Text:=IfNotEmpty(val.hunt_val1);

  SetListIndex(cbTarget2 ,val.hunt_mob2,rosmMonster);
  edVal2.Text:=IfNotEmpty(val.hunt_val2);

  SetListIndex(cbTarget3 ,val.hunt_mob3,rosmMonster);
  edVal3.Text:=IfNotEmpty(val.hunt_val3);

  SetListIndex(cbMobID1   ,val.drop_mob1 ,rosmMonster);
  SetListIndex(cbNameID1  ,val.drop_item1,rosmItem);
  edRate1.Text:=IfNotEmpty(val.drop_rate1);

  SetListIndex(cbMobID2   ,val.drop_mob2 ,rosmMonster);
  SetListIndex(cbNameID2  ,val.drop_item2,rosmItem);
  edRate2.Text:=IfNotEmpty(val.drop_rate2);

  SetListIndex(cbMobID3   ,val.drop_mob3 ,rosmMonster);
  SetListIndex(cbNameID3  ,val.drop_item3,rosmItem);
  edRate3.Text:=IfNotEmpty(val.drop_rate3);

  edQuestTitle.Text:=val.descr;
end;

//----- Data save -----

function GetListData(aMode:tRODBMode; cb:TComboBox):integer; overload;
var
  idx:integer;
begin
  idx:=GetListIndex(cb,aMode);
  if idx<0 then
    result:=0
  else
    result:=RODB_GetIDnum(idx,aMode);
end;

function GetValueData(ed:TEdit):integer; overload;
begin
  if ed.Text='' then
    result:=0
  else
    result:=StrToInt(ed.Text);
end;

procedure TQuestDBLineForm.btnSaveClick(Sender: TObject);
var
  lqdata:TROQuestData;
  i:integer;
  isnew:boolean;
begin
  if edQuestID.Text='' then
    exit;

  FillChar(lqdata,SizeOf(lqdata),0);
  lqdata.id:=StrToInt(edQuestID.Text);

  if edTimeLimit.Text<>'' then
    lqdata.time:=StrToInt(edTimeLimit.Text);

  lqdata.hunt_mob1:=GetListData (rosmMonster,cbTarget1);
  lqdata.hunt_val1:=GetValueData(edVal1);

  lqdata.hunt_mob2:=GetListData (rosmMonster,cbTarget2);
  lqdata.hunt_val2:=GetValueData(edVal2);

  lqdata.hunt_mob3:=GetListData (rosmMonster,cbTarget3);
  lqdata.hunt_val3:=GetValueData(edVal3);

  lqdata.drop_mob1 :=GetListData (rosmMonster,cbMobID1);
  lqdata.drop_item1:=GetListData (rosmMonster,cbNameID1);
  lqdata.drop_rate1:=GetValueData(edRate1);

  lqdata.drop_mob2 :=GetListData (rosmMonster,cbMobID2);
  lqdata.drop_item2:=GetListData (rosmMonster,cbNameID2);
  lqdata.drop_rate2:=GetValueData(edRate2);

  lqdata.drop_mob3 :=GetListData (rosmMonster,cbMobID3);
  lqdata.drop_item3:=GetListData (rosmMonster,cbNameID3);
  lqdata.drop_rate3:=GetValueData(edRate3);

  if edQuestTitle.Text='' then
    lqdata.descr:='<quest title>'
  else
    lqdata.descr:=edQuestTitle.Text;
  
  // here place to find and save quest
  isnew:=false;
  for i:=0 to High(ROQuestData) do
  begin
    if ROQuestData[i].id>lqdata.id then continue;

    if ROQuestData[i].id<lqdata.id then
    begin
      SetLength(ROQuestData,Length(ROQuestData)+1);
      move(ROQuestData[i],ROQuestData[i+1],SizeOf(tROQuestData));
      isnew:=true;
    end;
    ROQuestData[i]:=lqdata;
    // coz managed type
    ROQuestData[i].name    :=lqdata.name;
    ROQuestData[i].modified:=ROQuestData[i].modified or 1;
    if isnew then ReadQuestHelp(i);
    break;
  end;
end;

//----- Data insert ------

procedure GetListData(var res:String; aMode:tRODBMode; cb:TComboBox); overload;
var
  idx:integer;
begin
  idx:=GetListIndex(cb,aMode);
  if idx<0 then
    res:=res+'0'
  else
    res:=res+RODB_GetID(idx,aMode);

  res:=res+',';
end;

procedure GetValueData(var res:String; ed:TEdit); overload;
begin
  if ed.Text='' then
    res:=res+'0'
  else
    res:=res+ed.Text;
  res:=res+',';
end;

procedure TQuestDBLineForm.btnGetCodeClick(Sender: TObject);
var
  res:AnsiString;
begin
  res:='//';
  // quest id
  if edQuestID.Text='' then
    res:=res+'<questid>'
  else
    res:=res+edQuestID.Text;
  res:=res+',';

  // timer
  if edTimeLimit.Text='' then
    res:=res+'0'
  else
  begin // right now - just a seconds format
    res:=res+edTimeLimit.Text
  end;
  res:=res+',';

  // targets
  GetListData (res,rosmMonster,cbTarget1);
  GetValueData(res,edVal1);

  GetListData (res,rosmMonster,cbTarget2);
  GetValueData(res,edVal2);

  GetListData (res,rosmMonster,cbTarget3);
  GetValueData(res,edVal3);

  // items
  GetListData (res,rosmMonster,cbMobID1);
  GetListData (res,rosmItem   ,cbNameID1);
  GetValueData(res,edRate1);

  GetListData (res,rosmMonster,cbMobID2);
  GetListData (res,rosmItem   ,cbNameID2);
  GetValueData(res,edRate2);

  GetListData (res,rosmMonster,cbMobID3);
  GetListData (res,rosmItem   ,cbNameID3);
  GetValueData(res,edRate3);

  // quest title
  if edQuestTitle.Text='' then
    res:=res+'<quest title>'
  else
    res:=res+'"'+edQuestTitle.Text+'"';

  res:=res+#13#10;

  ActiveEditor.InsertTextAtCaret(res);
end;

end.
