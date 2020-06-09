unit ROSearchMob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  CheckLst, ExtCtrls;

type

  { TROSearchMobForm }

  TROSearchMobForm = class(TForm)
    btnSearch: TButton;
    cbMobName: TComboBox;
    clbSize: TCheckListBox;
    clbRace: TCheckListBox;
    clbElement: TCheckListBox;
    edId: TEdit;
    lblId: TLabel;
    lblRace: TLabel;
    lblElement: TLabel;
    lblSize: TLabel;
    lblMobName: TLabel;
    lblFound: TLabel;
    lblFrom: TLabel;
    lblTo: TLabel;
    sgSearchResult: TStringGrid;
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgSearchResultCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure sgSearchResultDblClick(Sender: TObject);
  private
    pnlLevel,
    pnlExp,
    pnlJExp,
    pnlHP,
    pnlAtk,
    pnlDef,
    pnlMDef:TPanel;
    procedure PrepareForm;
    procedure FillMobList;
    procedure FillOtherLists;
    procedure SetCell(acol,aval:integer);

  public

  end;

var
  ROSearchMobForm: TROSearchMobForm;

implementation

uses
  common,
  ROFullCardMob, ROHelpCardMob,
  rodata,rodatagui,rodatatypes;

{$R *.lfm}

resourcestring
  sFound = 'Search: found monsters = ';
  sTitle = 'Title';
  sLevel = 'Level';
  sExp   = 'Exp';
  sJExp  = 'JExp';
  sHP    = 'HP';
  sAtk   = 'Atk';
  sDef   = 'Def';
  sMDef  = 'MDef';

const
  DiaHeight = 28;
  DiaLeft   = 520;
const
  ResColumns: array [0..7] of AnsiString = (
    sTitle,
    sLevel,
    sExp,
    sJExp,
    sHP,
    sAtk,
    sDef,
    sMDef
  );

  { TROSearchMobForm }

procedure TROSearchMobForm.PrepareForm;
var
  col:TGridColumn;
  i,ltop:integer;
begin
  sgSearchResult.BeginUpdate;
  for i:=0 to High(ResColumns) do
  begin
    col:=sgSearchResult.Columns.Add;
    with col do
    begin
      ReadOnly:=true;
      if i=0 then SizePriority:=0;
      Title.Caption     :=ResColumns[i];
//      Title.Alignment   :=taCenter;
      Title.PrefixOption:=poHeaderClick;
      Title.Multiline   :=true;
      Width:=80;
    end;
  end;
  with sgSearchResult.Columns.Add do
  begin
    Visible:=false;
  end;
  sgSearchResult.EndUpdate;

  // Diapazones
  ltop:=DiaHeight;
  pnlLevel:=CreateDiapazonePanel(Self,sLevel);
  with pnlLevel do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlExp:=CreateDiapazonePanel(Self,sExp);
  with pnlExp do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlJExp:=CreateDiapazonePanel(Self,sJExp);
  with pnlJExp do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlHP:=CreateDiapazonePanel(Self,sHP);
  with pnlHP do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlAtk:=CreateDiapazonePanel(Self,sAtk);
  with pnlAtk do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlDef:=CreateDiapazonePanel(Self,sDef);
  with pnlDef do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;
  pnlMDef:=CreateDiapazonePanel(Self,sMDef);
  with pnlMDef do
  begin
    Top :=ltop; inc(ltop,DiaHeight);
    Left:=DiaLeft;
  end;

end;

procedure TROSearchMobForm.FillMobList;
var
  i:integer;
begin
  cbMobName.Items.Clear;
  cbMobName.Items.BeginUpdate;
  cbMobName.Items.Capacity:=Length(ROMobData)*3;
  for i:=0 to High(ROMobData) do
  begin
    cbMobName.Items.Add(ROMobData[i].descr);
    if ROMobData[i].kro<>'' then
      cbMobName.Items.Add(ROMobData[i].kro);
    if ROMobData[i].local<>'' then
      cbMobName.Items.Add(ROMobData[i].local);
  end;
  cbMobName.Items.EndUpdate;
end;

procedure TROSearchMobForm.FillOtherLists;
var
  i:integer;
begin
  clbRace.Items.Clear;
  for i:=0 to High(RORaces) do
    clbRace.Items.Add(RORaces[i]);
  clbElement.Items.Clear;
  for i:=0 to High(ROElements) do
    clbElement.Items.Add(ROElements[i]);
  clbSize.Items.Clear;
  for i:=0 to High(ROSizes) do
   clbSize.Items.Add(ROSizes[i]);
end;

procedure TROSearchMobForm.FormCreate(Sender: TObject);
begin
  PrepareForm;
  FillMobList;
  FillOtherLists;
end;

procedure TROSearchMobForm.SetCell(acol,aval:integer);
begin
  if aval>0 then
    sgSearchResult.Cells[acol,sgSearchResult.RowCount-1]:=IntToStr(aval);
end;

procedure TROSearchMobForm.btnSearchClick(Sender: TObject);
var
  lName:AnsiString;
  lId:integer;
  lLevelFrom,lLevelTo,
  lExpFrom  ,lExpTo,
  lJExpFrom ,lJExpTo,
  lHPFrom   ,lHPTo,
  lAtkFrom  ,lAtkTo,
  lDefFrom  ,lDefTo,
  lMDefFrom ,lMDefTo:integer;
  lElement,lRace,lSize:cardinal;
  ldata:PROMobData;
  i:integer;
begin
  lName:=AnsiUpperCase(cbMobName.Text);
  if edId.Text<>'' then
    lId:=StrToInt(edId.Text)
  else
    lId:=0;
  if lId<>0 then
  begin
    lElement:=0;
    lRace:=0;
    lSize:=0;
    for i:=0 to High(RORaces) do
      if clbRace.Checked[i] then
        lRace:=lRace or cardinal(1 shl i);
    for i:=0 to High(ROElements) do
      if clbElement.Checked[i] then
        lElement:=lElement or cardinal(1 shl i);
    for i:=0 to High(ROSizes) do
      if clbSize.Checked[i] then
        lSize:=lSize or cardinal(1 shl i);

    lLevelTo:=GetDiapazoneValue(pnlLevel,lLevelFrom);
    lExpTo  :=GetDiapazoneValue(pnlExp  ,lExpFrom  );
    lJExpTo :=GetDiapazoneValue(pnlJExp ,lJExpFrom );
    lHPTo   :=GetDiapazoneValue(pnlHP   ,lHPFrom   );
    lAtkTo  :=GetDiapazoneValue(pnlAtk  ,lAtkFrom  );
    lDefTo  :=GetDiapazoneValue(pnlDef  ,lDefFrom  );
    lMDefTo :=GetDiapazoneValue(pnlMDef ,lMDefFrom );
  end;

  // search
  sgSearchResult.Clear;
  sgSearchResult.BeginUpdate;
  sgSearchResult.RowCount:=1;
  sgSearchResult.Columns[0].Width:=200;
  for i:=0 to High(ROMobData) do
  begin
    ldata:=@ROMobData[i];

    if lId<>0 then
    begin
      if ldata^.id<>lId then continue;

    end
    else
    begin
      if lName<>'' then
      begin
        if (Pos(lName,AnsiUpperCase(ldata^.descr))=0) and
           (Pos(lName,AnsiUpperCase(ldata^.kro  ))=0) and
           (Pos(lName,AnsiUpperCase(ldata^.local))=0) then continue;
      end;
      // level
      if (lLevelFrom>=0) then if lLevelFrom>ldata^.Lvl then continue;
      if (lLevelTo  >=0) then if lLevelTo  <ldata^.Lvl then continue;
      // Exp
      if (lExpFrom>=0) then if cardinal(lExpFrom)>ldata^.Exp then continue;
      if (lExpTo  >=0) then if cardinal(lExpTo  )<ldata^.Exp then continue;
      // JExp
      if (lJExpFrom>=0) then if cardinal(lJExpFrom)>ldata^.JExp then continue;
      if (lJExpTo  >=0) then if cardinal(lJExpTo  )<ldata^.JExp then continue;
      // HP
      if (lHPFrom>=0) then if cardinal(lHPFrom)>ldata^.HP then continue;
      if (lHPTo  >=0) then if cardinal(lHPTo  )<ldata^.HP then continue;
      // Atk
      if (lAtkFrom>=0) then if lAtkFrom>ldata^.Atk1 then continue;
      if (lAtkTo  >=0) then
        if ldata^.Atk2>0 then
        begin
          if lAtkTo<ldata^.Atk2 then continue;
        end
        else
          if lAtkTo<ldata^.Atk1 then continue;
      // Race
      if lRace<>0 then if (lRace and (1 shl ldata^.Race))=0 then continue;
      // Element
      if lElement<>0 then
        if (lElement and (1 shl (ldata^.Element mod 20)))=0 then continue;
      // Size
      if lSize<>0 then if (lSize and (1 shl ldata^.Scale))=0 then continue;
      // Def
      if (lDefFrom>=0) then if lDefFrom>ldata^.Def then continue;
      if (lDefTo  >=0) then if lDefTo  <ldata^.Def then continue;
      // MDef
      if (lMDefFrom>=0) then if lMDefFrom>ldata^.MDef then continue;
      if (lMDefTo  >=0) then if lMDefTo  <ldata^.MDef then continue;
    end;

    sgSearchResult.RowCount:=sgSearchResult.RowCount+1;

    sgSearchResult.Cells[sgSearchResult.ColCount-1,sgSearchResult.RowCount-1]:=IntToStr(i);
    sgSearchResult.Cells[0,sgSearchResult.RowCount-1]:=RODB_GetText(i,rosmMonster); // ldata^.descr;
    SetCell(1, ldata^.Lvl);
    SetCell(2, ldata^.Exp);
    SetCell(3, ldata^.JExp);
    SetCell(4, ldata^.HP);
    SetCell(5, ldata^.Atk1);
    SetCell(6, ldata^.Def);
    SetCell(7, ldata^.MDef);

    if lId<>0 then break;
  end;

  sgSearchResult.EndUpdate;
  lblFound.Caption:=sFound+IntToStr(sgSearchResult.RowCount-1);
end;

procedure TROSearchMobForm.sgSearchResultCompareCells(Sender: TObject; ACol,
  ARow, BCol, BRow: Integer; var Result: integer);
var
  s1,s2:ansistring;
begin
  s1:=(Sender as TStringGrid).Cells[ACol,ARow];
  s2:=(Sender as TStringGrid).Cells[BCol,BRow];
  if (ACol<>0) and (BCol<>0) then
  begin
    result:=StrToIntDef(s1,0)-
            StrToIntDef(s2,0);
  end
  else
    result:=CompareStr(s1,s2);

  if (Sender as TStringGrid).SortOrder=soDescending then
    result:=-result;
end;

procedure TROSearchMobForm.sgSearchResultDblClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=StrToInt(sgSearchResult.Cells[sgSearchResult.ColCount-1,sgSearchResult.Row]);
  if ROShowFullCard then
    with tROFullCardMobForm.Create(Self,idx) do Show
  else
    with tROHelpCardMobForm.Create(Self,idx) do Show;
end;

end.

