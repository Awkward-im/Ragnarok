unit ROHelpTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, Grids, Arrow, Controls, Buttons,
  rodatatypes;

type

  { TROHelpTableForm }

  TROHelpTableForm = class(TForm)
    arNextSearch: TArrow;
    edSearch: TEdit;
    lblListTitle: TLabel;
    sgList: TStringGrid;
    sbFilter: TSpeedButton;
    procedure arNextSearchClick(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure sbFilterClick(Sender: TObject);
    procedure sgListDblClick(Sender: TObject);
    procedure sgListCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure sgListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMode: tRODBMode;
    FFields,
    FCount:integer;
    ffilter:boolean;

    procedure PrepareList;
    procedure FillList(const afilter:String);
    procedure Search(aGrid:TStringGrid; const aText:AnsiString; aRow:integer);
  public
    constructor Create(AOwner: TComponent; aMode:tRODBMode); overload;

    property Mode:tRODBMode read FMode write FMode;
  end;


implementation

uses
  rohelpcardmob,ROFullCardMob,ROFullCardItem,rowizquestdbline,roresplist;

{$R *.lfm}

{ TROHelpTableForm }

procedure TROHelpTableForm.PrepareList;
var
  i:integer;
begin
  lblListTitle.Caption:=ModeToText(FMode)+ ' ('+IntToStr(FCount)+')';

  sgList.BeginUpdate;

  // set columns with headers
  sgList.Columns[0].Title.Caption:=RODB_GetField(-1,0,FMode);
  sgList.Columns[0].Width:=80;
  for i:=1 to FFields-1 do
  begin
    with sgList.Columns.Add do
    begin
      Title.Alignment   :=taCenter;
      Title.PrefixOption:=poHeaderClick;
      Title.Caption     :=RODB_GetField(-1,i,FMode);
    end;
  end;
  // special for indexes
  with sgList.Columns.Add do
    Visible:=false;

  sgList.EndUpdate;
end;

procedure TROHelpTableForm.FillList(const afilter:String);
var
  lfilter:String;
  i,j,k,cnt:integer;
begin
  sgList.Clear;
//  PrepareList;

  sgList.BeginUpdate;

  if afilter='' then
  begin
    sgList.RowCount:=FCount+1;
    for i:=0 to FCount-1 do
    begin
      for j:=0 to FFields-1 do
        sgList.Cells[j,i+1]:=RODB_GetField(i,j,FMode);
      sgList.Cells[FFields,i+1]:=IntToStr(i);
    end;
  end
  else
  begin
    lfilter:=AnsiUpperCase(afilter);
    // count
    cnt:=0;
    for i:=0 to FCount-1 do
    begin
      for j:=0 to FFields-1 do
      begin
        if pos(lfilter,AnsiUpperCase(RODB_GetField(i,j,FMode)))>0 then
        begin
          inc(cnt);
          break;
        end;
      end;
    end;
    sgList.RowCount:=cnt+1;
    // fill
    cnt:=0;
    for i:=0 to FCount-1 do
    begin
      // check if need to include
      for j:=0 to FFields-1 do
      begin
        if pos(lfilter,AnsiUpperCase(RODB_GetField(i,j,FMode)))>0 then
        begin
          inc(cnt);
          for k:=0 to FFields-1 do
            sgList.Cells[k,cnt]:=RODB_GetField(i,k,FMode);
          sgList.Cells[FFields,cnt]:=IntToStr(i);
          break;
        end;
      end;
    end;
  end;

  sgList.EndUpdate;
  sgList.Row:=0;
end;

constructor TROHelpTableForm.Create(AOwner: TComponent; aMode:tRODBMode);
begin
  inherited Create(AOwner);
  FMode  :=aMode;
  FCount :=RODB_GetCount(aMode);
  FFields:=RODB_GetFieldCount(aMode);
  ffilter:=false;

  Caption:=sTabTable+ModeToText(FMode);

  PrepareList;
  FillList('');
end;

procedure TROHelpTableForm.Search(aGrid:TStringGrid; const aText:AnsiString; aRow:integer);
var
  ltext:AnsiString;
  i,j:integer;
begin
  ltext:=AnsiUpperCase(aText);
  for i:=aRow to aGrid.RowCount-1 do
  begin
    for j:=0 to aGrid.ColCount-1 do
    begin
      if Pos(ltext,AnsiUpperCase(aGrid.Cells[j,i]))>0 then
      begin
        aGrid.Row:=i;
        exit;
      end;
    end;
  end;
end;

procedure TROHelpTableForm.edSearchChange(Sender: TObject);
begin
  // Filter is OFF, just search
  if not ffilter then
  begin
    if sgList.RowCount<>(RODB_GetCount(FMode)+1) then
      FillList('');
    Search(sgList,edSearch.Text,sgList.Row)
  end
  // Filter is ON but too short - fill full list
  else if Length(edSearch.Text)<3 then
  begin
    if sgList.RowCount<>(RODB_GetCount(FMode)+1) then
      FillList('');
  end
  // Filter is ON  - fill by filter
  else
    FillList(edSearch.Text);

end;

procedure TROHelpTableForm.sbFilterClick(Sender: TObject);
begin
  ffilter:=not ffilter;
  sbFilter.Down:=ffilter;
  edSearchChange(Sender);
end;

procedure TROHelpTableForm.arNextSearchClick(Sender: TObject);
begin
  Search(sgList,edSearch.Text,sgList.Row+1);
end;

procedure TROHelpTableForm.sgListCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  s1,s2:ansistring;
begin
  s1:=(Sender as TStringGrid).Cells[ACol,ARow];
  s2:=(Sender as TStringGrid).Cells[BCol,BRow];
  if (ACol=0) and (BCol=0) and (StrToIntDef(s1,-1000)<>-1000) then
  begin
    result:=StrToInt(s1)-
            StrToInt(s2);
  end
  else
    result:=CompareStr(s1,s2);

  if (Sender as TStringGrid).SortOrder=soDescending then
    result:=-result;
end;

procedure TROHelpTableForm.sgListDblClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=StrToInt(sgList.Cells[FFields,sgList.Row]);
  ModalResult:=mrOk+(idx shl 4);

  if not (fsModal in FormState) then
  begin
    case FMode of
      rosmMap: begin
        with TRoRespListForm.Create(Self,rosmMap,idx) do Show;
      end;
      rosmMonster: begin
        if ROShowFullCard then
          with TROFullCardMobForm.Create(Self,idx) do Show
        else
          with tROHelpCardMobForm.Create(Self,idx) do Show;
      end;
      rosmItem: begin
        with TROFullCardItemForm.Create(Self,idx) do Show;
      end;
      rosmQuest: begin
        with TQuestDBLineForm.Create(Self,idx) do ShowModal;
      end;
    end;
  end;
end;

procedure TROHelpTableForm.sgListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cx,cy:integer;
begin
  if (Button=mbRight) then
  begin
    sgList.MouseToCell(X,Y,cx,cy);
    sgList.Row:=cy;
    cx:=StrToIntDef(sgList.Cells[FFields,cy],-1);
    if cx>=0 then
    begin
      case FMode of
        rosmMonster: begin
          if ROShowFullCard then
            with TROFullCardMobForm.Create(Self,cx) do Show
          else
            with tROHelpCardMobForm.Create(Self,cx) do Show;
        end;
        rosmItem: begin
          with TROFullCardItemForm.Create(Self,cx) do Show;
        end;
      end;
    end;
  end;
end;

end.
