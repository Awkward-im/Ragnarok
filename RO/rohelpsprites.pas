unit ROHelpSprites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, Types,
  rodatatypes;

type

  { TROHelpSpritesForm }

  TROHelpSpritesForm = class(TForm)
    cbStretch: TCheckBox;
    cbShowNames: TCheckBox;
    cbCategory: TComboBox;
    dgSpriteGrid: TDrawGrid;
    pnlTop: TPanel;
    procedure CheckBoxClick(Sender: TObject);
    procedure ChangeCategory(Sender: TObject);
    procedure ChooseSprite(Sender: TObject);
    procedure dgSpriteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgSpriteGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure dgSpriteGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    FMode:tRODBMode;
    FCount:integer;
    SGCount:integer;
    FFlg: array of record
      state:integer;
      hash :cardinal;
    end;
    FArr: array of TMemoryStream;
    FFlt: array of Integer;
    FilterProc:tSGFilterProc;

    procedure LoadPic(index:integer);
    function GetHint(aCol,aRow:integer):AnsiString;
  public
    constructor Create(AOwner:TComponent; aMode:tRODBMode;
          aFilter:tSGFilterProc=nil); overload;
  end;


implementation

uses
  ROFullCardItem,ROFullCardMob,ROHelpCardMob,RoRespList,
  common;

{$R *.lfm}

const
  sprNotChecked = 1;
  sprNotLoaded  = 2;
  sprLoaded     = 3;

{ TROHelpSpritesForm }

procedure TROHelpSpritesForm.LoadPic(index:integer);
var
  lname:String;
  lhash:cardinal;
  i:integer;
begin
  lname:=RODB_GetSprite(index,FMode);
  lhash:=Hash(pointer(lname),Length(lname));
  // check for existing
  for i:=0 to FCount-1 do
  begin
    if FFlg[i].state=sprLoaded then
    begin
      if lhash=FFlg[i].hash then
      begin
        FFlg[index].state:=-i;
        exit;
      end;
    end;
  end;

// trying to load
  if FileExists(lname) then
  begin
    FArr[index]:=TMemoryStream.Create;
    try
      FArr[index].LoadFromFile(lname);
      FFlg[index].state:=sprLoaded;
      FFlg[index].hash :=lhash;
    except
      FArr[index].Free;
      FFlg[index].state:=sprNotLoaded;
    end;
  end
  else
    FFlg[index].state:=sprNotLoaded;
end;

procedure TROHelpSpritesForm.dgSpriteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  tp:TPicture;
  lRect:TRect;
  idx,sgidx,org:integer;
  w,h:integer;
  koefW,koefH:double;
begin
  if Length(FFlg)=0 then exit;

  dgSpriteGrid.DefaultDrawCell(aCol,aRow,aRect,aState);
  sgidx:=aRow*4+aCol;
  if (sgidx<SGCount) then
  begin
    idx:=FFlt[sgidx];
    org:=idx;

    if FFlg[idx].state=sprNotChecked then
      LoadPic(idx);

    if FFlg[idx].state<=0 then
      idx:=ABS(FFlg[idx].state);

    if FFlg[idx].state=sprLoaded then
    begin
      FArr[idx].Position:=0;
      tp:=TPicture.Create;
      tp.LoadFromStream(FArr[idx]);
      // trying to resize image proportionally
      if cbStretch.Checked then
      begin
        // resize
        h:=aRect.Bottom-aRect.Top -4;
        w:=aRect.Right -aRect.Left-4;
        koefW := tp.Bitmap.Width  / w;
        koefH := tp.Bitmap.Height / h;
        if koefH > koefW then
          koefW := koefH;

        w := Round(tp.Bitmap.Width  / koefW);
        h := Round(tp.Bitmap.Height / koefW);
        // center
        h:=aRect.Bottom-aRect.Top -h;
        w:=aRect.Right -aRect.Left-w;
        if h>0 then h:=h div 2 else h:=0;
        if w>0 then w:=w div 2 else w:=0;
        lRect.Left  :=aRect.Left+w;
        lRect.Top   :=aRect.Top+h;
        lRect.Right :=aRect.Right-w;
        lRect.Bottom:=aRect.Bottom-h;

        (Sender as TDrawGrid).Canvas.StretchDraw(lRect, tp.Bitmap);
      end
      // trying to center image in cell
      else
      begin
        h:=aRect.Bottom-aRect.Top -tp.Height;
        w:=aRect.Right -aRect.Left-tp.Width;
        if h>0 then h:=h div 2 else h:=0;
        if w>0 then w:=w div 2 else w:=0;

        (Sender as TDrawGrid).Canvas.Draw(aRect.Left+w, aRect.Top+h, tp.Bitmap);
      end;
      tp.Free;
    end;

    if cbShowNames.Checked then
      (Sender as TDrawGrid).Canvas.TextOut(
          aRect.Left+8,aRect.Bottom-20,RODB_GetTitle(org,FMode));
  end;
end;

function TROHelpSpritesForm.GetHint(aCol,aRow:integer):AnsiString;
var
  idx:integer;
begin
  result:='';
  if (aCol>=0) and (aRow>=0) then
  begin
    idx:=aRow*4+aCol;
    if idx<SGCount then
      result:=RODB_GetTitle(FFlt[idx],FMode);
  end;
end;

procedure TROHelpSpritesForm.dgSpriteGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
  HintText:=GetHint(aCol,aRow);
end;

procedure TROHelpSpritesForm.dgSpriteGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  Caption:=sTabSprites+GetHint(aCol,aRow);
end;

procedure TROHelpSpritesForm.ChooseSprite(Sender: TObject);
var
  idx:integer;
begin
  idx:=FFlt[dgSpriteGrid.Row*4+dgSpriteGrid.Col];
  ModalResult:=mrOk+(idx shl 4);
  // called from menu, not wizard
  if not (fsModal in FormState) then
  begin
    case FMode of
      rosmItem: begin
        with TROFullCardItemForm.Create(Self, idx) do Show;
//        Self.Close;
      end;

      rosmMonster: begin
        if ROShowFullCard then
          with tROFullCardMobForm.Create(Self,idx) do Show
        else
          with tROHelpCardMobForm.Create(Self,idx) do Show;
//        Self.Close;
      end;

      rosmMap: begin
        with TRoRespListForm.Create(Self,rosmMap,idx) do Show;
      end;
    end;
  end;
end;

procedure TROHelpSpritesForm.CheckBoxClick(Sender: TObject);
begin
  dgSpriteGrid.Refresh;
end;

procedure TROHelpSpritesForm.ChangeCategory(Sender: TObject);
var
  i:integer;
begin
  if cbCategory.ItemIndex=0 then
  begin
    for i:=0 to FCount-1 do
      FFlt[i]:=i;
    SGCount:=FCount;
  end
  else
    SGCount:=FilterProc({pointer(FFlt)}@FFlt[0],cbCategory.ItemIndex);

  dgSpriteGrid.Clear;
  dgSpriteGrid.RowCount:=(SGCount+3) div 4;
  dgSpriteGrid.Col:=0;
  dgSpriteGrid.Row:=0;
end;

constructor TROHelpSpritesForm.Create(AOwner:TComponent; aMode:tRODBMode;
      aFilter:tSGFilterProc=nil); overload;
var
  i:integer;
begin
  inherited Create(AOwner);

  FMode:=aMode;

  FilterProc:=aFilter;

  FCount :=RODB_GetCount(aMode);
  SGCount:=FCount;

  SetLength(FArr,FCount);
  SetLength(FFlt,FCount);
  SetLength(FFlg,FCount);
  for i:=0 to FCount-1 do
  begin
    FFlg[i].state:=sprNotChecked;
    FFlt[i]:=i;
  end;

  if FilterProc=nil then
  begin
    case aMode of
      rosmItem   : FilterProc:=@FilterItem;
      rosmMonster: FilterProc:=@FilterMonster;
    end;
    if FilterProc<>nil then
    begin
      FilterProc(cbCategory.Items,-1);
      if cbCategory.Items.Count>0 then
      begin
        cbCategory.ItemIndex:=0;
        ChangeCategory(cbCategory);
      end;
    end
    else
      dgSpriteGrid.RowCount:=(FCount+3) div 4;
  end;

  cbCategory.Visible:=(FilterProc<>nil);
end;

procedure TROHelpSpritesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i:integer;
begin
  CloseAction:=caFree;

  for i:=0 to FCount-1 do
  begin
    if FFlg[i].state=sprLoaded then
      FArr[i].Free;
  end;
  SetLength(FFlg,0);
  SetLength(FArr,0);
  SetLength(FFlt,0);
end;

procedure TROHelpSpritesForm.FormResize(Sender: TObject);
var
  i:integer;
begin
  cbCategory.Width:=(Self.Width div 3)+40;
  cbCategory.Left :=(Self.Width-cbCategory.Width) div 2;

  dgSpriteGrid.BeginUpdate;
  for i:=0 to dgSpriteGrid.RowCount-1 do
    dgSpriteGrid.RowHeights[i]:=dgSpriteGrid.ColWidths[0];
  dgSpriteGrid.EndUpdate;
end;

end.
