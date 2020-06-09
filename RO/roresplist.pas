unit RORespList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids,
  rodatatypes;

type

  { TRORespListForm }

  TRORespListForm = class(TForm)
    imgSprite: TImage;
    sgRespList: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sgRespListDblClick(Sender: TObject);
    procedure sgRespListSelection(Sender: TObject; aCol, aRow: Integer);
  private
    fmode:tRODBMode;
    procedure PrepareForm(amode:tRODBMode; anidx:integer);
    procedure FillForm   (amode:tRODBMode; anidx:integer);

  public
    constructor Create(AOwner:TComponent; amode:tRODBMode; anidx:integer); overload;
  end;

var
  RORespListForm: TRORespListForm;

implementation

uses
  ROFullCardMob, ROHelpCardMob;

{$R *.lfm}

resourcestring
  sMapResp = 'Monsters on map';
  sMobResp = 'Maps with monster';

{ TRORespListForm }

procedure TRORespListForm.FillForm(amode:tRODBMode; anidx:integer);
var
  ls:AnsiString;
  lrespidx,lidx:integer;
  lid,cnt:integer;
begin
  sgRespList.Clear;
  sgRespList.RowCount:=1; // titles

  lrespidx:=-1;
  case amode of
    rosmMap: begin
      repeat
        lidx:=GetNextMapMonster(anidx,lrespidx);
        if lidx>=0 then
        begin
          cnt:=sgRespList.RowCount;
          sgRespList.RowCount:=sgRespList.RowCount+1;
          sgRespList.Cells[0,cnt]:=RODB_GetText(lidx,rosmMonster);
          sgRespList.Cells[1,cnt]:=IntToStr(ROMobRespData[lrespidx].amount);
          if ROMobRespData[lrespidx].delay2>0 then
            ls:='±'+IntToStr(ROMobRespData[lrespidx].delay2 div 1000)
          else
            ls:='';
          sgRespList.Cells[2,cnt]:=IntToStr(ROMobRespData[lrespidx].delay1 div 1000)+ls;
                               ;
          sgRespList.Cells[3,cnt]:=IntToStr(lidx);
        end
        else break;
      until false;
    end;

    rosmMonster: begin
      lid:=RODB_GetIdNum(anidx,rosmMonster);
      repeat
        lidx:=GetNextMonsterMap(lid,lrespidx);
        if lidx>=0 then
        begin
          cnt:=sgRespList.RowCount;
          sgRespList.RowCount:=sgRespList.RowCount+1;
          sgRespList.Cells[0,cnt]:=RODB_GetText(lidx,rosmMap);
          sgRespList.Cells[1,cnt]:=IntToStr(ROMobRespData[lrespidx].amount);
          if ROMobRespData[lrespidx].delay2>0 then
            ls:='±'+IntToStr(ROMobRespData[lrespidx].delay2 div 1000)
          else
            ls:='';
          sgRespList.Cells[2,cnt]:=IntToStr(ROMobRespData[lrespidx].delay1 div 1000)+ls;
          sgRespList.Cells[3,cnt]:=IntToStr(lidx);
        end
        else break;
      until false;
    end;
  end;
  if sgRespList.RowCount>1 then
    sgRespListSelection(sgRespList,0,1);
end;

procedure TRORespListForm.PrepareForm(amode:tRODBMode; anidx:integer);
var
  ls:AnsiString;
begin
  case amode of
    rosmMap: begin
      ls:=sMapResp;
    end;
    rosmMonster: begin
      ls:=sMobResp;
    end;
  end;
  sgRespList.Columns[0].Title.Caption:=ls;
  Caption:=ls+' '+RODB_GetText(anidx,amode);
end;

procedure TRORespListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

constructor TRORespListForm.Create(AOwner:TComponent; amode:tRODBMode; anidx:integer);
begin
  inherited Create(AOwner);

  fmode:=amode;
  PrepareForm(amode,anidx);
  FillForm(amode,anidx);
end;

procedure TRORespListForm.sgRespListSelection(Sender:TObject; aCol, aRow:Integer);
var
  lfname:AnsiString;
  idx:integer;
begin
  idx:=StrToInt(sgREspList.Cells[3,aRow]);
  imgSprite.Picture.Clear;
  case fmode of
    rosmMap    : lfname:=RODB_GetSprite(idx,rosmMonster);
    rosmMonster: lfname:=RODB_GetSprite(idx,rosmMap);
  end;
  try
    imgSprite.Picture.LoadFromFile(lfname);
  except
  end;
end;

procedure TRORespListForm.sgRespListDblClick(Sender: TObject);
var
  idx:integer;
begin
  idx:=StrToInt(sgRespList.Cells[3,sgRespList.Row]);
  case fmode of
    rosmMonster: begin
      fmode:=rosmMap;
      PrepareForm(fmode,idx);
      FillForm   (fmode,idx);
    end;
    rosmMap: begin
      if ROShowFullCard then
        with TROFullCardMobForm.Create(Self,idx) do Show
      else
        with tROHelpCardMobForm.Create(Self,idx) do Show;
    end;
  end;
end;

end.

