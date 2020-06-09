unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ComCtrls, {uTypes,zPack,uConst,} ExtCtrls;

const
  fwAsk          = 0;
  fwSkip         = 1;
  fwSkipAll      = 2;
  fwRename       = 3;
  fwOverwrite    = 4;
  fwOverwriteAll = 5;
  fwAbort        = 6;
const
  byExt  = 0;
  byName = 1;
  byDir  = 2;
  byText = 3;
type
  tGrfHdr=packed record {GRF file header}
    ID:array [0..15] of char;
    Un1:array [0..13] of byte;
    DirOffset:longword;  {Real offset=DirOffset+$2E}
    Un2:longword;
    DirEntries:longword; {Real Entries=DirEntries-Un2-7}
    fType:longword;
  end;
  tChunk=packed record {Directory chunk w/o name}
    psize1:longword;
    psize2:longword;
    usize:longword;
    Un1:byte;
    dofs:longword;
  end;
type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    btOpen: TButton;
    btSaveDir: TButton;
    btExit: TButton;
    StringGrid1: TStringGrid;
    Label1: TLabel;
    btExtract: TButton;
    Label2: TLabel;
    btSortMode: TButton;
    btUpdate: TButton;
    cbDumpDir: TCheckBox;
    btSearchNext: TButton;
    trkPack: TTrackBar;
    Label4: TLabel;
    btUpdateDir: TButton;
    edSearch: TLabeledEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function  ReadDir(fname:string):Boolean;
    function  fUnpack(aRow:UInt):integer;
    function  fPack(so:tStream;aRow:UInt):longword;
    procedure SetReady(aType:UInt);
    procedure CompileDir(so:tStream);
    procedure Search(StartPos:UInt);
    procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure UpdateDir1;
    procedure UpdateDir2;
    procedure cmOpen(Sender: TObject);
    procedure cmExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmUnpack(Sender: TObject);
    procedure cmSaveDir(Sender: TObject);
    procedure cmHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure cmSortMode(Sender: TObject);
    procedure cmSearch(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure smSearchNext(Sender: TObject);
    procedure cmPack(Sender: TObject);
    procedure cmUpdDir(Sender: TObject);
  private
    { Private declarations }
  public
    TmpVar:longint;
    ExtractFileMode:UInt;
    CurrentFName:string;
    SearchString:string;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  grfio,
//  zpack,
  Unit2;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ExtractFileMode:=fwAsk;
  SearchString:='';
  //----------------
  if grfio_init('grf-files.txt')>0 then ReadDir('');
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  grfio_final();
end;

procedure TForm1.cmOpen(Sender: TObject);
begin
   if OpenDialog1.Execute then
   begin
     Form1.Caption:=OpenDialog1.FileName;
     if ReadDir(OpenDialog1.FileName) then
     begin
       btSaveDir.Enabled:=true;
       btExtract.Enabled:=true;
       btUpdate .Enabled:=true;
       edSearch .Enabled:=true;

       ExtractFileMode:=fwAsk;
       Label2.Caption:='Unsorted';
       trkPack.Position:=6;
     end
     else
     begin
       btSaveDir.Enabled:=false;
       btExtract.Enabled:=false;
       btUpdate .Enabled:=false;
       edSearch .Enabled:=false;

       btSearchNext.Enabled:=false;
       Label2.Caption:='';
       StringGrid1.RowCount:=2;
       StringGrid1.Rows[1].Clear;
       Label1.Caption:='Total 0 records';
     end
   end
   else OpenDialog1.FileName:='';
end;

procedure TForm1.cmExit(Sender: TObject);
begin
  Close;
end;
{$H+}
type
  korstr = type string(949);

function TForm1.ReadDir(fname:string):Boolean;
var
  i:integer;
  tsize:longword;
  f:PFILELIST;
  ls:korstr;
begin
  result:=true;

  if filelist_count=0 then
  begin
    grfio_add(pointer(fname));
  end;

  StringGrid1.BeginUpdate;
  StringGrid1.RowCount:=filelist_count+1;
  tsize:=0;
  for i:=1 to filelist_count do
  begin
    f:=filelist_out(i-1);
    inc(tsize,f^.declen);
    StringGrid1.Cells[0,i]:='$'+IntToHex(f^.srcpos,8);
    StringGrid1.Cells[1,i]:='$'+IntToHex(f^.srclen,8);
    StringGrid1.Cells[2,i]:='$'+IntToHex(f^.declen,8);
    ls:=f^.fn;
    StringGrid1.Cells[3,i]:=ls;
    StringGrid1.Cells[4,i]:=IntToStr(i-1);
  end;
  StringGrid1.EndUpdate;
  Label1.Caption:='Total '+IntToStr(tsize)+' bytes ('+
                  IntToStr(tsize div (1024*1024))+' Mbytes) in '+
                  IntToStr(filelist_count)+' records';
end;

function TForm1.fUnpack(aRow:UInt):integer;
var
  buf:pByte;
  fp:file of byte;
  i:integer;
begin
  fUnpack:=0;
  with StringGrid1 do
  begin
    if aRow=0 then
      Exit;
    CurrentFName:=Cells[3,aRow];// filelist_out(StrToInt(Cells[4,aRow]))^.fn;

    i:=Length(CurrentFName);
    while CurrentFName[i]<>'\' do
      dec(i);
    ForceDirectories('.\'+Copy(CurrentFName,1,i));
    while FileExists(CurrentFName) do
    begin
      if ExtractFileMode=fwSkipAll then
      begin
        Exit;
      end
      else if ExtractFilemode=fwOverwriteAll then
        break;
      Form2.Edit1.Text:=CurrentFName;
      Form2.ShowModal;
      case ExtractFileMode of
        fwSkip: begin
          ExtractFileMode:=fwAsk;
          Exit;
        end;
        fwSkipAll: begin
          Exit;
        end;
        fwOverwrite: begin
          ExtractFileMode:=fwAsk;
          break;
        end;
        fwOverwriteAll: begin
          break;
        end;
        fwRename: begin
          ExtractFileMode:=fwAsk;
          CurrentFName:=Form2.Edit1.Text;
        end;
        fwAbort: begin
          ExtractFileMode:=fwAsk;
          fUnpack:=-2;
          Exit;
        end;
      end;
    end;
  end;

  buf:=grfio_reads(
    filelist_out(
      StrToInt(StringGrid1.Cells[4,aRow]))^.fn{pointer(CurrentFName)},@i);
  if buf=nil then
  begin
    MessageDlg('File '+CurrentFName+' unpacking error',mtError,[mbOk],0);
    fUnpack:=-1;
    exit;
  end
  else
    fUnpack:=1;

  AssignFile(fp,CurrentFName);
  Rewrite(fp);
  BlockWrite(fp,buf^,i);
  CloseFile(fp);

  FreeMem(buf);
end;

procedure TForm1.cmUnpack(Sender: TObject);
var
  myRect:TGridRect;
  i,j:UInt;
  k:integer;
  Msg:tagMsg;
begin
  myRect:=StringGrid1.Selection;
  j:=0;
  with myRect do
  begin
    if Top=Bottom then
    begin
      if fUnpack(Top)>0 then
        MessageDlg('File '+StringGrid1.Cells[3,Top]+' succesfuly unpacked',
                 mtInformation,[mbOk],0)
    end
    else
    begin
      SetReady(0);
      for i:=Top to Bottom do
      begin
        Caption:='Unpack file '+IntToStr(i)+' from '+IntToStr(Bottom-Top+1);
        k:=fUnpack(i);
        if k>=0 then inc(j);
{
        while PeekMessage(Msg,0,0,0,pm_Remove) do
        begin
          if Msg.Message=wm_Quit then
          begin
            MessageDlg('Aborted by User',mtInformation,[mbOk],0);
            k:=-2;
          end;
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
}
        if k=-2 then break;
      end;
      SetReady(1);
      if j>0 then
        MessageDlg(IntToStr(j)+' files succesfuly processed',
                   mtInformation,[mbOk],0);
    end;
  end;
end;

procedure TForm1.cmSaveDir(Sender: TObject);
var
  i:UInt;
  so:tStringList;
begin
  so:=tStringList.Create;
  with StringGrid1 do
  begin
    for i:=1 to RowCount-1 do
    begin
      so.Add(Cells[0,i]+':'+Cells[1,i]+'->'+Cells[2,i]+'='+Cells[3,i]);
    end;
  end;
  so.SaveToFile(OpenDialog1.FileName+'.dir');
  so.Free;
  MessageDlg('Directory succesfuly saved to '+OpenDialog1.FileName+'.dir',
             mtInformation,[mbOk],0);
end;

procedure TForm1.cmHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  Label2.Caption:='Sorted';
end;

procedure TForm1.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  s1,s2:String;
begin
  s1:=(Sender as TStringGrid).Cells[ACol,ARow];
  s2:=(Sender as TStringGrid).Cells[BCol,BRow];
  case aCol of
    0..2: result:=StrToInt(s1)-StrToInt(s2);
    3: begin
      case btSortMode.tag of
        byExt : result:=CompareText(ExtractFileExt (s1),ExtractFileExt (s2));
        byName: result:=CompareText(ExtractFileName(s1),ExtractFileName(s2));
        byDir : result:=CompareText(ExtractFileDir (s1),ExtractFileDir (s2));
      else
        result:=CompareText(s1,s2);
      end;
   end;
  end;
  if (Sender as TStringGrid).SortOrder=soDescending then
    result:=-result;
end;

procedure TForm1.cmSortMode(Sender: TObject);
begin
  with Sender as tButton do
  begin
    tag:=tag+1;
    if tag=4 then tag:=0;
    case tag of
      byExt : Caption:='Sort &By Ext';
      byName: Caption:='Sort &By Name';
      byDir : Caption:='Sort &By Dir';
      byText: Caption:='Sort &By Text';
    end;
  end;
end;

procedure TForm1.Search(StartPos:UInt);
var
  rc:UInt;
begin
  rc:=StringGrid1.RowCount;
  while StartPos<rc do
  begin
    if Pos(SearchString,UpperCase(StringGrid1.Cells[3,StartPos]))<>0 then
    begin
      StringGrid1.Row:=StartPos;
      Exit;
    end;
    inc(StartPos);
  end;
  btSearchNext.Enabled:=false;
end;

procedure TForm1.cmSearch(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
  if Key=VK_RETURN then
  begin
    StringGrid1.Row:=1;
    StringGrid1.SetFocus;
    SearchString:=UpperCase(edSearch.Text);
    Search(1);
    btSearchNext.Enabled:=true;
  end;
end;

procedure TForm1.smSearchNext(Sender: TObject);
begin
  StringGrid1.SetFocus;
  Search(StringGrid1.Row+1);
end;

function TForm1.fPack(so:tStream;aRow:UInt):longword;
var
  si:tFileStream;
  i:integer;
  li:longword;
begin
  if not FileExists(StringGrid1.Cells[3,aRow]) then
  begin
    if MessageDlg('File'+StringGrid1.Cells[3,aRow]+' not exist. Continue?',
                   mtWarning,[mbOk,mbCancel],0)<>mrOk then
      fPack:=$FFFFFFFF
    else
      fPack:=0;
    exit;
  end;
  si:=tFileStream.Create(StringGrid1.Cells[3,aRow],fmOpenRead);
  li:=so.Size;
//  i:=pack(si,so,si.Size,trkPack.Position);
  if i<>0 then
  begin
    fPack:=0;
  end
  else
    fPack:=so.Size-li;
  si.Free;
end;

procedure TForm1.SetReady(aType:UInt);
begin
  if aType=0 then
  begin
    btOpen     .Enabled:=false;
    btSaveDir  .Enabled:=false;
    btExtract  .Enabled:=false;
    btSortMode .Enabled:=false;
    btUpdate   .Enabled:=false;
    btUpdateDir.Enabled:=false;
    trkPack    .Enabled:=false;
    edSearch   .Enabled:=false;
    btExit.Caption:='Cancel';
    btSearchNext.Enabled:=false;
  end
  else
  begin
    btOpen     .Enabled:=true;
    btSaveDir  .Enabled:=true;
    btExtract  .Enabled:=true;
    btSortMode .Enabled:=true;
    btUpdate   .Enabled:=true;
    btUpdateDir.Enabled:=true;
    trkPack    .Enabled:=true;
    edSearch   .Enabled:=true;
    btExit.Caption:='E&xit';
  end;
end;

procedure TForm1.cmPack(Sender: TObject);
var
  myRect:TGridRect;
  grfHdr:tGrfHdr;
  so:tStream;
  dLen,lLen:longword;
  OldPos:longword;
  i,j:UInt;
  Msg:tagMsg;
begin
//  myRect:=StringGrid1.Selection;
//  so:=tMemoryStream.Create;

  so:=tFileStream.Create('\out.grf',fmCreate);
  so.Seek(SizeOf(GrfHdr),soFromBeginning);
  j:=0;
(*
  with myRect do
  begin
    if Top=Bottom then
    begin
      lLen:=fPack(so,Top);
      if lLen>0 then
      begin
        StringGrid1.Cells[1,Top]:='$'+IntToHex(lLen,8);
        MessageDlg('File '+StringGrid1.Cells[3,Top]+' succesfuly packed',
                 mtInformation,[mbOk],0)
      end
    end
    else
    begin
*)
      dLen:=0;
      SetReady(0);
//      for i:=Top to Bottom do
      UpdateDir1;
      for i:=1 to StringGrid1.RowCount-1 do
      begin
//        Caption:='Pack file '+IntToStr(i)+' from '+IntToStr(Bottom-Top+1);
        Caption:='Pack file '+IntToStr(i)+' from '+IntToStr(StringGrid1.RowCount-1);
        OldPos:=so.Position;
        lLen:=fPack(so,i);
        if lLen=$FFFFFFFF then
          break
        else if lLen>0 then
        begin
          inc(dLen,lLen);
          StringGrid1.Cells[0,i]:='$'+IntToHex(OldPos,8);
          StringGrid1.Cells[1,i]:='$'+IntToHex(lLen,8);
          inc(j);
        end;
{
        while PeekMessage(Msg,0,0,0,pm_Remove) do
        begin
          if Msg.Message=wm_Quit then
          begin
            MessageDlg('Aborted by User',mtInformation,[mbOk],0);
            so.Free;
            SetReady(1);
            exit;
          end;
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
}
      end;
      SetReady(1);
      if j>0 then
        MessageDlg(IntToStr(j)+' files succesfuly processed to '+IntToStr(dLen),
                   mtInformation,[mbOk],0);
//    end;
//  end;
  with GrfHdr do
  begin
    StrCopy(ID,'Master of Magic');
    Un2:=0;
    DirOffset:=so.Position-$2E;
    DirEntries:=StringGrid1.RowCount-1+7;
    fType:=$200;
  end;
  CompileDir(so);
  so.Seek(0,soFromBeginning);
  so.Write(GrfHdr,SizeOf(GrfHdr));
  so.Free;
end;

procedure TForm1.UpdateDir1;
var
  i,j:UInt;
  tSize:longword;
  SearchRec:TSearchRec;
begin
  i:=1;
  j:=0;
  tSize:=0;
  with StringGrid1 do
  begin
    while i<(RowCount-j) do
    begin
      if FindFirst(Cells[3,i],faAnyFile,SearchRec)=0 then
      begin
        Cells[2,i]:='$'+IntToHex(SearchRec.Size,8); {update usize}
        inc(tSize,SearchRec.Size);
        inc(i);
      end
      else  {move to end}
      begin
        Cols[0].Exchange(i,RowCount-1-j);
        Cols[1].Exchange(i,RowCount-1-j);
        Cols[2].Exchange(i,RowCount-1-j);
        Cols[3].Exchange(i,RowCount-1-j);
        inc(j);
      end;
      FindClose(SearchRec);
    end;
    RowCount:=RowCount-j; {delete non-existing files}
    Label1.Caption:='Total '+IntToStr(tsize)+' bytes ('+
                  IntToStr(tsize div (1024*1024))+' Mbytes) in '+
                  IntToStr(RowCount-1)+' records';
  end;
end;

procedure TForm1.cmUpdDir(Sender: TObject);
begin
  UpdateDir2;
  StringGrid1.SetFocus;
end;

procedure TForm1.UpdateDir2;
var
  tsize:longword;

  procedure Recurse(Dir:string);
  var
    SearchRec:TSearchRec;
    Separator:string;
  begin
    if Copy(Dir,Length(Dir),1)='\' then
      Separator:=''
    else
      Separator:='\';
    if FindFirst(Dir+Separator+'*.*',faAnyFile,SearchRec) = 0 then
    begin
      while FindNext(SearchRec)=0 do
      begin
        if (SearchRec.Attr and faDirectory)=0 then
        begin
          inc(tsize,SearchRec.Size);
          with StringGrid1 do
          begin
            Cells[0,RowCount]:='';
            Cells[1,RowCount]:='';
            Cells[2,RowCount]:='$'+IntToHex(SearchRec.Size,8);
            Cells[3,RowCount]:=Dir+Separator+SearchRec.Name;
            RowCount:=RowCount+1;
          end;
        end
        else if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then
          Recurse(Dir+Separator+SearchRec.Name);
      end;
    end;
    FindClose(SearchRec);
  end;

begin
  StringGrid1.RowCount:=1;
  Recurse('data\');
  StringGrid1.FixedRows:=1;
  Label1.Caption:='Total '+IntToStr(tsize)+' bytes ('+
                IntToStr(tsize div (1024*1024))+' Mbytes) in '+
                IntToStr(StringGrid1.RowCount-1)+' records';
end;

procedure TForm1.CompileDir(so:tStream);
var
  lso,pso:tMemoryStream;
  i:UInt;
  chunk:tChunk;
  n:array [0..255] of char;
  sLen,pLen:longword;
begin
  lso:=tMemoryStream.Create;
  with StringGrid1 do
  begin
    for i:=1 to RowCount-1 do
    begin
      StrPCopy(n,Cells[3,i]);
      lso.Write(n,StrLen(n)+1);
      with chunk do
      begin
        psize1:=StrToInt(Cells[1,i]);
        psize2:=psize1;
        usize:=StrToInt(Cells[2,i]);
        Un1:=1;
        dofs:=StrToInt(Cells[0,i])-$2E;
      end;
      lso.Write(chunk,sizeOf(chunk));
    end;
  end;
  lso.Seek(0,soFromBeginning);
  pso:=tMemoryStream.Create;
  sLen:=lso.Size;
//  pack(lso,pso,sLen,trkPack.Position);
  pLen:=pso.Size;
  so.Write(pLen,4);
  so.Write(sLen,4);
  so.CopyFrom(pso,0);
  pso.Free;
  lso.Free;
end;

end.
