unit rodoc;

interface

uses
  classes,
  simplelist;

type
  pRODocument = ^tRODocument;
  tRODocument = object
  private
    StoredFile:tStringList;

    ROCards :tSimpleList;
    ROParts :tSimpleList;
    ROCommon:tSimpleList;

    procedure Analize;
    function GetAnyDescr(const anArr:tSimpleList; aname:PAnsiChar):AnsiString;

    function GetDocCount:integer;
    function GetDoc(idx:integer):PAnsiChar;
    function GetDocDescr(acard:PAnsiChar):AnsiString;

    function GetPartCount:integer;
    function GetCardCount:integer;
    function GetPartNumber(idx:integer):integer;
    function GetPart (idx:integer):PAnsiChar;
    function GetCard (idx:integer):PAnsiChar;
    function GetDescr(acard:PAnsiChar):AnsiString;
    function GetCardPart(idx:integer):integer;
  public
    procedure Init;
    procedure Clear;
    procedure Free;
    function  Load(const aname:AnsiString=''):boolean;

    property DocParts:integer read GetDocCount;
    property Doc[idx:integer]:PAnsiChar read GetDoc;
    property DocDescr[acard:PAnsiChar]:AnsiString read GetDocDescr;
    
    property PartCount              :integer   read GetPartCount;
    property PartNumber[idx:integer]:integer   read GetPartNumber;
    property Parts     [idx:integer]:PAnsiChar read GetPart;

    property CardCount            :integer     read GetCardCount;
    // returns not real part number but part index
    property CardPart[idx:integer]:integer     read GetCardPart;
    property Cards   [idx:integer]:PAnsiChar   read GetCard;

    property Descr[acard:PAnsiChar]:AnsiString read GetDescr;
  end;

implementation

uses
  common;

//  constfilename = 'script_constants.hpp';

//===== Script documentation processing =====

type
  tROCard = record
    name :PAnsiChar;
    index:integer;
    ciend:integer;
    part :integer;
  end;
  tROPart = record
    name  :PAnsiChar;
    number:integer;
    sub   :integer;
  end;


function tRODocument.GetDocCount:integer;
begin
  result:=ROCommon.Count;
end;

function tRODocument.GetPartCount:integer;
begin
  result:=ROParts.Count;
end;

function tRODocument.GetCardCount:integer;
begin
  result:=ROCards.Count;
end;

function tRODocument.GetDoc(idx:integer):PAnsiChar;
begin
  if (idx>=0) and (idx<integer(ROCommon.Count)) then
    result:=tROCard(ROCommon[idx]^).name;
end;

function tRODocument.GetPartNumber(idx:integer):integer;
begin
  if (idx>=0) and (idx<integer(ROParts.Count)) then
    result:=tROPart(ROParts[idx]^).sub*1000+
            tROPart(ROParts[idx]^).number
end;

function tRODocument.GetPart(idx:integer):PAnsiChar;
begin
  if (idx>=0) and (idx<integer(ROParts.Count)) then
    result:=tROPart(ROParts[idx]^).name;
end;

function tRODocument.GetCard(idx:integer):PAnsiChar;
begin
  if (idx>=0) and (idx<integer(ROCards.Count)) then
    result:=tROCard(ROCards[idx]^).name
end;

function tRODocument.GetCardPart(idx:integer):integer;
begin
  if (idx>=0) and (idx<integer(ROCards.Count)) then
    result:=tROCard(ROCards[idx]^).part
  else
    result:=-1;
end;

function tRODocument.GetAnyDescr(const anArr:tSimpleList; aname:PAnsiChar):AnsiString;
var
  i,j:integer;
begin
  result:='';
  for i:=0 to anArr.Count-1 do
  begin
    if StrCmp(aname,tROCard(anArr[i]^).name)=0 then
    begin
      for j:=tROCard(anArr[i]^).index to tROCard(anArr[i]^).ciend do
      begin
        result:=result+StoredFile[j]+#13#10;
      end;

      break;
    end;
  end;
end;

function tRODocument.GetDocDescr(acard:PAnsiChar):AnsiString;
begin
  result:=GetAnyDescr(ROCommon,acard);
end;

function tRODocument.GetDescr(acard:PAnsiChar):AnsiString;
begin
  result:=GetAnyDescr(ROCards,acard);
end;

procedure tRODocument.Init;
begin
  StoredFile:=nil;

  FillChar(ROParts ,SizeOf(ROParts),0);
  FillChar(ROCards ,SizeOf(ROCards),0);
  FillChar(ROCommon,SizeOf(ROCards),0);

  ROParts .Init(SizeOf(tROPart));
  ROCards .Init(SizeOf(tROCard));
  ROCommon.Init(SizeOf(tROCard));
end;

procedure tRODocument.Clear;
var
  i:integer;
begin
  for i:=0 to ROParts.Count-1 do
    mFreeMem(tROPart(ROParts[i]^).name);
  ROParts.Clear;

  for i:=0 to ROCards.Count-1 do
    mFreeMem(tROCard(ROCards[i]^).name);
  ROCards.Clear;

  for i:=0 to ROCommon.Count-1 do
    mFreeMem(tROCard(ROCommon[i]^).name);
  ROCommon.Clear;
end;

procedure tRODocument.Free;
begin
  Clear;

  if StoredFile<>nil then
  begin
    StoredFile.Free;
    StoredFile:=nil;
  end;
  ROParts .Free;
  ROCards .Free;
  ROCommon.Free;
end;

procedure tRODocument.Analize;
var
  lsoc    :AnsiString;
  ldup    :PAnsiChar;
  lpart   :integer;
  lsubpart:integer;
  i       :integer;
  lpos,
  lstart,
  lend    :integer;
  lropart :tROPart;
  lrocard :tROCard;
begin
  Clear;

  lsoc:=StringOfChar('-',39);
  lrocard.part :=0;
  lrocard.index:=0;

  // insert "special" part
  lropart.number:=0;
  lropart.sub   :=0;
  StrDup(lropart.name,'All keywords');
  ROParts.Add(@lropart);
  lpart:=0;
  
  i:=0;
  while i<StoredFile.Count do
  begin
    if StoredFile[i]<>'' then
    begin

      //-- Check for non-command parts

      if lpart=0 then
      begin
        if (StoredFile[i][1]='-') and (StoredFile[i][2]='-') then
        begin
          if StoredFile[i-1]='' then
          begin
            lrocard.ciend:=i-1;
            ROCommon.Add(@lrocard);
          end
          else
          begin
            if lrocard.index>0 then
            begin
              lrocard.ciend:=i-2;
              ROCommon.Add(@lrocard);
            end;
            lrocard.index:=i+1;
            StrDup(lrocard.name,PAnsiChar(StoredFile[i-1]));
          end;
        end;
      end;

      //-- Check for part header
      
      if (StoredFile[i][1]='=') and (i<(StoredFile.Count-2)) and
         (StoredFile[i+1] <>'') and (StoredFile[i+2]<>'') then
      begin
        // looks like part header
        if (StoredFile[i+1][1]='|') and
           (StoredFile[i+2][1]='=') then
        begin
          lsubpart:=lropart.number;
          lpos:=1;
          repeat inc(lpos) until not (StoredFile[i+1][lpos] in [#9,' ']);
          lropart.number:=StrToInt(PAnsiChar(StoredFile[i+1])+lpos-1);
          if lropart.number>0 then
          begin
            if lropart.number=lsubpart then
              lropart.sub:=lropart.sub+1
            else
              lropart.sub:=0;
            lsubpart:=0;
            lstart:=Pos(' ',Copy(StoredFile[i+1],lpos,1000))+lpos;
            lend  :=Length( StoredFile[i+1])-1; // minus dot
            StrDup(lropart.name,PAnsiChar(StoredFile[i+1])+lstart-1,lend-lstart);
            lpart:=ROParts.Add(@lropart);
            inc(i,2);
          end;
        end;
      end;

      //-- Check for card and sub-part headers

      if (lpart>0) and (StoredFile[i][1]='-') and (i<(StoredFile.Count-4)) then
      begin
        if (StoredFile[i  ]=lsoc) and
           (StoredFile[i+1]<>'') and ((StoredFile[i+1][1]='\') or (StoredFile[i+1][1]='/')) and
           (StoredFile[i+2]<>'') and
           (StoredFile[i+3]<>'') and ((StoredFile[i+3][1]='\') or (StoredFile[i+3][1]='/')) and
           (StoredFile[i+4]=lsoc) then
        begin
          if (StoredFile[i+1][1]='\') and
             (Pos('nd of',StoredFile[i+2])=0) then // for slash formatting error
          begin
            lropart.sub:=lropart.sub+1;

            lstart:=Pos(' ',StoredFile[i+2])+1;
            lend  :=Length( StoredFile[i+2]);
            if StoredFile[i+2][lend]='.' then
              dec(lend);
            StrDup(lropart.name,PAnsiChar(StoredFile[i+2])+lstart-1,lend-lstart);
            lsubpart:=ROParts.Add(@lropart);
          end
          else
            lsubpart:=-lsubpart;

          inc(i,4);
        end;
        
        // looks like command description
        if (StoredFile[i  ]=lsoc) and
           (StoredFile[i+1]='') and
           (StoredFile[i+2]<>'') and
           (StoredFile[i+2][1] in ['@','*']) then
        begin
          inc(i,2);
          lrocard.index:=i;
          if lsubpart>0 then
            lrocard.part:=lsubpart
          else
            lrocard.part:=lpart;

          // Search end of description
          lrocard.ciend:=i+1;
          while StoredFile[lrocard.ciend]<>lsoc do inc(lrocard.ciend);
          dec(lrocard.ciend);

          ldup:=nil;
          // for case with several commands in one card
          while (StoredFile[i]<>'') and (StoredFile[i][1] in ['@','*']) do
          begin
            lend:=2;
            while (lend<=Length(StoredFile[i])) and
                (StoredFile[i][lend] in ['_','-','0'..'9','A'..'Z','a'..'z']) do
              inc(lend);
            StrDup(lrocard.name,PAnsiChar(StoredFile[i])+1,lend-2);
            if ldup<>nil then
            begin
              if StrCmp(lrocard.name,ldup)=0 then
              begin
                mFreeMem(lrocard.name);
              end;
            end;
            if lrocard.name<>nil then
            begin
              ROCards.Add(@lrocard);
              ldup:=lrocard.name;
            end;
            inc(i);
          end;

          i:=lrocard.ciend;
        end;
      end;
    end;

    inc(i);
  end;

end;

function tRODocument.Load(const aname:AnsiString):boolean;
begin
  if StoredFile<>nil then
  begin
    StoredFile.Free;
    StoredFile:=nil;
  end;
  StoredFile:=TStringList.Create;
  try
    StoredFile.LoadFromFile(aname);
  except
    result:=false;
    StoredFile.Free;
    StoredFile:=nil;
    exit;
  end;
  result:=true;
  Analize;
end;

end.
