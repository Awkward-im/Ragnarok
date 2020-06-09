{$H+}
{$I-}
unit rodataio;

interface

uses
  Simplelist,
  rodatatypes;

const
  //== rAthena
    //-- directories
  defDocDir               = 'doc\';
  defSrcDir               = 'src\map\';
  defDBDir                = 'db\re\';
  defNPCDir               = 'npc\re\';
    //-- doc
  defROEffectFileName     = 'effect_list.txt';
  defROKeywordFileName    = 'script_commands.txt';
  defATCommandsFileName   = 'atcommands.txt';
  defMapFlagFileName      = 'mapflags.txt';
    //-- src/map
  defROConstantFileName   = 'script_constants.hpp';
  defROSkillFileName      = 'skill.hpp';
  defRONPCFileName        = 'npc.hpp';
    //-- db/re
  defROQuestFileName      = 'quest_db.txt';
  defROItemFileName       = 'item_db.txt';
  defROItemTradeFileName  = 'item_trade.txt';
  defROMonsterFileName    = 'mob_db.txt';
  //== GRF
    //-- data
  defROQuestHelpFileName  = 'questid2display.txt';
  defROMapsFileName       = 'mapnametable.txt';
  defROItemSpriteFileName = 'idnum2itemresnametable.txt';
  defROCardSpriteFileName = 'num2cardillustnametable.txt';
  defROMapSpriteFileName  = 'resnametable.txt';

  defROSkillNameFileName  = 'skillnametable.txt';
    //-- data/luafiles514/lua files/datainfo
  defLUAJobNameFileName   = 'jobname.lua';
  defLUANPCConstFileName  = 'npcidentity.lua';

// highlights
type
  tROConstPart = record
    name :PAnsiChar;
    cfrom:integer;
    cto  :integer;
  end;

var
  ROConstParts :tSimpleList;
  ROConstValues:tSimpleList;

//===== Data loading =====

function  ReadConstants():boolean;
procedure FreeConstants;
// load at start
function ReadItems      ():boolean;
function ReadMobs       ():boolean;
function ReadMaps       ():boolean;
function ReadNPC        ():boolean;

function ReadLUANames   ():boolean;
//load by request
function GetQuestLine (const s:String; out rec:TROQuestData):boolean;
function ReadQuests   ():boolean;
function ReadQuestHelp(anidx:integer=-1):boolean;
function ReadSkills   ():boolean;
function ReadEffect   ():boolean;
function ReadMobResp  ():boolean;

//===== Data saving =====

function UpdateQuestList    ():integer;
function UpdateQuestHelpList():integer;
function UpdateMonsterList  ():integer;
function UpdateItemList     ():integer;
function UpdateItemTradeList():integer;

//===== Service functions =====

const
  glcEmpty   = 0;
  glcComment = 1;
  glcValue   = 2;

function GetLineClass(const s:String):integer; overload;
function GetLinePart(var p:PAnsiChar):PAnsiChar;
function GetNameLine(var p:PAnsiChar; var aname:PAnsiChar):PAnsiChar;
procedure SkipToNextLine(var p:PAnsiChar);


implementation

uses
  Classes, SysUtils, StrUtils,
  common, textcache;

//===== Support =====

function GetLineClass(const s:String):integer; overload;
begin
  if Length(s)<2 then exit(glcEmpty);

  if (s[1]='/') and (s[2]='/') then
  begin
    if (Length(s)>2) and (s[3] in ['1'..'9']) then
      result:=glcComment  // commented record
    else
      result:=glcEmpty;   // comment
  end
  else
    result:=glcValue;     // normal record
end;

function GetLineClass(astr:PAnsiChar):integer; overload;
begin
  if (astr=nil) or (astr^=#0) then exit(glcEmpty);
  if (astr^='/') and ((astr+1)^='/') then
  begin
    if (astr+2)^ in ['1'..'9'] then
      result:=glcComment  // commented record
    else
      result:=glcEmpty;   // comment
  end
  else
    result:=glcValue;     // normal record
end;

function GetLinePart(var p:PAnsiChar):PAnsiChar;
begin
  result:=p;

  while not (p^ in [#0,#13,#10,'#']) do inc(p);

  if p<>#0 then
  begin
    p^:=#0; inc(p);
//    while p^ in [#13,#10,'#'] do inc(i);
  end;
end;

procedure SkipToNextLine(var p:PAnsiChar);
begin
  while not (p^ in [#13,#10,#0]) do inc(p);
  while     (p^ in [#13,#10   ]) do inc(p);
end;

function GetNameLine(var p:PAnsiChar; var aname:PAnsiChar):PAnsiChar;
begin
  if p=nil then exit(nil);

  // new line
  while true do
  begin
    if ORD(p^)<32 then
    begin
      if p^=#0 then exit(nil);
    end
    else
    begin
      if (p^='/') and ((p+1)^='/') then
      begin
        while ORD(p^)>=32 do inc(p);
        continue;
      end;
      aname:=p;
      while (p^<>'#') and (ORD(p^)>=32) do inc(p);
      p^:=#0; inc(p);
      result:=p;
      while (p^<>'#') and (ORD(p^)>=32) do inc(p);
      p^:=#0; inc(p);
      exit;
    end;
    inc(p);
  end;
end;

function ReadNameFile(aname:PAnsiChar):PAnsiChar;
var
  f:File of byte;
  cnt:integer;
begin
  AssignFile(f,aname);
  {$I-}
  Reset(f);
  if IOResult=0 then
  begin
    cnt:=FileSize(f);
    GetMem(result,cnt+1);
    BlockRead(f,result^,cnt);
    result[cnt]:=#0;
    CloseFile(f);
  end
  else
    result:=nil;
end;

function GetNumValue(const s:String; var apos:integer):cardinal;
begin
  if apos>Length(s) then
  begin
    result:=0;
    exit;
  end;
  result:=cardinal(common.NumToInt(pointer(s)+apos-1));
//  if result<0 then result:=0; //!!cheat??
  while (apos<Length(s)) and not (s[apos] in [':',',']) do inc(apos);
  inc(apos);
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

//===== Quests =====

function GetQuestLine(const s:String; out rec:TROQuestData):boolean;
var
  lpos:integer;
begin
  case GetLineClass(s) of
    glcValue  : lpos:=1;
    glcComment: lpos:=3;
  else
    exit(false);
  end;

  result:=true;

  rec.id:=GetNumValue(s,lpos);
  // time (sec or HH-MM form)
  // NOW we process seconds only
  rec.time:=GetNumValue(s,lpos);
  // hunt
  rec.hunt_mob1:=GetNumValue(s,lpos);
  rec.hunt_val1:=GetNumValue(s,lpos);
  rec.hunt_mob2:=GetNumValue(s,lpos);
  rec.hunt_val2:=GetNumValue(s,lpos);
  rec.hunt_mob3:=GetNumValue(s,lpos);
  rec.hunt_val3:=GetNumValue(s,lpos);
  // items
  rec.drop_mob1 :=GetNumValue(s,lpos);
  rec.drop_item1:=GetNumValue(s,lpos);
  rec.drop_rate1:=GetNumValue(s,lpos);
  rec.drop_mob2 :=GetNumValue(s,lpos);
  rec.drop_item2:=GetNumValue(s,lpos);
  rec.drop_rate2:=GetNumValue(s,lpos);
  rec.drop_mob3 :=GetNumValue(s,lpos);
  rec.drop_item3:=GetNumValue(s,lpos);
  rec.drop_rate3:=GetNumValue(s,lpos);
  // name without qoutes
  rec.descr:=Copy(s,lpos+1,Length(s)-lpos-1);
  rec.hash :=common.Hash(AnsiUpperCase(rec.descr));
end;

function ReadQuests:boolean;
var
  s:AnsiString;
  slin:TStringList;
  cnt,line:integer;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defROQuestFileName
  else
    s:=RODataDirectory+DefDBDir+defROQuestFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
  begin
    if GetLineClass(slin[line])=glcValue then
      inc(cnt);
  end;

  Finalize (ROQuestData);
  SetLength(ROQuestData,cnt);
  if cnt>0 then
  begin
    FillChar(ROQuestData[0],cnt*SizeOf(ROQuestData[0]),0);

  //----- Fill -----

    cnt:=0;
    for line:=0 to slin.Count-1 do
    begin
      s:=slin[line];
      if GetLineClass(s)=glcValue then
      begin
        GetQuestLine(s,ROQuestData[cnt]);
        inc(cnt);
        if cnt=Length(ROQuestData) then break;
      end;
    end;
  end;

  slin.Free;
end;

procedure ReadQuestHelpLine(sl:TStringList; line:integer; idx:integer);
var
  s:AnsiString;
  i:integer;
begin
  // don't use rest of line atm
  inc(line); s:=sl[line];
  if s[Length(s)]='#' then SetLength(s,Length(s)-1);
  ROQuestData[idx].story:=s;
  inc(line); s:=sl[line];
  if s<>'' then
  begin
    i:=1; while (i<=Length(s)) and (s[i] in [' ','#']) do inc(i);
    if i>Length(s) then
      s:=''
    else
      s:=Copy(s,i,Length(s)-i);
  end;
  ROQuestData[idx].task:=s;
end;

function ReadQuestHelp(anidx:integer=-1):boolean;
var
  s:AnsiString;
  slin:TStringList;
  cnt,lid,prev,line:integer;
begin
  if Length(ROQuestData)=0 then exit(false);

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(defDataDirectory+defROQuestHelpFileName);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Fill -----

  line:=0;
  prev:=0;
  while line<slin.Count do
  begin
    s:=slin[line];
    if GetLineClass(s)=glcValue then
    begin
      lid:=common.StrToInt(pointer(s));
      if anidx>=0 then
      begin
        if ROQuestData[anidx].id=lid then
        begin
          ReadQuestHelpLine(slin,line,anidx);
          break;
        end;
      end
      else
      begin
        for cnt:=prev to High(ROQuestData) do
        begin
          if ROQuestData[cnt].id<lid then continue;

          if ROQuestData[cnt].id=lid then
          begin
            prev:=cnt+1;
            ReadQuestHelpLine(slin,line,cnt);
          end;
          inc(line,2);
          break;
        end;
        if prev>=Length(ROQuestData) then break;
      end;
    end;
    inc(line);
  end;

  slin.Free;
end;

//===== Skills =====

function ReadSkills():boolean;
var
  s:AnsiString;
  slin:TStringList;
  pc:PAnsiChar;
  cnt:integer;
  sline,line,sstart,spos:integer;
  lid:integer;
  lsign:boolean;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defROSkillFileName
  else
    s:=RODataDirectory+DefSrcDir+defROSkillFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  line:=0;
  while line<slin.Count do
  begin
    if pos('e_skill {',slin[line])>0 then
    begin
      inc(line);
      sline:=line;
      break;
    end;
    inc(line);
  end;
  while line<slin.Count do
  begin
    s:=slin[line];
    if s<>'' then
    begin
      spos:=1;
      while spos<=Length(s) do
      begin
        case s[spos] of
          #9,' ': begin
            inc(spos);
            continue;
          end;
          'A'..'Z': begin
            inc(cnt);
          end;
        end;
        break;
      end;
      if s[spos]='}' then break;
    end;
    inc(line);
  end;

  Finalize (ROSkillData);
  SetLength(ROSkillData,cnt);

  if cnt>0 then
  begin
    FillChar(ROSkillData[0],cnt*SizeOf(ROSkillData[0]),0);

    //----- Fill -----
    
    cnt:=0;
    lid:=0;
    while sline<slin.Count do
    begin
      s:=slin[sline];

      if s<>'' then
      begin
        lsign:=false;
        spos:=1;
        pc:=nil;
        while spos<=Length(s) do
        begin
          case s[spos] of
            ',','/','}': break;
            '-': begin
              lsign:=true;
            end;
            '1'..'9': begin
              lid:=0;
              while (spos<=Length(s)) and (s[spos] in ['0'..'9']) do
              begin
                lid:=lid*10+ord(s[spos])-ord('0');
                inc(spos);
              end;
              if lsign then lid:=-lid;
              break;
            end;
            'A'..'Z': begin
              inc(lid);
              sstart:=spos;
              while s[spos] in sLatWord do inc(spos);
              s[spos]:=#0;
              pc:=@s[sstart];
            end;
          end;
          inc(spos);
        end;
        if (spos<=Length(s)) and (s[spos]='}') then break;

        if pc<>nil then
        begin
          with ROSkillData[cnt] do
          begin
            id  :=lid;
            name:=StrCache.Append(pc);
            hash:=common.Hash(SysUtils.UpperCase(pc));
          end;
          inc(cnt);
          if cnt=Length(ROSkillData) then break;
        end;
      end;
      inc(sline);
    end;
  end;

  slin.Free;
end;

//===== constants =====

procedure _Init;
begin
  FillChar(ROConstParts ,SizeOf(ROConstParts ),0);
  FillChar(ROConstValues,SizeOf(ROConstValues),0);
  ROConstParts .Init(SizeOf(tROConstPart));
  ROConstValues.Init(SizeOf(PAnsiChar ));
end;

procedure _Clear;
var
  i:integer;
begin
  for i:=0 to ROConstParts.Count-1 do
    mFreeMem(tROConstPart(ROConstParts[i]^).name);

  for i:=0 to ROConstValues.Count-1 do
    mFreeMem(PAnsiChar(ROConstValues[i]^));
end;

procedure FreeConstants;
begin
  _Clear;

  ROConstParts .Free;
  ROConstValues.Free;
end;

function ReadConstants():boolean;
var
  f:text;
  lline,lname:AnsiString;
  lval:PAnsiChar;
  lpart:tROConstPart;
  lpos,lend:integer;
  curpartindex:integer;
  curconstindex:integer;
  lgotempty:boolean;
begin
  if RODataDirectory='' then
    lname:=defDataDirectory+defROConstantFileName
  else
    lname:=RODataDirectory+DefSrcDir+defROConstantFileName;

  FreeConstants;
  _Init;
  
  AssignFile(f,lname);
  Reset(f);
  if IOResult<>0 then
  begin
    result:=false;
    exit;
  end;

  result:=true;

  curpartindex :=-1;
  curconstindex:=-1;
  while not eof(f) do
  begin
    readln(f,lline);

    if lline<>'' then
    begin
      lpos:=1;
      // skip spaces
      while (lpos<Length(lline)) and (lline[lpos]<=' ') do inc(lpos);
    end;
    if (lline='') or (lpos>=Length(lline)) then
    begin
      lgotempty:=true;
      if curpartindex>=0 then
        tROConstPart(ROConstParts[curpartindex]^).cto:=curconstindex;
      continue;
    end;

    // skip comments and directives
    if (lline[lpos]='#') or
      ((lline[lpos]='/') and (lline[lpos+1]='/')) then
      continue;

    // add comments as new part name
    lpos:=Pos('/*',lline);
    if lpos>0 then
    begin
      inc(lpos,2);
      lend:=PosEx('*/',lline,lpos);
      while lline[lpos]<=' ' do inc(lpos);
      repeat
        dec(lend);
      until lline[lend]>' ';
      lname:=Copy(lline,lpos,lend-lpos+1);
      // skip NPC constants
      if Pos('NPC view',lname)=1 then
        continue;
      StrDup(lpart.name,pointer(lname));
      lpart.cfrom:=-1;
      curpartindex:=ROConstParts.Add(@lpart);
      lgotempty:=false;
    end;

    // add constants
    lpos:=Pos('(',lline);
    if lpos>0 then
    begin
      // skip NPC constants
      if Pos('export_constant_npc',lline)>0 then
        continue;
      inc(lpos);
      if lline[lpos]='"' then
      begin
        inc(lpos);
        lend:=PosEx('"',lline,lpos);
      end
      else
      begin
        lend:=lpos+1;
        while lline[lend] in ['A'..'Z','a'..'z','0'..'9','_'] do inc(lend);
      end;
      lname:=Copy(lline,lpos,lend-lpos);
      StrDup(lval,pointer(lname));
      curconstindex:=ROConstValues.Add(@lval);
      // no name for group,trying to get prefix
      if lgotempty then
      begin
        lpos:=Pos('_',lname);
        if lpos>0 then
        begin
          if (StrCmp(
                pointer(ROConstValues[curconstindex-1]^),
                lval,lpos)<>0) and
             (StrCmp(lpart.name,lval,lpos)<>0) then
          begin
            StrDup(lpart.name,pointer(lname),lpos);
            lpart.cfrom:=curconstindex;
            curpartindex:=ROConstParts.Add(@lpart);
          end;
        end;
        lgotempty:=false;
      end;

      if lpart.cfrom<0 then
      begin
        tROConstPart(ROConstParts[curpartindex]^).cfrom:=curconstindex;
        lpart.cfrom:=curconstindex;
      end;
    end;
  end;
end;

//===== Effect =====

function ReadEffect():boolean;
var
  s:AnsiString;
  slin:TStringList;
  line,i,cnt:integer;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defROEffectFileName
  else
    s:=RODataDirectory+DefDocDir+defROEffectFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count  -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
  begin
    s:=slin[line];
    if (s<>'') and (s[1] in ['0'..'9']) then inc(cnt);
  end;

  Finalize (ROEffectData);
  SetLength(ROEffectData,cnt);
  
  //----- Fill -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
  begin
    s:=slin[line];
    if (s<>'') and (s[1] in ['0'..'9']) then
    begin
      i:=1;
      while s[i] in ['0'..'9'] do inc(i);
      ROEffectData[cnt].id:=StrToInt(Copy(s,1,i-1));
      inc(i); // skip tab/space
      while s[i]<=' ' do inc(i);
      ROEffectData[cnt].descr:=Copy(s,i,1000);
//      ROEffectData[cnt].hash:=common.Hash(ROEffectData[cnt].descr);

      inc(cnt);
      if cnt=Length(ROEffectData) then break;
    end;
  end;

  slin.Free;
end;

//===== Items =====

function ReadItemTrade():boolean;
var
  slin:TStringList;
  s:AnsiString;
  line:integer;
  i,j,prev:integer;
  lid:integer;
begin
  if Length(ROItemData)=0 then exit(false);

  if RODataDirectory='' then
    s:=defDataDirectory+defROItemTradeFileName
  else
    s:=RODataDirectory+DefDBDir+defROItemTradeFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  prev:=0;
  for line:=0 to slin.Count-1 do
  begin
    s:=slin[line];
    if GetLineClass(s)=glcValue then
    begin
      i:=1;
      lid:=GetNumValue(s,i);
      // ignore group level and comments
      for j:=prev to High(ROItemData) do
      begin
        if ROItemData[j].id>=lid then
        begin
          if ROItemData[j].id=lid then
          begin
            prev:=j+1;
            ROItemData[j].trade:=GetNumValue(s,i);
          end;
          break;
        end;
      end;
    end;
  end;

  slin.Free;
end;

function ReadItemSprites():boolean;
var
  f:File of byte;
  ls:array [0..127] of AnsiChar;
  buf,p:PAnsiChar;
  lid:integer;
  len:integer;
  prev,idx:integer;
begin
  result:=false;

  if Length(ROItemData)=0 then exit;

  AssignFile(f,defDataDirectory+defROItemSpriteFileName);
  {$I-}
  Reset(f);
  if IOResult=0 then
  begin
    result:=true;
    len:=FileSize(f);
    GetMem(buf,len+1);
    BlockRead(f,buf^,len);
    buf[len]:=#0;
    p:=buf;
    CloseFile(f);

    prev:=0;

    StrCache.Capacity:=Length(ROItemData)*20;

    repeat
      if GetLineClass(p)=glcValue then
      begin
        lid:=common.StrToInt(GetLinePart(p));

        for idx:=prev to High(ROItemData) do
        begin
          if ROItemData[idx].id>=lid then
          begin
            if ROItemData[idx].id=lid then
            begin
              prev:=idx+1;
              if not ItemIsCard(idx) then
              begin
                ROItemData[idx].Sprite:=StrCache.Append(KoreanToAnsi(GetLinePart(p),@ls));
                CheckItemSprite(idx);
              end;
            end;
            break;
          end;
        end;
      end;
      SkipToNextLine(p);
    until p^=#0;
    FreeMem(buf);
  end;
end;

function ReadCardSprites():boolean;
var
  f:File of byte;
  ls:array [0..127] of AnsiChar;
  buf,p:PAnsiChar;
  lid,idx:integer;
  cnt,i:integer;
begin
  result:=false;

  if Length(ROItemData)=0 then exit;

  //----- Count -----

  cnt:=0;
  for i:=0 to High(ROItemData) do
    if ItemIsCard(i) then inc(cnt);

  //----- Fill -----

  if cnt>0 then
  begin

    AssignFile(f,defDataDirectory+defROCardSpriteFileName);
    {$I-}
    Reset(f);
    if IOResult=0 then
    begin
      StrCache.Capacity:=StrCache.Capacity+cnt*20;

      result:=true;
      cnt:=FileSize(f);
      GetMem(buf,cnt+1);
      BlockRead(f,buf^,cnt);
      buf[cnt]:=#0;
      p:=buf;
      CloseFile(f);

      repeat
        if GetLineClass(p)=glcValue then
        begin
          lid:=common.StrToInt(GetLinePart(p));
          idx:=RODB_GetIndex(lid,rosmItem);
          // just for existing items
          if ItemIsCard(idx) then
          begin
            ROItemData[idx].Sprite:=StrCache.Append(KoreanToAnsi(GetLinePart(p),@ls));
            CheckCardSprite(idx);
          end;
        end;
        SkipToNextLine(p);
      until p^=#0;
      FreeMem(buf);
    end;
  end;
end;

function GetScriptValue(const s:String; var apos:integer):String;
var
  deep:integer;
  start:integer;
begin
  deep:=0;
  start:=0;
  result:='';
  repeat
    if s[apos]='{' then
    begin
      if deep=0 then start:=apos+1;
      inc(deep);
    end
    else if s[apos]='}' then
    begin
      dec(deep);
      if deep=0 then
      begin
        if apos>start then
          result:=Copy(s,start,apos-start);
        break;
      end;
    end;
    // don't use not finished scripts
    if apos=Length(s) then exit;
    inc(apos);
  until false;
  if apos=Length(s) then exit;
  inc(apos);
  if s[apos]=',' then inc(apos);

  if (result<>'') and (result[1]=' ') then result:=Copy(result,2,Length(result));
  if (result<>'') and (result[Length(result)]=' ') then SetLength(result,Length(result)-1);
end;

procedure ReadItemCard(index:integer; const s:string; spos:integer);
begin
  with ROItemData[index] do
  begin
    ItemType  :=GetNumValue(s,spos);
    Buy       :=GetNumValue(s,spos);
    Sell      :=GetNumValue(s,spos);
    Weight    :=GetNumValue(s,spos);
    ATK       :=GetNumValue(s,spos);
    if s[spos-1]=':' then MATK:=GetNumValue(s,spos);
    DEF       :=GetNumValue(s,spos);
    Range     :=GetNumValue(s,spos);
    Slots     :=GetNumValue(s,spos);
    Job       :=GetNumValue(s,spos);
    CharClass :=GetNumValue(s,spos);
    Gender    :=GetNumValue(s,spos);
    Loc       :=GetNumValue(s,spos);
    wLevel    :=GetNumValue(s,spos);
    eLevel    :=GetNumValue(s,spos);
    if s[spos-1]=':' then MaxLevel:=GetNumValue(s,spos);
    Refineable:=GetNumValue(s,spos)<>0;
    View      :=GetNumValue(s,spos);
    Script          :=GetScriptValue(s,spos);
    OnEquip_Script  :=GetScriptValue(s,spos);
    OnUnequip_Script:=GetScriptValue(s,spos);
  end;
end;

function ReadItems():boolean;
var
  slin:TStringList;
  s:AnsiString;
  line,cnt:integer;
  i:integer;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defROItemFileName
  else
    s:=RODataDirectory+DefDBDir+defROItemFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
  begin
    case GetLineClass(slin[line]) of
      glcEmpty  : Continue;
      glcComment: begin
        if ROLoadCommented then
          inc(cnt)
        else
          Continue;
      end;
    else
      inc(cnt);
    end;
  end;

  Finalize (ROItemData);
  SetLength(ROItemData,cnt);
  if cnt>0 then
  begin
    FillChar(ROItemData[0],cnt*SizeOf(ROItemData[0]),0);
    
    //----- Fill -----

    cnt:=0;
    for line:=0 to slin.Count-1 do
    begin
      s:=slin[line];
      case GetLineClass(s) of
        glcEmpty  : Continue;
        glcComment: begin
          if ROLoadCommented then
          begin
            ROItemData[cnt].Disabled:=true;
            i:=3;
          end
          else
            Continue;
        end
      else
        i:=1;
      end;

      with ROItemData[cnt] do
      begin
        id    :=GetNumValue(s,i);
        name  :=StrCache.Append(pointer(GetStrValue(s,i)));
        hash  :=common.Hash(SysUtils.UpperCase(StrCache[name]));
        descr :=GetStrValue(s,i);
      end;
      ReadItemCard(cnt,s,i);

      inc(cnt);
      if cnt=Length(ROItemData) then break;
    end;
  end;

  slin.Free;

  ReadItemTrade;
  ReadItemSprites;
  ReadCardSprites;

end;

//===== Monsters =====

//----- Mob resp -----

procedure ReadRespFile(const fname:AnsiString);
var
  slin:TStringList;
  s,lcmd,lmap:AnsiString;
  line,lpos,i,cnt:integer;
begin
  slin:=TStringList.Create;
  try
    slin.LoadFromFile(fname);
  except
    slin.Free;
    exit;
  end;

  cnt:=Length(ROMobRespData);
  SetLength(ROMobRespData,cnt+slin.Count);
  for line:=0 to slin.Count-1 do
  begin
    s:=slin[line];
    if GetLineClass(s)=glcValue then
    begin
      lpos:=1;
      FillChar(ROMobRespData[cnt],SizeOf(TROMobRespData),0);
      with ROMobRespData[cnt] do
      begin
        lmap:=GetStrValue(s,lpos);
        i:=Pos('.',lmap);
        if i>0 then SetLength(lmap,i-1);
        // x,y%tab%monster%tab%name%tab%id,amount{,delay1{,delay2}}
        while (lpos<Length(s)) and (s[lpos]<>#9) do inc(lpos); inc(lpos);
        i:=lpos;
        while (lpos<Length(s)) and (s[lpos]<>#9) do inc(lpos);
        if lpos>Length(s) then continue;
        lcmd:=SysUtils.LowerCase(Copy(s,i,lpos-i)); inc(lpos);
        if (CompareStr(lcmd,'monster'     )<>0) and
           (CompareStr(lcmd,'boss_monster')<>0) then
          continue;

        i:=lpos;
        while (lpos<Length(s)) and (s[lpos]<>#9) do inc(lpos);
        if lpos>Length(s) then continue;
        name  :=RODB_GetIndex(lmap,rosmMap);
        descr :=Copy(s,i,lpos-i); inc(lpos);
        id    :=GetNumValue(s,lpos);
        amount:=GetNumValue(s,lpos);
        // auto set to 0 if not exists
        delay1:=GetNumValue(s,lpos);
        delay2:=GetNumValue(s,lpos);
      end;
      inc(cnt);
    end;
  end;
  SetLength(ROMobRespData,cnt);

  slin.Free;
end;

procedure FFLoop(const adir:AnsiString);
var
  sr:TSearchRec;
begin
  if FindFirst(adir+'\*.*',faAnyFile and faDirectory,sr)=0 then
  begin
    repeat
     if (sr.Attr and faDirectory)=faDirectory then
     begin
       if (sr.Name<>'.') and (sr.Name<>'..') then
         FFloop(adir+'\'+sr.Name);
     end
     else if ExtractFileExt(sr.Name)='.txt' then
         ReadRespFile(adir+'\'+sr.Name);
    until FindNext(sr)<>0;

    FindClose(sr);
  end;
end;

// adir = path <root>/npc/re/mobs
function ReadMobResp():boolean;
var
  s:AnsiString;
begin
//npc/re/mobs/dungeons/*.txt
//npc/re/mobs/fields/*.txt
//npc/re/mobs/championmobs.txt
  result:=false;

  Finalize(ROMobRespData);

  if RODataDirectory='' then
    s:=defDataDirectory
  else
    s:=RODataDirectory+DefNPCDir;

  FFloop(s+'mobs');
  FFloop(s+'custom');

  result:=Length(ROMobRespData)>0;
end;

procedure ReadMonsterCard(index:integer; const s:string; spos:integer);
var
  i:integer;
begin
  with ROMobData[index] do
  begin
    LVL    :=GetNumValue(s,spos);
    HP     :=GetNumValue(s,spos);
    SP     :=GetNumValue(s,spos);
    EXP    :=GetNumValue(s,spos);
    JEXP   :=GetNumValue(s,spos);
    Range1 :=GetNumValue(s,spos);
    ATK1   :=GetNumValue(s,spos);
    ATK2   :=GetNumValue(s,spos);
    DEF    :=GetNumValue(s,spos);
    MDEF   :=GetNumValue(s,spos);
    STR    :=GetNumValue(s,spos);
    AGI    :=GetNumValue(s,spos);
    VIT    :=GetNumValue(s,spos);
    INT    :=GetNumValue(s,spos);
    DEX    :=GetNumValue(s,spos);
    LUK    :=GetNumValue(s,spos);
    Range2 :=GetNumValue(s,spos);
    Range3 :=GetNumValue(s,spos);
    Scale  :=GetNumValue(s,spos);
    Race   :=GetNumValue(s,spos);
    Element:=GetNumValue(s,spos);
    Mode   :=GetNumValue(s,spos);
    Speed  :=GetNumValue(s,spos);
    aDelay :=GetNumValue(s,spos);
    aMotion:=GetNumValue(s,spos);
    dMotion:=GetNumValue(s,spos);
    MEXP   :=GetNumValue(s,spos);
    for i:=0 to ROMaxMVPDrops-1 do
    begin
      MVPDrop[i].id  :=GetNumValue(s,spos);
      MVPDrop[i].rate:=GetNumValue(s,spos);
    end;
    for i:=0 to ROMaxMobDrops-1 do
    begin
      Drop[i].id  :=GetNumValue(s,spos);
      Drop[i].rate:=GetNumValue(s,spos);
    end;
    Card.id  :=GetNumValue(s,spos);
    Card.rate:=GetNumValue(s,spos);
  end;
end;

function ReadMobs():boolean;
var
  slin:TStringList;
  s:AnsiString;
  lkro:AnsiString;
  line,cnt,i:integer;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defROMonsterFileName
  else
    s:=RODataDirectory+DefDBDir+defROMonsterFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
  begin
    case GetLineClass(slin[line]) of
      glcEmpty  : Continue;
      glcComment: begin
        if ROLoadCommented then
          inc(cnt)
        else
          Continue;
      end
    else
      inc(cnt);
    end;
  end;

  Finalize (ROMobData);
  SetLength(ROMobData,cnt);
  if cnt>0 then
  begin
    FillChar(ROMobData[0],cnt*SizeOf(ROMobData [0]),0);
    
    //----- Fill -----

    cnt:=0;
    for line:=0 to slin.Count-1 do
    begin
      s:=slin[line];
      case GetLineClass(s) of
        glcEmpty  : Continue;
        glcComment: begin
          if ROLoadCommented then
          begin
            ROMobData[cnt].Disabled:=true;
            i:=3;
          end
          else
            Continue;
        end
      else
        i:=1;
      end;
      with ROMobData[cnt] do
      begin
        id   :=GetNumValue(s,i);
        name :=StrCache.Append(pointer(GetStrValue(s,i)));
        hash :=common.Hash(SysUtils.UpperCase(StrCache[name]));
        lkro :=GetStrValue(s,i);
        descr:=GetStrValue(s,i);
        if lkro=descr then lkro:=''; //!!!!
        kro :=lkro;
      end;

      ReadMonsterCard(cnt,s,i);
      
      inc(cnt);
      if cnt=Length(ROMobData) then break;
    end;
  end;

  slin.Free;

  ReadMobResp;
end;

//===== Maps =====

function ReadMapSprites():boolean;
var
  s,virtmap,origmap:AnsiString;
  slin:TStringList;
  line,i,j:integer;
  idx:integer;
begin
  if Length(ROMapData)=0 then exit(false);

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(defDataDirectory+defROMapSpriteFileName);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  for line:=0 to slin.Count-1 do
  begin
    s:=slin[line];
    if GetLineClass(s)=glcValue then
    begin
      i:=pos('\map\',s);
      if i<>0 then
      begin
        inc(i,5);
        j:=i;
        while (j<=Length(s)) and (s[j]<>'.') do inc(j);
        virtmap:=Copy(s,i,j-i);
        j:=length(s);
        while (j>=1) and (s[j]<>'.') do dec(j);
        i:=j-1;
        while (i>1) and (s[i]<>'\') do dec(i);
        origmap:=copy(s,i+1,j-i-1);
        if virtmap<>origmap then
        begin
          idx:=RODB_GetIndex(virtmap,rosmMap);
          if idx>=0 then
            ROMapData[idx].sprite:=RODB_GetIndex(origmap,rosmMap);
        end;
      end;
    end;
  end;
  slin.Free;
end;

function ReadMapIndex():boolean;
var
  slin:TStringList;
  s:AnsiString;
  p:PAnsiChar;
  line,cnt,id:integer;
begin
  slin:=TStringList.Create;
  try
    if RODataDirectory='' then
      s:='db\'+'map_index.txt'
    else
      s:=RODataDirectory+'db\'+'map_index.txt';
    slin.LoadFromFile(s);
  except
    slin.Free;
    result:=false;
    exit;
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
    if GetLineClass(slin[line])=glcValue then inc(cnt);

  Finalize (ROMapData);
  SetLength(ROMapData,cnt);
  if cnt>0 then
  begin
    FillChar(ROMapData[0],cnt*SizeOf(ROMapData[0]),0);

    //----- Fill -----
    cnt:=0;
    id:=0;
    for line:=0 to slin.Count-1 do
    begin
      s:=slin[line];
      inc(id);
      if GetLineClass(s)=glcValue then
      begin
        p:=pointer(s);
        while not (p^ in [#0,#13,#10,'#']) do
        begin
          if p^ in [#9,' '] then
          begin
            id:=StrToInt(p+1);
            break;
          end;
          inc(p);
        end;
        p^:=#0;

        ROMapData[cnt].name:=StrCache.Append(pointer(s));
        ROMapData[cnt].hash:=common.Hash(SysUtils.LowerCase(pointer(s)));
        ROMapData[cnt].id  :=id;
        inc(cnt);
      end;
    end;

    if cnt<Length(ROMapData) then
      SetLength(ROMapData,cnt);
  end;

  slin.Free;
end;

function ReadMaps():boolean;
var
  slin:TStringList;
  s:AnsiString;
//  lname:AnsiString;
//  i:integer;
  p,lname:PAnsiChar;
  line,j,cnt:integer;
  doublefound:boolean;
begin
  slin:=TStringList.Create;
  slin.Sorted:=true;
  try
    slin.LoadFromFile(defDataDirectory+defROMapsFileName);
  except
    slin.Free;
    result:=ReadMapIndex();
    exit;
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  for line:=0 to slin.Count-1 do
    if GetLineClass(slin[line])=glcValue then inc(cnt);

  Finalize (ROMapData);
  SetLength(ROMapData,cnt);
  if cnt>0 then
  begin
    FillChar(ROMapData[0],cnt*SizeOf(ROMapData[0]),0);

    //----- Fill -----

    cnt:=0;
    for line:=0 to slin.Count-1 do
    begin
      s:=slin[line];
      if GetLineClass(s)=glcValue then
      begin
      // search cycle can be shorter when list sorted
      // then we need to check just previous record

        p:=pointer(s);
        lname:=GetLinePart(p); (p-5)^:=#0; // name without ext
        // check for doubles
        doublefound:=false;
        if cnt>0 then
        begin
          if not slin.Sorted then
          begin
            for j:=0 to cnt-1 do
            begin
              if StrCmp(StrCache[ROMapData[j].name],lname)=0 then
              begin
                doublefound:=true;
                break;
              end;
            end;
          end
          else
            doublefound:=StrCmp(StrCache[ROMapData[cnt-1].name],lname)=0;
        end;

        if not doublefound then
        begin
          ROMapData[cnt].descr:=GetLinePart(p);
          ROMapData[cnt].name :=StrCache.Append(lname);
          ROMapData[cnt].hash :=common.Hash(SysUtils.LowerCase(lname));
          inc(cnt);
        end;
{        
        i:=1;
        while s[i]<>'#' do inc(i);
        lname:=Copy(s,1,i-1-4); // name without ext

        // check for doubles
        doublefound:=false;
        for j:=0 to cnt-1 do
        begin
          if ROMapData[j].name=lname then
          begin
            doublefound:=true;
            break;
          end;
        end;

        if not doublefound then
        begin
          with ROMapData[cnt] do
          begin
            descr :=Copy(s,i+1,Length(s)-i-1);
            name  :=StrCache.Append(lname);
//            hash  :=common.Hash(SysUtils.LowerCase(name));
          end;
          
          inc(cnt);
        end;
}
      end;
    end;

    if cnt<Length(ROMapData) then
      SetLength(ROMapData,cnt);
  end;

  slin.Free;

  ReadMapSprites;
end;

//===== NPCs =====

function ReadNPC():boolean;
var
  s:AnsiString;
  slin:TStringList;
  pc:PAnsiChar;
  cnt:integer;
  line,sline,spos,sstart:integer;
  lid:integer;
  lsign:boolean;
begin
  if RODataDirectory='' then
    s:=defDataDirectory+defRONPCFileName
  else
    s:=RODataDirectory+DefSrcDir+defRONPCFileName;

  slin:=TStringList.Create;
  try
    slin.LoadFromFile(s);
  except
    slin.Free;
    exit(false);
  end;

  result:=true;

  //----- Count -----

  cnt:=0;
  line:=0;
  while line<slin.Count do
  begin
    if pos('e_job_types',slin[line])>0 then
    begin
      inc(line);
      sline:=line;
      break;
    end;
    inc(line);
  end;
  while line<slin.Count do
  begin
    s:=slin[line];
    if s<>'' then
    begin
      spos:=1;
      while spos<=Length(s) do
      begin
        case s[spos] of
          #9,' ': begin
            inc(spos);
            continue;
          end;
          'J': begin
            if (s[spos+1]='T') and
               (s[spos+2]='_') then
            begin
              inc(cnt);
            end;
          end;
        end;
        break;
      end;
      if s[spos]='}' then break;
    end;
    inc(line);
  end;

  Finalize (RONPCData);
  SetLength(RONPCData,cnt);
  if cnt>0 then
  begin
    FillChar(RONPCData[0],cnt*SizeOf(RONPCData[0]),0);

    //----- Fill -----
    
    cnt:=0;
    lid:=0;
    while sline<slin.Count do
    begin
      s:=slin[sline];

      if s<>'' then
      begin
        lsign:=false;
        spos:=1;
        pc:=nil;
        while spos<=Length(s) do
        begin
          case s[spos] of
            ',','/','}': break;
            '-': begin
              lsign:=true;
            end;
            '1'..'9': begin
              lid:=0;
              while (spos<=Length(s)) and (s[spos] in ['0'..'9']) do
              begin
                lid:=lid*10+ord(s[spos])-ord('0');
                inc(spos);
              end;
              if lsign then lid:=-lid;
              break;
            end;
            'A'..'Z': begin
              inc(lid);
              sstart:=spos;
              while s[spos] in sLatWord do inc(spos);
              if (s[sstart  ]='J') and
                 (s[sstart+1]='T') and
                 (s[sstart+2]='_') then
              begin
                s[spos]:=#0;
                pc:=@s[sstart+3];
              end;
            end;
          end;
          inc(spos);
        end;
        if (spos<=Length(s)) and (s[spos]='}') then break;

        if pc<>nil then
        begin
          with RONPCData[cnt] do
          begin
            id  :=lid;
            name:=StrCache.Append(pc);
            hash:=common.Hash(SysUtils.UpperCase(pc));
          end;
          inc(cnt);
          if cnt=Length(RONPCData) then break;
        end;
      end;
      inc(sline);
    end;
  end;

  slin.Free;
end;

function ReadLUAConst():boolean;
var
  f:file of byte;
  pc,p,buf:PAnsiChar;
  cnt:integer;
  lvalue:integer;
begin
  Assign(f,defDataDirectory+defLUANPCConstFileName);
  Reset(f);
  if IOResult<>0 then exit(false);
  cnt:=FileSize(f);
  GetMem(buf,cnt+1);
  BlockRead(f,buf^,cnt);
  Close(f);
  buf[cnt]:=#0;

  result:=true;

  //----- Count -----

  cnt:=0;
  p:=buf;
  while p^<>#0 do
  begin
    p:=StrPos(p,'JT_');
    if p<>nil then
    begin
      inc(cnt);
      inc(p,4);
    end
    else break;
  end;

  StrCache.Capacity:=StrCache.Capacity+cnt*20;
  
  Finalize (RONames);
  SetLength(RONames,cnt);

  //----- Fill -----

  if cnt>0 then
  begin
    FillChar(RONames[0],cnt*SizeOf(RONames[0]),0);

    p:=buf;
    cnt:=0;
    while p^<>#0 do
    begin
      p:=StrPos(p,'JT_');
      if p<>nil then
      begin
        inc(p,3);
        pc:=p;
        while pc^ in ['_','0'..'9','A'..'Z'] do inc(pc);
        if pc^=#0 then break
        else
        begin
          pc^:=#0;
          inc(pc);
        end;
        RONames[cnt].name:=StrCache.Append(p);
        while not (pc^ in [#0,#13,#10,'0'..'9']) do inc(pc);
        lvalue:=0;
        while pc^ in ['0'..'9'] do
        begin
          lvalue:=lvalue*10+(ord(pc^)-ord('0'));
          inc(pc);
        end;
        RONames[cnt].value:=lvalue;
        inc(cnt);
        p:=pc;
      end
      else
        break;
    end;
  end;

  FreeMem(buf);
end;

function ReadLUANames():boolean;
var
  f:file of byte;
  p,pc,buf:PAnsiChar;
  ls:array [0..300] of AnsiChar;
  i,cnt:integer;
  lgotid:boolean;
begin
  Assign(f,defDataDirectory+defLUAJobNameFileName);
  Reset(f);
  if IOResult<>0 then exit(false);

  i:=FileSize(f);
  GetMem(buf,i+1);
  BlockRead(f,buf^,i);
  Close(f);
  buf[i]:=#0;

  result:=true;

  lgotid:=ReadLUAConst();

  StrCache.Capacity:=StrCache.Capacity+(i div 2);

  //----- Count -----
  if Length(RONames)=0 then
  begin
    cnt:=0;
    p:=buf;
    while p^<>#0 do
    begin
      p:=StrPos(p,'jobtbl.JT_');
      if p<>nil then inc(cnt)
      else break;
      inc(p,10);
    end;

    Finalize (RONames);
    SetLength(RONames,cnt);
    FillChar (RONames[0],SizeOf(RONames[0])*cnt,0);
  end;

  //----- Fill -----

  p:=buf;
  cnt:=0;
  while p^<>#0 do
  begin
    p:=StrPos(p,'jobtbl.JT_');
    if p<>nil then
    begin
      inc(p,10);
      pc:=p;
      while pc^<>']' do inc(pc);
      if pc^=#0 then break;
      pc^:=#0;
      inc(pc);
      // search p(name) in RONames
      if lgotid then
      begin
        for i:=0 to High(RONames) do
        begin
          if StrCmp(p,StrCache[RONames[i].name])=0 then
          begin
            while not (pc^ in [#0,#13,#10,'"']) do inc(pc);
            if pc^='"' then
            begin
              inc(pc);
              p:=pc;
              while p^<>'"' do inc(p);
              p^:=#0;
              inc(p);
              // with ".gr2" extention atm
              RONames[i].data:=StrCache.Append(KoreanToAnsi(pc,@ls));
            end;
            break;
          end;
        end;
      end
      else
      begin
        RONames[cnt].name:=StrCache.Append(p);
        while not (pc^ in [#0,#13,#10,'"']) do inc(pc);
        if pc^='"' then
        begin
          inc(pc);
          p:=pc;
          while p^<>'"' do inc(p);
          p^:=#0;
          inc(p);
          // with ".gr2" extention atm
          RONames[cnt].data:=StrCache.Append(KoreanToAnsi(pc,@ls));
          inc(cnt);
        end;
      end;
    end
    else break;
  end;

  FreeMem(buf);

  //----- Mob sprites (NPC the same), can be placed to main cycle -----

  for i:=0 to High(RONames) do
  begin
     if RONames[i].value<>0 then
      cnt:=RODB_GetIndex(RONames[i].value,rosmMonster)
    else
      cnt:=RODB_GetIndex(StrCache[RONames[i].name],rosmMonster);
    if cnt>=0 then
      ROMobData[cnt].sprite:=RONames[i].data;
  end;

end;


//===== Data saving =====

//----- Common -----

type
  tIDtype = (id_found, id_comment, id_insert);

function SearchIDInList(sl:TStringList; anId:integer; out typ:tIDtype):integer;
var
  ls:AnsiString;
  i,j,lid:integer;
begin
  // linear search (not including commented)
  typ:=id_insert;
  for i:=0 to sl.Count-1 do
  begin
    ls:=sl[i];

    case GetLineClass(ls) of
      glcEmpty: continue;
      glcComment: j:=2;
    else
      j:=0;
    end;
{
    if ls='' then continue;
    if (ls[1]='/') and (ls[2]='/') then
      j:=2
    else
      j:=0;
}
    lid:=common.StrToInt(pointer(ls)+j);
    if anId=lid then
    begin
      if j=0 then typ:=id_found
      else        typ:=id_comment;
      exit(i)
    end
    else if anId<lid then
    begin
      exit(i);
    end;
  end;
  result:=sl.Count;
end;


//----- Monster -----

function GenerateMobLine(idx:integer):String;
var
  i:integer;
begin
  with ROMobData[idx] do
  begin
    if disabled then
      result:='//'
    else
      result:='';

    result:=result+IntToStr(id)+',';

    result:=result+StrCache[name]+',';
    if kro='' then
      result:=result+descr
    else
      result:=result+kro;
    result:=result+','+descr+',';

    result:=result+IntToStr(LVL)        +',';
    result:=result+IntToStr(HP)         +',';
    result:=result+IntToStr(SP)         +',';
    result:=result+IntToStr(EXP)        +',';
    result:=result+IntToStr(JEXP)       +',';
    result:=result+IntToStr(Range1)     +',';
    result:=result+IntToStr(ATK1)       +',';
    result:=result+IntToStr(ATK2)       +',';
    result:=result+IntToStr(DEF)        +',';
    result:=result+IntToStr(MDEF)       +',';
    result:=result+IntToStr(STR)        +',';
    result:=result+IntToStr(AGI)        +',';
    result:=result+IntToStr(VIT)        +',';
    result:=result+IntToStr(INT)        +',';
    result:=result+IntToStr(DEX)        +',';
    result:=result+IntToStr(LUK)        +',';
    result:=result+IntToStr(Range2)     +',';
    result:=result+IntToStr(Range3)     +',';
    result:=result+IntToStr(Scale)      +',';
    result:=result+IntToStr(Race)       +',';
    result:=result+IntToStr(Element)    +',';
    result:=result+'0x'+IntToHex(Mode,8)+',';
    result:=result+IntToStr(Speed)      +',';
    result:=result+IntToStr(aDelay)     +',';
    result:=result+IntToStr(aMotion)    +',';
    result:=result+IntToStr(dMotion)    +',';
    result:=result+IntToStr(MEXP)       +',';

    for i:=0 to ROMaxMVPDrops-1 do
    begin
      result:=result+IntToStr(MVPDrop[i].id  )+',';
      result:=result+IntToStr(MVPDrop[i].rate)+',';
    end;

    for i:=0 to ROMaxMobDrops-1 do
    begin
      result:=result+IntToStr(Drop[i].id  )+',';
      result:=result+IntToStr(Drop[i].rate)+',';
    end;

    result:=result+IntToStr(Card.id  )+',';
    result:=result+IntToStr(Card.rate)+',';
  end;
end;

function UpdateMonsterList():integer;
var
  sl:TStringList;
  s:AnsiString;
  i,j:integer;
  typ:tIDtype;
  dosave:boolean;
begin
  result:=0;

  // check for changed
  dosave:=false;
  for i:=0 to High(ROMobData) do
    if ROMobData[i].modified then
    begin
      dosave:=true;
      break;
    end;

  if not dosave then exit;

  if RODataDirectory='' then
    s:=defDataDirectory+defROMonsterFileName
  else
    s:=RODataDirectory+DefDBDir+defROMonsterFileName;

  dosave:=false;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(s);
    for i:=0 to High(ROMobData) do
    begin
      if ROMobData[i].modified then
      begin
        j:=SearchIDInList(sl,ROMobData[i].id,typ);
        if typ=id_found then
        begin
          inc(result);
          dosave:=true;
          sl[j]:=GenerateMobLine(i);
        end;
      end;
    end;
    if dosave then
      try
        sl.SaveToFile(s);
      except
        result:=-result;
      end;
  except
  end;
  sl.Free;

  if result>0 then
    for i:=0 to High(ROMobData) do
      ROMobData[i].modified:=false;
end;

//----- Item -----

function GenerateItemLine(idx:integer):String;
begin
  with ROItemData[idx] do
  begin
    if disabled then
      result:='//'
    else
      result:='';

    result:=IntToStr(id) +',';
    result:=result+StrCache[name]+',';
{
    if (Length(descr)>3) and (descr[Length(descr)-2]='[') then
      result:=result+Copy(descr,1,Length(descr)-3)
    else
}
    result:=result+descr;
    result:=result+',';

    result:=result+IntToStr(ItemType    )+',';
    result:=result+IntToStr(Buy         )+',';
    if (Sell<>0) then result:=result+IntToStr(Sell);
    result:=result+',';
    result:=result+IntToStr(Weight      )+',';

    if (ATK <>0) or (ItemType in [5,10]) or (MATK<>0) then result:=result+IntToStr(ATK);
    if (MATK<>0) then result:=result+':'+IntToStr(MATK);
    result:=result+',';
    if (DEF<>0) or (ItemType in [4,12]) then result:=result+IntToStr(DEF);
    result:=result+',';
    if (Range<>0) or (ItemType in [5]) then result:=result+IntToStr(Range);
    result:=result+',';
    if (Slots<>0) or (ItemType in [4,5,12]) then result:=result+IntToStr(Slots);
    result:=result+',';

    result:=result+'0x'+IntToHex(Job,8  )+',';
    result:=result+IntToStr(CharClass   )+',';
    result:=result+IntToStr(Gender      )+',';
    if (Loc<>0) or (ItemType in [4,5,10,12,18]) then result:=result+IntToStr(Loc);
    result:=result+',';
    if (wLevel<>0) or (ItemType in [5]) then result:=result+IntToStr(wLevel);
    result:=result+',';
    if (eLevel<>0) then result:=result+IntToStr(eLevel);
    if (eLevel>0) and (MaxLevel>0) then
      result:=result+':'+IntToStr(MaxLevel);
    result:=result+',';
    if (Refineable) or (ItemType in [4,5,12]) then result:=result+IntToStr(ord(Refineable));
    result:=result+',';

    if (View<>0) then result:=result+IntToStr(View);
    result:=result+',';

    if Script          ='' then result:=result+'{},'
    else                        result:=result+'{ '+Script+' },';
    if OnEquip_Script  ='' then result:=result+'{},'
    else                        result:=result+'{ '+OnEquip_Script  +' },';
    if OnUnEquip_Script='' then result:=result+'{}'
    else                        result:=result+'{ '+OnUnEquip_Script+' }';
  end;
end;

function UpdateItemList():integer;
var
  sl:TStringList;
  s:AnsiString;
  i,j:integer;
  typ:tIDtype;
  dosave:boolean;
begin
  result:=0;

  dosave:=false;
  for i:=0 to High(ROItemData) do
    if (ROItemData[i].modified and 1)<>0 then
    begin
      dosave:=true;
      break;
    end;

  if not dosave then exit;

  if RODataDirectory='' then
    s:=defDataDirectory+defROItemFileName
  else
    s:=RODataDirectory+DefDBDir+defROItemFileName;

  dosave:=false;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(s);
    for i:=0 to High(ROItemData) do
    begin
      if (ROItemData[i].modified and 1)<>0 then
      begin
        j:=SearchIDInList(sl,ROItemData[i].id,typ);
        if typ=id_found then
        begin
          sl[j]:=GenerateItemLine(i);
          inc(result);
          dosave:=true;
        end;
      end;
    end;
    if dosave then
      try
        sl.SaveToFile(s);
      except
        result:=-result;
      end;
  except
  end;
  sl.Free;

  if result>0 then
    for i:=0 to High(ROItemData) do
      ROItemData[i].modified:=ROItemData[i].modified and not 1;
end;

function UpdateItemTradeList():integer;
var
  sl:TStringList;
  s:AnsiString;
  res:AnsiString;
  typ:tIDtype;
  i,j:integer;
  dosave:boolean;
begin
  result:=0;
  
  dosave:=false;
  for i:=0 to High(ROItemData) do
    if (ROItemData[i].modified and 2)<>0 then
    begin
      dosave:=true;
      break;
    end;

  if not dosave then exit;
  
  if RODataDirectory='' then
    s:=defDataDirectory+defROItemTradeFileName
  else
    s:=RODataDirectory+DefDBDir+defROItemTradeFileName;

  dosave:=false;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(s);
    for i:=0 to High(ROItemData) do
    begin
      with ROItemData[i] do
      begin
        if (modified and 2)<>0 then
        begin
          inc(result);
          dosave:=true;

          j:=SearchIDInList(sl,id,typ);
          // trade<>0, need to add or replace
          if (trade<>0) {and not disabled} then
          begin
            res:=IntToStr(id) +','+IntToStr(trade)+',100  // '+
              RODB_GetName(RODB_GetIndex(id,rosmItem),rosmItem);
            if typ=id_insert then
              sl.Insert(j,res)
            else // if (typ=id_found) or (typ=id_comment) then
            begin
              sl[j]:=res;
            end;
          end
          // trade=0, need to delete or comment at least
          else
          begin
            if typ=id_found then
              if ROCommentToDel then
                sl[j]:='//'+sl[j]
              else
                sl.Delete(j);
          end;
        end;
      end;
    end;
    if dosave then
      try
        sl.SaveToFile(s);
      except
        result:=-result;
      end;
  except
  end;
  sl.Free;

  if result>0 then
    for i:=0 to High(ROItemData) do
      ROItemData[i].modified:=ROItemData[i].modified and not 2;
end;

//----- Quest -----

function GenerateQuestLine(idx:integer):String;
begin
  result:='';
  with ROQuestData[idx] do
  begin
    result:=result+IntToStr(id)+',';
    result:=result+IntToStr(time)+',';

    result:=result+IntToStr(hunt_mob1)+',';
    result:=result+IntToStr(hunt_val1)+',';

    result:=result+IntToStr(hunt_mob2)+',';
    result:=result+IntToStr(hunt_val2)+',';

    result:=result+IntToStr(hunt_mob3)+',';
    result:=result+IntToStr(hunt_val3)+',';

    result:=result+IntToStr(drop_mob1 )+',';
    result:=result+IntToStr(drop_item1)+',';
    result:=result+IntToStr(drop_rate1)+',';

    result:=result+IntToStr(drop_mob2 )+',';
    result:=result+IntToStr(drop_item2)+',';
    result:=result+IntToStr(drop_rate2)+',';

    result:=result+IntToStr(drop_mob3 )+',';
    result:=result+IntToStr(drop_item3)+',';
    result:=result+IntToStr(drop_rate3)+',';

    result:=result+'"'+descr+'"';
  end;
end;

function UpdateQuestList():integer;
var
  sl:TStringList;
  s:AnsiString;
  i,j:integer;
  typ:tIDtype;
  dosave:boolean;
begin
  result:=0;

  dosave:=false;
  for i:=0 to High(ROQuestData) do
    if (ROQuestData[i].modified and 1)<>0 then
    begin
      dosave:=true;
      break;
    end;

  if not dosave then exit;

  if RODataDirectory='' then
    s:=defDataDirectory+defROQuestFileName
  else
    s:=RODataDirectory+DefDBDir+defROQuestFileName;

  dosave:=false;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(s);
    for i:=0 to High(ROQuestData) do
    begin
      if (ROQuestData[i].modified and 1)<>0 then
      begin
        j:=SearchIDInList(sl,ROQuestData[i].id,typ);
        if typ=id_found then
        begin
          sl[j]:=GenerateQuestLine(i);
          inc(result);
          dosave:=true;
        end;
      end;
    end;
    if dosave then
      try
        sl.SaveToFile(s);
      except
        result:=-result;
      end;
  except
  end;
  sl.Free;

  if result>0 then
    for i:=0 to High(ROQuestData) do
      ROItemData[i].modified:=ROItemData[i].modified and not 1;
end;

function UpdateQuestHelpList():integer;
begin
  result:=0;
end;

end.
