{$H+}

unit rodatatypes;

interface

uses
  TextCache;

resourcestring
  sAEName  = 'AE name';
  sName    = 'Name';
  sDescr   = 'Description';
  sLocal   = 'Local';
  sMap     = 'Maps';
  sItem    = 'Items';
  sMonster = 'Monsters';
  sEffect  = 'Effects';
  sSkill   = 'Skills';
  sNumber  = 'Number';
  sQuest   = 'Quest';
  sAll     = 'All';
  sBoss    = 'Boss';
  sSize    = 'Size';
  sRace    = 'Race';
  sElement = 'Element';

  sTabSprites = 'Sprites: ';
  sTabTable   = 'Table: ';

var
  StrCache:tTextCache;

const
  defDataDirectory = 'data\';
  defSettingsFile  = 'rodata.ini';

//===== Filter =====

type
  tSGFilterProc = function (aptr:pointer; aval:integer):integer;

function FilterItem   (aptr:pointer; aval:integer):integer;
function FilterMonster(aptr:pointer; aval:integer):integer;

//===== NPCs =====

type
  TFacing = (
    facNorth,
    facNorthWest,
    facWest,
    facSouthWest,
    facSouth,
    facSouthEast,
    facEast,
    facNorthEast
    );

type
  tRONPCdata = record
    NPCName: AnsiString;
    NPCMap : AnsiString;
    Sprite : AnsiString;
    XPos,
    YPos   : cardinal;
    Facing : TFacing;
  end;

const
  ROShopTypeCount = 5;
  cRoShopType:array [0..ROShopTypeCount-1] of String = (
    'shop',
    'cashshop',
    'itemshop',
    'pointshop',
    'marketshop'
  );

//===== Types =====

var
  RONames:array of record
    name :integer;
    value:integer;
    data :integer;
  end;

//===== Base =====

{
  Sprite:
    =0 - not checked (use name)
    >0 - StrCache index (index=0 reserved)
    -1 - unknown?
}
type
  pROBaseData = ^tROBaseData;
  tROBaseData = object
    name  :integer;   // internal, saved in cache
    descr :AnsiString; // display, !!rename to title (and add "descr" to quests)
    hash  :cardinal;   // name hash (maybe will not needed)
    id    :integer;
    sprite:integer;
  end;
  tROBaseArray = array of tROBaseData;

var ROEffectData:array of tROBaseData;
var ROSkillData :array of tROBaseData;
var RONPCData   :array of tROBaseData;

//----- Quest -----

type
  tROQuestData = object(tROBaseData)
    // questid2display.txt data
    story:AnsiString;
    task :AnsiString;
    // quest_db.txt data
    time:integer;
    hunt_mob1,
    hunt_mob2,
    hunt_mob3:integer;
    drop_mob1,
    drop_mob2,
    drop_mob3:integer;
    drop_item1,
    drop_item2,
    drop_item3:integer;
    hunt_val1,
    hunt_val2,
    hunt_Val3:word;
    drop_rate1,
    drop_rate2,
    drop_rate3:word;

    modified:byte;
  end;
var ROQuestData:array of TROQuestData;

type
  tROMapData = object(tROBaseData)
    local :AnsiString;
  end;
var ROMapData:array of tROMapData;

//----- Items -----

type
  pROItemData = ^tROItemData;
  tROItemData = object(tROBaseData)
    local :AnsiString;
    Script          :AnsiString;
    OnEquip_Script  :AnsiString;
    OnUnequip_Script:AnsiString;
    Buy       :cardinal;
    Sell      :cardinal;
    Job       :cardinal;
    Loc       :cardinal;
    View      :cardinal;
    Weight    :word;
    ATK       :word;
    MATK      :word;
    DEF       :word;
    Trade     :word;
    ItemType  :byte;
    Range     :byte;
    Slots     :byte;
    CharClass :byte;
    Gender    :byte;
    wLevel    :byte;
    eLevel    :byte;
    MaxLevel  :byte;
    Modified  :byte;
    Refineable:bytebool;
    Disabled  :bytebool;
  end;
var ROItemData:array of tROItemData;

//----- Monsters -----

type
  tRODropData = record
    id  :integer;
    rate:integer;
  end;
const
  ROMaxMVPDrops = 3;
  ROMaxMobDrops = 9;
type
  pROMobData = ^tROMobData;
  tROMobData = object(tROBaseData)
    kro     :AnsiString;
    local   :AnsiString;
    HP      :cardinal;
    SP      :cardinal;
    EXP     :cardinal;
    JEXP    :cardinal;
    MEXP    :cardinal;
    Mode    :cardinal;
    MVPDrop :array [0..ROMaxMVPDrops-1] of tRODropData;
    Drop    :array [0..ROMaxMobDrops-1] of tRODropData;
    Card    :tRODropData;
    ATK1    :word;
    ATK2    :word;
    DEF     :word;
    MDEF    :word;
    STR     :word;
    AGI     :word;
    VIT     :word;
    INT     :word;
    DEX     :word;
    LUK     :word;
    Range1,
    Range2,
    Range3  :word;
    Speed   :word;
    aDelay  :word;
    aMotion :word;
    dMotion :word;
    LVL     :byte;
    Scale   :byte;
    Race    :byte;
    Element :byte; //element+level*20
    Disabled:bytebool;
    Modified:bytebool;
  end;
var ROMobData:array of tROMobData;

type
  tROMobRespData = object(tROBaseData)
    amount:integer;
    delay1,
    delay2:integer;
  end;
var ROMobRespData:array of tROMobRespData;

//===== Keywords =====

const
  ROAddKeyCount = 16;
  ROAddKeyNames:array [0..ROAddKeyCount-1] of AnsiString = (
    'script',
    'monster',
    'mapflag',
    'monster',
    'warp',
    'warp2',
    'duplicate',
    'shop',
    'cashshop',
    'itemshop',
    'pointshop',
    'marketshop',
    'else',
    'case',
    'break',
    'default'
  );


//===== procedures =====

//----- NPC related -----

function GetNPCPos (const arg:tRONPCdata):AnsiString;
function GetNPCCode(const cmd:AnsiString; const arg:tRONPCdata):AnsiString;

//===== Object =====

type
  tRODBMode = (
    rosmNone,
    rosmItem,rosmMonster,rosmNPC,rosmMap,
    rosmEffect,rosmSkill,rosmQuest);

function RODB_GetInfo  (                    aMode:tRODBMode; out acount,asize:integer):pointer;
function RODB_GetSize  (                    aMode:tRODBMode):integer;
function RODB_GetCount (                    aMode:tRODBMode):integer;
function RODB_GetIdNum (idx:integer       ; aMode:tRODBMode):integer;
function RODB_GetId    (idx:integer       ; aMode:tRODBMode):String;  overload;
function RODB_GetId    (const aname:String; aMode:tRODBMode):String;  overload;
function RODB_GetName  (idx:integer       ; aMode:tRODBMode):String; 
function RODB_GetText  (idx:integer       ; aMode:tRODBMode):String;  overload;
function RODB_GetText  (idx,alt:integer   ; aMode:tRODBMode):String;  overload;
function RODB_GetTitle (idx:integer       ; aMode:tRODBMode):String; 
function RODB_GetBase  (idx:integer       ; aMode:tRODBMode):integer;
function RODB_GetSprite(idx,num:integer   ; aMode:tRODBMode):String;  overload;
function RODB_GetSprite(idx:integer       ; aMode:tRODBMode):String;  overload;
function RODB_GetSprite(const aname:String; aMode:tRODBMode):String;  overload;
function RODB_GetIndex (anid:integer      ; aMode:tRODBMode):integer; overload;
function RODB_GetIndex (const aname:String; aMode:tRODBMode):integer; overload;
function RODB_GetField(idx:integer; fnum:integer; aMode:tRODBMode):String;
function RODB_GetFieldCount(aMode:tRODBMode):integer;


function ModeToText(aMode:tRODBMode):String;

function GetNextDropMonster(itemid:integer; curmob:integer; out rate:integer):integer;
function GetNextMapMonster(amap:integer; var idx:integer):integer;
function GetNextMonsterMap(anid:integer; var idx:integer):integer;

function KoreanToAnsi(astr:PAnsiChar):AnsiString; overload;
function KoreanToAnsi(astr,buf:PAnsiChar):PAnsiChar; overload;

function  GetItemSprite  (idx:integer):String;
procedure CheckItemSprite(idx:integer);
procedure CheckCardSprite(idx:integer);
function ItemIsCard(idx:integer):boolean;

//----- Settings -----

procedure LoadSettings;
procedure SaveSettings;

var
  RODataDirectory:AnsiString = '';
  ROColorScheme  :AnsiString = '';
  ROShowFullCard :boolean=false;
  ROAllowEditCard:boolean=false;
  ROAutoLoadLocal:boolean=true;
  ROFilesAreACP  :boolean=true;
  ROCommentToDel :boolean=true;
  ROLoadCommented:boolean=false;
  ROAutoAddScheme:boolean=true;


implementation

uses
  SysUtils, Classes,
  cmemini, common,
  rodata;

//===== Special Sprites =====

const
  sprHealing  = 'cat_healing';
  sprUsable   = 'cat_usable';
  sprEtc      = 'cat_etc';
  sprCard     = 'cat_card';
  sprPetEgg   = 'cat_petegg';
  sprPetEquip = 'cat_petequip';
  sprSkill    = 'cat_skill';
  sprDelayed  = 'cat_delayed';
  sprHead     = 'cat_head';
  sprGarment  = 'cat_garment';
  sprAccess   = 'cat_access';
  sprShield   = 'cat_shield'; // separate?
  sprArmor    = 'cat_armor';
  sprShoes    = 'cat_shoes';
  sprUnknown  = 'cat_unknown';

const
  WeaponSprites: array [0..30] of AnsiString = (
    'cat_barefist',
    'cat_daggers',
    'cat_one-handedswords',
    'cat_two-handedswords',
    'cat_one-handedspears',
    'cat_two-handedspears',
    'cat_one-handedaxes',
    'cat_two-handedaxes',
    'cat_maces',
    '',
    'cat_staves',
    'cat_bows',
    'cat_knuckles',
    'cat_instruments',
    'cat_whips',
    'cat_books',
    'cat_katars',
    'cat_revolvers',
    'cat_rifles',
    'cat_gatlingguns',
    'cat_shotguns',
    'cat_grenadelaunchers',
    'cat_fuumashurikens',
    'cat_two-handedstaves',
    '',
    'cat_dual-wielddaggers',
    'cat_dual-wieldswords',
    'cat_dual-wieldaxes',
    'cat_dagger-sword',
    'cat_dagger-axe',
    'cat_sword-axe'
  );
const
  AmmoSprites: array [0..9] of AnsiString = (
    '',
    'cat_arrows',
    'cat_thdaggers',
    'cat_bullets',
    'cat_shells',
    'cat_grenades',
    'cat_shuriken',
    'cat_kunai',
    'cat_cannonballs',
    'cat_sling'
  );

//----- Settings -----

procedure LoadSettings;
var
  lini:tINIFile;
  pc:PAnsiChar;
begin
  CreateINIFile(lini,defSettingsFile);
  pc:='Settings';

  RODataDirectory:=lini.ReadString(nil,pc,'DataDir'       ,RODataDirectory);
  if (RODataDirectory<>'') and
     (RODataDirectory[Length(RODataDirectory)]<>'\') then
      RODataDirectory:=RODataDirectory+'\';
  ROColorScheme  :=lini.ReadString(nil,pc,'ColorScheme'   ,ROColorScheme);
  ROShowFullCard :=lini.ReadBool  (nil,pc,'ShowFullCard'  ,ROShowFullCard);
  ROAllowEditCard:=lini.ReadBool  (nil,pc,'AllowEditCard' ,ROAllowEditCard);
  ROAutoLoadLocal:=lini.ReadBool  (nil,pc,'AutoLoadLocal' ,ROAutoLoadLocal);
  ROFilesAreACP  :=lini.ReadBool  (nil,pc,'FilesAreACP'   ,ROFilesAreACP);
  ROCommentToDel :=lini.ReadBool  (nil,pc,'CommentDeleted',ROCommentToDel);
  ROLoadCommented:=lini.ReadBool  (nil,pc,'LoadCommented' ,ROLoadCommented);
  ROAutoAddScheme:=lini.ReadBool  (nil,pc,'AutoAddScheme' ,ROAutoAddScheme);
  
  FreeINIFile(lini);
end;

procedure SaveSettings;
var
  lini:tINIFile;
  pc:PAnsiChar;
begin
  CreateINIFile(lini,defSettingsFile);
  pc:='Settings';

  lini.WriteString(nil,pc,'DataDir'       , RODataDirectory);
  lini.WriteString(nil,pc,'ColorScheme'   , ROColorScheme);
  lini.WriteBool  (nil,pc,'ShowFullCard'  , ROShowFullCard);
  lini.WriteBool  (nil,pc,'AllowEditCard' , ROAllowEditCard);
  lini.WriteBool  (nil,pc,'AutoLoadLocal' , ROAutoLoadLocal);
  lini.WriteBool  (nil,pc,'FilesAreACP'   , ROFilesAreACP);
  lini.WriteBool  (nil,pc,'CommentDeleted', ROCommentToDel);
  lini.WriteBool  (nil,pc,'LoadCommented' , ROLoadCommented);
  lini.WriteBool  (nil,pc,'AutoAddScheme' , ROAutoAddScheme);

  lini.Flush;
  FreeINIFile(lini);
end;

//----- NPC related -----

function GetNPCPos(const arg:tRONPCdata):AnsiString;
begin
  if arg.NPCMap='' then
    result:='-'
  else
    result:=arg.NPCMap+','+IntToStr(arg.XPos)+','+IntToStr(arg.YPos)+','+IntToStr(ord(arg.Facing));
end;

function GetNPCCode(const cmd:AnsiString; const arg:tRONPCdata):AnsiString;
var
  ls:AnsiString;
begin
  if arg.NPCName='' then
    ls:='NPC'
  else
    ls:=arg.NPCName;
  result:=GetNPCPos(arg)+#9+cmd+#9+ls+#9+arg.Sprite+',';
end;

//===== Object =====

const
  sROImgDir     = 'img';
  sRONPCDir     = sROImgDir+'\'+'npc'  +'\';
  sROMapDir     = sROImgDir+'\'+'maps' +'\';
  sROMobDir     = sROImgDir+'\'+'mobs' +'\';
  sROCardDir    = sROImgDir+'\'+'cards'+'\';
  sROItemDir    = sROImgDir+'\'+'items'+'\';
  sROSubItemDir = sROItemDir   +'sub'  +'\';
  sRODefExt     = '.png';
  sROPicExt     = '.jpg';

//----- Info -----

function RODB_GetInfo(aMode:tRODBMode; out acount,asize:integer):pointer;
begin
  case aMode of
    rosmItem: begin
      acount:=Length (ROItemData);
      asize :=SizeOf (ROItemData[0]);
      result:=pointer(ROItemData);
    end;
    rosmMonster: begin
      acount:=Length (ROMobData);
      asize :=SizeOf (ROMobData[0]);
      result:=pointer(ROMobData);
    end;
    rosmNPC: begin
      acount:=Length (RONPCData);
      asize :=SizeOf (RONPCData[0]);
      result:=pointer(RONPCData);
    end;
    rosmMap: begin
      acount:=Length (ROMapData);
      asize :=SizeOf (ROMapData[0]);
      result:=pointer(ROMapData);
    end;
    rosmEffect: begin
      acount:=Length (ROEffectData);
      asize :=SizeOf (ROEffectData[0]);
      result:=pointer(ROEffectData);
    end;
    rosmSkill: begin
      acount:=Length (ROSkillData);
      asize :=SizeOf (ROSkillData[0]);
      result:=pointer(ROSkillData);
    end;
    rosmQuest: begin
      acount:=Length (ROQuestData);
      asize :=SizeOf (ROQuestData[0]);
      result:=pointer(ROQuestData);
    end;
  else
    acount:=0;
    asize :=0;
    result:=nil;
  end;
end;

//----- Size -----

function RODB_GetSize(aMode:tRODBMode):integer;
begin
  case aMode of
    rosmItem   : result:=SizeOf(TROItemData);
    rosmMonster: result:=SizeOf(TROMobData);
    rosmNPC    : result:=SizeOf(TROBaseData);
    rosmMap    : result:=SizeOf(TROMapData);
    rosmEffect : result:=SizeOf(TROBaseData);
    rosmSkill  : result:=SizeOf(TROBaseData);
    rosmQuest  : result:=SizeOf(TROQuestData);
  else
    result:=0;
  end;
end;

//----- Count -----

function RODB_GetCount(aMode:tRODBMode):integer;
begin
  case aMode of
    rosmItem   : result:=Length(ROItemData);
    rosmMonster: result:=Length(ROMobData);
    rosmNPC    : result:=Length(RONPCData);
    rosmMap    : result:=Length(ROMapData);
    rosmEffect : result:=Length(ROEffectData);
    rosmSkill  : result:=Length(ROSkillData);
    rosmQuest  : result:=Length(ROQuestData);
  else
    result:=0;
  end;
end;

//----- ID num -----

function RODB_GetIdNum(idx:integer; aMode:tRODBMode):integer;
var
  lptr:pByte;
  lcnt,lsize:integer;
begin
  result:=0;
{
  case aMode of
    rosmItem   : if (idx>=0) and (idx<Length(ROItemData  )) then result:=ROItemData  [idx].id;
    rosmMonster: if (idx>=0) and (idx<Length(ROMobData   )) then result:=ROMobData   [idx].id;
    rosmNPC    : if (idx>=0) and (idx<Length(RONPCData   )) then result:=RONPCData   [idx].id;
    rosmEffect : if (idx>=0) and (idx<Length(ROEffectData)) then result:=ROEffectData[idx].id;
    rosmSkill  : if (idx>=0) and (idx<Length(ROSkillData )) then result:=ROSkillData [idx].id;
    rosmQuest  : if (idx>=0) and (idx<Length(ROQuestData )) then result:=ROQuestData [idx].id;
  end;
}
  if (idx>=0) then
  begin
    lptr:=RODB_GetInfo(aMode,lcnt,lsize);
    if (idx<lcnt) then
      result:=pROBaseData(lptr+idx*lsize)^.id;
  end;
end;

//----- Id -----

function RODB_GetId(idx:integer; aMode:tRODBMode):String;
var
  lptr:pByte;
  lcnt,lsize:integer;
begin
{
  result:='';
  case aMode of
    rosmItem   : if (idx>=0) and (idx<Length(ROItemData  )) then result:=IntToStr(ROItemData  [idx].id);
    rosmMonster: if (idx>=0) and (idx<Length(ROMobData   )) then result:=IntToStr(ROMobData   [idx].id);
    rosmNPC    : if (idx>=0) and (idx<Length(RONPCData   )) then result:=IntToStr(RONPCData   [idx].id);
    rosmMap    : if (idx>=0) and (idx<Length(ROMapData   )) then result:=ROMapData[idx].name;
    rosmEffect : if (idx>=0) and (idx<Length(ROEffectData)) then result:=IntToStr(ROEffectData[idx].id);
    rosmSkill  : if (idx>=0) and (idx<Length(ROSkillData )) then result:=IntToStr(ROSkillData [idx].id);
    rosmQuest  : if (idx>=0) and (idx<Length(ROQuestData )) then result:=IntToStr(ROQuestData [idx].id);
  end;
}
  if aMode=rosmMap then
    result:=StrCache[ROMapData[idx].name]
  else
  begin
    result:='';
    if (idx>=0) then
    begin
      lptr:=RODB_GetInfo(aMode,lcnt,lsize);
      if (idx<lcnt) then
        result:=IntToStr(pROBaseData(lptr+idx*lsize)^.id);
    end;
  end;
end;

function RODB_GetId(const aname:String; aMode:tRODBMode):String;
var
  i:integer;
begin
  if aname='' then
  begin
    result:='';
    exit;
  end;
  if aMode=rosmMap then
  begin
    i:=Pos(',',aname);
    result:=aname;
    if i>0 then
      SetLength(result,i-1);
  end
  else
  begin
    result:=RODB_GetId(RODB_GetIndex(aname,aMode),aMode);
  end;
end;

//----- Name -----

function RODB_GetName(idx:integer; aMode:tRODBMode):String;
var
  lptr:pByte;
  lcnt,lsize:integer;
begin
  result:='';
{
  case aMode of
    rosmItem   : if (idx>=0) and (idx<Length(ROItemData  )) then result:=ROItemData  [idx].name;
    rosmMonster: if (idx>=0) and (idx<Length(ROMobData   )) then result:=ROMobData   [idx].name;
    rosmNPC    : if (idx>=0) and (idx<Length(RONPCData   )) then result:=RONPCData   [idx].name;
    rosmMap    : if (idx>=0) and (idx<Length(ROMapData   )) then result:=ROMapData   [idx].name;
    rosmEffect : if (idx>=0) and (idx<Length(ROEffectData)) then result:=ROEffectData[idx].name;
    rosmSkill  : if (idx>=0) and (idx<Length(ROSkillData )) then result:=ROSkillData [idx].name;
    rosmQuest  : if (idx>=0) and (idx<Length(ROQuestData )) then result:=ROQuestData [idx].name;
  end;
}
  if (idx>=0) then
  begin
    lptr:=RODB_GetInfo(aMode,lcnt,lsize);
    if (idx<lcnt) then
      result:=StrCache[pROBaseData(lptr+idx*lsize)^.name];
  end;
end;

//----- Text -----

function RODB_GetText(idx:integer; aMode:tRODBMode):String;
begin
  result:='';
  case aMode of
    rosmItem:
      if (idx>=0) and (idx<Length(ROItemData)) then
      begin
        if ROItemData[idx].local='' then
          result:=ROItemData[idx].descr
        else
          result:=ROItemData[idx].local;

        if ROItemData[idx].Slots>0 then
          result:=result+'['+AnsiChar(ROItemData[idx].Slots+ORD('0'))+']';

      end;
    rosmMonster:
      if (idx>=0) and (idx<Length(ROMobData)) then
      begin
        if ROMobData[idx].local='' then
          result:=ROMobData[idx].descr
        else
          result:=ROMobData[idx].local;
      end;
{
    rosmNPC   : if (idx>=0) and (idx<Length(RONPCData   )) then result:=RONPCData   [idx].name;
    rosmMap   : if (idx>=0) and (idx<Length(ROMapData   )) then result:=ROMapData   [idx].name;
    rosmEffect: if (idx>=0) and (idx<Length(ROEffectData)) then result:=ROEffectData[idx].name;
    rosmSkill : if (idx>=0) and (idx<Length(ROSkillData )) then result:=ROSkillData [idx].name;
    rosmQuest : if (idx>=0) and (idx<Length(ROQuestData )) then result:=ROQuestData [idx].name;
}
  else
    result:=RODB_GetName(idx,aMode);
  end;
end;

function RODB_GetText(idx:integer; alt:integer; aMode:tRODBMode):String;
begin
  case aMode of
    rosmItem: begin
      if (idx>=0) and (idx<Length(ROItemData)) then
      begin
        if alt>=0 then
          result:=ROItemData[idx].descr
        else
          result:=ROItemData[idx].local;

        if ROItemData[idx].Slots>0 then
          result:=result+'['+AnsiChar(ROItemData[idx].Slots+ORD('0'))+']';
      end
      else
        result:='';
    end;

    rosmMonster: begin
      if (idx>=0) and (idx<Length(ROMobData)) then
      begin
        if alt=0 then
          result:=ROMobData[idx].descr
        else if alt>0 then
          result:=ROMobData[idx].kro
        else
          result:=ROMobData[idx].local;
      end
      else
        result:='';
    end;

  else
    result:=RODB_GetText(idx,aMode);
  end;
end;

//----- Title -----

function RODB_GetTitle(idx:integer; aMode:tRODBMode):String;
var
  ls:String;
  lls:array [0..127] of AnsiChar;
  pc:PAnsiChar;
  lid:integer;
//  lptr:pByte;
//  lcnt,lsize:integer;
begin
  result:='';

  lid:=-1;
  ls :='';

  case aMode of
    rosmItem:
      if (idx>=0) and (idx<Length(ROItemData)) then
      begin
        if ROItemData[idx].local='' then
          ls:=ROItemData[idx].descr
        else
          ls:=ROItemData[idx].local;
        lid :=ROItemData[idx].id;
      end;

    rosmMonster:
      if (idx>=0) and (idx<Length(ROMobData)) then
      begin
        if ROMobData[idx].local='' then
          ls:=ROMobData[idx].descr
        else
          ls:=ROMobData[idx].local;
        lid :=ROMobData[idx].id;
      end;

    rosmNPC:
      if (idx>=0) and (idx<Length(RONPCData)) then
      begin
        lid:=RONPCData[idx].id;
        ls :=StrCache[RONPCData[idx].name];
      end;

    rosmMap: begin
      if (idx>=0) and (idx<Length(ROMapData)) then
      begin
        if ROMapData[idx].local='' then
          ls:=ROMapData[idx].descr
        else
          ls:=ROMapData[idx].local;
        result:=StrCache[ROMapData[idx].name]+', '+ls;
      end;
      exit;
    end;

    rosmSkill: 
      if (idx>=0) and (idx<Length(ROSkillData)) then
      begin
        lid:=ROSkillData[idx].id;
        ls :=StrCache[ROSkillData[idx].name];
      end;

    rosmQuest:
      if (idx>=0) and (idx<Length(ROQuestData)) then
      begin
        lid:=ROQuestData[idx].id;
        ls :=StrCache[ROQuestData[idx].name];
      end;
  else
    result:=RODB_GetText(idx, aMode);
    exit;
  end;

  if lid>=0 then
  begin
    lls[0]:='[';
    pc:=StrEnd(common.IntToStr(@lls[1],lid));
    pc^:=']'; inc(pc);
    pc^:=' '; inc(pc);
    pc:=StrCopyE(pc,pointer(ls));
  
    if aMode=rosmItem then
    begin
      if ROItemData[idx].Slots>0 then
      begin
        pc^:='['; inc(pc);
        pc^:=AnsiChar(ROItemData[idx].Slots+ORD('0')); inc(pc);
        pc^:=']'; inc(pc);
        pc^:=#0;
      end;
    end;
    result:=PAnsiChar(@lls);
  end;
end;

//----- Base -----

function GetMobBase(idx:integer):integer;
var
  lname:AnsiString;
  i:integer;
  lhash:cardinal;
  changed:boolean;
begin
  result:=idx;
  if (idx<0) or (idx>=Length(ROMobData)) then exit;

  // Lets think what base data was before

  // Search for name without prefixes
  lname:=StrCache[ROMobData[idx].name];
  changed:=false;
  if (Length(lname)>3) and (lname[3]='_') then
  begin
    // C1-Swift, C2-Solid, C3-Ringleader, C4-Furious, C5-Elusive
    if ((lname[1]='C') and (lname[2] in ['1'..'5']))
    or ((lname[1]='M') and (lname[2]='D')) then
    begin
      i:=1+3;
      changed:=true;
    end
  end
  else
  if (Length(lname)>2) and (lname[2]='_') then
  begin
    i:=1+2;
    changed:=true;
  end
  else
  if (Length(lname)>6) and (lname[6]='_') then
  begin
    if (Pos('EVENT',lname)=1) then
    begin
      i:=1+6;
      changed:=true;
    end;
  end;

  if changed then
    lname:=copy(lname,i,1000);

  // search aename without ending "_"
  if lname[Length(lname)]='_' then
  begin
    SetLength(lname,Length(lname)-1);
    changed:=true;
  end;

  if changed then
  begin
    // Double 1-char prefix
    if (Length(lname)>2) and (lname[2]='_') then
      lname:=Copy(lname,3,1000);

    lhash:=common.Hash(SysUtils.UpperCase(lname));
    for i:=0 to idx-1 do
      if lhash=ROMobData[i].hash then
        exit(i);
  end;

  // Search the same kRO/iRO names
  lname:=ROMobData[idx].descr;
  for i:=0 to idx-1 do
  begin
    if (CompareStr(lname,ROMobData[i].descr)=0) or
       (CompareStr(lname,ROMobData[i].kro  )=0) then
      exit(i);
  end;

end;

function GetItemBase(idx:integer):integer;
var
  lname:AnsiString;
  i,st:integer;
begin
  result:=idx;

  if (idx<=0) or (idx>=Length(ROItemData)) then exit;

  lname:=ROItemData[idx].descr;
  // search back for another-slotted weapon-equip
  if idx>3 then
    st:=idx-4
  else
    st:=0;
  for i:=st to idx-1 do
  begin
    if lname=ROItemData[i].descr then
      exit(i);
  end;
  // if not found, search just alternative item
  lname:=sysutils.LowerCase(lname);
  for i:=0 to st-1 do
  begin
    if CompareStr(lname,sysutils.LowerCase(ROItemData[i].descr))=0 then
      exit(i);
  end;
end;

function RODB_GetBase(idx:integer; aMode:tRODBMode):integer;
begin
  case aMode of
    rosmItem   : result:=GetItemBase(idx);
    rosmMonster: result:=GetMobBase (idx);
  else
    result:=idx;
  end;
end;

//----- Sprite  -----

function GetItemSprite(idx:integer):String;
begin
  result:='';

  case ROItemData[idx].ItemType of
    0 : result:=sprHealing;
    2 : result:=sprUsable;
    3 : result:=sprEtc;
    6 : result:=sprCard;
    7 : result:=sprPetEgg;
    8 : result:=sprPetEquip;
    11: result:=sprSkill;
    18: result:=sprDelayed;
    4 : begin
      if (ROItemData[idx].Loc and (EQP_HEAD or EQP_COSTUME_HEAD))<>0 then result:=sprHead
      else
        case ROItemData[idx].Loc of
          EQP_COSTUME_GARMENT,
          EQP_GARMENT: result:=sprGarment;
          EQP_ACC_L,
          EQP_ACC_R  : result:=sprAccess;
          EQP_HAND_L : result:=sprShield; // separate?
          EQP_ARMOR  : result:=sprArmor;
          EQP_SHOES  : result:=sprShoes;
        end;
    end;
    5 : begin if ROItemData[idx].View<Length(WeaponSprites) then
      result:=WeaponSprites[ROItemData[idx].View];
    end;
    10: if ROItemData[idx].View<Length(AmmoSprites) then
      result:=AmmoSprites[ROItemData[idx].View];
    12: begin
      case ROItemData[idx].Loc of
        EQP_SHADOW_WEAPON: begin
          if ROItemData[idx].View<Length(WeaponSprites) then
            result:=WeaponSprites[ROItemData[idx].View];
        end;
        EQP_SHADOW_ARMOR : result:=sprArmor;
        EQP_SHADOW_SHIELD: result:=sprShield;
        EQP_SHADOW_SHOES : result:=sprShoes;
        EQP_SHADOW_ACC_R,
        EQP_SHADOW_ACC_L : result:=sprAccess;
      end;
    end;
  end;
  if result='' then result:=sprUnknown;
end;

function MakeFileName(buf:PAnsiChar; dir:PAnsiChar; idx:integer; ext:PAnsiChar):PAnsiChar; inline;
begin
  result:=buf;
  common.StrCopy(
    common.StrCopyE(
      common.StrCopyE(buf,dir),
      StrCache[idx]),
    ext);
end;

procedure CheckCardSprite(idx:integer);
var
  lname:array [0..300] of AnsiChar;
begin
  MakeFileName(lname,sROCardDir,ROItemData[idx].Sprite,sROPicExt);
  if not FileExists(PAnsiChar(@lname)) then
    ROItemData[idx].Sprite:=-ROItemData[idx].Sprite;
end;

procedure CheckItemSprite(idx:integer);
var
  lname:array [0..300] of AnsiChar;
  i:integer;
  cond:boolean;
begin
  cond:=ROItemData[idx].Sprite=0;
  if not cond then
  begin
    // little profit (11kk operations, 2kk memory)
    MakeFileName(lname,sROItemDir,ROItemData[idx].Sprite,sROPicExt);
    cond:=not FileExists(PAnsiChar(@lname));
  end;
  if cond then
  begin
    i:=RODB_GetBase(idx,rosmItem);
    if i=idx then
      ROItemData[idx].Sprite:=-ROItemData[idx].Sprite
    else
      ROItemData[idx].Sprite:=ROItemData[i].Sprite;
  end;
end;

function RODB_GetSprite(idx:integer; aMode:tRODBMode):String;
var
  lname:array [0..300] of AnsiChar;
  i:integer;
begin
  result:='';
  case aMode of
    rosmItem: begin
      if (idx>=0) and (idx<Length(ROItemData)) then
      begin
        if ROItemData[idx].Sprite=0 then
          CheckItemSprite(idx);

        if ROItemData[idx].Sprite>0 then
        begin
          if ItemIsCard(idx) then
            result:=sROCardDir
          else
            result:=sROItemDir;
          // main profit (27kk operations,835kk memory)
          MakeFileName(lname,pointer(result),ROItemData[idx].Sprite,sROPicExt);
          result:=PAnsiChar(@lname);
        end
        else
        begin
//          result:=sROItemDir+GetItemSprite(idx)+sROPicExt;
          // little profit (2k operations, 16,5kk memory)
//          MakeFileName(lname,sROItemDir,{ROItemData[idx].Sprite},sROPicExt);
          common.StrCopy(
            common.StrCopyE(
              common.StrCopyE(lname,sROItemDir),
              pointer(GetItemSprite(idx))),
            sROPicExt);

          result:=PAnsiChar(@lname);
        end;
      end;
    end;

    rosmMonster: begin
      if (idx>=0) and (idx<Length(ROMobData)) then
      begin
        if ROMobData[idx].sprite=0 then
          ROMobData[idx].sprite:=ROMobData[idx].name;
        
        if ROMobData[idx].sprite>0 then
        begin
          MakeFileName(lname,sROMobDir,ROMobData[idx].sprite,sRODefExt);
          result:=PAnsiChar(@lname);
        end;

        if not FileExists(result) then
        begin
          i:=RODB_GetBase(idx,rosmMonster);
          if (i<>idx) then
          begin
            if (ROMobData[i].sprite>0) then
              ROMobData[idx].sprite:=ROMobData[i].sprite
            else
              ROMobData[idx].sprite:=ROMobData[i].name;

            MakeFileName(lname,sROMobDir,ROMobData[idx].sprite,sRODefExt);
            result:=PAnsiChar(@lname);
          end
//          else
//            result:='';
        end;
      end;
    end;

    rosmNPC:
      if (idx>0) and (idx<Length(RONPCData)) then
      begin
        MakeFileName(lname,sRONPCDir,RONPCData[idx].name,sRODefExt);
        result:=PAnsiChar(@lname);
      end
      else
        result:=sRONPCDir+'empty'+sRODefExt;

    rosmMap:
      if (idx>=0) and (idx<Length(ROMapData)) then
      begin
        if ROMapData[idx].sprite>0 then idx:=ROMapData[idx].sprite;
        MakeFileName(lname,sROMapDir,ROMapData[idx].name,sRODefExt);
        result:=PAnsiChar(@lname);
      end;

  else
  end;
end;

function RODB_GetSprite(idx,num:integer; aMode:tRODBMode):String;
var
  lname:array [0..300] of AnsiChar;
begin
  if (aMode<>rosmItem) or (num=0) then
    result:=RODB_GetSprite(idx,aMode)
  else
  begin
    result:='';
    if (idx>=0) and (idx<Length(ROItemData)) then
    begin
      // if uses GetItemSprite(idx), we don't need ItemIsCard() branch
      // even if set what ItemType = card is not real cards only
      if ItemIsCard(idx) then
        result:=sROSubItemDir+sprCard+sRODefExt
      else if ROItemData[idx].Sprite>=0 then
      begin
        MakeFileName(lname,sROSubItemDir,ROItemData[idx].Sprite,sRODefExt);
        result:=PAnsiChar(@lname);
      end
      // !! sprite not set
      else
        result:=''; // sROSubItemDir+GetItemSprite(idx)+sRODefExt;
    end;
  end;
end;

function RODB_GetSprite(const aname:String; aMode:tRODBMode):String;
var
  i:integer;
begin
  result:='';
  if aname='' then exit;
  case aMode of
    rosmItem   : result:=sROItemDir+aname+sRODefExt;
    rosmMonster: result:=sROMobDir +aname+sRODefExt;
    rosmNPC    : result:=sRONPCDir +aname+sRODefExt;
    rosmMap    : begin
      i:=Pos(',',aname);
      if i=0 then
        result:=sROMapDir+aname+sRODefExt
      else
        result:=sROMapDir+Copy(aname,1,i-1)+sRODefExt;
    end;
  else
  end;
end;

//----- Index -----

//0 if bad number, else - number (possible in [])
function CheckForID(const aname:AnsiString):integer;
var
  i:integer;
begin
  result:=0;
  // check name type
  if aname[1]='[' then
    i:=2
  else
    i:=1;
  while i<=Length(aname) do
  begin
    if aname[i] in ['0'..'9'] then
      result:=result*10+ord(aname[i])-ord('0')
    else
    begin
      if aname[i]<>']' then
        result:=0;
      break;
    end;
    inc(i);
  end;
end;

function GetNPCIndex(const aname:AnsiString):integer;
var
  lhash:cardinal;
  i,lid:integer;
begin
  if aname[1]<>'-' then // force -1
  begin
    lid:=CheckForID(aname);
    if lid>0 then
    begin
      result:=RODB_GetIndex(lid,rosmNPC);
      exit;
    end;
    lhash:=common.Hash(sysutils.UpperCase(aname));
    for i:=0 to High(RONPCData) do
    begin
      if lhash=RONPCData[i].hash then exit(i);
    end;
  end;
  result:=High(RONPCData);
end;

function GetEffectIndex(const aname:AnsiString):integer;
var
  i,lid:integer;
begin
  lid:=CheckForID(aname);

  if lid>0 then
  begin
    result:=RODB_GetIndex(lid,rosmEffect);
    exit;
  end;
  for i:=0 to High(ROEffectData) do
  begin
    if (Pos(aname,ROEffectData[i].name)>0) then exit(i);
  end;
  result:=-1;
end;

function GetSkillIndex(const aname:AnsiString):integer;
var
  lhash:cardinal;
  i,lid:integer;
begin
  lid:=CheckForID(aname);
  if lid>0 then
  begin
    result:=RODB_GetIndex(lid,rosmSkill);
    exit;
  end;
  lhash:=common.Hash(sysutils.UpperCase(aname));
  for i:=0 to High(ROSkillData) do
  begin
    if lhash=ROSkillData[i].hash then exit(i);
  end;
  result:=-1;
end;

function GetQuestIndex(const aname:AnsiString):integer;
var
  lhash:cardinal;
  i,lid:integer;
begin
  lid:=CheckForID(aname);
  if lid>0 then
  begin
    result:=RODB_GetIndex(lid,rosmQuest);
    exit;
  end;
  lhash:=common.Hash(AnsiUpperCase(aname));
  for i:=0 to High(ROQuestData) do
  begin
    if lhash=ROQuestData[i].hash then exit(i);
  end;
  result:=-1;
end;

function GetMapIndex(const aname:AnsiString):integer;
var
  ls:AnsiString;
  i,d:integer;
begin
  result:=-1;
  ls:=aname;
  i:=Pos(',',ls);
  if i>0 then
    SetLength(ls,i-1);

  i:=High(ROMapData);
  while i>=0 do
  begin
      d:=AnsiCompareStr(StrCache[ROMapData[i].name],ls);
    if d{<}=0 then break;
    dec(i);
  end;
  if d=0 then result:=i;
end;

function GetMobIndex(const aname:AnsiString):integer;
var
  lname:AnsiString;
  lhash:cardinal;
  i,lid:integer;
begin
  lid:=CheckForID(aname);
  if lid>0 then
  begin
    result:=RODB_GetIndex(lid,rosmMonster);
    exit;
  end;
  lname:=AnsiUpperCase(aname);
  lhash:=common.Hash(lname);
  for i:=0 to High(ROMobData) do
  begin
    with ROMobData[i] do
      if (lhash=hash) or
         (CompareStr(lname,sysutils.UpperCase(descr))=0) or
         (CompareStr(lname,sysutils.UpperCase(kro  ))=0) or
         (CompareStr(lname,AnsiUpperCase(local))=0) then exit(i);
  end;
  result:=-1;
end;

function GetItemIndex(const aname:AnsiString):integer;
var
  lname:AnsiString;
  lhash:cardinal;
  i,lid:integer;
begin
  lid:=CheckForID(aname);
  if lid>0 then
  begin
    result:=RODB_GetIndex(lid,rosmItem);
    exit;
  end;
  lname:=AnsiUpperCase(aname);
  lhash:=common.Hash(lname);
  for i:=0 to High(ROItemData) do
  begin
    with ROItemData[i] do
      if (lhash=hash) or
         (CompareStr(lname,sysutils.UpperCase(descr))=0) or
         (CompareStr(lname,AnsiUpperCase(local))=0) then exit(i);
  end;
  result:=-1;
end;

function RODB_GetIndex(const aname:String; aMode:tRODBMode):integer;
begin
  result:=-1;
  if aname='' then exit;
  case aMode of
    rosmItem   : result:=GetItemIndex  (aname);
    rosmMonster: result:=GetMobIndex   (aname);
    rosmNPC    : result:=GetNPCIndex   (aname);
    rosmMap    : result:=GetMapIndex   (aname);
    rosmEffect : result:=GetEffectIndex(aname);
    rosmSkill  : result:=GetSkillIndex (aname);
    rosmQuest  : result:=GetQuestIndex (aname);
  else
  end;
end;

function RODB_GetIndex(anid:integer; aMode:tRODBMode):integer;
var
  i:integer;
  L,R,dir:integer;
  lptr:pByte;
  lsize:integer;
begin
  result:=-1;

  case aMode of
    rosmItem,rosmMonster,rosmNPC,
    rosmEffect,rosmSkill,rosmQuest: begin
      lptr:=RODB_GetInfo(aMode,R,lsize);
{
      // linear, universal
      i:=0;
      while i<R do
      begin
        if anid<=pROBaseData(lptr)^.id then break;
        inc(lptr,lsize);
        inc(i);
      end;
      if anid=pROBaseData(lptr)^.id then result:=i;
}
      // binary, universal
      L:=0;
      dec(R);
      while L<=R do
      begin
        i:=L+(R-L) div 2;
        dir:=pROBaseData(lptr+i*lsize)^.id-anid;
        if dir<0 then
          L:=i+1
        else if dir>0 then
          R:=i-1
        else
          exit(i);
      end;
    end;
  else
  end;
end;

//----- Table support -----

function RODB_GetFieldCount(aMode:tRODBMode):integer;
begin
  case aMode of
    rosmItem   : result:=1+2+1;
    rosmMonster: result:=1+3+1;
    rosmNPC    : result:=1+1;
    rosmMap    : result:=1+1+1;
    rosmEffect : result:=1+1;
    rosmSkill  : result:=1+1{2};
    rosmQuest  : result:=1+1;
  else
    result:=0;
  end;
end;

function RODB_GetField(idx:integer; fnum:integer; aMode:tRODBMode):String;
var
  resTitle:boolean;
begin
  if (fnum>=0) and (fnum<RODB_GetFieldCount(aMode)) then
  begin
    resTitle:=(idx<0) or (idx>=RODB_GetCount(aMode));
    case aMode of
      rosmItem: begin
        case fnum of
          0: if resTitle then result:='ID'    else result:=IntToStr(ROItemData[idx].id);
          1: if resTitle then result:=sAEName else result:=StrCache[ROItemData[idx].name];
          2: if resTitle then result:=sName   else result:=ROItemData[idx].descr;
          3: if resTitle then result:=sLocal  else result:=ROItemData[idx].local;
        end;
      end;

      rosmMonster: begin
        case fnum of
          0: if resTitle then result:='ID'    else result:=IntToStr(ROMobData[idx].id);
          1: if resTitle then result:=sAEName else result:=StrCache[ROMobData[idx].name];
          2: if resTitle then result:='kRO'   else result:=ROMobData[idx].kro;
          3: if resTitle then result:='iRO'   else result:=ROMobData[idx].descr;
          4: if resTitle then result:=sLocal  else result:=ROMobData[idx].local;
        end;
      end;

      rosmNPC: begin
        case fnum of
          0: if resTitle then result:='ID'  else result:=IntToStr(RONPCData[idx].id);
          1: if resTitle then result:=sName else result:=StrCache[RONPCData[idx].name];
        end;
      end;

      rosmMap: begin
        case fnum of
          0: if resTitle then result:=sName  else result:=StrCache[ROMapData[idx].name];
          1: if resTitle then result:=sDescr else result:=ROMapData[idx].descr;
          2: if resTitle then result:=sLocal else result:=ROMapData[idx].local;
        end;
      end;

      rosmEffect: begin
        case fnum of
          0: if resTitle then result:=sNumber else result:=IntToStr(ROEffectData[idx].id);
          1: if resTitle then result:=sDescr  else result:=ROEffectData[idx].descr;
        end;
      end;

      rosmSkill: begin
        case fnum of
          0: if resTitle then result:='ID'   else result:=IntToStr(ROSkillData[idx].id);
          1: if resTitle then result:=sName  else result:=StrCache[ROSkillData[idx].name];
//          2: if resTitle then result:=sDescr else result:=ROSkillData[idx].descr;
        end;
      end;

      rosmQuest: begin
        case fnum of
          0: if resTitle then result:='ID'  else result:=IntToStr(ROQuestData[idx].id);
          1: if resTitle then result:=sName else result:=ROQuestData[idx].descr;
        end;
      end;

    else
      result:='';
    end;
  end;
end;

//===== Misc =====

function ModeToText(aMode:tRODBMode):String;
begin
  case aMode of
    rosmItem   : result:=sItem;
    rosmMonster: result:=sMonster;
    rosmNPC    : result:='NPC';
    rosmMap    : result:=sMap;
    rosmEffect : result:=sEffect;
    rosmSkill  : result:=sSkill;
    rosmQuest  : result:=sQuest;
  else
    result:='';
  end;
end;

function GetNextMapMonster(amap:integer; var idx:integer):integer;
begin
  inc(idx);
  while idx<Length(ROMobRespData) do
  begin
    if ROMobRespData[idx].name=amap then
    begin
      result:=RODB_GetIndex(ROMobRespData[idx].id,rosmMonster);
      exit;
    end;
    inc(idx);
  end;
  result:=-1;
end;

function GetNextMonsterMap(anid:integer; var idx:integer):integer;
begin
  inc(idx);
  while idx<Length(ROMobRespData) do
  begin
    if ROMobRespData[idx].id=anid then
    begin
      result:=ROMobRespData[idx].name;
      exit;
    end;
    inc(idx);
  end;
  result:=-1;
end;

function GetNextDropMonster(itemid:integer; curmob:integer; out rate:integer):integer;
var
  j:integer;
begin
  result:=curmob+1;
  while result<Length(ROMobData) do
  begin
    with ROMobData[result] do
    begin
      for j:=0 to ROMaxMVPDrops-1 do
        if MVPDrop[j].id=itemid then
        begin
          rate:=MVPDrop[j].rate;
          exit;
        end;
      for j:=0 to ROMaxMobDrops-1 do
        if Drop[j].id=itemid then
        begin
          rate:=Drop[j].rate;
          exit;
        end;
      if Card.id=itemid then
      begin
        rate:=Card.rate;
        exit;
      end;
    end;
    inc(result);
  end;
  result:=-1;
end;

function KoreanToAnsi(astr:PAnsiChar):AnsiString;
var
  ls:array [0..127] of AnsiChar;
  i,j:integer;
begin
  j:=0;
  for i:=0 to StrLen(astr)-1 do
  begin
    if ord(astr[i])<128 then
    begin
      ls[j]:=astr[i];
      inc(j);
    end
    else
    begin
      ls[j  ]:=HexDigitChr[ord(astr[i]) shr  4];
      ls[j+1]:=HexDigitChr[ord(astr[i]) and $F];
      inc(j,2);
    end;
  end;
  ls[j]:=#0;
  result:=ls;
end;

function KoreanToAnsi(astr,buf:PAnsiChar):PAnsiChar;
var
  j:integer;
begin
  j:=0;
  if astr<>nil then
  begin
    while astr^<>#0 do
    begin
      if ord(astr^)<128 then
      begin
        buf[j]:=astr^;
        inc(j);
      end
      else
      begin
        buf[j  ]:=HexDigitChr[ord(astr^) shr  4];
        buf[j+1]:=HexDigitChr[ord(astr^) and $F];
        inc(j,2);
      end;
      inc(astr);
    end;
  end;
  buf[j]:=#0;
  result:=buf;
end;

function ItemIsCard(idx:integer):boolean;
begin
  result:=(idx>=0) and (ROItemData[idx].ItemType=6) and
          (Pos('_Card', StrCache[ROItemData[idx].name])>0);
end;

//===== Filter =====

type
  tfilter = array of integer;

//----- Monster filter -----

function FilterMonster(aptr:pointer; aval:integer):integer;
var
  i:integer;
begin
  result:=0;
  if aval<0 then
  begin
    TStringList(aptr).Clear;
    TStringList(aptr).Add(sAll);
    TStringList(aptr).Add(sBoss);
    TStringList(aptr).Add(sMMMVP);
    for i:=0 to High(ROSizes) do
      TStringList(aptr).Add(sSize+': '+ROSizes[i]);
    for i:=0 to High(RORaces) do
      TStringList(aptr).Add(sRace+': '+RORaces[i]);
    for i:=0 to High(ROElements) do
      TStringList(aptr).Add(sElement+': '+ROElements[i]);
  end
  else
  begin
    // Bosses
    if aval=1 then
    begin
      for i:=0 to High(ROMobData) do
      begin
        //!! temporal mask
        if (ROMobData[i].Mode and
           (MD_KNOCKBACK_IMMUNE or MD_DETECTOR or MD_STATUS_IMMUNE))=
           (MD_KNOCKBACK_IMMUNE or MD_DETECTOR or MD_STATUS_IMMUNE) then
        begin
          tfilter(aptr)[result]:=i;
          inc(result);
        end;
      end;
      exit;
    end
    // MVP
    else if aval=2 then
    begin
      for i:=0 to High(ROMobData) do
      begin
        if (ROMobData[i].Mode and MD_MVP)<>0 then
        begin
          tfilter(aptr)[result]:=i;
          inc(result);
        end;
      end;
      exit;
    end;
    dec(aval,3);
    // Scale
    if aval<Length(ROSizes) then
    begin
      for i:=0 to High(ROMobData) do
      begin
        if ROMobData[i].Scale=aval then
        begin
          tfilter(aptr)[result]:=i;
          inc(result);
        end;
      end;
      exit;
    end;
    dec(aval,Length(ROSizes));
    // Race
    if aval<Length(RORaces) then
    begin
      for i:=0 to High(ROMobData) do
      begin
        if ROMobData[i].Race=aval then
        begin
          tfilter(aptr)[result]:=i;
          inc(result);
        end;
      end;
      exit;
    end;
    dec(aval,Length(RORaces));
    // Element
//    if aval<Length(ROElements) then
    begin
      for i:=0 to High(ROMobData) do
      begin
        if (ROMobData[i].Element mod 20)=aval then
        begin
          tfilter(aptr)[result]:=i;
          inc(result);
        end;
      end;
    end;
  end;
end;

//----- Item Filter -----

function FilterItem(aptr:pointer; aval:integer):integer;
var
  i,lcat:integer;
begin
  result:=0;
  // IN : TStringList (cb.Items), -1 means fill
  // OUT: no matter
  if aval<0 then
  begin
    TStringList(aptr).Clear;
    TStringList(aptr).Add(sAll);
    TStringList(aptr).Add(sItemEquipment);
    TStringList(aptr).Add(sItemEquipment+': '+sHeadgear);
    TStringList(aptr).Add(sItemEquipment+': '+sLocArmor);
    TStringList(aptr).Add(sItemEquipment+': '+sLocGarment);
    TStringList(aptr).Add(sItemEquipment+': '+sLocFootgear);
    TStringList(aptr).Add(sItemEquipment+': '+sLocShield);
    TStringList(aptr).Add(sItemEquipment+': '+sAccess);
    TStringList(aptr).Add(sItemWeapon);
    for i:=1 to ROWeaponMaxType-1 do
      TStringList(aptr).Add(sItemWeapon+': '+ROWeaponView[i]);
    TStringList(aptr).Add(sItemCard);
    TStringList(aptr).Add(sItemPetEgg);
    TStringList(aptr).Add(sItemPetEquip);
    TStringList(aptr).Add(sItemAmmo);
    TStringList(aptr).Add(sItemShadowEquip);
    TStringList(aptr).Add(sItemUsable); // +sItemHealing
    TStringList(aptr).Add(sItemEtc);    // +sItemSkillItem+sItemDelayed
  end
  else
  begin
    // Equip
    if aval<8 then
    begin
      case aval of
        2: lcat:=EQP_HEAD or EQP_COSTUME_HEAD;
        3: lcat:=EQP_ARMOR;
        4: lcat:=EQP_GARMENT or EQP_COSTUME_GARMENT;
        5: lcat:=EQP_SHOES;
        6: lcat:=EQP_HAND_L;
        7: lcat:=EQP_ACC_RL;
      else
        lcat:=$FFFF;
      end;
      for i:=0 to High(ROItemData) do
      begin
        if ROItemData[i].ItemType=4 then
        begin
          if (ROItemData[i].Loc and lcat)<>0 then
          begin
            tfilter(aptr)[result]:=i;
            inc(result);
          end;
        end;
      end;
      exit;
    end;
    dec(aval,8);
    // weapon
    if aval<ROWeaponMaxType then
    begin
      for i:=0 to High(ROItemData) do
      begin
        if ROItemData[i].ItemType=5 then
        begin
          if (aval=0) or (ROItemData[i].View=aval) then
          begin
            tfilter(aptr)[result]:=i;
            inc(result);
          end;
        end;
      end;
      exit;
    end;
    dec(aval,ROWeaponMaxType);
    case aval of
      0: lcat:=6;  // card
      1: lcat:=7;  // petegg
      2: lcat:=8;  // pet equip
      3: lcat:=10; // ammo
      4: lcat:=12; // shadow
      5: begin     // healing+usable
        for i:=0 to High(ROItemData) do
        begin
          if ROItemData[i].ItemType in [0,2] then
          begin
            tfilter(aptr)[result]:=i;
            inc(result);
          end;
        end;
        exit;
      end;
      6: begin     // etc, skill, delayed
        for i:=0 to High(ROItemData) do
        begin
          if ROItemData[i].ItemType in [3,11,18] then
          begin
            tfilter(aptr)[result]:=i;
            inc(result);
          end;
        end;
        exit;
      end;
    end;

    for i:=0 to High(ROItemData) do
    begin
      if ROItemData[i].ItemType=lcat then
      begin
        tfilter(aptr)[result]:=i;
        inc(result);
      end;
    end;
  end
end;


initialization
  StrCache.Init;
  // for use "0" as "not checked" state for sprites
  StrCache.Append(nil);

finalization
  StrCache.Clear;

end.
