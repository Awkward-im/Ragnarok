unit rodata;

interface

//===== constants =====

resourcestring
  // size
  sSmall     = 'Small';
  sMedium    = 'Medium';
  sLarge     = 'Large';
  // element
  sNeutral   = 'Neutral';
  sWater     = 'Water';
  sEarth     = 'Earth';
  sFire      = 'Fire';
  sWind      = 'Wind';
  sPoison    = 'Poison';
  sHoly      = 'Holy';
  sShadow    = 'Shadow';
  sGhost     = 'Ghost';
  sUndead    = 'Undead';
  // race
  sFormless  = 'Formless';
  sBrute     = 'Brute';
  sPlant     = 'Plant';
  sInsect    = 'Insect';
  sFish      = 'Fish';
  sDemon     = 'Demon';
  sDemiHuman = 'Demi-Human';
  sAngel     = 'Angel';
  sDragon    = 'Dragon';
  sPlayer    = 'Player';
  // range
  sMelee     = '(melee)';
  sRanged    = '(ranged)';
  // gender
  sFemale    = 'Female';
  sMale      = 'Male';
  sBoth      = 'Both';
  // common
  sUnknown     = 'Unknown';
  sUnused      = 'Unused';
  // item type
  sItemHealing     = 'Healing';
  sItemUsable      = 'Usable';
  sItemEtc         = 'Misc';
  sItemEquipment   = 'Equipment';
  sItemWeapon      = 'Weapon';
  sItemCard        = 'Card';
  sItemPetegg      = 'Pet egg';
  sItemPetEquip    = 'Pet equipment';
  sItemAmmo        = 'Ammo';
  sItemSkillItem   = 'Skill item';
  sItemShadowEquip = 'Shadow equipment';
  sItemDelayed     = 'Delayed';
  // Weapon views
  sWeaponBareFist         = 'bare fist';
  sWeaponDaggers          = 'Daggers';
  sWeaponOneHandedSwords  = 'One-handed swords';
  sWeaponTwoHandedSwords  = 'Two-handed swords';
  sWeaponOneHandedSpears  = 'One-handed spears';
  sWeaponTwoHandedSpears  = 'Two-handed spears';
  sWeaponOneHandedAxes    = 'One-handed axes';
  sWeaponTwoHandedAxes    = 'Two-handed axes';
  sWeaponMaces            = 'Maces';
  sWeaponStaves           = 'Staves';
  sWeaponBows             = 'Bows';
  sWeaponKnuckles         = 'Knuckles';
  sWeaponInstruments      = 'Musical Instruments';
  sWeaponWhips            = 'Whips';
  sWeaponBooks            = 'Books';
  sWeaponKatars           = 'Katars';
  sWeaponRevolvers        = 'Revolvers';
  sWeaponRifles           = 'Rifles';
  sWeaponGatlingGuns      = 'Gatling guns';
  sWeaponShotguns         = 'Shotguns';
  sWeaponGrenadeLaunchers = 'Grenade launchers';
  sWeaponFuumaShurikens   = 'Fuuma shurikens';
  sWeaponTwoHandedStaves  = 'Two-handed staves';
  sWeaponMaxType          = 'Max Type'; // looks like dummy
  sWeaponDualWieldDaggers = 'Dual-wield Daggers';
  sWeaponDualWieldSwords  = 'Dual-wield swords';
  sWeaponDualWieldAxes    = 'Dual-wield Axes';
  sWeaponDaggerSword      = 'Dagger + sword';
  sWeaponDaggerAxe        = 'Dagger + Axe';
  sWeaponSwordAxe         = 'Sword + Axe';
  // ammo view
  sAmmoArrows          = 'Arrows';
  sAmmoThrowableDagers = 'Throwable daggers';
  sAmmoBullets         = 'Bullets';
  sAmmoShells          = 'Shells';
  sAmmoGrenades        = 'Grenades';
  sAmmoShurikens       = 'Shuriken';
  sAmmoKunai           = 'Kunai';
  sAmmoCannonballs     = 'Cannonballs';
  sAmmoThrowableItems  = 'Throwable Items';
  // jobs
  sJobNovice        = '(S.) Novice';
  sJobSwordman      = 'Swordman';
  sJobMagician      = 'Magician';
  sJobArcher        = 'Archer';
  sJobAcolyte       = 'Acolyte';
  sJobMerchant      = 'Merchant';
  sJobThief         = 'Thief';
  sJobKnight        = 'Knight';
  sJobPriest        = 'Priest';
  sJobWizard        = 'Wizard';
  sJobBlacksmith    = 'Blacksmith';
  sJobHunter        = 'Hunter';
  sJobAssassin      = 'Assassin';
  sJobCrusader      = 'Crusader';
  sJobMonk          = 'Monk';
  sJobSage          = 'Sage';
  sJobRogue         = 'Rogue';
  sJobAlchemist     = 'Alchemist';
  sJobBardDancer    = 'Bard/Dancer';
  sJobTaekwon       = 'Taekwon';
  sJobStarGladiator = 'Star Gladiator';
  sJobSoulLinker    = 'Soul Linker';
  sJobGunslinger    = 'Gunslinger';
  sJobNinja         = 'Ninja';
  sJobGangsi        = 'Gangsi';
  sJobDeathKnight   = 'Death Knight';
  sJobDarkCollector = 'Dark Collector';
  sJobKagerouOboro  = 'Kagerou/Oboro';
  sJobRebellion     = 'Rebellion';
  sJobSummoner      = 'Summoner';
  // classes
  sClassNormal      = 'Normal';
  sClassTrans       = 'Transcedent';
  sClassBaby        = 'Baby';
  sClassThird       = 'Third';
  sClassTransThird  = 'Transcedent-Third';
  sClassThirdBaby   = 'Third-Baby';
  // Equip location
  sLocUpper          = 'Upper Headgear';
  sLocMiddle         = 'Middle Headgear';
  sLocLower          = 'Lower Headgear';
  sLocArmor          = 'Armor';
  sLocWeapon         = 'Weapon';
  sLocShield         = 'Shield';
  sLocGarment        = 'Garment';
  sLocFootgear       = 'Footgear';
  sLocAccessRight    = 'Accessory Right';
  sLocAccessLeft     = 'Accessory Left';
  sLocCostumeTop     = 'Costume Top Headgear';
  sLocCostumeMid     = 'Costume Mid Headgear';
  sLocCostumeLow     = 'Costume Low Headgear';
  sLocCostumeGarment = 'Costume Garment/Robe';
  sLocAmmo           = 'Ammo';
  sLocShadowArmor    = 'Shadow Armor';
  sLocShadowWeapon   = 'Shadow Weapon';
  sLocShadowShield   = 'Shadow Shield';
  sLocShadowShoes    = 'Shadow Shoes';
  sLocShadowAccRight = 'Shadow Acc.Right (Earring)';
  sLocShadowAccLeft  = 'Shadow Acc.Left (Pendant)';
  // special
  sHeadgear          = 'Headgear';
  sAccess            = 'Accessory';
  sUpper             = 'Upper';
  sMiddle            = 'Middle';
  sLower             = 'Lower';
  // Monster modes
  sMMCanMove               = 'Can move';
  sMMCanMoveHint           = 'Enables the mob to move/chase characters.';
  sMMLooter                = 'Looter';
  sMMLooterHint            = 'The mob will loot up nearby items on the ground when it''s on idle state.';
  sMMAgressive             = 'Agressive';
  sMMAgressiveHint         = 'Normal aggressive mob, will look for a close-by player to attack.';
  sMMAssist                = 'Assist';
  sMMAssistHint            = 'When a nearby mob of the same class attacks, assist types will join them.';
  sMMCastsensorIdle        = 'Castsensor idle';
  sMMCastsensorIdleHint    = 'Will go after characters who start casting on them if idle'#13#10+
                             'or walking (without a target)';
  sMMNoRandomWalk          = 'No random walk';
  sMMNoRandomWalkHint      = 'The mob will not randomly walk around while in the idle state.';
  sMMNoCastSkill           = 'No cast skill';
  sMMNoCastSkillHint       = 'The mob will be unable to cast skills.';
  sMMCanAttack             = 'Can attack';
  sMMCanAttackHint         = 'Enables the mob to attack/retaliate when you are within attack'#13#10+
                             'range. Note that this only enables them to use normal attacks, skills are'#13#10+
                             'always allowed.';
  sMMCastsensorChase       = 'Castsensor chase';
  sMMCastsensorChaseHint   = 'Will go after characters who start casting on them if idle'#13#10+
                             'or chasing other players (they switch chase targets)';
  sMMChangeChase           = 'Change chase';
  sMMChangeChaseHint       = 'Allows chasing mobs to switch targets if another player happens'#13#10+
                             'to be within attack range (handy on ranged attackers, for example)';
  sMMAngry                 = 'Angry';
  sMMAngryHint             = 'These mobs are "hyper-active". Apart from "chase"/"attack", they have'#13#10+
                             'the states "follow"/"angry". Once hit, they stop using these states and use'#13#10+
                             'the normal ones. The new states are used to determine a different skill-set'#13#10+
                             'for their "before attacked" and "after attacked" states. Also, when'#13#10+
                             '"following", they automatically switch to whoever character is closest.';
  sMMChangeTargetMelee     = 'Change target melee';
  sMMChangeTargetMeleeHint = 'Enables a mob to switch targets when hit by a normal attack'#13#10+
                             'while attacking someone else.';
  sMMChangeTargetChase     = 'Change target chase';
  sMMChangeTargetChaseHint = 'Enables a mob to switch targets when hit by any attack'#13#10+
                             'while chasing another character.';
  sMMTargetWeak            = 'Target weak';
  sMMTargetWeakHint        = 'Allows aggressive monsters to only be aggressive against '#13#10+
                             'characters that are five levels below it''s own level.'#13#10+
                             'For example, a monster of level 104 will not pick fights with a level 99.';
  sMMRandomTarget          = 'Random target';
  sMMRandomTargetHint      = 'Picks a new random target in range on each attack / skill.';
  sMMIgnoreMelee           = 'Ignore melee';
  sMMIgnoreMeleeHint       = 'The mob will take 1 HP damage from physical attacks.';
  sMMIgnoreMagic           = 'Ignore magic';
  sMMIgnoreMagicHint       = 'The mob will take 1 HP damage from magic attacks.';
  sMMIgnoreRanged          = 'Ignore ranged';
  sMMIgnoreRangedHint      = 'The mob will take 1 HP damage from ranged attacks.';
  sMMMVP                   = 'MVP';
  sMMMVPHint               = 'Flagged as MVP which makes mobs resistant to Coma. Also displays the'#13#10+
                             'MVP sign and gives players MVP EXP or MVP items.';
  sMMIgnoreMisc            = 'Ignore misc';
  sMMIgnoreMiscHint        = 'The mob will take 1 HP damage from "none" attack type.';
  sMMKnockbackImmune       = 'Knockback immune';
  sMMKnockbackImmuneHint   = 'The mob will be unable to be knocked back.';
  sMMTeleportBlock         = 'Teleport block';
  sMMTeleportBlockHint     = 'Not implemented yet.';
  sMMFixedItemDrop         = 'Fixed item drop';
  sMMFixedItemDropHint     = 'The mob''s drops are not affected by item drop modifiers.';
  sMMDetector              = 'Detector';
  sMMDetectorHint          = 'Enables mob to detect and attack characters who are in hiding/cloak.';
  sMMStatusImmune          = 'Status immune';
  sMMStatusImmuneHint      = 'Immune to being affected by statuses.';
  sMMSkillImmune           = 'Skill immune';
  sMMSkillImmuneHint       = 'Immune to being affected by skills.';
  // trade modes
  sTradeNoDrop    = 'No drop';
  sTradeNoVend    = 'No vending';
  sTradeWedding   = 'Wedding trade';
  sTradeNoSold    = 'No NPC Sold';
  sTradeNoCart    = 'No Cart';
  sTradeNoStorage = 'No Storage';
  sTradeNoGuild   = 'No Guild Storage';
  sTradeNoMail    = 'No Mail';
  sTradeNoAuction = 'No Auction';


//===== Lists =====

const
  ROItemType:array [0..18] of AnsiString = (
    sItemHealing,
    sUnknown,
    sItemUsable,
    sItemEtc,
    sItemEquipment,
    sItemWeapon,
    sItemCard,
    sItemPetegg,
    sItemPetEquip,
    sUnknown,
    sItemAmmo,
    sItemSkillItem,
    sItemShadowEquip,
    sUnknown,
    sUnknown,
    sUnknown,
    sUnknown,
    sUnknown,
    sItemDelayed
  );
const
  ROAmmoView:array [0..9] of AnsiString = (
    sUnknown,
    sAmmoArrows,
    sAmmoThrowableDagers,
    sAmmoBullets,
    sAmmoShells,
    sAmmoGrenades,
    sAmmoShurikens,
    sAmmoKunai,
    sAmmoCannonballs,
    sAmmoThrowableItems
  );
const
  ROWeaponMaxType = 24; // Max type = 24
  ROWeaponView:array [0..30] of AnsiString = (
    sWeaponBareFist,
    sWeaponDaggers,
    sWeaponOneHandedSwords,
    sWeaponTwoHandedSwords,
    sWeaponOneHandedSpears,
    sWeaponTwoHandedSpears,
    sWeaponOneHandedAxes,
    sWeaponTwoHandedAxes,
    sWeaponMaces,
    sUnused,
    sWeaponStaves,
    sWeaponBows,
    sWeaponKnuckles,
    sWeaponInstruments,
    sWeaponWhips,
    sWeaponBooks,
    sWeaponKatars,
    sWeaponRevolvers,
    sWeaponRifles,
    sWeaponGatlingGuns,
    sWeaponShotguns,
    sWeaponGrenadeLaunchers,
    sWeaponFuumaShurikens,
    sWeaponTwoHandedStaves,
    sWeaponMaxType,
    sWeaponDualWieldDaggers,
    sWeaponDualWieldSwords,
    sWeaponDualWieldAxes,
    sWeaponDaggerSword,
    sWeaponDaggerAxe,
    sWeaponSwordAxe
  );

const
  ROGender:array [0..2] of AnsiString = (sFemale, sMale, sBoth);
const
  RORange:array [boolean] of AnsiString = (sMelee,sRanged);
const
  ROSizes:array [0..2] of AnsiString = (sSmall, sMedium, sLarge);
const
  ROElements:array [0..9] of AnsiString = (
    sNeutral,
    sWater,
    sEarth,
    sFire,
    sWind,
    sPoison,
    sHoly,
    sShadow,
    sGhost,
    sUndead
  );
const
  RORaces:array [0..10] of AnsiString = (
    sFormless,
    sUndead,
    sBrute,
    sPlant,
    sInsect,
    sFish,
    sDemon,
    sDemiHuman,
    sAngel,
    sDragon,
    sPlayer
  );

//===== Masks =====

type
  tROMask = record
    name:AnsiString;
    hint:AnsiString;
    mask:cardinal;
  end;
type
  tROMask2 = record
    name :AnsiString;
    hint :AnsiString;
    mask1:cardinal;
    mask2:cardinal;
  end;
const
  ROTrade:array [0..8] of tROMask = (
    (name:sTradeNoDrop   ; hint:''; mask:$00000001;),
    (name:sTradeNoVend   ; hint:''; mask:$00000002;),
    (name:sTradeWedding  ; hint:''; mask:$00000004;),
    (name:sTradeNoSold   ; hint:''; mask:$00000008;),
    (name:sTradeNoCart   ; hint:''; mask:$00000010;),
    (name:sTradeNoStorage; hint:''; mask:$00000020;),
    (name:sTradeNoGuild  ; hint:''; mask:$00000040;),
    (name:sTradeNoMail   ; hint:''; mask:$00000080;),
    (name:sTradeNoAuction; hint:''; mask:$00000100;)
  );

const
  ROJobs:array [0..29] of tROMask = (
    (name:sJobNovice         ; hint:''; mask:$00000001;),
    (name:sJobSwordman       ; hint:''; mask:$00000002;),
    (name:sJobMagician       ; hint:''; mask:$00000004;),
    (name:sJobArcher         ; hint:''; mask:$00000008;),
    (name:sJobAcolyte        ; hint:''; mask:$00000010;),
    (name:sJobMerchant       ; hint:''; mask:$00000020;),
    (name:sJobThief          ; hint:''; mask:$00000040;),
    (name:sJobKnight         ; hint:''; mask:$00000080;),
    (name:sJobPriest         ; hint:''; mask:$00000100;),
    (name:sJobWizard         ; hint:''; mask:$00000200;),
    (name:sJobBlacksmith     ; hint:''; mask:$00000400;),
    (name:sJobHunter         ; hint:''; mask:$00000800;),
    (name:sJobAssassin       ; hint:''; mask:$00001000;),
//    (name:sUnused            ; hint:''; mask:$00002000;),
    (name:sJobCrusader       ; hint:''; mask:$00004000;),
    (name:sJobMonk           ; hint:''; mask:$00008000;),
    (name:sJobSage           ; hint:''; mask:$00010000;),
    (name:sJobRogue          ; hint:''; mask:$00020000;),
    (name:sJobAlchemist      ; hint:''; mask:$00040000;),
    (name:sJobBardDancer     ; hint:''; mask:$00080000;),
//    (name:sUnused            ; hint:''; mask:$00100000;),
    (name:sJobTaekwon        ; hint:''; mask:$00200000;),
    (name:sJobStarGladiator  ; hint:''; mask:$00400000;),
    (name:sJobSoulLinker     ; hint:''; mask:$00800000;),
    (name:sJobGunslinger     ; hint:''; mask:$01000000;),
    (name:sJobNinja          ; hint:''; mask:$02000000;),
    (name:sJobGangsi         ; hint:''; mask:$04000000;),
    (name:sJobDeathKnight    ; hint:''; mask:$08000000;),
    (name:sJobDarkCollector  ; hint:''; mask:$10000000;),
    (name:sJobKagerouOboro   ; hint:''; mask:$20000000;),
    (name:sJobRebellion      ; hint:''; mask:$40000000;),
    (name:sJobSummoner       ; hint:''; mask:$80000000;)
  );

const
  ROClasses:array [0..5] of tROMask = (
    (name:sClassNormal    ; hint:''; mask:$00000001;),
    (name:sClassTrans     ; hint:''; mask:$00000002;),
    (name:sClassBaby      ; hint:''; mask:$00000004;),
    (name:sClassThird     ; hint:''; mask:$00000008;),
    (name:sClassTransThird; hint:''; mask:$00000010;),
    (name:sClassThirdBaby ; hint:''; mask:$00000020;)
  );

const
  EQP_HEAD_LOW         = $00000001;
  EQP_HEAD_MID         = $00000200; // 512
  EQP_HEAD_TOP         = $00000100; // 256
  EQP_HAND_R           = $00000002; // 2
  EQP_HAND_L           = $00000020; // 32
  EQP_ARMOR            = $00000010; // 16
  EQP_SHOES            = $00000040; // 64
  EQP_GARMENT          = $00000004; // 4
  EQP_ACC_R            = $00000008; // 8
  EQP_ACC_L            = $00000080; // 128
  EQP_COSTUME_HEAD_TOP = $00000400; // 1024
  EQP_COSTUME_HEAD_MID = $00000800; // 2048
  EQP_COSTUME_HEAD_LOW = $00001000; // 4096
  EQP_COSTUME_GARMENT  = $00002000; // 8192
  //EQP_COSTUME_FLOOR  = $00004000; // 16384
  EQP_AMMO             = $00008000; // 32768
  EQP_SHADOW_ARMOR     = $00010000; // 65536
  EQP_SHADOW_WEAPON    = $00020000; // 131072
  EQP_SHADOW_SHIELD    = $00040000; // 262144
  EQP_SHADOW_SHOES     = $00080000; // 524288
  EQP_SHADOW_ACC_R     = $00100000; // 1048576
  EQP_SHADOW_ACC_L     = $00200000; // 2097152
  // Combined
  EQP_HEAD             = EQP_HEAD_TOP or EQP_HEAD_MID or EQP_HEAD_LOW;
  EQP_COSTUME_HEAD     = EQP_COSTUME_HEAD_TOP or EQP_COSTUME_HEAD_MID or EQP_COSTUME_HEAD_LOW;
  EQP_ACC_RL           = EQP_ACC_R or EQP_ACC_L;
  EQP_SHADOW_ACC_RL    = EQP_SHADOW_ACC_R or EQP_SHADOW_ACC_L;


const
  ROLocations:array [0..20] of tROMask = (
    (name:sLocUpper         ; hint:''; mask:EQP_HEAD_TOP        ;),
    (name:sLocMiddle        ; hint:''; mask:EQP_HEAD_MID        ;),
    (name:sLocLower         ; hint:''; mask:EQP_HEAD_LOW        ;),
    (name:sLocArmor         ; hint:''; mask:EQP_ARMOR           ;),
    (name:sLocWeapon        ; hint:''; mask:EQP_HAND_R          ;),
    (name:sLocShield        ; hint:''; mask:EQP_HAND_L          ;),
    (name:sLocGarment       ; hint:''; mask:EQP_GARMENT         ;),
    (name:sLocFootgear      ; hint:''; mask:EQP_SHOES           ;),
    (name:sLocAccessRight   ; hint:''; mask:EQP_ACC_R           ;),
    (name:sLocAccessLeft    ; hint:''; mask:EQP_ACC_L           ;),
    (name:sLocCostumeTop    ; hint:''; mask:EQP_COSTUME_HEAD_TOP;),
    (name:sLocCostumeMid    ; hint:''; mask:EQP_COSTUME_HEAD_MID;),
    (name:sLocCostumeLow    ; hint:''; mask:EQP_COSTUME_HEAD_LOW;),
    (name:sLocCostumeGarment; hint:''; mask:EQP_COSTUME_GARMENT ;),
    (name:sLocAmmo          ; hint:''; mask:EQP_AMMO            ;),
    (name:sLocShadowArmor   ; hint:''; mask:EQP_SHADOW_ARMOR    ;),
    (name:sLocShadowWeapon  ; hint:''; mask:EQP_SHADOW_WEAPON   ;),
    (name:sLocShadowShield  ; hint:''; mask:EQP_SHADOW_SHIELD   ;),
    (name:sLocShadowShoes   ; hint:''; mask:EQP_SHADOW_SHOES    ;),
    (name:sLocShadowAccRight; hint:''; mask:EQP_SHADOW_ACC_R    ;),
    (name:sLocShadowAccLeft ; hint:''; mask:EQP_SHADOW_ACC_L    ;)
  );

// Monster modes
const
  MD_CANMOVE            = $00000001;
  MD_LOOTER             = $00000002;
  MD_AGGRESSIVE         = $00000004;
  MD_ASSIST             = $00000008;
  MD_CASTSENSOR_IDLE    = $00000010;
  MD_NORANDOM_WALK      = $00000020;
  MD_NOCAST_SKILL       = $00000040;
  MD_CANATTACK          = $00000080;
//  FREE                  = $00000100;
  MD_CASTSENSOR_CHASE   = $00000200;
  MD_CHANGECHASE        = $00000400;
  MD_ANGRY              = $00000800;
  MD_CHANGETARGET_MELEE = $00001000;
  MD_CHANGETARGET_CHASE = $00002000;
  MD_TARGETWEAK         = $00004000;
  MD_RANDOMTARGET       = $00008000;
  MD_IGNOREMELEE        = $00010000;
  MD_IGNOREMAGIC        = $00020000;
  MD_IGNORERANGED       = $00040000;
  MD_MVP                = $00080000;
  MD_IGNOREMISC         = $00100000;
  MD_KNOCKBACK_IMMUNE   = $00200000;
  MD_TELEPORT_BLOCK     = $00400000;
//  FREE                  = $00800000;
  MD_FIXED_ITEMDROP     = $01000000;
  MD_DETECTOR           = $02000000;
  MD_STATUS_IMMUNE      = $04000000;
  MD_SKILL_IMMUNE       = $08000000;

const
  ROMobModes:array [0..25] of tROMask = (
    (name:sMMCanMove          ; hint:sMMCanMoveHint          ; mask:MD_CANMOVE           ;),
    (name:sMMLooter           ; hint:sMMLooterHint           ; mask:MD_LOOTER            ;),
    (name:sMMAgressive        ; hint:sMMAgressiveHint        ; mask:MD_AGGRESSIVE        ;),
    (name:sMMAssist           ; hint:sMMAssistHint           ; mask:MD_ASSIST            ;),
    (name:sMMCastsensorIdle   ; hint:sMMCastsensorIdleHint   ; mask:MD_CASTSENSOR_IDLE   ;),
    (name:sMMNoRandomWalk     ; hint:sMMNoRandomWalkHint     ; mask:MD_NORANDOM_WALK     ;),
    (name:sMMNoCastSkill      ; hint:sMMNoCastSkillHint      ; mask:MD_NOCAST_SKILL      ;),
    (name:sMMCanAttack        ; hint:sMMCanAttackHint        ; mask:MD_CANATTACK         ;),

    (name:sMMCastsensorChase  ; hint:sMMCastsensorChaseHint  ; mask:MD_CASTSENSOR_CHASE  ;),
    (name:sMMChangeChase      ; hint:sMMChangeChaseHint      ; mask:MD_CHANGECHASE       ;),
    (name:sMMAngry            ; hint:sMMAngryHint            ; mask:MD_ANGRY             ;),
    (name:sMMChangeTargetMelee; hint:sMMChangeTargetMeleeHint; mask:MD_CHANGETARGET_MELEE;),
    (name:sMMChangeTargetChase; hint:sMMChangeTargetChaseHint; mask:MD_CHANGETARGET_CHASE;),
    (name:sMMTargetWeak       ; hint:sMMTargetWeakHint       ; mask:MD_TARGETWEAK        ;),
    (name:sMMRandomTarget     ; hint:sMMRandomTargetHint     ; mask:MD_RANDOMTARGET      ;),
    (name:sMMIgnoreMelee      ; hint:sMMIgnoreMeleeHint      ; mask:MD_IGNOREMELEE       ;),
    (name:sMMIgnoreMagic      ; hint:sMMIgnoreMagicHint      ; mask:MD_IGNOREMAGIC       ;),
    (name:sMMIgnoreRanged     ; hint:sMMIgnoreRangedHint     ; mask:MD_IGNORERANGED      ;),
    (name:sMMMVP              ; hint:sMMMVPHint              ; mask:MD_MVP               ;),
    (name:sMMIgnoreMisc       ; hint:sMMIgnoreMiscHint       ; mask:MD_IGNOREMISC        ;),
    (name:sMMKnockbackImmune  ; hint:sMMKnockbackImmuneHint  ; mask:MD_KNOCKBACK_IMMUNE  ;),
    (name:sMMTeleportBlock    ; hint:sMMTeleportBlockHint    ; mask:MD_TELEPORT_BLOCK    ;),

    (name:sMMFixedItemDrop    ; hint:sMMFixedItemDropHint    ; mask:MD_FIXED_ITEMDROP    ;),
    (name:sMMDetector         ; hint:sMMDetectorHint         ; mask:MD_DETECTOR          ;),
    (name:sMMStatusImmune     ; hint:sMMStatusImmuneHint     ; mask:MD_STATUS_IMMUNE     ;),
    (name:sMMSkillImmune      ; hint:sMMSkillImmuneHint      ; mask:MD_SKILL_IMMUNE      ;)
  );


implementation

end.
