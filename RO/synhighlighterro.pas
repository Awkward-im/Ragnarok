unit SynHighlighterRO;

interface

uses
  SysUtils, Classes, Controls, Graphics,
  SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase;

type
  TProcTableProc = procedure of object;

  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkLabel,
    tkVariable, tkSpace, tkString, tkSymbol, tkUnknown, tkConstant);

  TRangeState = (rsANil, rsCStyle, rsUnKnown);

type
  TROCodeFoldBlockType = (rofbUnknown,rofbBrakets,rofbCase,rofdComment);

type
  { TSynAnySyn }

  TSynROSyn = class(TSynCustomFoldHighlighter)
  private
    fRange: TRangeState;
    fCurRange: Integer;
    fLine: PChar;
    fLineNumber : Integer;
    Run: LongInt;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentChars: TSynIdentChars;
    fProcTable: array[#0..#255] of TProcTableProc;
    fTabAttri       : TSynHighlighterAttributes;
    fCommentAttri   : TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri       : TSynHighlighterAttributes;
    fLabelAttri     : TSynHighlighterAttributes;
    fConstantAttri  : TSynHighlighterAttributes;
    fNumberAttri    : TSynHighlighterAttributes;
    fSpaceAttri     : TSynHighlighterAttributes;
    fStringAttri    : TSynHighlighterAttributes;
    fSymbolAttri    : TSynHighlighterAttributes;
    fVariableAttri  : TSynHighlighterAttributes;
    fKeyWords :TStringList;
    fConstants:TStringList;
    procedure VarCharProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure RoundCloseProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
    procedure CStyleProc;
    procedure SetKeyWords(const Value: TStringList);
    function  GetIdentifierChars: string;
    procedure SetIdentifierChars(const Value: string);
    procedure SetConstants(const Value: TStringList);
    function  GetCaseSense:boolean;
    procedure SetCaseSense(arg:boolean);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function StartROCodeFoldBlock
             (ABlockType: TROCodeFoldBlockType): TSynCustomCodeFoldBlock;
    procedure EndROCodeFoldBlock;
    function TopROCodeFoldBlockType(DownIndex: Integer = 0): TROCodeFoldBlockType;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  GetEol: Boolean; override;

    function  GetTokenID  : TtkTokenKind;
    function  GetToken    : String; override;
    function  GetTokenKind: integer; override;
    function  GetTokenPos : Integer; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;

    function  IsKeyword (const AKeyword : string): boolean; override;
    function  IsConstant(const AConstant: string): boolean;
    procedure Next; override;

    function  GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure LoadHighLighter(const aFile, aScheme: string);
  published
    property CommentAttri   : TSynHighlighterAttributes read fCommentAttri    write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri       : TSynHighlighterAttributes read fKeyAttri        write fKeyAttri;
    property LabelAttri     : TSynHighlighterAttributes read fLabelAttri      write fLabelAttri;
    property ConstantAttri  : TSynHighlighterAttributes read fConstantAttri   write fConstantAttri;
    property VariableAttri  : TSynHighlighterAttributes read fVariableAttri   write fVariableAttri;
    property NumberAttri    : TSynHighlighterAttributes read fNumberAttri     write fNumberAttri;
    property TabAttri       : TSynHighlighterAttributes read fTabAttri        write fTabAttri;
    property SpaceAttri     : TSynHighlighterAttributes read fSpaceAttri      write fSpaceAttri;
    property StringAttri    : TSynHighlighterAttributes read fStringAttri     write fStringAttri;
    property SymbolAttri    : TSynHighlighterAttributes read fSymbolAttri     write fSymbolAttri;
    property IdentifierChars: string read GetIdentifierChars  write SetIdentifierChars;
    property KeyWords : TStringList read fKeyWords  write SetKeyWords;
    property Constants: TStringList read fConstants write SetConstants;
    property CaseSensitive: boolean read GetCaseSense write SetCaseSense;
  end;

var
  SynROSyn:TSynROSyn=nil;


implementation

uses
  SynEditStrConst,
  cmemini;
//  inilist;

var
  Identifiers: array[#0..#255] of ByteBool;

constructor TSynROSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).CaseSensitive:=True;
  TStringList(fKeyWords).Sorted       :=True;
  TStringList(fKeyWords).Duplicates   :=dupIgnore;

  fConstants := TStringList.Create;
  TStringList(fConstants).CaseSensitive:=True;
  TStringList(fConstants).Sorted       :=True;
  TStringList(fConstants).Duplicates   :=dupIgnore;

  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);

  fVariableAttri := TSynHighlighterAttributes.Create('ro_variable');
  fVariableAttri.Style     :=[fsBold];
  fVariableAttri.Foreground:=clpurple;
  AddAttribute(fVariableAttri);

  fTabAttri := TSynHighlighterAttributes.Create('ro_tab');
  fTabAttri.Background:=$F0F0F0;
  AddAttribute(fTabAttri);

  fLabelAttri := TSynHighlighterAttributes.Create('ro_label');
  fLabelAttri.Style := [fsBold,fsUnderline];
  AddAttribute(fLabelAttri);

  fConstantAttri := TSynHighlighterAttributes.Create('ro_constant');
  fConstantAttri.Style     :=[fsBold];
  fConstantAttri.Foreground:=clfuchsia;
  AddAttribute(fConstantAttri);

  SetAttributesOnChange(@DefHighlightChange);

  fIdentChars := inherited GetIdentChars;
  MakeMethodTables;
  fRange := rsUnknown;
end; { Create }

destructor TSynROSyn.Destroy;
begin
  fKeyWords.Free;
  FConstants.Free;

  inherited Destroy;
end; { Destroy }


function TSynROSyn.StartROCodeFoldBlock(ABlockType: TROCodeFoldBlockType): TSynCustomCodeFoldBlock;
begin
  Result:=StartCodeFoldBlock(Pointer(PtrInt(ABlockType)));
end;

procedure TSynROSyn.EndROCodeFoldBlock;
begin
  EndCodeFoldBlock(true);
end;

function TSynROSyn.TopROCodeFoldBlockType(DownIndex: Integer = 0): TROCodeFoldBlockType;
var
  p: Pointer;
begin
  p := TopCodeFoldBlockType(DownIndex);
  Result := TROCodeFoldBlockType(PtrUInt(p));
end;

procedure TSynROSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '"': fProcTable[I] := @StringProc;
      '$','#','.','@','''': fProcTable[I] := @VarCharProc;

      '{': fProcTable[I] := @BraceOpenProc;
      '}': fProcTable[I] := @BraceCloseProc;

      ';': fProcTable[I] := @PointCommaProc;
      '(': fProcTable[I] := @RoundOpenProc;
      ')': fProcTable[I] := @RoundCloseProc;

      #13: fProcTable[I] := @CRProc;
      #10: fProcTable[I] := @LFProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #0: fProcTable[I] := @NullProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '/': fProcTable[I] := @SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      else fProcTable[I] := @UnknownProc;
    end;
end;

procedure TSynROSyn.VarCharProc;
begin
  if (fLine[Run]='#') and (Run>0) and
     (fLine[Run-1] in ['0'..'9','a'..'z','A'..'Z','_']) and
     (fLine[Run+1] in ['0'..'9','a'..'z','A'..'Z','_']) then
  begin
    inc(Run);
    fTokenID := tkSymbol;
    exit;
  end;

  fTokenID := tkVariable;
  if ((fLine[Run]='$') and (fLine[Run+1]='@')) or
     ((fLine[Run]='.') and (fLine[Run+1]='@')) or
     ((fLine[Run]='#') and (fLine[Run+1]='#')) then
  begin
   inc(Run);
  end;
  repeat
    inc(Run);
  until not (fLine[Run] in ['0'..'9','a'..'z','A'..'Z','_']);
  if fLine[Run]='$' then
    inc(Run);
end;

procedure TSynROSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  if fRange <> rsCStyle then
    StartROCodeFoldBlock(rofbBrakets);
end;

procedure TSynROSyn.BraceCloseProc;
var
  i:integer;
begin
  inc(Run);
  FTokenID := tkSymbol;
  if fRange <> rsCStyle then
  begin
    i:=integer(TopROCodeFoldBlockType());
    while (i<>0) and (i<>PtrInt(rofbBrakets)) do
    begin
      EndROCodeFoldBlock();
      i:=integer(TopROCodeFoldBlockType());
    end;
    EndCodeFoldBlock(true);
  end;
end;

procedure TSynROSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynROSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynROSyn.RoundCloseProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynROSyn.IdentProc;
var
  aToken:string;
  maybelabel:boolean;
begin
  maybelabel:=Run=0;
  while Identifiers[fLine[Run]] do inc(Run);
  if (fLine[Run]=':') and (fLine[Run+1] in [#0,#10,#13]) and maybelabel then
  begin
    inc(Run);
    fTokenId:=tkLabel;
    exit;
  end;
  aToken:=GetToken;
  if IsKeyWord (aToken) then
  begin
    if aToken='case' then
    begin
      if fRange <> rsCStyle then StartROCodeFoldBlock(rofbCase)
    end
    // don't check in "if" and same conditions
{}
    else if aToken='break' then
    begin
      if (fRange <> rsCStyle) and
         (TopROCodeFoldBlockType()=rofbCase) then EndROCodeFoldBlock()
    end
    else if aToken='close' then
    begin
      if (fRange <> rsCStyle) and
         (TopROCodeFoldBlockType()=rofbCase) then EndROCodeFoldBlock();
    end;
{}
    fTokenId:=tkKey
  end
  else if IsConstant(aToken) then fTokenId:=tkConstant
  else fTokenId := tkIdentifier;
end;

procedure TSynROSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynROSyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;

procedure TSynROSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E', 'x'] do
  begin
    case FLine[Run] of
      'x': begin // handle C style hex numbers
             IntegerProc;
             break;
           end;
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynROSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
    '*':
      begin
        fTokenID := tkComment;
        fRange := rsCStyle;
        StartCodeFoldBlock(Pointer(PtrInt(rofdComment)));
        inc(Run);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                fRange := rsUnKnown;
                inc(Run, 2);
                EndCodeFoldBlock();
                break;
              end else inc(Run);
            #10: break;
            #13: break;
          else inc(Run);
          end;
      end;
  else
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynROSyn.StringProc;
begin
  fTokenID := tkString;
  if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = '"';
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynROSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnKnown;
end;

procedure TSynROSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynROSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynROSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynROSyn.CStyleProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        EndCodeFoldBlock();
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynROSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsCStyle  : CStyleProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

procedure TSynROSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynROSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD   : Result := fKeyAttri;
    SYN_ATTR_STRING    : Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL    : Result := fSymbolAttri;
    SYN_ATTR_NUMBER    : Result := fNumberAttri;
    SYN_ATTR_VARIABLE  : Result := fVariableAttri;
  else
    Result := nil;
  end;
end;

function TSynROSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;


function TSynROSyn.GetRange: Pointer;
begin
  CodeFoldRange.RangeType := Pointer(PtrUInt(fCurRange*1000+ord(fRange)));

  Result := inherited GetRange;
end;

procedure TSynROSyn.ReSetRange;
begin
  inherited ResetRange;

  fCurRange := 0;
  fRange    := rsANil;
end;

procedure TSynROSyn.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);

  fCurRange :=             PtrUInt(CodeFoldRange.RangeType) div 1000;
  fRange    := tRangeState(PtrUInt(CodeFoldRange.RangeType) mod 1000);
end;


function TSynROSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynROSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Result:='';
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynROSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynROSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynROSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynROSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment   : Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey       : Result := fKeyAttri;
    tkConstant  : Result := fConstantAttri;
    tkNumber    : Result := fNumberAttri;
    tkString    : Result := fStringAttri;
    tkSymbol    : Result := fSymbolAttri;
    tkVariable  : Result := fVariableAttri;
    tkLabel     : Result := fLabelAttri;
    tkUnknown   : Result := fSymbolAttri;
    tkSpace     : begin
      if FLine[fTokenPos] = #9 then
        Result := fTabAttri
      else
        Result := fSpaceAttri;
    end
  else
    Result := nil;
  end;
end;


function TSynROSyn.IsKeyword(const AKeyword: string): boolean;
begin
  result:=fKeywords.IndexOf(AKeyword)>=0;
end; { IsKeyWord }

function TSynROSyn.IsConstant(const AConstant: string): boolean;
begin
  result:=fConstants.IndexOf(AConstant)>=0;
end; { IsConstant }

procedure TSynROSyn.SetKeyWords(const Value: TStringList);
begin
  fKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynROSyn.SetConstants(const Value: TStringList);
begin
  fConstants.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynROSyn.GetCaseSense:boolean;
begin
  result:=fKeywords.CaseSensitive or fConstants.CaseSEnsitive
end;

procedure TSynROSyn.SetCaseSense(arg:boolean);
begin
  fKeywords .CaseSensitive:=arg;
  fConstants.CaseSEnsitive:=arg;
end;


class function TSynROSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;


function TSynROSyn.GetIdentifierChars: string;
var
  ch: char;
  s: shortstring;
begin
  s := '';
  for ch := #0 to #255 do
    if ch in fIdentChars then s := s + ch;
  Result := s;
end;

procedure TSynROSyn.SetIdentifierChars(const Value: string);
var
  i: integer;
begin
  fIdentChars := [];
  for i := 1 to Length(Value) do begin
    fIdentChars := fIdentChars + [Value[i]];
  end; //for
end;

function TSynROSyn.GetIdentChars: TSynIdentChars;
begin
  Result := fIdentChars;
end;

(*
procedure ReadAttribute(hini:TIniList; attr:TSynHighlighterAttributes;attrname:string);
var
  s:string;
begin
  s:='';
  if (fsbold      in attr.Style) then s:='bold,';
  if (fsitalic    in attr.Style) then s:=s+'italic,';
  if (fsunderline in attr.Style) then s:=s+'underline';
  s:=hini.ReadString(AttrName,'Style',s);
  if (s='normal') or (s='') then
    attr.style:=[]
  else begin
    if pos('bold'     ,s)>0 then attr.style:=attr.style+[fsbold]
                            else attr.style:=attr.style-[fsbold];
    if pos('italic'   ,s)>0 then attr.style:=attr.style+[fsitalic]
                            else attr.style:=attr.style-[fsitalic];
    if pos('underline',s)>0 then attr.style:=attr.style+[fsunderline]
                            else attr.style:=attr.style-[fsunderline];
  end;
  s:=colortostring(Attr.Background);
  s:=hini.ReadString(AttrName,'Background',s);
  Attr.Background:=stringtocolor(s);
  s:=colortostring(Attr.Foreground);
  s:=hini.ReadString(AttrName,'Foreground',s);
  Attr.Foreground:=stringtocolor(s);
end;

procedure TSynROSyn.LoadHighLighter(const aFile, aScheme: string);
var
  hini:TIniList;
  HL:TSynROSyn;
{
s:string;
  b:boolean;
  i:integer;
  genlist:TStringlist;
}
begin
  if FileExists(aFile) then
  begin
    HL:=self;
    hini:=TINIList.Create;
    hini.LoadFromFile(aFile);
    // attributes
    ReadAttribute(hini, HL.Commentattri   ,'Comment');
    ReadAttribute(hini, HL.Identifierattri,'Identifier');
    ReadAttribute(hini, HL.Keyattri       ,'Key');
    ReadAttribute(hini, HL.Constantattri  ,'Constant');
    ReadAttribute(hini, HL.Numberattri    ,'Number');
    ReadAttribute(hini, HL.Spaceattri     ,'Space');
    ReadAttribute(hini, HL.Tabattri       ,'Tab');
    ReadAttribute(hini, HL.Labelattri     ,'Label');
    ReadAttribute(hini, HL.Stringattri    ,'String');
    ReadAttribute(hini, HL.Symbolattri    ,'Symbol');
    ReadAttribute(hini, HL.Variableattri  ,'Variables');

{
    genlist:=TStringList.create;
    // read keywords
    hini.ReadSectionNames('Keywords',genlist);
    if genlist.count>0 then
      for i:=0 to genlist.count-1 do
        genlist[i]:=uppercase(genlist[i]);
    HL.KeyWords.assign(genlist);
    hini.ReadSectionNames('Constants',genlist);
    if genlist.count>0 then
      for i:=0 to genlist.count-1 do
        genlist[i]:=uppercase(genlist[i]);
    HL.Constants.assign(genlist);
    genlist.free;
    makemethodtables;
}
    hini.Free;
  end;
end;
*)

procedure ReadAttribute(var hini:TIniFile; attr:TSynHighlighterAttributes; const AttrName:string);
var
  s,ls:string;
begin
  if hini.IsNamespace('Scheme') then
    ls:=AttrName+'.'
  else
  begin
    ls:='';
//    hini.Section:=pointer(AttrName);
  end;

  s:=hini['Scheme',pointer(AttrName),PAnsiChar(ls+'Style')];
  if s='' then
  begin
    if (fsbold      in attr.Style) then s:='bold,';
    if (fsitalic    in attr.Style) then s:=s+'italic,';
    if (fsunderline in attr.Style) then s:=s+'underline';
  end;
  if (s='normal') or (s='') then
    attr.style:=[]
  else
  begin
    if pos('bold'     ,s)>0 then attr.style:=attr.style+[fsbold]
                            else attr.style:=attr.style-[fsbold];
    if pos('italic'   ,s)>0 then attr.style:=attr.style+[fsitalic]
                            else attr.style:=attr.style-[fsitalic];
    if pos('underline',s)>0 then attr.style:=attr.style+[fsunderline]
                            else attr.style:=attr.style-[fsunderline];
  end;

  s:=hini[PAnsiChar(ls+'Background')];
  if s<>'' then Attr.Background:=stringtocolor(s);

  s:=hini[PAnsiChar(ls+'Foreground')];
  if s<>'' then Attr.Foreground:=stringtocolor(s);
end;

procedure TSynROSyn.LoadHighLighter(const aFile, aScheme: string);
var
  hini:TIniFile;
  HL:TSynROSyn;
begin
  if FileExists(aFile) then
  begin
    HL:=self;
    CreateINIFile(hini,aFile,true);
    if aScheme<>'' then
    begin
//      hini.Namespace:='Scheme';
//      hini.Section  :=pointer(aScheme);
    end;

    // attributes
    ReadAttribute(hini,HL.Commentattri   , 'Comment'   );
    ReadAttribute(hini,HL.Identifierattri, 'Identifier');
    ReadAttribute(hini,HL.Keyattri       , 'Key'       );
    ReadAttribute(hini,HL.Constantattri  , 'Constant'  );
    ReadAttribute(hini,HL.Numberattri    , 'Number'    );
    ReadAttribute(hini,HL.Spaceattri     , 'Space'     );
    ReadAttribute(hini,HL.Tabattri       , 'Tab'       );
    ReadAttribute(hini,HL.Labelattri     , 'Label'     );
    ReadAttribute(hini,HL.Stringattri    , 'String'    );
    ReadAttribute(hini,HL.Symbolattri    , 'Symbol'    );
    ReadAttribute(hini,HL.Variableattri  , 'Variables' );

    FreeINIFile(hini);
  end;
end;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Identifiers[I] := I in ['_', '0'..'9', 'a'..'z', 'A'..'Z','$'];
//    Identifiers[I] := I in ['_', '0'..'9', 'a'..'z', 'A'..'Z','$','#','.','@',''''];
  end;
end;


initialization
  MakeIdentTable;
end.

