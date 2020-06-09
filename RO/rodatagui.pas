unit rodatagui;

interface

uses
  Classes, Forms, StdCtrls, ExtCtrls,
  SynEdit,
  rodatatypes,rodata;

// editor
function GetPickedText(mi:TObject=nil; anEdit:TSynEdit=nil):String;

// ComboBox
procedure SetListIndex(cb:TComboBox; atext:AnsiString); overload;
procedure SetListIndex(cb:TComboBox; id:integer; aMode:tRODBMode); overload;
function  GetListIndex(cb:TComboBox; aMode:tRODBMode):integer;
procedure AddListButton(var cb; aMode:tRODBMode);

// Mask (combobox) forms
function CreateMaskForm(aMask:TEdit;
    const anArr:array of tROMask ; const aTitle:String=''):TForm;
function CreateDoubleMaskForm(aMask1,aMask2:TEdit;
    const anArr:array of tROMask2; const aTitle:String=''):TForm;

// forms
function CreateSpriteForm      (TheOwner:TComponent; const aStr:String; aMode:tRODBMode):TForm;
function CreateQuestHelpForm   (TheOwner:TComponent; id:integer):TForm;
// script form
function CreateScriptEditorForm(TheOwner:TComponent; const aStr:String):TForm;
procedure AddScriptButton(var edt);

// diapazone panel
function GetDiapazoneValue(apanel:TPanel; out afrom:integer):integer;
function CreateDiapazonePanel(aOwner:TComponent; aTitle:String):TPanel;

//----- Global variables -----

type
  pROTabUSerData = ^tROTabUSerData;
  tROTabUSerData = record
    FileName:String;
    Editor  :TSynEdit;
  end;
var
  ROFindString   :String = '';
  ROReplaceString:String = '';
var
  ActiveEditor:TSynEdit = nil;


implementation

uses
  SysUtils,
  Controls, Graphics, Menus,
  synhighlighterro,
  roHelpSprites,roHelpTable;

//----- Common functions -----

function MakeMethod(Data, Code:Pointer):TMethod;
begin
  Result.Data:=Data;
  Result.Code:=Code;
end;

procedure CloseFormWithFree(Self:pointer; Sender:TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

//===== public functions =====

//----- Editor -----

function GetPickedText(mi:TObject=nil; anEdit:TSynEdit=nil):String;
var
  pt:TPoint;
begin
  if anEdit=nil then
    anEdit:=ActiveEditor;

  result:=anEdit.SelText;
  if result='' then
  begin
    if mi<>nil then
    begin
      pt:=anEdit.PixelsToLogicalPos(
          anEdit.ScreenToClient(
          ((mi as TMenuItem).GetParentMenu as TPopupMenu).PopupPoint));
    end
    else
    begin
      pt:=anEdit.LogicalCaretXY;
    end;
    result:=anEdit.GetWordAtRowCol(pt);
  end;
end;

//----- Combobox -----

procedure SetListIndex(cb:TComboBox; atext:AnsiString);
var
  i:integer;
begin
  if cb.Style=csDropDownList then
  begin
    for i:=0 to cb.Items.Count-1 do
    begin
      if cb.Items[i]=atext then
      begin
        cb.ItemIndex:=i;
        exit;
      end;
    end;
    cb.ItemIndex:=-1;
  end
  else
    cb.Text:=atext;
end;

procedure SetListIndex(cb:TComboBox; id:integer; aMode:tRODBMode);
var
  i,idx:integer;
begin
  if id=0 then
  begin
    cb.Text:='';
    exit;
  end;
  idx:=RODB_GetIndex(id,aMode);
  for i:=0 to cb.Items.Count-1 do
  begin
    if integer(cb.Items.Objects[i])=idx then
    begin
      cb.ItemIndex:=i;
      exit;
    end;
  end;
  cb.Text:=IntToStr(id);
end;

function GetListIndex(cb:TComboBox;aMode:tRODBMode):integer;
begin
  result:=cb.ItemIndex;
  if result<0 then
  begin
    result:=StrToIntDef(cb.Text,-1);
    if result<0 then
      result:=RODB_GetIndex(cb.Text,aMode)
    else
      result:=RODB_GetIndex(result,aMode);
  end
  else
  begin
    result:=integer(cb.Items.Objects[result]);
  end;
end;

procedure ShowSprites(Self:pointer; Sender: TObject);
var
  aMode:tRODBMode;
  tc:TComponent;
  ChoosedSprite:integer;
begin
  tc:=(Sender as TButton).Owner;

  aMode:=tRODBMode((Sender as TButton).Tag);

  with TROHelpSpritesForm.Create(tc,aMode) do
    ChoosedSprite:=ShowModal;

  if (ChoosedSprite mod 16)=mrOk then
  begin
    ChoosedSprite:=ChoosedSprite shr 4;
    if      tc is TComboBox then (tc as TComboBox).Text:=RODB_GetTitle(ChoosedSprite,aMode)
    else if tc is TEdit     then (tc as TEdit    ).Text:=RODB_GetTitle(ChoosedSprite,aMode);
    (tc as TWinControl).SetFocus;
  end;
end;

procedure ShowList(Self:pointer; Sender:TObject);
var
  aMode:tRODBMode;
  ChoosedItem:integer;
  tc:TComponent;
begin
  tc:=(Sender as TButton).Owner;
  aMode:=tRODBMode((Sender as TButton).Tag);

  with TROHelpTableForm.Create(tc,aMode) do
    ChoosedItem:=ShowModal;

  if (ChoosedItem mod 16)=mrOk then
  begin
    ChoosedItem:=ChoosedItem shr 4;
    if      tc is TComboBox then (tc as TComboBox).Text:=RODB_GetText(ChoosedItem,aMode)
    else if tc is TEdit     then (tc as TEdit    ).Text:=RODB_GetText(ChoosedItem,aMode);
    (tc as TWinControl).SetFocus;
  end;
end;

procedure AddListButton(var cb; aMode:tRODBMode);
var
  ctrl:TControl;
begin
  ctrl:=TControl(cb);
  with TButton.Create(ctrl) do
  begin
    Tag    :=ORD(aMode);
    Parent :=ctrl.Parent;
    Height :=30;
    Width  :=30;
    Caption:='?';
    Font.Style:=[fsBold];

    AnchorSideBottom.Control:=ctrl;
    AnchorSideBottom.Side   :=asrCenter;
    Top :=ctrl.Top-((ctrl.Height-Height) div 2);

    if (ctrl.AnchorSideRight.Control<>nil) and
       (ctrl.AnchorSideRight.Side<>asrCenter) then
    begin
      Anchors:=[akRight,akBottom];
      AnchorSideRight.Control:=ctrl.AnchorSideRight.Control;
      AnchorSideRight.Side   :=asrBottom;
      BorderSpacing.Right:=ctrl.BorderSpacing.Right;
      ctrl.BorderSpacing.Right:=ctrl.BorderSpacing.Right+Width+2;
    end
    else
    begin
      Anchors:=[akLeft,akBottom];
      AnchorSideLeft.Control:=ctrl;
      AnchorSideLeft.Side   :=asrBottom;
      BorderSpacing.Left:=2;

      ctrl.Width:=ctrl.Width-Width-2;
      Left:=ctrl.Left+ctrl.Width+2;
    end;

    if      aMode in [rosmItem,rosmMonster] then OnClick:=TNotifyEvent(MakeMethod(nil,@ShowList))
    else if aMode in [rosmNPC,rosmMap]      then OnClick:=TNotifyEvent(MakeMethod(nil,@ShowSprites));

    Visible:=true;
  end;
end;

//----- Mask Form -----

procedure BtnMaskForm(Self:pointer; Sender:TObject; var CloseAction: TCloseAction);
var
  lForm:TForm;
  res:cardinal;
  i:integer;
begin
  lForm:=TForm((Sender as TButton).Owner);
  if (lForm.Owner as TEdit).Enabled then
  begin
    res:=0;
    for i:=0 to lForm.ControlCount-1 do
    begin
      if lForm.Controls[i] is TCheckBox then
        if TCheckBox(lForm.Controls[i]).Checked then
          res:=res or Cardinal(TCheckBox(lForm.Controls[i]).Tag);
    end;

    (lForm.Owner as TEdit).Text:=IntToHex(res,8);
  end;
  lForm.Close;
end;

function CreateMaskForm(aMask:TEdit; const anArr:array of tROMask; const aTitle:String=''):TForm;
var
  cnt,maxrows,col:integer;
  curcnt,currows:integer;
  i,j,lcol:integer;
  lForm:TForm;
  cb:array of tCheckBox;
  lMask:cardinal;
begin
  cnt:=Length(anArr);
  case cnt of
    1..7 : col:=1;
    8    : col:=2;
    9..12: col:=3;
  else
    col:=4;
  end;
  maxrows:=(cnt+(col-1)) div col;

  SetLength(cb,cnt);
  lForm:=TForm.Create(aMask);
  lForm.OnClose:=TCloseEvent(MakeMethod(nil,@CloseFormWithFree));
  lForm.Caption:=aTitle;

  lMask:=Cardinal(StrToIntDef('$'+aMask.Text,0));
  curcnt:=cnt;
  cnt:=0;
  for i:=0 to col-1 do
  begin
    lcol:=col-i;
    currows:=((curcnt+(lcol-1)) div lcol);
    for j:=0 to currows-1 do
    begin
      cb[cnt]:=TCheckBox.Create(lForm);
      with cb[cnt] do
      begin
        Parent  :=lForm;
        Caption :=anArr[cnt].name;
        Hint    :=anArr[cnt].hint;
        ShowHint:=true;
        Tag     :=PtrInt(anArr[cnt].mask);
        Checked :=(lMask and anArr[cnt].mask)<>0;
        Left    :=6+i*(150+2);
        Top     :=4+j*(1+cb[0].Height);
      end;
      inc(cnt);
    end;
    dec(curcnt,currows);
  end;

  with TButton.Create(lForm) do
  begin
    Parent :=lForm;
    Caption:='&OK';
    Default:=True;
    lForm.Width :=6+col*(2+150+2)+2;
    lForm.Height:=4+maxrows*(1+cb[0].Height+1)+2+Height+4;
    Top    :=lForm.Height-4-Height; // or calc from rows
    Left   :=lForm.Width -2-Width;
    OnClick:=TNotifyEvent(MakeMethod(nil,@BtnMaskForm))
  end;
  lForm.Left:=(Screen.Width -lForm.Width ) div 2;
  lForm.Top :=(Screen.Height-lForm.Height) div 2;

  result:=lForm;
end;

//----- Double Mask Form -----

type
  pMaskData = ^tMaskData;
  tMaskData = record
    mask1:cardinal;
    mask2:cardinal;
  end;

procedure BtnDoubleMaskForm(Self:pointer; Sender:TObject; var CloseAction: TCloseAction);
var
  lForm:TForm;
  pm:pMaskData;
  res1,res2:cardinal;
  i:integer;
begin
  lForm:=TForm((Sender as TButton).Owner);

  res1:=0;
  res2:=0;
  for i:=0 to lForm.ControlCount-1 do
  begin
    if lForm.Controls[i] is TCheckBox then
    begin
      pm:=pMaskData(TCheckBox(lForm.Controls[i]).Tag);
      if TCheckBox(lForm.Controls[i]).Checked then
      begin
        res1:=res1 or pm^.Mask1;
        res2:=res2 or pm^.Mask2;
      end;
      FreeMem(pm);
    end;
  end;

  if (lForm.Owner as TEdit).Enabled then (lForm.Owner as TEdit).Text:=IntToHex(res1,8);
  if TEdit(lForm.Tag      ).Enabled then TEdit(lForm.Tag      ).Text:=IntToHex(res2,8);

  lForm.Close;
end;

function CreateDoubleMaskForm(aMask1,aMask2:TEdit; const anArr:array of tROMask2;
         const aTitle:String=''):TForm;
var
  cnt,maxrows,col:integer;
  curcnt,currows:integer;
  i,j,lcol:integer;
  lForm:TForm;
  pm:pMaskData;
  cb:array of tCheckBox;
  lMask1,lMask2:cardinal;
begin
  cnt:=Length(anArr);
  case cnt of
    1..7 : col:=1;
    8    : col:=2;
    9..12: col:=3;
  else
    col:=4;
  end;
  maxrows:=(cnt+(col-1)) div col;

  SetLength(cb,cnt);
  lForm:=TForm.Create(aMask1);
  lForm.Tag:=UIntPtr (aMask2);
  lForm.OnClose:=TCloseEvent(MakeMethod(nil,@CloseFormWithFree));
  lForm.Caption:=aTitle;

  lMask1:=Cardinal(StrToIntDef('$'+aMask1.Text,0));
  lMask2:=Cardinal(StrToIntDef('$'+aMask2.Text,0));
  curcnt:=cnt;
  cnt:=0;
  for i:=0 to col-1 do
  begin
    lcol:=col-i;
    currows:=((curcnt+(lcol-1)) div lcol);
    for j:=0 to currows-1 do
    begin
      GetMem(pm,SizeOf(tMaskData));
      pm^.mask1:=anArr[cnt].mask1;
      pm^.mask2:=anArr[cnt].mask2;

      cb[cnt]:=TCheckBox.Create(lForm);
      with cb[cnt] do
      begin
        Parent  :=lForm;
        Caption :=anArr[cnt].name;
        Hint    :=anArr[cnt].hint;
        ShowHint:=true;
        Tag     :=UIntPtr(pm);
        Checked :=((lMask1 and pm^.mask1)<>0) and ((lMask2 and pm^.mask2)<>0);
        Left    :=6+i*(150+2);
        Top     :=4+j*(1+cb[0].Height);
      end;
      inc(cnt);
    end;
    dec(curcnt,currows);
  end;

  with TButton.Create(lForm) do
  begin
    Parent :=lForm;
    Caption:='&OK';
    Default:=True;
    lForm.Width :=6+col*(2+150+2)+2;
    lForm.Height:=4+maxrows*(1+cb[0].Height+1)+2+Height+4;
    Top    :=lForm.Height-4-Height; // or calc from rows
    Left   :=lForm.Width -2-Width;
    OnClick:=TNotifyEvent(MakeMethod(nil,@BtnDoubleMaskForm))
  end;
  lForm.Left:=(Screen.Width -lForm.Width ) div 2;
  lForm.Top :=(Screen.Height-lForm.Height) div 2;

  result:=lForm;
end;

//----- Sprite Form -----

function CreateSpriteForm(TheOwner:TComponent; const aStr:String; aMode:tRODBMode):TForm;
var
  lForm:TForm;
  i:integer;
begin
  if aStr<>'' then
  begin
    i:=RODB_GetIndex(aStr,aMode);
    if i>=0 then
    begin
      lForm:=TForm.Create(TheOwner);
      with lForm do
      begin
        OnClose:=TCloseEvent(MakeMethod(nil,@CloseFormWithFree));
        SetBounds(100,100,240,240);
        Constraints.MinWidth :=128;
        Constraints.MinHeight:=128;
        AutoSize:=true;
        Caption:=RODB_GetTitle(i,aMode);
      end;

      with TImage.Create(lForm) do
      begin
        Parent:=lForm;
        Align :=alClient;
        Center:=true;
        try
          Picture.LoadFromFile(RODB_GetSprite(i,aMode));
        except
          lForm.Free;
          exit;
        end;
      end;

      lForm.Show;
      result:=lForm;
    end;
  end;
end;

//----- Script Editor -----

procedure BtnScriptForm(Self:pointer; Sender:TObject; var CloseAction: TCloseAction);
var
  lForm:TForm;
  se:TSynEdit;
  res,res1:String;
begin
  lForm:=TForm((Sender as TButton).Owner);
  if not (lForm.Owner as TEdit).ReadOnly then
  begin
    se:=TSynEdit(lForm.Tag);
    res:=se.Text;

    res:=StringReplace(res,#13#10,' ',[rfReplaceAll]);
    res:=StringReplace(res,#9    ,' ',[rfReplaceAll]);

    repeat
      res1:=res;
      res:=StringReplace(res1,'  ',' ',[rfReplaceAll]);
    until res1=res;

    (lForm.Owner as TEdit).Text:=res;
  end;
  lForm.Close;
end;

function CreateScriptEditorForm(TheOwner:TComponent; const aStr:String):TForm;
var
  lForm:TForm;
  se:TSynEdit;
  lstr,ls:String;
  i,ltab,ldst:integer;
begin
  lForm:=TForm.Create(TheOwner);
  with lForm do
  begin
    OnClose:=TCloseEvent(MakeMethod(nil,@CloseFormWithFree));
    SetBounds(100,100,320,240);
  end;

  with TButton.Create(lForm) do
  begin
    Parent :=lForm;
    Caption:='&OK';
    Default:=True;
    Top    :=lForm.Height-4-Height; // or calc from rows
    Left   :=lForm.Width -2-Width;
    Anchors:=[akRight,akBottom];
    OnClick:=TNotifyEvent(MakeMethod(nil,@BtnScriptForm));
    ldst   :=Top-4;
  end;

  se:=TSynEdit.Create(lForm);
  with se do
  begin
    Parent     :=lForm;
//    Align      :=alClient;
    TabWidth   :=2;
    Highlighter:=SynROSyn;
    Color      :=SynROSyn.SpaceAttri.Background;
    Options    :=Options
        +[eoTabIndent,
          eoTrimTrailingSpaces]
        -[eoTabsToSpaces];
    i:=Keystrokes.FindKeyCode(ord('N'),[ssCtrl]);
    if i>=0 then
      Keystrokes.Items[i].ShortCut:=0;

    Top   :=0;
    Left  :=0;
    Width :=lForm.Width;
    Height:=ldst;
    Anchors:=[akTop,akLeft,akRight,akBottom];
  end;

  lForm.Tag:=IntPtr(se);

(*
  lstr:=aStr;
  lstr:=StringReplace(lstr,'{ ','{'#13#10,[rfReplaceAll]);
  lstr:=StringReplace(lstr,'} ','}'#13#10,[rfReplaceAll]);
  lstr:=StringReplace(lstr,'; ',';'#13#10,[rfReplaceAll]);
*)
  ltab:=0;
  ldst:=1;
  lstr:='';
  ls:='';
  i:=1;
//  while (i<=Length(aStr)) and (aStr[i]=' ') do inc(i);
  while i<=Length(aStr) do
  begin
    if aStr[i]=';' then
    begin
      lstr:=lstr+';'#13#10+ls;
    end
    else if aStr[i]='{' then
    begin
      inc(ltab,2);
      ls:=StringOfChar(' ',ltab);
      lstr:=lstr+'{'#13#10+ls;
    end
    else if aStr[i]='}' then
    begin
      dec(ltab,2);
      if ltab>0 then
        ls:=StringOfChar(' ',ltab)
      else
        ls:='';
      lstr:=lstr+'}'#13#10+ls;
    end
    else
    begin
      lstr:=lstr+aStr[i];
      inc(i);
      continue;
    end;
    inc(i);
    if (i<Length(aStr)) and (aStr[i]=' ') then inc(i);
  end;
  se.Text:=lstr;

  result:=lForm;
end;

procedure ShowEditor(Self:pointer; Sender:TObject);
var
  tc:TComponent;
begin
  tc:=(Sender as TButton).Owner;

  if (tc as TEdit).Visible then
    with CreateScriptEditorForm(tc,(tc as TEdit).Text) do
      ShowModal;
end;

procedure AddScriptButton(var edt);
var
  ctrl:TControl;
begin
  ctrl:=TControl(edt);
  with TButton.Create(ctrl) do
  begin
    Parent :=ctrl.Parent;
    Height :=30;
    Width  :=30;
    Caption:='E';
    Font.Style:=[fsBold];

    AnchorSideBottom.Control:=ctrl;
    AnchorSideBottom.Side   :=asrCenter;
    Top :=ctrl.Top-((ctrl.Height-Height) div 2);

    if (ctrl.AnchorSideRight.Control<>nil) and
       (ctrl.AnchorSideRight.Side<>asrCenter) then
    begin
      Anchors:=[akRight,akBottom];
      AnchorSideRight.Control:=ctrl.AnchorSideRight.Control;
      AnchorSideRight.Side   :=asrBottom;
      BorderSpacing.Right:=ctrl.BorderSpacing.Right;
      ctrl.BorderSpacing.Right:=ctrl.BorderSpacing.Right+Width+2;
    end
    else
    begin
      Anchors:=[akLeft,akBottom];
      AnchorSideLeft.Control:=ctrl;
      AnchorSideLeft.Side   :=asrBottom;
      BorderSpacing.Left:=2;

      ctrl.Width:=ctrl.Width-Width-2;
      Left:=ctrl.Left+ctrl.Width+2;
    end;

    OnClick:=TNotifyEvent(MakeMethod(nil,@ShowEditor));

    Visible:=true;
  end;
end;

//----- Quest Help -----

function CreateQuestHelpForm(TheOwner:TComponent; id:integer):TForm;
var
  lForm:TForm;
  idx:integer;
begin
  lForm:=TForm.Create(TheOwner);
  with lForm do
  begin
    OnClose:=TCloseEvent(MakeMethod(nil,@CloseFormWithFree));
    SetBounds(100,100,320,220);
  end;

  idx:=RODB_GetIndex(id,rosmQuest);

  with TMemo.Create(lForm) do
  begin
    Parent:=lForm;
    SetBounds(8,8,304,128);
    Anchors:=[akLeft,akTop,akRight,akBottom];
    if idx>=0 then
      Lines.Text:=ROQuestData[idx].descr;
  end;

  with TMemo.Create(lForm) do
  begin
    Parent:=lForm;
    SetBounds(8,144,304,70);
    Anchors:=[akLeft,akTop,akRight,akBottom];
    if idx>=0 then
      Lines.Text:=ROQuestData[idx].task;
  end;
 
  result:=lForm;
end;

//----- Diapazone panel -----

function GetDiapazoneValue(apanel:TPanel; out afrom:integer):integer;
begin
  afrom :=-1;
  result:=-1;
  if apanel.Visible then
  begin
    if TEdit(apanel.Controls[0]).Text<>'' then afrom :=StrToInt(TEdit(apanel.Controls[0]).Text);
    if TEdit(apanel.Controls[1]).Text<>'' then result:=StrToInt(TEdit(apanel.Controls[1]).Text);
  end;
end;

procedure CopyValue(Self:pointer; Sender:TObject);
var
  pnl:TPanel;
begin
  pnl:=TPanel((Sender as TButton).Owner);
  (pnl.Controls[1] as TEdit).Text:=(pnl.Controls[0] as TEdit).Text;
end;


function CreateDiapazonePanel(aOwner:TComponent; aTitle:String):TPanel;
begin
  result:=TPanel.Create(aOwner);
  with result do
  begin
    Parent    :=TWinControl(aOwner);
    Height    :=27;
    Width     :=300;
    Anchors   :=[akTop, akLeft, akRight];
    BevelOuter:=bvNone;
  end;

  with TEdit.Create(result) do
  begin
    Parent:=result;
    SetBounds(104,0,80,23);
    Anchors    :=[akTop, akRight];
    AutoSize   :=False;
    NumbersOnly:=True;
  end;
  with TEdit.Create(result) do
  begin
    Parent:=result;
    SetBounds(218,0,80,23);
    Anchors    :=[akTop, akRight];
    AutoSize   :=False;
    NumbersOnly:=True;
  end;
  with TButton.Create(result) do
  begin
    Parent:=result;
    SetBounds(188,0,26,23);
    Anchors:=[akTop, akRight];
    Caption:='->';
    OnClick:=TNotifyEvent(MakeMethod(nil,@CopyValue));
  end;
  with TLabel.Create(result) do
  begin
    Parent:=result;
    SetBounds(64,4,33,15);
    Alignment:=taRightJustify;
    Anchors  :=[akTop, akRight];
//    AutoSize :=False
    Caption  :=aTitle;
  end;
//  result.Visible:=true;
end;

end.
