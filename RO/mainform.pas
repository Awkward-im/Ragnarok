unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, SynEdit,
  Types, SynEditKeyCmds, LCLType, rodatatypes, rodoc;

type
  tSbPanel = (sb_panel0, sb_panel1, sb_panel2, sb_panel3);
  tSbPanels = set of tSbPanel;

type

  { TROMainForm }

  TROMainForm = class(TForm)
    dlgFind   : TFindDialog;
    dlgReplace: TReplaceDialog;

    ilToolButtons: TImageList;
    ilBookmarks  : TImageList;
    mnuWindows: TMenuItem;
    miHelpDictItemSprites: TMenuItem;

    mnuBookmarkGo: TMenuItem;
    miGoBookmark0: TMenuItem;
    miGoBookmark1: TMenuItem;
    miGoBookmark2: TMenuItem;
    miGoBookmark3: TMenuItem;
    miGoBookmark4: TMenuItem;
    miGoBookmark5: TMenuItem;
    miGoBookmark6: TMenuItem;
    miGoBookmark7: TMenuItem;
    miGoBookmark8: TMenuItem;
    miGoBookmark9: TMenuItem;

    mnuBookmarkSet: TMenuItem;
    miSetBookmark0: TMenuItem;
    miSetBookmark1: TMenuItem;
    miSetBookmark2: TMenuItem;
    miSetBookmark3: TMenuItem;
    miSetBookmark4: TMenuItem;
    miSetBookmark5: TMenuItem;
    miSetBookmark6: TMenuItem;
    miSetBookmark7: TMenuItem;
    miSetBookmark8: TMenuItem;
    miSetBookmark9: TMenuItem;

    mnuMain: TMainMenu;

    mnuFile: TMenuItem;
    miFileNew        : TMenuItem;
    miFileOpen       : TMenuItem;
    miFileSave       : TMenuItem;
    miFileSaveAs     : TMenuItem;
    miFileSaveAll    : TMenuItem;
    miFileClose      : TMenuItem;
    miFileCloseAll   : TMenuItem;
    miFileDelimeter1 : TMenuItem;
    miFileReloadColor: TMenuItem;
    miFileDelimeter2 : TMenuItem;
    miFileSettings   : TMenuItem;
    miFileDelimeter0 : TMenuItem;
    miFileExit       : TMenuItem;

    mnuFileLocal: TMenuItem;
    miFileLocalMap : TMenuItem;
    miFileLocalMob : TMenuItem;
    miFileLocalItem: TMenuItem;

    mnuFileExpert: TMenuItem;
    miFileExpertSaveQuest: TMenuItem;
    miFileExpertSaveItems: TMenuItem;
    miFileExpertSaveMobs : TMenuItem;

    mnuEdit: TMenuItem;
    miEditFind      : TMenuItem;
    miEditFindNext  : TMenuItem;
    miEditReplace   : TMenuItem;
    miEditDelimeter1: TMenuItem;
    miEditFoldAll   : TMenuItem;
    miEditUnfoldAll : TMenuItem;

    mnuWizard: TMenuItem;
    miWizardNPC        : TMenuItem;
    miWizardShop       : TMenuItem;
    miWizardCreateQuest: TMenuItem;
    miWizardQuestDBLine: TMenuItem;

    mnuHelp: TMenuItem;
    miHelpMapFlags  : TMenuItem;
    miHelpConstant  : TMenuItem;
    miHelpCommands  : TMenuItem;
    miHelpEffects   : TMenuItem;
    miHelpSkills    : TMenuItem;
    miHelpDelimeter : TMenuItem;
    miHelpATCommands: TMenuItem;
    miHelpDelimeter1: TMenuItem;
    miHelpItemSearch: TMenuItem;
    miHelpMobSearch : TMenuItem;
    miHelpAbout     : TMenuItem;

    mnuHelpDictionary: TMenuItem;
    miHelpDictItemTable : TMenuItem;
    miHelpDictMobTable  : TMenuItem;
    miHelpDictMapTable  : TMenuItem;
    miHelpDictDelimeter : TMenuItem;
    miHelpDictNPCSprites: TMenuItem;
    miHelpDictMapSprites: TMenuItem;
    miHelpDictMobSprites: TMenuItem;
    miHelpDictDelimeter1: TMenuItem;
    miHelpDictQuest     : TMenuItem;

    ROPopupMenu: TPopupMenu;
    miPopupWordHelp     : TMenuItem;
    miPopupDelimeter1   : TMenuItem;
    miPopupShowNPCSprite: TMenuItem;
    miPopupShowMapSprite: TMenuItem;
    miPopupShowItemCard : TMenuItem;
    miPopupShowMobCard  : TMenuItem;
    miPopupDelimeter0   : TMenuItem;
    miPopupQuestDB      : TMenuItem;

    TabPopupMenu: TPopupMenu;
    miTabPopupNew  : TMenuItem;
    miTabPopupClose: TMenuItem;

    sbMain: TStatusBar;
    pcMain: TPageControl;

    tbMain: TToolBar;
    tbFileNew    : TToolButton;
    tbFileOpen   : TToolButton;
    tbFileSave   : TToolButton;
    tbDelimeter1 : TToolButton;
    tbEditFind   : TToolButton;
    tbEditReplace: TToolButton;
    tbDelimeter2 : TToolButton;
    tbHelpDoc    : TToolButton;
    tbDelimeter3 : TToolButton;
    tbFoldAll    : TToolButton;
    tbUnfoldAll  : TToolButton;
    tbSettings: TToolButton;

    procedure dlgFindFind(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);

    procedure miEditReplaceClick(Sender: TObject);
    procedure miEditFoldAllClick(Sender: TObject);
    procedure miEditUnfoldAllClick(Sender: TObject);
    procedure miEditFindClick(Sender: TObject);
    procedure miEditFindNextClick(Sender: TObject);
    procedure miFileCloseAllClick(Sender: TObject);
    procedure miFileCloseClick(Sender: TObject);
    procedure miFileExpertSaveItemsClick(Sender: TObject);
    procedure miFileExpertSaveMobsClick(Sender: TObject);
    procedure miFileExpertSaveQuestClick(Sender: TObject);
    procedure miFileLocalItemClick(Sender: TObject);
    procedure miFileLocalMapClick(Sender: TObject);
    procedure miFileLocalMobClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileSaveAllClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileSettingsClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileReloadColorClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miGoBookmarkClick(Sender: TObject);
    procedure miHelpATCommandsClick(Sender: TObject);
    procedure miHelpConstantClick(Sender: TObject);
    procedure miHelpCommandsClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);
    procedure miHelpDictItemSpritesClick(Sender: TObject);
    procedure miHelpDictItemTableClick(Sender: TObject);
    procedure miHelpDictMapSpritesClick(Sender: TObject);
    procedure miHelpDictMapTableClick(Sender: TObject);
    procedure miHelpDictMobSpritesClick(Sender: TObject);
    procedure miHelpDictMobTableClick(Sender: TObject);
    procedure miHelpDictNPCSpritesClick(Sender: TObject);
    procedure miHelpDictQuestClick(Sender: TObject);
    procedure miHelpEffectsClick(Sender: TObject);
    procedure miHelpItemSearchClick(Sender: TObject);
    procedure miHelpMapFlagsClick(Sender: TObject);
    procedure miHelpMobSearchClick(Sender: TObject);
    procedure miHelpSkillsClick(Sender: TObject);
    procedure miPopupQuestDBClick(Sender: TObject);
    procedure miPopupShowItemCardClick(Sender: TObject);
    procedure miPopupShowMobCardClick(Sender: TObject);
    procedure miPopupWordHelpClick(Sender: TObject);
    procedure miPopupShowNPCSpriteClick(Sender: TObject);
    procedure miPopupShowMapSpriteClick(Sender: TObject);
    procedure miSetBookmarkClick(Sender: TObject);
    procedure miTabPopupCloseClick(Sender: TObject);
    procedure miTabPopupNewClick(Sender: TObject);
    procedure miWizardCreateQuestClick(Sender: TObject);
    procedure miWizardNPCClick(Sender: TObject);
    procedure miWizardQuestDBLineClick(Sender: TObject);
    procedure miWizardShopClick(Sender: TObject);
    procedure mnuWindowsClick(Sender: TObject);

    procedure pcMainChange(Sender: TObject);
    procedure PageEnter(Sender: TObject);
    procedure TabPopupMenuPopup(Sender: TObject);
  private
    RODictItemForm  :TForm;
    RODictMobsForm  :TForm;
    RODictMapForm   :TForm;
    RODictSkillForm :TForm;
    RODictEffectForm:TForm;
    RODictQuestForm :TForm;
    ROHelpDocForm:TForm;
    ROMapFlagForm:TForm;
    ROATCommandsForm:TForm;
    ROCmdDoc:tRODocument;

    procedure UpdateStatusBar(apanels:tSbPanels);
    procedure DoFindReplace(const aFindStr:String; const aReplaceStr:string;
         anOptions:TFindOptions);
    function CreateNewPage:integer;
    function ClosePage(i:integer):boolean;
    function GetPageEditor(idx:integer):TSynEdit;
    procedure ChangePageCaption(lpage:TTabSheet);
    procedure ImportScheme(const aFileIn,aFileOut:AnsiString);
    procedure ActivateMIWindow(Sender: TObject);
  public

  end;

var
  ROMainForm: TROMainForm;


implementation

uses
  LazUTF8,
  synhighlighterro, synedittypes,
  ROFileSettings, ROFullCardMob, ROFullCardItem,
  ROWizNPCBase, ROWizShop, ROWizCreateQuest, ROWizQuestDBLine,
  ROHelpDoc, roHelpTable, ROHelpConst, ROHelpSprites, ROHelpCardMob,
  ROSearchItem, ROSearchMob,
  cmemini,
  rodatalocal, rodataio, rodatagui;

{$R *.lfm}

resourcestring
  sCardLabelMapflag = 'Map flags';
  sCardLabelCommand = 'Commands / Keywords';
  sCardLabelATCmd   = 'AT Commands';
  sNewFile          = 'New file';
  sOpenScript       = 'Open script file';
  sSaveScript       = 'Save Script file as';
  sOpenScheme       = 'Open editor settings';
  sSaveOnClose      = 'Modified file not saved. Save?';
  sDataOrderChanged = 'Settings related to load Item and monster data was changed.'#13#10+
                      'Reload Item and Monster data right now?';
  sModified         = 'Modified';
  sInsertMode       = 'INS';
  sOverwriteMode    = 'OVR';
  sMobSaving        = 'Saving Monster info';
  sQuestSaving      = 'Saving Quest info';
  sQuestHelpSaving  = 'Saving Quest help info';
  sItemSaving       = 'Saving Item info';
  sItemTradeSaving  = 'Saving Item trade info';
  sFoundUnsaved     = 'Found but not saved records - ';
  sFoundSaved       = 'Found and saved records - ';
  sNotFoundSaved    = 'Nothing found to save';


const
  DefaultScriptExt = '.txt';
  ScriptFilter     = 'Script files|*.txt';
  DefaultSchemeExt = '.ini';
  SchemeFilter     = 'INI files|*.ini';

{ TROMainForm }

procedure TROMainForm.FormCreate(Sender: TObject);
var
  s:AnsiString;
  i:integer;
begin
  LoadSettings;

  ROCmdDoc.Init;

  ReadMaps;

  ReadItems;

  ReadMobs;
  ReadNPC;
  ReadLUANames;

  if ROAutoLoadLocal then
  begin
    if LoadMapLocal  then miFileLocalMap .Checked:=true;
    if LoadMobLocal  then miFileLocalMob .Checked:=true;
    if LoadItemLocal then miFileLocalItem.Checked:=true;
  end;

  SynROSyn:=TSynROSyn.Create(Self);
  SynROSyn.CaseSensitive:=false;
  SynROSyn.LoadHighLighter(defSettingsFile,ROColorScheme);

  SynROSyn.Constants.Clear;
  if ReadConstants then
    for i:=0 to ROConstValues.Count-1 do
      SynROSyn.Constants.Add(PAnsiChar(ROConstValues[i]^));

  SynROSyn.KeyWords.Clear;

  if RODataDirectory='' then
    s:=defDataDirectory+defROKeywordFileName
  else
    s:=RODataDirectory+DefDocDir+defROKeywordFileName;

  if ROCmdDoc.Load(s) then
  begin
    for i:=0 to ROAddKeyCount-1 do
      SynROSyn.KeyWords.Add(RoAddKeyNames[i]);

    for i:=0 to ROCmdDoc.CardCount-1 do
    begin
      SynROSyn.KeyWords.Add(ROCmdDoc.Cards[i]);
    end;
  end;

  CreateNewPage;
  ActiveEditor:=GetPageEditor(pcMain.ActivePageIndex);

end;

procedure TROMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i:integer;
begin
  CanClose:=true;

  for i:=pcMain.PageCount-1 downto 0 do
    CanClose:=CanClose and ClosePage(i);

  for i:=0 to Self.ComponentCount-1 do
    if (Self.Components[i] is TForm) then
       (Self.Components[i] as TForm).Close;

  if CanClose then
  begin
    ROCmdDoc.Free;
    FreeConstants;
  end;
end;

procedure TROMainForm.DoFindReplace(const aFindStr:String; const aReplaceStr:string;
     anOptions:TFindOptions);
var
  lOptions: TSynSearchOptions;
begin
  {  (
      ssoSelectedOnly,
      ssoSearchInReplacement,    // continue search-replace in replacement (with ssoReplaceAll) // replace recursive
      ssoRegExpr, ssoRegExprMultiLine,
    );
  }
  lOptions:=[];
  if frMatchCase       in anOptions  then lOptions:=lOptions+[ssoMatchCase];
  if frWholeWord       in anOptions  then lOptions:=lOptions+[ssoWholeWord];
  if frEntireScope     in anOptions  then lOptions:=lOptions+[ssoEntireScope];
  if frReplace         in anOptions  then lOptions:=lOptions+[ssoReplace];
  if frReplaceAll      in anOptions  then lOptions:=lOptions+[ssoReplaceAll];
  if frPromptOnReplace in anOptions  then lOptions:=lOptions+[ssoPrompt];
  if frFindNext        in anOptions  then lOptions:=lOptions+[ssoFindContinue];
  if not (frDown       in anOptions) then lOptions:=lOptions+[ssoBackwards];
  ActiveEditor.SearchReplace(aFindStr,aReplaceStr,lOptions);
end;

procedure TROMainForm.dlgFindFind(Sender: TObject);
begin
  if Sender=dlgFind then
    doFindReplace(dlgFind.FindText,'',dlgFind.Options)
  else if Sender=dlgReplace then
    doFindReplace(dlgReplace.FindText,dlgReplace.ReplaceText,dlgReplace.Options);
end;

//----- File menu -----

procedure TROMainForm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TROMainForm.miFileNewClick(Sender: TObject);
var
  i:integer;
begin
  i:=CreateNewPage;
  pcMain.PageIndex:=i;
  ActiveEditor:=GetPageEditor(i);
  ActiveEditor.SetFocus;
  UpdateStatusBar([sb_panel0,sb_panel1,sb_panel2,sb_panel3]);
end;

procedure TROMainForm.miFileCloseClick(Sender: TObject);
begin
  if pcMain.PageCount>1 then
    ClosePage(pcMain.ActivePageIndex);
end;

procedure TROMainForm.miFileCloseAllClick(Sender: TObject);
var
  i:integer;
begin
  for i:=pcMain.PageCount-1 downto 1 do
    ClosePage(i);
end;

procedure TROMainForm.miFileOpenClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  se:TSynEdit;
  sl:TStringList;
  i,j:integer;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
//    OpenDialog.InitialDir:=ExtractFilePath(AnUnitInfo.Filename);
    OpenDialog.DefaultExt:=DefaultScriptExt;
    OpenDialog.Filter    :=ScriptFilter;
    OpenDialog.Title     :=sOpenScript;
    OpenDialog.Options   :=OpenDialog.Options+[
      ofAllowMultiSelect,
      ofNoResolveLinks
    ];
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then
    begin
      for i:=0 to OpenDialog.Files.Count-1 do
      begin
        j:=CreateNewPage();
        se:=GetPageEditor(j);

        if ROFilesAreACP then
        begin
          sl:=TStringList.Create;
          sl.LoadFromFile(OpenDialog.Files.Strings[i]);
          se.Text:=WinCPToUTF8(sl.Text);
          sl.Free;
        end
        else
          se.Lines.LoadFromFile(OpenDialog.Files.Strings[i]);

        se.Modified:=false;
        pROTabUserData(
          pcMain.Pages[j].Tag)^.FileName:=         OpenDialog.Files.Strings[i];
          pcMain.Pages[j].Caption:=ExtractFileName(OpenDialog.Files.Strings[i]);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TROMainForm.miFileSaveAsClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
//  sl:TStringList;
  f:file of byte;
  s:AnsiString;
begin
  SaveDialog:=TSaveDialog.Create(nil);
  try
    SaveDialog.DefaultExt:=DefaultScriptExt;
    SaveDialog.Filter    :=ScriptFilter;
    SaveDialog.Title     :=sSaveScript;
    SaveDialog.Options   :=SaveDialog.Options+[ofOverwritePrompt];

    if (not SaveDialog.Execute) or (ExtractFileName(SaveDialog.Filename)='') then
      exit;

    if ROFilesAreACP then
    begin
      assignfile(f,SaveDialog.FileName);
      rewrite(f);
      s:=UTF8ToWinCP(ActiveEditor.Text);
      blockWrite(f,pAnsiChar(s)^,Length(s));
      closefile(f);
{
      sl:=TStringList.Create;
      sl.Text:=UTF8ToWinCP(ActiveEditor.Text);
      sl.SaveToFile(SaveDialog.FileName);
      sl.Free;
}    end
    else
      ActiveEditor.Lines.SaveToFile(SaveDialog.FileName);

    ActiveEditor.Modified:=false;

    pcMain.ActivePage.Caption:=ExtractFileName(SaveDialog.FileName);
    pROTabUSerData(pcMain.ActivePage.Tag)^.FileName:=SaveDialog.FileName;
    UpdateStatusBar([sb_panel3]);
  finally
    SaveDialog.Free;
  end;
end;

procedure TROMainForm.miFileSaveClick(Sender: TObject);
var
  lfname:String;
//  sl:TStringList;
  f:file of byte;
  s:AnsiString;
begin
  lfname:=pROTabUSerData(pcMain.ActivePage.Tag)^.FileName;
  if lfname='' then
  begin
    miFileSaveAsClick(Sender);
  end
  else
  begin
    if ROFilesAreACP then
    begin
      assignfile(f,lfname);
      rewrite(f);
      s:=UTF8ToWinCP(ActiveEditor.Text);
      blockWrite(f,pAnsiChar(s)^,Length(s));
      closefile(f);
{
      sl:=TStringList.Create;
      sl.Text:=UTF8ToWinCP(ActiveEditor.Text);
      sl.SaveToFile(lfname);
      sl.Free;
}
    end
    else
      ActiveEditor.Lines.SaveToFile(lfname);
    ActiveEditor.Modified:=false;
  end;
end;

procedure TROMainForm.miFileSaveAllClick(Sender: TObject);
var
  i:integer;
begin
  for i:=0 to pcMain.PageCount-1 do
  begin
    if GetPageEditor(i).Modified then
    begin
      pcMain.PageIndex:=i;
      ActiveEditor:=GetPageEditor(i);
      miFileSaveClick(Sender);
    end;
  end;
end;

procedure TROMainForm.miFileReloadColorClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  i:integer;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    OpenDialog.Title     :=sOpenScheme;
    OpenDialog.DefaultExt:=DefaultSchemeExt;
    OpenDialog.Filter    :=SchemeFilter;
    if OpenDialog.Execute then
    begin
      // right now, Scheme='' means old style format
      SynROSyn.LoadHighLighter(OpenDialog.FileName,'');
      for i:=0 to pcMain.PageCount-1 do
      begin
        GetPageEditor(i).Color:=SynROSyn.SpaceAttri.Background;
      end;
      //!! Search file name / scheme in saved and add (with rename if needs)
      // Scheme = filename or filename_postfix OR from [Scheme:<name>]
      if ROAutoAddScheme then
        ImportScheme(OpenDialog.FileName,defSettingsFile);
    end;
  finally
    OpenDialog.Free;
  end;

end;

procedure TROMainForm.miFileSettingsClick(Sender: TObject);
var
  res,i:integer;
begin
  with TROSettingsForm.Create(Self) do
  begin
    res:=ShowModal;
    if (res mod 16)=mrOk then
    begin
      case res shr 8 of
        1,2: begin
          if MessageDlg(sDataOrderChanged,mtWarning,[mbOk,mbCancel],0)=mrOk then
          begin
            if RODictItemForm<>nil then
            begin
              RODictItemForm.Free;
              RODictItemForm:=nil;
            end;
            ReadItems;
            if RODictMobsForm<>nil then
            begin
              RODictMobsForm.Free;
              RODictMobsForm:=nil;
            end;
            ReadMobs;
            if ROAutoLoadLocal then
            begin
              if LoadMobLocal  then miFileLocalMob .Checked:=true;
              if LoadItemLocal then miFileLocalItem.Checked:=true;
            end;
          end;
        end;
        4: begin
          for i:=0 to pcMain.PageCount-1 do
          begin
            GetPageEditor(i).Color:=SynROSyn.SpaceAttri.Background;
          end;
        end;
      end;

    end;
  end;
end;

//----- Expert -----

procedure TROMainForm.miFileExpertSaveItemsClick(Sender: TObject);
var
  i:integer;
begin
  i:=UpdateItemList();
  if i<0 then
    MessageDlg(sItemSaving,sFoundUnsaved+IntToStr(i),mtInformation,[mbOk],0)
  else if i>0 then
    MessageDlg(sItemSaving,sFoundSaved+IntToStr(i),mtInformation,[mbOk],0)
  else
    MessageDlg(sItemSaving,sNotFoundSaved,mtInformation,[mbOk],0);
  i:=UpdateItemTradeList();
  if i<0 then
    MessageDlg(sItemTradeSaving,sFoundUnsaved+IntToStr(i),mtInformation,[mbOk],0)
  else if i>0 then
    MessageDlg(sItemTradeSaving,sFoundSaved+IntToStr(i),mtInformation,[mbOk],0)
  else
    MessageDlg(sItemTradeSaving,sNotFoundSaved,mtInformation,[mbOk],0);
end;

procedure TROMainForm.miFileExpertSaveMobsClick(Sender: TObject);
var
  i:integer;
begin
  i:=UpdateMonsterList();
  if i<0 then
    MessageDlg(sMobSaving,sFoundUnsaved+IntToStr(i),mtInformation,[mbOk],0)
  else if i>0 then
    MessageDlg(sMobSaving,sFoundSaved+IntToStr(i),mtInformation,[mbOk],0)
  else
    MessageDlg(sMobSaving,sNotFoundSaved,mtInformation,[mbOk],0);
end;

procedure TROMainForm.miFileExpertSaveQuestClick(Sender: TObject);
var
  i:integer;
begin
  i:=UpdateQuestList();
  if i<0 then
    MessageDlg(sQuestSaving,sFoundUnsaved+IntToStr(i),mtInformation,[mbOk],0)
  else if i>0 then
    MessageDlg(sQuestSaving,sFoundSaved+IntToStr(i),mtInformation,[mbOk],0)
  else
    MessageDlg(sQuestSaving,sNotFoundSaved,mtInformation,[mbOk],0);
  i:=UpdateQuestHelpList();
  if i<0 then
    MessageDlg(sQuestHelpSaving,sFoundUnsaved+IntToStr(i),mtInformation,[mbOk],0)
  else if i>0 then
    MessageDlg(sQuestHelpSaving,sFoundSaved+IntToStr(i),mtInformation,[mbOk],0)
  else
    MessageDlg(sQuestHelpSaving,sNotFoundSaved,mtInformation,[mbOk],0);
end;

//----- Edit menu -----

procedure TROMainForm.miEditFindClick(Sender: TObject);
begin
  if dlgReplace.FindText<>'' then
  begin
    dlgFind.FindText:=dlgReplace.FindText;
    dlgFind.Options :=dlgReplace.Options-[frReplace,frReplaceAll];
    dlgReplace.FindText:='';
    dlgReplace.Options :=[frDown,frReplace,frReplaceAll];
  end;
  dlgFind.Execute;
end;

procedure TROMainForm.miEditFindNextClick(Sender: TObject);
begin
  if dlgReplace.FindText<>'' then
  begin
    dlgReplace.Options:=dlgReplace.Options+[frFindNext];
    dlgFindFind(dlgReplace);
  end
  else if dlgFind.FindText<>'' then
  begin
    dlgFind.Options:=dlgFind.Options+[frFindNext];
    dlgFindFind(dlgFind)
  end
  else dlgFind.Execute;
end;

procedure TROMainForm.miEditReplaceClick(Sender: TObject);
begin
  if dlgFind.FindText<>'' then
  begin
    dlgReplace.FindText:=dlgFind.FindText;
    dlgReplace.Options :=dlgFind.Options+[frReplace,frReplaceAll];
    dlgFind.FindText:='';
    dlgFind.Options :=[frDown];
  end;
  dlgReplace.Execute;
end;

procedure TROMainForm.miEditFoldAllClick(Sender: TObject);
begin
  ActiveEditor.FoldAll;
end;

procedure TROMainForm.miEditUnfoldAllClick(Sender: TObject);
begin
  ActiveEditor.UnFoldAll;
end;

//----- Wizards menu -----

procedure TROMainForm.miWizardCreateQuestClick(Sender: TObject);
begin
  with TNPCCreateForm.Create(Self) do ShowModal;
end;

procedure TROMainForm.miWizardNPCClick(Sender: TObject);
begin
  with TNPCBase.Create(Self,true) do ShowModal;
end;

procedure TROMainForm.miWizardQuestDBLineClick(Sender: TObject);
begin
  with TQuestDBLineForm.Create(Self,'') do ShowModal;
end;

procedure TROMainForm.miWizardShopClick(Sender: TObject);
begin
  with TNPCShopForm.Create(Self) do ShowModal;
end;

//----- Documentation -----

procedure TROMainForm.miHelpATCommandsClick(Sender: TObject);
var
  s:AnsiString;
  lRODoc:pRODocument;
begin
  if ROATCommandsForm=nil then
  begin
    GetMem(lRODoc,SizeOf(tRODocument));
    lRODoc^.Init;
    if RODataDirectory='' then
      s:=defDataDirectory+defATCommandsFileName
    else
      s:=RODataDirectory+DefDocDir+defATCommandsFileName;
    if lRODoc^.Load(s) then
    begin
      ROATCommandsForm:=TROHelpDocForm.Create(Self,lRODoc^,sCardLabelATCmd,true);
      ROATCommandsForm.Show;
    end
    else
      FreeMem(lRODoc);
  end
  else
  begin
    ROATCommandsForm.Show;
    ROATCommandsForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpCommandsClick(Sender: TObject);
begin
  if ROHelpDocForm=nil then
  begin
    ROHelpDocForm:=TROHelpDocForm.Create(Self,ROCmdDoc,sCardLabelCommand,false);
    ROHelpDocForm.Show;
  end
  else
  begin
    ROHelpDocForm.Show;
    ROHelpDocForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpMapFlagsClick(Sender: TObject);
var
  s:AnsiString;
  lRODoc:pRODocument;
begin
  if ROMapFlagForm=nil then
  begin
    GetMem(lRODoc,SizeOf(tRODocument));
    lRODoc^.Init;
    if RODataDirectory='' then
      s:=defDataDirectory+defMapFlagFileName
    else
      s:=RODataDirectory+DefDocDir+defMapFlagFileName;
    if lRODoc^.Load(s) then
    begin
      ROMapFlagForm:=TROHelpDocForm.Create(Self,lRODoc^,sCardLabelMapflag,true);
      ROMapFlagForm.Show;
    end
    else
      FreeMem(lRODoc);
  end
  else
  begin
    ROMapFlagForm.Show;
    ROMapFlagForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpSkillsClick(Sender: TObject);
begin
  if RODictSkillForm=nil then
  begin
    if Length(ROSkillData)=0 then ReadSkills;
    RODictSkillForm:=TROHelpTableForm.Create(Self,rosmSkill);
    RODictSkillForm.Show;
  end
  else
  begin
    RODictSkillForm.Show;
    RODictSkillForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpEffectsClick(Sender: TObject);
begin
  if RODictEffectForm=nil then
  begin
    if Length(ROEffectData)=0 then ReadEffect;
    RODictEffectForm:=TROHelpTableForm.Create(Self,rosmEffect);
    RODictEffectForm.Show;
  end
  else
  begin
    RODictEffectForm.Show;
    RODictEffectForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpItemSearchClick(Sender: TObject);
begin
  with tROSearchItemForm.Create(Self) do ShowModal;
end;

procedure TROMainForm.miHelpMobSearchClick(Sender: TObject);
begin
  with tROSearchMobForm.Create(Self) do ShowModal;
end;

procedure TROMainForm.miHelpConstantClick(Sender: TObject);
begin
  if RODictConstForm=nil then
  begin
    RODictConstForm:=TRODictConstForm.Create(Self);
    RODictConstForm.Show;
  end
  else
  begin
    RODictConstForm.Show;
    RODictConstForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpDictItemTableClick(Sender: TObject);
begin
  if RODictItemForm=nil then
  begin
    RODictItemForm:=TROHelpTableForm.Create(Self,rosmItem);
    RODictItemForm.Show;
  end
  else
  begin
    RODictItemForm.Show;
    RODictItemForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpDictMobTableClick(Sender: TObject);
begin
  if RODictMobsForm=nil then
  begin
    RODictMobsForm:=TROHelpTableForm.Create(Self,rosmMonster);
    RODictMobsForm.Show;
  end
  else
  begin
    RODictMobsForm.Show;
    RODictMobsForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpDictMapTableClick(Sender: TObject);
begin
  if RODictMapForm=nil then
  begin
    RODictMapForm:=TROHelpTableForm.Create(Self,rosmMap);
    RODictMapForm.Show;
  end
  else
  begin
    RODictMapForm.Show;
    RODictMapForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpDictMapSpritesClick(Sender: TObject);
begin
  with TROHelpSpritesForm.Create(Self,rosmMap) do Show;
end;

procedure TROMainForm.miHelpDictNPCSpritesClick(Sender: TObject);
begin
  with TROHelpSpritesForm.Create(Self,rosmNPC) do Show;
end;

procedure TROMainForm.miHelpDictMobSpritesClick(Sender: TObject);
begin
  with TROHelpSpritesForm.Create(Self,rosmMonster) do Show;
end;

procedure TROMainForm.miHelpDictItemSpritesClick(Sender: TObject);
begin
  with TROHelpSpritesForm.Create(Self,rosmItem) do Show;
end;

procedure TROMainForm.miHelpDictQuestClick(Sender: TObject);
begin
  if RODictQuestForm=nil then
  begin
    if Length(ROQuestData)=0 then
      if ReadQuests then
        ReadQuestHelp;
    RODictQuestForm:=TROHelpTableForm.Create(Self,rosmQuest);
    RODictQuestForm.Show;
  end
  else
  begin
    RODictQuestForm.Show;
    RODictQuestForm.BringToFront;
  end;
end;

procedure TROMainForm.miHelpAboutClick(Sender: TObject);
begin

end;

//----- Popup Menu -----

procedure TROMainForm.miPopupWordHelpClick(Sender: TObject);
var
  ls:String;
begin
  ls:=GetPickedText(Sender);
  if ls<>'' then
  begin
    // show doc window
    miHelpCommandsClick(Sender);
    tROHelpDocForm(ROHelpDocForm).SelectWord(ls);
  end;
end;

procedure TROMainForm.miPopupQuestDBClick(Sender: TObject);
var
  s:String;
begin
  s:=ActiveEditor.LineText;
  with TQuestDBLineForm.Create(Self,s) do
    ShowModal;
end;

procedure TROMainForm.miPopupShowNPCSpriteClick(Sender: TObject);
begin
  CreateSpriteForm(Self,GetPickedText(Sender),rosmNPC);
end;

procedure TROMainForm.miPopupShowMapSpriteClick(Sender: TObject);
begin
  CreateSpriteForm(Self,GetPickedText(Sender),rosmMap);
end;

procedure TROMainForm.miPopupShowItemCardClick(Sender: TObject);
var
  ls:String;
  i:integer;
begin
  ls:=GetPickedText(Sender);
  if ls<>'' then
  begin
    i:=StrToIntDef(ls,-1);
    if i<0 then
      i:=RODB_GetIndex(ls,rosmItem)
    else
      i:=RODB_GetIndex(i,rosmItem);
    if i>=0 then
    begin
      with TROFullCardItemForm.Create(Self,i) do Show
    end;
  end;
end;

procedure TROMainForm.miPopupShowMobCardClick(Sender: TObject);
var
  ls:String;
  i:integer;
begin
  ls:=GetPickedText(Sender);
  if ls<>'' then
  begin
    i:=StrToIntDef(ls,-1);
    if i<0 then
      i:=RODB_GetIndex(ls,rosmMonster)
    else
      i:=RODB_GetIndex(i,rosmMonster);
    if i>=0 then
    begin
      if ROShowFullCard then
        with tROFullCardMobForm.Create(Self,i) do Show
      else
        with tROHelpCardMobForm.Create(Self,i) do Show;
    end;
  end;
end;

//----- Localization -----

procedure TROMainForm.miFileLocalMapClick(Sender: TObject);
begin
  if miFileLocalMap.Checked then
  begin
    UnloadMapLocal;
    miFileLocalMap.Checked:=false;
  end
  else if LoadMapLocal then
  begin
    miFileLocalMap.Checked:=true;
  end;
end;

procedure TROMainForm.miFileLocalItemClick(Sender: TObject);
begin
  if miFileLocalItem.Checked then
  begin
    UnloadItemLocal;
    miFileLocalItem.Checked:=false;
  end
  else if LoadItemLocal then
  begin
    miFileLocalItem.Checked:=true;
  end;
end;

procedure TROMainForm.miFileLocalMobClick(Sender: TObject);
begin
  if miFileLocalMob.Checked then
  begin
    UnloadMobLocal;
    miFileLocalMob.Checked:=false;
  end
  else if LoadMobLocal then
  begin
    miFileLocalMob.Checked:=true;
  end;
end;

//----- Tab Popup Menu -----

procedure TROMainForm.miTabPopupCloseClick(Sender: TObject);
var
  m:tpopupmenu;
  mi:tmenuitem;
  pt:tpoint;
  i:integer;
begin
  mi:=Sender as TMenuItem;
  m :=mi.GetParentMenu as TPopupMenu; // =TabPopupMenu
  pt:=pcMain.ScreenToClient(m.PopupPoint);
  i :=pcMain.IndexOfPageAt(pt);
  ClosePage(i);
end;

procedure TROMainForm.miTabPopupNewClick(Sender: TObject);
var
  i:integer;
begin
  i:=CreateNewPage;
  pcMain.PageIndex:=i;
  ActiveEditor:=GetPageEditor(i);
end;

procedure TROMainForm.TabPopupMenuPopup(Sender: TObject);
begin
  miTabPopupClose.Enabled:=pcMain.PageCount>1;
end;

//----- Bookmarks -----

procedure TROMainForm.miSetBookmarkClick(Sender: TObject);
var
  pt:TPoint;
//  num:integer;
begin
//  num:=Length((Sender as TMenuItem).Name);
//  num:=ord((Sender as TMenuItem).Name[num])-ord('0');
  pt:=ActiveEditor.PixelsToLogicalPos(
            ActiveEditor.ScreenToClient(ROPopupMenu.PopupPoint)
            );
  ActiveEditor.SetBookMark((Sender as TMenuItem).ImageIndex{num},1,pt.Y);
end;

procedure TROMainForm.miGoBookmarkClick(Sender: TObject);
//var
//  num:integer;
begin
//  num:=Length((Sender as TMenuItem).Name);
//  num:=ord((Sender as TMenuItem).Name[num])-ord('0');
  ActiveEditor.GotoBookMark((Sender as TMenuItem).ImageIndex{num});
end;

//----- Editor -----

procedure TROMainForm.UpdateStatusBar(apanels:tSbPanels);
var
  ltext:String;
  pt:TPoint;
begin
  if (apanels * [sb_panel0])<>[] then
  begin
    pt:=ActiveEditor.{Logical}CaretXY;
    ltext:=IntToStr(pt.Y)+' : '+IntToStr(pt.X);
    sbMain.Panels[0].Text:=ltext;
  end;

  if (apanels * [sb_panel1])<>[] then
  begin
    if ActiveEditor.Modified then
      ltext:=sModified
    else
      ltext:='';
    sbMain.Panels[1].Text:=ltext;
  end;

  if (apanels * [sb_panel2])<>[] then
  begin
    if ActiveEditor.InsertMode then
      ltext:=sInsertMode
    else
      ltext:=sOverwriteMode;
    sbMain.Panels[2].Text:=ltext;
  end;

  if (apanels * [sb_panel3])<>[] then
  begin
    sbMain.Panels[3].Text:=pROTabUserData(pcMain.ActivePage.Tag)^.FileName;
  end;
end;

procedure TROMainForm.EditorProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if (Command>=ecUserDefinedFirst) and (Command<=ecUserDefinedLast) then
  begin
    case Command-ecUserDefinedFirst of
      0: begin
        miPopupWordHelpClick(nil);
      end;
    end;
  end
  else
    inherited;
end;

procedure TROMainForm.ChangePageCaption(lpage:TTabSheet);
var
  UserData:pROTabUserData;
  lname:String;
begin
  UserData:=pROTabUserData(lpage.Tag);
  if UserData^.FileName='' then
    lname:=sNewFile
  else
    lname:=ExtractFileName(UserData^.FileName);
  if UserData^.Editor.Modified then
    lname:='*'+lname;
  lpage.Caption:=lname;
end;

procedure TROMainForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  i:integer;
begin
//  if Sender<>ActiveEditor then application.Messagebox('ough','');
  if (Changes * [scCaretX,scCaretY])<>[] then
    UpdateStatusBar([sb_panel0]);

  if (Changes * [scInsertMode])<>[] then
    UpdateStatusBar([sb_panel2]);

  if (Changes * [scModified])<>[] then
  begin
    UpdateStatusBar([sb_panel1]);
    //really, must be ActiveEditor
    for i:=0 to pcMain.PageCount-1 do
    begin
      if GetPageEditor(i)=Sender then
      begin
        ChangePageCaption(pcMain.Pages[i]);
        break;
      end;
    end;
  end;
end;

//----- Tab Control -----

procedure TROMainForm.pcMainChange(Sender: TObject);
begin
  ActiveEditor:=GetPageEditor(pcMain.ActivePageIndex);
  ActiveEditor.SetFocus;
  UpdateStatusBar([sb_panel0,sb_panel1,sb_panel2,sb_panel3]);
end;

procedure TROMainForm.PageEnter(Sender: TObject);
begin
  if Visible then
    GetPageEditor((Sender as TTabSheet).PageIndex).SetFocus;
end;

function TROMainForm.ClosePage(i:integer):boolean;
var
  ltab:TTabSheet;
  led:TSynEdit;
begin
  result:=false;
  led:=GetPageEditor(i);

  if led.Modified then
  begin
    case MessageDlg(sSaveOnClose,mtInformation,[mbYes,mbNo,mbCancel],0) of
      mrYes   : miFileSaveClick(nil);
      mrNo    : ;
      mrCancel: exit;
    end;
  end;

  result:=true;

  ltab:=pcMain.Pages[i];
  ltab.Parent:=nil;

  pROTabUserData(ltab.Tag)^.FileName:='';
  FreeMem(pROTabUserData(ltab.Tag));
  ltab.Free;

  if pcMain.PageCount>0 then
    pcMainChange(nil);
end;

function TROMainForm.CreateNewPage:integer;
var
  lpage:TTabSheet;
  se:TSynEdit;
  lptr:pROTabUSerData;
  i:integer;
begin
  result:=pcMain.PageCount;

  lpage:=pcMain.AddTabSheet;
  lpage.Caption:=sNewFile;

  GetMem  (lptr ,SizeOf(tROTabUserData));
  FillChar(lptr^,SizeOf(tROTabUserData),0);
  lpage.Tag:=UIntPtr(lptr);
//  lpage.OnShow:=@PageEnter;
//  lpage.OnEnter:=@PageEnter;

  se:=TSynEdit.Create(lpage);
  se.Parent     :=lpage;
  se.Align      :=alClient;
  se.TabWidth   :=2;
  se.Highlighter:=SynROSyn;
  se.Color      :=SynROSyn.SpaceAttri.Background;
  se.Options    :=se.Options
      +[eoTabIndent,
        eoTrimTrailingSpaces]
      -[eoTabsToSpaces];
  i:=se.Keystrokes.FindKeyCode(ord('N'),[ssCtrl]);
  if i>=0 then
    se.Keystrokes.Items[i].ShortCut:=0;

  // main unit-related
  se.PopupMenu  :=ROPopupMenu;
  se.BookMarkOptions.BookmarkImages:=ilBookmarks;
  // Temporary i hope (need sender=nil)
  se.AddKey(ecUserDefinedFirst+0,VK_F1,[ssCtrl],0,[]);
  se.OnProcessCommand:=@EditorProcessCommand;
  se.OnStatusChange  :=@EditorStatusChange;

  lptr^.Editor:=se;
end;

function TROMainForm.GetPageEditor(idx:integer):TSynEdit; inline;
begin
  result:=pROTabUserData(pcMain.Pages[idx].Tag)^.Editor;
end;

procedure TROMainForm.ImportScheme(const aFileIn,aFileOut:AnsiString);
const
  AttrNames: array [0..10] of AnsiString =(
    'Comment',
    'Identifier',
    'Key',
    'Constant',
    'Number',
    'Space',
    'Tab',
    'Label',
    'String',
    'Symbol',
    'Variables'
  );
var
  hin,hout:TINIFile;
  s:AnsiString;
  i:integer;
begin
  CreateINIFile(hin ,aFileIn ,true);
  CreateINIFile(hout,aFileOut,true);

  s:=ExtractFileName(PAnsiChar(aFileIn));
  i:=Length(s);
  while (i>1) do
  begin
    if s[i]='.' then
    begin
      SetLength(s,i-1);
      break;
    end;
    dec(i);
  end;
  ROColorScheme:=s;
  for i:=0 to High(AttrNames) do
  begin
    s:=hin[nil,pointer(AttrNames[i]),PAnsiChar('Style')];
    hout['Scheme',pointer(s),PAnsiChar(AttrNames[i]+'.'+'Style')]:=pointer(s);
    s:=hin[nil,pointer(AttrNames[i]),PAnsiChar('Background')];
    hout['Scheme',pointer(s),PAnsiChar(AttrNames[i]+'.'+'Background')]:=pointer(s);
    s:=hin[nil,pointer(AttrNames[i]),PAnsiChar('Foreground')];
    hout['Scheme',pointer(s),PAnsiChar(AttrNames[i]+'.'+'Foreground')]:=pointer(s);
  end;
  hout.Flush();

  FreeINIFile(hin);
  FreeINIFile(hout);
end;

//----- Windows -----

procedure TROMainForm.ActivateMIWindow(Sender: TObject);
begin
  TForm((Sender as TMenuItem).Tag).Show;
  TForm((Sender as TMenuItem).Tag).WindowState:=wsNormal;
  TForm((Sender as TMenuItem).Tag).BringToFront;
end;

procedure TROMainForm.mnuWindowsClick(Sender: TObject);
var
  mnu:TPopupMenu;
  mi:TMenuItem;
  i:integer;
begin
  mnu:=TPopupMenu.Create(Self);
  for i:=0 to Self.ComponentCount-1 do
  begin
    if (Self.Components[i] is TForm) then
    begin
      mi:=TMenuItem.Create(mnuWindows);
      mi.Tag    :=UIntPtr(Self.Components[i]);
      mi.OnClick:=@ActivateMIWindow;
      mi.Caption:=(Self.Components[i] as TForm).Caption;
      if mi.Caption='' then mi.Caption:='Untitled';
      mi.Checked:=not (Self.Components[i] as TForm).Visible;

      mnu.Items.Add(mi);
    end;
  end;
  mnu.Popup;
end;


end.
