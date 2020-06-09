unit ROFileSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn;

type

  { TROSettingsForm }

  TROSettingsForm = class(TForm)
    bbApply: TBitBtn;
    bbSchemeReload: TBitBtn;
    cbFullCard: TCheckBox;
    cbAllowEdit: TCheckBox;
    cbAutoLoadLocal: TCheckBox;
    cbWinCPFiles: TCheckBox;
    cbCommentToDel: TCheckBox;
    cbLoadCommented: TCheckBox;
    cbScheme: TComboBox;
    cbAutoAddScheme: TCheckBox;
    ebDataDir: TEditButton;
    lblDataDir: TLabel;
    dlgSelectDataDir: TSelectDirectoryDialog;
    lblScheme: TLabel;
    procedure bbApplyClick(Sender: TObject);
    procedure bbSchemeReloadClick(Sender: TObject);
    procedure ebDataDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    modified:boolean;
    procedure FillSchemeList;

  public

  end;

var
  ROSettingsForm: TROSettingsForm;

implementation

uses
  cmemini,
  synhighlighterro,
  rodatatypes,rodatagui;

{$R *.lfm}

{ TROSettingsForm }

procedure TROSettingsForm.ebDataDirClick(Sender: TObject);
var
  s:AnsiString;
begin
  if Pos(':\',ebDataDir.Text)=0 then
    s:=GetCurrentDir+'\'
  else
    s:='';
  dlgSelectDataDir.InitialDir:=s+ebDataDir.Text;
  dlgSelectDataDir.FileName  :=s+ebDataDir.Text;
  if dlgSelectDataDir.Execute then
  begin
    ebDataDir.Text:=dlgSelectDataDir.FileName;
  end;
end;

procedure TROSettingsForm.bbApplyClick(Sender: TObject);
var
  s:AnsiString;
  res:integer;
begin
  res:=0;

  // if color scheme was changed
  if modified then
    res:=res or (4 shl 8);

  s:=ebDataDir.Text;
  if (s<>'') and (s[Length(s)]<>'\') then s:=s+'\';

  if s<>RODataDirectory then
  begin
    res:=res or (1 shl 8);
    RODataDirectory:=s;
    modified:=true;
  end;

  if ROShowFullCard<>cbFullCard.Checked then
  begin
    ROShowFullCard:=cbFullCard.Checked;
    modified:=true;
  end;
  if ROAllowEditCard<>cbAllowEdit.Checked then
  begin
    ROAllowEditCard:=cbAllowEdit.Checked;
    modified:=true;
  end;
  if ROAutoLoadLocal<>cbAutoLoadLocal.Checked then
  begin
    ROAutoLoadLocal:=cbAutoLoadLocal.Checked;
    modified:=true;
  end;
  if ROFilesAreACP<>cbWinCPFiles.Checked then
  begin
    ROFilesAreACP:=cbWinCPFiles.Checked;
    modified:=true;
  end;
  if ROCommentToDel<>cbCommentToDel.Checked then
  begin
    ROCommentToDel:=cbCommentToDel.Checked;
    modified:=true;
  end;
  if ROLoadCommented<>cbLoadCommented.Checked then
  begin
    ROLoadCommented:=cbLoadCommented.Checked;
    res:=res or (2 shl 8);
    modified:=true;
  end;
  if ROAutoAddScheme<>cbAutoAddScheme.Checked then
  begin
    ROAutoAddScheme:=cbAutoAddScheme.Checked;
    modified:=true;
  end;

  if modified then
    SaveSettings;

  ModalResult:=res+mrOk;
end;

procedure TROSettingsForm.bbSchemeReloadClick(Sender: TObject);
begin
  if cbScheme.ItemIndex>=0 then
  begin
    modified:=true;
    ROColorScheme:=cbScheme.Items[cbScheme.ItemIndex];
    SynROSyn.LoadHighLighter(defSettingsFile,ROColorScheme);
  end;
end;

procedure TROSettingsForm.FillSchemeList;
var
  lini:TINIFile;
  p:PAnsiChar;
begin
  cbScheme.Items.Clear;
  CreateINIFile(lini,defSettingsFile,true);
  p:=lini.SectionList['Scheme'];
  if p<>nil then
    while p^<>#0 do
    begin
      cbScheme.Items.Add(p);
      p:=StrEnd(p)+1;
    end;

  FreeINIFile(lini);
  SetListIndex(cbScheme,ROColorScheme)
end;

procedure TROSettingsForm.FormCreate(Sender: TObject);
begin
  modified:=false;
  ebDataDir.Text:=RODataDirectory;
  cbFullCard     .Checked:=ROShowFullCard;
  cbAllowEdit    .Checked:=ROAllowEditCard;
  cbAutoLoadLocal.Checked:=ROAutoLoadLocal;
  cbWinCPFiles   .Checked:=ROFilesAreACP;
  cbCommentToDel .Checked:=ROCommentToDel;
  cbLoadCommented.Checked:=ROLoadCommented;
  cbAutoAddScheme.Checked:=ROAutoAddScheme;

  FillSchemeList;
end;

end.
