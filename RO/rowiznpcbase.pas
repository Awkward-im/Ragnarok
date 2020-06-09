unit ROWizNPCBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, ExtCtrls, StdCtrls,
  Spin,
  rodatatypes;

type

  { TNPCBase }

  TNPCBase = class(TForm)
    btnInsert: TButton;
    cbNPCMap: TComboBox;
    cbNPCNames: TCheckBox;
    edNPCSprite: TEdit;
    edNPCName: TEdit;
    gbNPCCoord: TGroupBox;
    GroupBox1: TGroupBox;
    imgMapSprite: TImage;
    imgNPCSprite: TImage;
    lrb0: TLabel;
    lrb4: TLabel;
    lNPCSprite: TLabel;
    lYPos: TLabel;
    lXPos: TLabel;
    lNPCMap: TLabel;
    lNPCName: TLabel;
    rb0: TRadioButton;
    rb1: TRadioButton;
    rb2: TRadioButton;
    rb3: TRadioButton;
    rb4: TRadioButton;
    rb5: TRadioButton;
    rb6: TRadioButton;
    rb7: TRadioButton;
    seXPos: TSpinEdit;
    seYPos: TSpinEdit;
    procedure btnInsertClick(Sender: TObject);
    procedure cbNPCMapChoosed(Sender: TObject);
    procedure edNPCSpriteExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure FillMapList;
  public
    constructor Create(AOwner:TComponent; amodal:boolean=false); overload;
    procedure SaveNPC(out   data:TRONPCdata);
//    procedure LoadNPC(const data:TRONPCdata);
  end;

var
  NPCBase: TNPCBase=nil;

implementation

uses
  Controls,
  rodatagui;

{$R *.lfm}

{ TNPCBase }

constructor TNPCBase.Create(AOwner:TComponent; amodal:boolean=false);
begin
  inherited Create(AOwner);

  if amodal then
  begin
    btnInsert.Visible:=true;
    BorderStyle:=bsSizeable;
  end;
end;

{
procedure TNPCBase.LoadNPC(const data:TRONPCdata);
begin
  edNPCName.Text:=data.NPCName;
  cbNPCMap.Text:=data.NPCMap;
  seXPos.Value:=data.XPos;
  seYPos.Value:=data.YPos;

  edNPCSprite.Text:=RODB_Title[IndexOf[data.Sprite,rosmNPC],rosmNPC];

  case ord(data.Facing) of
    0: rb0.Checked:=true;
    1: rb1.Checked:=true;
    2: rb2.Checked:=true;
    3: rb3.Checked:=true;
    4: rb4.Checked:=true;
    5: rb5.Checked:=true;
    6: rb6.Checked:=true;
    7: rb7.Checked:=true;
  end;
end;
}
procedure TNPCBase.SaveNPC(out data:TRONPCdata);
var
  idx:integer;
begin
  data.NPCName:=edNPCName.Text;

  idx:=RODB_GetIndex(edNPCSprite.Text,rosmNPC);
  if idx>=0 then
  begin
    if cbNPCNames.Checked then
      data.Sprite:=RODB_GetName(idx,rosmNPC)
    else
      data.Sprite:=RODB_GetID(idx,rosmNPC);
  end
  else
    data.Sprite:=edNPCSprite.Text;

  data.NPCMap:=RODB_GetID(cbNPCMap.Text,rosmMap);

  data.XPos:=seXPos.Value;
  data.YPos:=seYPos.Value;

  if      rb0.Checked then data.Facing:=TFacing(0)
  else if rb1.Checked then data.Facing:=TFacing(1)
  else if rb2.Checked then data.Facing:=TFacing(2)
  else if rb3.Checked then data.Facing:=TFacing(3)
  else if rb4.Checked then data.Facing:=TFacing(4)
  else if rb5.Checked then data.Facing:=TFacing(5)
  else if rb6.Checked then data.Facing:=TFacing(6)
  else if rb7.Checked then data.Facing:=TFacing(7);
end;

procedure TNPCBase.FillMapList;
var
  i,cnt:integer;
begin
  cbNPCMap.Clear;
  cnt:=RODB_GetCount(rosmMap);
  cbNPCMap.Items.Capacity:=cnt;
  for i:=0 to cnt-1 do
  begin
    cbNPCMap.Items.Add(RODB_GetTitle(i,rosmMap));
  end;
end;

procedure TNPCBase.FormCreate(Sender: TObject);
begin
  AddListButton(edNPCSprite,rosmNPC);
  AddListButton(cbNPCMap,rosmMap);

  FillMapList;
  rb0.Checked:=true;
end;

procedure TNPCBase.cbNPCMapChoosed(Sender: TObject);
begin
  try
    imgMapSprite.Picture.LoadFromFile(RODB_GetSprite(cbNPCMap.Text,rosmMap));
  except
    imgMapSprite.Picture.Clear;
  end;
end;

procedure TNPCBase.edNPCSpriteExit(Sender: TObject);
begin
  try
    imgNPCSprite.Picture.LoadFromFile(RODB_GetSprite(edNPCSprite.Text,rosmNPC));
  except
    try
      imgNPCSprite.Picture.LoadFromFile(
          RODB_GetSprite(RODB_GetIndex(edNPCSprite.Text,rosmNPC),rosmNPC));
    except
      imgNPCSprite.Picture.Clear;
    end;
  end;
end;

procedure TNPCBase.btnInsertClick(Sender: TObject);
var
  data:TRONPCdata;
begin
  SaveNPC(data);
  ActiveEditor.InsertTextAtCaret(GetNPCCode('script',data)+#13#10);
end;

end.
