unit Unit2;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    rbSkip: TRadioButton;
    rbRename: TRadioButton;
    rbOverwrite: TRadioButton;
    rbOverwriteAll: TRadioButton;
    btAbort: TButton;
    rbSkipAll: TRadioButton;
    btOK: TButton;
    procedure cmEnter(Sender: TObject);
    procedure cmBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.lfm}

procedure TForm2.cmEnter(Sender: TObject);
begin
  if Sender=rbRename then
    Edit1.Enabled:=true
  else
    Edit1.Enabled:=false;
end;

procedure TForm2.cmBtnClick(Sender: TObject);
var
  lMode:UInt;
begin
  lMode:=fwAbort;
  if Sender=btOK then
  begin
    if rbSkip.Checked then
      lMode:=fwSkip
    else if rbSkipAll.Checked then
      lMode:=fwSkipAll
    else if rbOverwrite.Checked then
      lMode:=fwOverwrite
    else if rbOverwriteAll.Checked then
      lMode:=fwOverwriteAll
    else if rbRename.Checked then
    begin
      lMode:=fwRename
    end;
  end;
  Form1.ExtractFileMode:=lMode;
  ModalResult:=mrOK;
end;

end.
