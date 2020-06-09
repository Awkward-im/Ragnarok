unit rodatalocal;

interface

uses
  rodatatypes;

const
  defLocalItemFileName = defDataDirectory+'idnum2itemdisplaynametable.txt';
  defLocalMapFileName  = defDataDirectory+'mapnametable_ru.txt';

function LoadMapLocal(const aname:AnsiString=defLocalMapFileName):boolean;
procedure UnloadMapLocal;

function LoadMobLocal (const aname:AnsiString=''):boolean;
procedure UnloadMobLocal;

function LoadItemLocal(const aname:AnsiString=defLocalItemFileName):boolean;
procedure UnloadItemLocal;


implementation

uses
  Classes,
  SysUtils,
  LazUTF8, // WinCPtoUTF8
  rodataio;

//----- Maps -----

procedure UnloadMapLocal;
var
  i:integer;
begin
  for i:=0 to High(ROMapData) do
    ROMapData[i].local:='';
end;

function LoadMapLocal(const aname:AnsiString=defLocalMapFileName):boolean;
var
  sl:TStringList;
  lname,p:PAnsiChar;
  d,line,i:integer;
begin
  result:=false;
  if Length(ROMapData)=0 then exit;

  sl:=TStringList.Create;
  try
    sl.Sorted:=true;
    sl.LoadFromFile(aname);

    UnloadMapLocal;

    line:=0;
    while line<sl.Count do
    begin
      if GetLineClass(sl[line])=glcValue then
      begin
        p:=pointer(sl[line]);
        lname:=GetLinePart(p); (p-5)^:=#0;
        for i:=0 to High(ROMapData) do
        begin
          d:=CompareStr(StrCache[ROMapData[i].name],lname);
          if d>=0 then
          begin
            if d=0 then
            begin
//              ROMapData[i].local:=WinCPToUTF8(GetLinePart(p));
              ROMapData[i].local:=GetLinePart(p);
              SetCodePage(RawByteString(ROMapData[i].local), CP_ACP, false);
            end;
            break;
          end;
        end;
      end;
      inc(line);
    end;
    result:=true;
  except

  end;
  sl.Free;
end;

//----- Mobs -----

procedure UnloadMobLocal;
var
  i:integer;
begin
  for i:=0 to High(ROMobData) do
    ROMobData[i].local:='';
end;

function LoadMobLocal(const aname:AnsiString=''):boolean;
begin
  result:=false;
end;

//----- Items -----

procedure UnloadItemLocal;
var
  i:integer;
begin
  for i:=0 to High(ROItemData) do
    ROItemData[i].local:='';
end;

function LoadItemLocal(const aname:AnsiString=defLocalItemFileName):boolean;
var
  sl:TStringList;
  p:PAnsiChar;
  prev,line,i,lid:integer;
begin
  result:=false;
  if Length(ROItemData)=0 then exit;

  sl:=TStringList.Create;
  try
    sl.LoadFromFile(aname);

    UnloadItemLocal;

    prev:=0;
    for line:=0 to sl.Count-1 do
    begin
      if GetLineClass(sl[line])=glcValue then
      begin
        p:=pointer(sl[line]);
        lid:=StrToInt(GetLinePart(p));

        for i:=prev to High(ROItemData) do
          if ROItemData[i].id>=lid then
          begin
            if ROItemData[i].id=lid then
            begin
              prev:=i+1;
//              ROItemData[i].local:=WinCPToUTF8(StringReplace(GetLinePart(p),'_',' ',[rfReplaceAll]));
              ROItemData[i].local:=StringReplace(GetLinePart(p),'_',' ',[rfReplaceAll]);
              SetCodePage(RawByteString(ROItemData[i].local), CP_ACP, false);
            end;
            break;
          end;
      end;
    end;
   
    result:=true;
  except

  end;
  sl.Free;
end;

end.
