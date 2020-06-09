// Copyright (c) Athena Dev Teams - Licensed under GNU GPL
// For more information, see LICENCE in the main folder
{$I-}
unit grfio;

interface

function  grfio_init(fname:PAnsiChar):integer;
procedure grfio_final();
procedure grfio_resourcecheck()
function  grfio_add      (fname:PAnsiChar):integer;
function  grfio_reads    (fname:PAnsiChar; size:pinteger=nil):pointer;
function  grfio_find_file(fname:PAnsiChar):PAnsiChar; // need to implement find by name only

//----------------------------
//  file entry table struct
//----------------------------

const
  FILELIST_TYPE_FILE           = 01; // entry is a file
  FILELIST_TYPE_ENCRYPT_HEADER = 04; // encryption mode 1 (header DES only)
  FILELIST_TYPE_ENCRYPT_MIXED  = 02; // encryption mode 0 (header DES + periodic DES/shuffle)

type
  PFILELIST = ^TFILELIST;
  TFILELIST = record
    fn            :array [0..128-4*5-1] of AnsiChar; // file name
    fnd           :PAnsiChar; // if the file was cloned, contains name of original file
    srclen        :integer;   // compressed size
    srclen_aligned:integer;
    declen        :integer;   // original size
    srcpos        :integer;   // position of entry in grf
    next          :integer;   // index of next filelist entry with same hash (-1: end of entry chain)
    typ           :shortint;
    gentry        :Shortint;  // read grf file select
  end;
//gentry ... > 0 : data read from a grf file (gentry_table[gentry-1])
//gentry ... 0   : data read from a local file (data directory)
//gentry ... < 0 : entry "-(gentry)" is marked for a local file check
//                 - if local file exists, gentry will be set to 0 (thus using the local file)
//                 - if local file doesn't exist, sign is inverted (thus using the original file inside a grf)
//                 (NOTE: this case is only used once (during startup) and only if GRFIO_LOCAL is enabled)
//                 (NOTE: probably meant to be used to override grf contents by files in the data directory)

function filelist_count:integer;
function filelist_out(idx:integer):PFILELIST;


implementation

uses
  des,
  crc,
  common,
  zcompres,
  zuncompr;

type
  tGRFHeader = packed record {GRF file header}
    Signature :array [0..15] of AnsiChar;
    Un1       :array [0..13] of byte;
    DirOffset :longword; {Real offset=DirOffset+$2E=SizeOf(tGRFHeader)}
    Un2       :longword;
    DirEntries:longword; {Real Entries=DirEntries-Un2-7}
    fType     :longword;
  end;

  pGRFFileEntry = ^tGRFFileEntry;
  tGRFFileEntry = packed record {Directory chunk w/o name}
    psize1:longword; // packed
    psize2:longword; // packed aligned
    usize :longword; // unpacked
    typ   :byte;
    dofs  :longword; // data offset
  end;

const
  GENTRY_ADDS   = 4;    // The number increment of gentry_table entries
  FILELIST_ADDS = 1024; // number increment of file lists `

var
// stores info about every loaded file
  filelist:array of TFILELIST = nil;
  filelist_entrys:integer = 0;

// stores grf file names
  gentry_table :array of PAnsiChar = nil;
  gentry_entrys:integer = 0;

// the path to the data directory
  data_dir:array [0..1023] of AnsiChar = '';


// little endian char array to uint conversion
function getlong(p:pByte):cardinal;
begin
  result:=p[0] or (p[1] shl 8) or (p[2] shl 16) or (p[3] shl 24);
end;

procedure NibbleSwap(src:pByte; len:integer);
begin
  while len>0 do
  begin
    src^:=(src^ shr 4) or (src^ shl 4);
    inc(src);
    dec(len);
  end;
end;


/// Substitutes some specific values for others, leaves rest intact. Obfuscation.
/// NOTE: Operation is symmetric (calling it twice gives back the original input).
function grf_substitution(_in:byte):byte;
begin
  case _in of
    $00: result := $2B;
    $2B: result := $00;
    $6C: result := $80;
    $01: result := $68;
    $68: result := $01;
    $48: result := $77;
    $60: result := $FF;
    $77: result := $48;
    $B9: result := $C0;
    $C0: result := $B9;
    $FE: result := $EB;
    $EB: result := $FE;
    $80: result := $6C;
    $FF: result := $60;
  else result := _in;
  end;
end;

procedure grf_shuffle_dec(var src:TBIT64);
var
  _out:TBIT64;
begin
  _out[0] := src[3];
  _out[1] := src[4];
  _out[2] := src[6];
  _out[3] := src[0];
  _out[4] := src[1];
  _out[5] := src[2];
  _out[6] := src[5];
  _out[7] := grf_substitution(src[7]);

  src := _out;
end;


procedure grf_decode_header(buf:pbyte; len:integer);
var
  p:PBIT64;
  nblocks:integer;
  i:integer;
begin
  p:=pointer(buf);
  nblocks:=len div sizeof(TBIT64);
  // first 20 blocks are all des-encrypted
  i:=0;
  while (i<20) and (i<nblocks) do
  begin
    des_decrypt_block(p);
    inc(p);
    inc(i);
  end;
  // the rest is plaintext, done.
end;


procedure grf_decode_full(buf:pByte; len:integer; cycle:integer);
var
  p:PBIT64;
  nblocks:integer;
  dcycle, scycle:integer;
  i, j:integer;
begin
  p:=pointer(buf);
  nblocks:=len div sizeof(TBIT64);

  // first 20 blocks are all des-encrypted
  i:=0;
  while (i<20) and (i<nblocks) do
  begin
    des_decrypt_block(p);
    inc(p);
    inc(i);
  end;

  // after that only one of every 'dcycle' blocks is des-encrypted
  dcycle:=cycle;

  // and one of every 'scycle' plaintext blocks is shuffled (starting from the 0th but skipping the 0th)
  scycle:=7;

  // so decrypt/de-shuffle periodically
  j:=0; // 0, adjusted to fit the ++j step
  for i:=20 to nblocks-1 do
  begin
    if (i mod dcycle)=0 then
      // decrypt block
      des_decrypt_block(p)
    else
    begin
      if ((j mod scycle)=0) and (j<>0) then
        // de-shuffle block
        grf_shuffle_dec(p^);
      inc(j);
    end;
    inc(p);

    // plaintext, do nothing.
  end;
end;


/// Decodes grf data.
/// @param buf data to decode (in-place)
/// @param len length of the data
/// @param entry_type flags associated with the data
/// @param entry_len true (unaligned) length of the data
procedure grf_decode(buf:pByte; len:integer; entry_type:shortint; entry_len:integer);
var
  digits:integer;
  cycle:integer;
  i:integer;
begin
  if (entry_type and FILELIST_TYPE_ENCRYPT_MIXED)<>0 then
  begin // fully encrypted
    // compute number of digits of the entry length
    digits:=1;
    i:=10;
    while i<=entry_len do
    begin
      inc(digits);
      i:=i*10;
    end;

    // choose size of gap between two encrypted blocks
    // digits:  0  1  2  3  4  5  6  7  8  9 ...
    //  cycle:  1  1  1  4  5 14 15 22 23 24 ...
    if      digits<3 then cycle:=1
    else if digits<5 then cycle:=digits+1
    else if digits<7 then cycle:=digits+9
    else                  cycle:=digits+15;

    grf_decode_full(buf, len, cycle);
  end
  else if (entry_type and FILELIST_TYPE_ENCRYPT_HEADER)<>0 then
  begin // header encrypted
    grf_decode_header(buf, len);
  end
  else
  begin // plaintext
    ;
  end;
end;


{******************************************************
 ***                Zlib Subroutines                ***
 ******************************************************}

/// zlib crc32
function grfio_crc32(buf:pByte; len:cardinal):cardinal;
begin
  result:=crc32(crc32(0, nil, 0), buf, len);
end;


/// zlib uncompress
function decode_zip(dest:pointer; var destLen:longword; source:pointer; sourceLen:longword):integer;
begin
  result:=uncompress(pbyte(dest), destLen, pbyte(source)^, sourceLen);
end;


/// zlib compress
function encode_zip(dest:pointer; var destLen:longword; source:pointer; sourceLen:longword):integer;
begin
  result:=compress(pbyte(dest), destLen, pbyte(source)^, sourceLen);
end;


{***********************************************************
 ***                File List Subroutines                ***
 ***********************************************************}
var
// file list hash table
  filelist_hash: array [0..255] of integer;

// initializes the table that holds the first elements of all hash chains
procedure hashinit();
var
  i:integer;
begin
  for i:=0 to 255 do
    filelist_hash[i]:=-1;
end;

// hashes a filename string into a number from {0..255}
function filehash(fname:PAnsiChar):integer;
var
  lhash:cardinal;
begin
  lhash:=0;
  while fname^<>#0 do
  begin
    lhash:=(lhash shl 1) + (lhash shr 7)*9 + ord(system.LowerCase(fname^));
    inc(fname);
  end;

  result:=lhash and 255;
end;

function filelist_count:integer;
begin
  result:=filelist_entrys;
end;

function filelist_out(idx:integer):PFILELIST;
begin
  if (idx<0) or (idx>=filelist_entrys) then
    exit(nil);
  
  result:=@filelist[idx];
end;


// finds a FILELIST entry with the specified file name

// type korstr = type ansistring(949);

function filelist_find(fname:PAnsiChar):PFILELIST;
var
  index:integer;
//  tmp:korstr;
begin
  if filelist=nil then
  begin
    result:=nil;
    exit;
  end;

  index:=filelist_hash[filehash(fname)];
  while index<>-1 do
  begin
//    tmp:=filelist[index].fn;
//    if tmp=fname then break;
    if strcmp(filelist[index].fn, fname)=0 then //!! StrCmpI
      break;
    index:=filelist[index].next;
  end;

  if index>=0 then
    result:=@filelist[index]
  else
    result:=nil;
end;

// returns the original file name
function grfio_find_file(fname:PAnsiChar):PAnsiChar;
var
  filelist_res:PFILELIST;
begin
  filelist_res:=filelist_find(fname);
  if filelist_res=nil then
  begin
    result:=nil;
    exit;
  end;
  if filelist_res^.fnd=nil then
    result:=filelist_res^.fn
  else
    result:=filelist_res^.fnd;
end;

// adds a FILELIST entry into the list of loaded files
function filelist_add(entry:PFILELIST):PFILELIST;
var
  lhash:integer;
begin
  if filelist_entrys>=Length(filelist) then
    SetLength(filelist,Length(filelist)+FILELIST_ADDS);

  move(entry^, filelist[filelist_entrys], sizeof(TFILELIST));

  lhash:=filehash(entry^.fn);
  filelist[filelist_entrys].next:=filelist_hash[lhash];
  filelist_hash[lhash]:=filelist_entrys;

  result:=@filelist[filelist_entrys];

  inc(filelist_entrys);
end;

// adds a new FILELIST entry or overwrites an existing one
function filelist_modify(entry:PFILELIST):PFILELIST;
var
  fentry:PFILELIST;
  tmp:integer;
begin
  fentry:=filelist_find(entry^.fn);
  if fentry<>nil then
  begin
    tmp:=fentry^.next;
    mFreeMem(fentry^.fnd);
    move(entry^, fentry^, sizeof(TFILELIST));
    fentry^.next:=tmp;
  end
  else
  begin
    fentry:=filelist_add(entry);
  end;
  result:=fentry;
end;

// shrinks the file list array if too long
procedure filelist_compact();
begin
  if filelist_entrys=0 then
    exit;

  if filelist_entrys<Length(filelist) then
    SetLength(filelist,filelist_entrys);
end;


{***********************************************************
 ***                  Grfio Subroutines                  ***
 ***********************************************************}

/// Combines are resource path with the data folder location to create local resource path.
procedure grfio_localpath_create(buffer:PAnsiChar; size:integer; filename:PAnsiChar);
var
  p:PAnsiChar;
begin
  p:=StrCopyE(buffer,data_dir);
  if (data_dir[0]<>#0) and not ((p-1)^ in ['/','\']) then
  begin
    p^:='/';
    inc(p);
  end;
  StrCopy(p,filename);

  // normalize path
  p:=buffer;
  while p^<>#0 do
  begin
    if p^='\'  then
      p^:='/';
    inc(p);
  end;
end;


/// Reads a file into a newly allocated buffer (from grf or data directory).
function grfio_reads(fname:PAnsiChar; size:pinteger=nil):pointer;
var
  lfname:array [0..255] of AnsiChar;
  grfname:PAnsiChar;
  buf,buf2:PAnsiChar;
  _in:file of byte;
  entry:PFILELIST;
  declen,fsize:integer;
  len:cardinal;
begin
  buf2:=nil;
  entry:=filelist_find(fname);

  if (entry=nil) or (entry^.gentry<=0) then
  begin// LocalFileCheck
    if (entry<>nil) and (entry^.fnd<>nil) then
      buf:=entry^.fnd
    else
      buf:=fname;
    grfio_localpath_create(lfname, sizeof(lfname), buf);

    Assign(_in,PAnsiChar(@lfname));
    {$I-}
    Reset(_in);
    if IOResult=0 then
    begin
      declen:=FileSize(_in);
      buf2:=GetMem(declen+1);  // +1 for resnametable zero-termination
      BlockRead(_in,buf2^,declen,len);
//      if len<>declen then
//        ShowError("An error occured in fread grfio_reads, fname=%s \n",fname);
      Close(_in);
      buf2[declen]:=#0;

      if size<>nil then
        size^:=declen;
    end
    else
    begin
      if (entry<>nil) and (entry^.gentry<0) then
      begin
        entry^.gentry:=-entry^.gentry;  // local file checked
      end
      else
      begin
//        ShowError("grfio_reads: %s not found (local file: %s)\n", fname, lfname);
        result:=nil;
        exit;
      end
    end
  end;

  if (entry<>nil) and (entry^.gentry>0) then
  begin // Archive[GRF] File Read
    grfname:=gentry_table[entry^.gentry-1];
    Assign(_in,grfname);
    {$I-}
    Reset(_in);
    if IOResult=0 then
    begin
      fsize:=entry^.srclen_aligned;
      GetMem(buf,fsize);
      Seek(_in, entry^.srcpos);
      BlockRead(_in,buf^,fsize,len);
//      if len<>fsize then
//        ShowError("An error occured in fread in grfio_reads, grfname=%s\n",grfname);
      Close(_in);

      GetMem(buf2,entry^.declen+1);  // +1 for resnametable zero-termination
      if (entry^.typ and FILELIST_TYPE_FILE)<>0 then
      begin // file
        grf_decode(PByte(buf), fsize, entry^.typ, entry^.srclen);
        len:=entry^.declen;
        fsize:=decode_zip(buf2, len, buf, entry^.srclen);
        if (fsize<>0) or (len<>entry^.declen) then
        begin
//          ShowError("decode_zip size mismatch err: %d != %d\n", (int)len, entry->declen);
          FreeMem(buf);
          FreeMem(buf2);
          result:=nil;
          exit;
        end;
      end
      else
      begin// directory?
        move(buf^, buf2^, entry^.declen);
      end;
      buf2[entry^.declen]:=#0;
      if size<>nil then
        size^:=entry^.declen;

      FreeMem(buf);
    end
    else
    begin
//      ShowError("grfio_reads: %s not found (GRF file: %s)\n", fname, grfname);
      result:=nil;
      exit;
    end
  end;

  result:=buf2;
end;

/// Decodes encrypted filename from a version 01xx grf index.
function decode_filename(buf:PByte; len:integer):PAnsiChar;
var
  lop:integer;
begin
  lop:=0;
  while lop<len do
  begin
    NibbleSwap (buf+lop,8);
    des_decrypt(buf+lop,8);
    inc(lop,8);
  end;
  result:=PAnsiChar(buf);
end;


/// Compares file extension against known large file types.
/// @return true if the file should undergo full mode 0 decryption, and true otherwise.
function isFullEncrypt(fname:PAnsiChar):boolean;
var
  ext:PAnsiChar;
begin
  ext:=StrRScan(fname, '.');
  if ext<>nil then
  begin
    //!! StrCmpI
    if (strcmp(ext, '.gnd')=0) or
       (strcmp(ext, '.gat')=0) or
       (strcmp(ext, '.act')=0) or
       (strcmp(ext, '.str')=0) then
    begin
      result:=false;
      exit;
    end;
  end;

  result:=true;
end;


/// Loads all entries in the specified grf file into the filelist.
/// @param gentry index of the grf file name in the gentry_table
function grfio_entryread(grfname:PAnsiChar; gentry:integer):integer;
var
  eheader:array [0..7] of byte;
  grf_filelist:pByte;
  lptr:pByte;
  fname:PAnsiChar;
  rBuf:PAnsiChar;
  aentry:TFILELIST;
  fp:file of byte;
  list_size,
  grf_size:longword;
  gfe:pGRFFileEntry;
  grf_header:tGRFHeader;
  entry,entrys:integer;
  srclen:integer;
  ofs,ofs2:integer;
  rSize, eSize:longword;
  typ:byte;
begin
  Assign(fp,grfname);
  Reset(fp);
  if IOResult<>0 then
  begin
//    ShowWarning("GRF data file not found: '%s'\n",grfname);
    exit(1);  // 1:not found error
  end
  else
;//    ShowInfo("GRF data file found: '%s'\n",grfname);

  grf_size:=FileSize(fp);

  BlockRead(fp,grf_header,SizeOf(grf_header),srclen);
//  if srclen<>SizeOf(tGRFHeader) then
//    ShowError("Couldn't read all grf_header element of %s \n", grfname);

  if strcmp(grf_header.Signature,'Master of Magic')<>0 then
  begin
    Close(fp);
//    ShowError("GRF %s read error\n", grfname);
    exit(2);  // 2:file format error
  end;

  Seek(fp,SizeOf(grf_header)+grf_header.DirOffset);
  if IOResult<>0 then
  begin
    Close(fp);
//    ShowError("GRF %s read error\n", grfname);
    exit(2);  // 2:file format error
  end;

  case grf_header.fType shr 8 of
    // ****** Grf version 01xx ******
    1: begin
      list_size:=grf_size-{grf_header.DirOffset}FilePos(fp);
      GetMem(grf_filelist,list_size);
      BlockRead(fp,grf_filelist^,list_size,srclen);
//      if srclen<>list_size then
//        ShowError("Couldn't read all grf_filelist element of %s \n", grfname);
      Close(fp);

      entrys:=grf_header.DirEntries-grf_header.Un2-7;

      // Get an entry
      ofs:=0;
      lptr:=grf_filelist;
      for entry:=0 to entrys-1 do
      begin
        ofs2:=ofs+getlong(grf_filelist+ofs)+4;
        {
         0,4 - local tGRFFileEntry offset = (fname len + 6)
         4,2 - ??
         6,x - fname
        }

        gfe:=pointer(grf_filelist)+ofs2;

        typ:=gfe^.typ;

        if (typ and FILELIST_TYPE_FILE)<>0 then
        begin
          fname:=decode_filename(grf_filelist+ofs+6, grf_filelist[ofs]-6);
          if strlen(fname)>sizeof(aentry.fn)-1 then
          begin
  //          ShowFatalError("GRF file name %s is too long\n", fname);
            FreeMem(grf_filelist);
            exit(-1);
          end;

          if isFullEncrypt(fname) then
            typ:=typ or FILELIST_TYPE_ENCRYPT_MIXED
          else
            typ:=typ or FILELIST_TYPE_ENCRYPT_HEADER;

          aentry.srclen         := gfe^.psize1-gfe^.usize-715;
          aentry.srclen_aligned := gfe^.psize2-37579;
          aentry.declen         := gfe^.usize;
          aentry.srcpos         := gfe^.dofs+SizeOf(tGRFHeader);
          aentry.typ            := typ;
          StrCopy(aentry.fn, fname, sizeof(aentry.fn));
          aentry.fnd            := nil;
          aentry.gentry         := -(gentry+1);  // As Flag for making it a negative number carrying out the first time LocalFileCheck
          filelist_modify(@aentry);
        end;

        ofs:=ofs2+SizeOf(tGRFFileEntry);
      end;

      FreeMem(grf_filelist);
    end;

    // ****** Grf version 02xx ******
    2: begin
      BlockRead(fp,eheader,8,srclen);
//      if srclen<>8 then
//        ShowError("An error occured in fread while reading eheader buffer\n");
      rSize:=getlong(eheader);      // Read Size
      eSize:=getlong(@eheader[4]);  // Extend Size

      if rSize>(grf_size-FilePos(fp)) then
      begin
        Close(fp);
  //      ShowError("Illegal data format: GRF compress entry size\n");
        exit(4);
      end;

      GetMem(rBuf,rSize);          // Get a Read Size
      GetMem(grf_filelist,eSize);  // Get a Extend Size
      BlockRead(fp,rBuf^,rSize,srclen);
//      if srclen<>rSize then
//        ShowError("An error occured in fread \n");
      Close(fp);
      decode_zip(grf_filelist, eSize, rBuf, rSize);  // Decode function
      FreeMem(rBuf);

      entrys:=grf_header.DirEntries-7;

      // Get an entry
      lptr:=grf_filelist;
      for entry:=0 to entrys-1 do
      begin
        fname:=PAnsiChar(lptr);
        gfe:=pGRFFileEntry(lptr+strlen(fname)+1);

        if strlen(fname)>(sizeof(aentry.fn)-1) then
        begin
  //        ShowFatalError("GRF file name %s is too long\n", fname);
          FreeMem(grf_filelist);
          exit(-1);
        end;

        if (gfe^.typ and FILELIST_TYPE_FILE)<>0 then
        begin// file
          aentry.srclen         := gfe^.psize1;
          aentry.srclen_aligned := gfe^.psize2;
          aentry.declen         := gfe^.usize;
          aentry.srcpos         := gfe^.dofs+SizeOf(tGRFHeader);
          aentry.typ            := gfe^.typ;
          StrCopy(aentry.fn, fname, sizeof(aentry.fn));
          aentry.fnd            := nil;
          aentry.gentry         := -(gentry+1); // As Flag for making it a negative number carrying
          filelist_modify(@aentry);
        end;

        lptr:=pByte(gfe)+SizeOf(tGRFFileEntry);
      end;

      FreeMem(grf_filelist);
    end;

  // ****** Grf Other version ******
  else
    Close(fp);
//    ShowError("GRF version %04x not supported\n",getlong(grf_header.typ));
    exit(4);
  end;

  filelist_compact();  // Unnecessary area release of filelist

  result:=0;  // 0:no error
end;


function grfio_parse_restable_row(row:PAnsiChar):boolean;
var
  ls, src, dst,
  local: array [0..255] of AnsiChar;
  entry :PFILELIST;
  fentry:TFILELIST;
  p:PAnsiChar;
  f:file of byte;
  len:integer;
begin
  result:=false;

  p:=StrScan(row,'#');
  len:=StrLen(row);
  if (p=nil) or (row[len-1]<>'#') then
    exit;

  StrCopy(ls,p+1,row+len-p-2);
  // we only need the maps' GAT and RSW files
  if (StrPos(ls,'.gat')=nil) and
     (StrPos(ls,'.rsw')=nil) then
    exit;

  StrCopy(StrCopyE(dst,'data\'),ls);

  entry:=filelist_find(dst);

  if entry<>nil then          // alias for GRF resource
    move(entry^, fentry, sizeof(TFILELIST))
  else
  begin
    grfio_localpath_create(local, sizeof(local), dst);

    Assign(f,PAnsiChar(@local));
    Reset(f);
    if IOResult=0 then        // alias for local resource
    begin
      Close(f);
      FillChar(fentry, sizeof(fentry),0)
    end
    else
      exit;
  end;
  StrCopy(StrCopyE(src,'data\'),row,p-row);
  StrCopy(fentry.fn, src, sizeof(fentry.fn));
//  StrCopy(StrCopyE(fentry.fn,'data\'),row,p-row);
  StrDup (fentry.fnd, dst);
  filelist_modify(@fentry);
  result:=true;
end;


const
  resname = 'data\resnametable.txt';

/// Grfio Resource file check.
procedure grfio_resourcecheck();
var
  fp:file of byte;
  restable:array [0..255] of AnsiChar;
  buf,ps,pe:PAnsiChar;
  size:integer;
begin
  // read resnametable from data directory and return if successful
  grfio_localpath_create(restable, sizeof(restable), resname);

  Assign(fp,PAnsiChar(@restable));
  Reset(fp);
  if IOResult=0 then
  begin
    size:=FileSize(fp);
    GetMem(buf,size+1);
    BlockRead(fp,buf^,size);
    Close(fp);
  end
  else
  begin
    buf:=grfio_reads(resname, @size);
    if buf=nil then
      exit;
  end;
  buf[size]:=#0;
  ps:=buf;

  while ps^<>#0 do
  begin
    pe:=ps;
    while not (pe^ in [#0,#13,#10]) do inc(pe);
    if pe^<>#0 then
    begin
      pe^:=#0;
      repeat
        inc(pe);
      until not (pe^ in [#13,#10]);
    end;
    if (ps^<>#0) and (ps^<>'/') and ((ps+1)^<>'/') then
      grfio_parse_restable_row(ps);
    ps:=pe;
  end;

  FreeMem(buf);
end;


/// Reads a grf file and adds it to the list.
function grfio_add(fname:PAnsiChar):integer;
begin
  if gentry_entrys>=Length(gentry_table) then
  begin
    SetLength(gentry_table,Length(gentry_table)+GENTRY_ADDS);
  end;

  StrDup(gentry_table[gentry_entrys],fname);

  result:=grfio_entryread(fname, gentry_entrys);
  inc(gentry_entrys);
end;


/// Finalizes grfio.
procedure grfio_final();
var
  i:integer;
begin
  if filelist_entrys>0 then
  begin
    for i:=0 to filelist_entrys-1 do
      mFreeMem(filelist[i].fnd);

    SetLength(filelist,0);
  end;
  filelist_entrys:=0;

  if gentry_entrys>0 then
  begin
    for i:=0 to gentry_entrys-1 do
      mFreeMem(gentry_table[i]);

    SetLength(gentry_table,0);
  end;
  gentry_entrys:=0;
end;


/// Initializes grfio.
function grfio_init(fname:PAnsiChar):integer;
var
//  sl:TStringList;
  fp:TextFile;
  line:AnsiString;
begin
  result:=0;

  if (fname<>nil) and (fname^<>#0) then
  begin
    AssignFile(fp,fname);
    Reset(fp);
    while not Eof(fp) do
    begin
      Readln(fp,line);
      if (line='') or ((line[1]='/') and (line[2]='/')) then
        continue; // skip comments

      if Pos('grf: ',line)=1 then
      begin
        if grfio_add(@line[6])=0 then
          inc(result);
      end
      else if Pos('data_dir: ',line)=1 then
      begin
        StrCopy(data_dir, @line[11], SizeOf(data_dir));
      end
      else
        continue; // skip unrecognized lines
    end;
    CloseFile(fp);

    // Unneccessary area release of filelist
    filelist_compact();

    // Resource check
    grfio_resourcecheck();
  end;
end;

initialization
  hashinit();  // hash table initialization

end.
