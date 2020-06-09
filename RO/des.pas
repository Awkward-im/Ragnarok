// Copyright (c) Athena Dev Teams - Licensed under GNU GPL
// For more information, see LICENCE in the main folder
unit des;

interface

type
  PBIT64 = ^TBIT64;
  TBIT64 = array [0..7] of byte;

procedure des_decrypt_block(block:PBIT64);
procedure des_decrypt(data:PByte; size:integer);


implementation

/// DES (Data Encryption Standard) algorithm, modified version.
/// @see http://www.eathena.ws/board/index.php?autocom=bugtracker&showbug=5099.
/// @see http://en.wikipedia.org/wiki/Data_Encryption_Standard
/// @see http://en.wikipedia.org/wiki/DES_supplementary_material


/// Bitmask for accessing individual bits of a byte.
const
  mask:array [0..7] of byte = (
	  $80, $40, $20, $10, $08, $04, $02, $01
  );


const
	ip_table:array [0..63] of byte = (
		58, 50, 42, 34, 26, 18, 10,  2,
		60, 52, 44, 36, 28, 20, 12,  4,
		62, 54, 46, 38, 30, 22, 14,  6,
		64, 56, 48, 40, 32, 24, 16,  8,
		57, 49, 41, 33, 25, 17,  9,  1,
		59, 51, 43, 35, 27, 19, 11,  3,
		61, 53, 45, 37, 29, 21, 13,  5,
		63, 55, 47, 39, 31, 23, 15,  7
	);

/// Initial permutation (IP).
procedure IP(src:PBIT64);
var
  tmp:TBIT64;
  i:integer;
  j:byte;
begin
	FillChar(tmp,SizeOf(tmp),0);

	for i:=0 to High(ip_table) do
	begin
		j:=ip_table[i]-1;
		if (src^[(j shr 3) and 7] and mask[j and 7])<>0 then
			tmp[(i shr 3) and 7]:=tmp[(i shr 3) and 7] or mask[i and 7];
	end;

	src^:=tmp;
end;


const
	fp_table:array [0..63] of byte = (
		40,  8, 48, 16, 56, 24, 64, 32,
		39,  7, 47, 15, 55, 23, 63, 31,
		38,  6, 46, 14, 54, 22, 62, 30,
		37,  5, 45, 13, 53, 21, 61, 29,
		36,  4, 44, 12, 52, 20, 60, 28,
		35,  3, 43, 11, 51, 19, 59, 27,
		34,  2, 42, 10, 50, 18, 58, 26,
		33,  1, 41,  9, 49, 17, 57, 25
	);

/// Final permutation (IP^-1).
procedure FP(src:PBIT64);
var
  tmp:TBIT64;
  i:integer;
  j:byte;
begin
	FillChar(tmp,SizeOf(tmp),0);

	for i:=0 to High(fp_table) do
	begin
		j:=fp_table[i]-1;
		if (src^[(j shr 3) and 7] and mask[j and 7])<>0 then
			tmp[(i shr 3) and 7]:=tmp[(i shr 3) and 7] or mask[i and 7];
	end;

	src^:=tmp;
end;


const
	expand_table:array [0..47] of byte = (
		32,  1,  2,  3,  4,  5,
		 4,  5,  6,  7,  8,  9,
		 8,  9, 10, 11, 12, 13,
		12, 13, 14, 15, 16, 17,
		16, 17, 18, 19, 20, 21,
		20, 21, 22, 23, 24, 25,
		24, 25, 26, 27, 28, 29,
		28, 29, 30, 31, 32,  1
	);

/// Expansion (E).
/// Expands upper four 8-bits (32b) into eight 6-bits (48b).
procedure E(src:PBIT64);
var
  tmp:TBIT64;
  i:integer;
  j:byte;
begin
	FillChar(tmp,SizeOf(tmp),0);

  if false then
  begin// original
  	for i:=0 to High(expand_table) do
  	begin
  		j:=expand_table[i]-1;
  		if (src^[(j div 8)+4] and mask[j mod 8])<>0 then
  			tmp[i div 6+0]:=tmp[i div 6+0] or mask[i mod 6];
  	end;
  end
  else
  begin// optimized
  	tmp[0]:=((src^[7] shl 5) or (src^[4] shr 3)) and $3F;	// ..0 vutsr
  	tmp[1]:=((src^[4] shl 1) or (src^[5] shr 7)) and $3F;	// ..srqpo n
  	tmp[2]:=((src^[4] shl 5) or (src^[5] shr 3)) and $3F;	// ..o nmlkj
  	tmp[3]:=((src^[5] shl 1) or (src^[6] shr 7)) and $3F;	// ..kjihg f
  	tmp[4]:=((src^[5] shl 5) or (src^[6] shr 3)) and $3F;	// ..g fedcb
  	tmp[5]:=((src^[6] shl 1) or (src^[7] shr 7)) and $3F;	// ..cba98 7
  	tmp[6]:=((src^[6] shl 5) or (src^[7] shr 3)) and $3F;	// ..8 76543
  	tmp[7]:=((src^[7] shl 1) or (src^[4] shr 7)) and $3F;	// ..43210 v
  end;

	src^:=tmp;
end;


const
	tp_table: array [0..31] of byte = (
		16,  7, 20, 21,
		29, 12, 28, 17,
		 1, 15, 23, 26,
		 5, 18, 31, 10,
		 2,  8, 24, 14,
		32, 27,  3,  9,
		19, 13, 30,  6,
		22, 11,  4, 25
	);

/// Transposition (P-BOX).
procedure TP(src:PBIT64);
var
  tmp:TBIT64;
  i:integer;
  j:byte;
begin
	FillChar(tmp,SizeOf(tmp),0);

	for i:=0 to High(tp_table) do
	begin
		j:=tp_table[i]-1;
		if (src^[(j shr 3)+0] and mask[j and 7])<>0 then
			tmp[(i shr 3)+4]:=tmp[(i shr 3)+4] or mask[i and 7];
	end;

	src^:=tmp;
end;


const
	s_table: array [0..3,0..63] of byte = (
		  (
			$EF, $03, $41, $FD, $D8, $74, $1E, $47,  $26, $EF, $FB, $22, $B3, $D8, $84, $1E,
			$39, $AC, $A7, $60, $62, $C1, $CD, $BA,  $5C, $96, $90, $59, $05, $3B, $7A, $85,
			$40, $FD, $1E, $C8, $E7, $8A, $8B, $21,  $DA, $43, $64, $9F, $2D, $14, $B1, $72,
			$F5, $5B, $C8, $B6, $9C, $37, $76, $EC,  $39, $A0, $A3, $05, $52, $6E, $0F, $D9
		),(
			$A7, $DD, $0D, $78, $9E, $0B, $E3, $95,  $60, $36, $36, $4F, $F9, $60, $5A, $A3,
			$11, $24, $D2, $87, $C8, $52, $75, $EC,  $BB, $C1, $4C, $BA, $24, $FE, $8F, $19,
			$DA, $13, $66, $AF, $49, $D0, $90, $06,  $8C, $6A, $FB, $91, $37, $8D, $0D, $78,
			$BF, $49, $11, $F4, $23, $E5, $CE, $3B,  $55, $BC, $A2, $57, $E8, $22, $74, $CE
		),(
			$2C, $EA, $C1, $BF, $4A, $24, $1F, $C2,  $79, $47, $A2, $7C, $B6, $D9, $68, $15,
			$80, $56, $5D, $01, $33, $FD, $F4, $AE,  $DE, $30, $07, $9B, $E5, $83, $9B, $68,
			$49, $B4, $2E, $83, $1F, $C2, $B5, $7C,  $A2, $19, $D8, $E5, $7C, $2F, $83, $DA,
			$F7, $6B, $90, $FE, $C4, $01, $5A, $97,  $61, $A6, $3D, $40, $0B, $58, $E6, $3D
		),(
			$4D, $D1, $B2, $0F, $28, $BD, $E4, $78,  $F6, $4A, $0F, $93, $8B, $17, $D1, $A4,
			$3A, $EC, $C9, $35, $93, $56, $7E, $CB,  $55, $20, $A0, $FE, $6C, $89, $17, $62,
			$17, $62, $4B, $B1, $B4, $DE, $D1, $87,  $C9, $14, $3C, $4A, $7E, $A8, $E2, $7D,
			$A0, $9F, $F6, $5C, $6A, $09, $8D, $F0,  $0F, $E3, $53, $25, $95, $36, $28, $CB
		)
	);

/// Substitution boxes (S-boxes).
/// NOTE: This implementation was optimized to process two nibbles in one step (twice as fast).
procedure SBOX(src:PBIT64);
var
  tmp:TBIT64;
  i:integer;
begin
	FillChar(tmp,SizeOf(tmp),0);

	for i:=0 to High(s_table) do
	begin
		tmp[i]:=(s_table[i][src^[i*2+0]] and $F0) or
		        (s_table[i][src^[i*2+1]] and $0F);
	end;

	src^:=tmp;
end;


/// DES round function.
/// XORs src[0..3] with TP(SBOX(E(src[4..7]))).
procedure RoundFunction(src:PBIT64);
var
  tmp:TBIT64;
begin
	tmp:=src^;

	E   (@tmp);
	SBOX(@tmp);
	TP  (@tmp);

	src^[0]:=src^[0] xor tmp[4];
	src^[1]:=src^[1] xor tmp[5];
	src^[2]:=src^[2] xor tmp[6];
	src^[3]:=src^[3] xor tmp[7];
end;


procedure des_decrypt_block(block:PBIT64);
begin
	IP(block);
	RoundFunction(block);
	FP(block);
end;


procedure des_decrypt(data:pByte; size:integer);
var
  p:PBIT64;
  i:integer;
begin
	p:=pointer(data);
	i:=0;
	while i*8<size do
	begin
		des_decrypt_block(p);
		inc(i,8);
	end;
end;

end.
