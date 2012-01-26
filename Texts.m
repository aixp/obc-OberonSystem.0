MODULE Texts;

	(*
		A. V. Shiryaev, 2012.01
		Based on original ETH implementation

		Objects not implemented (ignored)
		Load Oberon files (extract ASCII text only)
		Store ASCII text only
	*)

(* ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/ *)

(** The Texts module implements the text abstract data type. Texts are sequences of
characters and objects, with different colors, different fonts, and vertical offsets.
*)

IMPORT Files:=OFiles, Reals, Display;

CONST
	(** Scanner symbol classes.*)
	Inval* = 0;			 (** Invalid symbol. *)
	Name* = 1;		(** Name s (of length len).*)
	String* = 2;		(** Quoted string s (length len). *)
	Int* = 3;			 (** Integer i (decimal or hexadecimal). *)
	Real* = 4;			(** Real number x. *)
	LongReal* = 5;	(** Long real number y. *)
	Char* = 6;			(** Special character c. *)

	TAB = 9X; CR = 0DX; LF = 0AX;
	OldTextBlockId = 1X; OldTextSpex = 0F0X;

	(* TextBlock = TextBlockId type hlen run {run} 0 tlen {AsciiCode} [font block].
		run = font [name] col voff len. *)

	BufSize = 64;

TYPE
	Piece = POINTER TO PieceDesc;

	PieceDesc = RECORD
		f: Files.File;
		off: INTEGER;
		len: INTEGER;
		prev, next: Piece
	END;

	Text* = POINTER TO TextDesc;

	TextDesc* = RECORD
		len-: INTEGER;	(** Text consists of characters 0 to T.len - 1. *)
		trailer: Piece;
		org: INTEGER; (*cache*)
		pce: Piece;
		stamp: INTEGER (* A. V. Shiryaev: Object field *)
	END;

	UpdateMsg* = RECORD (Display.FrameMsg)	(** Message broadcast to indicate that part of a text changed. *)
		text-: Text;	(** The text that changed. *)
		beg-, end-, len-: INTEGER	(** Change location. *)
	END;

	Reader* = RECORD	(** Character-wise reader of a text stream. *)
		ref: Piece;
		T: Text;
		org: INTEGER;
		off: INTEGER;
		R: Files.Rider;
		stamp: INTEGER;
		buf: ARRAY BufSize OF CHAR;
		bufpos, buflen: INTEGER;
		eot-: BOOLEAN	(** Reader has reached end of the text stream. *)
	END;

	Scanner* = RECORD (Reader)	(** Scanner for symbol streams. *)
		nextCh-: CHAR;	(** Character immediately following the last symbol scanned. *)
		line-: SHORTINT;	(** # carriage returns scanned so far. *)
		class-: SHORTINT;	(** Scan result: Int, Real, String etc. *)
		i-: INTEGER;
		x-: REAL;
		y-: LONGREAL;
		c-: CHAR;
		len-: SHORTINT;	(** Length of name or string scanned. *)
		s*: ARRAY 256 OF CHAR
	END;

	Buffer* = POINTER TO BufDesc;	(** Temporary container of text stretches. *)

	BufDesc* = RECORD
		len-: INTEGER;	(** # characters in buffer. *)
		header, last: Piece
	END;

	Writer* = RECORD	(** Used to write a stream of textual data in a buffer. *)
		R: Files.Rider;
		buf-: Buffer;	(** Associated buffer. *)
	END;

VAR TextBlockId- (** First character of a text block. *), DocBlockId, NoSpex, NoSpex2, TextSpex: CHAR;
	Wfile: Files.File; R: Files.Rider; DelBuf: Buffer;
	nameChars-: ARRAY 256 OF BOOLEAN;

(** Load text block from ASCII file f to text T. *)
PROCEDURE LoadAscii* (T: Text; f: Files.File);
	VAR Q, q: Piece; len: INTEGER;
BEGIN len := Files.Length(f);
	NEW(Q); Q.f := Wfile; Q.off := 0; Q.len := 1;
	NEW(q); q.f := f; q.off := 0; q.len := len;
	Q.next := q; q.prev := Q; q.next := Q; Q.prev := q;
	T.trailer := Q; T.len := len;
	T.org := -1; T.pce := T.trailer (*init cache*)
END LoadAscii;

PROCEDURE ReadDocHeader(VAR R: Files.Rider; VAR ch: CHAR);
	VAR len: INTEGER; x, y, w, h: SHORTINT; name: ARRAY 256 OF CHAR;
BEGIN Files.Read(R, ch);
	Files.ReadString(R, name);
	Files.ReadInt(R, x); Files.ReadInt(R, y);
	Files.ReadInt(R, w); Files.ReadInt(R, h);
	Files.Read(R, ch);
	IF ch = 0F7X THEN	(* skip meta info *)
		Files.Read(R, ch);
		IF ch = 08X THEN
			Files.ReadLInt(R, len); Files.Set(R, Files.Base(R), Files.Pos(R) + len); Files.Read(R, ch)
		END
	END
END ReadDocHeader;

(** Load text block from file f at position pos to text T (assumes that the text id has been read already). len returns length. *)
PROCEDURE Load* (T: Text; f: Files.File; pos: INTEGER; VAR len: INTEGER);
	VAR
		R: Files.Rider;
		Q, q: Piece;
		hlen, tlen: INTEGER;
		type: CHAR;
BEGIN
	Files.Set(R, f, pos);
	Files.Read(R, type);
	Files.ReadLInt(R, hlen);
	Files.Set(R, f, pos - 1 + hlen - 4);
	Files.ReadLInt(R, tlen);

	NEW(Q); Q.f := Wfile; Q.off := 0; Q.len := 1;
	NEW(q); q.f := f; q.off := pos - 1 + hlen; q.len := tlen;
	Q.next := q; q.prev := Q; q.next := Q; Q.prev := q;
	T.trailer := Q; T.len := tlen;
	T.org := -1; T.pce := T.trailer; (*init cache*)

	len := hlen - 1 + tlen
END Load;

(** Store text T on disk file f at position pos. Writes the first id character too. len is the number of bytes written. *)
PROCEDURE Store* (T: Text; f: Files.File; pos: INTEGER; VAR len: INTEGER);
	VAR p: Piece;
		R, W: Files.Rider;
		m: INTEGER;
		ch: CHAR;
BEGIN
	Files.Set(W, f, pos);
	p := T.trailer.next;
	WHILE p # T.trailer DO
		Files.Set(R, p.f, p.off); m := p.len;
		WHILE m # 0 DO Files.Read(R, ch);
			Files.Write(W, ch); DEC(m)
		END;
		p := p.next
	END;
	len := T.len
END Store;

PROCEDURE GenNew (T: Text);
	VAR Q: Piece;
BEGIN
	NEW(Q); Q.f := Wfile; Q.off := 0; Q.len := 1;
	Q.next := Q; Q.prev := Q;
	T.trailer := Q; T.len := 0;
	T.org := -1; T.pce := T.trailer (*init cache*)
END GenNew;

(** Open text T from file specified by name. A new text is opened when name = "". *)
PROCEDURE Open* (T: Text; CONST name: ARRAY OF CHAR);
	VAR f: Files.File; R: Files.Rider; len: INTEGER; ch: CHAR;
BEGIN
	f := Files.Old(name);
	IF f # NIL THEN
		Files.Set(R, f, 0); Files.Read(R, ch);
		IF ch = DocBlockId THEN ReadDocHeader(R, ch) END;
		IF (ch = TextBlockId) OR (ch = OldTextBlockId) THEN Load(T, f, Files.Pos(R), len)
		ELSE LoadAscii(T, f)
		END
	ELSE GenNew(T)
	END
END Open;

PROCEDURE FindPiece (T: Text; pos: INTEGER; VAR org: INTEGER; VAR p: Piece);
	VAR n: INTEGER;
BEGIN
	IF pos < T.org THEN T.org := -1; T.pce := T.trailer END;
	org := T.org; p := T.pce; (*from cache*)
	n := 0;
	WHILE pos >= org + p.len DO org := org + p.len; p := p.next; INC(n) END;
	IF n > 50 THEN T.org := org; T.pce := p END
END FindPiece;

PROCEDURE SplitPiece (p: Piece; off: INTEGER; VAR pr: Piece);
	VAR q: Piece;
BEGIN
	IF off > 0 THEN NEW(q);
		q.len := p.len - off;
		q.f := p.f; q.off := p.off + off;
		p.len := off;
		q.next := p.next; p.next := q;
		q.prev := p; q.next.prev := q;
		pr := q
	ELSE pr := p
	END
END SplitPiece;

(** Insert buffer B in text T position pos. B is emptied. *)
PROCEDURE Insert* (T: Text; pos: INTEGER; B: Buffer);
	VAR pl, pr, p, qb, qe: Piece; org: INTEGER; M: UpdateMsg;
BEGIN
	FindPiece(T, pos, org, p); SplitPiece(p, pos - org, pr);
	IF T.org >= org THEN (*adjust cache*)
		T.org := org - p.prev.len; T.pce := p.prev
	END;
	pl := pr.prev; qb := B.header.next;
	IF (qb # NIL) & (qb.f = pl.f) & (qb.off = pl.off + pl.len) THEN
		pl.len := pl.len + qb.len; qb := qb.next
	END;
	IF qb # NIL THEN
		qe := B.last; qb.prev := pl; pl.next := qb; qe.next := pr; pr.prev := qe
	END;
	T.len := T.len + B.len;
	M.text := T; M.F := NIL; M.beg := pos; M.end := pos; M.len := B.len;
	B.last := B.header; B.last.next := NIL; B.len := 0;
	Display.Broadcast(M); T.stamp := M.stamp
END Insert;

(** Append buffer to the end of text T. B is emptied. *)
PROCEDURE Append* (T: Text; B: Buffer);
BEGIN Insert(T, T.len, B)
END Append;

(** Delete text stretch [beg, end[. *)
PROCEDURE Delete* (T: Text; beg, end: INTEGER);
	VAR pb, pe, pbr, per: Piece; orgb, orge: INTEGER; M: UpdateMsg;
BEGIN
	IF beg < end THEN
		FindPiece(T, beg, orgb, pb); SplitPiece(pb, beg - orgb, pbr);
		FindPiece(T, end, orge, pe); SplitPiece(pe, end - orge, per);
		IF T.org >= orgb THEN (*adjust cache*)
			T.org := orgb - pb.prev.len; T.pce := pb.prev
		END;
		DelBuf.header.next := pbr; DelBuf.last := per.prev;
		DelBuf.last.next := NIL; DelBuf.len := end - beg;
		per.prev := pbr.prev; pbr.prev.next := per;
		T.len := T.len - end + beg;
		M.text := T; M.F := NIL; M.beg := beg; M.end := end; M.len := 0;
		Display.Broadcast(M); T.stamp := M.stamp
	END
END Delete;

(** Replace [beg, end[ of T with contents of buffer B. B is emptied. *)
PROCEDURE Replace* (T: Text; beg, end: INTEGER; B: Buffer);
	VAR M: UpdateMsg; pb, pe, pbr, per, pl, qb, qe: Piece; orgb, orge: INTEGER;
BEGIN
	IF beg < end THEN
		FindPiece(T, beg, orgb, pb); SplitPiece(pb, beg - orgb, pbr);
		FindPiece(T, end, orge, pe); SplitPiece(pe, end - orge, per);
		IF T.org >= orgb THEN (*adjust cache*)
			T.org := orgb - pb.prev.len; T.pce := pb.prev
		END;
		DelBuf.header.next := pbr; DelBuf.last := per.prev;
		DelBuf.last.next := NIL; DelBuf.len := end - beg;
		per.prev := pbr.prev; pbr.prev.next := per;
		pl := pbr.prev; qb := B.header.next;
		IF (qb # NIL) & (qb.f = pl.f) & (qb.off = pl.off + pl.len) THEN
			pl.len := pl.len + qb.len; qb := qb.next
		END;
		IF qb # NIL THEN
			qe := B.last; qb.prev := pl; pl.next := qb; qe.next := per; per.prev := qe
		END;
		T.len := T.len - end + beg + B.len;
		M.text := T; M.F := NIL; M.beg := beg; M.end := end; M.len := B.len;
		B.last := B.header; B.last.next := NIL; B.len := 0;
		Display.Broadcast(M); T.stamp := M.stamp
	END
END Replace;

(** Open a new text buffer B. *)
PROCEDURE OpenBuf* (B: Buffer);
BEGIN NEW(B.header); (*null piece*)
	B.last := B.header; B.len := 0
END OpenBuf;

(** Save stretch [beg, end[ of T in buffer B. *)
PROCEDURE Save* (T: Text; beg, end: INTEGER; B: Buffer);
	VAR p, q, qb, qe: Piece; org: INTEGER;
BEGIN
	IF beg < end THEN
		FindPiece(T, beg, org, p);
		NEW(qb); qb^ := p^;
		qb.len := qb.len - (beg - org);
		qb.off := qb.off + (beg - org);
		qe := qb;
		WHILE end > org + p.len DO
			org := org + p.len; p := p.next;
			NEW(q); q^ := p^;
			qe.next := q; q.prev := qe; qe := q
		END;
		qe.next := NIL; qe.len := qe.len - (org + p.len - end);
		B.last.next := qb; qb.prev := B.last; B.last := qe;
		B.len := B.len + (end - beg)
	END
END Save;

(** Append copy of source buffer SB to destination buffer DB. *)
PROCEDURE Copy* (SB, DB: Buffer);
	VAR Q, q, p: Piece;
BEGIN
	p := SB.header; Q := DB.last;
	WHILE p # SB.last DO p := p.next;
		NEW(q); q^ := p^; Q.next := q; q.prev := Q; Q := q
	END;
	DB.last := Q; DB.len := DB.len + SB.len
END Copy;

(** Recall previously deleted text. *)
PROCEDURE Recall* (VAR B: Buffer); (*deleted text*)
BEGIN Copy(DelBuf, B)
END Recall;

(** Open text reader R and set it up at position pos in text T. *)
PROCEDURE OpenReader* (VAR R: Reader; T: Text; pos: INTEGER);
	VAR p: Piece; org: INTEGER;
BEGIN
	FindPiece(T, pos, org, p); R.T := T;
	R.stamp := T.stamp; R.bufpos := 0; R.buflen := 0;
	R.ref := p; R.org := org; R.off := pos - org;
	Files.Set(R.R, p.f, p.off + R.off); R.eot := p.f = Wfile
END OpenReader;

(** Read next character into ch. R.eot is set when the last character is read. The fields lib, voff and col of R give
information about the last character read. *)
PROCEDURE Read* (VAR R: Reader; VAR ch: CHAR);
	VAR ref: Piece;
BEGIN
	IF (R.stamp # R.T.stamp) OR (R.bufpos >= R.buflen) THEN
		IF R.stamp = R.T.stamp THEN
			ref := R.ref; R.bufpos := 0; R.buflen := ref.len-R.off;
			IF R.buflen <= 0 THEN
				R.org := R.org + ref.len; R.off := 0; ref := ref.next; R.ref := ref;
				Files.Set(R.R, ref.f, ref.off);
				R.buflen := ref.len; IF ref.f = Wfile THEN R.eot := TRUE END
			END;
			IF R.buflen > BufSize THEN R.buflen := BufSize END;
			Files.ReadBytes(R.R, R.buf, R.buflen)
		ELSE
			OpenReader(R, R.T, R.org + R.off);
			Read(R, ch); RETURN
		END
	END;
	ch := R.buf[R.bufpos]; INC(R.bufpos); INC(R.off)
END Read;

(** Return reader's position within the text. *)
PROCEDURE Pos* (VAR R: Reader): INTEGER;
BEGIN RETURN R.org + R.off
END Pos;

(** Open text scanner S and set it up at position pos in text T. *)
PROCEDURE OpenScanner* (VAR S: Scanner; T: Text; pos: INTEGER);
BEGIN OpenReader(S, T, pos); S.line := 0; S.class := Inval; Read(S, S.nextCh)
END OpenScanner;

(* Scanners --------------- NW --------------- *)

(*IEEE floating-point formats	(BM 1992.1.1): (-1)^s * 1.m * 2^(e-e0), where

								s				 e				 e0				m
		REAL			 1-bit	 8-bit biased		127	1+23-bit explicit
		LONGREAL	 1-bit	 11-bit biased	 1023	 1+52-bit explicit*)

(** Read the next symbol. Whitespace is ignored. CR increments the line counter. *)
PROCEDURE Scan* (VAR S: Scanner);
	CONST maxD = 256; (* fixed size: maxD <= LEN(S.s)! *)
	VAR ch, E: CHAR;
		neg, negE, hex, sign: BOOLEAN;
		i, j, h, e, k, k1, k2, k3: INTEGER;
		y: LONGREAL;
		d: ARRAY maxD OF CHAR;
(* A. V. Shiryaev: NOTE: S.lib # NIL; S.lib IS Fonts.Font *)
BEGIN ch := S.nextCh; i := 0;
	LOOP
		IF ch = CR THEN INC(S.line)
		ELSIF (ch # " ") & (ch # TAB) & (ch # LF) THEN EXIT
		END ;
		Read(S, ch)
	END;
	IF ("A" <= CAP(ch)) & (CAP(ch) <= "Z") OR (ch = ".") OR (ch = "/") (*OR (ch = ":")*) THEN (*name*)
		REPEAT
			S.s[i] := ch; INC(i); Read(S, ch)
		UNTIL ~(nameChars[ORD(ch)]) OR (i = LEN(S.s)-1);
		S.s[i] := 0X;
		IF (i = 1) & ((CAP(S.s[0]) < "A") OR (CAP(S.s[0]) > "Z")) THEN
			S.c := S.s[0]; S.class := Char
		ELSE
			S.len := SHORT(i); S.class := Name
		END
	ELSIF ch = 22X THEN (*literal string*)
		Read(S, ch);
		WHILE (ch # 22X) & (ch >= " ") & (i # LEN(S.s)-1) DO
			S.s[i] := ch; INC(i); Read(S, ch)
		END;
		WHILE (ch # 22X) & (ch >= " ") DO Read(S, ch) END;
		S.s[i] := 0X; S.len := SHORT(i); Read(S, ch); S.class := String
	ELSE
		IF ch = "-" THEN sign := TRUE; neg := TRUE; Read(S, ch)
		ELSIF ch = "+" THEN sign := TRUE; neg := FALSE; Read(S, ch)
		ELSE sign := FALSE; neg := FALSE
		END;
		IF ("0" <= ch) & (ch <= "9") THEN (*number*)
			hex := FALSE; j := 0;
			LOOP
				d[i] := ch; INC(i); Read(S, ch);
				IF (ch < "0") OR (i >= maxD) THEN EXIT END;
				IF "9" < ch THEN
					IF ("A" <= ch) & (ch <= "F") THEN hex := TRUE; ch := CHR(ORD(ch)-7)
					ELSIF ("a" <= ch) & (ch <= "f") THEN hex := TRUE; ch := CHR(ORD(ch)-27H)
					ELSE EXIT
					END
				END
			END;
			IF (ch = "H") THEN (*hex number*)
				Read(S, ch); S.class := Int;
				IF i-j > 8 THEN j := i-8 END ;
				k := ORD(d[j]) - 30H; INC(j);
				IF (i-j = 7) & (k >= 8) THEN DEC(k, 16) END ;
				WHILE j < i DO k := k*10H + (ORD(d[j]) - 30H); INC(j) END ;
				IF neg THEN S.i := -k ELSE S.i := k END
			ELSIF (ch = ".") THEN (*read real*)
				Read(S, ch); h := i;
				WHILE ("0" <= ch) & (ch <= "9") & (i < maxD) DO
					d[i] := ch; INC(i); Read(S, ch)
				END;
				(*-------- begin floating-point handling BM 1993.3.10 -----------------------------------*)
				WHILE i MOD 8 # 0 DO d[i] := "0"; INC(i) END;
				j := 0; k := 0; k1 := 0; k2 := 0; k3 := 0; (* store digits 0..7, 8..15, 16..23, 24..31 in k, k1, k2, k3 *)
				WHILE j < 8 DO k := k*10 + ORD(d[j]) - ORD("0"); INC(j) END;
				IF 8 < i THEN
					WHILE j < 16 DO k1 := k1*10 + ORD(d[j]) - ORD("0"); INC(j) END
				END;
				IF 16 < i THEN
					WHILE j < 24 DO k2 := k2*10 + ORD(d[j]) - ORD("0"); INC(j) END
				END;
				IF 24 < i THEN
					WHILE j < 32 DO k3 := k3*10 + ORD(d[j]) - ORD("0"); INC(j) END
				END;
				e := 0; E := ch;
				IF ((E = "D") OR (E = "E")) THEN Read(S, ch);
					IF (ch = "-") THEN negE := TRUE; Read(S, ch)
					ELSE negE := FALSE;
						IF (ch = "+") THEN Read(S, ch) END
					END;
					WHILE ("0" <= ch) & (ch <= "9") DO
						e := e*10 + ORD(ch) - ORD("0");
						Read(S, ch)
					END;
					IF negE THEN e := - e END
				END;
				y := k3*Reals.Ten(-32) + k2*Reals.Ten(-24); y := y + k1*Reals.Ten(-16);
				IF ABS(e+h) < 308 THEN y := (y + k*Reals.Ten(-8)) / Reals.Ten(-e-h)
				ELSE y := (y + k*Reals.Ten(-8)) * Reals.Ten(h);
					IF (e <= 308-32) OR (e <= 308) & (y < MAX(LONGREAL) / Reals.Ten(e)) THEN y := y * Reals.Ten(e)
						ELSE y := MAX(LONGREAL)
					END
				END;
				IF E = "D" THEN
					IF y = MAX(LONGREAL) THEN S.class := Inval (* NaN *)
					ELSE S.class := LongReal;
						IF neg THEN S.y := - y ELSE S.y := y END;
						IF Reals.ExpoL(S.y) = 0 THEN S.y := 0 END
					END
				ELSIF MAX(REAL) < y THEN S.class:= Inval (* NaN *)
				ELSE S.class := Real;
					IF neg THEN S.x := SHORT(- y) ELSE S.x := SHORT(y) END;
					IF Reals.Expo(S.x) = 0 THEN S.x := 0 END
				END;
				(*-------- end floating-point handling BM 1993.3.10 -----------------------------------*)
				IF hex THEN S.class := Inval END
			ELSE (*decimal integer*)
				S.class := Int; k := 0;
				WHILE (j # i) & ((k < MAX(INTEGER) DIV 10) OR
					(k = MAX(INTEGER) DIV 10) & ((ORD(d[j]) - 30H) <= MAX(INTEGER) MOD 10)) DO (*JG*)
					k := k*10 + (ORD(d[j]) - 30H); INC(j)
				END;
				IF j # i THEN S.class := Inval
				ELSE
					IF neg THEN S.i := -k ELSE S.i := k END;
					IF hex THEN S.class := Inval ELSE S.class := Int END
				END
			END
		ELSE S.class := Char;
			IF sign THEN IF neg THEN S.c := "-" ELSE S.c := "+" END
			ELSE S.c := ch; Read(S, ch)
			END
		END
	END;
	S.nextCh := ch
END Scan;

(** Open a new writer W. *)
PROCEDURE OpenWriter* (VAR W: Writer);
BEGIN
	NEW(W.buf); OpenBuf(W.buf);
	Files.Set(W.R, Files.New(""), 0)
END OpenWriter;

(** Write character ch to writer W's buffer. *)
  PROCEDURE Write* (VAR W: Writer; ch: CHAR);
    VAR p, q: Piece;
  BEGIN p := W.buf.last;
    IF Files.Base(W.R) # p.f THEN
      NEW(q);
      q.f := Files.Base(W.R); q.off := Files.Pos(W.R); q.len := 0;
      q.next := NIL; p.next := q; q.prev := p; p := q;
      W.buf.last := p
    END;
    Files.Write(W.R, ch);
    INC(p.len); INC(W.buf.len)
  END Write;

(** Write an end-of-line character to W's buffer. *)
PROCEDURE WriteLn* (VAR W: Writer);
BEGIN Write(W, CR)
END WriteLn;

(** Write string s to W's buffer. *)
PROCEDURE WriteString* (VAR W: Writer; CONST s: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE s[i] # 0X DO Write(W, s[i]); INC(i) END
END WriteString;

(** Write integer x to W's buffer. Spaces are padded to the left until the number field is at least n characters long. *)
PROCEDURE WriteInt* (VAR W: Writer; x, n: INTEGER);
	VAR i, x0: INTEGER;
		a: ARRAY 10 OF CHAR;
BEGIN i := 0;
	IF x < 0 THEN
		IF x = MIN(INTEGER) THEN WriteString(W, " -2147483648"); RETURN
		ELSE DEC(n); x0 := -x
		END
	ELSE x0 := x
	END;
	REPEAT
		a[i] := CHR(x0 MOD 10 + 30H); x0 := x0 DIV 10; INC(i)
	UNTIL x0 = 0;
	WHILE n > i DO Write(W, " "); DEC(n) END;
	IF x < 0 THEN Write(W, "-") END;
	REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
END WriteInt;

(** Write a hexadecimal representation of x to W's buffer. *)
PROCEDURE WriteHex* (VAR W: Writer; x: INTEGER);
	VAR i, y: INTEGER;
		a: ARRAY 10 OF CHAR;
BEGIN i := 0; Write(W, " ");
	REPEAT y := x MOD 10H;
		IF y < 10 THEN a[i] := CHR(y + 30H) ELSE a[i] := CHR(y + 37H) END;
		x := x DIV 10H; INC(i)
	UNTIL i = 8;
	REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
END WriteHex;

(** Write the hexadecimal representation of x to W's buffer. *)
PROCEDURE WriteRealHex* (VAR W: Writer; x: REAL);
BEGIN (* BM 1991.12.25 *) WriteHex(W, Reals.Int(x))
END WriteRealHex;

(** Write the hexadecimal representation of x to W's buffer. *)
PROCEDURE WriteLongRealHex* (VAR W: Writer; x: LONGREAL);
	VAR h, l: INTEGER; (* BM 1991.12.25 *)
BEGIN Reals.IntL(x, h, l); WriteHex(W, h); WriteHex(W, l)
END WriteLongRealHex;

(** Write real x to W's buffer using n character positions. *)
PROCEDURE WriteReal* (VAR W: Writer; x: REAL; n: INTEGER);
	(* BM 1993.4.22. Do not simplify rounding! *)
	VAR e, h, i: INTEGER; y: LONGREAL; z: REAL; d: ARRAY 8 OF CHAR;
BEGIN
	e:= Reals.Expo(x);
	IF e = 255 THEN
		WHILE n > 8 DO Write(W, " "); DEC(n) END;
		h := Reals.NaNCode(x);
		IF h # 0 THEN WriteString(W, "	 NaN")
		ELSIF x < 0 THEN WriteString(W, "	-INF")
		ELSE WriteString(W, "		INF")
		END
	ELSE
		IF n <= 8 THEN n := 1 ELSE DEC(n, 7) END;
		REPEAT Write(W, " "); DEC(n) UNTIL n <= 7; (* 0 <= n <= 7 fraction digits *)
		IF (e # 0) & (x < 0) THEN Write(W, "-"); x := - x ELSE Write(W, " ") END;
		IF e = 0 THEN h := 0 (* no denormals *)
		ELSE e := (e - 127) * 301 DIV 1000; (* ln(2)/ln(10) = 0.301029996 *)
			IF e < 38 THEN z := SHORT(Reals.Ten(e+1));
				IF x >= z THEN y := LONG(x)/LONG(z); INC(e) ELSE y := x * Reals.Ten(-e) END
			ELSE y := x * Reals.Ten(-38) END;
			IF y >= 10 THEN y := y * Reals.Ten(-1) + 0.5D0 / Reals.Ten(n); INC(e)
			ELSE y := y + 0.5D0 / Reals.Ten(n);
				IF y >= 10 THEN y := y * Reals.Ten(-1); INC(e) END
			END;
			y := y * Reals.Ten(7); h := ENTIER(y)
		END;
		i := 7;
		WHILE i >= 0 DO d[i] := CHR(h MOD 10 + ORD("0")); h := h DIV 10; DEC(i) END;
		Write(W, d[0]); Write(W, "."); i := 1; WHILE i <= n DO Write(W, d[i]); INC(i) END;
		IF e < 0 THEN WriteString(W, "E-"); e := - e ELSE WriteString(W, "E+") END;
		Write(W, CHR(e DIV 10 + ORD("0")));
		Write(W, CHR(e MOD 10 + ORD("0")))
	END
END WriteReal;

(** Write real x in a fixed point notation. n is the overall minimal length for the output field,
f the number of fraction digits following the decimal point, E the fixed exponent (printed only
when E # 0). *)
PROCEDURE WriteRealFix* (VAR W: Writer; x: REAL; n, f, E: INTEGER);
	(* BM 1993.4.22. Do not simplify rounding ! / JG formatting adjusted *)
	VAR e, h, i: INTEGER; r, y: LONGREAL; z: REAL; s: CHAR; d: ARRAY 8 OF CHAR;
BEGIN
	e := Reals.Expo(x);
	IF (e = 255) OR (ABS(E) > 38) THEN
		WHILE n > 8 DO Write(W, " "); DEC(n) END;
		h := Reals.NaNCode(x);
		IF h # 0 THEN WriteString(W, "	 NaN")
		ELSIF x < 0 THEN WriteString(W, "	-INF")
		ELSE WriteString(W, "		INF")
		END
	ELSE
		IF E = 0 THEN DEC(n, 2) ELSE DEC(n, 6) END;
		IF f < 0 THEN f := 0 END;
		IF n < f + 2 THEN n := f + 2 END;
		DEC(n, f);
		IF (e # 0) & (x < 0) THEN s:= "-"; x:= - x ELSE s:= " " END;
		IF e = 0 THEN h := 0; DEC(e, E-1) (* no denormals *)
		ELSE
			e := (e - 127) * 301 DIV 1000; (* ln(2)/ln(10) = 0.301029996 *)
			IF e < 38 THEN z := SHORT(Reals.Ten(e+1));
				IF x >= z THEN y := LONG(x)/LONG(z); INC(e) ELSE y := x * Reals.Ten(-e) END
			ELSE y := x * Reals.Ten(-38) END;
			DEC(e, E-1); i := -(e+f);
			IF i <= 0 THEN r := 5 * Reals.Ten(i) ELSE r := 0 END;
			IF y >= 10 THEN y := y * Reals.Ten(-1) + r; INC(e)
			ELSE y := y + r;
				IF y >= 10 THEN y := y * Reals.Ten(-1); INC(e) END
			END;
			y := y * Reals.Ten(7); h := ENTIER(y)
		END;
		i := 7;
		WHILE i >= 0 DO d[i] := CHR(h MOD 10 + ORD("0")); h := h DIV 10; DEC(i) END;
		IF n <= e THEN n := e + 1 END;
		IF e > 0 THEN WHILE n > e DO Write(W, " "); DEC(n) END;
			Write(W, s); e := 0;
			WHILE n > 0 DO DEC(n);
				IF e < 8 THEN Write(W, d[e]); INC(e) ELSE Write(W, "0") END
			END;
			Write(W, ".")
		ELSE
			WHILE n > 1 DO Write(W, " "); DEC(n) END;
			Write(W, s); Write(W, "0"); Write(W, ".");
			WHILE (0 < f) & (e < 0) DO Write(W, "0"); DEC(f); INC(e) END
		END;
		WHILE f > 0 DO DEC(f);
			IF e < 8 THEN Write(W, d[e]); INC(e) ELSE Write(W, "0") END
		END;
		IF E # 0 THEN
			IF E < 0 THEN WriteString(W, "E-"); E := - E
			ELSE WriteString(W, "E+")
			END;
			Write(W, CHR(E DIV 10 + ORD("0"))); Write(W, CHR(E MOD 10 + ORD("0")))
		END
	END
END WriteRealFix;

(** Write LONGREAL x to W's buffer using n character positions. *)
PROCEDURE WriteLongReal* (VAR W: Writer; x: LONGREAL; n: INTEGER);
	(* BM 1993.4.22. Do not simplify rounding! *)
	VAR e, h, l, i: INTEGER; z: LONGREAL; d: ARRAY 16 OF CHAR;
BEGIN
	e:= Reals.ExpoL(x);
	IF e = 2047 THEN
		WHILE n > 9 DO Write(W, " "); DEC(n) END;
		Reals.NaNCodeL(x, h, l);
		IF (h # 0) OR (l # 0) THEN WriteString(W, "		NaN")
		ELSIF x < 0 THEN WriteString(W, "	 -INF")
		ELSE WriteString(W, "		INF")
		END
	ELSE
		IF n <= 9 THEN n:= 1 ELSE DEC(n, 8) END;
		REPEAT Write(W, " "); DEC(n) UNTIL n <= 15; (* 0 <= n <= 15 fraction digits *)
		IF (e # 0) & (x < 0) THEN Write(W, "-"); x:= - x ELSE Write(W, " ") END;
		IF e = 0 THEN h:= 0; l:= 0 (* no denormals *)
		ELSE e:= (e - 1023) * 301029 DIV 1000000; (* ln(2)/ln(10) = 0.301029996 *)
			z:= Reals.Ten(e+1);
			IF x >= z THEN x:= x/z; INC(e) ELSE x:= x * Reals.Ten(-e) END;
			IF x >= 10 THEN x:= x * Reals.Ten(-1) + 0.5D0 / Reals.Ten(n); INC(e)
			ELSE x:= x + 0.5D0 / Reals.Ten(n);
				IF x >= 10 THEN x:= x * Reals.Ten(-1); INC(e) END
			END;
			x:= x * Reals.Ten(7); h:= ENTIER(x); x:= (x-h) * Reals.Ten(8); l:= ENTIER(x)
		END;
		i:= 15; WHILE i > 7 DO d[i]:= CHR(l MOD 10 + ORD("0")); l:= l DIV 10; DEC(i) END;
		WHILE i >= 0 DO d[i]:= CHR(h MOD 10 + ORD("0")); h:= h DIV 10; DEC(i) END;
		Write(W, d[0]); Write(W, "."); i:= 1; WHILE i <= n DO Write(W, d[i]); INC(i) END;
		IF e < 0 THEN WriteString(W, "D-"); e:= - e ELSE WriteString(W, "D+") END;
		Write(W, CHR(e DIV 100 + ORD("0"))); e:= e MOD 100;
		Write(W, CHR(e DIV 10 + ORD("0"))); Write(W, CHR(e MOD 10 + ORD("0")))
	END
END WriteLongReal;

(** Write LONGREAL x in a fixed point notation. n is the overall minimal length for the output field, f the number of fraction digits following the decimal point, D the fixed exponent (printed only when D # 0). *)
PROCEDURE WriteLongRealFix* (VAR W: Writer; x: LONGREAL; n, f, D: INTEGER);
	(* BM 1993.4.22. Do not simplify rounding! / JG formatting adjusted *)
	VAR e, h, l, i: INTEGER; r, z: LONGREAL; d: ARRAY 16 OF CHAR; s: CHAR;
BEGIN
	e := Reals.ExpoL(x);
	IF (e = 2047) OR (ABS(D) > 308) THEN
		WHILE n > 9 DO Write(W, " "); DEC(n) END;
		Reals.NaNCodeL(x, h, l);
		IF (h # 0) OR (l # 0) THEN WriteString(W, "		NaN")
		ELSIF x < 0 THEN WriteString(W, "	 -INF")
		ELSE WriteString(W, "		INF")
		END
	ELSE
		IF D = 0 THEN DEC(n, 2) ELSE DEC(n, 7) END;
		IF n < 2 THEN n := 2 END;
		IF f < 0 THEN f := 0 END;
		IF n < f + 2 THEN n := f + 2 END;
		DEC(n, f);
		IF (e # 0) & (x < 0) THEN s := "-"; x := - x ELSE s := " " END;
		IF e = 0 THEN h := 0; l := 0; DEC(e, D-1) (* no denormals *)
		ELSE
			e := (e - 1023) * 301029 DIV 1000000; (* ln(2)/ln(10) = 0.301029996 *)
			z := Reals.Ten(e+1);
			IF x >= z THEN x := x/z; INC(e) ELSE x:= x * Reals.Ten(-e) END;
			DEC(e, D-1); i := -(e+f);
			IF i <= 0 THEN r := 5 * Reals.Ten(i) ELSE r := 0 END;
			IF x >= 10 THEN x := x * Reals.Ten(-1) + r; INC(e)
			ELSE x := x + r;
				IF x >= 10 THEN x := x * Reals.Ten(-1); INC(e) END
			END;
			x := x * Reals.Ten(7); h:= ENTIER(x); x := (x-h) * Reals.Ten(8); l := ENTIER(x)
		END;
		i := 15;
		WHILE i > 7 DO d[i] := CHR(l MOD 10 + ORD("0")); l := l DIV 10; DEC(i) END;
		WHILE i >= 0 DO d[i] := CHR(h MOD 10 + ORD("0")); h := h DIV 10; DEC(i) END;
		IF n <= e THEN n := e + 1 END;
		IF e > 0 THEN WHILE n > e DO Write(W, " "); DEC(n) END;
			Write(W, s); e:= 0;
			WHILE n > 0 DO DEC(n);
				IF e < 16 THEN Write(W, d[e]); INC(e) ELSE Write(W, "0") END
			END;
			Write(W, ".")
		ELSE
			WHILE n > 1 DO Write(W, " "); DEC(n) END;
			Write(W, s); Write(W, "0"); Write(W, ".");
			WHILE (0 < f) & (e < 0) DO Write(W, "0"); DEC(f); INC(e) END
		END;
		WHILE f > 0 DO DEC(f);
			IF e < 16 THEN Write(W, d[e]); INC(e) ELSE Write(W, "0") END
		END;
		IF D # 0 THEN
			IF D < 0 THEN WriteString(W, "D-"); D := - D
			ELSE WriteString(W, "D+")
			END;
			Write(W, CHR(D DIV 100 + ORD("0"))); D := D MOD 100;
			Write(W, CHR(D DIV 10 + ORD("0"))); Write(W, CHR(D MOD 10 + ORD("0")))
		END
	END
END WriteLongRealFix;

(** Write the time and date to W's buffer. *)
PROCEDURE WriteDate* (VAR W: Writer; t, d: INTEGER);

	PROCEDURE WritePair(ch: CHAR; x: INTEGER);
	BEGIN Write(W, ch);
		Write(W, CHR(x DIV 10 + 30H)); Write(W, CHR(x MOD 10 + 30H))
	END WritePair;

BEGIN
	WritePair(" ", d MOD 32); WritePair(".", d DIV 32 MOD 16);
	Write(W, ".");	WriteInt(W, 1900 + d DIV 512, 1);
	WritePair(" ", t DIV 4096 MOD 32); WritePair(":", t DIV 64 MOD 64); WritePair(":", t MOD 64)
END WriteDate;

(** Write a SET value to writer W. *)

PROCEDURE WriteSet*(VAR W: Writer; s: SET);
	VAR
		i, last: INTEGER;
		dots: BOOLEAN;
BEGIN
	Write(W, "{"); last := MIN(INTEGER);
	FOR i := MIN(SET) TO MAX(SET) DO
		IF i IN s THEN
			IF last = (i-1) THEN
				IF dots THEN
					WriteString(W, " .. "); dots := FALSE
				END;
				IF (i = MAX(SET)) OR ~((i+1) IN s) THEN
					WriteInt(W, i, 0)
				END
			ELSE
				IF last >= MIN(SET) THEN
					WriteString(W, ", ")
				END;
				WriteInt(W, i, 0); dots := TRUE
			END;
			last := i
		END
	END;
	Write(W, "}")
END WriteSet;

PROCEDURE InitScan;
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO 255 DO nameChars[i] := FALSE END;
	FOR i := 80H TO 96H DO nameChars[i] := TRUE END; (* german characters *)
	FOR i := ORD("0") TO ORD("9") DO nameChars[i] := TRUE END;
	FOR i := ORD("A") TO ORD("Z") DO nameChars[i] := TRUE END;
	FOR i := ORD("a") TO ORD("z") DO nameChars[i] := TRUE END;
	nameChars[ORD("@")] := TRUE; (* mail, compiler *)
	nameChars[ORD(".")] := TRUE;	(* mail, filenames, compiler *)
	nameChars[ORD("/")] := TRUE;	(* filenames *)
	nameChars[ORD(":")] := TRUE;	(* filenames (Mac) *)
	nameChars[ORD("_")] := TRUE
END InitScan;

BEGIN
	TextBlockId := 0F0X; DocBlockId := 0F7X; NoSpex := 0X; TextSpex := 1X; NoSpex2 := 2X;
	Wfile := Files.New(""); Files.Set(R, Wfile, 0); Files.Write(R, 0X);
	NEW(DelBuf); OpenBuf(DelBuf); InitScan()
END Texts.

(** Remarks:

1. Text streams consists of sequence of characters (type Fonts.Char) and and
non-character objects (in different colors, fonts, and vertical offsets). The only
way to distinguish between a character and an object in the text stream is by
fetching the character/object from its library and then making a type test.
The library of a character/object is given by the lib field of the reader while
advancing through a text stream. The reference number of a character/object
is the ordinal number of the character read (i.e. ORD(ch)). As character objects
are bound to character fonts (Fonts.Font), a quick type test of the Reader lib
field against Fonts.Font also settles the question. Non-character objects of a
text are typically bound to the obs library field of the text descriptor.

2. The non-character objects of a text stream must have reference numbers
in the range 0 <= ref < 256, and must be bound to a library (not necessarily
obs of the text descriptor). Writing non-character objects involves binding it
to a library (say T.obs), changing the font of the Writer, and the writing the
reference number of the non-character object into the writer's buffer.
Afterwards the writer font is reset to its old value. More that 256 non-character
objects can be written into the text by allocating a new library when the old
library is full, and attaching it to the obs field of the text descriptor. The obs field
just acts as a placeholder for libraries and is not used by the texts directly.

3. There are two mechanisms to read from a text and one to write to a text.
The Readers allow characterwise reading from a certain text position onwards.
The Scanners allow reading of formatted tokens like names, strings, numbers and
characters. Writers are used to write characters into temporary holding areas
called buffers. Buffers contains large sequences of objects (both character and
non-character) and allow low-level temporary manipulation. The difference
between texts and buffers involve the display update operations. Each text can
possibly be represented on the display by some kind of text editor or viewer.
When a module manipulates a text, a message called the UpdateMsg (type
Texts.UpdateMsg) is broadcast to all viewers or text editors representing the text.
They then update their representation accordingly. To prevent broadcasts being
sent for potentially each character being written into a text, the text manipulation
is first done in a buffer. Operations on buffers do not result in update messages
being broadcasted. Only when a buffer is applied to a text (inserted or appended),
the texts broadcasts an update message. By convention, once a buffer is applied
to a text, its contents is emptied.

4. The scanner classes indicate what token was scanned. The scanner understands
the following token types:

	Name	Longest sequence starting with "A".."Z", "a".."z", ".", "/", and containing
		"A".."Z", "a".."z", "0".."9", "@", ".", "/", ":", "_", 80X..96X
	String	Any character sequence surrounded by double quotes, i.e. "string".
		The quotes are not returned in the s field of the scanner descriptor.
	Int	Any valid integer number.
	Real	Any valid REAL number, including exponent E.
	LongReal	Any valid LONGREAL number, including exponent D.
	Char	A character (single) not classified as one of the above.

5. The end of line character is carriage return (CR or 0DX), tabulators are 9X.
Unprintable characters are show on the display as smallish square boxes.

6. Vertical offsets are typically measured in screen pixels (positive or negative
to the text base line).

7. The Finder allow quick searching for non-character objects in a text.

8. The meaning of the UpdateMsg fields are defined as in the following table
listed according to the procedures that broadcast the message. Note that a text
stretch identified by (beg, end) does not include the character at position end
in the text. Below, M is of type Texts.UpdateMsg and B stands for a buffer.

	Delete(beg, end)	M.beg = beg
		M.end = end
		M.len = 0
	Replace(beg, end, B)	M.beg = beg
		M.end = end
		M.len = B.len
	ChangeLooks(beg, end)	M.beg = beg
		M.end = end
		M.len = end - beg
	Insert(pos, buf)	M.beg = pos
		M.end = pos
		M.len = B.len

The general scheme is that the stretch between M.beg and M.end was "deleted",
and a new stretch of length M.len was inserted at M.beg. The message indicates
a change AFTER it has already been made by the texts module.

9. There is an asymmetry in writing and reading texts to a file. Each text "block"
in a file is identified by a first character. Reading a text block requires that the
starting position does not include this character, while writing a text block writes
the id character automatically.

10. Opening of non-text files is allowed with Texts.Open; they are simply converted
to ASCII streams. Storing such an opened text will convert it into an Oberon text.
Note that the EditTools package allows the manipulation of ASCII texts both in
MSDOS and UNIX format.
*)
