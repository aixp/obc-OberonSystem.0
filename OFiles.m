MODULE OFiles;

(*
	A. V. Shiryaev, 2012.01

	NOTES:
		INTEGER is 32-bit signed integer
		REAL is 32-bit IEEE-754 number
		LONGREAL is 64-bit IEEE number

		seekable files only supported, and, partially, stdin
	TODO:
		see Remarks and re-implement this module
			(at least 2nd item)
		support stdin via freopen() ?
		partial support of non-seekable files ?:
			freopen() ?
			use isatty()
*)

IMPORT SYSTEM, Files, Conv, Out;

TYPE
	File* = POINTER TO RECORD
		f: Files.File;
		name: ARRAY 1024 OF CHAR
	END;

	Rider* = RECORD
		eof-: BOOLEAN;	(** has end of file been passed *)
		res-: INTEGER;	(** leftover byte count for ReadBytes/WriteBytes *)
		pos: INTEGER;
		f: File
	END;

	VAR
		tmpIdx: INTEGER;

PROCEDURE NewTmpName (VAR name: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	Conv.ConvInt(tmpIdx, name); INC(tmpIdx);
	i := 0; WHILE name[i] # 0X DO INC(i) END;
	name[i] := "."; INC(i);
	name[i] := "t"; INC(i);
	name[i] := "m"; INC(i);
	name[i] := "p"; INC(i);
	name[i] := 0X
END NewTmpName;

(** Creates a new file with the specified name. *)
PROCEDURE New* (CONST name: ARRAY OF CHAR): File;
	VAR f: File;
		file: Files.File;
		i: INTEGER;
		name1: ARRAY 32 OF CHAR;
BEGIN
	IF name = "" THEN (* anonymous file *)
		NewTmpName(name1);
		file := Files.Open(name1, "w+")
	ELSE
		file := Files.Open(name, "w+")
	END;
	IF file # NIL THEN
		NEW(f); f.f := file;
		i := 0; WHILE (i < LEN(name)) & (name[i] # 0X) DO f.name[i] := name[i]; INC(i) END; f.name[i] := 0X
	ELSE f := NIL
	END;
RETURN f
END New;

(** Open an existing file. The same file descriptor is returned if a file is opened multiple times. *)
PROCEDURE Old* (CONST name: ARRAY OF CHAR): File;
	VAR f: File;
		file: Files.File;
		i: INTEGER;
BEGIN
	IF name = "/dev/stdin" THEN
		file := Files.stdin
	ELSE
		file := Files.Open(name, "r+")
	END;
	IF file # NIL THEN
		NEW(f); f.f := file;
		i := 0; WHILE (i < LEN(name)) & (name[i] # 0X) DO f.name[i] := name[i]; INC(i) END; f.name[i] := 0X
	ELSE f := NIL
	END;
RETURN f
END Old;

(** Register a file created with New in the directory, replacing the previous file in the directory with the same name. The file is automatically closed. *)
PROCEDURE Register* (f: File);
BEGIN
	Files.Flush(f.f); Files.Close(f.f); f.f := NIL
END Register;

(** Flushes the changes made to a file to disk. Register will automatically Close a file. *)
PROCEDURE Close* (f: File);
BEGIN
	Files.Flush(f.f); Files.Close(f.f); f.f := NIL
END Close;

(** Returns the current length of a file. *)
PROCEDURE Length* (f: File): INTEGER;
	VAR pos, posEnd: INTEGER;
BEGIN
IF f.f = Files.stdin THEN
	posEnd := MAX(INTEGER)
ELSE
	pos := Files.Tell(f.f);
	Files.Seek(f.f, 0, Files.SeekEnd);
	posEnd := Files.Tell(f.f);
	Files.Seek(f.f, pos, Files.SeekSet)
END;
RETURN posEnd
END Length;

(** Returns the time (t) and date (d) when a file was last modified. *)
PROCEDURE GetDate* (f: File; VAR t, d: INTEGER);
BEGIN
	ASSERT(FALSE, 126);
	t := 0; d := 0
END GetDate;

(** Sets the modification time (t) and date (d) of a file. *)
PROCEDURE SetDate* (f: File; t, d: INTEGER);
BEGIN
	ASSERT(FALSE, 126)
END SetDate;

(** Positions a Rider at a certain position in a file. Multiple Riders can be positioned at different locations in a file. A Rider cannot be positioned beyond the end of a file. *)
PROCEDURE Set* (VAR r: Rider; f: File; pos: INTEGER);
BEGIN
	IF f # NIL THEN
		r.eof := FALSE; r.res := 0; r.f := f;
		IF pos < 0 THEN
			r.pos := 0
		ELSE
			r.pos := pos
		END
	ELSE r.f := NIL
	END
END Set;

(** Returns the offset of a Rider positioned on a file. *)
PROCEDURE Pos* (VAR r: Rider): INTEGER;
BEGIN
RETURN r.pos
END Pos;

(** Returns the File a Rider is based on. *)
PROCEDURE Base* (VAR r: Rider): File;
BEGIN
RETURN r.f
END Base;

(** Read a byte from a file, advancing the Rider one byte further. R.eof indicates if the end of the file has been passed. *)
PROCEDURE Read* (VAR r: Rider; VAR x: CHAR);
BEGIN
	IF r.f.f # Files.stdin THEN
		Files.Seek(r.f.f, r.pos, Files.SeekSet)
	END;
	IF Files.Eof(r.f.f) THEN
		x := 0X;
		r.eof := TRUE
	ELSE
		Files.ReadChar(r.f.f, x);
		INC(r.pos);
		r.eof := FALSE
	END
END Read;

(** Reads a sequence of length n bytes into the buffer x, advancing the Rider. Less bytes will be read when reading over the length of the file. r.res indicates the number of unread bytes. x must be big enough to hold n bytes. *)
PROCEDURE ReadBytes* (VAR r: Rider; VAR x: ARRAY OF CHAR; len: INTEGER);
	VAR m: INTEGER; ch: CHAR;
BEGIN
	ASSERT(LEN(x) >= len, 19);
	m := 0;
	LOOP
		IF len <= 0 THEN EXIT END;
		Read(r, ch);
		IF r.eof THEN EXIT END;
		x[m] := ch; INC(m); DEC(len)
	END;
	r.res := len
END ReadBytes;

(**
Portable routines to read the standard Oberon types.
*)

PROCEDURE ReadInt* (VAR r: Rider; VAR x: SHORTINT);
	VAR x0, x1: CHAR;
BEGIN
	Read(r, x0); Read(r, x1);
	x := SHORT(ORD(x1) * 100H + ORD(x0))
END ReadInt;

PROCEDURE ReadLInt* (VAR r: Rider; VAR x: INTEGER);
	VAR x0, x1, x2, x3: CHAR;
BEGIN
	Read(r, x0); Read(r, x1); Read(r, x2); Read(r, x3);
	x := ORD(x0) + ORD(x1) * 100H + ORD(x2) * 10000H + ORD(x3) * 1000000H
END ReadLInt;

PROCEDURE ReadSet* (VAR r: Rider; VAR x: SET);
	VAR x0: INTEGER;
BEGIN
	ReadLInt(r, x0);
	x := SYSTEM.VAL(SET, x0)
END ReadSet;

PROCEDURE ReadBool* (VAR r: Rider; VAR x: BOOLEAN);
	VAR s: CHAR;
BEGIN
	Read(r, s); x := s # 0X
END ReadBool;

PROCEDURE ReadReal* (VAR r: Rider; VAR x: REAL);
	VAR x0: INTEGER;
BEGIN
	ReadLInt(r, x0);
	x := SYSTEM.VAL(REAL, x0)
END ReadReal;

PROCEDURE ReadLReal* (VAR r: Rider; VAR x: LONGREAL);
	VAR x0: INTEGER;
BEGIN
	ReadLInt(r, x0);
	SYSTEM.PUT(SYSTEM.ADR(x), x0);
	ReadLInt(r, x0);
	SYSTEM.PUT(SYSTEM.ADR(x) + 4, x0)
END ReadLReal;

PROCEDURE ReadString* (VAR r: Rider; VAR x: ARRAY OF CHAR);
VAR i: SHORTINT; ch: CHAR;
BEGIN i := 0;
	LOOP
		Read(r, ch); x[i] := ch; INC(i);
		IF ch = 0X THEN EXIT END;
		IF i = LEN(x) THEN x[i-1] := 0X;
			REPEAT Read(r, ch) UNTIL ch = 0X;
			EXIT
		END
	END
END ReadString;

(** Reads a number in compressed variable length notation using the minimum amount of bytes. *)
(* NOTE: if ASSERT(s) fails, re-implement LSL:
		LSL1(x,n) = IF ABS(n) >= 32 THEN RETURN 0 ELSE RETURN LSL(x, n) END;
*)
PROCEDURE ReadNum* (VAR r: Rider; VAR x: INTEGER);
VAR ch: CHAR; n: SHORTINT; y: INTEGER;
BEGIN
	n := 0; y := 0; Read(r, ch);
	WHILE ch >= 80X DO ASSERT(n < 32, 100); INC(y, LSL(ORD(ch) - 128, n)); INC(n, 7); Read(r, ch) END;
	ASSERT(n-25 < 32, 100);
	x := ASH(LSL(ORD(ch), 25), n-25) + y
END ReadNum;

(** Writes a byte into the file at the Rider position, advancing the Rider by one. *)
PROCEDURE Write* (VAR r: Rider; x: CHAR);
BEGIN
	Files.Seek(r.f.f, r.pos, Files.SeekSet);
	Files.WriteChar(r.f.f, x);
	INC(r.pos)
END Write;

(** Writes the buffer x containing n bytes into a file at the Rider position. *)
PROCEDURE WriteBytes* (VAR r: Rider; CONST x: ARRAY OF CHAR; len: INTEGER);
	VAR m: INTEGER;
BEGIN
	ASSERT(LEN(x) >= len, 19);
	m := 0;
	WHILE len > 0 DO
		Write(r, x[m]); INC(m); DEC(len)
	END;
	r.res := len
END WriteBytes;

(**
Portable routines to write the standard Oberon types.
*)

PROCEDURE WriteInt* (VAR r: Rider; x: SHORTINT);
BEGIN
	Write(r, CHR(x MOD 100H)); Write(r, CHR(x DIV 100H))
END WriteInt;

PROCEDURE WriteLInt* (VAR r: Rider; x: INTEGER);
BEGIN
	Write(r, CHR(x MOD 100H));
	Write(r, CHR(x DIV 100H MOD 100H));
	Write(r, CHR(x DIV 10000H MOD 100H));
	Write(r, CHR(x DIV 1000000H MOD 100H))
END WriteLInt;

PROCEDURE WriteSet* (VAR r: Rider; x: SET);
BEGIN
	WriteLInt(r, SYSTEM.VAL(INTEGER, x))
END WriteSet;

PROCEDURE WriteBool* (VAR r: Rider; x: BOOLEAN);
BEGIN
	IF x THEN Write(r, 1X) ELSE Write(r, 0X) END
END WriteBool;

PROCEDURE WriteReal* (VAR r: Rider; x: REAL);
BEGIN
	WriteLInt(r, SYSTEM.VAL(INTEGER, x))
END WriteReal;

PROCEDURE WriteLReal* (VAR r: Rider; x: LONGREAL);
	VAR x0: INTEGER;
BEGIN x0 := 0; (* prevent warning "variable x0 may not be initialized" *)
	SYSTEM.GET(SYSTEM.ADR(x), x0);
	WriteLInt(r, x0);
	SYSTEM.GET(SYSTEM.ADR(x) + 4, x0);
	WriteLInt(r, x0)
END WriteLReal;

PROCEDURE WriteString* (VAR r: Rider; CONST x: ARRAY OF CHAR);
VAR i: SHORTINT; ch: CHAR;
BEGIN
	i := 0;
	LOOP ch := x[i]; Write(r, ch); INC(i);
		IF ch = 0X THEN EXIT END;
		IF i = LEN(x) THEN Write(r, 0X); EXIT END
	END
END WriteString;

(** Writes a number in a compressed format. *)
PROCEDURE WriteNum* (VAR r: Rider; x: INTEGER);
BEGIN
	WHILE (x < - 64) OR (x > 63) DO Write(r, CHR(x MOD 128 + 128)); x := x DIV 128 END;
	Write(r, CHR(x MOD 128))
END WriteNum;

(** Deletes a file. res = 0 indicates success. *)
PROCEDURE Delete* (name: ARRAY OF CHAR; VAR res: SHORTINT);
BEGIN
	Out.String('Oberon:Files.Delete "'); Out.String(name); Out.String('": NOT IMPLEMENTED'); Out.Ln;
	res := -1
END Delete;

(** Renames a file. res = 0 indicates success. *)
PROCEDURE Rename* (CONST old, new: ARRAY OF CHAR; VAR res: SHORTINT);
	VAR f1, f2: Files.File; c: CHAR;
BEGIN
	f1 := Files.Open(old, "r");
	IF f1 # NIL THEN
		f2 := Files.Open(new, "w");
		IF f2 # NIL THEN
			WHILE ~Files.Eof(f1) DO
				Files.ReadChar(f1, c);
				Files.WriteChar(f2, c)
			END;
			Files.Close(f1);
			Files.Close(f2);
			Delete(old, res)
		ELSE
			Files.Close(f1);
			res := -1
		END
	ELSE
		res := -1
	END
END Rename;

(** Returns the full name of a file. *)
PROCEDURE GetName* (f: File; VAR name: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	i := 0; WHILE (i < LEN(f.name)) & (f.name[i] # 0X) DO name[i] := f.name[i]; INC(i) END; name[i] := 0X
END GetName;

BEGIN
	tmpIdx := 0
END OFiles.

(* from original A2:Oberon:Files: *)

(** Remarks:

1. Oberon uses the little-endian byte ordering for exchanging files between different Oberon platforms.

2. Files are separate entities from directory entries. Files may be anonymous by having no name and not being registered in a directory. Files only become visible to other clients of the Files module by explicitly passing a File descriptor or by registering a file and then opening it from the other client. Deleting a file of which a file descriptor is still available, results in the file becoming anonymous. The deleted file may be re-registered at any time.

3. Files and their access mechanism (Riders) are separated. A file might have more than one rider operating on it at different offsets in the file.

4. The garbage collector will automatically close files when they are not required any more. File buffers will be discarded without flushing them to disk.  Use the Close procedure to update modified files on disk.

5. Relative and absolute filenames written in the directory syntax of the host operating system are used. By convention, Oberon filenames consists of the letters A..Z, a..z, 0..9, and ".". The directory separator is typically / or :. Oberon filenames are case sensitive. *)

(*
to do:
o Rename duplicate methods/procedures in Files (e.g. Register0 method)
o remove Read/Write methods to encourage buffering (bad idea?)
- handle case where underlying file is changed by someone else (e.g. a log file being written by an active object)
- check if file handle is a good "key" (yes, because it can not be re-used while we hold it in the list, through the rider)
*)
