MODULE Oberon;

	(*
		A. V. Shiryaev, 2012.01
	*)

	IMPORT Args, Texts, Display, Files;

	VAR
		Log-: Texts.Text;
		Par-: RECORD
			pos-: INTEGER;
			text-: Texts.Text
		END;
		OptionChar*: CHAR; (** Option character "/" or "\" *)

	PROCEDURE GetSelection* (VAR t: Texts.Text; VAR beg, end, time: INTEGER);
		VAR s: ARRAY 32 OF CHAR;
			(* f: Files.File; *)
			W: Texts.Writer;
	BEGIN
		IF Args.argc >= 2 THEN
			Args.GetArg(1, s);

			(*
			f := Files.Open(s, "r");
			t := Texts.NewText(f);
			*)
			Texts.Open(t, s);

			time := 0;
			beg := 0; end := t.len
		ELSE
			Texts.OpenWriter(W);
			Texts.WriteString(W, "usage: ");
			Args.GetArg(0, s);
			Texts.WriteString(W, s);
			Texts.WriteString(W, " filename");
			Texts.WriteLn(W);
			Texts.Append(Log, W.buf);
			t := NIL;
			time := -1;
			beg := 0; end := 0
		END
	END GetSelection;

	PROCEDURE MarkedText* (): Texts.Text;
		VAR beg, end, time: INTEGER;
			t: Texts.Text;
	BEGIN
		NEW(t);
		GetSelection(t, beg, end, time);
		RETURN t
	END MarkedText;

	PROCEDURE Broadcast (VAR M: Display.FrameMsg);
		VAR R: Texts.Reader;
			i: INTEGER; c: CHAR;
	BEGIN
		WITH M: Texts.UpdateMsg DO
			IF M.text = Log THEN
				IF (M.beg = M.end) & (M.len > 0) THEN (* catch Texts.Append(Oberon.Log, buf) *)
					Texts.OpenReader(R, M.text, M.beg);
					i := M.len;
					REPEAT
						Texts.Read(R, c);
						IF c = 0DX THEN c := 0AX END;
						Files.Write(Files.stdout, c);
						(* Out.Char(c); *)
						DEC(i)
					UNTIL i = 0;
					Files.Flush(Files.stdout)
				END
			END
		ELSE
		END
	END Broadcast;

BEGIN
	OptionChar := '\';
	Display.Broadcast := Broadcast;

	NEW(Log);
	Texts.Open(Log, "");

	NEW(Par.text);
	(* Texts.LoadAscii(Par.text, OFiles.Old("/dev/stdin")); *)
	Texts.Open(Par.text, "par");
	Par.pos := 0
END Oberon.
