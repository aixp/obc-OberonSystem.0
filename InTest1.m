MODULE InTest1;

	(*
		A. V. Shiryaev, 2012.01
	*)

	IMPORT Files:=OFiles, Oberon, Texts, Out;

	PROCEDURE Do;
		VAR t: Texts.Text;
			S: Texts.Scanner;
			W: Texts.Writer;
	BEGIN
		Texts.OpenWriter(W);

		NEW(t);
		(* NOTE: do not use Texts.Open, because stdin is not seekable; Texts.Open checks signature of file *)
		Texts.LoadAscii(t, Files.Old("/dev/stdin"));

		Texts.WriteString(W, "open; len = "); Texts.WriteInt(W, t.len, 0); Texts.WriteLn(W);
		Texts.Append(Oberon.Log, W.buf);

		Texts.OpenScanner(S, t, 0);
		Texts.Scan(S);
		WHILE S.class = Texts.Int DO
			Texts.WriteInt(W, S.i, 0); Texts.WriteLn(W);
			Texts.Append(Oberon.Log, W.buf);

			Texts.Scan(S)
		END;
		Texts.WriteString(W, "All done."); Texts.WriteLn(W);
		Texts.Append(Oberon.Log, W.buf)
	END Do;

BEGIN Do
END InTest1.
