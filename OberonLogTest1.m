MODULE OberonLogTest1;

	(*
		A. V. Shiryaev, 2012.01
	*)

	IMPORT Texts, Oberon;

	PROCEDURE Do;
		VAR W: Texts.Writer;
	BEGIN
		Texts.OpenWriter(W);
		Texts.WriteString(W, "Log:"); Texts.WriteLn(W);
		Texts.Append(Oberon.Log, W.buf);
		Texts.WriteString(W, "	don't worry"); Texts.WriteLn(W);
		Texts.Append(Oberon.Log, W.buf)
	END Do;

BEGIN Do
END OberonLogTest1.
