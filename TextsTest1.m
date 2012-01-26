MODULE TextsTest1;

	(*
		A. V. Shiryaev, 2012.01
	*)

	IMPORT Texts, Files:=OFiles, Out;

	VAR
		Log: Texts.Text;

	PROCEDURE NewW1 (VAR W: Texts.Writer);
	BEGIN
		Texts.OpenWriter(W);
		Texts.WriteString(W, "hello"); Texts.Write(W, ' '); Texts.WriteInt(W, 7FFFFFFFH, 0);
		Texts.Write(W, ' '); Texts.WriteSet(W, {0..14,30}); Texts.Write(W, ' '); Texts.WriteReal(W, 0.1, 0);
		Texts.WriteLn(W)
	END NewW1;

	PROCEDURE Do;
		VAR W: Texts.Writer;
			B: Texts.Buffer;
			T: Texts.Text;
	BEGIN
		NewW1(W);
		Texts.Append(Log, W.buf);
		Texts.Append(Log, W.buf);

		NEW(T);
		Texts.Open(T, "CR.TXT");
		Out.String("CR.TXT len = "); Out.Int(T.len, 0); Out.Ln;
		NEW(B);
		Texts.OpenBuf(B);
		Texts.Save(T, 0, T.len, B);
		Texts.Append(Log, B)
	END Do;

	PROCEDURE SaveLog;
		VAR f: Files.File;
			len: INTEGER;
	BEGIN
		f := Files.New("TextsTest1.log");
		Texts.Store(Log, f, 0, len);
		Out.String("stdout len = "); Out.Int(len, 0); Out.Ln
	END SaveLog;

BEGIN
	NEW(Log); Texts.Open(Log, "");
	Do;
	SaveLog
END TextsTest1.
