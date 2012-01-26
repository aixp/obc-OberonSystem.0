.SUFFIXES: .Mod .m .k

all: Oberon.Sets.m Oberon.k Texts.k OFiles.k Reals.k TextsTest1 OberonLogTest1 InTest1

TextsTest1: Reals.k OFiles.k Display.k Texts.k Oberon.k TextsTest1.k
	obc -o ${.TARGET} ${.ALLSRC}

OberonLogTest1: Reals.k OFiles.k Display.k Texts.k Oberon.k OberonLogTest1.k
	obc -o ${.TARGET} ${.ALLSRC}

InTest1: Reals.k OFiles.k Display.k Texts.k Oberon.k InTest1.k
	obc -o ${.TARGET} ${.ALLSRC}

##

TextsTest1.k: OFiles.k Texts.k

OberonLogTest1.k: Texts.k Oberon.k

InTest1: Texts.k Oberon.k OFiles.k

#

Oberon.k: Texts.k Display.k

Texts.k: OFiles.k Reals.k Display.k

Sets.k: Texts.k

##

.Mod.m:
	o2txt ${.IMPSRC} | sed 's/SHORTINT/BYTEE/g;s/INTEGER/SHORTINT/g;s/LONGINT/INTEGER/g;s/HUGEINT/LONGINT/g;s/ IN Oberon;/;/g;s/, Files;/, Files:=OFiles;/g' > ${.TARGET}

.m.k:
	obc -c ${.IMPSRC}

clean:
	rm -f *.k Oberon.Sets.m TextsTest1 TextsTest1.log OberonLogTest1 InTest1 *.tmp
