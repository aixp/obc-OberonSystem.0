#! /usr/bin/env python2.7
#
# A. V. Shiryaev, 2012.01

import sys, re, os

def splitToLines (text):
	lines = []
	[ [ lines.append(l1) for l1 in re.split( '[\r\n]', l ) ] for l in text.split('\r\n') ]
	return lines

def setLineSep (fileName, lineSep):
	try:
		fh = open(fileName, 'rb')
		text = fh.read()
		fh.close()
	except Exception, e:
		sys.stderr.write("%s\n" % str(e))
	else:
		text = lineSep.join(splitToLines(text))

		fh = open(fileName, 'wb')
		fh.write(text)
		fh.close()

def usage ():
	sys.stderr.write("usage: %s ( dos | unix | mac ) file { file }\n" % (sys.argv[0],))

def main ():
	if len(sys.argv) < 3:
		usage()
	else:
		lineSep = { 'unix': '\n', 'dos': '\r\n', 'mac': '\r' }.get(sys.argv[1])
		if lineSep == None:
			usage()
		else:
			[ setLineSep(fileName, lineSep) for fileName in sys.argv[2:] ]

if __name__ == '__main__':
	main()
