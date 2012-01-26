#! /usr/bin/env python2.7
# -*- coding: koi8-r -*-
#
# convert Oberon System and A2 files to ASCII
# implementation based on definition (see Texts.Mod)

import sys, re

# разделить такст на строки (в тексте могут использоваться разные разделители строк)
def splitToLines (text):
	lines = []
	for l in text.split('\r\n'):
		for l1 in re.split( '[\r\n]', l ):
			lines.append(l1)
	return lines
normalizeLineSep = lambda text, lineSep: lineSep.join( splitToLines(text) )

class Reader:

	def __init__ (self, f, pos):
		self.f = f
		self.pos = pos
		self.eot = False

	def SetPos (self, pos):
		self.eot = False
		try:
			self.f.seek(pos)
		except:
			self.eot = True
		self.pos = pos

	def Read (self):
		if self.eot:
			return chr(0)
		else:
			try:
				c = self.f.read(1)
			except:
				self.eot = True
				return chr(0)
			else:
				self.pos = self.pos + 1
				return c

	# 4 B signed integer, little-endian
	def ReadInt (self):
		c0 = self.Read()
		c1 = self.Read()
		c2 = self.Read()
		c3 = self.Read()
		x = ord(c0) + 0x100 * ord(c1) + 0x10000 * ord(c2) + 0x1000000 * ord(c3)
		if x >= 2147483648:
			x = x - 4294967296
		return x

def ReadTextBlock (r):
	c = r.Read()
	if c == chr(0xf0):
		tip = ord(r.Read())
		assert tip == 1
		hlen = r.ReadInt()
		r.SetPos(r.pos + hlen - 11)
		zero = ord(r.Read())
		assert zero == 0
		tlen = r.ReadInt()
		text = r.f.read(tlen)
		r.SetPos(r.pos + tlen)
		return text

def main ():
	if (len(sys.argv) != 2) and (len(sys.argv) != 3):
		print "usage: %s oberon-filename [ txt-filename ]" % (sys.argv[0],)
	else:
		res = []

		f = open(sys.argv[1], 'rb')
		r = Reader(f, 0)
		if r.Read() == chr(0xf0): # Oberon file
			r.SetPos(0)
			text = ReadTextBlock(r)
			while text != None:
				res.append(text)
				text = ReadTextBlock(r)
		else: # ASCII file?
			f.seek(0)
			t = f.read()
			if t.startswith('<?xml '): # A2?: strip XML
				r1 = r = re.match('.*(MODULE [^;]+;.*END [^\.]+\.).*', t, re.S)
				if r1 != None:
					res.append( ''.join(map(lambda x: {chr(2): '', chr(0x0D): chr(0x0A)}.get(x, x), r1.group(1))) )
					del r1
			else:
				res.append(t)
			del t
		f.close()

		res = ''.join(res)
		res = normalizeLineSep(res, '\n')

		if len(sys.argv) == 2:
			dst = sys.stdout
		else:
			dst = open(sys.argv[2], 'w')

		dst.write(res)

		if dst <> sys.stdout:
			dst.close()

if __name__ == '__main__':
	main()
