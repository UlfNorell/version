
version : $(wildcard *.hs)
	ghc --make -O2 -o $@ Main.hs

