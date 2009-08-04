all:
	ghc --make -Wall -O2 Main.hs -o hsbot

clean:
	- rm hsbot
	- find ./ -name \*.o -exec rm {} +
	- find ./ -name \*.hi -exec rm {} +

