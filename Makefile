all:
	ghc --make -Wall -O2 Main.hs -o hsbot -XScopedTypeVariables

listen:
	nc -l 127.0.0.1 6667

run:
	runghc -XScopedTypeVariables Main

clean:
	- rm hsbot
	- find ./ -name \*.o -exec rm {} \;
	- find ./ -name \*.hi -exec rm {} \;

