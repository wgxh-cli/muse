#build: clean
#	ghc -i.:src -outputdir./build Main.hs
#
#run: build
#	./Main
#
#clean:
#	rm -rf build Main

clean:
	rm -rf build

build: clean
	stack build

run: clean
	stack run
