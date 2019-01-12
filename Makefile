export PATH :=/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:$(PATH)

flags=--make -cpp -DRUNTIME_PATH=\"${CURDIR}/lib/runtime.bc\" -isrc/common:src/frontend:src/backend

cleaned=*.log *.aux *.hi *.o *.dvi

all: deps nodeps

nodeps: lib latc latc_llvm

.PHONY: lib latc

deps:
	cabal update
	cabal install mtl
	cabal install dlist

lib:
	llvm-as lib/runtime.ll -o lib/runtime.bc

latc:
	ghc ${flags} src/common/Main.hs -o latc

latc_llvm:
	cp latc latc_llvm

clean:
	-cd src/common && rm -f ${cleaned}
	-cd src/frontend && rm -f ${cleaned}
	-cd src/backend && rm -f ${cleaned}

