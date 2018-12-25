all:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o TestLatte

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd XMLLatte.* Makefile*
	
