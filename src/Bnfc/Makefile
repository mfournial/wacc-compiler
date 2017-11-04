all:
	happy -gca ParWacc.y
	alex -g LexWacc.x
	ghc --make ../Data/Waskell/Error.hs AbsWacc ParWacc LexWacc ErrM SkelWacc PrintWacc TestWacc.hs -o TestWacc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocWacc.* LexWacc.* ParWacc.* LayoutWacc.* SkelWacc.* PrintWacc.* TestWacc.* AbsWacc.* TestWacc ErrM.* SharedString.* ComposOp.* wacc.dtd XMLWacc.* Makefile*
	

