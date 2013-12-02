testnum : Comp/Utils.hs Comp/List.hs Comp/Num.hs TestNum.hs
	ghc -O2 Comp/Utils.hs Comp/List.hs Comp/Num.hs TestNum.hs -o testnum

clean :
	rm -rf *.hi *.o a.out testnum
	cd Comp; rm -rf *.hi *.o a.out
