
eightfold : Lang.hs Main.hs Parser.hs
	ghc --make Main.hs -o eightfold

Parser.hs : Parser.y
	happy Parser.y
