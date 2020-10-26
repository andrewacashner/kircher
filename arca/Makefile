haddock = "--no-haddock-deps arca"

.PHONY : app docs docs-full view

app : 
	stack build

docs : 
	stack haddock --no-haddock-deps arca

docs-full : 
	stack haddock arca

view :
	stack haddock --no-haddock-deps arca --open
