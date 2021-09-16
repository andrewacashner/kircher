.PHONY : app install docs docs-full view

app : 
	stack build

install : 
	stack install

docs : 
	stack haddock --no-haddock-deps arca

docs-full : 
	stack haddock arca

view :
	stack haddock --no-haddock-deps arca --open
