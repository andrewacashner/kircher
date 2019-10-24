exec 	= arca

aux_dir = aux
doc_dir = doc
build_dir = build
dirs 	= $(aux_dir) $(doc_dir) $(build_dir)

hs_input = $(wildcard *.hs)
lhs_input = $(wildcard *.lhs)
input	= $(hs_input) $(lhs_input)

obj 	= $(input:%.hs=%.o) $(input:%.lhs=%.o)
data 	= $(input:%.hs=%.hi) $(input:%.lhs=%.hi)
tmp	= $(obj) $(data)
aux	= $(addprefix $(aux_dir)/, $(tmp))

doc	= $(addprefix $(doc_dir)/, $(lhs_input:%.lhs=%.pdf))
output  = $(addprefix $(build_dir)/, $(exec))

haskell  = ghc --make
auxfiles = -hidir $(aux_dir) -odir $(aux_dir)

literate = lhs2TeX 
latex    = latexmk -pdf -outdir=$(aux_dir)

.PHONY : all clean 

all : $(output) $(doc) 

$(doc) : $(lhs_input)

$(doc_dir)/%.pdf : $(aux_dir)/%.pdf
	mv $< $@

$(aux_dir)/%.pdf : $(aux_dir)/%.tex
	$(latex) $<

$(aux_dir)/%.tex : %.lhs | $(dirs)
	$(literate) -o $@ $<


$(output) : Main.hs $(input) | $(dirs)
	$(haskell) -o $(output) $(auxfiles) $<


$(dirs) :
	-mkdir -p $(dirs)

view : $(doc)
	evince $(doc) &> /dev/null &

run : $(output)
	./$(output)

clean : 
	-rm -rf $(dirs)
