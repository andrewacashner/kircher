exec 	= arca

aux_dir = aux
doc_dir = doc
build_dir = build
dirs = $(aux_dir) $(aux_dir)/Syntagma0 \
       $(doc_dir) $(doc_dir)/Syntagma0 \
       $(build_dir)

main_input = $(wildcard *.lhs)
sub_input  = $(wildcard Syntagma0/*.lhs)
input	   = $(main_input) $(sub_input)

obj 	= $(input:%.lhs=%.o)
data 	= $(input:%.lhs=%.hi)
tmp	= $(obj) $(data)
aux	= $(addprefix $(aux_dir)/, $(tmp))

tex	= $(addprefix $(aux_dir)/, $(input:%.lhs=%.tex))
doc	= $(addprefix $(doc_dir)/, $(input:%.lhs=%.pdf))
output  = $(addprefix $(build_dir)/, $(exec))

haskell  = ghc --make
auxfiles = -hidir $(aux_dir) -odir $(aux_dir)

literate = lhs2TeX 
latex    = latexmk -pdf 

.PHONY : all view run clean 

all : $(output) $(doc)

$(doc_dir)/%.pdf : $(aux_dir)/%.pdf
	cp -u $< $@

$(aux_dir)/%.pdf : $(aux_dir)/%.tex
	$(latex) $< -outdir=$(patsubst %/,%,$(dir $<))

$(aux_dir)/%.tex : %.lhs | $(dirs)
	$(literate) -o $@ $<


$(output) : Main.lhs $(input) | $(dirs)
	$(haskell) -o $(output) $(auxfiles) $<


$(dirs) :
	-mkdir -p $(dirs)

view : $(doc)
	evince $(doc) &> /dev/null &

run : $(output)
	./$(output)

clean : 
	-rm -rf $(dirs)
