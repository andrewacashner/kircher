dirs        = output
input       = $(wildcard input/*.xml)
mei_out     = $(addprefix output/,$(notdir $(input:%.xml=%.mei)))
pdf_out	    = $(mei_out:%.mei=%.pdf)

.PHONY : all clean

all : $(pdf_out)

output/%-0-Simple.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh -s $<

output/%-1-Florid.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh -f $< 

output/%.pdf : output/%-0-Simple.pdf output/%-1-Florid.pdf
	pdfunite $^ $@
	@echo "tone-cycle PDF output written to $@"

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)


