#!/usr/bin/env make

dirs        = output
input       = $(wildcard input/*.xml)
pdf_out     = $(addprefix output/,$(notdir $(input:%.xml=%.pdf)))
pdf_out_noficta = $(pdf_out:%.pdf=%-noficta.pdf)
master_pdf  = output/tone_cycle.pdf
ficta_log   = output/total-corrections.log

.PHONY : all clean 

all : $(master_pdf) $(pdf_out) $(pdf_out_noficta) $(ficta_log)

output/%-0-Simple.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh --ficta --simple $<

output/%-1-Florid.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh --ficta --florid $< 

output/%.pdf : output/%-0-Simple.pdf output/%-1-Florid.pdf
	pdfunite $^ $@
	@echo "tone-cycle PDF output written to $@"

output/%-0-Simple-noficta.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh --noficta --simple $<

output/%-1-Florid-noficta.pdf : input/%.xml | $(dirs)
	./tone-cycle.sh --noficta --florid $< 

output/%-noficta.pdf : output/%-0-Simple-noficta.pdf output/%-1-Florid-noficta.pdf
	pdfunite $^ $@
	@echo "tone-cycle PDF output written to $@"

$(master_pdf) : $(pdf_out) $(pdf_out_noficta)
	pdfunite $^ $@

$(dirs) :
	mkdir -p $(dirs)

clean :
	rm -rf $(dirs)

$(ficta_log) : $(pdf_out)
	./count_ficta_corrections.sh
	@echo "tone-cycle Ficta corrections log written to $@"

