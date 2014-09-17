NAME := howto

CHAPTERS := vine-overview.tex vine-il.tex \
            irformulagen.tex stp.tex utils.tex
FIGURES := vine-components.eps \
           ../install-vine-release.sh

all: doc

doc: $(NAME).pdf $(NAME).html

$(NAME).dvi: $(NAME).tex $(CHAPTERS) $(FIGURES)
	latex $(NAME).tex
	latex $(NAME).tex
	latex $(NAME).tex

$(NAME).html: $(NAME).tex $(CHAPTERS) $(FIGURES)
	hevea -fix $(NAME).tex

$(NAME).pdf: $(NAME).dvi
	dvipdf $<

%.eps: %.fig
	fig2dev -L eps $< $@

distdir: $(FIGURES)
	cp --parents -t $(distdir) $(CHAPTERS) $(FIGURES) $(NAME).tex Makefile

clean:
	-rm -f $(NAME).dvi $(NAME).pdf $(NAME).aux $(NAME).out $(NAME).log
	-rm -f $(NAME).toc
	-rm -f comment.cut
	-rm -f $(NAME).html $(NAME)[0-9][0-9][0-9].png
	-rm -f $(NAME).haux $(NAME).htoc 
	-rm -f $(NAME).image.tex $(NAME).image.out
	-rm -f *.eps
