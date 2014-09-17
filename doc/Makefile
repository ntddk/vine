all: 
	echo "Run 'make doc' to build documentation"

doc:
	(cd howto && $(MAKE) doc)

distdir:
	cp --parents -t $(distdir) Makefile; \
	test -d "$(distdir)/howto" \
	  || mkdir -p "$(distdir)/howto" \
	  || exit 1; \
	(cd howto && \
	  $(MAKE) $(AM_MAKEFLAGS) \
	    distdir="$$distdir/howto" \
	    distdir) \
	  || exit 1;

clean:
	(cd howto && $(MAKE) clean)
