.PHONY: all clean install test

GUILE_CONFIG := guile-config

SCM_FILES := shapefile.scm $(shell find shapefile -type f -name \*.scm)
GO_FILES := $(addprefix ccache/,$(SCM_FILES:.scm=.go))

SITEDIR=$(shell $(GUILE_CONFIG) info sitedir)
EXTENSIONDIR=$(shell $(GUILE_CONFIG) info extensiondir)
CCACHEDIR=$(shell $(GUILE_CONFIG) info siteccachedir)

all:
	@echo Use install instead

clean:
	-rm -r ccache

install: $(GO_FILES)
	install -d $(DESTDIR)/$(CCACHEDIR)/
	rsync -a ccache/ $(DESTDIR)/$(CCACHEDIR)/
	install -m 644 -D -t $(DESTDIR)/$(SITEDIR)/ shapefile.scm
	rsync -a shapefile $(DESTDIR)/$(SITEDIR)/shapefile

ccache/%.go: %.scm
	guild compile -L. $< -o $@
