.PHONY: all clean install test docs for-package

GUILE_CONFIG := guile-config

SCM_FILES := shapefile.scm $(shell find shapefile -type f -name \*.scm)
GO_FILES := $(addprefix ccache/,$(SCM_FILES:.scm=.go))

TEXI_FILES := guile-shapefile.texi
INFO_FILES := $(TEXI_FILES:.texi=.info)

SITEDIR=$(shell $(GUILE_CONFIG) info sitedir)
EXTENSIONDIR=$(shell $(GUILE_CONFIG) info extensiondir)
CCACHEDIR=$(shell $(GUILE_CONFIG) info siteccachedir)

# Prefix is only used for info-files.
PREFIX := /usr/local

all: $(INFO_FILES) docs
	@echo Scheme code is only built for install.
	@echo Usually, Guiles built in auto compiler is sufficient.

clean:
	-rm -r ccache

for-package: $(GO_FILES) $(INFO_FILES)

install: $(GO_FILES) $(INFO_FILES)
	install -d $(DESTDIR)/$(CCACHEDIR)/
	cp -a ccache/ $(DESTDIR)/$(CCACHEDIR)/
	install -m 644 -D -t $(DESTDIR)/$(SITEDIR)/ shapefile.scm
	cp -a shapefile $(DESTDIR)/$(SITEDIR)/shapefile
	install -m 644 -D -t $(DESTDIR)/$(PREFIX)/share/info/ $(INFO_FILES)

ccache/%.go: %.scm
	guild compile -L. $< -o $@

%.info: %.texi
	makeinfo $<

# docs are for github pages.
# But can of course be used for other stuff also

%.svg: $(wildcard sample-data/an_riks.*)
	env GUILE_LOAD_PATH=. examples/to-svg.scm \
		--file riks/an_riks \
		--height 300

docs/%.png: %.svg
	inkscape --export-filename=$@ $<

docs: $(TEXI_FILES) docs/an_riks.png LICENSE
	ln -s docs/an_riks.png
	makeinfo --html guile-shapefile.texi -o $@
