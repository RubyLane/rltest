DESTDIR=/usr/local
PACKAGE_NAME=rltest
VER=1.5
TCLSH=tclsh

all: tm/$(PACKAGE_NAME)-$(VER).tm

tm/$(PACKAGE_NAME)-$(VER).tm: rltest.tcl
	mkdir -p tm
	cp rltest.tcl tm/$(PACKAGE_NAME)-$(VER).tm

install-tm: tm/$(PACKAGE_NAME)-$(VER).tm
	mkdir -p $(DESTDIR)/lib/tcl8/site-tcl
	cp $< $(DESTDIR)/lib/tcl8/site-tcl/

install: install-tm

clean:
	rm -r tm

.PHONY: all clean install install-tm
