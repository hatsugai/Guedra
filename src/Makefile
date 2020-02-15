ifndef OSTYPE
OSTYPE=$(shell uname -s)
endif

ifeq ($(OSTYPE), Darwin)

#
#  macOS
#
CC=gcc
CXX=g++
BACKEND=win-gtk
BACKEND_EXT=.c
CFLAGS=
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm

else
ifeq ($(OSTYPE), Linux)

#
#  Linux
#
CC=gcc
CXX=g++
BACKEND=win-gtk
BACKEND_EXT=.c
CFLAGS=
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm

else

#
#  MS-Windows
#
CC=x86_64-w64-mingw32-gcc
CXX=x86_64-w64-mingw32-g++
BACKEND=win-d2d
BACKEND_EXT=.cpp
EXE=.exe
CFLAGS=-D_UNICODE -DUNICODE
LDFLAGS=-lasmrun -lunix -lm -ld2d1 -ldwrite

endif
endif

OCAML_CSP=../../ocaml-csp/src
CSP_CMO=$(OCAML_CSP)/csp.cmo
CSP_CMX=$(OCAML_CSP)/csp.cmx
GUEDRA_CMA=guedra.cma
GUEDRA_CMXA=guedra.cmxa
OCAML_INCLUDE=-I `ocamlc -where` -I $(OCAML_CSP)
INCLUDE:=$(INCLUDE) $(OCAML_INCLUDE)
OBJECTS=win.o $(BACKEND).o

OCAML_SOURCES = \
	guedra.ml \
	window.ml \
	form.ml \
	splitter.ml \
	vsplitter.ml \
	hsplitter.ml \
	button.ml \
	scroll.ml \
	vscroll.ml \
	hscroll.ml \
	listbox.ml \
	listboxvs.ml \
	checklist.ml \
	checklistvs.ml \
	staticText.ml \
	staticTextVs.ml

OCAML_CMO = \
	guedra.cmo \
	window.cmo \
	form.cmo \
	splitter.cmo \
	vsplitter.cmo \
	hsplitter.cmo \
	button.cmo \
	scroll.cmo \
	vscroll.cmo \
	hscroll.cmo \
	listbox.cmo \
	listboxvs.cmo \
	checklist.cmo \
	checklistvs.cmo \
	staticText.cmo \
	staticTextVs.cmo

OCAML_CMX = \
	guedra.cmx \
	window.cmx \
	form.cmx \
	splitter.cmx \
	vsplitter.cmx \
	hsplitter.cmx \
	button.cmx \
	scroll.cmx \
	vscroll.cmx \
	hscroll.cmx \
	listbox.cmx \
	listboxvs.cmx \
	checklist.cmx \
	checklistvs.cmx \
	staticText.cmx \
	staticTextVs.cmx

%.cmi:	%.mli
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

%.cmx:	%.ml
	ocamlopt $(OCAML_INCLUDE) -bin-annot -c $<

%.cmo:	%.ml
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

all:	$(GUEDRA_CMA) $(GUEDRA_CMXA) win.o $(BACKEND).o


$(OCAML_CMO):	$(CSP_CMO)
$(OCAML_CMX):	$(CSP_CMX)

$(GUEDRA_CMA):	$(OCAML_CMO)
	ocamlc -a -o $@ $^

$(GUEDRA_CMXA):	$(OCAML_CMX)
	ocamlopt -a -o $@ $^

win.o:	win.c win.h
	$(CC) -c $(CFLAGS) $(INCLUDE) $<

win-gtk.o:	win-gtk.c win.h
	$(CC) -c $(CFLAGS) $(INCLUDE) $<

win-d2d.o:	win-d2d.cpp win.h
	$(CXX) -c -municode $(CFLAGS) $(INCLUDE) $<

clean:
	rm *.cmi *.cmo *.cmx *.cmt *.cmti *.o *.cma *.cmxa test test.exe

dep:
	ocamldep $(OCAML_SOURCES) > .depend

-include .depend
