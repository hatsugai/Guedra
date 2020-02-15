ifndef OSTYPE
OSTYPE=$(shell uname -s)
endif

ifeq ($(OSTYPE), Darwin)
#
#  macOS
#
CC=gcc
CXX=g++
WINBASE=win-gtk
EXE=
CFLAGS = -g -O0
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm

else
ifeq ($(OSTYPE), Linux)
#
#  Linux
#
CC=gcc
CXX=g++
WINBASE=win-gtk
EXE=
CFLAGS = -g -O0
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm
else
#
#  MS-Windows
#

CC=x86_64-w64-mingw32-gcc
CXX=x86_64-w64-mingw32-g++
WINBASE=win-d2d
EXE=.exe
CFLAGS=-D_UNICODE -DUNICODE
LDFLAGS= -lasmrun -lunix -lm -ld2d1 -ldwrite

endif
endif

OCAML_CSP=../../ocaml-csp/src
GUEDRA_PATH=../src
OCAML_INCLUDE=-I `ocamlc -where` -I $(OCAML_CSP) -I $(GUEDRA_PATH)
INCLUDE := $(INCLUDE) $(OCAML_INCLUDE)
COBJECTS=$(GUEDRA_PATH)/win.o $(GUEDRA_PATH)/$(WINBASE).o
GUEDRA_LIB=$(GUEDRA_PATH)/guedra.cmxa $(COBJECTS)

TARGETS=\
	test_window.x \
	test_button.x \
	test_vsplitter.x \
	test_vsplitter2.x \
	test_hsplitter.x \
	test_vscroll.x \
	test_hscroll.x \
	test_listbox.x \
	test_listboxvs.x \
	test_checklist.x \
	test_checklistvs.x \
	test_form.x \
	test_form4.x \
	test_staticTextVs.x

%.cmi:	%.mli
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

%.cmx:	%.ml
	ocamlopt $(OCAML_INCLUDE) -bin-annot -c $<

%.cmo:	%.ml
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

%.x:	%.cmx
	ocamlopt -o $@ $(OCAML_CSP)/csp.cmx $(GUEDRA_LIB) $< \
		-cclib "-L `ocamlc -where`" -cclib "$(LDFLAGS)"

all:	$(TARGETS)

$(TARGETS):	$(OCAML_CSP)/csp.cmx $(GUEDRA_LIB)

clean:
	rm *.cmi *.cmo *.cmx *.cmt *.cmti *.o *.x *.exe
