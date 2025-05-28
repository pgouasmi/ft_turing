# NAME=ft_turing
# FILE=turing_machine.ml

# all: compile

# compile:
# 	ocamlfind ocamlc -package yojson -linkpkg -o ${NAME} ${FILE}

# exec: compile
# 	./${NAME} sub.json 111-11

# clean:
# 	rm -f *.cmi *.cmo ${NAME}

# re: clean all


OCAMLC = ocamlfind ocamlc
PACKAGES = -package yojson
LINKPKG = -linkpkg

SOURCES = types.ml machine.ml main.ml
TARGET = ft_turing

all: $(TARGET)

$(TARGET): $(SOURCES)
	$(OCAMLC) $(PACKAGES) $(LINKPKG) -o $@ $^

clean:
	rm -f *.cmi *.cmo $(TARGET)

.PHONY: all clean