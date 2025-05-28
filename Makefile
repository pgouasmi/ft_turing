NAME=ft_turing
FILE=turing_machine.ml

all: compile exec

compile:
	ocamlfind ocamlc -package yojson -linkpkg -o ${NAME} ${FILE}

exec:
	./${NAME} sub.json 111-11

clean:
	rm -f *.cmi *.cmo ${NAME}

re: clean all