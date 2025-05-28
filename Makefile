NAME=ft_turing
FILE=turing_machine.ml

all: compile

compile:
	ocamlfind ocamlc -package yojson -linkpkg -o ${NAME} ${FILE}

exec: compile
	./${NAME} sub.json 111-11

clean:
	rm -f *.cmi *.cmo ${NAME}

re: clean all