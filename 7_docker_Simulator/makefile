all: main.native

main.native: 
	ocamlbuild -use-menhir -pkgs 'domainslib,unix' main.native

clean: 
	rm -r ./_build
	rm main.native
	rm -r IoT-System