all: kk

FILES = Core.ml Heap.ml Core_in_channel.ml kk.ml

kk: $(FILES)
	ocamlbuild -lib unix kk.native;
	cp -L kk.native kk

clean:
	rm -rf _build kk.native kk
