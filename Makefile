all: kk

FILES = Core.ml Heap.ml Core_in_channel.ml Karmarkar_karp.ml 

kk: $(FILES) kk.ml
	ocamlbuild -lib unix kk.native;
	cp -L kk.native kk

rand: $(FILES) Solution.ml rand.ml
	ocamlbuild -cflags -g -lib unix rand.native
	cp -L rand.native rand

clean:
	rm -rf _build kk.native kk rand.native rand
