all:
	make -C madagascar
	cp ./madagascar/MpC ./

	cd lingeling-bbe;./configure.sh;make
	cp lingeling-bbe/liblgl.a incplan/lib

	cd incplan/build/; cmake ../src/; make incplan-lgl
	cp incplan/bin/incplan-lgl ./