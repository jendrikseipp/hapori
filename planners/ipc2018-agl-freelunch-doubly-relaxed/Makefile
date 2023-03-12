all:
	cd freelunch-master;ant -f ./build.xml;ant -f ./fl-ipc18.xml
	cp freelunch-master/fl-ipc18.jar ./

	cd lingeling-bbe;./configure.sh;make
	cp lingeling-bbe/liblgl.a incplan/lib

	cd incplan/build/; cmake ../src/; make incplan-lgl
	cp incplan/bin/incplan-lgl ./

