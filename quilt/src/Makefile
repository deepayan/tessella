all:
	bash createProject.sh
	make -f Makefile.quilt
	if test -e libquilt.so; then cp libquilt.so quilt.so; fi
	## for macx:
	if test -e libquilt.dylib; then cp libquilt.dylib quilt.so; fi


clean:
	rm -f *.so *.o *.a libqt* quilt.pro Makefile.quilt moc*.cpp
