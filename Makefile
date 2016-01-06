yaml-union.1 : yaml-union.1.md
	pandoc -t man -s $< > $@
