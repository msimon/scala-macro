do:
	scalac -feature macros.scala && scalac -feature test.scala && scala Test

debug:
	scalac macros.scala && scalac -Ymacro-debug-lite test.scala && scala Test

clean:
	rm -f *.class
