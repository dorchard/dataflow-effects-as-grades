(by Andrej Ivaskovic, Alan Mycroft (University of Cambridge) and Dominic Orchard (University of Kent)).

This code is best for interacting with via `ghci`, i.e., this is not a standalone application, but rather an explanation
of and idea about graded monads, effects, and dataflow.

(This code has been tested with at least GHC 8.6.2 but it should work with versions at least as early as 8.2.*)

Running

     cabal build

should make sure you have the necessary dependencies, which is really just `type-level-set-0.8.9.0` which can be installed separately, e.g.

	cabal install type-level-sets-0.8.9.0

You can then interact via ghci, e.g.

	$ ghci Dataflow.hs
	Ok, two modules loaded.

	*Dataflow> :t example3
	example3
		:: MultiState s b
			-> MultiState (Gen "x" :|> (Gen "y" :|> (Kill "z" :|> s))) b

	*Dataflow> :t example3'
	example3' :: Set '["x", "y"]

	*Dataflow> :t exampleAlt
	exampleAlt
		:: MultiState
			(Gen "x" :|> (Gen "y" :|> Alt (Gen "z") (Gen "r"))) Int

	*Dataflow> :t exampleAlt'
	exampleAlt' :: Set '["r", "x", "y", "z"]
