Accompanying code to "Data-flow analyses as effects and graded monads"
by Andrej Ivaskovic, Alan Mycroft (University of Cambridge) and
Dominic Orchard (University of Kent) appearing at FSCD 2020.

This code is associated with Section 4.4 and Appendix B of the paper.

This code is best for interacting with via `ghci`, i.e., this is not a
standalone application, but rather an explanation of an idea about
graded monads, effects, and dataflow.

(This code has been tested with at least GHC 8.10.1 but it should work
with versions at least as early as 8.2.*)

Running

     cabal build

should make sure you have the necessary dependencies, which is really
just `type-level-set` and `constraints` which can be installed separately, e.g.

    cabal install type-level-sets-0.8.9.0
    cabal install constraints-0.12

(or run `cabal install`).

You can then interact via ghci, e.g.

    $ cd src
	$ ghci Dataflow.hs
	Ok, two modules loaded.

	*Dataflow> :t exm3
	example3
		:: MultiState s b
			-> MultiState (Gen "x" :|> (Gen "y" :|> (Kill "z" :|> s))) b

	*Dataflow> :t exm3'
	example3' :: Set '["x", "y"]

	*Dataflow> :t exampleAlt
	exampleAlt
		:: MultiState
			(Gen "x" :|> (Gen "y" :|> Alt (Gen "z") (Gen "r"))) Int

	*Dataflow> :t exampleAlt'
	exampleAlt' :: Set '["r", "x", "y", "z"]
