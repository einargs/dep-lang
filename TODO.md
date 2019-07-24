# TODO
1. Implement a type checker that validates a finalized, no-module environment.
2. Build elaborator system ala Idris for translating higher-level langauge into
   LTT.
3. Make a mock higher-level AST to actually translate into LTT.
4. Consider using typeclass-associated pattern synonyms to make the LTT AST
   easily extensible so that, e.g., WHNF and the removal of all holes and guesses
   can be guarenteed by construction.

# FUTURE
- Totality checking
- Use `liquidhaskell-cabal` to automate liquid type checking

# NOTES
- Let's work with explicit universe levels for now. There are lots of different
  ways of solving the problem, and I'd rather focus on other aspects first.
  resources:
  - https://pigworker.wordpress.com/2015/01/09/universe-hierarchies/
  - https://github.com/pikelet-lang/pikelet/issues/10
