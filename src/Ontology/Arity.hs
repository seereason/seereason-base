module Ontology.Arity
    ( HasArity(arity)
    ) where

-- |A class that characterizes how many arguments a predicate or
-- function takes.  Depending on the context, a result of Nothing may
-- mean that the arity is undetermined or unknown.  However, even if
-- this returns Nothing, the same number of arguments must be passed
-- to all uses of a given predicate or function.
class HasArity p where
    arity :: p -> Maybe Int
