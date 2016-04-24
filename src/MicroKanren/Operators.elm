module MicroKanren.Operators(..) where

{-|
@docs (++), (>>=), (===), (≡)
-}
import MicroKanren exposing ( Goal, Stream, Term, mplus, bind, identical )


{-|
Merges two streams fairly.
-}
(++) : Stream a -> Stream a -> Stream a
(++) = mplus

{-|
Invokes the goal on each element in the stream merging the results fairly.
-}
(>>=) : Stream a -> Goal a -> Stream a
(>>=) = bind


{-|
Succeeds when the arguments unify.
-}
(===) : Term a -> Term a -> Goal a
(===) = identical

{-|
Succeeds when the arguments unify.
-}
(≡) : Term a -> Term a -> Goal a
(≡) = identical
