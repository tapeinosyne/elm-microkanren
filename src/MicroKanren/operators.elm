module MicroKanren.Operators(..) where

{-| Infix operators for μKanren

@docs (++), (>>=), (===), (≡)
-}

import MicroKanren exposing (Term, Stream, Goal, mplus, bind, identical)

{-| Infix alias of `mplus`. -}
(++) : Stream a -> Stream a -> Stream a
(++) = mplus

{-| Infix alias of `bind`. -}
(>>=) : Stream a -> Goal a -> Stream a
(>>=) = bind


{-| Infix alias of `identical`. -}
(===) : Term a -> Term a -> Goal a
(===) = identical

{-| Unicode alias of `identical`. -}
(≡) : Term a -> Term a -> Goal a
(≡) = identical
