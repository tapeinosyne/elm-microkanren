module MicroKanren.Operators(..) where

import MicroKanren exposing ( mplus, bind, identical )


(++) = mplus

(>>=) = bind


(===) = identical

(≡) = identical
