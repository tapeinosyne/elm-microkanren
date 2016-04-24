module MicroKanren
  ( Var
  , Term(LVar, LVal, Pair)
  , Substitution, State
  , Stream(Empty, Immature, Mature)
  , Goal
  , unit, mzero
  , walk, ext_s, unify
  , callFresh, identical, conj, disj
  , mplus, bind
  ) where

{-| μKanren […]

Forsaken docs justly quibble the vexed programmer's waning zeal

@docs Var, Term, Substitution, State, Stream, Goal
@docs unit, mzero, walk, ext_s, unify, callFresh, identical, conj, disj, mplus, bind
-}

import Dict

{-|
An index for a fresh logic variable.
-}
type alias Var = Int

{-|
A fresh logic variable, any elm value and pairs of the foregoing.
-}
type Term a
  = LVar Var
  | LVal a
  | Pair (Term a) (Term a)

{-|
An association between variables and terms.
-}
type alias Substitution a = Dict.Dict Var (Term a)

{-|
A pair of a substitution and a non-negative fresh variable counter.
-}
type alias State a
  = { s : Substitution a
    , c : Var }

{-|
A possibly-unbounded list of states.
-}
type Stream a
  = Empty
  | Immature (() -> Stream a)
  | Mature (State a) (Stream a)

{-|
Analogous to a predicate with a sequence of (enlarged) states.
-}
type alias Goal a
  = State a -> Stream a


{-|
Lifts the state into a stream whose only element is that state.
-}
unit : Goal a
unit sc = Mature sc Empty

{-|
The empty stream.
-}
mzero : Stream a
mzero = Empty


{-|
Searches for a variables value in the substitution.
-}
walk : Term a -> Substitution a -> Term a
walk t s =
  case t of
    LVar v -> case Dict.get v s of
      Just u  -> walk u s
      Nothing -> t
    _ -> t

{-|
Extends the substitution with a new binding.
-}
ext_s : Var -> Term a -> Substitution a -> Substitution a
ext_s k v s = Dict.insert k v s

{-|
Extends the substitution if exactly one term walks to a variable.

Leaves the substitution unaltered if both terms:
* walk to the same variable
* walk to the same value
* pairwise walk to the same substitution.

Fails otherwise.
-}
unify : Term a -> Term a -> Substitution a -> Maybe (Substitution a)
unify u v s =
  let u' = walk u s
      v' = walk v s
  in
      case (u', v') of
        (LVar n, LVar m) -> if n == m then Just s else Nothing
        (LVal x, LVal y) -> if x == y then Just s else Nothing
        (LVar n, _) -> Just (ext_s n v' s)
        (_, LVar m) -> Just (ext_s m u' s)
        (Pair x y, Pair x1 y1) -> case unify x x1 s of
                                    Just s' -> unify y y1 s'
                                    Nothing -> Nothing
        _ -> Nothing


{-|
Merges two streams fairly.
-}
mplus : Stream a -> Stream a -> Stream a
mplus s1 s2 =
  case s1 of
    Empty            -> s2
    Immature stream  -> Immature (\() -> mplus s2 (stream ()))
    Mature sc stream -> Mature sc (mplus s2 stream)

{-|
Invokes the goal on each element in the stream merging the results fairly.
-}
bind : Stream a -> Goal a -> Stream a
bind s g =
  case s of
    Empty            -> mzero
    Immature stream  -> Immature (\() -> bind (stream ()) g)
    Mature sc stream -> mplus (g sc) (bind stream g)


{-|
Succeeds when the terms unify.
-}
identical : Term a -> Term a -> Goal a
identical u v =
  \sc -> case unify u v sc.s of
           Just s' -> unit {sc | s = s' }
           Nothing -> mzero

{-|
Creates a fresh (new) logic variable.
-}
callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
  \sc -> f (LVar sc.c) {sc | c = sc.c + 1}

{-|
Binary disjunction of goals.
-}
disj : Goal a -> Goal a -> Goal a
disj g1 g2 =
  \sc -> g1 sc `mplus` g2 sc

{-|
Binary conjunction of goals.
-}
conj : Goal a -> Goal a -> Goal a
conj g1 g2 =
  \sc -> g1 sc `bind` g2
