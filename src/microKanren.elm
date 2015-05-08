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

-}

import Dict


type alias Var = Int

type Term a
  = LVar Var
  | LVal a
  | Pair (Term a) (Term a)

type alias Substitution a = Dict.Dict Var (Term a)

type alias State a
  = { s : Substitution a
    , c : Var }

type Stream a
  = Empty
  | Immature (() -> Stream a)
  | Mature (State a) (Stream a)

type alias Goal a
  = State a -> Stream a


unit : Goal a
unit sc = Mature sc Empty

mzero : Stream a
mzero = Empty


walk : Term a -> Substitution a -> Term a
walk t s =
  case t of
    LVar v -> case Dict.get v s of
      Just u  -> walk u s
      Nothing -> t
    _ -> t

ext_s : Var -> Term a -> Substitution a -> Substitution a
ext_s k v s = Dict.insert k v s

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


mplus : Stream a -> Stream a -> Stream a
mplus s1 s2 =
  case s1 of
    Empty            -> s2
    Immature stream  -> Immature (\() -> mplus s2 (stream ()))
    Mature sc stream -> Mature sc (mplus s2 stream)

bind : Stream a -> Goal a -> Stream a
bind s g =
  case s of
    Empty            -> mzero
    Immature stream  -> Immature (\() -> bind (stream ()) g)
    Mature sc stream -> mplus (g sc) (bind stream g)


identical : Term a -> Term a -> Goal a
identical u v =
  \sc -> case unify u v sc.s of
           Just s' -> unit {sc | s <- s' }
           Nothing -> mzero

callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
  \sc -> f (LVar sc.c) {sc | c <- sc.c + 1}

disj : Goal a -> Goal a -> Goal a
disj g1 g2 =
  \sc -> g1 sc `mplus` g2 sc

conj : Goal a -> Goal a -> Goal a
conj g1 g2 =
  \sc -> g1 sc `bind` g2
