module MicroKanren exposing
  ( Var, Term(LVar, LVal, Pair)
  , Substitution, State
  , Stream(Empty, Immature, Mature)
  , Goal
  , unit, mzero
  , mplus, bind
  , walk, extend, unify
  , callFresh, identical, conjoin, disjoin
  )

{-| μKanren provides a minimal, independent core for relational programming in Elm,
as described by Hemann and Friedmann in [µKanren: A Minimal Functional Core
for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf).

This module only provides a barebones typed foundation upon which it is possible
to build larger languages in the [miniKanren family](http://www.minikanren.org),
and lacks much of the convenience user-facing libraries rightfully require.

## Terms of a μKanren Program
@docs Var, Term, Substitution

### Traversing and Manipulating Substitutions
@docs walk, extend, unify

## Creating and Manipulating Streams
@docs State
@docs Stream, mzero
@docs Goal, unit
@docs mplus, bind

## Goal Constructors
@docs callFresh, identical, conjoin, disjoin
-}

import Dict

{-| The `Var` type is an index (or counter),
used to track the results of logic operations. -}
type alias Var = Int

{-| The terms on which a μKanren program operates.

Three variants are established:
  - `LVar`, the identifier of a logic variable; effectively a term-level `Var`
    acting as a reference.
  - `LVal`, which wraps some value of type `a` for use by logic operations.
  - `Pair`, two terms of the same type `a`.
-}
type Term a
  = LVar Var
  | LVal a
  | Pair (Term a) (Term a)

{-| A dictionary of variable-term bindings. -}
type alias Substitution a = Dict.Dict Var (Term a)

{-| A state encapsulates the substitution `s`, which encompasses current variable-term bindings,
and the counter `c`, which represents the index of the next unbound (“fresh”) logic variable. -}
type alias State a
  = { s : Substitution a
    , c : Var }

{-| A potentially infinite sequence of states.

A stream may be:
  - empty;
  - mature, when the head is a state that has already been computed;
  - immature, when the head is a thunk containing a delayed computation.
-}
type Stream a
  = Empty
  | Immature (() -> Stream a)
  | Mature (State a) (Stream a)

{-| A goal to be pursued within the received state.
A successful goal returns a non-empty stream, containing the states which satisfied it. -}
type alias Goal a
  = State a -> Stream a

{-| The trivial goal, which lifts the state into a mature stream
whose only element is that state. -}
unit : Goal a
unit = \sc -> Mature sc Empty

{-| An alias for the empty stream. -}
mzero : Stream a
mzero = Empty

{-| Given a `LVar`, traverse the substitution and return its value.
If the given term cannot be found, or is not a variable, the term itself is returned. -}
walk : Term a -> Substitution a -> Term a
walk t s =
  case t of
    LVar v -> case Dict.get v s of
      Just u  -> walk u s
      Nothing -> t
    _ -> t

{-| Extend the substitution with a new variable-term binding. -}
extend : Var -> Term a -> Substitution a -> Substitution a
extend k v s = Dict.insert k v s

{-| Unify two terms in the received substitution `s`,
potentially extending it.

- `LVar` variables unify when they refer to the same index,
  i.e. they are the same variable. `s` is returned unchanged..
- `LVal` values unify when they are equivalent under Elm's native `(==)`.
  `s` is returned unchanged.
- Pairs unify when their terms unify pairwise.
- Finally, an `LVar` variable and an `LVal` value unify under an extended substitution `s1`,
  where they form a new binding.

If unification fails, `Nothing` is returned.
-}
unify : Term a -> Term a -> Substitution a -> Maybe (Substitution a)
unify u v s =
  let u1 = walk u s
      v1 = walk v s
  in
      case (u1, v1) of
        (LVar n, LVar m) -> if n == m then Just s else Nothing
        (LVal x, LVal y) -> if x == y then Just s else Nothing
        (LVar n, _) -> Just (extend n v1 s)
        (_, LVar m) -> Just (extend m u1 s)
        (Pair x y, Pair x1 y1) -> case unify x x1 s of
                                    Just s1 -> unify y y1 s1
                                    Nothing -> Nothing
        _ -> Nothing


{-| Merge two streams by interleaving their states, so that infinite streams
will not prevent finite streams from yielding their state. -}
mplus : Stream a -> Stream a -> Stream a
mplus s1 s2 =
  case s1 of
    Empty            -> s2
    Immature stream  -> Immature (\() -> mplus s2 (stream ()))
    Mature sc stream -> Mature sc (mplus s2 stream)

{-| Invoke a goal on each element of the given stream,
and interleave the resulting streams. -}
bind : Stream a -> Goal a -> Stream a
bind s g =
  case s of
    Empty            -> mzero
    Immature stream  -> Immature (\() -> bind (stream ()) g)
    Mature sc stream -> mplus (g sc) (bind stream g)


{-| Create a goal which succeeds if the two terms unify in the received state. -}
identical : Term a -> Term a -> Goal a
identical u v =
  \sc -> case unify u v sc.s of
           Just s1 -> unit { sc | s = s1 }
           Nothing -> mzero

{-| Create a goal which introduces a new logic variable for use by another goal. -}
callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
  \sc -> f (LVar sc.c) {sc | c = sc.c + 1}


{-| Create a goal which succeeds if either of the received goals are achieved.

Effectively a binary disjunction of goals. -}
disjoin : Goal a -> Goal a -> Goal a
disjoin g1 g2 =
  \sc -> mplus (g1 sc) (g2 sc)

{-| Create a goal which succeeds if the second goal is achievable within the stream
generated by the first goal.

Effectively a binary conjunction of goals. -}
conjoin : Goal a -> Goal a -> Goal a
conjoin g1 g2 =
  \sc -> bind (g1 sc) g2
