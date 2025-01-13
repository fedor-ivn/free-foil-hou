{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SOAS where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import Control.Monad.Free.Foil
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Maybe (mapMaybe)
import Data.ZipMatchK
import Debug.Trace (trace)
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)
import qualified Language.Lambda.Syntax.Abs as Raw
import Unsafe.Coerce (unsafeCoerce)

-- | Second-order signature for metavariable applications.
-- This way, metavariables can be added to any other signature using 'Sum'.
data MetaAppSig metavar scope term
  = MetaAppSig metavar [term] -- ^ A metavariable is always fully applied to a list of arguments.
  deriving (Functor, Foldable, Traversable, GHC.Generic)
deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig
deriveGenericK ''MetaAppSig
instance (ZipMatchK a) => ZipMatchK (MetaAppSig a)

-- | A second-order abstract syntax (SOAS) is generated from a signature
-- by adding parametrised metavariables.
--
-- Note that here we also parametrise SOAS by the type of binders (patterns) @binder@.
type SOAS binder metavar sig n = AST binder (Sum sig (MetaAppSig metavar)) n

-- | A scoped version of 'SOAS'. 
-- 'ScopedSOAS' is to 'SOAS' what 'ScopedAST' is to 'AST'.
type ScopedSOAS binder metavar sig n = ScopedAST binder (Sum sig (MetaAppSig metavar)) n

-- | A convenient pattern synonym for parametrised metavariables.
pattern MetaApp
  :: metavar
  -> [SOAS binder metavar sig n]
  -> SOAS binder metavar sig n
pattern MetaApp metavar args = Node (R2 (MetaAppSig metavar args))

-- | A body of a metavariable substitution for one metavariable.
data MetaAbs binder sig t where
  MetaAbs
    :: Foil.NameBinderList Foil.VoidS n -- ^ A list of binders corresponding to metavariable arguments.
    -> Foil.NameMap n t                 -- ^ Types of parameters.
    -> AST binder sig n                 -- ^ Term to substitute the metavariable with.
    -> MetaAbs binder sig t

-- | A metavariable substitution is a pair of a metavariable name and its body.
newtype MetaSubst binder sig metavar ext t = MetaSubst
  { getMetaSubst :: (metavar, MetaAbs binder (Sum sig ext) t)
  }

-- | A collection of metavariable substitutions (for simultaneous substitution of multiple metavariables).
newtype MetaSubsts binder sig metavar ext t = MetaSubsts
  { getMetaSubsts :: [MetaSubst binder sig metavar ext t]
  }

-- | Apply metavariable substitutions to a SOAS term.
applyMetaSubsts
  :: ( Bifunctor sig
     , Eq metavar
     , Foil.Distinct n
     , Foil.CoSinkable binder
     , Foil.SinkableK binder
     )
  => (metavar -> metavar')  -- ^ Renaming of metavariables (normally, 'id' or some kind of injection).
  -> Foil.Scope n           -- ^ Scope of terms.
  -> MetaSubsts binder sig metavar (MetaAppSig metavar') t    -- ^ Metavariable substitutions.
  -> SOAS binder metavar sig n    -- ^ The original SOAS term.
  -> SOAS binder metavar' sig n
applyMetaSubsts rename scope substs = \case
  Var x -> Var x
  -- FIXME: MetaApp metavar args ->
  Node (R2 (MetaAppSig metavar args)) ->
    let args' = map apply args
     in case lookup metavar (getMetaSubst <$> getMetaSubsts substs) of
          Just (MetaAbs names _types body) ->
            let nameMap = toNameMap Foil.emptyNameMap names args'
                substs' = Foil.nameMapToSubstitution nameMap
             in substitute scope substs' body
          Nothing -> MetaApp (rename metavar) args'
  Node (L2 term) ->
    let term' = bimap (goScoped rename scope substs) apply term
     in Node (L2 term')
 where
  apply = applyMetaSubsts rename scope substs

  toNameMap :: Foil.NameMap n a -> Foil.NameBinderList n l -> [a] -> Foil.NameMap l a
  toNameMap nameMap Foil.NameBinderListEmpty [] = nameMap
  toNameMap nameMap (Foil.NameBinderListCons binder rest) (x : xs) = toNameMap fresh rest xs
   where
    fresh = Foil.addNameBinder binder x nameMap
  toNameMap _ _ _ = error "mismatched name list and argument list"

  goScoped
    :: ( Bifunctor sig
       , Eq metavar
       , Foil.Distinct n
       , Foil.CoSinkable binder
       , Foil.SinkableK binder
       )
    => (metavar -> metavar')
    -> Foil.Scope n
    -> MetaSubsts binder sig metavar (MetaAppSig metavar') t
    -> ScopedAST binder (Sum sig (MetaAppSig metavar)) n
    -> ScopedAST binder (Sum sig (MetaAppSig metavar')) n
  goScoped rename' scope' substs' (ScopedAST binder body) =
    case Foil.assertDistinct binder of
      Foil.Distinct ->
        ScopedAST binder (applyMetaSubsts rename' newScope substs' body)
   where
    newScope = Foil.extendScopePattern binder scope'

-- | Combine (compose) metavariable substitutions.
-- 
-- TODO: refactor
combineMetaSubsts
  :: ( Eq metavar
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     )
  => [MetaSubsts binder sig metavar ext t]
  -> [MetaSubsts binder sig metavar ext t]
combineMetaSubsts = foldr go []
 where
  go x [] = [x]
  go x xs = mapMaybe (inner x) xs

  inner (MetaSubsts xs) (MetaSubsts ys)
    | conflicts = trace "there are conflicts" Nothing
    | otherwise = trace "no conflicts" return (MetaSubsts (xs ++ ys))
   where
    conflicts =
      and
        [ case Foil.unifyPatterns binders binders' of
          Foil.SameNameBinders _ ->
            case Foil.assertDistinct binders of
              Foil.Distinct ->
                let scope' = Foil.extendScopePattern binders Foil.emptyScope
                 in alphaEquiv scope' body body'
          Foil.NotUnifiable -> False
          -- FIXME: RenameLeftNameBinder _ _ ->
          _ -> error "unexpected renaming"
        | MetaSubst (m, MetaAbs binders _types body) <- xs
        , MetaSubst (m', MetaAbs binders' _types' body') <- ys
        , m == m'
        ]

-- | Match left-hand side (with metavariables) against the rigid right-hand side.
--
-- If matching is successful, it produces metavariable substitutions that when applied to LHS make it syntactically equal to RHS.
-- For example, matching
--   M[f x, g] = g (f x)
-- produces substitution
--   M[z₁, z₂] ↦ z₂ z₁
--
-- There may be more than one solution for matching, e.g.
--   M[f x, f x] = f x
-- can be solved with two different substitutions:
--   1. M[z₁, z₂] ↦ z₁
--   2. M[z₁, z₂] ↦ z₂
-- 
-- Hence, this function produces a list of possible substitutions.
match
  :: ( Bitraversable sig
     , ZipMatchK sig
     , Eq metavar
     , Foil.Distinct n
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     )
  => Foil.Scope n                           -- ^ The current scope.
  -> SOAS binder metavar sig n              -- ^ The left-hand side (with metavariables that we solve for).
  -> AST binder (Sum sig ext) n             -- ^ The right hand side (rigid).
  -> [MetaSubsts binder sig metavar ext t]
match scope lhs rhs =
  case (lhs, rhs) of
    (Var x, Var y) | x == y -> trace "matched same vars" return (MetaSubsts [])
    (MetaApp metavar args, _) ->
      withFreshNameBinderList
        args
        Foil.emptyScope
        Foil.NameBinderListEmpty
        $ \scope' binderList ->
          trace
            "matching metavar"
            map
            ( \(term, MetaSubsts substs) ->
                let dummyTypes =
                      Foil.addNameBinders
                        binderList
                        ( unsafeCoerce (repeat (Raw.Base (Raw.VarIdent "T")))
                        )
                        Foil.emptyNameMap
                    metaAbs = MetaAbs binderList dummyTypes term
                    subst = MetaSubst (metavar, metaAbs)
                 in MetaSubsts (subst : substs)
            )
            (matchMetavar scope' binderList scope args rhs)
    (Node (L2 leftTerm), Node (L2 rightTerm)) ->
      -- AppSig t1 t2 -- left term
      -- AppSig a1 a2 -- right term
      -- AppSig (t1, a1) (t2, a2) -- node
      -- AppSig [s1, s2] [s3, s4] -- bimap _ (match ...) node
      -- [AppSig s1 s3, AppSig s1 s4, AppSig s2 s3, AppSig s2 s4] -- bitraverse _ (match ...) node
      -- [s1 + s3, s1 + s4, s2 + s3, s2 + s4] -- map (combineMetaSubsts' . biList) ...
      -- [[s13], [], [], [s24]]
      case zipMatch2 leftTerm rightTerm of
        Just node ->
          let traversed = bitraverse (uncurry (matchScoped scope)) (uncurry (match scope)) node
           in trace "terms matched, combine substitutions" $
                concatMap (combineMetaSubsts . biList) traversed
        Nothing -> trace "term structs doesn't match" []
    (Node (L2 _term), Node (R2 _ext)) -> []
    (_, Var _) -> []
    (Var _, _) -> []
    _ -> []

-- | Same as 'match' but for scoped terms.
matchScoped
  :: ( Bitraversable sig
     , ZipMatchK sig
     , Foil.Distinct n
     , Eq metavar
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable ext
     , ZipMatchK (Sum sig ext)
     )
  => Foil.Scope n
  -> ScopedSOAS binder metavar sig n
  -> ScopedAST binder (Sum sig ext) n
  -> [MetaSubsts binder sig metavar ext t]
matchScoped scope (ScopedAST binder lhs) (ScopedAST binder' rhs) =
  case trace "matching scoped terms" Foil.unifyPatterns binder binder' of
    -- \x.t1 = \x.t2
    Foil.SameNameBinders _ ->
      case trace "same name binders" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
           in match scope' lhs rhs
    -- \x.t1 = \y.t2
    Foil.RenameLeftNameBinder _ rename ->
      case trace "rename left binder" Foil.assertDistinct binder' of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder' scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) lhs
           in match scope' lhs' rhs
    Foil.RenameRightNameBinder _ rename ->
      case trace "rename right binder" Foil.assertDistinct binder of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binder scope
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename) rhs
           in match scope' lhs rhs'
    Foil.RenameBothBinders binders rename1 rename2 ->
      case trace "rename both binders" Foil.assertDistinct binders of
        Foil.Distinct ->
          let scope' = Foil.extendScopePattern binders scope
              lhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename1) lhs
              rhs' = Foil.liftRM scope' (Foil.fromNameBinderRenaming rename2) rhs
           in match scope' lhs' rhs'
    Foil.NotUnifiable -> trace "not unifiable" []

-- | A special case of 'match', when LHS is a parametrised metavariable.
--
-- Note that here we do not have an explicit name for that metavariable,
-- and only consider its arguments.
-- This is helpful when recursively matching with fresh metavariables,
-- since we do not need to generate any actual fresh names for such metavariables,
-- saving in complexity and performance.
--
-- For each possible solution, this function produces a pair of
--
-- 1. Body of the metavariable substitution.
-- 2. Metavariable substitutions for the metavariables that occur in the parameters of the root metavariable on the LHS.
matchMetavar
  :: forall metavar n m binder sig ext t
   . ( Eq metavar
     , Foil.Distinct n
     , Foil.Distinct m
     , Foil.UnifiablePattern binder
     , Foil.SinkableK binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK (Sum sig ext)
     )
  => Foil.Scope m                         -- ^ Scope for the body of the metavariable substitution (at root of LHS).
  -> Foil.NameBinderList Foil.VoidS m     -- ^ List of binders for the metavariable parameters (size of the list should match the actual list of parameters).
  -> Foil.Scope n                         -- ^ Current scope.
  -> [SOAS binder metavar sig n]          -- ^ A list of arguments of the parametrised metavariable on the left-hand side.
  -> AST binder (Sum sig ext) n           -- ^ The right-hand side term (rigid).
  -> [(AST binder (Sum sig ext) m, MetaSubsts binder sig metavar ext t)]
matchMetavar metavarScope metavarNameBinders scope args rhs =
  let
    projections = project metavarNameBinders args
   in
    projections
      ++ case rhs of
        -- M[t1, t2] = F(x.u1, u2)
        -- T1[t1, t2, x] = u1
        -- -- [([z1, z2, z3] -> w1, substs1)]
        -- T2[t1, t2] = u2
        -- -- [([z1, z2] -> w2, substs2)]
        -- bitraverse ... node = [F(([z1, z2, z3] -> w1, substs1), ([z1, z2] -> w2, substs2))]
        -- final result: [([z1, z2, z3] -> w, substs)]
        Node node -> do
          traversed <-
            bitraverse
              (matchMetavarScoped metavarScope metavarNameBinders scope args)
              (matchMetavar metavarScope metavarNameBinders scope args)
              node
          let term = Node (bimap fst fst traversed)
          substs <- combineMetaSubsts (biList (bimap snd snd traversed))
          return (term, substs)
        _ -> []
 where
  project
    :: (Foil.Distinct i)
    => Foil.NameBinderList i m
    -> [SOAS binder metavar sig n]
    -> [(AST binder (Sum sig ext) m, MetaSubsts binder sig metavar ext t)]
  project Foil.NameBinderListEmpty [] = []
  project (Foil.NameBinderListCons x xs) (arg : args') =
    case Foil.assertDistinct x of
      Foil.Distinct ->
        case (Foil.assertExt xs, Foil.assertDistinct xs) of
          (Foil.Ext, Foil.Distinct) ->
            let substs = match scope arg rhs
                term = Var (Foil.sink (Foil.nameOf x))
             in map (term,) substs ++ project xs args'
  project _ _ = error "mismatched name list and argument list"

-- | Same as 'matchMetavar' but for scoped term.
matchMetavarScoped
  :: forall n m metavar binder sig ext t
   . ( Foil.Distinct n
     , Foil.Distinct m
     , Eq metavar
     , Foil.UnifiablePattern binder
     , Bitraversable sig
     , Bitraversable ext
     , ZipMatchK sig
     , ZipMatchK (Sum sig ext)
     , Foil.SinkableK binder
     )
  => Foil.Scope m
  -> Foil.NameBinderList Foil.VoidS m
  -> Foil.Scope n
  -> [SOAS binder metavar sig n]
  -> ScopedAST binder (Sum sig ext) n
  -> [(ScopedAST binder (Sum sig ext) m, MetaSubsts binder sig metavar ext t)]
matchMetavarScoped metavarScope metavarNameBinders scope args (ScopedAST binder rhs) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      Foil.withRefreshedPattern @_ @_ @(AST binder sig)
        metavarScope
        binder
        $ \_extendSubst metavarBinder ->
          let metavarScope' = Foil.extendScopePattern metavarBinder metavarScope
              freshNameBinders = Foil.nameBinderListOf metavarBinder
              metavarNameBinders' =
                concatNameBinderLists freshNameBinders metavarNameBinders
              scope' = Foil.extendScopePattern binder scope
              args' = map Foil.sink args ++ map Var (Foil.namesOfPattern binder)
              result = matchMetavar metavarScope' metavarNameBinders' scope' args' rhs
           in map (first (ScopedAST metavarBinder)) result

-- | Generate fresh name binders for a list of things.
-- This is useful to generate proper name binders of metavariable parameters.
withFreshNameBinderList
  :: (Foil.Distinct n)
  => [a]
  -> Foil.Scope n
  -> Foil.NameBinderList i n
  -> ( forall l
        . (Foil.Distinct l)
       => Foil.Scope l
       -> Foil.NameBinderList i l
       -> r
     )
  -> r
withFreshNameBinderList [] scope binders cont = cont scope binders
withFreshNameBinderList (_ : xs) scope binders cont =
  Foil.withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        binders' = push binder binders
     in withFreshNameBinderList xs scope' binders' cont

-- | /O(n)/. Push a name binder into the end of a name binder list.
--
-- /Should be in "Control.Monad.Foil"./
push :: Foil.NameBinder i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
push x Foil.NameBinderListEmpty = Foil.NameBinderListCons x Foil.NameBinderListEmpty
push x (Foil.NameBinderListCons y ys) = Foil.NameBinderListCons y (push x ys)

-- | /O(n)/. Concatenate name binder lists.
--
-- Should be in "Control.Monad.Foil".
concatNameBinderLists :: Foil.NameBinderList i l -> Foil.NameBinderList n i -> Foil.NameBinderList n l
concatNameBinderLists lst Foil.NameBinderListEmpty = lst
concatNameBinderLists lst (Foil.NameBinderListCons x xs) =
  Foil.NameBinderListCons x (concatNameBinderLists lst xs)
