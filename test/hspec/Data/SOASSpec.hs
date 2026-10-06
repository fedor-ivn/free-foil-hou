{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.SOASSpec (spec) where

import qualified Control.Monad.Foil.Internal as Foil
import Control.Monad.Free.Foil (AST (..), ScopedAST (..), alphaEquiv)
import Control.Monad (forM_)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Sum (Sum (..))
import Data.Bifunctor.TH
import Data.List (elemIndex, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.SOAS
import Data.ZipMatchK
import Data.ZipMatchK.Bifunctor ()
import GHC.Generics (Generic)
import Generics.Kind.TH (deriveGenericK)
import Test.Hspec

-- A small language with constants, mixed operators and multiple binders.
-- Binder annotations record the types in binding order.
data Ty = A | B | Product Ty Ty | Arrow Ty Ty | Binders [Ty]
  deriving (Eq, Show)
instance ZipMatchK Ty where zipMatchWithK = zipMatchViaEq

newtype Id = Id Int deriving (Eq, Ord, Show, Num)
instance ZipMatchK Id where zipMatchWithK = zipMatchViaEq

data Sig scoped term
  = Constant Id
  | Unary term
  | Pair term term
  | Bind scoped
  | Let term scoped
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
deriveBifunctor ''Sig
deriveBifoldable ''Sig
deriveBitraversable ''Sig
deriveGenericK ''Sig
instance ZipMatchK Sig

instance TypedBinder Foil.NameBinderList Ty where
  addBinderTypes binders (Binders types) = Foil.addNameBinderList binders types
  addBinderTypes _ _ = error "test binder needs a list of types"

type Term n = TypedSOAS Foil.NameBinderList Id Sig n Ty
type Scoped n = TypedScopedSOAS Foil.NameBinderList Id Sig n Ty
type Substs = MetaSubsts (AnnBinder Ty Foil.NameBinderList)
  (AnnSig Ty (Sum Sig (MetaAppSig Id))) Id Ty

-- Views use positions, not foil identifiers, and do not call matching.
data View = V Int | N Ty (Sig View View) | M Id Ty [View] | Scope Ty View
  deriving (Eq, Show)

view :: Foil.Distinct n => [Foil.Name n] -> Term n -> View
view names (Var x) = V (fromJust (elemIndex x names))
view names (MetaApp m args ty) = M m ty (map (view names) args)
view names (Node' node ty) = N ty (bimap viewScoped (view names) node)
 where
  viewScoped (ScopedAST ann@(AnnBinder binders ty') body) =
    case (Foil.assertExt ann, Foil.assertDistinct ann) of
      (Foil.Ext, Foil.Distinct) -> Scope ty' (view (map Foil.sink names ++ Foil.namesOfPattern binders) body)

viewSubsts :: Substs -> [(Id, View)]
viewSubsts (MetaSubsts substs) = sortOn fst
  [(m, view (Foil.namesOfPattern binders) body)
  | MetaSubst (m, MetaAbs binders body) <- substs
  , Foil.Distinct <- [Foil.assertDistinct binders]]

constant :: Ty -> Id -> Term n
constant ty tag = Node' (Constant tag) ty

withNames
  :: Foil.Distinct n
  => Foil.Scope n -> [Int]
  -> (forall l. Foil.Distinct l => Foil.NameBinderList n l -> Foil.Scope l -> r)
  -> r
withNames scope [] k = k Foil.NameBinderListEmpty scope
withNames scope (i : rest) k =
  Foil.withRefreshed scope (Foil.UnsafeName i) $ \binder ->
    withNames (Foil.extendScope binder scope) rest $ \binders scope' ->
      k (Foil.NameBinderListCons binder binders) scope'

scoped
  :: Foil.Distinct n
  => Foil.Scope n -> [Int] -> [Ty]
  -> (forall l. (Foil.Distinct l, Foil.Ext n l) => Foil.Scope l -> [Term l] -> Term l)
  -> Scoped n
scoped scope ids types body = withNames scope ids $ \binders scope' ->
  case Foil.assertExt binders of
    Foil.Ext -> ScopedAST (AnnBinder binders (Binders types))
      (body scope' (map Var (Foil.namesOfPattern binders)))

scoped1
  :: Foil.Distinct n
  => Foil.Scope n -> Int -> Ty
  -> (forall l. (Foil.Distinct l, Foil.Ext n l) => Foil.Scope l -> Term l -> Term l)
  -> Scoped n
scoped1 scope i ty body = scoped scope [i] [ty] $ \scope' vars ->
  case vars of
    [x] -> body scope' x
    _ -> error "scoped1: expected one variable"

scoped2
  :: Foil.Distinct n
  => Foil.Scope n -> [Int] -> [Ty]
  -> (forall l. (Foil.Distinct l, Foil.Ext n l) => Foil.Scope l -> Term l -> Term l -> Term l)
  -> Scoped n
scoped2 scope ids types body = scoped scope ids types $ \scope' vars ->
  case vars of
    [x,y] -> body scope' x y
    _ -> error "scoped2: expected two variables"

check
  :: [(Id, ([Ty], Ty))] -> Term Foil.VoidS -> Term Foil.VoidS
  -> [[(Id, View)]] -> Expectation
check declarations lhs rhs expected = do
  let solutions = match Foil.emptyScope (Map.fromList declarations) Foil.emptyNameMap lhs rhs
  map viewSubsts solutions `shouldMatchList` expected
  forM_ solutions $ \solution ->
    alphaEquiv Foil.emptyScope (applyMetaSubsts Foil.emptyScope solution lhs) rhs `shouldBe` True

spec :: Spec
spec = do
  describe "generic SOAS matching" $ do
    it "decomposes equal nullary operators" $
      check [] (constant A 0) (constant A 0) [[]]
    it "imitates a nullary operator" $
      check [(0, ([], A))] (MetaApp 0 [] A) (constant A 0)
        [[(0, N A (Constant 0))]]
    it "rejects distinct nullary operators" $
      check [] (constant A 0) (constant A 1) []
    it "rejects a metavariable with the wrong result type" $
      check [(0, ([], A))] (MetaApp 0 [] A) (constant B 0) []
    it "rejects a result annotation inconsistent with the declaration" $
      check [(0, ([], B))] (MetaApp 0 [] A) (constant A 0) []
    it "rejects the wrong number of metavariable arguments" $
      check [(0, ([A], A))] (MetaApp 0 [] A) (constant A 0) []
    it "rejects an argument type inconsistent with the declaration" $
      check [(0, ([B], A))] (MetaApp 0 [constant A 0] A) (constant A 0) []
    it "does not project an argument of a different type" $
      check [(0, ([A], B)), (1, ([], A))]
        (MetaApp 0 [MetaApp 1 [] A] B) (constant B 0)
        [[(0, N B (Constant 0))]]
    it "uses the types of individual children during imitation" $
      check [(0, ([], Product A B))] (MetaApp 0 [] (Product A B))
        (Node' (Pair (constant A 0) (constant B 1)) (Product A B))
        [[(0, N (Product A B) (Pair (N A (Constant 0)) (N B (Constant 1))))]]
    it "checks repeated metavariables in separate children" $
      check [(0, ([], A))]
        (Node' (Pair (MetaApp 0 [] A) (MetaApp 0 [] A)) (Product A A))
        (Node' (Pair (constant A 0) (constant A 1)) (Product A A)) []
    it "rejects conflicting values in a nested occurrence" $ do
      let lhs = Node' (Bind (scoped Foil.emptyScope [0] [A] $ \_ vars ->
            MetaApp 0 [MetaApp 0 vars A] A)) (Arrow A A)
          rhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x ->
            Node' (Unary x) A)) (Arrow A A)
      check [(0, ([A], A))] lhs rhs []
    it "does not capture a variable missing from the arguments" $ do
      let lhs = Node' (Bind (scoped Foil.emptyScope [0] [A] $ \_ _ -> MetaApp 0 [] A)) (Arrow A A)
          rhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x -> x)) (Arrow A A)
      check [(0, ([], A))] lhs rhs []
    it "keeps both projections of a repeated argument" $ do
      let lhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x -> MetaApp 0 [x,x] A)) (Arrow A A)
          rhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x -> x)) (Arrow A A)
      check [(0, ([A,A], A))] lhs rhs [[(0,V 0)],[(0,V 1)]]
    it "leaves metavariables in discarded arguments unconstrained" $ do
      let lhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x -> MetaApp 0 [x,MetaApp 1 [x] A] A)) (Arrow A A)
          rhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x -> x)) (Arrow A A)
      check [(0, ([A,A], A)), (1, ([A], A))] lhs rhs
        [[(0,V 0)],[(0,V 1),(1,V 0)]]
    it "checks binder annotations even when the body ignores them" $ do
      let term ty = Node' (Bind (scoped Foil.emptyScope [0] [ty] $ \_ _ -> constant A 0)) A
      check [] (term A) (term B) []
    it "checks binder annotations when merging repeated assignments" $ do
      let term ty = Node' (Bind (scoped Foil.emptyScope [0] [ty] $ \_ _ -> constant A 0)) A
      check [(0, ([], A))]
        (Node' (Pair (MetaApp 0 [] A) (MetaApp 0 [] A)) (Product A A))
        (Node' (Pair (term A) (term B)) (Product A A)) []
    it "renames both sides of a multi-variable binding position" $ do
      let term ids = Node' (Bind (scoped2 Foil.emptyScope ids [A,B] $ \_ x y ->
            Node' (Pair x y) (Product A B))) (Product A B)
      check [] (term [0,3]) (term [1,2]) [[]]
    it "preserves variable types when both sides are renamed" $ do
      let lhs = Node' (Bind (scoped2 Foil.emptyScope [0,3] [A,B] $ \_ x y ->
            MetaApp 0 [x,y] B)) B
          rhs = Node' (Bind (scoped2 Foil.emptyScope [1,2] [A,B] $ \_ _ y -> y)) B
      check [(0, ([A,B], B))] lhs rhs [[(0,V 1)]]
    it "imitates a mixed operator and preserves parameter order under its binder" $ do
      let resultTy = Product A B
          lhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \_ x ->
            MetaApp 0 [x] resultTy)) resultTy
          rhs = Node' (Bind (scoped1 Foil.emptyScope 0 A $ \scope x ->
            Node' (Let x (scoped1 scope 1 B $ \_ y ->
              Node' (Pair (Foil.sink x) y) resultTy)) resultTy)) resultTy
          body = N resultTy (Let (V 0) (Scope (Binders [B])
            (N resultTy (Pair (V 0) (V 1)))))
      check [(0, ([A], resultTy))] lhs rhs [[(0,body)]]
