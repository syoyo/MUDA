-------------------------------------------------------------------------------
---- |
---- Module      :  Language.MUDA.AST
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lighttransport.com
---- Stability   :  experimental
---- Portability :  GHC 6.10
----
---- AST         :  Abstract Syntax Tree.
----
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.MUDA.AST where

import Data.Generics                -- syb

-- | MUDA types.
data Type
 = TyUnknown
 | TyVoid
 | TyInt32
 | TyFloat32
 | TyInt64                -- Long
 | TyFloat64              -- Double
 | TyVector Type Int
 deriving (Show, Eq, Typeable, Data)


-- | MUDA expressions.
data Expr
 = VarDef Type String     -- variable definition.
 deriving (Show, Eq, Typeable, Data)


-- | MUDA symbols.
data Symbol
 = Symbol String
 deriving (Show, Eq, Typeable, Data)

type SymbolTable
 = [(String, [Symbol])]
  
data Func
 = Func String [Expr]
 deriving (Show, Eq, Typeable, Data)

type MUDAUnit = [Func]
