-------------------------------------------------------------------------------
---- |
---- Module      :  Parser
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lighttransport.com
---- Stability   :  experimental
---- Portability :  GHC 6.10
----
---- Parser      :  Parser for MUDA language.
----
-------------------------------------------------------------------------------

module Parser where

import AST

import Text.ParserCombinators.Parsec
