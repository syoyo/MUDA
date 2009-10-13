-------------------------------------------------------------------------------
---- |
---- Module      :  Language.MUDA.PPrint
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lighttransport.com
---- Stability   :  experimental
---- Portability :  GHC 6.10
----
---- PPrint      :  Pretty Printer for MUDA AST.
----
-------------------------------------------------------------------------------

module Language.MUDA.PPrint where

import Text.PrettyPrint.HughesPJ

-- Import local modules
import Language.MUDA.AST


-- | Pretty printer class
class Pretty p where
  pretty  :: p -> Doc

instance Pretty Func where
  pretty (Func name stms) = text name

instance Pretty MUDAUnit where
  pretty (MUDAUnit funcs) = vcat (map pretty funcs)
