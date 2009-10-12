-------------------------------------------------------------------------------
---- |
---- Module      :  Language.MUDA.Parser
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

module Language.MUDA.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error

import Control.Monad.State
import Debug.Trace

-- Import local modules
import Language.MUDA.AST

-- | MUDA parser state
data MUDAState = MUDAState  { symbolTable :: SymbolTable
                            , n           :: Int
                            }



-- | MUDA parser having MUDA parser state
type MUDAParser a = GenParser Char MUDAState a

-- | Returns initial state of MUDA parser env.
--   Builtin variables are added in global scope.
initMUDAState :: MUDAState
initMUDAState = MUDAState {
    symbolTable = [("global", [])]
  , n           = 0
  }


program             = do  { ast <- many (spaces >> global)
                          ; return ast
                          }

global              =     functionDefinition

varDef              = do  { ty <- primType
                          ; name <- identifier
                          ; semi
                          ; return (VarDef ty name)
                          }

statement :: MUDAParser [Expr]
statement           = do  { vdef <- varDef 
                          ; return [vdef]
                          }
                          


statements          = do  { stms <- many statement      -- [[]]
                          ; return $ concat stms        -- []
                          }

functionDefinition  = do  { reserved "__kernel"
                          ; reserved "void" 
                          ; name <- identifier 
                          ; symbol "("
                          ; symbol ")"
                          ; symbol "{"
                          ; stms <- statements
                          ; symbol "}"
                          ; return (Func name stms)
                          }

-- | Parse primitive types.
primType  =   (reserved "float16"   >> return (TyVector TyFloat32 16))
          <|> (reserved "float8"    >> return (TyVector TyFloat32 8))
          <|> (reserved "float4"    >> return (TyVector TyFloat32 4))
          <|> (reserved "float2"    >> return (TyVector TyFloat32 2))
          <|> (reserved "float"     >> return (TyFloat32))
          <|> (reserved "double16"  >> return (TyVector TyFloat64 16))
          <|> (reserved "double8"   >> return (TyVector TyFloat64 8))
          <|> (reserved "double4"   >> return (TyVector TyFloat64 4))
          <|> (reserved "double2"   >> return (TyVector TyFloat64 2))
          <|> (reserved "int"       >> return (TyInt32))
          <?> "MUDA types"


-- -------------------------------------------------------------------------
--
-- Pareser interface
--
-- -------------------------------------------------------------------------

-- | Parse MUDA source code.
parseMUDAFromFile :: MUDAParser a -> SourceName -> IO (Either ParseError a)
parseMUDAFromFile p fname =
  do { input <- readFile fname
     ; return (runParser p initMUDAState fname input)
     }

--
-- Almost same as in Error.hs of Parsec, just replace filename to show.
--
showErrorMsg :: ParseError -> FilePath -> String
showErrorMsg err fname =
  show (setSourceName (errorPos err) fname) ++ ":" ++
  showErrorMessages "or" "unknown parse error"
                    "expecting" "unexpected" "end of input"
                   (errorMessages err)

mkMyError :: ParseError -> FilePath -> ParseError
mkMyError err fname = setErrorPos (setSourceName (errorPos err) fname) err

offt :: Int -> String
offt n = replicate n ' '

showLine :: SourceName -> Int -> Int -> IO ()
showLine name n m =
  do  input <- readFile name

      if (length (lines input)) < n

        then

          if length (lines input) == 0

            then putStrLn ""

          else

            do  { putStrLn $ (lines input) !! ((length (lines input)) - 1)
                ; putStrLn ""
                ; putStrLn $ ((offt (m-1)) ++ "^")
                }

        else

          do  { let l = (lines input) !! (n-1)
              ; putStrLn l
              ; putStrLn $ ((offt (m-1)) ++ "^")
              }


run :: MUDAParser MUDAUnit -> FilePath -> FilePath -> (MUDAUnit -> IO ()) -> IO ()
run p prepname name proc =
  do  { result <- parseMUDAFromFile p prepname
      ; case (result) of
          Left err -> do  { -- Parse preprocessed file, but print original file
                            -- when reporting error.
                            putStrLn "Parse err:"
                          ; showLine name (sourceLine (errorPos err)) (sourceColumn (errorPos err))
                          ; print (mkMyError err name)
                          }
          Right x  -> do  { proc $ x
                          }
      }


runLex :: MUDAParser MUDAUnit -> FilePath -> FilePath -> (MUDAUnit -> IO ()) -> IO ()
runLex p prepname name proc =
  run (do { whiteSpace
          ; x <- p
          ; eof
          ; return x
          }
      ) prepname name proc

--
-- Useful parsing tools
--
lexer           = P.makeTokenParser mudaStyle

whiteSpace      = P.whiteSpace lexer
lexeme          = P.lexeme lexer
symbol          = P.symbol lexer
natural         = P.natural lexer
naturalOrFloat  = P.naturalOrFloat lexer
stringLiteral   = P.stringLiteral lexer
float           = P.float lexer
parens          = P.parens lexer
braces          = P.braces lexer
semi            = P.semi lexer
commaSep        = P.commaSep lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer


mudaStyle = javaStyle
  { reservedNames = [ "const"
                    , "break", "continue"
                    , "while", "if", "for"
                    , "extern"
                    , "__kernel"
                    -- More is TODO
                    ]
  , reservedOpNames = ["+", "-", "*", "/"] -- More is TODO
  , caseSensitive   = True
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  }
