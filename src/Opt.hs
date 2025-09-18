module Opt (Option (..), options) where

import System.Console.GetOpt

data Option = Typing String | Subtyping String | Variant String | Meta | Grammar
  deriving (Eq, Show)

options :: [OptDescr Option]
options =
  [ Option [] ["typing"] (ReqArg Typing "TYPING_ALG_NAME") "TYPING_ALG_NAME",
    Option [] ["subtyping"] (ReqArg Subtyping "SUBTYPING_MODE") "SUBTYPING_MODE (e.g., 'Revisiting')",
    Option [] ["variant"] (ReqArg Variant "VARIANT_NAME") "VARIANT_NAME (e.g., 'nominal', 'recursive')",
    Option [] ["meta"] (NoArg Meta) "Output algorithm metadata as JSON",
    Option [] ["grammar"] (NoArg Grammar) "Output TextMate grammar for syntax highlighting"
  ]
