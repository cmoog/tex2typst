{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.HashMap.Strict (HashMap, fromList, lookupDefault)
import Data.Text as T
import Data.Text.IO as T
import System.IO
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Types

main :: IO ()
main = do
  tex <- T.getContents
  -- mapM_ print $ readTeX tex -- debug TeX expression
  case convert tex of
    Right a -> T.putStrLn a
    Left a -> T.hPutStrLn stderr $ "error: " <> a

convert :: Text -> Either Text Text
convert rawTex = do
  exprs <- readTeX rawTex
  return . T.unwords $ render <$> exprs

render :: Exp -> Text
render (EDelimited left right inner) = left <> T.unwords (delimited <$> inner) <> right
render (EFraction _ a b) = func "frac" [a, b]
render (EGrouped exprs) = parens . T.unwords $ render <$> exprs -- (e.g. {...} ) in TeX, <mrow>...</mrow> in MathML.
render (ENumber a) = a
render (EOver _ a b) = render a <> "^" <> render b
render (EPhantom a) = render a
render (ERoot a b) = func "root" [a, b]
render (ESqrt a) = func "sqrt" [a]
render (ESub base script) = render base <> "_" <> render script
render (ESubsup base sub sup) = parens (render base) <> "_" <> parens (render sub) <> "^" <> parens (render sup)
render (ESuper base script) = render base <> "^" <> render script
render (EUnder _ a b) = render a <> "_" <> render b
render (EBoxed a) = "#block(stroke: 1pt, inset: 5pt)[$" <> render a <> "$]" -- TODO: verify
render (EScaled factor expr) = "#scale(x: " <> showScaleFactor factor <> ", y: " <> showScaleFactor factor <> ")[$" <> render expr <> "$]"
render (ESymbol _ a) = a
render (EUnderover _ base under over) = render base <> "_" <> render under <> "^" <> render over
render (EMathOperator op) =
  if op `Prelude.elem` builtinOps
    then op
    else "op" <> parens (quoted op)
render (EIdentifier a) = lookupDefault a a builtinIdents
render (EStyled _ exprs) = T.unwords $ render <$> exprs -- TODO: style
render (ESpace _) = "space" -- TODO: variable width
render (EText _ a) = quoted a -- TODO: styles
render (EArray {}) = error "todo array"

-- TODO: generate from reference
builtinIdents :: HashMap Text Text
builtinIdents =
  fromList
    [ ("\2211", "sum"),
      ("\960", "pi"),
      ("\969", "omega"),
      ("\945", "alpha"),
      ("\946", "beta")
    ]

-- | builtin math operation identifiers: https://typst.app/docs/reference/math/op/
builtinOps :: [Text]
builtinOps =
  [ "arccos",
    "arcsin",
    "arctan",
    "arg",
    "cos",
    "cosh",
    "cot",
    "coth",
    "csc",
    "csch",
    "ctg",
    "deg",
    "det",
    "dim",
    "exp",
    "gcd",
    "hom",
    "id",
    "im",
    "inf",
    "ker",
    "lg",
    "lim",
    "liminf",
    "limsup",
    "ln",
    "log",
    "max",
    "min",
    "mod",
    "Pr",
    "sec",
    "sech",
    "sin",
    "sinc",
    "sinh",
    "sup",
    "tan",
    "tanh",
    "tg",
    "tr"
  ]

-- | render a scale factor as a percentage
showScaleFactor :: Rational -> Text
showScaleFactor a = (T.pack . show $ (fromRational a :: Double) * 100) <> "%"

delimited :: InEDelimited -> Text
delimited (Left _) = "|" -- (this is the case of the TeX expression `\mid`)
delimited (Right a) = render a

parens :: Text -> Text
parens a = "(" <> a <> ")"

quoted :: Text -> Text
quoted a = "\"" <> a <> "\""

args :: [Exp] -> Text
args = intercalate ", " . fmap render

func :: Text -> [Exp] -> Text
func name exprs = name <> parens (args exprs)
