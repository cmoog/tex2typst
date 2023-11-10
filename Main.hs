{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

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
  let converted = render <$> exprs
  return $ T.unwords converted

render :: Exp -> Text
render (EDelimited left right inner) = left <> T.unwords (delimited <$> inner) <> right
render (EFraction _ a b) = func "frac" [a, b]
render (EGrouped exprs) = parens . T.unwords $ render <$> exprs -- (e.g. {...} ) in TeX, <mrow>...</mrow> in MathML.
render (EMathOperator op) = "op" <> parens (quoted op)
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
render (EIdentifier a) = a -- TODO: map UTF to typst identifiers
render (EStyled _ exprs) = T.unwords $ render <$> exprs -- TODO: style
render (EText _ a) = quoted a -- TODO: styles
render (ESpace _) = " space " -- TODO: variable width
render (EArray {}) = error "todo array"
render (EUnderover {}) = error "todo underover"

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
