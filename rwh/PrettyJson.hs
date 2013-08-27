module PrettyJson where

import Json (JValue(..))
import Prettify (Doc, string, double, text, series, (<>))

renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n 
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JArray a)    = series '[' ']' renderJValue a
renderJValue (JObject o)   = series '{' '}' field o
   where field (name, val) = string name <> text ": " <> renderJValue val

{-putJValue :: JValue -> IO ()-}
{-putJValue v = putStrLn (renderJValue v)-}




