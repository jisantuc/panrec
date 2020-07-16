module Data.Primitive
  ( Primitive(..)
  , printer
  , tsPrinter
  )
where

import           Data.List                      ( intersperse )

data Primitive
  = Int'
  | Double'
  | Float'
  | Char'
  | String'
  | Boolean'
  | Option' Primitive
  | Either' Primitive Primitive
  | IO' Primitive
  | List' Primitive
  | VendorHK String [Primitive]
  | Vendor String
  | Any
  deriving (Eq)

tsPrinter :: Primitive -> String
tsPrinter prim = case prim of
  Int'        -> "number"
  Double'     -> "number"
  Float'      -> "number"
  Char'       -> "string"
  String'     -> "string"
  Boolean'    -> "boolean"
  Any         -> "any"
  Option' p   -> "Option<" ++ tsPrinter p ++ ">"
  Either' e a -> "Either<" ++ tsPrinter e ++ ", " ++ tsPrinter a ++ ">"
  IO'    p    -> "Promise<" ++ tsPrinter p ++ ">"
  List'  p    -> "Array<" ++ tsPrinter p ++ ">"
  Vendor s    -> s
  VendorHK nm prims ->
    nm ++ "<" ++ mconcat (intersperse ", " $ tsPrinter <$> prims) ++ ">"

printer :: String -> String -> Primitive -> String
printer _     _   Int'        = "Int"
printer _     _   Double'     = "Double"
printer _     _   Float'      = "Float"
printer _     _   Char'       = "Char"
printer _     _   String'     = "String"
printer _     _   Boolean'    = "Boolean"
printer _     _   Any         = "Any"
printer start end (Option' a) = "Option" ++ start ++ printer start end a ++ end
printer start end (Either' e a) =
  "Either" ++ start ++ printer start end e ++ ", " ++ printer start end a ++ end
printer start end (IO'   a) = "IO" ++ start ++ printer start end a ++ end
printer start end (List' a) = "List" ++ start ++ printer start end a ++ end
printer start end (VendorHK name prims) =
  name
    ++ start
    ++ mconcat (intersperse ", " $ printer start end <$> prims)
    ++ end
printer _ _ (Vendor s) = s

{-| Provide a show instance based on Scala's type parameter syntax -}
instance Show Primitive where
  show = printer "[" "]"
