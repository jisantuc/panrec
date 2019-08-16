module Data.Primitive (Primitive (..)) where

data Primitive =
  Int'
  | Double'
  | Float'
  | Char'
  | String'
  | Boolean'
  | Option' Primitive
  | Either' Primitive Primitive
  | IO' Primitive
  | List' Primitive
  | Vendor String
  | Any deriving (Eq)

instance Show Primitive where
  show Int'          = "Int"
  show Double'       = "Double"
  show Float'        = "Float"
  show Char'         = "Char"
  show String'       = "String"
  show Boolean'      = "Boolean"
  show Any           = "Any"
  show (Option' a)   = "Option[" ++ show a ++ "]"
  show (Either' e a) = "Either[" ++ show e ++ ", " ++ show a ++ "]"
  show (IO' a)       = "IO[" ++ show a ++ "]"
  show (List' a)     = "List[" ++ show a ++ "]"
  show (Vendor s)    = s
