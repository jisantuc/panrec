module TypescriptInterfaceSpec (typescriptInterfaceSpec) where

import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (append, pack)
import           Data.List                        (intersperse)
import           Data.Primitive
import           Data.Record
import           Data.RecordIO
import qualified Data.TypescriptInterface         as TS
import           Test.Hspec
import           Text.RawString.QQ

import Debug.Trace

typescriptInterfaceSpec :: Spec
typescriptInterfaceSpec = do
  describe "Typescript interface parsers" $ do
    it "should parse an interface correctly" $ do
      TS.parseInterface exampleInterface `shouldBe`
        (Right $
         TS.Interface [ ("id", String')
                      , ("name", String')
                      , ("friends", VendorHK "ReadonlyArray" [ Vendor "Friend" ])
                      , ("enemies", VendorHK "ReadonlyArray" [ Vendor "relationships.Enemy" ]) ]
          "Character")
    it "should parse and encode and parse an interface correctly" $ do
      do
        parsed <- parseRecords (emptyR :: TS.Interface) exampleInterfaceWithKW
        let encoded = constructor <$> parsed
        let concatted = (++ "\n") $ mconcat $ intersperse "\n" encoded
        trace concatted $ parseRecords (emptyR :: TS.Interface) (pack concatted)
      `shouldBe`
        ( Right $ [ TS.Interface [ ("id", String')
                                 , ("name", String')
                                 , ("friends", VendorHK "ReadonlyArray" [ Vendor "Friend" ])
                                 , ("enemies", VendorHK "ReadonlyArray" [ Vendor "relationships.Enemy" ]) ]
                    "Character" ] )


exampleInterface :: ByteString
exampleInterface = [r|Character {
  readonly id: string;
  readonly name: string;
  readonly friends: ReadonlyArray<Friend>;
  readonly enemies: ReadonlyArray<relationships.Enemy>;
}
|]

exampleInterfaceWithKW :: ByteString
exampleInterfaceWithKW = "interface " `append` exampleInterface
