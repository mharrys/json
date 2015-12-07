import Numeric (showHex)

import Test.Hspec
import Test.QuickCheck

import Text.JSON

main :: IO ()
main = hspec $ do
    describe "Null" $
        it "should parse null" $
            cmp "null" JNull

    describe "Bool" $ do
        it "should parse true" $
            cmp "true" (JBool True)
        it "should parse false" $
            cmp "false" (JBool False)

    describe "Number" $ do
        it "should parse numbers" $
            property $ \x -> cmp (show x) (JNumber (x :: Double))
        it "should parse zero" $ do
            cmp "0" (JNumber 0.0)
            cmp "0.0" (JNumber 0.0)
            cmp "0.0e0" (JNumber 0.0)
            cmp "0.0E0" (JNumber 0.0)
            cmp "-0" (JNumber 0.0)
        it "should not parse repeated leading zeros" $ do
            nok "00"
            nok "-00"
        it "should parse exponent" $ do
            cmp "10E2" (JNumber 1000)
            cmp "10.0E2" (JNumber 1000)
            cmp "0.0001e10" (JNumber 1000000)
            cmp "1234e-4" (JNumber 0.1234)

    describe "String" $ do
        it "should parse safe text" $
            property $ forAll genSafeString $
                \x -> cmp ("\"" ++ x ++ "\"") (JString x)
        it "should parse safe unicode" $
            property $ forAll genUnicodeString $
                \x -> case fromJSON ("\"\\u" ++ x ++ "\"") of
                        Right (JString a) -> toHex a `shouldBe` x
                        a -> fail $ show a
        it "should parse empty text" $
            cmp "\"\"" (JString "")
        it "should parse allowed escaped characters in text" $ do
            -- note that this syntax is not needed when reading from file
            cmp "\"\\\\/\b\f\n\r\t\"" (JString "\\/\b\f\n\r\t")
            nok "\"\b\\a\\r\""
        it "should parse unicode" $ do
            nok "\"\\u0\""
            nok "\"\\u00\""
            nok "\"\\u000\""
            cmp "\"\\u0000\"" (JString "\NUL")
            cmp "\"\\uffff\"" (JString "\65535")

    describe "Array" $ do
        it "should parse empty array" $ do
            cmp "[]" (JArray [])
            cmp "[[[]]]" (JArray [JArray [JArray []]])
            nok "["
            nok "[["
            nok "]"
            nok "]]"
            nok "[true"
        it "should parse non-empty array" $ do
            cmp "[null]" (JArray [JNull])
            cmp "[null, true]" (JArray [JNull, JBool True])
            cmp "[null, [true, \"foobar\"], false]"
                (JArray
                    [JNull, JArray
                        [JBool True, JString "foobar"], JBool False])

    describe "Object" $ do
        it "should parse empty object" $ do
            cmp "{}" (JObject [])
            nok "{{}}"
            nok "{"
            nok "}"
            nok "{{"
            nok "}}"
        it "should parse non-empty objects" $ do
            cmp "{\"foo\": null}" (JObject [(JString "foo", JNull)])
            cmp "{\"foo\": {\"bar\": [null, true]}}"
                (JObject
                    [(JString "foo",
                        JObject
                            [(JString "bar",
                                JArray [JNull, JBool True])])])
            cmp "{\"foo\": null, \"bar\": null}"
                (JObject
                    [(JString "foo", JNull),
                     (JString "bar", JNull)])

-- From unicode hex storage to hex characters.
toHex :: String -> String
toHex x = take (4 - length hex) ['0', '0'..] ++ hex -- prepend leading zeros, better way?
  where
    hex = showHex ((fromEnum . head) x) ""

-- Generate unescaped character.
genSafeChar :: Gen Char
genSafeChar = suchThat (arbitrary :: Gen Char) (`notElem` "\"\\")

-- Generate string without escaped characters.
genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString String

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

-- Generate unicode character.
genUnicodeChar :: Gen Char
genUnicodeChar = elements $ ['0'..'9'] ++ ['a'..'f']

-- Generate unicode string.
genUnicodeString :: Gen String
genUnicodeString = vectorOf 4 genUnicodeChar

newtype UnicodeString = UnicodeString String

instance Arbitrary UnicodeString where
    arbitrary = UnicodeString <$> genUnicodeString

-- | Validate expression.
cmp :: String -> JValue -> Expectation
cmp s r = case fromJSON s of
    Left  x -> fail $ show x
    Right x -> x `shouldBe` r

-- | Invalid expression.
nok :: String -> Expectation
nok s = cmp s JNull `shouldThrow` anyException
