module Text.JSON.Type (JValue(..)) where

data JValue = JObject [(JValue, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Double
            | JBool Bool
            | JNull
            deriving (Eq, Show)
