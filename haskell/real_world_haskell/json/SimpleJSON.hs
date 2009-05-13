{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module SimpleJSON
    (
        JValue(..),
        getString,
        getInt,
        getDouble,
        getBool,
        getObject,
        getArray,
        isNull
    ) where

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)
    
newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just n
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v = v == JNull

type JSONError = String
class JSON a where
    toJValue   :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue   = id
    fromJValue = Right

instance JSON Bool where
    toJValue             = JBool
    fromJValue (JBool b) = Right b
    fromJValue _         = Left "not a JSON boolean"
    
instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON number"

instance JSON Int where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round
    
instance JSON Integer where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue   = JNumber
    fromJValue = doubleToJValue id

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JAry a) where
    toJValue      = jaryToJValue
    fromJValue    = jaryFromJValue
    
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray
