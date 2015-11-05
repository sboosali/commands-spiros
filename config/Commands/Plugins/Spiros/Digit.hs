{-# LANGUAGE LambdaCase #-}
module Commands.Plugins.Spiros.Digit where 


newtype Digit = Digit Int       -- TODO modular arithmetic: type Digit = Natural `Mod` 10
 deriving (Show,Read,Eq,Ord)

instance Bounded Digit where 
 minBound = Digit 0
 maxBound = Digit 9

instance Enum Digit where 
 toEnum i
  | minBound <= i && i <= maxBound = Digit i 
  | otherwise                      = error ("Digit.toEnum: " ++ show i ++ " is not a single digit integer")
 fromEnum (Digit i) = i

parseDigit :: String -> Maybe Digit 
parseDigit = \case 
 "1" -> Just $ Digit 1
 "2" -> Just $ Digit 2
 "3" -> Just $ Digit 3
 "4" -> Just $ Digit 4
 "5" -> Just $ Digit 5
 "6" -> Just $ Digit 6
 "7" -> Just $ Digit 7
 "8" -> Just $ Digit 8
 "9" -> Just $ Digit 9
 _   -> Nothing 

-- TODO use digits everywhere, ordinal grammars and cardinal garments  

