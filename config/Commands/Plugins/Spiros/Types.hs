{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commands.Plugins.Spiros.Types where


-- | an ordinal number. i.e. "first", "second", "third", "fourth", et cetera  
newtype Ordinal = Ordinal { unOrdinal :: Integer }  -- NOTE a GeneralizedNewtypeDeriving Show, doesn't show field accessories in constructor
 deriving (Show,Read,Eq,Ord,Enum,Num,Integral,Real)

