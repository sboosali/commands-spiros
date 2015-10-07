{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, LambdaCase, TypeFamilies, ViewPatterns, FlexibleContexts  #-}
module Commands.Plugins.Spiros.Template where

import           Commands.Backends.OSX
import Commands.Sugar.Keys

import           Text.InterpolatedString.Perl6 (qc) 

import qualified Data.List as List
import Control.Monad (replicateM_) 
import           GHC.Exts                          (IsString (..), IsList (..))


{- | a simple monoid that can hold the metadata of a cursor position. you can then can set the position of the cursor after inserting a template. 

can be used with @-XQuasiQuotes@:

@
'haddockTemplate' = ['qc'|
\{-| {'cursor'}

-} 
|]
@

for interpolation, just use 'qc':

@
'haddockTemplateWith' text = ['qc'|
\{-| {text}{'cursor'}

-} 
|]
@


-}
data Template
 = TemplateCursor 
 | TemplateText String 
 | TemplateList [Template] 
 deriving (Show,Eq,Ord)

{- | when constructed with 'mappend', a @Template@ is always flat (see 'flattenTemplate'). 

-} 
instance Monoid Template where
 mempty = TemplateList [] 
 mappend x y = TemplateList (mappend (toTemplateList x) (toTemplateList y))

instance IsString Template where
 fromString = TemplateText

instance IsList Template where
 type Item Template = Template 
 fromList = fromTemplateList
 toList = toTemplateList

toTemplateList :: Template -> [Template]
toTemplateList = \case
 TemplateList ts -> ts 
 t -> [t]

fromTemplateList :: [Template] -> Template
fromTemplateList = TemplateList 

toTemplateText :: Template -> String
toTemplateText = \case
 TemplateCursor  -> "" 
 TemplateText s  -> s 
 TemplateList ts -> concatMap toTemplateText ts



-- ================================================================ --

{- | expects zero or one 'TemplateCursor'(s). 

strips one leading newline and one trailing newline, 
which increases the readability of quasiquotes. 

-}
insertTemplate :: MonadWorkflow m => Template -> m ()                        
insertTemplate template = do
 let (before, after) = mungeTemplate template 
 insert before 
 insert after 
 replicateM_ (length after) (press "<left>") 

{-| munges the template, making it 'insert'able. 

strips one leading newline and one trailing newline. 

increases readability of quasi-quotes, e.g.:

@
haddockTemplate = [qc|
\{-| {cursor}

-} 
|]
@

rather than: 

@
haddockTemplate = [qc|\{-| {cursor}

-}|]
@

if you want that leading/trailing white space, just add "extra" whitespace, or use a string literal.

-} 
mungeTemplate :: Template -> (String, String)
mungeTemplate template = (before, after)
 where
 before = (lstrip . toTemplateText) beforeTemplate
 after  = (rstrip . toTemplateText) afterTemplate
 (beforeTemplate, afterTemplate) = splitTemplateByCursor template 
 rstrip = reverse.lstrip.reverse -- TODO
 lstrip = \case 
  ('\n':xs) -> xs
  xs -> xs

{- | input should have zero or one 'TemplateCursor'(s) (splits on the first, when multiple). 

outputs should have zero 'TemplateCursor'(s).

-}
splitTemplateByCursor :: Template -> (Template, Template)
splitTemplateByCursor
 = bothmap fromTemplateList
 . bothmap (filter (/=TemplateCursor))
 . splitOn TemplateCursor
 . toTemplateList
 . flattenTemplate
 where 
 bothmap f (x,y) = (f x, f y)
 splitOn :: Eq a => a -> [a] -> ([a], [a]) 
 splitOn x xs = (List.break (==x) xs) 

{-| outputs a "flat" 'Template'. i.e. the @ts@ in a @'TemplateList' ts@ are only either @'TemplateCursor'@ or @'TemplateText' s@. thus, @'TemplateList' [Template]@ could be @'TemplateList' (Maybe String)@.  

(when a 'Template' has been constructed with constructors, rather than 'mappend',  
and you can 'flattenTemplate' to make sure this holds.)   

-}
flattenTemplate :: Template -> Template
flattenTemplate = \case
 TemplateList ts -> TemplateList (map flattenTemplate ts) 
 t -> t

-- | @cursor = 'TemplateCursor'@
cursor :: Template
cursor = TemplateCursor




-- ================================================================ --

haddockTemplate :: Template 
haddockTemplate = [qc|
\{-| {cursor}

-}

|]

