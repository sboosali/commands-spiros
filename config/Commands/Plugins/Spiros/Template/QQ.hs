{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Commands.Plugins.Spiros.Template.QQ where 
import           Commands.Plugins.Spiros.Template.Types 

import           Text.InterpolatedString.Perl6 (qc) 


{-| 

@
'haddockTemplate' text = ['qc'|
\{-| {text} {'cursor'}

-} 
|]
@

-}
haddockTemplate :: String -> Template 
haddockTemplate "" = [qc|
\{-| {cursor}

-}

|]
haddockTemplate text = [qc|
\{-| {text} {cursor}

-}

|]


grammarTemplate :: String -> Template 
grammarTemplate text = [qc|
{text} :: R z _
{text} = '{text} <=> empty
 <|> {cursor}

|]


