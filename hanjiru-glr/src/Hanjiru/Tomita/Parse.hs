module Hanjiru.Tomita.Parse where

import Hanjiru.Token

import Prettyprinter

-- | A 'Parse' is the interpretation(s) of an input according to the grammar.
data Parse a =
    -- | A literal token taken directly from the input.
      Literal       Token
    
    -- | A non-terminal symbol that comes from the application of a production rule.
    | Production    Token
                    Int
                    [Parse a]
    
    -- | A symbol with multiple ambiguous interpretations.
    | Ambiguity     Token
                    [Parse a]
    deriving Eq

symbol :: Parse a -> Token
symbol x =
    case x of
        Literal    tok      -> tok
        Production tok _ _  -> tok
        Ambiguity  tok _    -> tok

ambiguity :: Parse a -> Parse a -> Parse a
ambiguity x y =
    case y of
        Literal tok ->
            Ambiguity tok [x, y]
        Production tok _ _  ->
            Ambiguity tok [x, y]
        Ambiguity tok ys ->
            Ambiguity tok (x:ys)

instance Show (Parse a) where
    show = show . prettyParse

prettyParse :: Parse a -> Doc ()
prettyParse x =
    case x of
        Literal tok -> viaShow tok
        Production tok _ args ->
            viaShow tok <> line <> indent 4 (vsep $ map prettyParse args)
        Ambiguity tok args ->
            viaShow tok <+> enclose "[" "]"
                (line <> indent 4 (vsep $ map prettyParse args) <> line)