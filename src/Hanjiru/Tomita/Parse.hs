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
        Ambiguity  tok   _  -> tok

ambiguity :: Parse a -> Parse a -> Parse a
ambiguity x y =
    case y of
        Literal    tok      -> Ambiguity tok [x, y]
        Production tok _ _  -> Ambiguity tok [x, y]
        Ambiguity  tok   ys -> Ambiguity tok (x:ys)

instance Pretty (Parse a) where
    pretty = prettyParse

instance Show (Parse a) where
    show = show . prettyParse

prettyParse :: Parse a -> Doc ann
prettyParse x =
    case x of
        Literal    tok      -> viaShow tok
        Production tok _ xs -> prettyProduction tok xs
        Ambiguity  tok   xs -> prettyAmbiguity  tok xs
    where
    prettyProduction tok xs =
        viaShow tok <> line
            <> (indent 4 $ vsep $ map prettyParse xs)
    
    prettyAmbiguity tok xs =
        viaShow tok <+> lbracket <> line
            <> (indent 4 $ vsep $ map prettyParse xs)
            <> rbracket <> line