module Hanjiru.Tomita.Parse where

import Prettyprinter

-- | A 'Parse' is the interpretation(s) of an input according to the grammar.

data Parse token a =
    -- | A literal token taken directly from the input.
      Literal       token
    
    -- | A non-terminal symbol that comes from the application of a production rule.
    | Production    token
                    Int
                    [Parse token a]
    
    -- | A symbol with multiple ambiguous interpretations.
    | Ambiguity     token
                    [Parse token a]
    deriving (Eq, Ord)

symbol :: Parse token a -> token
symbol x =
    case x of
        Literal    tok      -> tok
        Production tok _ _  -> tok
        Ambiguity  tok   _  -> tok

ambiguity :: Parse token a -> Parse token a -> Parse token a
ambiguity x y =
    case y of
        Literal    tok      -> Ambiguity tok [x, y]
        Production tok _ _  -> Ambiguity tok [x, y]
        Ambiguity  tok   ys -> Ambiguity tok (x:ys)

instance Pretty token => Pretty (Parse token a) where
    pretty = prettyParse

instance Pretty token => Show (Parse token a) where
    show = show . prettyParse

prettyParse :: Pretty token => Parse token a -> Doc ann
prettyParse x =
    case x of
        Literal    tok      -> pretty tok
        Production tok _ xs -> prettyProduction tok xs
        Ambiguity  tok   xs -> prettyAmbiguity  tok xs
    where
        prettyProduction tok xs =
            pretty tok <> line <> (indent 4 $ vsep $ map prettyParse xs)
    
        prettyAmbiguity tok xs =
            pretty tok <+> lbracket <> line
                <> (indent 4 $ vsep $ map prettyParse xs)
                <> rbracket <> line