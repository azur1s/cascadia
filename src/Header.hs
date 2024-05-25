module Header where
import Text.Parsec (ParseError)

data ErrKind
    = ParseError ParseError
    | InferError String

fmtErrKind :: ErrKind -> String
fmtErrKind (ParseError e) = show e
fmtErrKind (InferError e) = e

data Result a
    = Yay a
    | Err ErrKind

instance Functor Result where
    fmap f (Yay a) = Yay (f a)
    fmap _ (Err e) = Err e

instance Applicative Result where
    pure = Yay
    Yay f <*> Yay a = Yay (f a)
    Err e <*> _ = Err e
    _ <*> Err e = Err e

instance Monad Result where
    Yay a >>= f = f a
    Err e >>= _ = Err e
