module Api.Paths where
import Web.PathPieces

newtype Natural = Natural Integer deriving (Eq, Show)

instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i 
    fromPathPiece s = fromPathPiece s >>= \i -> (if i < 1 then Nothing else Just $ Natural i)

