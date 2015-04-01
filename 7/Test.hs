import System.Environment
import Control.Monad
import Scrabble
import Prelude
import Buffer
import JoinListBuffer
import Sized

main = do
    f   <- readFile "carol.txt"
    return ((fromString f) :: JoinList (Score, Size) String)

carolJoinList :: IO (JoinList (Score, Size) String)
carolJoinList = do
    f  <- readFile "carol.txt"
    return ((fromString f) :: JoinList (Score, Size) String)