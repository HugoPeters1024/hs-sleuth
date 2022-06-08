
import Data.List.Stream
import Prelude hiding (map,sum,head)
import System.Environment

main = do
    n <- getArgs >>= readIO . head
    print (sum (map (+1) [1..(n::Int)])) -- 1 fusion site.
    print (sum (map (+1) [1..(10::Int)])) -- 2 fusion site.
