import Control.Monad.Trans.List
import Control.Monad.State
import Control.Monad.Trans.Writer

logSums :: [Int] -> ListT (WriterT [String] (State Int)) Int
logSums nums = do
  n <- init nums
  lift $ tell ["I am number " ++ (show n)]
  guard (n `mod` 2 == 0)
  x <- get 
  put (x+n)
  return n
  where init l = ListT $ WriterT $ state (\s -> ((l,[]), s))

main = do
  let ((res,log), final) = flip runState 0 $ runWriterT $ runListT $ logSums [1..10]
  print res
  forM log print
  print final
  

