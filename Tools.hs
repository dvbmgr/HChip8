module Tools (replace, modifyElemIORef) where
	import Data.IORef

	replace :: Int -> a -> [a] -> [a]  
  	replace index replacement (n:ns) 
  		| index == 0 = replacement:ns
  		| otherwise = (n:(replace (index-1) replacement ns))

  	modifyElemIORef :: IORef [a] -> Int -> a -> IO ()
  	modifyElemIORef iref index replacement = do
  		a <- readIORef iref 
  		writeIORef iref $ replace index replacement a