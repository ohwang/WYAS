
import Data.IORef
-- mutable reference in the IO Monad

-- mutable Env because it's IORef
-- mutable LispVal because it's IORef
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- use ErrorT to wrap existing Monad
-- not applicable to our application
type IOThrowsError = ErrorT LispError IO

--

