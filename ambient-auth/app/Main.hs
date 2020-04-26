{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Comonad
import Control.Comonad.Env

import System.IO.Unsafe (unsafePerformIO)

-- Auths (assume constructors are not exported)
data SystemAuth = SystemAuth
data OutputAuth = OutputAuth
data InputAuth  = InputAuth

deriveOutputAuth :: SystemAuth -> OutputAuth
deriveOutputAuth _ = OutputAuth

deriveInputAuth :: SystemAuth -> InputAuth
deriveInputAuth _ = InputAuth

readLine :: Env InputAuth () -> String
readLine _ = unsafePerformIO getLine

putLine :: Env OutputAuth String -> ()
putLine e = unsafePerformIO (putStrLn (extract e))

system :: Env SystemAuth ()
system = env SystemAuth ()

-------------------------------------------------------------------
---- Main logic (assume in another module)

main :: IO ()
main = do
  let !(_, res) = runEnv (system =>> main')
  pure res

main' :: Env SystemAuth () -> ()
main' = liftWith deriveInputAuth readLine =>= liftWith deriveOutputAuth putLine

liftWith :: (auth -> auth') -> (Env auth' a -> b) -> (Env auth a -> b)
liftWith f g env' = g (local f env')
