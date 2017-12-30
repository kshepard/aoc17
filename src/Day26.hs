{-# LANGUAGE FlexibleContexts #-}
module Day26 where

import Control.Monad.Reader

data Env = Env { test1 :: Int, test2 :: Int }

fun1 :: (MonadReader Env m, MonadIO m) => m(String)
fun1 = do
  env <- ask
  liftIO . print $ test1 env
  return "A"

fun2 :: (MonadReader Env m, MonadIO m) => m(Int)
fun2 = do
  env <- ask
  liftIO . print $ test2 env
  return 8

fun3 :: (MonadReader Env m, MonadIO m) => m()
fun3 = do
  f1 <- fun1
  f2 <- fun2
  liftIO $ print $ f1
  liftIO $ print $ f2

main :: IO ()
main = do
  let env = Env { test1 = 1, test2 = 2 }
  runReaderT fun3 env
  print "___________"
