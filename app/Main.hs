module Main where

import System.Environment (getEnv)

import Lib

main :: IO ()
main = getEnv "PORT" >>= read . someFunc
