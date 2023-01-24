module Main where

import NTT
import CompileKernel

path = [(Phi 4 2 0 4 5), Kernel_Repeat 4 2 (Phi 2 2 0 4 5), Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))]

main :: IO ()
main = putStrLn "Hello, Haskell!"
