module Main where

import System.Environment
import System.TimeIt
import ArraySort

main :: IO ()
main = do arg <- getArgs
    case args of [x, y] do
    arr = [1..y]
    let arr1 = shuffle x arr
    let arr2 = arr1
    let arr3 = arr2
    let result1 = timeIt $ qsort arr1
    let result2 = timeIt $ isort arr2
    let result3 = timeIt $ bsort arr3
    print $ "qsort: " ++ show result1
    print $ "isort: " ++ show result2
    print $ "bsort: " ++ show result3
