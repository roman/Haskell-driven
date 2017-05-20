{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Lib (inc)

import Protolude

main :: IO ()
main = print . inc $ (41 :: Int)
