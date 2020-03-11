{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UI where

import qualified Data.Text as T
import NeatInterpolation (text)

mainPage :: T.Text
mainPage = [text|
<h1>Cookies</h1>
<h2>how many do you need?</h2>
<input></input>
|]
