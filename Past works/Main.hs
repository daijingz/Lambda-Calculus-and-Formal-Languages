{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
 import qualified A5

-- p
 main :: IO ()
 main = do
 print $$(A5.st1)
 print $$(A5.st2)
 print $$(A5.st3)
 print $$(A5.st4)
 print $$(A5.st5)
 print $$(A5.st6)
 print $$(A5.st7)
 print $$(A5.st8)
 print $$(A5.st9)
 print $$(A5.st10)

 -- Expected output
 {-
(6,())
(True,())
("Hello",())
((False,True),())
((11,9),())
(("059","400201"),())
(0,())
(True,())
(300,())
(False,())
 -}