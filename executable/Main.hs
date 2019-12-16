-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-#LANGUAGE DataKinds #-}

module Main where

import qualified Example
import Core


x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f5 = 2*x*y^^(-1) + 2*y^^(-1) + (-2)
    
res = makeFigs [f1,f2]

main :: IO ()
main = Example.main
