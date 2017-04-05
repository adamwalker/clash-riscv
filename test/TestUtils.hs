module TestUtils where

import CLaSH.Prelude

--Hack to make block rams output something defined on the first cycle
firstCycleDef' :: a -> Signal a -> Signal a
firstCycleDef' defa = mealy step False
    where
    step False _ = (True, defa)
    step True  x = (True, x)

firstCycleDef :: Default a => Signal a -> Signal a
firstCycleDef = firstCycleDef' def

