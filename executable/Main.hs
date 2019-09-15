-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Example

data Day= Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
    deriving(Eq, Ord, Show, Read, Bounded, Enum)


newF1, newF2 :: [(Int, Int, Int)]

newF1 = [(2,0,3), (1,1,0), (0,2,3), (1,0,1),(0,1,1), (0,0,0)]

newF2 = [(3, 0, 3), (2, 1, 1), (1, 2, 1), (0, 3, 3), (2, 0, 1), (1, 1, 0), (0, 2, 1), (1, 0, 1), (0, 1, 1), (0, 0, 3)]
main :: IO ()
main = Example.main
