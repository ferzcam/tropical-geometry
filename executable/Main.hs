-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Example

data Day= Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
    deriving(Eq, Ord, Show, Read, Bounded, Enum)


main :: IO ()
main = Example.main
