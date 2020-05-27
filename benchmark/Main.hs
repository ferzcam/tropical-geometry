-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.

{-#LANGUAGE DataKinds #-}

import Core
import Criterion.Main
import Hyper
import Hyper3
import Hyper4
import Hyper5

main :: IO ()
main = defaultMain [
        benchHyper,
        benchHyper3,
        benchHyper4,
        benchHyper5
    ] 