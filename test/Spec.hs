import Test.QuickCheck (quickCheck, Positive)
import Lib

confirm :: Double -> Double -> Double -> Double -> Bool
confirm μ1 σ12 μ2 σ22 = x >= 0 || isNaN x
                        where x = experiment μ1 σ12 μ2 σ22

main :: IO ()
main = quickCheck confirm
