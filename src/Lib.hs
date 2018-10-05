module Lib (gaussian, klDiv, experiment) where

import Numeric.Integration.TanhSinh (everywhere, simpson, result, Result())

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

integrate :: (Double -> Double) -> Double
integrate f = avg $ result <$> everywhere simpson f

gaussian :: Double -> Double -> Double -> Double
gaussian μ σ2 x = 1/(sqrt(2 * pi * σ2)) * (exp 1)**(-(x-μ)^2 / (2*σ2))

-- Kullback-Leibler divergence
klDiv :: (Double -> Double) -> (Double -> Double) -> Double
klDiv f g = integrate $ \x -> (f x) * log((f x)/(g x))

experiment :: Double -> Double -> Double -> Double -> Double
experiment μ1 σ12 μ2 σ22 = let
    f = gaussian μ1 σ12
    g = gaussian μ2 σ22
    in klDiv f g
