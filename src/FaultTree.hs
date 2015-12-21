{-# LANGUAGE ScopedTypeVariables #-}
import Data.SBV


main :: IO ()
main = do
    let fe1 = failureExpression
    res <- allSat fe1
    putStrLn "\nSolutions for Failure Expression: \n"
    putStrLn $ show res

    -- show the resulting SMT-LIB2 formula:
    {- compiledFe1 <- compileToSMTLib True True fe1
    putStrLn compiledFe1-}

    res2 <- allSat thresholdExpression
    putStrLn "\nSolutions for Treshold Expression: \n"
    putStrLn $ show res2

probability :: String -> Symbolic SDouble
probability varName = do
    x <- sDouble varName
    constrain $ fpIsPositive x &&& x .<= 1
    return x

failureExpression :: Predicate
failureExpression = do
        haveBattery <- sBool "haveBattery"
        let batteryBurns = (1e-9 :: SDouble)

        haveSensor <- sBool "haveSensor"
        let sensorFails = (5e-9 :: SDouble)

        andFails <- probability "AndFails"
        constrain $ andFails .== andGate haveBattery batteryBurns haveSensor sensorFails

        orFails <- probability "OrFails"
        constrain $ orFails .== orGate haveBattery batteryBurns haveSensor sensorFails

        return true

thresholdExpression :: Predicate
thresholdExpression = do
    haveBattery <- sBool "haveBattery"
    let batteryBurns = (1e-9 :: SFloat)

    haveSensor <- sBool "haveSensor"
    let sensorFails = (5e-9 :: SFloat)

    orFails <- sFloat "OrFails"
    constrain $ fpIsPositive orFails
            &&& orFails .== orGate haveBattery batteryBurns haveSensor sensorFails

    let safetyThreshold = (6e-9 :: SFloat)

    return $ orFails .< safetyThreshold

andGate :: (Mergeable a, Floating a) => SBool -> a -> SBool  -> a -> a
andGate    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)

orGate :: (Mergeable a, Floating a) => SBool -> a -> SBool  -> a -> a
orGate    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb + rightProb - leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)
