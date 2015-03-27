{-# LANGUAGE ScopedTypeVariables #-}
import Data.SBV


main :: IO ()
main = do
    res <- allSat failureExpression
    putStrLn $ show res
    res2 <- allSat thresholdExpression
    putStrLn $ show res2

--CAUTION: Impacts solving time negatively!
probability :: String -> Symbolic SDouble
probability varName = do
    x <- sDouble varName
    constrain $ x .>= 0.0 &&& x .<= 1 &&& ((1 / x) .> 0)
    return x

failureExpression :: Predicate
failureExpression = do
        --safetyThreshold <- exists "SafetyThreshold"
        --constrain $ safetyThreshold .== 1e-9
        battery <- sBool "Battery"
        let
            batteryBurns :: SDouble
            batteryBurns = 1e-9
        sensor <- sBool "Sensor"
        let
            sensorFails :: SDouble
            sensorFails = 5e-9

        andFails <- probability "AndFails"
        constrain $ andFails .== andGate battery batteryBurns sensor sensorFails

        orFails <- probability "OrFails"
        constrain $ orFails .== orGate battery batteryBurns sensor sensorFails

        return true

thresholdExpression :: Predicate
thresholdExpression = do
    battery <- exists "Battery"
    let
        batteryBurns :: SFloat
        batteryBurns = 1e-9

    sensor <- exists "Sensor"
    let
        sensorFails :: SFloat
        sensorFails = 5e-9
    orFails <- exists "OrFails"
    constrain $ orFails .== orGate battery batteryBurns sensor sensorFails

    safetyThreshold <- exists "SafetyThreshold"
    constrain $ safetyThreshold .== 6e-9

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
