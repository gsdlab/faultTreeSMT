{-# LANGUAGE ScopedTypeVariables #-}
import Data.SBV


main :: IO ()
main = do
    res <- allSat failureExpression
    --res <- allSat thresholdExpression
    putStrLn $ show res

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
        battery <- exists "Battery"
        let
            batteryBurns :: SDouble
            batteryBurns = 1e-9
        sensor <- exists "Sensor"
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
    constrain $ orFails .== orGateSingle battery batteryBurns sensor sensorFails

    safetyThreshold <- exists "SafetyThreshold"
    constrain $ safetyThreshold .== 6e-9

    return $ orFails .< safetyThreshold

andGate :: SBool -> SDouble -> SBool  -> SDouble -> SDouble
andGate    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)

orGate :: SBool -> SDouble -> SBool  -> SDouble -> SDouble
orGate    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb + rightProb - leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)

andGateSingle :: SBool -> SFloat -> SBool  -> SFloat -> SFloat
andGateSingle    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)

orGateSingle :: SBool -> SFloat -> SBool  -> SFloat -> SFloat
orGateSingle    haveLeft leftProb  haveRight rightProb =
    ite (haveLeft)
        (ite haveRight (leftProb + rightProb - leftProb * rightProb) leftProb)
        (ite haveRight rightProb 0)

