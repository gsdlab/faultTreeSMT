{-# LANGUAGE ScopedTypeVariables #-}
import Data.SBV


main :: IO ()
main = do
    res <- allSat failureExpression
    putStrLn $ show res

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

        andFails <- exists "AndFails"
        constrain $ andFails .== andGate battery batteryBurns sensor sensorFails

        orFails <- exists "OrFails"
        constrain $ orFails .== orGate battery batteryBurns sensor sensorFails

        failure  <- exists "Failure"
        constrain $ failure .== batteryBurns

        return true

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

