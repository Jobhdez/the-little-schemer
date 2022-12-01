import qualified Data.Map as Map

type Months = Float
type HoursWorked = Float
type HourlyRate = Float
type TotalSavings = Float
type TotalIncome = Float

taxRates = Map.fromList [("WA", 10.0), ("CA", 12.0), ("NY", 12.0)]

describeIncome :: TotalSavings -> String
describeIncome savings =
  "You will save " ++ show savings ++ ":)"


computeTaxes :: Months ->  HoursWorked -> HourlyRate -> TotalSavings
computeTaxes months hrs rate =
  income - taxes
  where
    income = computeIncome months hrs rate
    taxes = (taxRate / 100.0) * income2 where
      taxRate = 10.0
      income2 = computeIncome months hrs rate
  
computeIncome :: Months -> HoursWorked -> HourlyRate -> TotalIncome
computeIncome months hours rate =
  months * income
  where
    income = (hours * rate) * weeks where
      weeks = 4.0

main :: IO ()
main = do
  putStrLn "Enter the projected months"
  months <- getLine
  putStrLn "Enter the hours worked"
  hrs <- getLine
  putStrLn "Enter the hourly rate"
  rate <- getLine
  let ms = read months
  let hours = read hrs
  let r = read rate
  let income = computeTaxes ms hours r
  putStrLn (describeIncome income)

