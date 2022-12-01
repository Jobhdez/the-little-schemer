import qualified Data.Map as Map

type Months = Int
type HoursWorked = Int
type HourlyRate = Int
type TotalSavings = Int

describeIncome :: TotalSavings -> String
describeIncome savings =
  "You will save " ++ show savings ++ ":)"


computeTaxes :: Months ->  HoursWorked -> HourlyRate -> TotalSavings
computeTaxes months hrs rate =
  income - taxes
  where
    income = computeIncome months hrs rate
    taxes = 200
  
computeIncome :: Months -> HoursWorked -> HourlyRate -> TotalSavings
computeIncome months hours rate =
  months * income
  where
    income = (hours * rate) * weeks where
      weeks = 4

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

