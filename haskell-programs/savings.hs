import qualified Data.Map as Map

{-
this little program computes the top 10 highest income tax rates. see the hashmap.
-}

type Months = Double
type HoursWorked = Double
type HourlyRate = Double
type Income = Double
type TotalSavings = Double
type TotalIncome = Double
type Month = String
type Expenses = Double

taxRates = Map.fromList [("CA", 13.3), ("Hawaii", 11.0), ("NJ", 10.75), ("Oregon", 9.9), ("Minnesota", 9.85), ("Columbia", 8.95), ("NY", 8.82), ("Vermont", 8.75), ("Iowa", 8.53), ("Wisconsin", 7.65)]




describeIncome :: TotalSavings -> String
describeIncome savings =
  "You will save " ++ show savings ++ ":)"


computeTaxes :: Months -> Month -> HoursWorked -> HourlyRate -> Expenses -> TotalSavings
computeTaxes months month hrs rate expenses =
  (income - taxes) - (expenses * months)
  where
    income = computeIncome months hrs rate
    taxes = (taxRate / 100.0) * income2 where 
      income2 = computeIncome months hrs rate
      taxRate = getTaxValue trate where
      trate = Map.lookup month taxRates
  
computeIncome :: Months -> HoursWorked -> HourlyRate -> TotalIncome
computeIncome months hours rate =
  months * income
  where
    income = (hours * rate) * weeks where
      weeks = 4.0

getTaxValue :: Maybe Double -> Double
getTaxValue x =
  case x of
    Nothing -> 0.0
    Just val -> val

main :: IO ()
main = do
  putStrLn "How many months do you plan to save?"
  months <- getLine
  putStrLn "Enter the hours worked per week"
  hrs <- getLine
  putStrLn "Enter the hourly rate"
  rate <- getLine
  putStrLn "What state do you live in?"
  m <- getLine
  putStrLn "How much are your monthly expenses?"
  exp <- getLine
  let ms = read months
  let hours = read hrs
  let r = read rate
  let month = m
  let expenses = read exp
  let income = computeTaxes ms month hours r expenses
  putStrLn (describeIncome income)

