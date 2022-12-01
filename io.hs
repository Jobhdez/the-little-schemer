type NumberOfAs = Int 
type Allowance = Int
type FavoriteNumber = Int

computeAllowance :: NumberOfAs -> FavoriteNumber -> Int 
computeAllowance grades num = grades * num 

describeAllowance :: Allowance -> String
describeAllowance allowance =
    "You will get " ++ show allowance ++ " per month! :)"

main :: IO ()
main = do
    putStrLn "Hello, what is your favorite number?"
    num <- getLine
    putStrLn "How many A's did you get this quarter?"
    grades <- getLine
    let as = read grades
    let n = read num
    let allowance = computeAllowance as n
    putStrLn (describeAllowance allowance)
    
