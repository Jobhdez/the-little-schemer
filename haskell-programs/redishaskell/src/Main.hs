{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (findIndex, sortOn)

data Expression = Expression { exprId :: Int } deriving (Eq, Show)

getLalgKey :: Int -> ByteString
getLalgKey id = pack $ "expression:" ++ show id ++ ":computed with"

expressionsComputed :: Connection -> [Expression] -> IO ()
expressionsComputed conn expressions = do
    let expressionIds = map exprId expressions
    forM_ expressionIds $ \expressionId ->
        forM_ expressionIds $ \withId ->
            if expressionId /= withId
            then runRedis conn $ do
                _ <- zincrby (getLalgKey expressionId) 1 (pack $ show withId)
                return ()
            else return ()

suggestExpressionsFor :: Connection -> [Expression] -> Int -> IO [Expression]
suggestExpressionsFor conn expressions maxResults = do
    let expressionIds = map exprId expressions
    suggestions <- if length expressions == 1
        then runRedis conn $ do
            result <- zrangeWithscores (getLalgKey $ head expressionIds) 0 (-1)
            return $ case result of
                Left _ -> []
                Right res -> take maxResults $ map fst res
        else runRedis conn $ do
            let flatIds = concatMap show expressionIds
            let tmpKey = pack $ "tmp_" ++ flatIds
            let keys = map getLalgKey expressionIds
            _ <- zunionstore tmpKey keys Sum -- Fixed: Use Sum directly for aggregation
            _ <- zrem tmpKey $ map (pack . show) expressionIds
            result <- zrangeWithscores tmpKey 0 (-1)
            _ <- del [tmpKey]
            return $ case result of
                Left _ -> []
                Right res -> take maxResults $ map fst res

    let suggestedExpressionsIds = map (read . unpack) suggestions
    -- Assuming you have a function to fetch the expressions by IDs
    suggestedExpressions <- fetchExpressionsByIds suggestedExpressionsIds
    return $ sortOn (`indexIn` suggestedExpressionsIds) suggestedExpressions

indexIn :: Expression -> [Int] -> Int
indexIn expr ids = maybe (-1) id $ findIndex (== exprId expr) ids

fetchExpressionsByIds :: [Int] -> IO [Expression]
fetchExpressionsByIds ids = return $ map Expression ids

main :: IO ()
main = do
    conn <- checkedConnect defaultConnectInfo
    let expressions = [Expression 1, Expression 2, Expression 3]
    expressionsComputed conn expressions
    suggestions <- suggestExpressionsFor conn [Expression 2] 6
    print suggestions
