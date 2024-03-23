removeEmpty :: [[a]] -> [[a]]
removeEmpty = filter (not . null)

createLists :: [Int] -> [[Int]]
createLists list =  removeEmpty $ createLists' list 1 1 True False False
  where
    createLists' [] _ _ _ _ _ = []
    createLists' l n keepN up top down
        | (length list - 2*sum[1..n] >= (n+1)*2) && up =    let (first, rest) = splitAt n l
                                                            in first : createLists' rest (n + 1) (n + 1) up top down
        | up =                                              let (first, rest) = splitAt n l
                                                            in first : createLists' rest (length list - (2*sum[1..n])) n False True False
        | top =                                             let (first, rest) = splitAt n l
                                                            in first : createLists' rest keepN keepN False False True
        | down =                                            let (first, rest) = splitAt n l
                                                            in first : createLists' rest (n - 1) (n-1) False False True



main :: IO ()
main = do
  print $ createLists []
  print $ createLists [1]
  print $ createLists [1..2]
  print $ createLists [1..3]
  print $ createLists [1..10]
  print $ createLists [1..12]
  print $ createLists [1..13]
  print $ createLists [1..30]
