module Exercise4 where

import Exercise3

emptySud sud [] = sud
emptySud sud (x:xs) = emptySud (eraseN sud x) xs

test = do
    [r] <- rsolveNs [emptyN]
    showNode r
    let remove = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6),(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]
    let result = emptySud r remove
    showNode result
    temp <- genProblem result
    showNode temp
    result <- checkMinimal temp
    print(result)