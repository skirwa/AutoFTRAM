
ftarm <- function(DB, k, minconf) {
  R <- list()
  L <- list()
  # Scan transaction list from db to get the itemSet I 
  I <- list()
  
  Initialize_Remove(DB, k, mconf)
  
  # Select two items
  for (i in DB) {
    for (j in DB) {
     if (i != j & sup(i) >= minsup & sup(j) >= minsup) {
       newItems <- i.append(j)
       if (sup(newItems) >= minsup) {
         if (conf(i, j) >= minconf) {
           save(newSet, L, k, minsup)
         }
         if (conf(j, i) >= minconf) {
           save(newSet, L, k, minsup)
         }
         # compute MAXITEM here
         # update R
       }
     } 
    }
  }
  for (r in R) {
    if (sup(r) > minsup) {
      for (r in R) {
        if (maxSupR == 0 | sup(r) > maxSupR) {
          maxSupR <- r
        } 
      }
      if (maxSupR.expandLR == TRUE) {
        expand_L(r, L, R, k, minsup, minconf)
        expand_R(r, L, R, k, minsup, minconf)
      } else {
        expand_R(r, L, R, k, minsup, minconf)
      }
      
      newR <- list()
      for (r1 in R) {
        if (r != r1 & sup(r1) >= minsup) {
          newR <- newR.append(r1)
        }
      }
    }
    R <- newR
  }
  return (L)
}