
# DB - Database
# k - Number of  rules that the user wants to be generated.
# L - A set containing valid rules. If the support of a rule meets the minimum support value,
#     then the rule is added to the list L.
#     L contains the k most frequent association rules that meet the minimum support and confidence values.
# I - A set of items (i, j). 

# User-defined functions to be called:
# Initialize_Remove, Save, Expand_L, Expand_R
Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k
  m <- k + 1
  
  # sort based on sup from DB
  DB1 <- sortDescBasedOnSup(DB)

  for (i in seq(from=1, to=length(DB1), by=m)) {
    I <- head(DB1,m)
    print(I)
    if (minConf(I) > mconf) {
      break;
    }
  }
  msup <- minSup(DB, I)
  
  for (i in data) {
    if (i.sup < msup) {
      # Remove from the db
    }
  } m
}

minConf <- function(x, DB) {
  maxTid = -1
  print(x)
  for (i in x) {
    if (length(tid(i)) > maxTid) {
      maxTid = length(tid(i))
    }
  }
  return(length(tids(x))/maxTid)
}

minSup <- function(x, data) {
   return(length(tids(x))/length(data))
}

tids <- function(x, data) {
  # query from db to get the tids for given set of items
}

sortDescBasedOnSup <- function(x) {
  swap_done <- TRUE
  while (swap_done) {
    swap_done <- FALSE
    for (i in 1:(length(x) - 1)) {
      if (sup(x[i]) < sup(x[i + 1])) {
        tmp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- tmp
        swap_done <- TRUE
      }
    }
  }
  return(x)
}

ftarm <- function(DB, k, minconf) {
  R <- list()
  L <- list()
  # Scan transaction list from the database to get the itemSet I 
  I <- list()
  
  Initialize_Remove(DB, k, mconf)
  
  # Select two items
  for (i in DB) {
    for (j in DB) {
     if (i != j & sup(i) >= minsup & sup(j) >= minsup) {
       newItems <- i.append(j)
       if (sup(newItems) >= minsup) {
         if (conf(i, j) >= minconf) {
           Save(newSet, L, k, minsup)
         }
         if (conf(j, i) >= minconf) {
           Save(newSet, L, k, minsup)
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
        Expand_L(r, L, R, k, minsup, minconf)
        Expand_R(r, L, R, k, minsup, minconf)
      } else {
        Expand_R(r, L, R, k, minsup, minconf)
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