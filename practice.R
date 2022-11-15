---
title: "r-algorithms"
output: pdf_document
---


## Algorithm 3

# Let 
# I = {i1, i2, i3, ..., im} be a set of items 
# DB = {T1, T2, T3, ..., Tn} be a set of transactions, 
# Each transaction Tj (1 ≤ j ≤ n) comprises a subset of items and has tid

# Tidset (Transaction identifier set): Set of items
# Support: the proportion of transactions satisfied by r in DB, where Tj is said to be satisfied by r if it contains all the items of r



# Class for the rules are stored in list L

setClass("item", slots=list(totalOrderPosition="numeric");
setClass("rule", slots=list(itemset="list");



### Initialize_Remove ###


# Needs: minConf, I, sup, minSup, tid, sortDescBasedOnSup 
Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k
  m <- k + 1
  
  # sort based on sup from DB
  DB1 <- sortDescBasedOnSup(DB)

  for (i in seq(from=1, to=length(DB1), by=m)) {
    I <- head(DB1,m)
    if (minConf(I) >= mconf) {
      break;
    }
  }
  msup <- minSup(DB, I)
  
  for (i in DB) {
    if (i.sup < msup) {
      # Remove from the db
    }
  }
}

minConf <- function(x, DB) {
  maxTid = -1
  for (i in x) {
    if (length(tid(i)) > maxTid) {
      maxTid = length(tid(i))
    }
  }
  return(length(tids(x))/maxTid)
}

minSup <- function(x, DB) {
   return(length(tids(x))/length(DB))
}

tids <- function(x, DB) {
  # query from db to get the tids for given set of items
}

sup <- function(x) {
  return(length(tids(x)), length(DB))
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


### Save ###

save <- function(r, L, k, minsup) {
  L <- append(L, r)
  if (length(L) > k) {
    count = 0
    for (rule in L) {
      if (sup(l) == minsup) {
       count <- count + 1
      }
    }
    
    if (length(L) - count >= k) {
      # remove rules with support equal to minsup
      newL <- list()
      for (rule in L) {
        if (sup(l) != minsup) {
          append(newL, l)
        }
      }
      L <- newL
      
      # set minsup to lowest support of rules in L
      newMinSup <- 99999999
      for (rule in L) {
        if (sup(l) < newMinSup) {
          newMinSup <- sup(l)
        }
      }
      minsup <- newMinSup
      
      # Find max item based on minsup and position in total order 
      MaxItem <- 0
      for (i in I) {
        if (sup(i) > minsup & i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          MaxItem <- i
        }
      }
    }
  }
}


### Expand_L ###

expand_L <- function(r, L, R, k, minsup, minconf) {
  # Scan transaction list from db to get the itemSet I 
  I <- list()
  
  for (i in I) {
    r1.itemSet <- append(r.itemSet, i)
    
    if (sup(r) >= minsup) {
      if (conf(r.itemset, i) >= minconf) {
        save(r1, L, k, minsup)
      }
      expandLR <- TRUE
      for (i in r1.itemSet) {
        if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          expandLR <- FALSE
        }
      }
    }
    R <- append(R, r1)
  }
}

sup <- function(x) {
  return(length(tids(x)), length(DB))
}

conf <- function(x, y) {
    totalItems = append(x, y)
    return (length(tids(totalItems)/length(tids(x))))
}

tids <- function(x, DB) {
  # query from db to get the tids for given set of items
}


### Expand_R ###

expand_R <- function(r, L, R, k, minsup, minconf) {
  # Scan transaction list from db to get the itemSet I 
  I <- list()
  
  for (i in I) {
    r1.itemSet <- append(r.itemSet, i)
    
    if (sup(r1) >= minsup) {
      if (conf(r.itemset, i) >= minconf) {
        save(r1, L, k, minsup)
      }
      for (i in r1.itemSet) {
        if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          expandLR <- FALSE
          R <- append(R, r1)
        }
      }
    }
    
  }
}




### Main Function ###

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

