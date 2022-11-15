
# DB - Database
# k - Number of  rules that the user wants to be generated.
# L - A set containing valid rules. If the support of a rule meets the minimum support value,
#     then the rule is added to the list L.
#     L contains the k most frequent association rules that meet the minimum support and confidence values.
# I - A set of items (i, j). 

# User-defined functions to be called:
# Initialize_Remove, Save, Expand_L, Expand_R

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

# Algorithm 4 - expand_L

# Algorithm 5 - expand_R
  expand_R <- function(r, L, R, k, minsup, minconf) {
    # Scan transaction list from db to get the itemSet I
    # FIXME: I should be a candidate item set of the CONSEQUENT of r
    I <- list()
    for (i in I) { # for every item i in I
      # add i to the consequent of r to get r'
      r1.itemSet <- append(r.itemSet, i)
      # if the support value of r' is greater than or equal to our provided minimum support value 
      if (sup(r1) >= minsup) {
        if (conf(r.itemset, i) >= minconf) { # and
        # save with new rule r', but the same L, k, and minimum support value
          save(r1, L, k, minsup)
        }
        # FIXME: if the LARGEST item of the CONSEQUENT of r' is LESS THAN MaxItem
        for (i in r1.itemSet) {
          if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
            expandLR <- FALSE
            R <- append(R, r1) # FIXME: algorithm: R := R union {r'}
          }
        }
      }
    }
  }
