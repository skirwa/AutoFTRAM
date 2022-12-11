# DB - Database
# k - Number of  rules that the user wants to be generated.
# L - A set containing valid rules. If the support of a rule meets the minimum support value,
#     then the rule is added to the list L.
#     L contains the k most frequent association rules that meet the minimum support and confidence values.
# I - A set of items (i, j). 

# Algorithm 1 - ftarm algorithm

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


# Algorithm 2 - Initialize_Remove
# Sharon
Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k.
  m <- k * (2/15) # Derived from the example in the article (Propert 5). 
  
  # Sort items in the database based descending order of support.
  DB1 <- SortItems(DB) 
  
  # A list of minimum number of items m required to generate k rules.
  I <- head(DB1,m) 

  # For each item in the list I:
  for (i in I) {   
    if (minConf(I, DB1) >= mconf) { 
      break;
    }
  }
  msup <- minSup(DB, I)
  
  # For each item in list I:
  for (i in I) { 
    # If it's support value is less than the minimum support value:
    if (sup(i) < msup) {
      # Remove the item from the list.  
      I[i] <- NULL 
    }
  }
}

# Calculate the support value for an item. WORKING
sup <- function(DB, i) {
  # Support is the no of transactions (I) in DB with item i / total number of transactions in DB.   
  support <- tids(i, DB) / length(DB)
  return(support)
}

# Calculate the confidence value for an item WORKING
conf <- function(DB, x, y) {
    #totalItems = append(x, y)
    # Confidence is the no of transactions with items x and y / no of transactions with item x.
    # 
    confidence = tidsForTwo(x, y, DB) / tids(x, DB)
    return (confidence)
}

# A list of items should be parsed into minConf and minSup
# However, we were unable to write a tids function that takes in a list.
# Therefore, for minConf and minSup, we used the tidsForTwo function instead.
# The minConf function is supposed to calculate the minimum confidence value.
minConf <- function(DB, searchFor1, searchFor2) {
  maxTid = -1
  x <- list()
  x <- append(x, searchFor1, searchFor2)
  for (i in x) {
    if (tids(i, DB) > maxTid) {
      maxTid = tids(i, DB)
    }
  }
  return(tidsForTwo(searchFor1, searchFor2, DB)/maxTid)
}

# The minSup function calculates the minimum support value.
minSup <- function(x, data) {
   return(tids(x, data)/length(data))
}

# Tids is the transaction identifier of a set of items.
# tids(x) - how many transactions (columns) have item x?
# tids(x,y) - how many transactions (columns) have items x and y combined?
# Jason
tids <- function(searchFor, DB) {
  # movie_ratings implementation: count how many columns x appears in
  DB <- as.list(DB)
  result <- lapply(DB, function(x) {
    sum(x == searchFor)
  })
  
  total <- 0
  for(i in result) {
    if(i[1] == 1) {
      total <- total + 1
    }
  }
  return(total)
}

tidsForTwo <- function(searchFor1, searchFor2, DB) {
  # calculate the number of columns that contain x AND y
  # FIXME: reformat this for matrix
  #diff(t(DB@data)@p)[which(DB@itemInfo$labels == x)]
  # movie_ratings implementation: count how many columns x AND y appear in

  DB <- as.list(DB)
  result <- lapply(DB, function(x) sum(x == searchFor1) != 0 && sum(x == searchFor2) != 0)
  
  total <- 0
  for(i in result) {
    if(i[1]) {
      total <- total + 1
    }
  }
  return(total)
}


# Function for sorting the items in the dataset based on their support value
SortItems <- function(DB) {
  # Define an empty vector
  itemsinData <- c()
  # Store the ids of the items in the database in the empty vector
  for (i in DB){
    itemsinData <- c(itemsinData, i)
  }
  # Convert the list of items to a dataframe
  new_df <- data.frame(sapply(itemsinData,c))
  # Rename the column with ids
  colnames(new_df)[1] ="id"
  # This function returns the support value for each item.
  func <- function(new_df){
    return(sapply(new_df$id, function(id) sum(new_df$id == id)/nrow(new_df)))
  }
  # Create a new column that contains the support values of the corresponding items.
  new_df$support <- func(new_df)
  # Sort the items in descending order based on their support values
  swapped <- new_df %>% arrange(desc(support))
  swapped
}


# Algorithm 3 - Save

# This function updates the set L, minsup and MaxItem
# in real time during the algorithm's execution.
# Sharon NOT WORKING
save <- function(r, L, k, minsup) {
  # Add rule r to the set L.
  L <- append(L, r)
  # If L contains atleast k rules after removing
  # the rules with a support of minsup, these rules are removed.
  if (length(L) >= k) {
    count = 0
    # Append rule to count if its support is equal to minsup
    for (rule in L) {
      if (sup(rule) == minsup) {
        # Count contains the number of rules in L with support equal to minsup
        count <- count + 1
      }
    }
    
    if (length(L) - count >= k) {
      # remove rules with support equal to minsup
      newL <- list()
      for (rule in L) {
        if (sup(rule) != minsup) {
          append(newL, rule)
        }
      }
      # L contains rules with support not equal to minsup
      L <- newL
      
      # set minsup to lowest support of rules in L
      # newMinSup <- min(unlist(L)) - Alternate procedure for getting newMinSup
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


# Algorithm 4 - expand_L NOT WORKING
expand_L <- function(r, L, R, k, minsup, minconf) {
  # Scan transaction list from db to get the itemSet I
  # FIXME: I should be a candidate item set of the ANTECEDENT of r
  I <- list()
  
  for (i in I) {
    r1.itemSet <- append(r.itemSet, i)
    
    if (sup(r1) >= minsup) {
      if (conf(r1.itemset, i) >= minconf) {
        save(r1, L, k, minsup)
      }
      expandLR <- TRUE
      # FIXME: Only check largest item, not all
      for (i in r1.itemSet) {
        if (i.totalOrderPosition > MaxItem.totalOrderPosition ) {
          expandLR <- FALSE
        } else {
          expandLR <- TRUE
        }
      }
    }
    R <- append(R, r1)
  }
}

# Algorithm 5 - expand_R NOT WORKING
  expand_R <- function(r, L, R, k, minsup, minconf) {
    # Scan transaction list from db to get the itemSet I
    # FIXME: I should be a candidate item set of the CONSEQUENT of r
    I <- list()
    for (i in I) { # for every item i in I
      # add i to the consequent of r to get r'
      r1.itemSet <- append(r.itemSet, i)
      # if the support value of r' is greater than or equal to
      # our provided minimum support value
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
