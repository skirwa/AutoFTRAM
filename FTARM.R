#' @title Initialize_Remove
#' @description TODO: add description
#' @details
#' @param DB
#' @param k
#' @param mconf
#' @return 
#' @author @skirwa
Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k.
  m <- k * (2/15) # Derived from the example in the article (Propert 5). 
  
  # Sort items in the database based descending order of support.
  DB1 <- sortDescBasedOnSup(DB) 
  
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

#' @title sup
#' @description Calculate the support value for an item.
#' @details
#' @param x The item whose support value will be calculated.
#' @param DB The data set the item originates from.
#' @return The support value.
#' @author @skirwa
sup <- function(x, DB) {
  # Support is the no of transactions (I) in DB with item i / total number of transactions in DB.   
  support <- tids(x, DB) / length(DB)
  return(support)
}

#' @title conf
#' @description Calculate the confidence value for a pair of items in the data.
#' @details
#' @param x The first of the two items to compute the confidence value for.
#' @param y The second of the two items to compute the confidence value for.
#' @param DB The data set the two items originate from.
#' @return The confidence value.
#' @author @skirwa
conf <- function(x, y, DB) {
    # Confidence is the no of transactions with items x and y / no of transactions with item x.
    confidence = tidsForTwo(x, y, DB) / tids(x, DB)
    return (confidence)
}

#' @title minConf
#' @description Calculate the minimum confidence value for a pair of items in the data.
#' @details
#' @param x The first of the two items to compute the confidence value for.
#' @param y The second of the two items to compute the confidence value for.
#' @param DB The data set the two items originate from.
#' @return The minimum acceptable confidence value for the pair of items.
#' @author @skirwa
minConf <- function(x, y, DB) {
  maxTid = -1
  
  if (tidsForTwo(x, y, DB) > maxTid) {
    maxTid = tidsForTwo(x, y, DB)
  }

  return(tidsForTwo(x, y, DB)/maxTid)
}

#' @title minSup
#' @description Calculate the minimum support value for a pair of items in the data.
#' @details
#' @param x The first of the two items to compute the confidence value for.
#' @param y The second of the two items to compute the confidence value for.
#' @param DB The data set the two items originate from.
#' @return The minimum acceptable support value for the pair of items.
#' @author @skirwa
minSup <- function(x, y, DB) {
   return(tidsForTwo(x, y, DB)/length(DB))
}

#' @title tids
#' @description Find the number of transaction IDs that contain a certain value.
#' @details 
#' @param searchFor The element being searched for in each column of the database.
#' @param DB The database being searched for instances of searchFor.
#' @return The number of instances of searchFor in DB (as an integer).
#' @author @NessXToJason
tids <- function(searchFor, DB) {
  # DB must be coerced into a list first
  DB <- as.list(DB)

  # Find the total number of times searchFor appears in ONE column
  # (this is typically no more than once for our purposes)
  result <- lapply(DB, function(x) {
    sum(x == searchFor)
  })
  
  # Total up the number of times searchFor appeared in ALL columns
  total <- 0
  for(i in result) {
    if(i[1] == 1) {
      total <- total + 1
    }
  }
  return(total)
}

#' @title tidsForTwo
#' @description Find the number of transaction IDs that contain BOTH of two given values.
#' @details
#' @param searchFor1 The first element being searched for in each column of the database.
#' @param searchFor2 The second element being searched for in each column of the database.
#' @param DB The database being searched for instances of searchFor.
#' @return The number of columns BOTH values appeared in.
#' @author @NessXToJason
tidsForTwo <- function(searchFor1, searchFor2, DB) {
  # DB must be coerced into a list first
  DB <- as.list(DB)

  # Find the number of times BOTH values appear in each column
  # (should be a maximum of once per column)
  result <- lapply(DB, function(x) sum(x == searchFor1) != 0 && sum(x == searchFor2) != 0)
  
  # Total up the number of times BOTH values appeared in ALL columns
  total <- 0
  for(i in result) {
    if(i[1]) {
      total <- total + 1
    }
  }
  return(total)
}

#' @title SortItem
#' @description Sort the items in the data set based on their support value.
#' @details 
#' @param DB The data whose items will be sorted.
#' @return A descending list of the items in the data based on their support values.
#' @author @skirwa
SortItem <- function(DB) {
  #Define an empty vector
  itemsinData <- c()
  #New database
  for (i in DB){
    itemsinData <- c(itemsinData, i)
  }
  new_df <- data.frame(sapply(itemsinData,c))
  colnames(new_df)[1] ="id"
  
  func <- function(new_df){
    return(sapply(new_df$id, function(id) sum(new_df$id == id)/nrow(new_df)))
  }
  
  new_df$support <- func(new_df)
  swapped <- new_df[order(support), ]
  swapped
}