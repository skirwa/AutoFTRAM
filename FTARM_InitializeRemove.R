#' @title Initialize_Remove
#' @description Calculate the number of items to generate the desired number of rules,
#' and prepare to remove unneeded items.
#' @details This function is necessary at the start of the full FTARM algorithm to
#' cleanse the data of items useless to generating rules, and to determine the minimum
#' support value.
#' @param DB The data to generate rules for.
#' @param k The number of rules to be generated.
#' @param mconf The minimum accepted confidence value.
#' @return The list of items to generate the desired number of rules.
#' @author @skirwa
#' @export 
Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k.
  m <- k * (2/15) # Derived from the example in the article (Propert 5). 
  
  # Sort items in the database based descending order of support.
  DB1 <- SortItem(DB) 
  
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