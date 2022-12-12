#' @title sup
#' @description Calculate the support value for an item.
#' @details The support value of items will need to be compared to the
#' minimum support value to determine which items to remove to accurately
#' generate the appropriate number of rules.
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
#' @details The confidence value of items will need to be compared to the
#' minimum confidence value to determine which items to remove to accurately
#' generate the appropriate number of rules.
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
#' @details The minimum confidence value will be used for generating and expanding rules.
#' @param x The first of the two items to compute the confidence value for.
#' @param y The second of the two items to compute the confidence value for.
#' @param DB The data set the two items originate from.
#' @return The minimum acceptable confidence value for the pair of items.
#' @author @skirwa
minConf <- function(DB, searchFor1, searchFor2) {
  maxTid = -1
  x <- list()
  x <- append(x, searchFor1, searchFor2)
  for (i in x) {
    if (tids(i, DB) > maxTid) {
      maxTid = tids(i, DB)
    }
  }
  out = round(tidsForTwo(searchFor1, searchFor2, DB)/maxTid,3)
  return(tidsForTwo(searchFor1, searchFor2, DB)/maxTid)
}

#' @title minSup
#' @description Calculate the minimum support value for a pair of items in the data.
#' @details The minimum support value will be used for generating and expanding rules.
#' @param x The first of the two items to compute the confidence value for.
#' @param y The second of the two items to compute the confidence value for.
#' @param DB The data set the two items originate from.
#' @return The minimum acceptable support value for the pair of items.
#' @author @skirwa
minSup <- function(x, y, DB) {
   return(tidsForTwo(x, y, DB)/length(DB))
}