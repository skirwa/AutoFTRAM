# DB - Database
# m - Minimum number of items required to generate k rules.
# k - Number of  rules that the user wants to be generated.
# I - A set of items (i, j).

Initialize_Remove <- function (DB, k, mconf) {
  # Calculate minimum number of items m required to generate k rules based on k
  m <- k * (2/15) # calculated this value based on the figures given in the book. i.e,
                  # when k=30, then m=4
  
  # sort based on sup from DB
  DB1 <- sortDescBasedOnSup(DB)

  for (i in seq(from=1, to=length(DB1), by=m)) {
    I <- head(DB1,m)
    if (minConf(I) > mconf) {
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

sortDescBasedOnSup <- function(x) {
  swap_done <- TRUE
  while (swap_performed) {
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

