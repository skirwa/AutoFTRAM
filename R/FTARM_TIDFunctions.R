#' @title tids
#' @description Find the number of transaction IDs that contain a certain value.
#' @details This function will be used as a helper function in the sup function.
#' @param searchFor The element being searched for in each column of the database.
#' @param DB The database being searched for instances of searchFor.
#' @return The number of instances of searchFor in DB (as an integer).
#' @author @NessXToJason
#' @export
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
#' @details Calculating confidence and minimum support/confidence values will require getting
#' the TIDs for multiple items.
#' @param searchFor1 The first element being searched for in each column of the database.
#' @param searchFor2 The second element being searched for in each column of the database.
#' @param DB The database being searched for instances of searchFor.
#' @return The number of columns BOTH values appeared in.
#' @author @NessXToJason
#' @export
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