#' @title SortItems
#' @description Sort the items in the data set based on their support value.
#' @details Updating the minimum support value in Initialize_Remove is a simpler process
#' when the data set is sorted by the items' support value.
#' @param DB The data whose items will be sorted.
#' @return A descending list of the items in the data based on their support values.
#' @author @skirwa
#' @export 
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