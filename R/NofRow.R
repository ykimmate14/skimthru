#' Brief look at the data before processing.
#'
#' This function returns data frame that shows 
#' 1) unique value of a given column of dataset and 
#' 2) its number of occurrence
#' The function may be used as a supplement to summary(), str() in basic R package and glimpse() in dplyr package
#' to gain basic understanding of given dataset before conducting cleaning and processing missing values.
#' @param data Dataset as in data frame
#' @param c index of column that you want to skim through
#' @keywords data, summary, str
#' @export
#' @examples
#' NofRow(data = df, c = column_number)
NofRow <- function(data, c){
    list1 <- unique(data[,c])
    list2 <- list()
    for(i in 1:length(list1)){
        list2 <- c(list2, sum(data[,2] %in% list1[i]))
    }
    result <- data.frame(uniqueV = list1, count = unlist(list2))
    return(result)
}
