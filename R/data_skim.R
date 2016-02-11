#' Brief look at the data before cleaning.
#'
#' This function returns data frame that shows 
#' 1) features of the input data and 
#' 2) class, number of unique values, and number of NA values for each feature.
#' The function may be used as a supplement to summary(), str() in basic R package and glimpse() in dplyr package
#' to gain basic understanding of given dataset before conducting cleaning and processing missing values.
#' @param data Dataset as in data frame
#' @keywords data, summary, str
#' @export
#' @examples
#' data_skim(data = df)
data_skim <- function(data){
    list1 <- list()
    list2 <- list()
    list3 <- list()
    for(i in 1:ncol(data)){
        list1 <- c(list1, class(data[,i]))
        list2 <- c(list2, length(unique(data[,i])))
        list3 <- c(list3, sum(is.na(data[,i])))
    }
    result <- data.frame(columns = names(data), class = unlist(list1), count = unlist(list2), NAnum = unlist(list3))
    names(result) <- c("features", "class", "num_of_unique_value", "num_of_NA_value")
    return(result)
}