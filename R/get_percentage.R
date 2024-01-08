#' Will get the percentage of marks scoring above a specified percentage
#'
#' @param marks A vector containing the marks of all the students
#' @param percentage The percentage we are interested in
#' @param max_mark The maximum possible mark on the test
#'
#' @return The percentage of marks above the specified percentage
#' @export

get_percentage <- function(marks, percentage, max_mark){
  UseMethod("get_percentage")
}

#'@export

get_percentage.default <- function(marks, percentage, max_mark)
  {
    per_mark <- percentage*max_mark/100
    count <- length(marks[marks>=per_mark])
    per_pass <- count/length(marks)*100
    return(per_pass)
  }
