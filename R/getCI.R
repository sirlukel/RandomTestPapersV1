#' @export

getPerCI <- function(marks, percentage, max_mark)
{
  UseMethod("getPerCI")
}

#'@export

getPerCI.default <- function(marks, percentage, max_mark)
{
  per_mark <- percentage*max_mark/100
  count <- length(marks[marks>=per_mark])
  per_pass <- count/length(marks)*100
  se <- sd(c(rep(1, count), rep(0, length(marks)-count)))/sqrt(length(marks))*100
  return(c(per_pass - 1.96*se, per_pass + 1.96*se))
}

#'@export

getAvCI <- function(marks)
{
  UseMethod("getAvCI")
}

#'@export

getAvCI.default <- function(marks)
{
  return(c(mean(marks)- 1.96*sd(marks)/sqrt(length(marks)), mean(marks) + 1.96*sd(marks)/sqrt(length(marks))))
}
