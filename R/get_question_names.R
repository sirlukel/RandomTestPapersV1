#' Will get the names of each of the questions in a test
#'
#' @param t The test that the question names should be extracted from
#'
#' @return A vector of all the question names from the test
#' @export
#'
#' @examples
get_question_names <- function(t = test())
{
  UseMethod('get_question_names')
}

get_question_names.default <- function(t = test())
{
  question_names <- c()
  for(i in t$questions)
  {
    question_names <- c(question_names, i$name)
  }
  question_names<-question_names
}
