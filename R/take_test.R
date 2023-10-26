#' Makes a given student answer all the questions in a given test
#'
#' @param s The student that will take the test
#' @param t The test that the student will answer
#'
#' @return A student with updated marks reflecting the results of answering the test
#' @export

take_test <- function(s = student(), t = test()){
  UseMethod("take_test")
}

#'@export

take_test.default<-function(s = student(), t = test()){
  #Gets the given student to answer each question in a test in turn
  for (i in t$questions)
  {
    s <- answer_question(s = s, q = i)
  }
  if (s$total_marks < 0 && t$min_marks == FALSE){
    s$total_marks <- 0
  }
  s <- s
}
