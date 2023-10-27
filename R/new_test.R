#' Creates a new object with class 'test'
#'
#' @param questions A list of questions that makes up the test
#' @param name The name of the test
#' @param can_neg A logical value. set to TRUE if a student should be able to achieve negative marks taking the test
#'
#' @return A new Test class object
#' @export

new_test <- function(questions = list(), name = chr(), can_neg = logical()){
  max_marks <-0
  min_marks <-0
  #Will calculate the maximum and minimum marks for a created test
  for (i in questions){
    if(i$partial_score){
    max_marks <- max_marks + as.numeric(i$correct_score)*length(i$correct)
    }else{
      max_marks <- max_marks + as.numeric(i$correct_score)
    }
    if (can_neg){
      min_marks <-min_marks + i$wrong_loss
    }
  }
  structure(list(questions = questions, name = name, max_marks = max_marks, min_marks = min_marks), class = "test")
}
