#' Creates a new object with class 'student'
#'
#' @param answer A list of option numbers that the student will use as the answer to the question
#' @param marks The current amount of marks that the student has
#'
#' @return A new Student class object
#' @export

new_student <- function(answer, marks){
  structure(list(given_answer = as.integer(answer),total_marks = as.numeric(marks)), class = "student")
}
