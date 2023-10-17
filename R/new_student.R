#Creates a new object with class 'student'
new_student <- function(answer = integer(), marks = numeric()){
  structure(list(given_answer = answer,total_marks = marks), class = "student")
}
