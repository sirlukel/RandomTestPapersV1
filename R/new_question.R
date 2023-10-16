#Creates a new object with class 'question'
new_question <- function(option_num = integer(), correct_score = numeric(), wrong_loss = numeric(), partial_score = logical(), given_no_correct = logical(), correct = list()){
  #Updates score per correct answer if you are allowed partial marks
  if (partial_score == TRUE) {correct_score <- correct_score/length(correct)}
  structure(list(option_num = option_num,correct_score = correct_score, wrong_loss = wrong_loss, partial_score = partial_score, given_no_correct = given_no_correct,correct= sort(correct)), class = "question")
}
