#' Will Generate a test based on a list of question names
#'
#' @param all_qs A data.frame containing all current created questions
#' @param qs A list of all the question names you wish to add to the test
#' @param can_neg A logical argument. TRUE will mean a student can achieve a negative score on the test
#'
#' @return A new test containing the listed questions
#' @export

generate_test <- function(all_qs = data.frame(), qs = character(), can_neg = logical()){
  UseMethod('generate_test')
}

#'@export

generate_test.default <- function(all_qs = data.frame(), qs = character(), can_neg = logical()){
  x <- list()
  no <- c()
  for (i in 2:length(qs)){
    q <- extract_question(name = qs[i], all_qs = all_qs)
    x[[i-1]] <-q
    no <- c(no, paste("q",i-1, sep = ""))
  }
  #give each question an ordered name
  names(x) <- no
  x <- new_test(x, qs[1], can_neg = can_neg)
}
