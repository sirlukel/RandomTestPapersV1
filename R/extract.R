#' Will get a question from a list of all currently stored questions via its name
#'
#' @param name The name of the desired question
#' @param all_qs A data.frame of all currently saved questions
#'
#' @return The question with the given name if such a question exists
#' @export

extract_question <- function(name = character(), all_qs = data.frame())
{
  UseMethod("extract_question")
}

extract_question.default <- function(name = character(), all_qs = data.frame())
{
  temp = all_qs[all_qs$name == name, ]
  correct <-c()
  question<-NULL
  if(length(temp) == 0){print("question not found")}
  else{
    correct <- unlist((strsplit(as.character(temp$correct), split = ",")))
    correct <-as.numeric(correct)
    question <- temp[-length(temp)]
  }
  if(question$partial_score == TRUE){question$correct_score <- as.numeric(question$correct_score) * length(correct)}
  question <- new_question(name = question$name, option_num = question$option_num,correct_score = question$correct_score,wrong_loss = question$wrong_loss, partial_score = question$partial_score, given_no_correct = question$given_no_correct, correct = correct)
}

#' Will return a test object given the name of the test and a list of all tests and all questions
#'
#' @param name The name of the desired test
#' @param all_ts The list of all currently stored tests
#' @param all_qs A data.frame of all the currently stored questions
#'
#' @return The test with the given name if it exists
#' @export

extract_test <- function(name = character, all_ts = list(), all_qs = data.frame()){
  UseMethod('extract_test')
}

extract_test.default <- function(name = character(), all_ts = list(), all_qs = data.frame()){
  x<-NULL
  for(i in 1:length(all_ts))
  {
    if(all_ts[[i]][1] == name){
      x <- i
      break
    }
  }
  if(is.null(x))
  {
    print("Test not found")
  }else{
    y<- unlist(all_ts[x])
    q<-list()
    for (i in 2:length(y))
    {
      q[[i-1]] <- extract_question(name = y[i], all_qs = all_qs)
    }
    t <- new_test(q, name = y[1], can_neg = F)
  }
  t<-t
}
