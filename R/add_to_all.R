#' Will add a question to the data.frame of all current questions
#'
#' @param x The questions to be added to the data.frame
#' @param addto The data.frame the question should be added to
#'
#' @return The new data.frame with the new question added
#' @export

add_to_all <- function(x, addto)
{
  UseMethod('add_to_all')
}

##This could be source of new questions being read as string
add_to_all.question <- function(x = question(), addto = data.frame())
{
  nullcheck <-FALSE
  if(is.null(addto)){
    nullcheck <- TRUE
  }
  duplicate <- FALSE
  for (i in addto$name){
    if(x$name == i){
      duplicate <- TRUE
    }
  }
  if(duplicate){
    print(addto[addto$name == x$name])
    print('question of this name already exists, please choose a different name')
    addto <- addto
  }else{
    x$correct <- paste(x$correct, collapse = ",")
    addto <- rbind(addto, unlist(x))
    if(nullcheck)
    {
      names(addto) <- c('name', 'option_num', 'correct_score', 'wrong_loss', 'partial_score', 'given_no_correct', 'correct')
    }
    addto<-addto
  }
}

#' Will add a test to the list of all current tests
#'
#' @param x The list containing the test name and the name of all questions in the test
#' @param addto The list of tests to be added to
#'
#' @return The list of tests containing the new test
#' @export

add_to_all_test <- function( x= character(), addto){
  UseMethod('add_to_all_test')
}

add_to_all_test.default <- function( x= character(), addto){
  addto[[length(addto)+1]] <- x
  addto<-addto
}
