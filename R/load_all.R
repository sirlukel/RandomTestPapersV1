#' Will load the names of all the tests currently stored in 'tests.csv'
#'
#' @return A list of the names of all the currently saved tests
#' @export

load_all_tests <-function(){
  UseMethod('load_all_tests')
}

#'@export
load_all_tests.default <- function()
{
  if(file.exists("tests.csv")){
    temp <- read.csv(file = "tests.csv", header = FALSE)
    all_ts <- list()
    for(i in 1:length(temp[1,])){
      name <- temp[1,i]
      qs <- c()
      for(j in 2:length(temp[,i])){
        if(is.na(temp[j,i])){
          break
        }else{
          qs<- c(qs,temp[j,i])
        }
        all_ts[[i]] <- c(name,qs)
      }
    }
    all_ts <- all_ts
  }
  else{
    all_ts <- NULL
  }
}

#' Will load all the questions currently saved in 'questions.csv'
#'
#' @return A data.frame of all the currently saved questions
#' @export

load_all_questions <-function()
{
  UseMethod('load_all_questions')
}

#'@export

load_all_questions.default <- function()
{
  if(file.exists("questions.csv"))
  {
    all_questions <- read.csv(file = "questions.csv", header = TRUE)
  }else
  {
    all_questions <- NULL
  }
}


