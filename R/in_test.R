#' Will get a list of all tests that a question is contained in
#'
#' @param x The name of the question
#' @param all_tests The list of all currently saved tests
#'
#' @return A list of the names of tests that the question is contained in
#' @export

in_test <-function(x = character(),all_tests = list()){
  UseMethod('in_test')
}

#'@export

in_test.default <-function(x = character(),all_tests = list()){
  t <- c()
  if(length(all_tests) == 0){
    t<-t
  }else{
    for(j in x){
      for(i in 1:length(all_tests)){
        if (j %in% all_tests[[i]] && !(all_tests[[i]][1] %in% t))
        {
          t <- c(t, all_tests[[i]][1])
        }
      }
    }
    t<-t
  }
}
