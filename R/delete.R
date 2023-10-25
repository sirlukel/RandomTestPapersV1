#' Will delete a question or a test from the collection of all questions or tests
#'
#' @param x The name of the question or test that should be deleted
#' @param removefrom The collection of either questions or tests that the items should be deleted
#'
#' @return The collection of questions or tests with the named item deleted
#' @export

delete <- function(x, removefrom){
  UseMethod('delete')
}

#'@export

delete.character <- function(x = character(), removefrom){
  if(is.data.frame(removefrom)){
    for(i in x){
      removefrom <- removefrom[!(removefrom$name == i),]

    }
    removefrom <- removefrom
  }
  else{
    for(i in x){
      removefrom <- removefrom[!sapply(removefrom, '[[', 1) == i]
    }
    removefrom <- removefrom
  }
}
