##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title check_NAs
##' @param DF
##' @return
##' @author gorkang
##' @export
check_NAs <- function(DF) {
  
  DF_CHECK_NA = DF %>% 
    select(ends_with("_NA")) 
  
  if (ncol(DF_CHECK_NA) == 2) {
  
    # Check we have the same number of NAs in RAW and PROC DFs
    if (!identical(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])) stop("Missing data when processing RAW responses")
    
    # [REWIEW]: Other ways to check equality
    # all(DF_CHECK_NA[1] == DF_CHECK_NA[2])
    # all.equal(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])
    
  } else {
    
    cat(crayon::blue("\n  - Can't perform NA check, DF does not have RAW_NA and PROC_NA columns\n"))
    
  }
  
  
}