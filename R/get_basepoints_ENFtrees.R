
#'
#'
#'
#' @title Extract base points from an ENFtree object
#' @description Extract base points from an ENFtree object and obtain those as ppp objects
#' @param{ENFtree}  An "ENFtree" class object with two slots (Basepoints and Endpoints). Each slot is a ppp object
#' @usage
#' get.basepoints_ENFtrees(ENFtree)
#'
#' @return A ppp object of the base points from an ENFtree object
#' @export
#'
#' @examples
#' get.basepoints_ENFtrees(X_thin[[1]])
#'@seealso \code{\link{get.endpoints_ENFtrees}}


get.basepoints_ENFtrees <- function(ENFtree){
  ENFtree@Basepoints
}
