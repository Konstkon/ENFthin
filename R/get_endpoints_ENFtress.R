
#'
#'
#'
#' @title Extract end points from an ENFtree object
#' @description Extract end points from an ENFtree object and obtain those as ppp objects
#' @param{ENFtree}  An "ENFtree" class object with two slots (Basepoints and Endpoints). Each slot is a ppp object
#' @usage
#' get.endpoints_ENFtrees(ENFtree)
#'
#' @return A ppp object of the end points from an ENFtree object
#' @export
#'
#' @examples
#' get.endpoints_ENFtrees(X_thin[[1]])
#' @seealso \code{\link{get.basepoints_ENFtrees}}

get.endpoints_ENFtrees <- function(ENFtree){
ENFtree@Endpoints
}
