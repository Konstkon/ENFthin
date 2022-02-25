
#'
#'
#'
#' @title ENFtrees Class
#' @description A class "ENFtrees" is represented by two slots
#' \itemize{
#' \item Basepoints: A ppp object for the base points
#' \item Endpoints: A ppp object for the end points
#' }

#' @export
#'

setClass("ENFtrees", slots=
           c(Basepoints = "ppp", Endpoints="ppp"))
