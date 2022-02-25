#'
#'
#'
#' @title Create a pp3 object
#' @description Creates a three dimensional marked point pattern with marks "Tree" and "type" from a dataframe.
#' @param{df}  A dataframe with columns
#' \itemize{
#' \item $X : X coordinate
#' \item $Y : Y coordinate
#' \item $Z : Z coordinate
#' \item $Tree : An integer denoting the tree id.
#' }
#' @param{type} The type of point. If type = 1 then it is a base point and if type = 0 then it is an end point.
#' @param{use.marks} Whether to add marks to the pattern or not. Defauls=TRUE.
#' @return A three-dimensional point pattern (object of class "pp3") with marks Tree and type.
#' @export
#'
#' @examples
#' create_pp3_from_df(realbpdf[[1]],1)

create_pp3_from_df <- function( df,
                                type,
                                use.marks =TRUE){
  if(class(df)=="data.frame"){

    res = pp3(df$X,df$Y,df$Z,box3(c(0,430),c(-330,0),c( min(df$Z)-5, max(df$Z)+55)))

  }
else{
  res = lapply( df, function(x)
    pp3(x$X,x$Y,x$Z,box3(c(0,430),c(-330,0),c( min(x$Z)-5, max(x$Z)+5))))
}
  if(use.marks == TRUE){
  res = lapply(seq_along(df),function(x) setmarks(res[[x]],
         data.frame(Tree=df[[x]]$Tree,
                    type = type*rep(1,length(res[[x]]$data$z))
                    )
         )
  )
  }
  res
}
