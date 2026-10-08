#' Internal function to generate color palettes
#'
#' @return object of class colorRampPalette
#'
#' @export

.genXmasColors <- function(colors = NULL, rev.palette = F)
{
  if(is.null(colors)){
    if(!rev.palette) cols <- colorRampPalette(c("red", "orange", "yellow",
                                                "palegreen", "darkgreen"))
    else cols <- colorRampPalette(c("darkgreen", "palegreen", "yellow",
                                    "orange","red"))
  }
  else cols <- colorRampPalette(colors)
  cols
}
