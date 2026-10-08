#' Internal function to add up and down triangles to the plot
#'
#' @return invisible(NULL)
#'
#' @export

.addDirLegend <- function(direct.leg.x = -0.9,
                          direct.leg.y = 0.95)

{
  points(direct.leg.x,
         direct.leg.y + .025,
         pch = 2,
         cex = 2)
  text(direct.leg.x,
       direct.leg.y + .025,
       "   increasing",
       pos = 4,
       cex = 0.9)
  points(direct.leg.x,
         direct.leg.y - .025,
         pch = 6,
         cex = 2)
  text(direct.leg.x,
       direct.leg.y - .025,
       "   decreasing",
       pos = 4,
       cex = 0.9)
  invisible(NULL)
}

