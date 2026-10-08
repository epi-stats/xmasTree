#' Internal function to plot the grid
#'
#' @return invisible(NULL)

.plotGrid <- function(mid = mid,
                      xscale = xscale,
                      yscale = yscale,
                      zscale = zscale,
                      grid.col = "gray",
                      lab.dig.x = 1,
                      lab.dig.y = 1,
                      lab.dig.z = 1,
                      lab.cex = 0.7,
                      lab.col = "gray20")
{
  par(mar = rep(0, 4))
  plot(
    NA,
    NA,
    xlim = c(-1, 1),
    ylim = 0:1,
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = ""
  )
  lines(c(0, 0), c(mid, 1))
  lines(c(-1, 0), c(mid / 2, mid))
  lines(c(0, 1), c(mid, mid / 2))
  for (i in 0:3) {
    lines(c(-1 + i / 4, 0 + i / 4), c(mid / 2 + (0.5 * mid) * (i / 4), 0 + (0.5 *
                                                                              mid) * (i / 4)), col = grid.col)
    lines(c(0 - i / 4, 1 - i / 4), c(0 + (0.5 * mid) * (i / 4), mid / 2 +
                                       (0.5 * mid) * (i / 4)), col = grid.col)
  }
  text(
    c(0:4 / 4) + 0.05,
    c((0.5 * mid) * (0:4 / 4)) - 0.01,
    round(seq(xscale[1], xscale[2], length.out = 5)[5:1], lab.dig.x),
    col = lab.col,
    cex = lab.cex
  )
  text(
    c(0 - 0:4 / 4) - 0.05,
    c((0.5 * mid) * (0:4 / 4)) - 0.01,
    round(seq(yscale[1], yscale[2], length.out = 5)[5:1], lab.dig.y),
    col = lab.col,
    cex = lab.cex
  )
  text(
    rep(0, 5) + 0.05,
    seq(mid, 1, length.out = 5),
    round(seq(zscale[1], zscale[2], length.out = 5)[1:5], lab.dig.z),
    col = lab.col,
    cex = lab.cex
  )
  invisible(NULL)
}
