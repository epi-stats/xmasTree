#' Function to plot multidimensional data
#'
#' Function to plot highly multidemensional data.
#' Variables x, y, and z1 are plotted similar to a 3d scatter plot
#' except that in z dimension an arrow is plotted.
#' The arrow head has either a fixed size or - if z1 and z2 ar both provided -
#' the head size ranges from z2 to z1.
#' The heads are showing either up or down if z1-z2 is pos or negative.
#' The head can have additional color codes c1 and c2.
#' The plot can be quite complex. Best way is to check the examples.
#' If you have even more variables to plot have a look at
#'  \link[doubleTree]{xmasTree}
#'
#' @param x numeric variable, NA not allowed
#' @param y numeric variable, NA not allowed
#' @param z1 numeric variable specifies the tip of the arrow head
#' @param z2 if sepecifies the base of the arrow head (default NULL)
#' @param c1 variable for the color of the left part of the arrow head (dafault = z1)
#' @param c2 variable for the color of the right part of the arrow head (dafault = c1)
#' @param mid specifying the ration of the base grid to the Z expansion (default = 1/3)
#' @param xscale Manual scale for x, usually you should use the default (auto)
#' @param xar Width of the arrow base (default = 0.03)
#' @param lab.dig lab.dig.x ... number of digits for the x/y grid annotation
#' @param xlab ylab zlab axis labels (similar to base plot)
#' @param direct.leg Should arrow directions shown as legend (default = F)
#' @param arrow.leg Should an arrow color legend be added (default= F)
#' @param rev.palette Should the default color palette reverted that red is low and green = high?
#' @param c1.col color palette for c1 (default from green to red)
#' @param c2.col color palette for c2 (default from green to red)
#'
#' @return invisible(NULL)
#'
#' @examples
#' xmasTree(x=iris$Sepal.Width,
#'          y=iris$Sepal.Length,
#'          z1=iris$Petal.Width,
#'          z2=iris$Petal.Length,
#'          arrow.leg = T,
#'          xlab= "Sepal.Width",
#'          ylab = "Sepal.Length",
#'          zlab="Petal difference")
#'
#' xmasTree(x=iris$Sepal.Width,
#'          y=iris$Sepal.Length,
#'          z1=iris$Petal.Width,
#'          z2=iris$Petal.Length,
#'          c1 =as.numeric(iris$Species))
#'
#' df <- data.frame(x = cos(seq(0, pi*1.3, length.out=20)),
#'                  y = sin(seq(0, pi*1.3, length.out=20)),
#'                  z1 = 1:20,
#'                  z2 = seq(15, 5, length.out=20),
#'                  c1 = rep(1:3, 7)[1:20],
#'                  c2 = rep(4:1, 6)[1:20],
#'                  c3 = rep(0:1, each = 10),
#'                  c4 = 1:20)
#'  xmasTree(x=df$x,
#'           y=df$y,
#'           z1=df$z1,
#'           z2=df$z2,
#'           c1=df$c1,
#'           c2=df$c2,
#'           c1.col = c("blue","cornflowerblue","orchid"),
#'           c2.col = c("orange","red","darkred"),
#'           arrow.leg = T)
#'
#' @export



xmasTree <- function(x,
                     y,
                     z1,
                     z2 = NULL,
                     c1 = NULL,
                     c2 = NULL,
                     mid = 1 / 3,
                     xscale = NULL,
                     yscale = NULL,
                     zscale = NULL,
                     xar = 0.03,
                     grid.col = "gray",
                     lab.col = "gray20",
                     lab.cex = 0.7,
                     lab.dig = 1,
                     lab.dig.x = lab.dig,
                     lab.dig.y = lab.dig,
                     lab.dig.z = lab.dig,
                     vert.lines = T,
                     vert.wide = 1,
                     vert.anot = F,
                     vert.col = "black",
                     xlab = "X",
                     ylab = "Y",
                     zlab = "Z",
                     direct.leg = F,
                     direct.leg.x = -0.9,
                     direct.leg.y = 0.95,
                     arrow.leg = F,
                     arrow.txt = "",
                     rev.palette = F,
                     c1.col = NULL,
                     c2.col = NULL) {
  if (any(is.na(c(x, y, z1, z2))))
    stop("NA not allowed in x,y,z1")
  old_mar <- par("mar")
  par(mar = rep(0, 4))

  if (is.null(z2)) {
    z2 <- z1
    z1 <- z1 - diff(range(z1, na.rm = T)) * 0.5
  }

  #generate colors
  if (is.null(c1))
    c1 <- z1
  if (is.null(c2))
    c2 <- c1

  col01 <- .genXmasColors(colors = c1.col, rev.palette = rev.palette)(101)
  col1 <- col01[round(((c1) - (min(c1))) / diff(range(c1)) * 100) + 1]
  col02 <- .genXmasColors(colors = c2.col, rev.palette = rev.palette)(101)
  col2 <- col02[round(((c2) - (min(c2))) / diff(range(c2)) * 100) + 1]


  # generate scales if not provided
  if (is.null(xscale))
    xscale <- range(x) + c(-1, 1) * diff(range(x)) * 0.05
  if (is.null(yscale))
    yscale <- range(y) + c(-1, 1) * diff(range(y)) * 0.05
  if (is.null(zscale))
    zscale <- range(c(z1, z2)) + c(-1, 1) * diff(range(c(z1, z2))) * 0.05

  # plot grid
  .plotGrid(
    mid = mid,
    xscale = xscale,
    yscale = yscale,
    zscale = zscale,
    grid.col = grid.col,
    lab.dig.x = lab.dig.x,
    lab.dig.y = lab.dig.y,
    lab.dig.z = lab.dig.z,
    lab.cex = lab.cex,
    lab.col = lab.col
  )

  # calculate, direction, x, and y0, zmin, zmax
  maxz <- ifelse(z1 > z2, z1, z2)
  minz <- ifelse(z1 > z2, z2, z1)
  ups <- which(z2 > z1)
  downs <- which(z2 < z1)
  xloc <- yshift <- rep(NA, length(maxz))
  for (i in 1:length(maxz)) {
    xloc[i] <- (xscale[2] - x[i]) / (xscale[2] - xscale[1]) -
      (yscale[2] - y[i]) / (yscale[2] - yscale[1])
    yshift[i] <- ((xscale[2] - x[i]) / (xscale[2] - xscale[1]) +
                    (yscale[2] - y[i]) / (yscale[2] - yscale[1])) * mid /
      2
  }
  zhigh <- (yshift + (maxz - zscale[1]) / (zscale[2] - zscale[1]) * (1 -
                                                                       mid))
  zlow <- (yshift + (minz - zscale[1]) / (zscale[2] - zscale[1]) * (1 -
                                                                      mid))
  zmean <- (zhigh + zlow) / 2
  # lines below just for debugging
  #print(zlow)
  #print(zhigh)

  # print vertical lines
  if (vert.lines) {
    for (i in 1:length(maxz)) {
      lines(rep(xloc[i], 2),
            c(
              yshift[i],
              yshift[i] + (maxz[i] - zscale[1]) / (zscale[2] - zscale[1]) * (1 - mid)
            ),
            lwd = vert.wide,
            col = vert.col)
    }
    if (vert.anot)
      text(xloc, yshift, paste0(round(y, lab.dig.y), ",", round(x, lab.dig.x)), cex =
             0.5)
    # lines below just for debugging
    # text(xloc, yshift + (maxz-zscale[1])/(zscale[2]-zscale[1]) * (1-mid),
    #     round(maxz,lab.dig.z), cex=0.5)
  }

  # print up triangles
  for (i in ups) {
    polygon(c(xloc[i], xloc[i], xloc[i] - xar), +c(zlow[i], zhigh[i], zlow[i]), col =
              col1[i])
    polygon(c(xloc[i], xloc[i], xloc[i] + xar), +c(zlow[i], zhigh[i], zlow[i]), col =
              col2[i])
   }
  for (i in downs) {
    polygon(c(xloc[i], xloc[i], xloc[i] - xar), +c(zhigh[i], zlow[i], zhigh[i]), col =
              col1[i])
    polygon(c(xloc[i], xloc[i], xloc[i] + xar), +c(zhigh[i], zlow[i], zhigh[i]), col =
              col2[i])
  }
  text(0.55,
       .02,
       xlab,
       srt = 15,
       cex = 0.9,
       col = lab.col)
  text(-0.55,
       .02,
       ylab,
       srt = -15,
       cex = 0.9,
       col = lab.col)
  text(0.15, .97, zlab , cex = 0.9, col = lab.col)

  if (direct.leg)
    .addDirLegend(direct.leg.x = direct.leg.x, direct.leg.y = direct.leg.y)

  if (arrow.leg) {
    xar <- xar * 1.4
    polygon(c(0.8, 0.8, 0.8 - xar), +c(1, 0.9, 1), col = col01[101])
    polygon(c(0.8, 0.8, 0.8 - xar / 3 * 2), +c(0.966, 0.9, 0.966), col =
              col01[51])
    polygon(c(0.8, 0.8, 0.8 - xar / 3), +c(0.933, 0.9, 0.933), col = col01[1])
    polygon(c(0.8, 0.8, 0.8 + xar), +c(1, 0.9, 1), col = col02[101])
    polygon(c(0.8, 0.8, 0.8 + xar / 3 * 2), +c(0.966, 0.9, 0.966), col =
              col02[51])
    polygon(c(0.8, 0.8, 0.8 + xar / 3), +c(0.933, 0.9, 0.933), col = col02[1])
    text(
      x = rep(0.8, 4),
      y = c(0.83, 0.93, 0.83, 0.93),
      arrow.txt,
      pos = c(2, 2, 4, 4),
      cex = 0.8
    )
    text(
      x = rep(0.8, 6),
      y = c(0.91, 0.95, 0.99, 0.91, 0.95, 0.99),
      round(c(
        min(c1),
        (min(c1) + max(c1)) / 2,
        max(c1),
        min(c2),
        (min(c2) + max(c2)) / 2,
        max(c2)
      ), 2),
      pos = c(2, 2, 2, 4, 4, 4),
      cex = 0.6,
      col = lab.col
    )
  }
  par(mar = old_mar)
  invisible(NULL)
}
