library(tidyverse)
library(magick)

# functions from https://github.com/djnavarro/ashtree
# helper functions --------------------------------------------------------

radians <- function(degree) {
  2 * pi * degree / 360
}

adjust_scale <- function(s, scales) {
  s * sample(x = scales, size = length(s), replace = TRUE)
}

adjust_angle <- function(a, angles) {
  a + sample(x = angles, size = length(a), replace = TRUE)
}

adjust_x <- function(x, scale, angle) {
  x + scale * cos(radians(angle))
}

adjust_y <- function(y, scale, angle) {
  y + scale * sin(radians(angle))
}

# ashtree functions -------------------------------------------------------

grow_sapling <- function() {

  sapling <- tibble(
    old_x = 0,  # sapling grows from the origin
    old_y = 0,  # sapling grows from the origin
    new_x = 0,  # sapling doesn't grow horizontally
    new_y = 1,  # sapling does grow vertically
    angle = 90, # angle from horizontal is 90 degrees
    scale = 1,  # length of the sapling is 1
  )
  return(sapling)
}

grow_from <- function(tips, settings) {

  # read off the relevant settings
  all_scales <- settings$scales
  all_angles <- settings$angles

  # mutate the tips tibble
  new_growth <- tips %>%
    mutate(
      scale = adjust_scale(scale, all_scales),      # change segment length
      angle = adjust_angle(angle, all_angles),      # change segment angle
      old_x = new_x,                                # begin where last seg ended
      old_y = new_y,                                # begin where last seg ended
      new_x = adjust_x(old_x, scale, angle),        # end where this seg ends!
      new_y = adjust_y(old_y, scale, angle)         # end where this seg ends!
    )
  return(new_growth)
}

grow_many <- function(tips, settings) {

  # read off the relevant settings
  splits <- settings$splits

  new_tips <- map_dfr(1:splits, ~grow_from(tips, settings))
  return(new_tips)
}

grow_tree <- function(settings) {

  # read off the relevant settings
  cycles <- settings$cycles

  # initialise
  tips <- grow_sapling()

  # grow tree in a loop
  tree <- accumulate(2:cycles, ~grow_many(.x, settings), .init = tips)
  tree <- bind_rows(tree)

  return(tree)
}

draw_tree <- function(tree) {

  pic <- ggplot(
    data = tree,
    mapping = aes(
      x = old_x,
      y = old_y,
      xend = new_x,
      yend = new_y
    )
  ) +
    geom_segment(show.legend = FALSE) +
    theme_void() +
    coord_equal()

  return(pic)
}

# generate a tree ------------------------------------

settings <- list(
  seed = 1,
  cycles = 14,
  splits = 2,
  scales = c(.5, .8, .9),
  angles = c(-25, -10, -5, 5, 10, 20, 35)
)

set.seed(settings$seed)

tree <- grow_tree(settings)

# simple base plot to draw the tree ----------------------

base_plot <- function(tree, subset=NULL, xlim=NULL, ylim=NULL, add=FALSE, ...) {
  if (is.null(xlim))
    xlim <- c(range(tree$old_x, tree$new_x))
  if (is.null(ylim))
    ylim <- c(range(tree$old_y, tree$new_y))
  if (!add)
    plot(0, type="n", axes=FALSE, ann=FALSE, xaxs = "i", yaxs = "i", asp=1,
      xlim=xlim, ylim=ylim)
  segments(tree$old_x, tree$old_y, tree$new_x, tree$new_y, ...)
  invisible(tree)
}

# save images ------------------------------

k <- 20 # number of images to save
files <- sprintf("image-%s.png", 0:(2*k))

# background color
bg <- 1
# color palette to display along the branches
col <- colorRampPalette(c("darkgrey", "grey", "lightgrey", "orange"))(k)

lwd <- rep(0.5, k) # line widths
br <- exp(seq(0, log(nrow(tree)), length.out = k+2)) # breaks
br <- round(br[-1])
br[1] <- 0
ii <- as.integer(cut(seq_len(nrow(tree)), br))

# 1st image is background only, i.e. no tree
png(files[1])
op <- par(mar=c(0,0,0,0), bg=bg)
base_plot(tree, col=bg)
par(op)
dev.off()

# save images for the growing tree:
# draw branches with orange color at the tips
for (j in 1:k) {
  png(files[j+1])
  op <- par(mar=c(0,0,0,0), bg=bg)
  base_plot(tree, col=bg)
  for (i in 1:j) {
    .col <- rev(rev(col)[1:j])
    .lwd <- rev(rev(lwd)[1:j])
    base_plot(tree[ii==i,], col=.col[i], add=TRUE, lwd=.lwd[i])
  }
  par(op)
  dev.off()
}

# save images for fade-out:
# all the orange fades away to leave a grey tree behind
for (j in 1:k) {
  png(files[j+k+1])
  op <- par(mar=c(0,0,0,0), bg=bg)
  base_plot(tree, col=bg)
  .col <- c(rep(col[1], j), rev(rev(col)[-(1:j)]))
  for (i in 1:k) {
    base_plot(tree[ii==i,], col=.col[i], add=TRUE, lwd=lwd[i])
  }
  par(op)
  dev.off()
}

img <- image_read(files) # read in images
img3 <- image_animate(img, 10, loop=1, optimize=TRUE) # animate
image_write(img3, "tree.gif") # save gif
unlink(files) # clean up png files

