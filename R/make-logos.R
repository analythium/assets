library(magick)
library(rsvg)
library(jsonlite)
source("logo.R")

view_svg <- function(x, ...) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(x, f)
  image_read_svg(f, ...)
}
# default logo
l <- analythium_logo("#ffef00", "#ffffff", "#000000", "#ffef00", "#000000", sw=3)
view_svg(l, width=400, height=400)
f <- paste0("../docs/logo/logo.svg")
writeLines(l, f)
image_write(image_read_svg(f), paste0("../docs/logo/logo.png"))

## palettes
pal <- hcl.pals("sequential")
names(pal) <- tolower(gsub("_", "", gsub("-", "", gsub(" ", "", pal))))
stopifnot(length(names(pal)) == length(unique(names(pal))))
writeLines(toJSON(sort(names(pal))), paste0("../docs/logo/index.json"))

for (i in seq_along(pal)) {
  cat(pal[i], "\n")
  flush.console()
  ## make a 3-color palette: dark, mid, light
  col <- hcl.colors(3, pal[i])
  ## lajolla, oslo, turku luminance is reversed
  ## check luminence and reverse where needed
  rgb <- col2rgb(col[-2])
  lum <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
  if (lum[1] > lum[2])
    col <- rev(col)
  ## make SVG: no stroke, dark stroke, light stroke
  ln <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=0)
  ld <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=3)
  ll <- analythium_logo(col[2], col[3], col[1], col[2], col[3], sw=3)
  ## write SVG and PNG files to gh pages
  j <- names(pal)[i]
  if (!dir.exists(paste0("../docs/logo/", j)))
    dir.create(paste0("../docs/logo/", j))
  ## default (dark) stroke
  f <- paste0("../docs/logo/", j, "/logo.svg")
  writeLines(ld, f)
  image_write(image_read_svg(f), paste0("../docs/logo/", j, "/logo.png"))
  ## no stroke
  if (!dir.exists(paste0("../docs/logo/", j, "/none")))
    dir.create(paste0("../docs/logo/", j, "/none"))
  f <- paste0("../docs/logo/", j, "/none/logo.svg")
  writeLines(ln, f)
  image_write(image_read_svg(f), paste0("../docs/logo/", j, "/none/logo.png"))
  ## light stroke
  if (!dir.exists(paste0("../docs/logo/", j, "/light")))
    dir.create(paste0("../docs/logo/", j, "/light"))
  f <- paste0("../docs/logo/", j, "/light/logo.svg")
  writeLines(ll, f)
  image_write(image_read_svg(f), paste0("../docs/logo/", j, "/light/logo.png"))
}
