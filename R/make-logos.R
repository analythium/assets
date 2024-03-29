library(magick)
library(rsvg)
library(jsonlite)
library(ggsci)
source("logo.R")

view_svg <- function(x, ...) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(x, f)
  image_read_svg(f, ...)
}
## https://www.brodieg.com/2019/02/09/favicons-from-the-comfort-of-your-r-session/
write_ico <- function(im, file) {
  sizes <- 8 * c(2, 3, 4, 6, 8)
  img.sizes <- sprintf("%dx%d", sizes, sizes)
  dir <- tempfile()
  dir.create(dir)
  files <- file.path(dir, sprintf("favicon-%s.png", sizes))
  img.resize <- lapply(img.sizes, image_resize, image=im)
  invisible(Map(image_write, img.resize, files))
  list.files(dir)
  imgs <- image_read(files)
  ico <- image_convert(imgs, 'ico')
  image_write(ico, file)
  unlink(dir, recursive=TRUE)
  invisible()
}
# default logo
#l <- analythium_logo("#ffef00", "#ffffff", "#000000", "#ffef00", "#000000", sw=3)
#view_svg(l, width=400, height=400)
#f <- paste0("../docs/logo/logo.svg")
#writeLines(l, f)
#im <- image_read_svg(f)
#image_write(im, paste0("../docs/logo/logo.png"))
#write_ico(im, "../docs/logo/favicon.ico")

col <- c(dark="#000000", mid="#ffd700", light="#ffffff")
ld <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=3)
ll <- analythium_logo(col[2], col[3], col[1], col[2], col[3], sw=3)
lm <- analythium_logo(col[2], col[3], col[1], col[2], col[2], sw=3)

## default (dark) stroke
f <- paste0("../docs/logo/logo.svg")
writeLines(ld, f)
im <- image_read_svg(f)
image_write(im, paste0("../docs/logo/logo.png"))
write_ico(im, paste0("../docs/logo/favicon.ico"))
## light stroke
f <- paste0("../docs/logo/logo-light.svg")
writeLines(ll, f)
im <- image_read_svg(f)
image_write(im, paste0("../docs/logo/logo-light.png"))
## medium stroke
f <- paste0("../docs/logo/logo-mid.svg")
writeLines(lm, f)
im <- image_read_svg(f)
image_write(im, paste0("../docs/logo/logo-mid.png"))


## palettes
pal <- hcl.pals("sequential")
names(pal) <- tolower(gsub("_", "", gsub("-", "", gsub(" ", "", pal))))
stopifnot(length(names(pal)) == length(unique(names(pal))))

pal2 <- c("red", "pink", "purple", "deep-purple",
  "indigo", "blue", "light-blue", "cyan", "teal", "green", "light-green",
  "lime", "yellow", "amber", "orange", "deep-orange", "brown", "grey",
  "blue-grey")
names(pal2) <- paste0("material-", pal2)
pal0 <- "default"
names(pal0) <- pal0
pal <- c(pal0, pal, pal2)
writeLines(toJSON(sort(names(pal))), paste0("../docs/logo/index.json"))

cols <- list()
for (i in seq_along(pal)) {
  cat(pal[i], "\n")
  flush.console()
  ## make a 3-color palette: dark, mid, light
  if (pal[i] == "default") {
    col <- c("#000000", "#ffef00", "#ffffff")
  } else {
    col <- if (startsWith(names(pal)[i], "material"))
      pal_material(pal[i], reverse=TRUE, n=3)(3) else hcl.colors(3, pal[i])
  }
  ## lajolla, oslo, turku luminance is reversed
  ## check luminence and reverse where needed
  rgb <- col2rgb(col[-2])
  lum <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
  if (lum[1] > lum[2])
    col <- rev(col)
  cols[[pal[i]]] <- data.frame(
    id=pal[i],
    dark=col[1], mid=col[2], light=col[3],
    stringsAsFactors=FALSE)
  ## make SVG: no stroke, dark stroke, light stroke
  ln <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=0)
  ld <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=3)
  ll <- analythium_logo(col[2], col[3], col[1], col[2], col[3], sw=3)
  lm <- analythium_logo(col[2], col[3], col[1], col[2], col[2], sw=3)
  ## write SVG and PNG files to gh pages
  j <- names(pal)[i]
  if (!dir.exists(paste0("../docs/logo/", j)))
    dir.create(paste0("../docs/logo/", j))
  ## default (dark) stroke
  f <- paste0("../docs/logo/", j, "/logo.svg")
  writeLines(ld, f)
  im <- image_read_svg(f)
  image_write(im, paste0("../docs/logo/", j, "/logo.png"))
  write_ico(im, paste0("../docs/logo/", j, "/favicon.ico"))
  ## no stroke
  if (!dir.exists(paste0("../docs/logo/", j, "/none")))
    dir.create(paste0("../docs/logo/", j, "/none"))
  f <- paste0("../docs/logo/", j, "/none/logo.svg")
  writeLines(ln, f)
  im <- image_read_svg(f)
  image_write(im, paste0("../docs/logo/", j, "/none/logo.png"))
  write_ico(im, paste0("../docs/logo/", j, "/none/favicon.ico"))
  ## light stroke
  if (!dir.exists(paste0("../docs/logo/", j, "/light")))
    dir.create(paste0("../docs/logo/", j, "/light"))
  f <- paste0("../docs/logo/", j, "/light/logo.svg")
  writeLines(ll, f)
  im <- image_read_svg(f)
  image_write(im, paste0("../docs/logo/", j, "/light/logo.png"))
  write_ico(im, paste0("../docs/logo/", j, "/light/favicon.ico"))
  ## medium stroke
  if (!dir.exists(paste0("../docs/logo/", j, "/mid")))
    dir.create(paste0("../docs/logo/", j, "/mid"))
  f <- paste0("../docs/logo/", j, "/mid/logo.svg")
  writeLines(lm, f)
  im <- image_read_svg(f)
  image_write(im, paste0("../docs/logo/", j, "/mid/logo.png"))
  write_ico(im, paste0("../docs/logo/", j, "/mid/favicon.ico"))
}

cols <- do.call(rbind, cols)
cols$id <- names(pal)
rownames(cols) <- NULL
writeLines(toJSON(cols), paste0("../docs/logo/colors.json"))

if (FALSE) {


unislug <- function(n) {
  n <- tolower(n)
  n <- gsub("%+", "-pct-", n)
  n <- gsub("\\$+", "-dollars-", n)
  n <- gsub("\\++", "-plus-", n)
  n <- gsub("_+", "-", n)
  n <- gsub("\\*+", "-star-", n)
  n <- gsub("#+", "-cnt-", n)
  n <- gsub("&+", "-and-", n)
  n <- gsub("@+", "-at-", n)
  n <- gsub("[^a-zA-Z0-9_]+", "-", n)
  n <- gsub("([A-Z][a-z])", "-\\1", n)
  n <- tolower(trimws(n))
  n <- gsub("(^-+|-+$)", "", n)
  n <- gsub("-+", "-", n)
  make.unique(n, sep = "-")
}

## default palette
PAL0 <- data.frame(
  id="default",
  package="none",
  palette="none",
  type="none",
  dark="#000000", mid="#ffef00", light="#ffffff",
  stringsAsFactors=FALSE)

## base sequential hcl palettes
PAL1 <- data.frame(
  package="base",
  palette=hcl.pals("sequential"),
  type="sequential",
  dark="", mid="", light="",
  stringsAsFactors=FALSE)
for (i in seq_len(nrow(PAL1))) {
  ## make a 3-color palette: dark, mid, light
  col <- hcl.colors(3, PAL1$palette[i])
  ## lajolla, oslo, turku luminance is reversed
  ## check luminence and reverse where needed
  rgb <- col2rgb(col[-2])
  lum <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
  if (lum[1] > lum[2])
    col <- rev(col)
  PAL1$dark[i] <- col[1L]
  PAL1$mid[i] <- col[2L]
  PAL1$light[i] <- col[3L]
}

## paletteer sequential palettes
#install.packages("paletteer")
library(paletteer)

PAL2 <- palettes_c_names[palettes_c_names$type=="sequential",]
PAL2 <- data.frame(PAL2, dark="", mid="", light="", stringsAsFactors=FALSE)
for (i in seq_len(nrow(PAL2))) {
  col <- as.character(
    paletteer_c(paste0(PAL2$package[i], "::", PAL2$palette[i]), 3))
  rgb <- col2rgb(col)
  lum <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
  col <- col[order(lum)]
  PAL2$dark[i] <- substr(col[1L], 1, 7)
  PAL2$mid[i] <- substr(col[2L], 1, 7)
  PAL2$light[i] <- substr(col[3L], 1, 7)
}


PAL3 <- palettes_d_names[palettes_d_names$type=="sequential",]
PAL3 <- PAL3[PAL3$length >= 3,]
PAL3$length <- NULL
PAL3 <- data.frame(PAL3, dark="", mid="", light="", stringsAsFactors=FALSE)
for (i in seq_len(nrow(PAL3))) {
  col <- as.character(
    paletteer_d(paste0(PAL3$package[i], "::", PAL3$palette[i]), 3))
  rgb <- col2rgb(col)
  lum <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
  col <- col[order(lum)]
  PAL3$dark[i] <- substr(col[1L], 1, 7)
  PAL3$mid[i] <- substr(col[2L], 1, 7)
  PAL3$light[i] <- substr(col[3L], 1, 7)
}

PAL <- rbind(PAL1, PAL2, PAL3)
PAL$id <- unislug(paste0(PAL$package, "-", PAL$palette))
PAL <- rbind(PAL0, PAL[,colnames(PAL0)])
PAL <- PAL[PAL$dark != PAL$mid,]
PAL <- PAL[PAL$light != PAL$mid,]
PAL <- PAL[PAL$dark != PAL$light,]


for (i in seq_len(nrow(PAL))) {
  j <- PAL[i,"id"]
  cat(j, "\n")
  flush.console()
  col <- c(PAL[i,"dark"], PAL[i,"mid"], PAL[i,"light"])
  ld <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=3)
  f <- paste0("x/", j, ".svg")
  writeLines(ld, f)
  image_write(image_read_svg(f), paste0("x/", j, ".png"))
  unlink(f)
}

}


## Material colors

an_palette <- structure(
  c("#ffcdd2", "#f8bbd0", "#e1bee7", "#d1c4e9", "#c5cae9",
  "#bbdefb", "#b3e5fc", "#b2ebf2", "#b2dfdb", "#c8e6c9", "#dcedc8",
  "#f0f4c3", "#fff9c4", "#ffecb3", "#ffe0b2", "#ffccbc", "#d7ccc8",
  "#f5f5f5", "#cfd8dc", "#f44336", "#e91e63", "#9c27b0", "#673ab7",
  "#3f51b5", "#2196f3", "#03a9f4", "#00bcd4", "#009688", "#4caf50",
  "#8bc34a", "#cddc39", "#ffeb3b", "#ffc107", "#ff9800", "#ff5722",
  "#795548", "#9e9e9e", "#607d8b", "#b71c1c", "#880e4f", "#4a148c",
  "#311b92", "#1a237e", "#0d47a1", "#01579b", "#006064", "#004d40",
  "#1b5e20", "#33691e", "#827717", "#f57f17", "#ff6f00", "#e65100",
  "#bf360c", "#3e2723", "#212121", "#263238"), .Dim = c(19L, 3L
  ), .Dimnames = list(c("red", "pink", "purple", "deeppurple",
  "indigo", "blue", "lightblue", "cyan", "teal", "green", "lightgreen",
  "lime", "yellow", "amber", "orange", "deeporange", "brown", "grey",
  "bluegrey"), c("light", "mid", "dark")))

show_pal <- function() {
  op <- par(mar=c(4,8,4,4))
  on.exit(par(op))
  image(
    1:ncol(an_palette),
    1:nrow(an_palette),
    t(array(1:length(an_palette), dim(an_palette))),
    col=as.character(an_palette[nrow(an_palette):1,]), axes=FALSE, ann=FALSE)
  axis(2, 1:nrow(an_palette), rev(rownames(an_palette)), lwd=0, las=1)
  axis(3, 1:ncol(an_palette), colnames(an_palette), lwd=0, las=1)
  text(as.numeric(col(an_palette)), as.numeric(row(an_palette)),
    as.character(an_palette[nrow(an_palette):1,]),
    col=c("darkgrey", "grey", "white")[as.numeric(col(an_palette))], cex=0.8)
  invisible(an_palette)
}
show_pal()

bc <- list(dark="#616161", mid="#ffef00", light="#ffffff")

bc <- list(dark="#000000", mid="#ffd700", light="#ffffff")

l <- analythium_logo(bc$mid, bc$light, bc$dark, bc$mid, bc$dark, sw=4)
view_svg(l, width=400, height=400)

f <- tempfile()
writeLines(l, f)
im <- image_read_svg(f)
unlink(f)
image_write(im, "~/Desktop/logo-gr-yl.png")



## hosting shiny color palette

col <- c(dark="#5544b6", mid="#20f4a2", light="#ffffff")
ld <- analythium_logo(col[2], col[3], col[1], col[2], col[1], sw=3)
ll <- analythium_logo(col[2], col[3], col[1], col[2], col[3], sw=3)
lm <- analythium_logo(col[2], col[3], col[1], col[2], col[2], sw=3)

writeLines(ld, "~/Desktop/asinc/shiny-hosting/asinc-pugr.svg")
im <- image_read_svg("~/Desktop/asinc/shiny-hosting/asinc-pugr.svg")
image_write(im, "~/Desktop/asinc/shiny-hosting/asinc-pugr.png")

