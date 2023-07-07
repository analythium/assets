fun <- function(n=100, rate = 2, alpha=0.5) {
    op <- par(mar=c(0,0,0,0))
    on.exit(par(op))
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type="n")
    rect(-1,-1,2,2, 
        col = apply(col2rgb(sample(colors(), 1, replace = TRUE)), 2, \(x) {
            rgb(x[1], x[2], x[3], alpha=0.5*255, maxColorValue = 255)
        }), 
        border = NA)
    x0 <- runif(n, -1, 2)
    y0 <- runif(n, -1, 2)
    dx <- rexp(n, rate)
    dy <- rexp(n, rate)
    rect(xleft=x0, 
        ybottom=y0, 
        xright=x0+dx, 
        ytop=y0+dy, 
        col = apply(col2rgb(sample(colors(), n, replace = TRUE)), 2, \(x) {
            rgb(x[1], x[2], x[3], alpha=alpha*255, maxColorValue = 255)
        }), 
        border = NA)
}

set.seed(0)
for (i in 1:99) {
    png(paste0("docs/random/app-", if (i < 10) "0" else "", i, ".png"), 200, 200)
    fun()
    dev.off()
}



fun2 <- function(n=200, rate = 0.05, alpha=0.5) {
    op <- par(mar=c(0,0,0,0))
    on.exit(par(op))
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type="n")
    rect(-1,-1,2,2, 
        col = apply(col2rgb(sample(colors(), 1, replace = TRUE)), 2, \(x) {
            rgb(x[1], x[2], x[3], alpha=0.5*255, maxColorValue = 255)
        }), 
        border = NA)
    x0 <- runif(n, -1, 2)
    y0 <- runif(n, -1, 2)
    r <- rexp(n, rate)
    cl <- apply(col2rgb(sample(colors(), n, replace = TRUE)), 2, \(x) {
            rgb(x[1], x[2], x[3], alpha=alpha*255, maxColorValue = 255)
        })
    pch <- sample(21:25, 1)
    points(x0, y0, 
        pch=pch, cex=r,
        col = cl, bg=cl)
}

set.seed(0)
for (i in 1:99) {
    png(paste0("docs/random/app-", if (i < 10) "0" else "", i, ".png"), 200, 200)
    fun2()
    dev.off()
}

# https://en.wikipedia.org/wiki/Identicon
# https://github.blog/2013-08-14-identicons/
# generative avatar images
# 2^15 * 16^3 = 134217728 (134M)
fun3 <- function(string, n=5, bg="#F8F8F8", dark=1) {

    if (missing(string))
        string <- paste0(sample(c(letters, 0:9), sample(1:20, 1), replace=TRUE), collapse="")

    hex2int <- function(hash) {
        hash <- sapply(seq_len(nchar(hash)), function(z) substr(hash, z, z))
        sapply(hash, function(z) which(c(0:9, letters[1:6]) == z))
    }

    pad <- 0.5
    # bg <- "#F8F8F8" # "grey"
    # n <- 5
    sha256Hash <- digest::digest(enc2utf8(string), algo="sha256", serialize=FALSE)
    # sha256Numbers <- gsub('\\D', '', sha256Hash)
    # hash <- substr(sha256Numbers, 1, 16)
    hash <- hex2int(sha256Hash)

    if (n %% 2 != 1)
        stop("n must be an odd number.")
    if (n < 5)
        stop("n must be >= 3.")

    k <- 0
    m <- matrix(0, n, n)
    for (i in 1:n) {
        for (j in 1:ceiling(n/2)) {
            k <- k + 1
            m[i,j] <- ifelse(hash[k] > 7, 1, 0)
        }
    }
    for (j in (ceiling(n/2)+1):n) {
        m[,j] <- m[,n-j+1]
    }
    d <- data.frame(row=rep(1:n, n), col=rep(1:n, each=n), value=as.integer(m))
    d <- d[d$value > 0,]

    dark <- pmin(2, pmax(0, dark))
    col <- rgb(hash[k+1]-1, hash[k+2]-1, hash[k+3]-1, maxColorValue=15+round(dark*15))

    op <- par(mar=c(0,0,0,0))
    on.exit(par(op))
    plot(0, xlim=c(-pad,n+pad), ylim=c(-pad,n+pad), ann=FALSE, axes=FALSE, type="n")
    rect(-1,-1,n+1,n+1, col = bg, border = NA)
    for (i in seq_len(nrow(d))) {
        rect(d$col[i]-1,d$row[i]-1,d$col[i],d$row[i], col = col, border = NA)
    }
}

set.seed(0)
for (i in 1:99) {
    png(paste0("docs/random/avatar-", if (i < 10) "0" else "", i, ".png"), 200, 200)
    fun3()
    dev.off()
}

# https://github.com/laurentpayot/minidenticons
# https://laurentpayot.github.io/minidenticons/

fun4 <- function(string, saturation=0.95, lightness=0.45) {

    if (missing(string))
        string <- paste0(sample(c(letters, 0:9), sample(1:20, 1), replace=TRUE), collapse="")

    hex2int <- function(hash) {
        hash <- sapply(seq_len(nchar(hash)), function(z) substr(hash, z, z))
        sapply(hash, function(z) which(c(0:9, letters[1:6]) == z))
    }

    sha256Hash <- digest::digest(enc2utf8(string), algo="sha256", serialize=FALSE)
    hash <- hex2int(sha256Hash)

    n <- 5
    # if (n %% 2 != 1)
    #     stop("n must be an odd number.")
    # if (n < 5)
    #     stop("n must be >= 5.")

    k <- 0
    m <- matrix(0, n, n)
    for (i in 1:n) {
        for (j in 1:ceiling(n/2)) {
            k <- k + 1
            if (k > length(hash))
                stop("n too large.")
            m[i,j] <- ifelse(hash[k] > 7, 1, 0)
        }
    }
    for (j in (ceiling(n/2)+1):n) {
        m[,j] <- m[,n-j+1]
    }
    d <- data.frame(row=rep(1:n, n), col=rep(1:n, each=n), value=as.integer(m))
    d <- d[d$value > 0,]

    if (k+1 > length(hash))
        stop("n too large.")
    hue <- as.character(round(hash[k+1] * (360 / 15)))
    saturation <- as.character(pmin(100, pmax(0, round(saturation * 100))))
    lightness <- as.character(pmin(100, pmax(0, round(lightness * 100))))

    # svg <- paste0("<svg viewBox=\"-1.5 -1.5 8 8\" xmlns=\"http://www.w3.org/2000/svg\" fill=\"hsl(",
    #     hue, " ", saturation, "% ", lightness, "%)\">")
    p <- round(n / 5, 1)
    svg <- paste0("<svg viewBox=\"", 1-p, " ", 1-p, " ", n+1+p, " ", n+1+p, "\" xmlns=\"http://www.w3.org/2000/svg\" fill=\"hsl(",
        hue, " ", saturation, "% ", lightness, "%)\">")
    for (i in seq_len(nrow(d))) {
        rec <- paste0("<rect x=\"", d$col[i], "\" y=\"", d$row[i], "\" width=\"1\" height=\"1\"/>")
        svg <- paste0(svg, rec)
    }
    paste0(svg, "</svg>")
}

writeLines(fun4("p"), "test.svg")

s <- fun4("p")
z <- paste0('<!DOCTYPE HTML>
<html lang="en">
<head>
</head>
<body>
<div style="border-radius:50%;background-color:grey;height:48px;width:48px;">',
s,
'</div>
</body>
</html>', collapse="")
writeLines(z, "test.html")


# https://github.com/sykoram/gridenticon

svg_list <- list(
    "default" = c(
'<g id="0"></g>',
'<g id="1">
    <line x1="0.00" y1="0.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="2">
    <line x1="0.00" y1="10.00" x2="10.00" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="3">
    <line x1="0.00" y1="0.00" x2="0.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="10.00" y1="0.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="4">
    <line x1="0.00" y1="0.00" x2="10.00" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="10.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="5">
    <line x1="0.00" y1="0.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="10.00" x2="10.00" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="6">
    <line x1="0.00" y1="10.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="5.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="7">
    <line x1="0.00" y1="0.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="5.00" x2="10.00" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="8">
    <line x1="5.00" y1="5.00" x2="10.00" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="5.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="9">
    <line x1="0.00" y1="0.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="10.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="a">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 0 10.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 1 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="b">
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 1 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="c">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 0 10.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="d">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 1 0.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M10.00,0.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="e">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 1 0.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="f">
    <path d="M10.00,0.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>'
),
"dots" = c(
'<g id="0"></g>',
'<g id="1"><circle cx="5.00" cy="5.00" r="0.40" /></g>',
'<g id="2"><circle cx="5.00" cy="5.00" r="0.60" /></g>',
'<g id="3"><circle cx="5.00" cy="5.00" r="0.80" /></g>',
'<g id="4"><circle cx="5.00" cy="5.00" r="1.00" /></g>',
'<g id="5"><circle cx="5.00" cy="5.00" r="1.20" /></g>',
'<g id="6"><circle cx="5.00" cy="5.00" r="1.40" /></g>',
'<g id="7"><circle cx="5.00" cy="5.00" r="1.60" /></g>',
'<g id="8"><circle cx="5.00" cy="5.00" r="1.90" /></g>',
'<g id="9"><circle cx="5.00" cy="5.00" r="2.30" /></g>',
'<g id="a"><circle cx="5.00" cy="5.00" r="2.70" /></g>',
'<g id="b"><circle cx="5.00" cy="5.00" r="3.10" /></g>',
'<g id="c"><circle cx="5.00" cy="5.00" r="3.50" /></g>',
'<g id="d"><circle cx="5.00" cy="5.00" r="4.00" /></g>',
'<g id="e"><circle cx="5.00" cy="5.00" r="4.50" /></g>',
'<g id="f"><circle cx="5.00" cy="5.00" r="5.00" /></g>'
),
"lightning" = c(
'<g id="0">
    <line x1="0.00" y1="0.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="1">
    <path d="M0.00,0.00 A10.00,10.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="2">
    <path d="M0.00,0.00 A10.00,10.00 0.00 0 1 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="3">
    <path d="M0.00,0.00 A20.00,20.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="4">
    <path d="M0.00,0.00 A20.00,20.00 0.00 0 1 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="5">
    <line x1="0.00" y1="0.00" x2="2.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="6">
    <line x1="0.00" y1="0.00" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="2.50" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="7">
    <line x1="0.00" y1="0.00" x2="1.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="1.00" y1="5.00" x2="5.00" y2="9.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="9.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="8">
    <line x1="0.00" y1="0.00" x2="5.00" y2="1.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="1.00" x2="9.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="9.00" y1="5.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="9">
    <line x1="0.00" y1="0.00" x2="2.50" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="5.00" x2="7.50" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="5.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="a">
    <line x1="0.00" y1="0.00" x2="5.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="2.50" x2="5.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="5.00" y1="7.50" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="b">
    <line x1="0.00" y1="0.00" x2="3.00" y2="7.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="3.00" y1="7.00" x2="7.00" y2="3.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.00" y1="3.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="c">
    <line x1="0.00" y1="0.00" x2="7.00" y2="3.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.00" y1="3.00" x2="3.00" y2="7.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="3.00" y1="7.00" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="d">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 1 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="e">
    <path d="M0.00,0.00 A5.00,5.00 0.00 0 1 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="f">
    <line x1="0.00" y1="0.00" x2="3.33" y2="0.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="3.33" y1="0.00" x2="3.33" y2="3.33" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="3.33" y1="3.33" x2="6.66" y2="3.33" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="6.66" y1="3.33" x2="6.66" y2="6.66" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="6.66" y1="6.66" x2="10.00" y2="6.66" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="10.00" y1="6.66" x2="10.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>'
),
"stitching" = c(
'<g id="0">
</g>',

'<g id="1">
    <line x1="7.50" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="2">
    <line x1="0.00" y1="7.50" x2="2.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="3">
    <line x1="0.00" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="4">
    <line x1="7.50" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="5">
    <line x1="7.50" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="6">
    <line x1="7.50" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="7.50" x2="2.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="7">
    <line x1="7.50" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="8">
    <line x1="0.00" y1="2.50" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="9">
    <line x1="0.00" y1="2.50" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="a">
    <line x1="0.00" y1="7.50" x2="2.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="2.50" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="b">
    <line x1="0.00" y1="2.50" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="c">
    <line x1="0.00" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="d">
    <line x1="0.00" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="e">
    <line x1="0.00" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="7.50" x2="2.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="f">
    <line x1="0.00" y1="2.50" x2="10.00" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="0.00" y1="7.50" x2="10.00" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="2.50" y1="0.00" x2="2.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <line x1="7.50" y1="0.00" x2="7.50" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>'
),
"trees" = c(
'<g id="0">
</g>',

'<g id="1">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="2">
    <line x1="5.00" y1="0.00" x2="5.00" y2="3.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <circle cx="5.00" cy="5.00" r="0.50" style="fill:black" />
    <line x1="5.00" y1="7.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="3">
    <circle cx="5.00" cy="0.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="2.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="4.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="6.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="8.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="10.00" r="0.50" style="fill:black" />
</g>',

'<g id="4">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A5.00,5.00 0.00 0 0 5.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="5">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="6">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A5.00,5.00 0.00 0 0 5.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="7">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,0.00 A5.00,5.00 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="8">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="9">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,0.00 A5.00,5.00 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="a">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A5.00,5.00 0.00 0 0 5.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,0.00 A5.00,5.00 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="b">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="c">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A5.00,5.00 0.00 0 0 5.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="d">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,0.00 A5.00,5.00 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="e">
    <line x1="5.00" y1="0.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A5.00,5.00 0.00 0 0 5.00,0.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,0.00 A5.00,5.00 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M0.00,10.00 A5.00,5.00 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A5.00,5.00 0.00 0 0 10.00,10.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="f">
    <line x1="5.00" y1="0.00" x2="5.00" y2="2.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <circle cx="5.00" cy="4.00" r="0.50" style="fill:black" />
    <circle cx="5.00" cy="6.00" r="0.50" style="fill:black" />
    <line x1="5.00" y1="8.00" x2="5.00" y2="10.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>'
),
"waves" = c(
'<g id="0">
    <line x1="0.00" y1="5.00" x2="10.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="1">
    <line x1="0.00" y1="5.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="2">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <line x1="5.00" y1="5.00" x2="10.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
</g>',

'<g id="3">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="4">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="5">
    <line x1="0.00" y1="5.00" x2="5.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="6">
    <line x1="5.00" y1="5.00" x2="10.00" y2="5.00" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="7">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="8">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="9">
    <path d="M0.00,5.00 A6.25,6.25 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="a">
    <path d="M0.00,5.00 A6.25,6.25 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="b">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 2.50,2.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <line x1="2.50" y1="2.50" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M7.50,2.50 A2.50,2.50 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="c">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 0 2.50,7.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="7.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M7.50,7.50 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="d">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 0 2.50,2.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <line x1="2.50" y1="2.50" x2="7.50" y2="2.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M7.50,2.50 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="e">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 2.50,7.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <line x1="2.50" y1="7.50" x2="7.50" y2="7.50" style="stroke:black;stroke-width:1;stroke-linecap:round" />
    <path d="M7.50,7.50 A2.50,2.50 0.00 0 1 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>',

'<g id="f">
    <path d="M0.00,5.00 A2.50,2.50 0.00 0 1 2.50,2.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M2.50,2.50 A2.50,2.50 0.00 0 0 5.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M5.00,5.00 A2.50,2.50 0.00 0 1 7.50,7.50" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
    <path d="M7.50,7.50 A2.50,2.50 0.00 0 0 10.00,5.00" style="stroke:black;stroke-width:1;fill:none;stroke-linecap:round" />
</g>'
),
"squares" = c(
'<g id="0"></g>',
'<g id="1"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="2"><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="3"><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="4"><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="5"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="6"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="7"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="8"><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="9"><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="a"><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="b"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="c"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="d"><rect x="0.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="e"><rect x="0.00" y="5.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="0.00" width="5.00" height="5.00" style="fill:black" /><rect x="5.00" y="5.00" width="5.00" height="5.00" style="fill:black" /></g>',
'<g id="f"><rect x="0.00" y="0.00" width="10.00" height="10.00" style="fill:black" /></g>'
),
"triangles" = c(
'<g id="0"></g>',
'<g id="1"><polygon points="0,0 0,5 5,0" style="fill:black" /></g>',
'<g id="2"><polygon points="0,10 0,5 5,10" style="fill:black" /></g>',
'<g id="3"><polygon points="10,0 10,5 5,0" style="fill:black" /></g>',
'<g id="4"><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="5"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="0,10 0,5 5,10" style="fill:black" /></g>',
'<g id="6"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /></g>',
'<g id="7"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="8"><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /></g>',
'<g id="9"><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="a"><polygon points="10,0 10,5 5,0" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="b"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /></g>',
'<g id="c"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="d"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="e"><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>',
'<g id="f"><polygon points="0,0 0,5 5,0" style="fill:black" /><polygon points="0,10 0,5 5,10" style="fill:black" /><polygon points="10,0 10,5 5,0" style="fill:black" /><polygon points="10,10 10,5 5,10" style="fill:black" /></g>'
),
"pies" = c(
'<g id="0"></g>',
'<g id="1">',
'<path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /></g>',
'<g id="2"><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /></g>',
'<g id="3"><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /></g>',
'<g id="4"><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="5"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /></g>',
'<g id="6"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /></g>',
'<g id="7"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="8"><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /></g>',
'<g id="9"><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="a"><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="b"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /></g>',
'<g id="c"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="d"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="e"><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>',
'<g id="f"><path d="M 10,0 L 10,5 A 5 5 0 0 1 5 0 Z" /><path d="M 0,10 L 0,5 A 5 5 0 0 1 5 10 Z" /><path d="M 10,10 L 5,10 A 5 5 0 0 1 10 5 Z" /><path d="M 0,0 L 5,0 A 5 5 0 0 1 0 5 Z" /></g>'
)
)


fun5 <- function(string, type="default", n=8, col="black", bg="white") {

    tileSize = 10
    gridSize = n # 8 is the max
    border = 5
    bgColor = bg
    defs <- svg_list[[type]]

    hash <- digest::digest(enc2utf8(string), algo="sha256", serialize=FALSE)
    hash <- sapply(seq_len(nchar(hash)), function(z) substr(hash, z, z))
    g <- matrix(hash, gridSize, gridSize)

    w = gridSize * tileSize + 2*border
    h = gridSize * tileSize + 2*border

    svg <- c('<svg width="', w, '" height="', h, '" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><defs>',
        defs, '</defs>',
        '<rect x="0.00" y="0.00" width="', w, '" height="', h, '" style="stroke:none;fill:', bgColor, '" />')
    for (i in seq_len(gridSize)-1L) {
        for (j in seq_len(gridSize)-1L) {
            x <- i * tileSize + border
            y <- j * tileSize + border
            v <- paste0('<use x="', x, '" y="', y, '" xlink:href="#', g[i,j], '" />')
            v <- gsub("black", col, v)
            svg <- c(svg, v)
        }
    }
    svg <- c(svg, '</svg>')
    svg <- paste0(svg, collapse="")
    svg <- gsub("black", col, svg)
    svg
}

writeLines(fun5("aaa", "pies", col="blue"), "test.svg")
writeLines(fun5("aaa", "squares", col="blue"), "test.svg")
writeLines(fun5("aaa", "triangles", col="blue"), "test.svg")
writeLines(fun5("aaa", "default", col="blue"), "test.svg")
writeLines(fun5("aaa", "dots", col="blue"), "test.svg")
writeLines(fun5("aaa", "lightning", col="blue"), "test.svg")
writeLines(fun5("aaa", "stitching", col="blue"), "test.svg")
writeLines(fun5("aaa", "trees", col="blue"), "test.svg")
writeLines(fun5("aaa", "waves", col="blue"), "test.svg")

"default"   "dots"      "lightning" "stitching" "trees"     "waves"     "squares"   "triangles"

# https://drhus.github.io/awesome-identicons/

# https://github.com/donpark/identicon/blob/master/identicon-canvas/identicon_canvas.js

# for rect: x, y, w=h, rotation


fun6 <- function(x) {

    f <- function(x, y, r, col) {
        paste0('<circle cx="', x, '" cy="', y, '" r="', r, '" style="fill:', col, '" />')
    }
    w <- 100
    p <- 0
    # bg <- "#777777"
    get_hash <- function(x) {
        hash <- digest::digest(enc2utf8(x), algo="sha256", serialize=FALSE)
        hash <- sapply(seq_len(nchar(hash)), function(z) substr(hash, z, z))
        sapply(hash, function(z) which(c(0:9, letters[1:6]) == z))
    }
    hash <- get_hash(x)
    m <- matrix(hash, length(hash)/4, 4)
    m <- (m-1)/15

    bg <- paste0(c("#", names(hash)[1:6]), collapse="")
    svg <- c('<svg width="', w+2*p, '" height="', w+2*p, '" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">',
        '<rect x="0.00" y="0.00" width="', w+2*p, '" height="', w+2*p, '" style="stroke:none;fill:', bg, '" />')
    for (i in 1:nrow(m)) {
        v <- f(w*m[i,1]+p, w*m[i,2]+p, w*m[i,3]/5+w*0.1, hcl(360 * m[i,4], 95, 45, alpha=1))
        svg <- c(svg, v)
    }
    paste0(c(svg, '</svg>'), collapse="")
}

fun7 <- function(x) {

    f <- function(x, y, r, col, a=45) {
        paste0('<rect x="', x-r/2, '" y="', y-r/2, '" width="', r, '" height="', r, '" transform="rotate(', a, " ", x, " ", y, ')" style="fill:', col, '" />')
        # paste0('<circle cx="', x, '" cy="', y, '" r="', r, '" style="fill:', col, '" />')
    }
    w <- 100
    p <- w/10
    # bg <- "#777777"
    get_hash <- function(x) {
        hash <- digest::digest(enc2utf8(x), algo="sha256", serialize=FALSE)
        hash <- sapply(seq_len(nchar(hash)), function(z) substr(hash, z, z))
        sapply(hash, function(z) which(c(0:9, letters[1:6]) == z))
    }
    hash <- get_hash(x)
    m <- matrix(hash, length(hash)/4, 4)
    m <- (m-1)/15

    bg <- paste0(c("#", names(hash)[1:6]), collapse="")
    svg <- c('<svg width="', w+2*p, '" height="', w+2*p, '" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">',
        '<rect x="0.00" y="0.00" width="', w+2*p, '" height="', w+2*p, '" style="stroke:none;fill:', bg, '" />')
    for (i in 1:nrow(m)) {
        v <- f(w*m[i,1]+p, w*m[i,2]+p, 0.5*w*m[i,3]+0.5*w, hcl(360 * m[i,4], 95, 45, alpha=0.5))
        svg <- c(svg, v)
    }
    paste0(c(svg, '</svg>'), collapse="")
}

writeLines(fun7("qwerty"), "test.svg")
writeLines(fun7("s"), "test.svg")


# https://github.com/MBalthasar/rHarmonics

fun_X <- function(x, n=3) {
    r <- 2*pi*x
    N <- seq_len(n)
    o <- matrix(0, length(x), 2+n*2)
    colnames(o) <- c("intercept", "slope",
        paste0("sin", N)[N], paste0("cos", N)[N])
    o[,"intercept"] <- 1
    o[,"slope"] <- r
    for (i in N) {
        o[,paste0("sin", i)] <- sin(i*r)
        o[,paste0("cos", i)] <- cos(i*r)
    }
    o
}
fun_h <- function(x, y, n=3) {
    d <- fun_X(x, n=n)
    cf <- .lm.fit(d, y)$coefficients
    names(cf) <- colnames(d)
    cf
}

n <- 4
x <- 1:64/(64/n)
y <- sample(0:15, 64, replace=T) / 15

d <- fun_X(x, n = length(x))

# s <- c(1, 2, 4, 8, 16, 32)
#s <- c(1, 2, 3, 4, 6, 8, 10, 12, 16)
s <- seq(2, 32, 2)

cf <- lapply(s, function(n) {
  fun_h(x, y, n=n)
})

tmp <- sapply(cf, \(z) {
  v <- drop(d[,names(z)] %*% z)
  (v - min(v, na.rm = TRUE))/diff(range(v, na.rm = TRUE))
})
tmp <- cbind(tmp, y)

tmp2 <- cbind(tmp[,1])
for (i in 2:ncol(tmp)) {
  p <- tmp2[,ncol(tmp2)]
  k <- tmp[,i] # * i
  d <- max(p-k)
  tmp2 <- cbind(tmp2, k+d)
  # tmp2 <- cbind(tmp2, k)
}
tmp2 <- tmp2[,rev(seq_len(ncol(tmp2)))]

## calculate polar coordinates
rad <- (360 * seq_along(x)/length(x)) * (pi / 180)
xy <- lapply(1:ncol(tmp2), function(i) {
  pracma::pol2cart(cbind(rad, tmp2[,i]))
})

matplot(x, tmp2, type="l", lty=1)

plot(x, y, ylim=c(0,ncol(tmp)), type="n")
for (i in 1:ncol(tmp))
  lines(x, tmp[,i]+i-1)

col <- hcl.colors(ncol(tmp2), "viridis", rev=TRUE)
plot(0,type="n",asp=1, xlim=c(-max(tmp2), max(tmp2)), ylim=c(-max(tmp2), max(tmp2)),
  ann=FALSE, axes=FALSE)
for (i in 1:length(xy))
  polygon(xy[[i]], border=NA, col=col[i])

# https://frontend.horse/articles/generative-grids/

# triangle
<svg width="50" height="50" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<g id="0"><polygon points="5 0, 0 8.66, 10 8.66"/></g>
<g id="1"><polygon points="5 8.66, 0 0, 10 0"/></g>
</defs>

<g id="triangle" transform="rotate(180 20 17.32)">

<g id="top">
<use x="15" y="0" xlink:href="#0" fill="red" />
<use x="10" y="8.66" xlink:href="#0" fill="red" />
<use x="15" y="8.66" xlink:href="#1" fill="black" />
<use x="20" y="8.66" xlink:href="#0" fill="red" />
</g>

<g id="left">
<use x="5" y="17.32" xlink:href="#0" fill="blue" />
<use x="0" y="25.98" xlink:href="#0" fill="blue" />
<use x="5" y="25.98" xlink:href="#1" fill="green" />
<use x="10" y="25.98" xlink:href="#0" fill="blue" />
</g>

<g id="right">
<use x="25" y="17.32" xlink:href="#0" fill="black" />
<use x="20" y="25.98" xlink:href="#0" fill="red" />
<use x="25" y="25.98" xlink:href="#1" fill="green" />
<use x="30" y="25.98" xlink:href="#0" fill="blue" />
</g>

<g id="mid">
<use x="10" y="17.32" xlink:href="#1" fill="purple" />
<use x="20" y="17.32" xlink:href="#1" fill="pink" />
<use x="15" y="25.98" xlink:href="#1" fill="orange" />
<use x="15" y="17.32" xlink:href="#0" fill="yellow" />
</g>

</g>

</svg>
