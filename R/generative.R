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

# generative avatar images
fun3 <- function(n=5, pad=0.5, bg="grey") {

    if (n %% 2 != 1)
        stop("n must be an odd number.")
    if (n < 5)
        stop("n must be >= 3.")
    cols <- c("blue", "brown4",
        "darkcyan", "darkgoldenrod", 
        "darkgreen","darkmagenta", "darkolivegreen", 
        "darkorange", 
        "darkorchid", 
        "darkred", "darksalmon", 
        "darkslategrey", "darkturquoise", "darkviolet", "deeppink", 
        "firebrick",  
        "indianred2", "magenta4", "maroon", "mediumblue", "mediumslateblue", 
        "mediumvioletred", "midnightblue", 
        "navy")
    col <- sample(cols, 1L)

    p <- 0.5
    m <- matrix(0, n, n)
    for (i in 1:n) {
        for (j in 1:ceiling(n/2)) {
            m[i,j] <- rbinom(1, 1, p)
        }
    }
    for (j in (ceiling(n/2)+1):n) {
        m[,j] <- m[,n-j+1]
    }
    d <- data.frame(row=rep(1:n, n), col=rep(1:n, each=n), value=as.integer(m))
    d <- d[d$value > 0,]
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
    png(paste0("docs/random/avarat-", if (i < 10) "0" else "", i, ".png"), 200, 200)
    fun3()
    dev.off()
}

