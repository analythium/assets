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
