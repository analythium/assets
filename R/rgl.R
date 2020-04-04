library(rgl)
open3d()
shade3d( translate3d( dodecahedron3d(col = "grey"), 0, 0, 0))
play3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 5)
