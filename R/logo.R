analythium_logo <- function(
    tl = "#FFEB3B", # top left pentagon fill color
    tr = "#FFFDE7", # top right pentagon fill color
    bl = "#F9A825", # bottom left pentagon fill color
    br = "#FFEB3B", # bottom right pentagon fill color
    sc = "#F9A825", # stroke color
    sw = 2,         # stroke width
    pretty=FALSE
) {
    svg <- '<svg
xmlns=\"http://www.w3.org/2000/svg\"
xmlns:xlink=\"http://www.w3.org/1999/xlink\"
version=\"1.1\"
viewBox=\"0 0 210 190\"
width=\"210mm\"
height=\"210mm\"
id=\"logo\">
<g id=\"pentagons\">
<path
id=\"topleft\"
style=\"opacity:1;fill:{tl};fill-opacity:1;fill-rule:nonzero;stroke:{sc};stroke-width:{sw};stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1\"
d=\"M 69.231583,31.943134 100.7159,32.002614 84.788854,82.014173 42.507525,113.1127 32.168286,92.893368 Z\" />
<path
id=\"topright\"
style=\"opacity:1;fill:{tr};fill-opacity:1;fill-rule:nonzero;stroke:{sc};stroke-width:{sw};stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1\"
d=\"M 144.23976,32.122487 100.7159,32.002616 84.788854,82.014173 127.43091,112.61616 169.71224,81.517635 Z\" />
<path
id=\"bottomleft\"
style=\"opacity:1;fill:{bl};fill-opacity:1;fill-rule:nonzero;stroke:{sc};stroke-width:{sw};stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1\"
d=\"m 67.980004,162.50785 43.523836,0.11987 15.92707,-50.01156 L 84.788854,82.014173 42.507525,113.1127 Z\" />
<path
id=\"bottomright\"
style=\"opacity:1;fill:{br};fill-opacity:1;fill-rule:nonzero;stroke:{sc};stroke-width:{sw};stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1\"
d=\"m 142.98818,162.6872 -31.48434,-0.0595 15.92707,-50.01156 42.28133,-31.098526 10.33924,20.219316 z\" />
</g>
</svg>'
    svg <- gsub("{tl}", tl, svg, fixed=TRUE)
    svg <- gsub("{tr}", tr, svg, fixed=TRUE)
    svg <- gsub("{bl}", bl, svg, fixed=TRUE)
    svg <- gsub("{br}", br, svg, fixed=TRUE)
    svg <- gsub("{sc}", sc, svg, fixed=TRUE)
    svg <- gsub("{sw}", sw, svg, fixed=TRUE)
    if (!pretty) {
      svg <- gsub(">\n<", "><", svg, fixed=TRUE)
      svg <- gsub("\n", " ", svg, fixed=TRUE)
    }
    svg
}
