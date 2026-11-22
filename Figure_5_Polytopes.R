# Reproduces Figure 5 of the paper: two rectangles (Models I and II), their overlap (Model III) and a line segment (Model IV).
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Figure_5_Polytopes.R   (writes Figure_5_Polytopes.pdf / .png)
library(fairy)

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
I   <- fairy_h("p1 < .5; p2 > .25; p2 < .75", n_p = 2)      # dashed
II  <- fairy_h("p1 > .3; p1 < .7; p2 > .5", n_p = 2)         # solid
III <- fairy_intersect(I, II)                                  # grey rectangle
IV  <- fairy_h("p2 = .8; p1 > .2; p1 < .9", n_p = 2)          # dotted line segment (drawn as in the paper)

print(rbind(I = fairy_info(I, method = "exact"), II = fairy_info(II, method = "exact"), III = fairy_info(III, method = "exact"), IV = fairy_info(IV, method = "exact")))

v <- lapply(list(I = I, II = II, III = III, IV = IV), function(h) as.matrix(fairy_vertices(h)))   # the corners of the four models
print(v)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
hull <- function(x) x[chull(x), , drop = FALSE]

draw <- function() {
  op <- par(mar = c(4.5, 4.5, 1, 1), family = "serif"); on.exit(par(op))
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1, xlab = expression(italic(p)[1]), ylab = expression(italic(p)[2]),
       axes = FALSE, cex.lab = 1.4)
  box()
  axis(1, c(0, .3, .5, .7, 1), c("0", ".3", ".5", ".7", "1")); axis(2, c(.25, .5, .75, 1), c(".25", ".5", ".75", "1"), las = 1)
  polygon(hull(v$III), col = "grey60", border = NA)
  polygon(hull(v$I),  border = "black", lty = 2, lwd = 2)
  polygon(hull(v$II), border = "black", lty = 1, lwd = 2)
  lines(v$IV[order(v$IV[, 1]), ], lty = 3, lwd = 3)
  points(rbind(v$I, v$II, v$IV), pch = 19, cex = 1.3)
  points(v$III, pch = 21, bg = "grey60", cex = 1.3)
}
cairo_pdf("Figure_5_Polytopes.pdf", width = 4, height = 4); draw(); dev.off()
png("Figure_5_Polytopes.png", width = 1200, height = 1200, res = 300, type = "cairo"); draw(); dev.off()
message("written: Figure_5_Polytopes.pdf / .png")
