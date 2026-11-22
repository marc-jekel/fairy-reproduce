# Reproduces Figure 2 of the paper: the fairy's minimal description (H-representation) of Model 4.
#
# Needs the R package fairy (https://anonymous.4open.science/r/fairy-B0F2/, folder pkg).
# Run:  Rscript Figure_2_Hrep_Model4.R      (writes Figure_2_Hrep_Model4.pdf and .png next to the script)

library(fairy)

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# In short: h <- fairy_h("<the model as constraints>", n_p = 6); fairy_h_table(h)
# six probabilities: p1..p3 = P(->)_C, P(->)_T, P(->)_A (joke type 1) and p4..p6 = P(heart)_C, P(heart)_T, P(heart)_A (joke type 2)
model4 <- "p1 > p2 > p3; p4 < p5 < p6; p1 < p4; p2 < p5; p3 < p6"
h <- fairy_h(model4, n_p = 6)

# the non-redundant constraints, one per row (the paper shows them as "left side <= right side")
tab <- fairy_h_table(h, style = "text")
print(tab)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
parts <- strsplit(tab$Constraint, " <= ", fixed = TRUE)   # "p2 <= p1" -> "p2", "p1"
lhs <- vapply(parts, `[`, "", 1); rhs <- vapply(parts, `[`, "", 2)

# colour and typeset name of every probability (the colours of the app)
cols  <- c("#2f5fe0", "#d62728", "#10a050", "#e08a1a", "#8a3fd0", "#1f9ab5")
sym   <- c("↬", "↬", "↬", "♡", "♡", "♡")      # looparrowright, heart
cond  <- c("C", "T", "A", "C", "T", "A")
pname <- function(i) bquote(italic(P)[.(cond[i])]^.(sym[i]))

# a row's side such as "p3" or "1" or "0" -> expression
side <- function(s) {
  if (grepl("^p[1-6]$", s)) pname(as.integer(substring(s, 2))) else s
}
col_of <- function(s) if (grepl("^p[1-6]$", s)) cols[as.integer(substring(s, 2))] else "black"

draw <- function() {
  n <- nrow(tab)
  op <- par(mar = c(0.4, 0.4, 0.4, 0.4), family = "serif"); on.exit(par(op))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(n + 0.5, 0.5))
  for (r in seq_len(n)) {
    if (r %% 2 == 0) rect(0.2, r - 0.5, 0.8, r + 0.5, col = "#f3f5f9", border = NA)
    l <- lhs[r]; rr <- rhs[r]
    text(0.38, r, side(l), col = col_of(l), adj = 1, cex = 1.2)
    text(0.5,  r, "≤", cex = 1.2)
    text(0.62, r, side(rr), col = col_of(rr), adj = 0, cex = 1.2)
  }
}

grDevices::cairo_pdf("Figure_2_Hrep_Model4.pdf", width = 5, height = 2.8); draw(); dev.off()
grDevices::png("Figure_2_Hrep_Model4.png", width = 1500, height = 840, res = 300, type = "cairo"); draw(); dev.off()
message("written: Figure_2_Hrep_Model4.pdf / .png")
