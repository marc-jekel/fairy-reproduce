# Reproduces the H-representations and V-representations of Models 1 to 10 of Online Supplement D (Figures D1 to D20; the fairy's output for every model).
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Figure_D01-D20_Hrep_Vrep_Models.R   (writes Figure_D01_Hrep_Model_01.pdf ... Figure_D10_Hrep_Model_10.pdf, Figure_D11_Vrep_Model_01.pdf ... Figure_D20_Vrep_Model_10.pdf takes a few seconds)
source("_models.R"); source("_table_pdf.R")

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# For every model (see _models.R): its constraints (H-representation) and its extreme cases as numbers and in words (V-representation).
H     <- lapply(m, function(h) fairy_h_table(h, style = "text"))     # the non-redundant constraints
V     <- lapply(m, function(h) as.matrix(fairy_v_table(h)))          # the extreme cases as numbers (fractions where needed)
Words <- lapply(m, function(h) as.matrix(fairy_words(h)))            # the same extreme cases in words: 0 = none, 1 = all
for (nm in names(m)) {                  # console: the first constraints and extreme cases of every model (the PDFs show all of them)
  cat("\n==== ", nm, ": ", nrow(H[[nm]]), " constraints, ", nrow(V[[nm]]), " extreme cases ====\n", sep = ""); print(head(H[[nm]], 8))
  cat("-- extreme cases (numbers), first 6 --\n"); print(head(V[[nm]], 6)); cat("-- extreme cases (words), first 6 --\n"); print(head(Words[[nm]], 6))
}

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
sym <- c("↬", "↬", "↬", "♡", "♡", "♡"); cond <- c("C", "T", "A", "C", "T", "A")   # symbols and groups of the six probabilities
pname  <- function(i) bquote(italic(P)[.(cond[i])]^.(sym[i]))            # typeset: P with the symbol up and the group down
header <- c(list(NULL), lapply(1:6, pname), list(NULL), lapply(1:6, pname))

# one constraint as a typeset expression, e.g. "p2 <= p1" -> P_T^.. <= P_C^..
as_expr <- function(s) {
  s <- gsub(" = ", " == ", s, fixed = TRUE)
  for (i in 6:1) s <- gsub(paste0("p", i), sprintf('italic(P)[%s]^"%s"', cond[i], sym[i]), s, fixed = TRUE)
  parse(text = s)[[1]]
}
h_pdf <- function(tab, file, title) {
  ex <- lapply(tab$Constraint, as_expr); n <- length(ex); cex <- 0.95
  grDevices::cairo_pdf(file, width = 6, height = 4); op <- par(family = "serif")
  w <- max(vapply(ex, function(e) strwidth(e, units = "inches", cex = cex), 0)) + 0.6; dev.off()
  hh <- 0.3; W <- max(w, 3.2); H <- n * hh + 0.9
  grDevices::cairo_pdf(file, width = W, height = H); on.exit(dev.off())
  par(mar = c(0, 0, 0, 0), family = "serif"); plot.new(); plot.window(xlim = c(0, W), ylim = c(H, 0))
  text(0.2, 0.3, title, adj = c(0, 0.5), font = 2, cex = 1.0)
  for (i in seq_len(n)) {
    if (i %% 2 == 0) rect(0.1, 0.6 + (i - 1) * hh, W - 0.1, 0.6 + i * hh, col = "#f3f5f9", border = NA)
    text(W / 2, 0.6 + (i - 0.5) * hh, ex[[i]], cex = cex)
  }
}
v_pdf <- function(num, words, file, title) {
  colnames(num) <- colnames(words) <- paste0("P^", sym, "_", cond)
  table_pdf(cbind(num, ` ` = "", words), file, title, cex = 0.55, header_expr = header)
}

for (k in seq_along(m)) {
  id <- sprintf("%02d", k); nm <- names(m)[k]
  h_pdf(H[[nm]], sprintf("Figure_D%02d_Hrep_Model_%s.pdf", k, id), sprintf("H-representation of Model %d", k))
  v_pdf(V[[nm]], Words[[nm]], sprintf("Figure_D%02d_Vrep_Model_%s.pdf", k + 10, id), sprintf("V-representation of Model %d", k))
}
