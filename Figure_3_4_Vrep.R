# Reproduces Figures 3 and 4 of the paper: the V-representation (extreme cases) of Model 4 (Figure 3) and Model 6 (Figure 4),
# as numbers (left) and cast in words (right).
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Figure_3_4_Vrep.R   (writes Figure_3_Vrep_Model4.pdf and Figure_4_Vrep_Model6.pdf)
# (The order of the vertices V1, V2, ... is arbitrary; the script lists them in a fixed order, the app in the order of its algorithm.)
source("_models.R"); source("_table_pdf.R")

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# In short: fairy_v_table(m$M4) gives the extreme cases as numbers and fairy_words(m$M4) gives them in words (models: see _models.R).
num4   <- as.matrix(fairy_v_table(m$M4));  words4 <- as.matrix(fairy_words(m$M4))     # Figure 3 (Model 4)
num6   <- as.matrix(fairy_v_table(m$M6));  words6 <- as.matrix(fairy_words(m$M6))     # Figure 4 (Model 6)
cat("Model 4: extreme cases as numbers\n");  print(num4);  cat("\nModel 4: in words (0 = none, 1 = all)\n"); print(words4)
cat("\nModel 6: extreme cases as numbers\n"); print(num6);  cat("\nModel 6: in words (0 = none, 1 = all)\n"); print(words6)

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====

sym <- c("\u21ac", "\u21ac", "\u21ac", "\u2661", "\u2661", "\u2661"); cond <- c("C", "T", "A", "C", "T", "A")   # symbols and groups of the six probabilities
nm <- paste0("P^", sym, "_", cond)                                      # plain names (column names of the data)
pname <- function(i) bquote(italic(P)[.(cond[i])]^.(sym[i]))            # typeset: P with the symbol up and the group down
header <- c(list(NULL), lapply(1:6, pname), list(NULL), lapply(1:6, pname))   # first entry: the vertex column

vrep_figure <- function(num, words, file, title) {
  colnames(num) <- colnames(words) <- nm
  table_pdf(cbind(num, ` ` = "", words), file, title, cex = 0.55, header_expr = header)
}
vrep_figure(num4, words4, "Figure_3_Vrep_Model4.pdf", "Figure 3: V-representation of Model 4")
vrep_figure(num6, words6, "Figure_4_Vrep_Model6.pdf", "Figure 4: V-representation of Model 6")
