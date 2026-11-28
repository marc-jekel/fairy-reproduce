# Reproduces Table F1 of Online Supplement F (Heuristic-Systematic Model, HSM): the extreme cases (V-representation) of Prediction 1,
# with its parsimony (volume) and the largest possible Bayes factor.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_F1_HSM.R   (writes Tbl_F1_Vrep_Prediction1.pdf; takes a few seconds)
source("_table_pdf.R")
library(fairy)

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# 2 x 2: p1 = big gap / weak argument, p2 = big / strong, p3 = small / weak, p4 = small / strong
pred1 <- fairy_h("p1 < p2; p3 < p4", n_p = 4)                      # Prediction 1: a stronger argument never lowers the share who act

models <- list("Prediction 1 (argument strength)" = pred1)
info <- do.call(rbind, lapply(models, function(h) fairy_info(h, method = "exact")))   # dimension, vertices, constraints, volume, largest Bayes factor
print(info)

vwords <- function(h) as.matrix(fairy_words(h))                     # extreme cases in words: none = nobody acts, all = everybody acts
cat("\npred1: extreme cases in words\n"); print(vwords(pred1))

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
cols <- c("bW", "bS", "sW", "sS")
w <- vwords(pred1); w <- cbind(Vertex = paste0("V", seq_len(nrow(w))), w)
table_pdf(w, "Tbl_F1_Vrep_Prediction1.pdf", "Extreme cases for the impact of argument strength (Prediction 1)", row_names = FALSE, cex = 0.8,
          header_expr = c(list(NULL), lapply(cols, function(s) bquote(italic(P)[.(s)]))))
