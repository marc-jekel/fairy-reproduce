# Reproduces Table F3 of Online Supplement F (Heuristic-Systematic Model, HSM): the extreme cases (V-representation) of Prediction 3,
# and the parsimony (volume) and the largest possible Bayes factor of Prediction 3, its replications and the 3 x 3 design.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_F3_HSM.R   (writes Tbl_F3_Vrep_Prediction3.pdf; takes a few seconds)
source("_table_pdf.R")
library(fairy)

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# 2 x 2: p1 = big gap / weak argument, p2 = big / strong, p3 = small / weak, p4 = small / strong
pred1 <- fairy_h("p1 < p2; p3 < p4", n_p = 4)                      # Prediction 1: a stronger argument never lowers the share who act
pred2 <- fairy_h("p1 < p3; p2 < p4", n_p = 4)                      # Prediction 2: a smaller gap never lowers it
pred3 <- fairy_intersect(pred1, pred2)                              # Prediction 3: both together
pred3_joint <- fairy_replicate(pred3, 2, "joint")                   # holds in two populations
pred3_subst <- fairy_replicate(pred3, 2, "substitutable")           # ... with substitutable parameters
# 3 x 3: p1..p3 = big gap, p4..p6 = medium gap, p7..p9 = small gap, each with weak, medium, strong argument
pred_3x3 <- fairy_h("p1 < p2 < p3; p4 < p5 < p6; p7 < p8 < p9; p1 < p4 < p7; p2 < p5 < p8; p3 < p6 < p9", n_p = 9)

models <- list("Prediction 3 (both)" = pred3,
               "Prediction 3, two populations, joint" = pred3_joint, "Prediction 3, two populations, substitutable" = pred3_subst,
               "3 x 3 design (nine probabilities)" = pred_3x3)
info <- do.call(rbind, lapply(models, function(h) fairy_info(h, method = "exact")))   # dimension, vertices, constraints, volume, largest Bayes factor
print(info)

vwords <- function(h) as.matrix(fairy_words(h))                     # extreme cases in words: none = nobody acts, all = everybody acts
cat("\npred3: extreme cases in words\n"); print(vwords(pred3))

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
cols <- c("bW", "bS", "sW", "sS")
w <- vwords(pred3); w <- cbind(Vertex = paste0("V", seq_len(nrow(w))), w)
table_pdf(w, "Tbl_F3_Vrep_Prediction3.pdf", "Extreme cases under Prediction 3", row_names = FALSE, cex = 0.8,
          header_expr = c(list(NULL), lapply(cols, function(s) bquote(italic(P)[.(s)]))))
