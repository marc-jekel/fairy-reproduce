# Reproduces Tables F1 to F4 of the Heuristic-Systematic Model (HSM) case study of Online Supplement F:
# the extreme cases (V-representation) of Predictions 1 to 3, of the Override model, of Take-the-best (TTB) and of the mixture of both,
# and the parsimony (volume) and the largest possible Bayes factor of all HSM models, including the replications and the 3 x 3 design.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_F1-F4_HSM.R   (writes Tbl_F1_..., Tbl_F2_..., Tbl_F3_... and Tbl_F4_... as PDF; takes a few seconds)
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
# cues: p1 = incongruent / weak, p2 = none / weak, p3 = congruent / weak, p4 = incongruent / strong, p5 = none / strong, p6 = congruent / strong
override <- fairy_h("p1 = p2 < p3 < p6; p2 < p4 = p5 < p6", n_p = 6)   # Override model
tradeoff <- fairy_h("p1 < p2 < p3 < p6; p1 < p4 < p5 < p6; p2 < p5", n_p = 6)   # Trade-off model
ttb      <- fairy_h("p1 = p4 = 0; p2 = p3 = p5 = p6 = 1", n_p = 6)  # Take-the-best
mixture  <- fairy_mixture(override, ttb)                            # some follow Override, the rest TTB

models <- list("Prediction 1 (argument strength)" = pred1, "Prediction 2 (gap size)" = pred2, "Prediction 3 (both)" = pred3,
               "Prediction 3, two populations, joint" = pred3_joint, "Prediction 3, two populations, substitutable" = pred3_subst,
               "3 x 3 design (nine probabilities)" = pred_3x3,
               "Override" = override, "Trade-off" = tradeoff, "Take-the-best" = ttb, "Mixture of Override and Take-the-best" = mixture)
info <- do.call(rbind, lapply(models, function(h) fairy_info(h, method = "exact")))   # dimension, vertices, constraints, volume, largest Bayes factor
print(info)

vwords <- function(h) as.matrix(fairy_words(h))                     # extreme cases in words: none = nobody acts, all = everybody acts
for (nm in c("pred1", "pred2", "pred3", "override", "ttb")) { cat("\n", nm, ": extreme cases in words\n", sep = ""); print(vwords(get(nm))) }

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
hd <- function(x) lapply(x, function(s) bquote(italic(P)[.(s)]))
v_table <- function(h, cols, file, title) {
  w <- vwords(h); colnames(w) <- cols
  w <- cbind(Vertex = paste0("V", seq_len(nrow(w))), w)
  table_pdf(w, file, title, row_names = FALSE, cex = 0.8, header_expr = c(list(NULL), hd(cols)))
}
c22 <- c("bW", "bS", "sW", "sS"); c6 <- c("iW", "nW", "cW", "iS", "nS", "cS")
v_table(pred1, c22, "Tbl_F1_Vrep_Prediction1.pdf", "Extreme cases for the impact of argument strength (Prediction 1)")
v_table(pred2, c22, "Tbl_F2_Vrep_Prediction2.pdf", "Extreme cases for the impact of gap size (Prediction 2)")
v_table(pred3, c22, "Tbl_F3_Vrep_Prediction3.pdf", "Extreme cases under Prediction 3")
ot <- rbind(vwords(override), vwords(ttb)); colnames(ot) <- c6                  # Override model (top) and Take-the-best (bottom)
ot <- cbind(Model = c(rep("Override", nrow(vwords(override))), "Take-the-best"), Vertex = c(paste0("V", seq_len(nrow(vwords(override)))), "V1"), ot)
table_pdf(ot, "Tbl_F4_Vrep_Override_TTB.pdf", "Extreme cases of the Override model (top) and Take-the-best (bottom)", row_names = FALSE, cex = 0.8,
          header_expr = c(list(NULL, NULL), hd(c6)))
