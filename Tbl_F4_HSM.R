# Reproduces Table F4 of Online Supplement F (Heuristic-Systematic Model, HSM): the extreme cases (V-representation) of the Override model
# and of Take-the-best (TTB), and the parsimony (volume) and the largest possible Bayes factor of Override, Trade-off, TTB and the mixture.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript Tbl_F4_HSM.R   (writes Tbl_F4_Vrep_Override_TTB.pdf; takes a few seconds)
source("_table_pdf.R")
library(fairy)

# ===== PART 1: the fairy. This is all that is needed to get the numbers. =====
# cues: p1 = incongruent / weak, p2 = none / weak, p3 = congruent / weak, p4 = incongruent / strong, p5 = none / strong, p6 = congruent / strong
override <- fairy_h("p1 = p2 < p3 < p6; p2 < p4 = p5 < p6", n_p = 6)   # Override model
tradeoff <- fairy_h("p1 < p2 < p3 < p6; p1 < p4 < p5 < p6; p2 < p5", n_p = 6)   # Trade-off model
ttb      <- fairy_h("p1 = p4 = 0; p2 = p3 = p5 = p6 = 1", n_p = 6)  # Take-the-best
mixture  <- fairy_mixture(override, ttb)                            # some follow Override, the rest TTB

models <- list("Override" = override, "Trade-off" = tradeoff, "Take-the-best" = ttb, "Mixture of Override and Take-the-best" = mixture)
info <- do.call(rbind, lapply(models, function(h) fairy_info(h, method = "exact")))   # dimension, vertices, constraints, volume, largest Bayes factor
print(info)

vwords <- function(h) as.matrix(fairy_words(h))                     # extreme cases in words: none = nobody acts, all = everybody acts
for (nm in c("override", "ttb")) { cat("\n", nm, ": extreme cases in words\n", sep = ""); print(vwords(get(nm))) }

# ===== PART 2: layout only (formatting for the PDF); not needed for the result. =====
cols <- c("iW", "nW", "cW", "iS", "nS", "cS")
ot <- rbind(vwords(override), vwords(ttb)); colnames(ot) <- cols                # Override model (top) and Take-the-best (bottom)
ot <- cbind(Model = c(rep("Override", nrow(vwords(override))), "Take-the-best"), Vertex = c(paste0("V", seq_len(nrow(vwords(override)))), "V1"), ot)
table_pdf(ot, "Tbl_F4_Vrep_Override_TTB.pdf", "Extreme cases of the Override model (top) and Take-the-best (bottom)", row_names = FALSE, cex = 0.8,
          header_expr = c(list(NULL, NULL), lapply(cols, function(s) bquote(italic(P)[.(s)]))))
