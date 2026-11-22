# Models 1 to 10 of the cringyness study (shared by the scripts; you do not run this file yourself).
# Six probabilities p1..p6: the probability that a child, teen, or adult cringes at an illogical joke (p1..p3) or at a pun (p4..p6).
library(fairy)
# These models are the only input of the scripts: each one is written as order constraints and turned into its H-representation by fairy_h().

lab <- c("illogical: children", "illogical: teens", "illogical: adults",
         "pun: children", "pun: teens", "pun: adults")

strings <- c(
  M1 = "(p1+p4)/2 < (p2+p5)/2 < (p3+p6)/2",
  M2 = "p1 < p2 < p3; p4 < p5 < p6",
  M3 = "p1 > p2 > p3; p4 < p5 < p6; p1+p2+p3 < p4+p5+p6",
  M4 = "p1 > p2 > p3; p4 < p5 < p6; p1 < p4; p2 < p5; p3 < p6",
  M5 = "p1 = p2 = p3 = p4 = p5 = p6",
  M6 = "p1 > p2 > p3; p4 < p5 < p6; p1+p2+p3 = p4+p5+p6",
  M7 = "p1 > p2 > p3; p4 < p5 < p6; (p1+p2+p3)/3 = (p4+p5+p6)/3",
  M8 = "{p1,p2,p3,p4,p5,p6} = {p1,p2,p3,p4,p5,p6}",
  M9 = "p1 < p2 < p3; p4 > p5 > p6; p1 > p4; p2 > p5; p3 > p6"
)
m <- lapply(strings, fairy_h, n_p = 6)                          # Models 1 to 9
m$M7 <- fairy_h(strings[["M7"]], n_p = 6, approx_tol = 0.05)     # approximate equalities
m$M8 <- fairy_h(strings[["M8"]], n_p = 6, approx_tol = 0.05)
m$M10 <- fairy_mixture(m$M4, m$M9)                               # the mixture of Models 4 and 9
m <- m[paste0("M", 1:10)]

# The hypothetical data of the cringyness study (Tables E2 and E4 of Online Supplement E): frequencies of "cringe" out of n per cell, order p1..p6
data_sets <- list(
  "Mike's data"    = data.frame(probability = paste0("p", 1:6), n = 100, frequency = c(60, 64, 68, 60, 50, 38)),
  "Marc's data"    = data.frame(probability = paste0("p", 1:6), n = 250, frequency = c(150, 160, 170, 150, 125, 95)),
  "Meichai's data" = data.frame(probability = paste0("p", 1:6), n = 100, frequency = c(44, 40, 36, 50, 60, 70))
)
