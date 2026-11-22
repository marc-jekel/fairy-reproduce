# Reproduces every figure and table of the paper and of the online supplement that the fairy produces: one command, all scripts in turn.
# Needs the R package fairy; see https://modeling-for-everyone.eu/posts/fairy/#reproduce
# Run:  Rscript run_all.R          (all scripts; about one minute)
#       Rscript run_all.R quick    (skips the slow data analysis of Tables E3 and E5)
# Every script writes its PDFs next to itself; this script lists them at the end together with the run time of every script.
scripts <- c("Figure_2_Hrep_Model4.R", "Figure_3_4_Vrep.R", "Figure_5_Polytopes.R", "Tbl_3_Relationships.R", "Tbl_4_Replication.R",
             "Figure_D01-D20_Hrep_Vrep_Models.R", "Tbl_E2_E4_Data.R", "Tbl_E3_E5_DataAnalysis.R", "Tbl_F1-F4_HSM.R")
if ("quick" %in% commandArgs(TRUE)) scripts <- setdiff(scripts, "Tbl_E3_E5_DataAnalysis.R")
rscript <- file.path(R.home("bin"), "Rscript")
before  <- list.files(pattern = "\\.(pdf|png)$")
secs <- status <- numeric(length(scripts))
for (i in seq_along(scripts)) {
  cat(sprintf("[%d/%d] %s ... ", i, length(scripts), scripts[i])); flush.console()
  t0 <- Sys.time()
  status[i] <- system2(rscript, scripts[i], stdout = FALSE, stderr = FALSE)
  secs[i] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(if (status[i] == 0) "done" else "FAILED", sprintf("(%.0f s)\n", secs[i]))
}
made <- setdiff(list.files(pattern = "\\.(pdf|png)$"), before)
print(data.frame(script = scripts, seconds = round(secs), ok = status == 0), row.names = FALSE)
cat("\nFiles written:", length(made), "\n"); cat(paste0("  ", sort(list.files(pattern = "^(Figure|Tbl)_.*\\.pdf$"))), sep = "\n")
