# Run by Binder when the environment is built: installs the packages that the fairy needs, then the fairy itself (folder pkg).
install.packages(c("remotes", "coda", "multinomineq", "volesti", "qpdf"))
remotes::install_local("pkg", dependencies = TRUE, upgrade = "never")
stopifnot(requireNamespace("fairy", quietly = TRUE))     # stop the build if the fairy could not be installed
