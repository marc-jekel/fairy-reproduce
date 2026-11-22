# Run by Binder when the environment is built: installs the fairy and the packages it needs.
install.packages(c("remotes", "coda", "multinomineq", "volesti"))
remotes::install_url("https://anonymous.4open.science/api/repo/fairy-B0F2/zip?file=fairy.zip", subdir = "pkg", dependencies = TRUE)
