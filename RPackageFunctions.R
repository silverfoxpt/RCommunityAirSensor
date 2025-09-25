# Uninstall
remove.packages("testPackage")

src <- "D:/R/RCommunityAirSensor/"
library(pkgnet)
library(usethis)
library(devtools)

# Reinstalls
devtools::install(src, upgrade = "never")

# Load and generate pkgnet graph
library(pkgnet)
report <- CreatePackageReport(
  pkg_name      = "testPackage",       # must match DESCRIPTION: Package
  pkg_reporters = DefaultReporters()
)

# Write FunctionGenerator to graph

# Generate documentation based off of Roxygen2 comments
devtools::document()

# Check package okay
devtools::check()

# List all functions
lsf.str("package:testPackage")
