# Format/style code
system2("air", c("format", "."))

# Generate documentation
roxygen2md::roxygen2md()
devtools::document()

# README
devtools::build_readme()

# Build site
pkgdown::build_site()
browseURL("docs/index.html")

# Test & Check
devtools::test()
devtools::check()

rcmdcheck::rcmdcheck(
  args = c("--no-manual", "--as-cran"),
  build_args = "--resave-data=best",
  error_on = "warning"
)

# Code Coverage
covr::report(covr::package_coverage())

# Build Package
devtools::build()
