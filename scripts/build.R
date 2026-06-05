# README
devtools::build_readme()

# Generate documentation
devtools::document()

# Build site
pkgdown::build_site()

browseURL("docs/index.html")

devtools::test()

devtools::check()

rcmdcheck::rcmdcheck(
  args = c("--no-manual", "--as-cran"),
  build_args = "--resave-data=best",
  error_on = "warning"
)

devtools::build()
