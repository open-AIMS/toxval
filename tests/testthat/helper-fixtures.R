# Load example fitted-model fixtures used across the test suite. These live in
# tests/testthat/fixtures/ (not data/) because they are test scaffolding, not
# user-facing package data. Loading them here makes each object available by
# name to every test file.
for (f in list.files(
  test_path("fixtures"),
  pattern = "[.]rda$",
  full.names = TRUE
)) {
  load(f)
}
