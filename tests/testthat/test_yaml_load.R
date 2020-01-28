context("Able to find files ref'd with .PROJECT_ROOT keyword")

test_that("Can load files in correct directory", {
  dir <- system.file("test_data", "t1", package = "portrpaths")
  local <- file.path(dir, "local.yaml")

  setup_t1(local)

  pp <- PortrPath$new(local)

  root_expected <- setup_t1_expected_root()

  expect_equal(pp$root, root_expected)

})

context("Able to find files with abs roots")
test_that("More complex roots work", {

  dir <- system.file("test_data", "t2", package = "portrpaths")
  local <- file.path(dir, "local.yaml")

  setup_t2(local)

  tp <- PortrPath$new(local)

  root_expected <- setup_t2_expected_root()

  expect_equal(tp$root, root_expected)
})

context("Subsequent loads/reloads work correctly")

test_that("Can load/reload first example", {
  `%||%` <- rlang::`%||%`

  dir <- system.file("test_data", "t1", package = "portrpaths")
  local <- file.path(dir, "local.yaml")

  setup_t1(local)

  config_preload <- yaml::read_yaml(local)

  expect_null(config_preload$profiles)
  config_preload$profiles <- config_preload$profiles %||% list()
  expect_null(config_preload$profiles$default)

  p1 <- PortrPath$new(local)

  root_expect <- setup_t1_expected_root()
  expect_equal(p1$root, root_expect)

  config_loaded <- yaml::read_yaml(local)

  profile_expect <- ".PROJECT_ROOT"

  expect_equal(config_loaded$profiles$default, profile_expect)

  p2 <- PortrPath$new(local)

  expect_equal(p1$root, p2$root)

})
