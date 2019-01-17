context("Able to find files ref'd with .PROJECT_ROOT keyword")

test_that("Can load files in correct directory", {
  dir <- system.file("test_data", "t1", package = "portrpaths")
  local <- file.path(dir, "local.yaml")
  shared <- file.path(dir, "shared.yaml")

  setup_t1(local, shared)

  portrpath <- PortrPath$new(local, shared)
  expect_files(portrpath)
})

context("Able to find files with abs roots")
test_that("More complex roots work", {

  dir <- system.file("test_data", "t2", package = "portrpaths")
  local <- file.path(dir, "local.yaml")
  shared <- file.path(dir, "shared.yaml")
  setup_t2(local, shared)

  tp <- PortrPath$new(local, shared)

  expect_files(tp)
})

context("Subsequent loads/reloads work correctly")

test_that("Can load/reload first example", {
  dir <- system.file("test_data", "t1", package = "portrpaths")
  local <- file.path(dir, "local.yaml")
  shared <- file.path(dir, "shared.yaml")

  setup_t1(local, shared)
  p1 <- PortrPath$new(local, shared)
  expect_files(p1)

  p2 <- PortrPath$new(local, shared)
  expect_files(p2)
})
