context("Able to load yaml file correctly")


test_that("yaml file loads", {
  dir <- system.file("inst", "test_data", "t1", package = "portrpaths")

  local <- file.path(dir, "local.yaml")
  shared <- file.path(dir, "shared.yaml")

  portrpath <- PortrPaths$new(local, shared)
  fail("Test Cases not written!")
})
