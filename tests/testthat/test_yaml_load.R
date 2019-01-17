context("Able to find files ref'd with .PROJECT_ROOT keyword")

test_that("Can load files in correct directory", {
  dir <- system.file("test_data", "t1", package = "portrpaths")

  local <- file.path(dir, "local.yaml")
  shared <- file.path(dir, "shared.yaml")

  setup_t1_example(local, shared)

  portrpath <- PortrPath$new(local, shared)
  f_paths <- portrpath$get_file_paths()

  for (nm in names(f_paths)){
    f <- f_paths[[nm]]
    expect_true(file.exists(f), info = glue::glue("Can {nm}: {f} be found?"), label = nm)
  }

})


