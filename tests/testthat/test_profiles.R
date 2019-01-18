context("Test loading and assigning profiles works")

test_that("Can load file with profiles", {
  prof_d <- glue::glue(
    "{root}{sep}profile",
    root = system.file("test_data", package = "portrpaths"),
    sep = .Platform$file.sep)

  types <- c("shared","local")
  p <- list()

  p[types] <- glue::glue("{prof_d}{sep}{types}.yaml",
                                sep = .Platform$file.sep)
  print(p)

  setup_profile_test(p$local, p$shared)

  pather <- PortrPath$new(p$local, p$shared)
  expect_files(pather)
  for (prof in c("default", "t1", "t2")){
    pather$profile <- prof
    test_log$info(glue::glue("{prof}:","{pather$files}"))
    expect_files(pather)
  }
})
